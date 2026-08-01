;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               next-tweet
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     CLI
;;;;DESCRIPTION
;;;;
;;;;    Prints the text of the next unread tweet from the X (Twitter)
;;;;    "Following" (reverse chronological home) timeline, followed on
;;;;    the last line by the number of tweets remaining to be read.
;;;;    When there is no new tweet, prints just 0.
;;;;
;;;;    Tweets are fetched from the official X API v2 (paid,
;;;;    pay-per-request: one request returns up to :page-size tweets),
;;;;    and buffered in a local queue, so that each invocation pops one
;;;;    tweet from the queue, and the API is only queried when the
;;;;    queue is empty, and at most once every :fetch-interval seconds
;;;;    (so it can safely be called from a shell prompt).
;;;;
;;;;    Options:
;;;;
;;;;        --help          Prints this documentation.
;;;;
;;;;        --count         Prints only the number of tweets remaining
;;;;                        in the local queue (doesn't consume, doesn't
;;;;                        fetch).
;;;;
;;;;        --peek          Prints the next tweet and count like the
;;;;                        default behavior, but without consuming the
;;;;                        tweet.
;;;;
;;;;        --status        Prints the configuration, queue length,
;;;;                        since-id and last fetch time.
;;;;
;;;;    Configuration file: ~/.config/next-tweet/config.lisp
;;;;    (or $XDG_CONFIG_HOME/next-tweet/config.lisp), a property list:
;;;;
;;;;        ;; -*- mode:lisp -*-
;;;;        (:account         "informatimago"       ; login in ~/.authinfo
;;;;         :machine         "api.x.com"           ; machine prefix in ~/.authinfo
;;;;         :api-base        "https://api.x.com"   ; API endpoint base URL
;;;;         :page-size       100                   ; tweets per API request (5-100)
;;;;         :max-pages       1                     ; max API requests per refill
;;;;         :fetch-interval  900                   ; min. seconds between API calls
;;;;         :show-author     nil                   ; when true, prefix "@author: "
;;;;         :state-directory "~/.cache/next-tweet/")
;;;;
;;;;    Credentials: ~/.authinfo (netrc format).  Create an application
;;;;    in the X developer portal <https://developer.x.com/>, generate
;;;;    the "API Key and Secret" (consumer) and the "Access Token and
;;;;    Secret" (user context, read permission), and store them as:
;;;;
;;;;        machine api.x.com/consumer-key    login informatimago password XXXX
;;;;        machine api.x.com/consumer-secret login informatimago password XXXX
;;;;        machine api.x.com/access-token    login informatimago password XXXX
;;;;        machine api.x.com/access-secret   login informatimago password XXXX
;;;;
;;;;    Requests are signed with OAuth 1.0a (HMAC-SHA1) user context.
;;;;
;;;;    State (tweet queue, since-id, cached user id) is kept in
;;;;    :state-directory, in a subdirectory named after the account, so
;;;;    several accounts can be used with several configuration files
;;;;    (see the NEXT_TWEET_CONFIG environment variable).
;;;;
;;;;    Note: when more than :page-size × :max-pages tweets are
;;;;    published between two refills, only the most recent ones are
;;;;    enqueued; the count printed is the local queue length, not a
;;;;    server-side count.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-08-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2026 - 2026
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "SCRIPT")

(command :use-systems (:babel :cl-base64 :drakma :ironclad :split-sequence :yason)
         :documentation "Prints the text of the next unread tweet of the X home timeline,
and on the last line, the number of tweets remaining to be read (0 if none).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.
;;;

(defparameter *unix-epoch* 2208988800
  "Universal time of the unix epoch 1970-01-01T00:00:00Z.")

(defun unix-time ()
  (- (get-universal-time) *unix-epoch*))

(defun home-relative-pathname (path)
  "When PATH is a string starting with ~/ expand it relative to the
user home directory; otherwise return it as a pathname."
  (if (and (stringp path)
           (<= 2 (length path))
           (string= "~/" path :end2 2))
      (merge-pathnames (subseq path 2) (user-homedir-pathname))
      (pathname path)))

(defun getenv-or (variable default)
  (let ((value (getenv variable)))
    (if (and value (plusp (length value)))
        value
        default)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration.
;;;

(defparameter *default-configuration*
  '(:account         nil
    :machine         "api.x.com"
    :api-base        "https://api.x.com"
    :page-size       100
    :max-pages       1
    :fetch-interval  900
    :show-author     nil
    :state-directory "~/.cache/next-tweet/"))

(defvar *configuration* '())

(defun configuration-pathname ()
  (home-relative-pathname
   (getenv-or "NEXT_TWEET_CONFIG"
              (namestring
               (merge-pathnames "next-tweet/config.lisp"
                                (home-relative-pathname
                                 (getenv-or "XDG_CONFIG_HOME" "~/.config/")))))))

(defun load-configuration ()
  (let ((path (configuration-pathname)))
    (setf *configuration*
          (append (if (probe-file path)
                      (with-open-file (stream path)
                        (let ((*read-eval* nil))
                          (read stream nil '())))
                      '())
                  *default-configuration*))))

(defun config (key)
  (getf *configuration* key))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ~/.authinfo (netrc) parsing.
;;;

(defun authinfo-pathname ()
  (home-relative-pathname (getenv-or "NEXT_TWEET_AUTHINFO" "~/.authinfo")))

(defun tokenize-authinfo (stream)
  "Returns the list of tokens in the netrc STREAM.
Tokens are separated by whitespace; a token may be quoted with double-quotes,
in which case backslash escapes the next character."
  (loop
    :with tokens := '()
    :for ch := (read-char stream nil nil)
    :while ch
    :do (cond
          ((member ch '(#\space #\tab #\newline #\return #\page)))
          ((char= ch #\")
           (push (with-output-to-string (token)
                   (loop
                     :for c := (read-char stream nil nil)
                     :while (and c (char/= c #\"))
                     :do (write-char (if (char= c #\\)
                                         (or (read-char stream nil nil) c)
                                         c)
                                     token)))
                 tokens))
          (t
           (push (with-output-to-string (token)
                   (write-char ch token)
                   (loop
                     :for c := (peek-char nil stream nil nil)
                     :while (and c (not (member c '(#\space #\tab #\newline #\return #\page))))
                     :do (write-char (read-char stream) token)))
                 tokens)))
    :finally (return (nreverse tokens))))

(defun parse-authinfo (pathname)
  "Returns a list of entries; each entry is an alist of (key . value) strings.
Entries begin at each \"machine\" (or \"default\") token."
  (with-open-file (stream pathname :if-does-not-exist nil)
    (when stream
      (loop
        :with entries := '()
        :with entry := '()
        :with tokens := (tokenize-authinfo stream)
        :while tokens
        :do (let ((key (pop tokens)))
              (cond
                ((string= key "default")
                 (when entry (push (nreverse entry) entries))
                 (setf entry (list (cons "machine" "default"))))
                ((string= key "macdef") ; skip macro definitions
                 (pop tokens))
                (t
                 (let ((value (pop tokens)))
                   (when (string= key "machine")
                     (when entry (push (nreverse entry) entries))
                     (setf entry '()))
                   (push (cons key value) entry)))))
        :finally (when entry (push (nreverse entry) entries))
                 (return (nreverse entries))))))

(defun authinfo-password (entries machine login)
  "Returns the password of the ENTRIES entry matching MACHINE and LOGIN.
When LOGIN is NIL, the first entry matching MACHINE is used."
  (loop
    :for entry :in entries
    :when (and (equal machine (cdr (assoc "machine" entry :test (function string=))))
               (or (null login)
                   (equal login (cdr (assoc "login" entry :test (function string=))))))
      :do (return (cdr (assoc "password" entry :test (function string=))))))

(defun load-credentials ()
  "Returns a plist (:consumer-key :consumer-secret :access-token :access-secret)
read from ~/.authinfo, for the configured :machine and :account."
  (let* ((path    (authinfo-pathname))
         (entries (parse-authinfo path))
         (machine (config :machine))
         (login   (config :account)))
    (flet ((look-up (suffix)
             (or (authinfo-password entries (concat machine "/" suffix) login)
                 (error "Missing entry \"machine ~A/~A~@[ login ~A~] password …\" in ~A"
                        machine suffix login (namestring path)))))
      (list :consumer-key  (look-up "consumer-key")
            :consumer-secret (look-up "consumer-secret")
            :access-token  (look-up "access-token")
            :access-secret (look-up "access-secret")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OAuth 1.0a (RFC 5849) request signing, HMAC-SHA1.
;;;

(defun oauth-encode (string)
  "Percent-encodes STRING per RFC 3986 (only unreserved characters kept),
as required by OAuth 1.0a."
  (with-output-to-string (out)
    (loop
      :for octet :across (babel:string-to-octets string :encoding :utf-8)
      :for ch := (code-char octet)
      :do (if (or (char<= #\A ch #\Z)
                  (char<= #\a ch #\z)
                  (char<= #\0 ch #\9)
                  (member ch '(#\- #\. #\_ #\~)))
              (write-char ch out)
              (format out "~:@(%~2,'0X~)" octet)))))

(defun make-nonce ()
  "Returns a random hexadecimal string."
  (with-output-to-string (out)
    (handler-case
        (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
          (loop :repeat 16 :do (format out "~2,'0X" (read-byte urandom))))
      (error ()
        (let ((*random-state* (make-random-state t)))
          (format out "~8,'0X~8,'0X~8,'0X~8,'0X"
                  (random #x100000000) (random #x100000000)
                  (random #x100000000) (unix-time)))))))

(defun hmac-sha1-base64 (key message)
  (let ((hmac (ironclad:make-hmac (babel:string-to-octets key :encoding :utf-8)
                                  :sha1)))
    (ironclad:update-hmac hmac (babel:string-to-octets message :encoding :utf-8))
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun oauth-signature (method url parameters consumer-secret token-secret)
  "Computes the OAuth 1.0a HMAC-SHA1 signature.
METHOD: \"GET\" or \"POST\"; URL: the base URL without query string;
PARAMETERS: an alist of (name . value) strings, query and oauth parameters."
  (let* ((sorted (sort (mapcar (lambda (parameter)
                                 (cons (oauth-encode (car parameter))
                                       (oauth-encode (cdr parameter))))
                               parameters)
                       (lambda (a b)
                         (or (string< (car a) (car b))
                             (and (string= (car a) (car b))
                                  (string< (cdr a) (cdr b)))))))
         (parameter-string (format nil "~{~A~^&~}"
                                   (mapcar (lambda (parameter)
                                             (concat (car parameter) "=" (cdr parameter)))
                                           sorted)))
         (base-string (concat (string-upcase method)
                              "&" (oauth-encode url)
                              "&" (oauth-encode parameter-string)))
         (signing-key (concat (oauth-encode consumer-secret)
                              "&" (oauth-encode token-secret))))
    (hmac-sha1-base64 signing-key base-string)))

(defun oauth-authorization-header (method url query-parameters credentials
                                   &key (nonce (make-nonce))
                                     (timestamp (princ-to-string (unix-time))))
  "Returns the value of the Authorization header for the request."
  (let* ((oauth-parameters
           (list (cons "oauth_consumer_key"     (getf credentials :consumer-key))
                 (cons "oauth_nonce"            nonce)
                 (cons "oauth_signature_method" "HMAC-SHA1")
                 (cons "oauth_timestamp"        timestamp)
                 (cons "oauth_token"            (getf credentials :access-token))
                 (cons "oauth_version"          "1.0")))
         (signature (oauth-signature method url
                                     (append query-parameters oauth-parameters)
                                     (getf credentials :consumer-secret)
                                     (getf credentials :access-secret))))
    (format nil "OAuth ~{~A~^, ~}"
            (mapcar (lambda (parameter)
                      (format nil "~A=\"~A\""
                              (oauth-encode (car parameter))
                              (oauth-encode (cdr parameter))))
                    (append oauth-parameters
                            (list (cons "oauth_signature" signature)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; X API v2 requests.
;;;

(define-condition api-error (error)
  ((status  :initarg :status  :reader api-error-status)
   (message :initarg :message :reader api-error-message))
  (:report (lambda (condition stream)
             (format stream "X API error ~A: ~A"
                     (api-error-status condition)
                     (api-error-message condition)))))

(defun api-get (path query-parameters credentials)
  "Performs a signed GET request on (concat (config :api-base) PATH),
and returns the response JSON parsed by YASON (hash-tables)."
  (let* ((url  (concat (config :api-base) path))
         (auth (oauth-authorization-header "GET" url query-parameters credentials))
         ;; Build the query string ourselves with the same encoding as
         ;; the signature, so that the signed request is the sent request:
         (full-url (if query-parameters
                       (format nil "~A?~{~A~^&~}"
                               url
                               (mapcar (lambda (parameter)
                                         (concat (oauth-encode (car parameter))
                                                 "="
                                                 (oauth-encode (cdr parameter))))
                                       query-parameters))
                       url)))
    (multiple-value-bind (body status)
        (drakma:http-request full-url
                             :method :get
                             :additional-headers (list (cons "Authorization" auth))
                             :user-agent "next-tweet/1.0 (Common Lisp)"
                             :external-format-in :utf-8)
      (let ((text (if (stringp body)
                      body
                      (babel:octets-to-string body :encoding :utf-8))))
        (if (= 200 status)
            (yason:parse text)
            (error 'api-error :status status :message text))))))

(defun ref (object &rest keys)
  "Accesses nested YASON hash-tables."
  (reduce (lambda (obj key)
            (when (hash-table-p obj)
              (gethash key obj)))
          keys
          :initial-value object))

(defun fetch-user-id (credentials)
  "Returns the id (a string) of the authenticated user."
  (or (ref (api-get "/2/users/me" '() credentials) "data" "id")
      (error "Cannot get the authenticated user id.")))

(defun fetch-new-tweets (credentials user-id since-id)
  "Fetches the tweets of the reverse chronological home timeline newer
than SINCE-ID (up to (config :max-pages) requests of (config :page-size)
tweets).  Returns two values: a list of tweet plists sorted from oldest
to newest, and the new since-id."
  (let ((path (format nil "/2/users/~A/timelines/reverse_chronological" user-id))
        (tweets '())
        (newest-id since-id)
        (pagination-token nil))
    (loop
      :repeat (max 1 (config :max-pages))
      :do (let* ((parameters
                   (append (list (cons "max_results"
                                       (princ-to-string
                                        (min 100 (max 5 (config :page-size)))))
                                 (cons "expansions"   "author_id")
                                 (cons "tweet.fields" "created_at,author_id")
                                 (cons "user.fields"  "username"))
                           (when since-id
                             (list (cons "since_id" since-id)))
                           (when pagination-token
                             (list (cons "pagination_token" pagination-token)))))
                 (response (api-get path parameters credentials))
                 (users    (make-hash-table :test (function equal))))
            (dolist (user (ref response "includes" "users"))
              (setf (gethash (gethash "id" user) users)
                    (gethash "username" user)))
            (dolist (tweet (ref response "data"))
              (push (list :id      (gethash "id" tweet)
                          :author  (gethash (gethash "author_id" tweet) users)
                          :created (gethash "created_at" tweet)
                          :text    (gethash "text" tweet))
                    tweets))
            (let ((meta-newest (ref response "meta" "newest_id")))
              (when (and meta-newest
                         (or (null newest-id)
                             (string< (format nil "~40,'0D" (or newest-id 0))
                                      (format nil "~40,'0D" meta-newest))))
                (setf newest-id meta-newest)))
            (setf pagination-token (ref response "meta" "next_token")))
      :while pagination-token)
    ;; TWEETS contains pages of newest-to-oldest tweets, pushed in
    ;; reverse, so it's already sorted from oldest to newest:
    (values tweets newest-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Local state: tweet queue, since-id, cached user id.
;;;

(defun state-directory ()
  (let ((directory (merge-pathnames
                    (make-pathname :directory (list :relative
                                                    (or (config :account) "default")))
                    (home-relative-pathname (config :state-directory)))))
    (ensure-directories-exist (merge-pathnames "probe" directory))
    directory))

(defun state-file (name)
  (merge-pathnames name (state-directory)))

(defun load-sexp (pathname default)
  (if (probe-file pathname)
      (with-open-file (stream pathname :external-format :utf-8)
        (let ((*read-eval* nil))
          (read stream nil default)))
      default))

(defun save-sexp (pathname value)
  "Atomically (write to temporary + rename) saves VALUE to PATHNAME."
  (let ((temporary (merge-pathnames
                    (make-pathname :type (format nil "tmp~D" (getpid)))
                    pathname)))
    (with-open-file (stream temporary
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-pretty*   t)
              (*package*        (find-package "KEYWORD")))
          (print value stream)))
      (terpri stream))
    (rename-file temporary pathname #+clisp :if-exists #+clisp :overwrite)
    value))

(defun load-state () (load-sexp (state-file "state.lisp") '()))
(defun save-state (state) (save-sexp (state-file "state.lisp") state))
(defun load-queue () (load-sexp (state-file "queue.lisp") '()))
(defun save-queue (queue) (save-sexp (state-file "queue.lisp") queue))

(defmacro with-file-lock ((&key (name "lock") (stale 60)) &body body)
  "Evaluates BODY with an exclusive lock file in the state directory.
A lock file older than STALE seconds is considered stale and stolen.
When the lock cannot be acquired, BODY is evaluated anyway (better to
possibly show a tweet twice than to block the shell prompt)."
  (let ((vpath (gensym "PATH")) (vstream (gensym "STREAM")) (vi (gensym "I")))
    `(let ((,vpath (state-file ,name)))
       (unwind-protect
            (progn
              (loop
                :for ,vi :from 0 :below 10
                :for ,vstream := (open ,vpath :direction :output
                                              :if-exists nil
                                              :if-does-not-exist :create)
                :until (cond
                         (,vstream
                          (close ,vstream)
                          t)
                         ((let ((date (ignore-errors (file-write-date ,vpath))))
                            (and date (< (+ date ,stale) (get-universal-time))))
                          (ignore-errors (delete-file ,vpath))
                          nil)
                         (t
                          (sleep 0.2)
                          nil)))
              ,@body)
         (ignore-errors (delete-file ,vpath))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refill and display.
;;;

(defun refill-queue (queue state)
  "When QUEUE is empty and the last fetch is older than :fetch-interval,
fetches new tweets and appends them to QUEUE.
Returns two values: the new queue and the new state."
  (let ((now        (get-universal-time))
        (last-fetch (getf state :last-fetch 0)))
    (if (or queue
            (< now (+ last-fetch (config :fetch-interval))))
        (values queue state)
        (let* ((credentials (load-credentials))
               (user-id     (or (getf state :user-id)
                                (fetch-user-id credentials))))
          ;; Record the fetch time first, so that failures don't make
          ;; each shell prompt retry the API before the interval:
          (setf state (list* :last-fetch now
                             :user-id user-id
                             (progn
                               (remf state :last-fetch)
                               (remf state :user-id)
                               state)))
          (multiple-value-bind (tweets newest-id)
              (fetch-new-tweets credentials user-id (getf state :since-id))
            (when newest-id
              (setf state (list* :since-id newest-id
                                 (progn (remf state :since-id) state))))
            (values (append queue tweets) state))))))

(defun display-tweet (tweet remaining)
  (when tweet
    (if (and (config :show-author) (getf tweet :author))
        (format t "@~A: ~A~%" (getf tweet :author) (getf tweet :text))
        (format t "~A~%" (getf tweet :text))))
  (format t "~D~%" remaining)
  (finish-output))

(defun next-tweet (&key (consume t))
  "Prints the next tweet and the remaining count; returns the exit status."
  (with-file-lock ()
    (let ((queue (load-queue))
          (state (load-state)))
      (multiple-value-bind (queue state) (refill-queue queue state)
        (save-state state)
        (let ((tweet (first queue)))
          (when (and tweet consume)
            (save-queue (rest queue)))
          (display-tweet tweet (length (rest queue)))))))
  ex-ok)

(defun print-count ()
  (format t "~D~%" (length (load-queue)))
  (finish-output)
  ex-ok)

(defun print-status ()
  (let ((state (load-state)))
    (format t "configuration:   ~A~%" (namestring (configuration-pathname)))
    (format t "account:         ~A~%" (or (config :account) "(any)"))
    (format t "machine:         ~A~%" (config :machine))
    (format t "api-base:        ~A~%" (config :api-base))
    (format t "state-directory: ~A~%" (namestring (state-directory)))
    (format t "queue length:    ~D~%" (length (load-queue)))
    (format t "since-id:        ~A~%" (or (getf state :since-id) "(none)"))
    (format t "user-id:         ~A~%" (or (getf state :user-id) "(none)"))
    (let ((last-fetch (getf state :last-fetch)))
      (if last-fetch
          (multiple-value-bind (se mi ho da mo ye)
              (decode-universal-time last-fetch 0)
            (format t "last fetch:      ~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ~%"
                    ye mo da ho mi se))
          (format t "last fetch:      (never)~%"))))
  (finish-output)
  ex-ok)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main.
;;;

(defun main (arguments)
  (handler-case
      (progn
        (load-configuration)
        (cond
          ((member "--help" arguments :test (function string=))
           (format t "~A~%" (command-documentation (command-named *program-name*)))
           (format t "Usage: ~A [--help|--count|--peek|--status]~%" *program-name*)
           ex-ok)
          ((member "--count"  arguments :test (function string=)) (print-count))
          ((member "--status" arguments :test (function string=)) (print-status))
          ((member "--peek"   arguments :test (function string=)) (next-tweet :consume nil))
          (arguments
           (format *error-output* "~A: invalid arguments: ~{~A~^ ~}~%"
                   *program-name* arguments)
           ex-usage)
          (t (next-tweet))))
    (api-error (err)
      ;; Print 0 on stdout so a shell prompt degrades gracefully:
      (format *error-output* "~A: ~A~%" *program-name* err)
      (format t "0~%")
      (finish-output)
      ex-unavailable)
    (error (err)
      (format *error-output* "~A: ~A~%" *program-name* err)
      (format t "0~%")
      (finish-output)
      ex-software)))

;;;; THE END ;;;;
