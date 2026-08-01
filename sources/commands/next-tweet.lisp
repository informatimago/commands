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
;;;;    Tweets are fetched (one request returns up to :page-size
;;;;    tweets) and buffered in a local queue, so that each invocation
;;;;    pops one tweet from the queue, and the backend is only queried
;;;;    when the queue is empty, and at most once every :fetch-interval
;;;;    seconds (so it can safely be called from a shell prompt).
;;;;
;;;;    Two backends are selectable with the :backend configuration
;;;;    key:
;;;;
;;;;        :api  (default)  The official X API v2.  Requires a
;;;;                         developer account; reading is paid
;;;;                         (pay-per-request).  Credentials: the four
;;;;                         OAuth 1.0a secrets (see below).
;;;;
;;;;        :web             The private GraphQL API of the X web
;;;;                         client, authenticated with the session
;;;;                         cookies of a logged-in browser (auth_token
;;;;                         and ct0).  Free, but unofficial: it may be
;;;;                         contrary to the X terms of service, and the
;;;;                         GraphQL query ids and "features" parameter
;;;;                         drift over time and must occasionally be
;;;;                         refreshed from the browser (see the
;;;;                         readme).  Credentials: two cookies (see
;;;;                         below).
;;;;
;;;;    Options (parsed with the standard framework option machinery,
;;;;    so -h/--help and -V/--version work as for the other commands):
;;;;
;;;;        -h --help       Prints the option list and this documentation.
;;;;
;;;;        -V --version    Prints the command version and exits.
;;;;
;;;;        --debug         Dumps the backend requests and responses to
;;;;                        the standard error output (to diagnose API
;;;;                        errors).  May be combined with any action.
;;;;
;;;;        --count         Prints only the number of tweets remaining
;;;;                        in the current context (doesn't consume,
;;;;                        doesn't fetch).
;;;;
;;;;        --peek          Prints the next tweet and count like the
;;;;                        default behavior, but without consuming the
;;;;                        tweet.
;;;;
;;;;        --status        Prints the configuration, queue length,
;;;;                        since-id and last fetch time.
;;;;
;;;;        --enter         Enters the thread of the last displayed
;;;;                        tweet: the following invocations read the
;;;;                        tweets of that thread (the self-replies of
;;;;                        its author, in chronological order, each
;;;;                        prefixed by the author on its own first
;;;;                        line) instead of the timeline.  Prints the
;;;;                        number of thread tweets enqueued.  With the
;;;;                        :api backend the thread is fetched with the
;;;;                        recent search API, so only tweets from the
;;;;                        last 7 days are found.  When no thread tweet
;;;;                        is found, stays on the timeline and prints 0.
;;;;
;;;;        --leave         Returns to the main "Following" timeline
;;;;                        (whose queue and since-id are preserved
;;;;                        while reading a thread).  Prints the number
;;;;                        of tweets remaining in the timeline queue.
;;;;
;;;;    Configuration file: ~/.config/next-tweet/config.lisp
;;;;    (or $XDG_CONFIG_HOME/next-tweet/config.lisp), a property list:
;;;;
;;;;        ;; -*- mode:lisp -*-
;;;;        (:backend         :api                  ; :api or :web
;;;;         :account         "informatimago"       ; login in ~/.authinfo
;;;;         :machine         "api.x.com"           ; machine prefix in ~/.authinfo
;;;;         :api-base        "https://api.x.com"   ; :api endpoint base URL
;;;;         :page-size       100                   ; tweets per request (5-100)
;;;;         :max-pages       1                     ; max requests per refill
;;;;         :fetch-interval  900                   ; min. seconds between calls
;;;;         :show-author     nil                   ; when true, prefix "@author: "
;;;;         :state-directory "~/.cache/next-tweet/"
;;;;         ;; :web backend only:
;;;;         :web-base            "https://x.com/i/api/graphql"
;;;;         :web-query-id-home   nil               ; HomeLatestTimeline query id
;;;;         :web-query-id-detail nil               ; TweetDetail query id
;;;;         :web-features        nil)              ; features JSON string (or nil)
;;;;
;;;;    Credentials: ~/.authinfo (netrc format).  The several secrets
;;;;    share the same :machine (a bare FQDN) and are disambiguated by
;;;;    the port field; # comments and blank lines are ignored.
;;;;
;;;;    For the :api backend, create an application in the X developer
;;;;    portal <https://developer.x.com/>, generate the "API Key and
;;;;    Secret" (consumer) and the "Access Token and Secret" (user
;;;;    context, read permission), and store them as:
;;;;
;;;;        machine api.x.com port consumer-key    login informatimago password XXXX
;;;;        machine api.x.com port consumer-secret login informatimago password XXXX
;;;;        machine api.x.com port access-token    login informatimago password XXXX
;;;;        machine api.x.com port access-secret   login informatimago password XXXX
;;;;
;;;;    Requests are then signed with OAuth 1.0a (HMAC-SHA1) user
;;;;    context.
;;;;
;;;;    For the :web backend, copy the auth_token and ct0 cookies from a
;;;;    logged-in browser session (developer tools > Application >
;;;;    Cookies for x.com), and store them as:
;;;;
;;;;        machine api.x.com port auth-token login informatimago password XXXX
;;;;        machine api.x.com port csrf-token login informatimago password XXXX
;;;;
;;;;    (csrf-token is the ct0 cookie.)  You must also set
;;;;    :web-query-id-home (and :web-query-id-detail for --enter) in the
;;;;    configuration; see the readme for how to read them from the
;;;;    browser.
;;;;
;;;;    The legacy form, with the secret appended to the machine name
;;;;    (machine api.x.com/auth-token …), is still accepted as a
;;;;    fallback.
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
;;;;    Shell aliases may be convenient:
;;;;
;;;;        alias enter-thread='next-tweet --enter'
;;;;        alias leave-thread='next-tweet --leave'
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
         :version "1.1.0"
         :documentation "
Prints the text of the next unread tweet of the X (Twitter) \"Following\"
(reverse chronological home) timeline, and on the last line, the number
of tweets remaining to be read (0, alone, when there is none).

Tweets are buffered in a local queue; the backend (:api, the official
X API v2, or :web, the X web client GraphQL) is queried only when the
queue is empty and at most once every :fetch-interval seconds, so the
command is safe to call from a shell prompt.

Configuration: ~/.config/next-tweet/config.lisp (see the source header
and next-tweet-readme.org).  Credentials: ~/.authinfo.

With no option, prints (and consumes) the next tweet.  --enter switches
to the thread of the last displayed tweet; --leave returns to the
timeline.")

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
  '(:backend         :api                  ; :api (official) or :web (session cookies)
    :account         nil
    :machine         "api.x.com"
    :api-base        "https://api.x.com"
    ;; :web backend:
    :web-base        "https://x.com/i/api/graphql"
    :web-query-id-home   nil               ; HomeLatestTimeline query id (see readme)
    :web-query-id-detail nil               ; TweetDetail query id (see readme)
    :web-features    nil                   ; features JSON string (nil => built-in default)
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

(defun debug-log (control &rest arguments)
  "When the :debug configuration flag (or --debug) is set, prints a
formatted message to *ERROR-OUTPUT*."
  (when (config :debug)
    (format *error-output* "~&;; [next-tweet] ~?~%" control arguments)
    (finish-output *error-output*)))

(defun redact (value)
  "Returns a short, non-revealing representation of a secret VALUE, for
debug output."
  (let ((string (princ-to-string (or value ""))))
    (if (< 10 (length string))
        (format nil "~A…~A [~D chars]"
                (subseq string 0 3) (subseq string (- (length string) 3))
                (length string))
        "…")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ~/.authinfo (netrc) parsing.
;;;

(defun authinfo-pathname ()
  (home-relative-pathname (getenv-or "NEXT_TWEET_AUTHINFO" "~/.authinfo")))

(defun tokenize-authinfo (stream)
  "Returns the list of tokens in the netrc STREAM.
Tokens are separated by whitespace (blank lines are therefore
ignored).  A token may be quoted with double-quotes, in which case
backslash escapes the next character.  A # that is the first non-blank
character of a line introduces a comment that runs to the end of the
line; a # anywhere else is an ordinary character (passwords may
contain #).  A leading UTF-8 BOM is ignored."
  (loop
    :with tokens := '()
    :with bol    := t                    ; at the beginning of a line?
    :for ch := (read-char stream nil nil)
    :while ch
    :do (cond
          ((char= ch #\newline) (setf bol t))
          ((member ch '(#\space #\tab #\return #\page))) ; keep BOL as is
          ((= (char-code ch) #xFEFF))    ; ignore a byte-order mark
          ((and bol (char= ch #\#))      ; full-line comment: skip to EOL
           (loop :for c := (read-char stream nil nil)
                 :while (and c (char/= c #\newline)))
           (setf bol t))
          ((char= ch #\")
           (setf bol nil)
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
           (setf bol nil)
           (push (with-output-to-string (token)
                   (write-char ch token)
                   (loop
                     :for c := (peek-char nil stream nil nil)
                     :while (and c (not (member c '(#\space #\tab #\newline #\return #\page))))
                     :do (write-char (read-char stream) token)))
                 tokens)))
    :finally (return (nreverse tokens))))

(defparameter *authinfo-value-keywords*
  '("login" "user" "password" "account" "port" "protocol")
  "netrc keywords that are followed by a value token.")

(defun parse-authinfo (pathname)
  "Returns a list of entries; each entry is an alist of (key . value) strings.
A new entry starts at each \"machine\" or \"default\" token.  Recognized
keywords (login/user, password, account, port, protocol) take the
following token as their value (\"user\" is stored as \"login\"); any
other token is ignored, so a stray token (e.g. a leftover from a
comment) cannot desynchronize the parse."
  (with-open-file (stream pathname :if-does-not-exist nil)
    (when stream
      (let ((tokens  (tokenize-authinfo stream))
            (entries '())
            (entry   nil))
        (flet ((flush () (when entry (push (nreverse entry) entries) (setf entry nil))))
          (loop
            :while tokens
            :do (let ((key (pop tokens)))
                  (cond
                    ((string-equal key "machine")
                     (flush)
                     (setf entry (list (cons "machine" (or (pop tokens) "")))))
                    ((string-equal key "default")
                     (flush)
                     (setf entry (list (cons "machine" "default"))))
                    ((string-equal key "macdef") ; skip the macro name
                     (pop tokens))
                    ((member key *authinfo-value-keywords* :test (function string-equal))
                     (let ((value (pop tokens)))
                       (when (and entry value)
                         (push (cons (if (string-equal key "user") "login" (string-downcase key))
                                     value)
                               entry))))
                    (t nil)))            ; ignore unknown tokens
            :finally (flush))
          (nreverse entries))))))

(defun authinfo-field (entry key)
  (cdr (assoc key entry :test (function string=))))

(defun authinfo-password (entries machine login)
  "Returns the password of the ENTRIES entry matching MACHINE and LOGIN.
When LOGIN is NIL, the first entry matching MACHINE is used."
  (loop
    :for entry :in entries
    :when (and (equal machine (authinfo-field entry "machine"))
               (or (null login)
                   (equal login (authinfo-field entry "login"))))
      :do (return (authinfo-field entry "password"))))

(defun authinfo-lookup (entries machine suffix login)
  "Returns the password identifying the SUFFIX secret of (MACHINE, LOGIN).
Two conventions are accepted, in order of preference:
  1. a bare FQDN machine disambiguated by the port field
       machine MACHINE port SUFFIX login LOGIN password …
  2. the suffix appended to the machine (legacy)
       machine MACHINE/SUFFIX login LOGIN password …
LOGIN NIL matches any login."
  (flet ((login-ok (entry)
           (or (null login) (equal login (authinfo-field entry "login")))))
    (or (loop :for entry :in entries
              :when (and (equal machine (authinfo-field entry "machine"))
                         (equal suffix  (authinfo-field entry "port"))
                         (login-ok entry))
                :return (authinfo-field entry "password"))
        (loop :for entry :in entries
              :when (and (equal (concat machine "/" suffix) (authinfo-field entry "machine"))
                         (login-ok entry))
                :return (authinfo-field entry "password")))))

(defun make-authinfo-looker (path entries machine login)
  "Returns a function of one SUFFIX returning its password, or signaling
a helpful error naming both accepted entry forms."
  (lambda (suffix)
    (or (authinfo-lookup entries machine suffix login)
        (error "Missing ~A credential in ~A: add a line~%  machine ~A port ~A~@[ login ~A~] password …~@
                (or the legacy \"machine ~A/~A …\")"
               suffix (namestring path) machine suffix login machine suffix))))

(defun load-credentials ()
  "Returns a plist (:consumer-key :consumer-secret :access-token :access-secret)
read from ~/.authinfo, for the configured :machine and :account."
  (let* ((path    (authinfo-pathname))
         (look-up (make-authinfo-looker path (parse-authinfo path)
                                        (config :machine) (config :account))))
    (list :consumer-key    (funcall look-up "consumer-key")
          :consumer-secret (funcall look-up "consumer-secret")
          :access-token    (funcall look-up "access-token")
          :access-secret   (funcall look-up "access-secret"))))

(defun web-credentials ()
  "Returns a plist (:auth-token :ct0) read from ~/.authinfo, for the
configured :machine and :account.  These are the session cookies of
the X web client: auth_token and ct0 (the CSRF token)."
  (let* ((path    (authinfo-pathname))
         (look-up (make-authinfo-looker path (parse-authinfo path)
                                        (config :machine) (config :account))))
    (list :auth-token (funcall look-up "auth-token")
          :ct0        (funcall look-up "csrf-token"))))


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
    (when (config :debug)
      (debug-log "api GET ~A" full-url)
      (debug-log "  authorization: ~A" (redact auth)))
    (multiple-value-bind (body status)
        (drakma:http-request full-url
                             :method :get
                             :additional-headers (list (cons "Authorization" auth))
                             :user-agent "next-tweet/1.0 (Common Lisp)"
                             :external-format-in :utf-8)
      (let ((text (if (stringp body)
                      body
                      (babel:octets-to-string body :encoding :utf-8))))
        (when (config :debug)
          (debug-log "  HTTP ~A, ~D bytes" status (length text))
          (debug-log "  response: ~A" text))
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
                                 (cons "tweet.fields" "created_at,author_id,conversation_id")
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
              (push (list :id           (gethash "id" tweet)
                          :author       (gethash (gethash "author_id" tweet) users)
                          :conversation (gethash "conversation_id" tweet)
                          :created      (gethash "created_at" tweet)
                          :text         (gethash "text" tweet))
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

(defun fetch-thread-tweets (credentials conversation-id author after-id)
  "Fetches the tweets of the thread CONVERSATION-ID (the self-replies
of AUTHOR in this conversation) newer than AFTER-ID, with the recent
search API (which only covers the last 7 days).  Returns a list of
tweet plists sorted from oldest to newest."
  (let ((tweets '())
        (pagination-token nil))
    (loop
      :repeat (max 1 (config :max-pages))
      :do (let* ((parameters
                   (append (list (cons "query"
                                       (format nil "conversation_id:~A from:~A to:~A"
                                               conversation-id author author))
                                 (cons "max_results"
                                       (princ-to-string
                                        (min 100 (max 10 (config :page-size)))))
                                 (cons "tweet.fields" "created_at,author_id,conversation_id")
                                 (cons "expansions"   "author_id")
                                 (cons "user.fields"  "username"))
                           (when after-id
                             (list (cons "since_id" after-id)))
                           (when pagination-token
                             (list (cons "next_token" pagination-token)))))
                 (response (api-get "/2/tweets/search/recent" parameters credentials))
                 (users    (make-hash-table :test (function equal))))
            (dolist (user (ref response "includes" "users"))
              (setf (gethash (gethash "id" user) users)
                    (gethash "username" user)))
            (dolist (tweet (ref response "data"))
              (push (list :id           (gethash "id" tweet)
                          :author       (gethash (gethash "author_id" tweet) users)
                          :conversation (gethash "conversation_id" tweet)
                          :created      (gethash "created_at" tweet)
                          :text         (gethash "text" tweet))
                    tweets))
            (setf pagination-token (ref response "meta" "next_token")))
      :while pagination-token)
    ;; Pages are newest-to-oldest, pushed in reverse: oldest to newest.
    tweets))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; :web backend — X web client GraphQL, authenticated with the session
;;; cookies (auth_token + ct0).  This is the same private API the
;;; twitter.com/x.com single-page app uses.  It is not an officially
;;; supported interface: the GraphQL query ids and the "features"
;;; parameter change over time and must occasionally be refreshed from
;;; the browser (see the readme).  Using it may also be contrary to the
;;; X terms of service; it's provided as an alternative when a paid
;;; developer account is not available.
;;;

(defparameter *web-bearer*
  "AAAAAAAAAAAAAAAAAAAAANRILgAAAAAAnNwIzUejRCOuH5E6I8xnZz4puTs%3D1Zv7ttfk8LF81IUq16cHjhLTvJu4FA33AGWWjCpTnA"
  "The public bearer token embedded in the X web client.")

(defparameter *default-web-features*
  "{\"rweb_video_screen_enabled\":false,\"profile_label_improvements_pcf_label_in_post_enabled\":true,\"rweb_tipjar_consumption_enabled\":true,\"verified_phone_label_enabled\":false,\"creator_subscriptions_tweet_preview_api_enabled\":true,\"responsive_web_graphql_timeline_navigation_enabled\":true,\"responsive_web_graphql_skip_user_profile_image_extensions_enabled\":false,\"premium_content_api_read_enabled\":false,\"communities_web_enable_tweet_community_results_fetch\":true,\"c9s_tweet_anatomy_moderator_badge_enabled\":true,\"responsive_web_grok_analyze_button_fetch_trends_enabled\":false,\"responsive_web_grok_analyze_post_followups_enabled\":true,\"responsive_web_jetfuel_frame\":false,\"responsive_web_grok_share_attachment_enabled\":true,\"articles_preview_enabled\":true,\"responsive_web_edit_tweet_api_enabled\":true,\"graphql_is_translatable_rweb_tweet_is_translatable_enabled\":true,\"view_counts_everywhere_api_enabled\":true,\"longform_notetweets_consumption_enabled\":true,\"responsive_web_twitter_article_tweet_consumption_enabled\":true,\"tweet_awards_web_tipping_enabled\":false,\"responsive_web_grok_show_grok_translated_post\":false,\"responsive_web_grok_analysis_button_from_backend\":true,\"creator_subscriptions_quote_tweet_preview_enabled\":false,\"freedom_of_speech_not_reach_fetch_enabled\":true,\"standardized_nudges_misinfo\":true,\"tweet_with_visibility_results_prefer_gql_limited_actions_policy_enabled\":true,\"longform_notetweets_rich_text_read_enabled\":true,\"longform_notetweets_inline_media_enabled\":true,\"responsive_web_grok_image_annotation_enabled\":true,\"responsive_web_enhance_cards_enabled\":false}"
  "A default value for the GraphQL \"features\" parameter.  X changes the
required feature flags over time; when a request fails with a feature
error, refresh this from the browser and set :web-features in the config.")

(defun web-features ()
  (or (config :web-features) *default-web-features*))

(defun id< (a b)
  "Compares two decimal tweet id strings numerically (snowflake ids
have no leading zero, so length then lexicographic order suffices)."
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((/= (length a) (length b)) (< (length a) (length b)))
        (t (string< a b))))

;;; JSON accessors for YASON alist parsing (used by the :web backend,
;;; which parses objects as alists to preserve document order).

(defun json-object-p (node)
  "True when NODE is an alist representing a JSON object."
  (and (consp node) (consp (car node)) (stringp (car (car node)))))

(defun aget (node key)
  (when (json-object-p node)
    (cdr (assoc key node :test (function string=)))))

(defun json-walk (node function)
  "Calls FUNCTION on NODE and, recursively, on every sub-node
(object values and array elements), in document order."
  (funcall function node)
  (cond
    ((json-object-p node)
     (dolist (pair node) (json-walk (cdr pair) function)))
    ((consp node)
     (dolist (element node) (json-walk element function)))))

(defun tweet-result-screen-name (result)
  "Extracts the author screen name from a tweet result node, trying the
old (legacy) and new (core) user schemas."
  (let* ((user (aget (aget (aget result "core") "user_results") "result")))
    (or (aget (aget user "legacy") "screen_name")
        (aget (aget user "core") "screen_name"))))

(defun collect-web-tweets (json)
  "Walks the parsed GraphQL JSON and returns the list of tweet plists it
contains (in document order, i.e. newest first for a timeline),
de-duplicated by id."
  (let ((tweets '())
        (seen   (make-hash-table :test (function equal))))
    (json-walk
     json
     (lambda (node)
       (when (and (json-object-p node)
                  (equal (aget node "__typename") "Tweet")
                  (aget node "rest_id")
                  (aget node "legacy"))
         (let* ((id     (aget node "rest_id"))
                (legacy (aget node "legacy")))
           (unless (gethash id seen)
             (setf (gethash id seen) t)
             (push (list :id           id
                         :author       (tweet-result-screen-name node)
                         :conversation (aget legacy "conversation_id_str")
                         :created      (aget legacy "created_at")
                         :text         (or (aget legacy "full_text")
                                           (aget legacy "text")))
                   tweets))))))
    (nreverse tweets)))

(defun collect-bottom-cursor (json)
  "Returns the value of the Bottom cursor of the GraphQL response, or NIL."
  (let ((cursor nil))
    (json-walk
     json
     (lambda (node)
       (when (and (json-object-p node)
                  (equal (aget node "cursorType") "Bottom")
                  (aget node "value"))
         (setf cursor (aget node "value")))))
    cursor))

(defun web-get (url query-parameters)
  "Performs a GET request on URL with the web session credentials, and
returns the response JSON parsed by YASON as alists (objects) and lists
(arrays), preserving order.  QUERY-PARAMETERS is an alist of (name .
value) strings; drakma percent-encodes them (so the JSON variables and
features are encoded exactly once)."
  (let* ((credentials (web-credentials))
         (headers (list (cons "authorization" (concat "Bearer " *web-bearer*))
                        (cons "x-csrf-token"  (getf credentials :ct0))
                        (cons "x-twitter-active-user" "yes")
                        (cons "x-twitter-auth-type"   "OAuth2Session")
                        (cons "x-twitter-client-language" "en")
                        (cons "cookie" (format nil "auth_token=~A; ct0=~A"
                                               (getf credentials :auth-token)
                                               (getf credentials :ct0))))))
    (when (config :debug)
      (debug-log "web GET ~A" url)
      (dolist (parameter query-parameters)
        (debug-log "  ~A = ~A" (car parameter) (cdr parameter)))
      (debug-log "  auth_token: ~A  ct0: ~A"
                 (redact (getf credentials :auth-token))
                 (redact (getf credentials :ct0))))
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :get
                             :parameters query-parameters
                             :additional-headers headers
                             :user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:128.0) Gecko/20100101 Firefox/128.0"
                             :external-format-in :utf-8)
      (let ((text (if (stringp body)
                      body
                      (babel:octets-to-string body :encoding :utf-8))))
        (when (config :debug)
          (debug-log "  HTTP ~A, ~D bytes" status (length text))
          (debug-log "  response: ~A" text))
        (if (= 200 status)
            (let ((yason:*parse-object-as* :alist)
                  (yason:*parse-object-key-fn* (function identity)))
              (yason:parse text))
            (error 'api-error :status status :message text))))))

(defun web-home-variables (cursor)
  (format nil "{\"count\":~D,\"includePromotedContent\":false,\"latestControlAvailable\":true,\"requestContext\":\"launch\"~@[,\"cursor\":\"~A\"~]}"
          (min 100 (max 5 (config :page-size)))
          cursor))

(defun web-detail-variables (focal-id cursor)
  (format nil "{\"focalTweetId\":\"~A\",\"with_rux_injections\":false,\"includePromotedContent\":false,\"withCommunity\":true,\"withQuickPromoteEligibilityTweetFields\":false,\"withBirdwatchNotes\":false,\"withVoice\":false~@[,\"cursor\":\"~A\"~]}"
          focal-id cursor))

(defun fetch-new-tweets-web (since-id)
  "Fetches the Following (HomeLatestTimeline) tweets newer than SINCE-ID
using the web GraphQL API.  Returns two values: a list of tweet plists
sorted from oldest to newest, and the new since-id."
  (let* ((query-id (or (config :web-query-id-home)
                       (error "Set :web-query-id-home in the config (HomeLatestTimeline query id); see the readme.")))
         (url      (format nil "~A/~A/HomeLatestTimeline" (config :web-base) query-id))
         (all      '())
         (cursor   nil)
         (newest   since-id))
    (loop
      :repeat (max 1 (config :max-pages))
      :do (let* ((parameters (list (cons "variables" (web-home-variables cursor))
                                   (cons "features"  (web-features))))
                 (json   (web-get url parameters))
                 (tweets (collect-web-tweets json)))
            (setf all    (append all tweets)
                  cursor (collect-bottom-cursor json))
            ;; Stop paginating once we reach tweets we have already seen:
            (when (and since-id
                       (some (lambda (tweet) (not (id< since-id (getf tweet :id))))
                             tweets))
              (return)))
      :while cursor)
    (let ((fresh (if since-id
                     (remove-if-not (lambda (tweet) (id< since-id (getf tweet :id))) all)
                     all)))
      (dolist (tweet fresh)
        (when (id< newest (getf tweet :id))
          (setf newest (getf tweet :id))))
      ;; ALL is newest-first; reverse FRESH to get oldest-first:
      (values (reverse fresh) newest))))

(defun fetch-thread-tweets-web (conversation-id author after-id)
  "Fetches the self-replies of AUTHOR in the thread CONVERSATION-ID
newer than AFTER-ID using the web TweetDetail GraphQL API.  Returns a
list of tweet plists sorted from oldest to newest."
  (let* ((query-id (or (config :web-query-id-detail)
                       (error "Set :web-query-id-detail in the config (TweetDetail query id); see the readme.")))
         (url      (format nil "~A/~A/TweetDetail" (config :web-base) query-id))
         (all      '())
         (cursor   nil))
    (loop
      :repeat (max 1 (config :max-pages))
      :do (let* ((parameters (list (cons "variables" (web-detail-variables conversation-id cursor))
                                   (cons "features"  (web-features))))
                 (json (web-get url parameters)))
            (setf all    (append all (collect-web-tweets json))
                  cursor (collect-bottom-cursor json)))
      :while cursor)
    (let ((thread (remove-if-not
                   (lambda (tweet)
                     (and (equal (getf tweet :conversation) conversation-id)
                          (or (null author)
                              (equal (getf tweet :author) author))
                          (id< after-id (getf tweet :id))))
                   all)))
      (sort (copy-list thread) (function id<) :key (lambda (tweet) (getf tweet :id))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Backend dispatch.
;;;

(defun backend ()
  (or (config :backend) :api))

(defun timeline-fetch (state)
  "Fetches new timeline tweets.  Returns three values: a list of tweet
plists (oldest to newest), the new since-id, and the user id (or NIL)."
  (ecase (backend)
    (:api (let* ((credentials (load-credentials))
                 (user-id     (or (getf state :user-id)
                                  (fetch-user-id credentials))))
            (multiple-value-bind (tweets newest-id)
                (fetch-new-tweets credentials user-id (getf state :since-id))
              (values tweets newest-id user-id))))
    (:web (multiple-value-bind (tweets newest-id)
              (fetch-new-tweets-web (getf state :since-id))
            (values tweets newest-id (getf state :user-id))))))

(defun thread-fetch (last)
  "Fetches the thread of the LAST displayed tweet.  Returns a list of
tweet plists (oldest to newest)."
  (let ((conversation-id (or (getf last :conversation) (getf last :id))))
    (ecase (backend)
      (:api (fetch-thread-tweets (load-credentials) conversation-id
                                 (getf last :author) (getf last :id)))
      (:web (fetch-thread-tweets-web conversation-id
                                     (getf last :author) (getf last :id))))))


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
(defun load-thread-queue () (load-sexp (state-file "thread-queue.lisp") '()))
(defun save-thread-queue (queue) (save-sexp (state-file "thread-queue.lisp") queue))

(defun state-put (state key value)
  "Returns STATE with KEY set to VALUE."
  (let ((state (copy-list state)))
    (setf (getf state key) value)
    state))

(defun state-remove (state key)
  "Returns STATE without KEY."
  (let ((state (copy-list state)))
    (remf state key)
    state))

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
        (progn
          ;; Record the fetch time first, so that failures don't make
          ;; each shell prompt retry the API before the interval:
          (setf state (state-put state :last-fetch now))
          (multiple-value-bind (tweets newest-id user-id) (timeline-fetch state)
            (when user-id
              (setf state (state-put state :user-id user-id)))
            (when newest-id
              (setf state (state-put state :since-id newest-id)))
            (values (append queue tweets) state))))))

(defun display-tweet (tweet remaining &key with-author)
  "Prints TWEET then REMAINING on the last line.
When WITH-AUTHOR (thread reading), the author is printed on its own
first line, before the tweet text.  Otherwise, the author is prefixed
inline only when :show-author is set in the configuration."
  (when tweet
    (cond
      ((and with-author (getf tweet :author))
       (format t "@~A~%~A~%" (getf tweet :author) (getf tweet :text)))
      ((and (config :show-author) (getf tweet :author))
       (format t "@~A: ~A~%" (getf tweet :author) (getf tweet :text)))
      (t
       (format t "~A~%" (getf tweet :text)))))
  (format t "~D~%" remaining)
  (finish-output))

(defun next-tweet (&key (consume t))
  "Prints the next tweet (of the current thread when one was entered
with --enter, of the timeline otherwise) and the remaining count;
returns the exit status."
  (with-file-lock ()
    (let ((state (load-state)))
      (if (getf state :context)
          ;; Reading a thread:
          (let* ((queue (load-thread-queue))
                 (tweet (first queue)))
            (when tweet
              (setf state (state-put state :last-tweet tweet))
              (when consume
                (save-thread-queue (rest queue)))
              (save-state state))
            (unless tweet
              (format *error-output*
                      "~A: end of thread; use --leave to return to the timeline.~%"
                      *program-name*))
            (display-tweet tweet (length (rest queue)) :with-author t))
          ;; Reading the timeline:
          (multiple-value-bind (queue state) (refill-queue (load-queue) state)
            (let ((tweet (first queue)))
              (when tweet
                (setf state (state-put state :last-tweet tweet)))
              ;; Always save the queue: the refill may have fetched new
              ;; tweets, which must be kept even when not consuming:
              (save-queue (if (and tweet consume) (rest queue) queue))
              (save-state state)
              (display-tweet tweet (length (rest queue))))))))
  ex-ok)

(defun enter-thread ()
  "Enters the thread of the last displayed tweet: enqueues its author's
self-replies (newer than the displayed tweet) in the thread queue, and
switches the reading context to the thread.  Prints the number of
thread tweets enqueued.  When none is found, stays on the timeline and
prints 0."
  (with-file-lock ()
    (let* ((state (load-state))
           (last  (getf state :last-tweet)))
      (if (null last)
          (progn
            (format *error-output* "~A: no current tweet; read a tweet first.~%"
                    *program-name*)
            (format t "0~%"))
          (let* ((conversation-id (or (getf last :conversation) (getf last :id)))
                 (tweets          (thread-fetch last)))
            (if tweets
                (progn
                  (save-thread-queue tweets)
                  (save-state (state-put state :context
                                         (list :conversation-id conversation-id
                                               :author (getf last :author))))
                  (format t "~D~%" (length tweets)))
                (progn
                  (format *error-output* "~A: no thread found for this tweet~@[ (@~A)~].~%"
                          *program-name* (getf last :author))
                  (format t "0~%")))))))
  (finish-output)
  ex-ok)

(defun leave-thread ()
  "Returns to the main timeline; prints the number of tweets remaining
in the timeline queue."
  (with-file-lock ()
    (let ((state (load-state)))
      (when (getf state :context)
        (save-state (state-remove state :context)))
      (save-thread-queue '())
      (format t "~D~%" (length (load-queue)))))
  (finish-output)
  ex-ok)

(defun print-count ()
  "Prints the number of tweets remaining in the current context queue."
  (format t "~D~%" (length (if (getf (load-state) :context)
                               (load-thread-queue)
                               (load-queue))))
  (finish-output)
  ex-ok)

(defun print-status ()
  (let ((state (load-state)))
    (format t "configuration:   ~A~%" (namestring (configuration-pathname)))
    (format t "backend:         ~(~A~)~%" (backend))
    (format t "account:         ~A~%" (or (config :account) "(any)"))
    (format t "machine:         ~A~%" (config :machine))
    (format t "api-base:        ~A~%" (if (eq :web (backend))
                                          (config :web-base)
                                          (config :api-base)))
    (format t "state-directory: ~A~%" (namestring (state-directory)))
    (format t "queue length:    ~D~%" (length (load-queue)))
    (let ((context (getf state :context)))
      (if context
          (format t "context:         thread ~A of @~A (~D tweet~:P left)~%"
                  (getf context :conversation-id)
                  (getf context :author)
                  (length (load-thread-queue)))
          (format t "context:         timeline~%")))
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
;;; Options and main.
;;;

(defvar *action* :next
  "The action selected by the command-line options: one of :NEXT,
:PEEK, :COUNT, :STATUS, :ENTER, :LEAVE.")

(options "next-tweet"
         (option ("--peek") ()
                 "Print the next tweet and the remaining count, but without consuming the tweet."
                 (setf *action* :peek))
         (option ("--count") ()
                 "Print only the number of tweets remaining in the current context (no fetch, no consume)."
                 (setf *action* :count))
         (option ("--status") ()
                 "Print the configuration, reading context, queue lengths, since-id and last fetch time."
                 (setf *action* :status))
         (option ("--enter") ()
                 "Enter the thread of the last displayed tweet; the following calls read that thread."
                 (setf *action* :enter))
         (option ("--leave") ()
                 "Return to the main Following timeline (its queue and since-id are preserved)."
                 (setf *action* :leave))
         (option ("--debug") ()
                 "Dump the backend requests and responses to the standard error output."
                 (setf *configuration* (list* :debug t *configuration*)))
         (standard-options)
         (bash-completion-options))

(defun perform-action ()
  (ecase *action*
    (:next   (next-tweet))
    (:peek   (next-tweet :consume nil))
    (:count  (print-count))
    (:status (print-status))
    (:enter  (enter-thread))
    (:leave  (leave-thread))))

(defun main (arguments)
  (setf *action* :next)
  (handler-case
      (progn
        (load-configuration)
        (let ((status (parse-options *command* arguments)))
          (if (zerop status)
              (perform-action)
              status)))
    (api-error (err)
      ;; Print 0 on stdout so a shell prompt degrades gracefully:
      (perror "~A~%" err)
      (format t "0~%")
      (finish-output)
      ex-unavailable)
    (error (err)
      (perror "~A~%" err)
      (format t "0~%")
      (finish-output)
      ex-software)))

;;;; THE END ;;;;
