;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               surveille-web-page
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Send email notifications when the checksum on web pages change.
;;;;
;;;;    Put a line such as:
;;;;    * * */1 * * /home/pjb/bin/surveille-web-pages
;;;;    in your crontab.
;;;;
;;;;    ~/surveille-web.data contains something like:
;;;;
;;;;    (("pjb@informatimago.com" "Pascal Bourguignon"
;;;;      (("http://franz.com/careers/jobs/outside/" "Lisp Jobs"
;;;;        "8b036c81f55d61c6f3e22c1200229111  -")
;;;;       ("http://www.informatimago.com/develop/lisp" "Lisp Packages"
;;;;        "00000000000000000000000000000000  -")))
;;;;     ("ordimagic@yahoo.com" "Lacsap Bourguignon"
;;;;      (("http://www.listentome.net/movies8.php" "Listen to Me"
;;;;        "b830c6185f561dcf6e32c12022290111  -")))
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-17 <PJB> Corrected a bug.
;;;;    2005-01-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2005 - 2005
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

(command :use-systems (:babel :md5)
         :version "1.0.1"
         :documentation "
Send email notifications when the checksum of watched web pages changes.

For each task in ~/SURVEILLE-WEB.DATA, fetch its resources, compare their md5
checksums against the stored ones, and email a MIME notice to the task's
recipients listing the resources whose content changed.  Meant to be run from
cron; takes no arguments.")

(defun ensure-list (item)  (if (listp item) item (list item)))

(defparameter +code+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")
(defconstant +pad+ 64)

(defparameter *encoding-iso-8859-1*
  #+clisp (ext:make-encoding
           :charset charset:iso-8859-1
           :line-terminator :unix)
  #-clisp :iso-8859-1)

(defparameter *encoding-utf-8*
  #+clisp (ext:make-encoding
           :charset charset:utf-8
           :line-terminator :unix)
  #-clisp :utf-8)

(defun data-mime-type (data)
  "Return the MIME type of the octet vector DATA, as reported by file(1).
DATA is fed to `file -ib -' on stdin (preserved byte for byte through an
ISO-8859-1 round trip)."
  (string-trim '(#\Space #\Newline #\Return)
               (uiop:run-program (list "file" "-ib" "-")
                                 :input (make-string-input-stream
                                         (babel:octets-to-string data :encoding *encoding-iso-8859-1*))
                                 :output :string
                                 :external-format *encoding-iso-8859-1*
                                 :ignore-error-status t)))

(defun write-base64-sequence (sequence
                              &optional (*standard-output* *standard-output*))
  ;; aaaaaa aa bbbb bbbb cc cccccc
  ;; uuuuuu dd dddd tttt tt qqqqqq
  (let ((index 0))
    (flet ((get-byte ()
             (if (< index (length sequence))
                 (prog1 (aref sequence index) (incf index))
                 nil)))
      (loop
         :with i = 0
         :for a = (get-byte)
         :for b = (get-byte)
         :for c = (get-byte)
         :while a
         :do (let ((un (truncate a 4))
                   de tr qu)
               (cond
                 ((null b)
                  (setf de (mod (* a 16) 64)
                        tr +pad+
                        qu +pad+))
                 ((null c)
                  (setf de (mod (+ (* a 16) (truncate b 16)) 64)
                        tr (mod (* b 4) 64)
                        qu +pad+))
                 (t
                  (setf de (mod (+ (* a 16) (truncate b 16)) 64)
                        tr (mod (+ (* b 4) (truncate c 64)) 64)
                        qu (mod c 64))))
               (format t "~C~C~C~C"
                       (aref +code+ un) (aref +code+ de)
                       (aref +code+ tr) (aref +code+ qu))
               (setf i (mod (1+ i) 18))
               (when (zerop i) (format t "~%")))))))

(defstruct (recipient (:type list)) address name)
(defstruct (task      (:type list)) title sender recipients resources)
(defstruct (resource  (:type list)) kind uri title checksum data)

(defparameter +task-file+ (make-pathname :case :common
                                         :name "SURVEILLE-WEB"
                                         :type "DATA"
                                         :defaults (user-homedir-pathname)))

(defun load-tasks (file)
  (let ((tasks (with-open-file
                   (in file :direction :input :if-does-not-exist :error)
                 (read in)))
        (minlen (length (make-resource))))
    (dolist (task tasks)
      (dolist (resource (task-resources task))
        (when (< (length resource) minlen)
          (nconc resource (make-list (- minlen (length resource))
                                     :initial-element nil)))))
    tasks))

(defun save-tasks (file tasks)
  (dolist (task tasks)
    (dolist (resource (task-resources task))
      (setf (resource-data resource) nil)))
  (with-open-file (out file :direction :output :if-does-not-exist :create
                       :if-exists :supersede)
    (print tasks out)(terpri out)))

(defun compute-checksum (resource)
  (ecase (resource-kind resource)
    ((:page)
     ;; RESOURCE-DATA is the list of text lines; join them and md5 their
     ;; UTF-8 octets.
     (md5:md5sum-sequence
      (babel:string-to-octets
       (apply (function concatenate) 'string (resource-data resource))
       :encoding :utf-8)))
    ((:data)
     (md5:md5sum-sequence
      (coerce (resource-data resource)
              '(simple-array (unsigned-byte 8) (*)))))))

(defun get-resource (resource)
  ;; TODO: use drakma?
  (setf (resource-data resource)
        (ecase  (resource-kind resource)
          ((:page)
           ;; lynx -dump renders the page to text; collect it line by line.
           (with-input-from-string
               (in (uiop:run-program (list "lynx" "-dump" (resource-uri resource))
                                     :output :string
                                     :ignore-error-status t))
             (loop :for line = (read-line in nil nil)
                   :while line
                   :collect line)))
          ((:data)
           ;; wget the raw bytes to stdout; capture them through an ISO-8859-1
           ;; round trip (each octet <-> one character) so the data is exact.
           (let ((latin1 (uiop:run-program (list "wget" (resource-uri resource)
                                                 "-q" "-O" "-")
                                           :output :string
                                           :external-format *encoding-iso-8859-1*
                                           :ignore-error-status t)))
             (map '(simple-array (unsigned-byte 8) (*))
                  (function char-code) latin1))))))

(defun send-notice (task changes)
  (let ((message
          (with-output-to-string (msg)
            (let ((boundary
           (multiple-value-bind (se mi ho da mo ye)
               (decode-universal-time (get-universal-time))
             (format nil "~A-~8,'0X-~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
                     (with-input-from-string
                         (in (uiop:run-program "hostname -f"
                                               :output :string
                                               :force-shell t
                                               :ignore-error-status t))
                       (read-line in))
                     (random #.(expt 2 32))
                     ye mo da ho mi se))))
      (format msg "MIME-Version: 1.0~%")
      (format msg "Content-Language: en~%")
      (format msg "Content-Type: multipart/mixed; boundary=\"~A\"~%" boundary)
      (format msg "Subject: ~A~%"  (task-title task))
      (format msg "From: <~A>~%"  (task-sender task))
      (format msg "Reply-To: <~A>~%"  (task-sender task))
      (format msg "Error-To: <~A>~%"  "pjb@informatimago.com")
      (format msg "~2%--~A~%" boundary)
      (format msg "Content-Type: text/plain; charset=utf-8~%")
      (format msg "Content-Disposition: inline~%")
      (format msg "~%")
      (format msg "The following web page~P have changed:~2%~
                 ~:{    - ~*~A~%~
                 ~&      ~A~2%~}~%"
              (length changes)
              changes)
      (dolist (resource changes)
        (when (eq :page (resource-kind resource))
          ;; (format *trace-output* "changed: ~A~%" (resource-uri page))
          (format msg "~&--------------------~%")
          (format msg "~{~A~%~}" (resource-data resource))))
      (dolist (resource changes)
        (when (eq :data (resource-kind resource))
          (format msg "~2%--~A~%" boundary)
          (format msg "Content-Transfer-Encoding: base64~%")
          (format msg "Content-Type: ~A~%"
                  (data-mime-type (resource-data resource)))
          (format msg "Content-Disposition: inline~%")
          (format msg "~%")
          (write-base64-sequence (resource-data resource) msg)
          (format msg "~%--~A~%" boundary)
          (format msg "Content-Type: text/plain; charset=utf-8~%")
          (format msg "Content-Disposition: inline~%")
          (format msg "~%~A~2%" (resource-title resource))))
      (format msg "~2%--~A--~%" boundary)))))
    (uiop:run-program (list* "/usr/sbin/sendmail"
                             (mapcar (function recipient-address)
                                     (task-recipients task)))
                      :input (make-string-input-stream message)
                      :external-format *encoding-utf-8*
                      :ignore-error-status t)))

(options "surveille-web-pages" (standard-options))

(defun main (arguments)
  (parse-options *command* arguments)
  (unless (probe-file +task-file+)
    (format *error-output* "~A: no task file ~A~%"
            *program-name* (namestring +task-file+))
    (return-from main ex-noinput))
  (let ((tasks (load-tasks +task-file+)))
    ;; The probability for two users to check the same page is low.
    (dolist (task tasks)
      (let ((changes '()))
        (dolist (resource (task-resources task))
          (get-resource resource)
          (let ((new-checksum (compute-checksum resource)))
            (unless (equalp new-checksum (resource-checksum resource))
              (setf (resource-checksum resource) new-checksum)
              (push resource changes))))
        (when *verbose*
          (format t "~&~A: got ~A changes:~{~%    ~A~}~%"
                  (task-title task)
                  (length changes)
                  (mapcar (function resource-title) changes)))
        (when changes
          (send-notice task (nreverse changes)))))
    (save-tasks +task-file+ tasks))
  ex-ok)

;;;; THE END ;;;;
