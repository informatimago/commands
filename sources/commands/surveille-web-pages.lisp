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


(command :use-systems (:babel))

(defun make-pipe-input-stream  (command &key (external-format :default)
                                          (element-type 'character))
  (declare (ignore command external-format element-type))
  (error "Not implemented yet."))

(defun make-pipe-output-stream (command &key (external-format :default)
                                          (element-type 'character))
  (declare (ignore command external-format element-type))
  (error "Not implemented yet."))


(defun ensure-list (item)  (if (listp item) item (list item)))

(defconstant +code+
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
  (unwind-protect
       (progn
         #+clisp (setf custom:*default-file-encoding* *encoding-iso-8859-1*)
         (multiple-value-bind (io in out) (uiop:run-program
                                           (list "file" "-ib" "-")
                                           :input :stream
                                           :output :stream
                                           :wait nil)
           (close io)
           (ignore-errors
            (write-sequence (babel:octets-to-string data :encoding *encoding-iso-8859-1*) out))
           (close out)
           (prog1 (read-line in) (close in))))))

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
     (md5:md5sum-sequence
      (coerce (babel:octets-to-string
               (apply (function concatenate) 'string  (resource-data resource))
               :encoding :utf-8)
              '(simple-array (unsigned-byte 8) (*)))))
    ((:data)
     (md5:md5sum-sequence
      (coerce (resource-data resource)
              '(simple-array (unsigned-byte 8) (*)))))))

(defun get-resource (resource)
  (setf (resource-data resource)
        (ecase  (resource-kind resource)
          ((:page)
           (with-open-stream (in (make-pipe-input-stream
                                  (format nil "/usr/local/bin/lynx -dump ~S"
                                          (resource-uri resource))))
             (loop for line = (read-line in nil nil)
                while line
                collect line)))
          ((:data)
           (with-open-stream  (in (make-pipe-input-stream
                                   (format nil "/usr/local/bin/wget ~S -o /dev/null -O /dev/stdout"
                                           (resource-uri resource))
                                   :element-type '(unsigned-byte 8)))
             (loop
                :with buffer = (make-array 1 :adjustable t :fill-pointer 0)
                :for byte = (read-byte in nil nil)
                :while byte
                :do (vector-push-extend byte buffer)
                :finally (return buffer)))))))

(defun send-notice (task changes)
  (with-open-stream (msg (make-pipe-output-stream
                          (format nil "/usr/sbin/sendmail ~{~S ~}"
                                  (mapcar (function recipient-address)
                                          (task-recipients task)))
                          :external-format *encoding-utf-8*))
    (let ((boundary
           (multiple-value-bind (se mi ho da mo ye)
               (decode-universal-time (get-universal-time))
             (format nil "~A-~8,'0X-~4,'0D~2,'0D~2,'0D~2,'0D~2,'0D~2,'0D"
                     (with-open-stream
                         (in (uiop:run-program "hostname -f"
                                               :output :stream
                                               :wait nil
                                               :force-shell t))
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
      (format msg "~2%--~A--~%" boundary))))

(defun main (args)
  (let ((verbosep (member "-v" args :test (function string=)))
        (tasks (load-tasks +task-file+)))
    ;; The probability for two users to check the same page is low.
    (dolist (task tasks)
      (let ((changes '()))
        (dolist (resource (task-resources task))
          (get-resource resource)
          (let ((new-checksum (compute-checksum resource)))
            (unless (equalp new-checksum (resource-checksum resource))
              (setf (resource-checksum resource) new-checksum)
              (push resource changes))))
        (when verbosep
          (format t "~&~A: got ~A changes:~{~%    ~A~}~%"
                  (task-title task)
                  (length changes)
                  (mapcar (function resource-title) changes)))
        (when changes
          (send-notice task (nreverse changes)))))
    (save-tasks +task-file+ tasks))
  ex-ok)

;;;; THE END ;;;;
