;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               fetch-pop
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A POP-3 downloader for emacs.
;;;;    emacs 22.1 can't parse correctly unix mailboxes.  In 2007!
;;;;    So we download our POP-3 messages and write a unix mailbox as
;;;;    emacs 22.1 expects them...
;;;;
;;;;    Options:
;;;;
;;;;        -t file
;;;;              The name of the file the mail is to be moved to.  This
;;;;              must always be included in this string.
;;;;
;;;;        -s server
;;;;              The name of the server.
;;;;
;;;;        -P port
;;;;              The port number of the server.
;;;;
;;;;        -u user
;;;;              The user name to use.
;;;;
;;;;        -p password
;;;;              The password to use.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-06-21 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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
;;;;**************************************************************************

(command :use-systems (:md5 :usocket))

(defun socket-connect (port server &key (element-type 'character) (timeout 60))
  (usocket:socket-connect port server :element-type element-type :timeout timeout))

(defun socket-status (socket)
  (usocket:socket-state socket))

;;;---------------------------------------------------------------------

(defmacro scase (keyform &rest clauses)
  "
DO:         A CASE, but for string keys. That is, it uses STRING= as test
            insteand of the ''being the same'' test.
"
  (let ((key (gensym "KEY")))
    `(let ((,key ,keyform))
       (cond
         ,@(mapcar (lambda (clause)
                     (if (or (eq (car clause) 'otherwise) (eq (car clause) 't))
                         `(t ,@(cdr clause))
                         `((member ,key ',(car clause) :test (function string=))
                           ,@(cdr clause))))
                   clauses)))))

(defun apop-encode (stamp password)
  (format nil "~(~{~2,'0X~}~)"
          (coerce
           (md5:md5sum-sequence
            (coerce
             (map 'vector (function char-code)
                  (concatenate 'string stamp password))
             '(SIMPLE-ARRAY (UNSIGNED-BYTE 8))))
           'list)))

(defvar *log* nil)

(defun logger (ctrlstr &rest args)
  (when *log*
    (apply (function format) *log* ctrlstr args)
    (terpri *log*)
    (force-output *log*)))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *ascii-characters*
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

  (defun ascii-code (character)
    (cond
      ((char= #\newline character) 10)
      ((char= #\tab character) 32)
      (t (let ((pos (position character *ascii-characters*)))
           (if pos
               (+ 32 pos)
               (error "~C is not a character encodable into ASCII" character))))))

  (defun code-ascii (code)
    (cond
      ((= 9 code) #\space)
      ((= 10 code) #\newline)
      ((<= 32 code 126) (aref *ascii-characters* (- code 32)))
      (t (error "~D is not the ASCII code of any character." code))))

  (defun ascii-decode (bytes)
    (loop
      :with string = (make-string (- (length bytes) (count 13 bytes)))
      :with i = -1
      :for code :across bytes
      :unless (= 13 code)
        :do (setf (aref string (incf i))  (code-ascii code))
      :finally (return string)))

  (defun ascii-encode (string)
    (loop
      :with bytes = (make-array (+ (length string) (count #\newline string))
                                :element-type '(unsigned-byte 8)
                                :initial-element 0)
      :with i = -1
      :for ch :across string
      :do (if (char= #\newline ch)
              (setf (aref bytes (incf i)) 13
                    (aref bytes (incf i)) 10)
              (setf (aref bytes (incf i)) (ascii-code ch)))
      :finally (return bytes))))

(defun ascii-read-line (stream &optional (eof-error-p t) eof-value)
  (loop
     :with buffer = (make-array 128 :element-type (stream-element-type stream)
                                :adjustable t :fill-pointer 0 :initial-element 0)
     :for byte = (read-byte stream nil nil)
     :while (and byte (/= 10 byte))
     :do (vector-push-extend byte buffer (array-dimension buffer 0))
     :finally (cond
                (byte (when (and (plusp (fill-pointer buffer))
                                 (= 13 (aref buffer (1- (fill-pointer buffer)))))
                        (decf (fill-pointer buffer)))
                      (return (ascii-decode buffer)))
                (eof-error-p
                 (error 'end-of-file :stream stream))
                (t (return eof-value)))))

(defun ascii-format (stream control-string &rest arguments)
  (write-sequence (ascii-encode (apply (function format)
                                       nil control-string arguments))
                  stream))

(defun byte-read-line (stream &optional (eof-error-p t) eof-value)
  (loop
     :with buffer = (make-array 128 :element-type (stream-element-type stream)
                                :adjustable t :fill-pointer 0 :initial-element 0)
     :for byte = (read-byte stream nil nil)
     :while (and byte (/= 10 byte))
     :do (vector-push-extend byte buffer (array-dimension buffer 0))
     :finally (cond
                (byte
                 (when (and (plusp (fill-pointer buffer))
                            (= 13 (aref buffer (1- (fill-pointer buffer)))))
                   (decf (fill-pointer buffer)))
                 (return buffer))
                (eof-error-p
                 (error 'end-of-file :stream stream))
                (t
                 (return eof-value)))))

(defun byte-write-line (line stream)
  (write-sequence line stream)
  (write-byte 10 stream))



#-(and)
(progn
  (with-open-file (out "/tmp/test"
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (ascii-format out "Testing an ascii file written in binary~%")
    (write-sequence #(0 1 2 3 4 5 6 7 8 9) out)
    (ascii-format out "Second line~%"))

  (with-open-file (in  "/tmp/test"
                       :element-type '(unsigned-byte 8)
                       :direction :input
                       :if-does-not-exist :error)
    (print (ascii-read-line in nil nil))
    (let ((buffer (make-array 10 :element-type (stream-element-type in)
                                 :initial-element 0)))
      (print (read-sequence buffer in))
      (print buffer))
    (print (ascii-read-line in nil nil)))
  (values))


(defun positive-response-p (response)
  (and response
       (plusp (length response))
       (char= #\+ (aref response 0))))

(defun read-positive-response (server-socket)
  (let ((answer (ascii-read-line server-socket nil nil)))
    (unless (positive-response-p answer)
      (logger "Negative reponse ~S. Aborting." answer))
    (and (positive-response-p answer) answer)))

(defun pop-log-in (server-socket user password)
  (let ((headline (ascii-read-line server-socket nil nil)))
    (logger "Got head line ~S" headline)
    (unless (positive-response-p headline)
      (logger "Bad status. Aborting.")
      (return-from pop-log-in nil))
    (if (position #\< headline)
      (let* ((stamp (subseq headline
                            (position #\< headline)
                            (1+ (position #\> headline))))
             (auth (apop-encode stamp password)))
        (logger "Stamp is: ~A" stamp)
        (logger "Sending APOP ~A ~A" user auth)
        (ascii-format server-socket "APOP ~A ~A~%"
                      user (apop-encode stamp password)))
      (progn
        (ascii-format server-socket "USER ~A~%" user)
        (let ((answer (ascii-read-line server-socket nil nil)))
          (unless (positive-response-p headline)
            (logger "Bad user ~S. Aborting." answer)
            (return-from pop-log-in nil)))
        (ascii-format server-socket "PASS ~A~%" password)))
    (let ((answer (ascii-read-line server-socket nil nil)))
      (unless (positive-response-p headline)
        (logger "Bad password ~S. Aborting." answer))
      (positive-response-p headline))))

(defun copy-bytes (in out size)
  (loop
     :with buffer := (make-array 4096
                                :element-type (stream-element-type in)
                                :initial-element 0)
     :while (plusp size)
     :do (let ((bsize (min size (array-dimension buffer 0))))
           (read-sequence buffer in :end bsize)
           (write-sequence buffer out :end bsize)
           (decf size bsize))))

(defun download-messages (server-socket mailbox-stream delete-messages-p)
  (ascii-format server-socket "STAT~%")
  (loop
    :for i :from 1 :to (let ((answer (ascii-read-line server-socket nil nil)))
                         (when (or (null answer) (char/= #\+ (aref answer 0)))
                           (logger "Bad status. Aborting.")
                           (return-from download-messages nil))
                         (let ((nmesg (parse-integer
                                       answer
                                       :start (position #\space answer)
                                       :junk-allowed t)))
                           (logger "~D messages" nmesg)
                           nmesg))
    :do (logger "Retrieving message ~D" i)
        (ascii-format server-socket "RETR ~A~%" i)
        (unless (read-positive-response server-socket)
          (return-from download-messages nil))
        (loop
          :for first := t :then nil
          :for line := (byte-read-line server-socket nil nil)
          :while (and line (not (equalp #(46) line)))
          :do (byte-write-line
               (cond
                 (first
                  (when (or (< (length line) 6)
                            (not (equalp (subseq line 0 (min (length line) 5))
                                         #.(ascii-encode "From "))))
                    (byte-write-line
                     (multiple-value-bind (se mi ho da mo ye dow)
                         (decode-universal-time (get-universal-time))
                       (ascii-encode
                        (format nil
                                "From ~A  ~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~4,'0D~%"
                                "postmaster@localhost"
                                (aref #("Mon" "Thi" "Wed" "Tue" "Fri" "Sat" "Sun") dow)
                                (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                                      (1- mo))
                                da ho mi se  ye)))
                     mailbox-stream))
                  line)
                 ((and (<= 2 (length line))
                       (= 46 (aref line 0) (aref line 1)))
                  (subseq line 1))
                 ((equalp (subseq line 0 (min (length line) 5))
                          #.(ascii-encode "From "))
                  (concatenate 'vector #(#.(ascii-code #\>)) line))
                 (t  line))
               mailbox-stream)
          :finally (byte-write-line #() mailbox-stream)
                   (finish-output mailbox-stream))
        (when delete-messages-p
          (ascii-format server-socket "DELE ~A~%" i)
          (ascii-read-line server-socket nil nil))))

(defun pop-session (server-socket mailbox-stream
                    user password delete-messages-p)
  (unwind-protect
      (and (pop-log-in server-socket user password)
           (download-messages server-socket mailbox-stream delete-messages-p))
    (ascii-format server-socket "QUIT~%")
    (finish-output server-socket)
    (sleep 1)))

(defstruct job
  file
  server
  port
  user
  password)

(defun fetch-pop (job &optional delete-messages-p)
  (logger "Connecting to server ~A port ~A" (job-server job) (job-port job))
  (with-open-stream (socket (socket-connect
                             (job-port job) (job-server job)
                             :element-type '(unsigned-byte 8)
                             :timeout 30))
    (when (member (socket-status socket) '(nil :eof))
      (logger "No connection. Aborting.")
      (return-from fetch-pop))
    (logger "Connected.")
    (with-open-file (mailbox (job-file job)
                     :element-type '(unsigned-byte 8)
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists :append)
      (when (plusp (file-length mailbox))
        (ascii-format mailbox "~%"))
      (pop-session socket mailbox (job-user job) (job-password job)
                   delete-messages-p))))

(defun parse-arguments (arguments)
  "
RETURN: A JOB structure filled with the data given as ARGUMMENTS.
"
  (loop
     :with job = (make-job)
     :for (k v) :on arguments :by (function cddr)
     :do (scase k
           (("-t") (setf (job-file     job) v))
           (("-s") (setf (job-server   job) v))
           (("-P") (setf (job-port     job) (parse-integer v)))
           (("-u") (setf (job-user     job) v))
           (("-p") (setf (job-password job) v))
           (otherwise (error "Invalid option ~A" k)))
     :finally (return job)))

(defun main (arguments)
  (when arguments
    (with-open-file (*log* "/tmp/fetch-pop.log"
                           :direction :output
                           :if-exists :append
                           :if-does-not-exist :create)
      (multiple-value-bind (se mi ho da mo ye)
          (decode-universal-time (get-universal-time))
        (logger "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D ~S"
                ye mo da ho mi se arguments))
      (fetch-pop (parse-arguments arguments) 'delete-messages)))
  ex-ok)

;;;; THE END ;;;;


