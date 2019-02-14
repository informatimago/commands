;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               news-to-mbox
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Converts a news job file as produced by suck(1) into a mbox file.
;;;;    news-to-mbox <file.news >file.mbox
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-10-02 <PJB> Created.
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

(defvar *log* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *ascii-characters*
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")


  (defun ascii-code (character)
    (if (char= #\newline character)
        10
        (let ((pos (position character *ascii-characters*)))
          (if pos
              (+ 32 pos)
              (error "~C is not a character encodable into ASCII" character)))))


  (defun code-ascii (code)
    (cond
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

(defstruct message headers body)

(defun clean-tabs (line)
  (and line (substitute 32 9 line)))

(defun news-read-message (stream &optional (eof-error-p t) eof-value)
  "
DO:     Read a news message from the stream.
        Messages don't begin with a '^From ' line, but end with a single
        dot on a line.
        The numberof body lines is taken from the Lines: header, otherwise
        the body stops at the first line containing a single dot.
        (true single dot lines in the body are not escaped).

RETURN: If the stream is at end-of-file, then raise an END-OF-FILE condition
        if EOF-ERROR-P is true, or return the EOF-VALUE, otherwise return a
        MESSAGE object filled with the headers and body read.
"
  (flet ((parse-header (cline)
           (or (ignore-errors
                 (let* ((aline (ascii-decode cline))
                        (colon (position #\: aline)))
                   (unless colon
                     (error "Invalid header line ~S, missing a colon."
                            aline))
                   (let* ((key    (string-right-trim ; there's no space on
                                   " " (subseq aline 0 colon))) ; the left.
                          (value  (subseq aline (1+ colon))))
                     (list (intern (string-upcase key) "KEYWORD")
                           (string-trim " " value) cline))))
               (let ((colon (position (ascii-code #\:) cline)))
                 (unless colon
                   (error "Invalid header line ~S, missing a colon."
                          cline))
                 (let* ((key    (subseq cline 0 colon))
                        (akey   (string-right-trim ; there's no space on
                                 " " (ascii-decode key))) ; the left.
                        (value  (subseq cline (1+ colon))))
                   (list (intern (string-upcase akey) "KEYWORD")
                         value cline))))))
    (let* ((headers
            (loop
               :with headers = '()
               :with cline = ""
               :for line = (clean-tabs (byte-read-line stream nil nil))
               :while (and line (not (equalp line #())))
               :do (cond
                     ((= (aref line 0) 32)
                      (setf cline (concatenate 'vector cline #(10) line)))
                     ((zerop (length cline))
                      (setf cline line))
                     (t
                      (push (parse-header cline) headers)
                      (setf cline line)))
               :finally (if (and (null line)
                                 (null headers)
                                 (zerop (length cline)))
                            (if eof-error-p
                                (error 'end-of-file :stream stream)
                                (return-from news-read-message eof-value))
                            (progn (unless (zerop (length cline))
                                     (push (parse-header cline) headers))
                                   (return (nreverse headers))))))
           (lines (assoc :lines headers))
           (lines (when lines (parse-integer (second lines))))
           (body
             (loop
                :repeat (or lines 0)
                :collect (byte-read-line stream nil nil) :into body
                :finally (return
                           (nconc
                            body
                            (loop
                               :for line = (byte-read-line stream nil nil)
                               :while (and line
                                           (not (equalp line
                                                        #.(ascii-encode "."))))
                               :collect line))))))
      (make-message :headers headers :body body))))

(defun mbox-write-message (stream message)
  "
DO:     Write to the STREAM a mbox message: a first '^From ' line, followed
        by the header and the body of the message. If any line of the body
        starts with 'From ' or is a single dot, it's escaped with #\>.
RETURN: MESSAGE
"
  (multiple-value-bind (se mi ho da mo ye dw)
      (decode-universal-time (get-universal-time)) ; TODO: get it from the Date: header.
    (let ((from (format nil "~%From ~A  ~3A ~3A ~2D ~2D:~2,'0D:~2,'0D ~4,'0D~%"
                        "user@example.com" ; TODO get it from the From: header.
                        (aref #("Mon" "Tue" "Wed" "Thi" "Fri" "Sat" "Sun") dw)
                        (aref #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
                                "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") mo)
                        da ho mi se ye)))
      (write-sequence (ascii-encode from) stream)
      (dolist (h (message-headers message))
        (write-sequence (third h) stream)
        (write-sequence #(10) stream))
      (write-sequence #(10) stream)
      (dolist (l (message-body message))
        (write-sequence l stream)
        (write-sequence #(10) stream))
      message)))

(defun parse-arguments (arguments)
  "
RETURN: A JOB structure filled with the data given as ARGUMMENTS.
"
  (loop
    ;; :with job = (make-job)
    :for (k) :on arguments :by (function cddr)
    :do (scase k
               (("-h") (error "Invalid option ~A" k))
               ;; (("-t") (setf (job-file     job) v))
               ;; (("-s") (setf (job-server   job) v))
               ;; (("-P") (setf (job-port     job) (parse-integer v)))
               ;; (("-u") (setf (job-user     job) v))
               ;; (("-p") (setf (job-password job) v))
               (otherwise (error "Invalid option ~A" k)))
    :finally (return nil #| job |#)))

(defun news-to-mbox (news mbox)
  (loop
     :for m = (news-read-message news nil nil)
     :while m
     :do (progn
           (ignore-errors
             (princ (ascii-decode (third (assoc :from    (message-headers m)))))
             (terpri))
           (ignore-errors
             (princ (ascii-decode (third (assoc :subject (message-headers m)))))
             (terpri))
           (ignore-errors
             (princ (ascii-decode (third (assoc :date (message-headers m)))))
             (terpri))
           (terpri))
     :do (mbox-write-message mbox m)))

;; (let ((mbox-path "/home/pjb/cll-20070927T162100.mbox")
;;       (news-path "/home/pjb/test.news")
;;       (news-path "/home/pjb/cll-20070927T162100")
;;       )
;;   (with-open-file (mbox mbox-path
;;                         :element-type '(unsigned-byte 8)
;;                         :direction :output
;;                         :if-does-not-exist :create
;;                         :if-exists :supersede)
;;     (with-open-file (news news-path
;;                           :element-type '(unsigned-byte 8))
;;       #- (and)
;;       (news-read-message news)



(defun main (arguments)
  (declare (ignore arguments))
  #+clisp
  (setf (stream-element-type *standard-input*)  '(unsigned-byte 8)
        (stream-element-type *standard-output*) '(unsigned-byte 8))
  (news-to-mbox *standard-input* *standard-output*)
  ex-ok)

;;;; THE END ;;;;
