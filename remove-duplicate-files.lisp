#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-

;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP")
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))

(if (null ext:*args*)
    (error "Usage: ~A directory~%" (file-namestring *load-pathname*))
    (dolist (*base* ext:*args*)
      (when (and (stringp *base*)
                 (char/= #\/ (char *base* (1- (length *base*)))))
        (setf *base* (format nil "~A/" *base*)))
      (dolist (dir (directory
                    (make-pathname
                     :directory (append (pathname-directory *base*) '(:wild))
                     :defaults *base*)))
        (format *trace-output* "Processing ~A~%" dir)
        (let ((sumtab (make-hash-table)))
          (format *trace-output* "  Checksumming...~%")
          (with-open-stream (sums (ext:run-shell-command
                                   (format nil "md5sum ~A"
                                           (namestring
                                            (make-pathname :name :wild
                                                           :type :wild
                                                           :defaults dir)))
                                   :input nil :output :stream))
            (loop for (sum file) = (list (read sums nil nil)
                                         (read-line sums nil nil))
               while sum do (push (string-trim " " file) (gethash sum sumtab))))
          (format *trace-output* "  Deleting doubles...~%")
          (maphash (lambda (k v)
                     (when (< 1 (length v))
                       (pop v)
                       (dolist (f v) (delete-file f))))  sumtab)))))


(ext:exit 0)
