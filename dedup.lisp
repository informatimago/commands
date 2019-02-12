#!/usr/local/bin/cl
;; -*- mode:lisp;coding:utf-8 -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load #P"~/quicklisp/setup.lisp"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :com.informatimago.common-lisp.cesarum)
  (use-package :com.informatimago.common-lisp.cesarum.stream)
  (use-package :com.informatimago.common-lisp.cesarum.utility))


(defun main (arguments)
  (declare (ignore arguments))
 (let* ((lines (stream-to-string-list *standard-input*))
        (table (make-hash-table :test 'equal)))
   (loop :for (k v)
           :in (mapcar (lambda (line)
                         (let ((p (search "  " line)))
                           (list (subseq line 0 p)
                                 (subseq line (+ 2 p)))))
                       lines)
         :do (push v (gethash k table '())))
   (maphash (lambda (k vs)
              (unless (cdr vs)
                (remhash k table)))
            table)
   (maphash (lambda (k vs)
              (mapc 'delete-file (cdr vs)))
            table))
  0)


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
