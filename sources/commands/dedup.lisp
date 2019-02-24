;; -*- mode:lisp;coding:utf-8 -*-

(command :use-systems (:com.informatimago.common-lisp.cesarum)
         :main "COM.INFORMATIMAGO.COMMAND.DEDUP:MAIN")

(defpackage "COM.INFORMATIMAGO.COMMAND.DEDUP"
  (:use "COMMON-LISP"
        "SCRIPT"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "MAIN"))
(in-package "COM.INFORMATIMAGO.COMMAND.DEDUP")

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
              (declare (ignore k))
              (mapc 'delete-file (cdr vs)))
            table))
  ex-ok)

;;;; THE END ;;;;
