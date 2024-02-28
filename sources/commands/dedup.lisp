;; -*- mode:lisp;coding:utf-8 -*-
(in-package "SCRIPT")

(command :use-systems (:com.informatimago.common-lisp.cesarum)
         :use-packages ("COMMON-LISP"
                        "SCRIPT"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
         :main "COM.INFORMATIMAGO.COMMAND.DEDUP:MAIN")


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
