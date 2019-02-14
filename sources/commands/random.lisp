;;;; -*- mode:lisp; coding:utf-8 -*-

(defun random-elt (list)
  (elt list (random (length list))))

(defun main (arguments)
  (setf *random-state* (make-random-state t))
  (cond ((null arguments)
         (prin1 (random #x100000000)))
        ((and (null (rest arguments))
              (ignore-errors (realp (let ((*read-eval* nil))
                                      (read-from-string (first arguments))))))
         (prin1 (random (let ((*read-eval* nil))
                          (read-from-string (first arguments))))))
        (t
         (princ (random-elt arguments))))
  (terpri)
  (finish-output)
  ex-ok)

;;;; THE END ;;;;
