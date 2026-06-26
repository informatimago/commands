;;;; -*- mode:lisp; coding:utf-8 -*-

(defun random-elt (list)
  (elt list (random (length list))))

(options "random" (standard-options))

(defun main (arguments)
  (let ((operands '()))
    (parse-options *command* arguments nil
                   (lambda (arg rest) (push arg operands) rest))
    (let ((arguments (nreverse operands)))
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
      ex-ok)))

;;;; THE END ;;;;
