;; -*- mode:lisp;coding:utf-8 -*-

(options "buzzword" (standard-options))

(defun main (arguments)
  (parse-options *command* arguments)
  (setf *random-state*  (make-random-state t))
  (let ((a (random 10))
        (b (random 10))
        (c (random 10))
        (w (make-array '(10 3) :initial-contents
                       #((integrated        management           options)
                         (total             organizational       flexibility)
                         (systematized      monitored            capability)
                         (parallel          reciprocal           mobility)
                         (functional        digital              programming)
                         (responsive        logistical           concept)
                         (optional          transitional         time-phase)
                         (synchronized      incremental          projection)
                         (compatible        third-generation     hardware)
                         (balanced          policy               contingency))))
        (*print-case* :downcase))
    (format t "~&~A ~A ~A~%" (aref w a 0) (aref w b 1) (aref w c 2))
    (finish-output))
  ex-ok)

;;;; THE END ;;;;
