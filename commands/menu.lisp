;;;; -*- mode:lisp; coding:utf-8 -*-

(defparameter *program-version* "0.0.2")

(defun main (arguments)
  (declare (ignore arguments))
  (format *error-output* "~2%Not implemented yet.~2%")
  (exit ex-usage))

#|
(defun usage ()
  (format t "~%~
             ~A usage:~%~
             ~%~
             ~&    ~:*~A [-h|--help] item... ~%~
             ~%"
          *program-name*))

(defun width ()
  #+clisp (screen:with-window (screen:window-size screen:*window*))
  #-clisp 80)

(defun main (arguments)
  (let ((items '()))
    (dolist (arg arguments)
      (cond
       ((or (string= "-h" arg) (string= "--help" arg))
        (usage)
        (exit ex-ok))
       ((string= (aref arg 0) #\-)
        (format *error-output* "~a: invalid argument '~a'.~%" *program-name* arg)
        (usage)
        (exit ex-usage))
       (t (push arg items))))

    (when (= 0 (length items))
      (format *error-output* "~a: No menu item given. Aborting.~%" *program-name*)
      (usage)
      (exit ex-usage))))

|#
;;;; THE END ;;;;
