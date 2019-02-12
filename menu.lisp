#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-

(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "0.0.2")

(format *error-output* "~2%Not implemented yet.~2%")
(ext:exit 1)


(defun usage ()
  (format t "~%~
             ~A usage:~%~
             ~%~
             ~&    ~:*~A [-h|--help] item... ~%~
             ~%"
          pname))

(defun width ()
  (screen:with-window (screen:window-size screen:*window*)))

(defun main (arguments)
  (let ((items '()))
    (dolist (arg arguments)
      (cond
       ((or (string= "-h" arg) (string= "--help" arg))
        (usage)
        (ext:exit ex-ok))
       ((string= (aref arg 0) #\-)
        (format *error-output* "~a: invalid argument '~a'.~%" pname arg)
        (usage)
        (ext:exit ex-usage))
       (t (push arg items))))

    (when (= 0 (length items))
      (format *error-output*
        "~a: No menu item given. Aborting.~%" pname)
      (usage)
      (ext:exit ex-usage))))





#-testing-script
(progn
  (main  ext:*args*)
  (ext:exit ex-ok))


;;;; THE END ;;;;
