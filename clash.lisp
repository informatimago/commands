;;;; -*- mode:lisp; coding:utf-8 -*-

(SETQ *LOAD-VERBOSE* NIL)
(LOAD (merge-pathnames #P".clash.lisp" (user-homedir-pathname) nil))
(LOOP
   (handler-case
       (progn (terpri) (princ "> ")
              (print (eval (read *terminal-io*))))
     (error (err) (format t "~&~S~%" err))))
