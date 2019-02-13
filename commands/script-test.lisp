;; -*- mode:lisp; coding:utf-8 -*-

(command :use-systems (:cffi))

(cffi:defcvar (environ "environ") :pointer)

(defun environment ()
  (loop
    :for i :from 0
    :for s := (print (cffi:mem-aref environ :pointer i))
    :until (cffi:null-pointer-p s)
    :collect (cffi:foreign-string-to-lisp s)))

;;;;--------------------------------------------------------------------

(defun main (arguments)
  (declare (ignore arguments))
  ;; (apropos "*" "UIOP")
  ;; (print uiop:*COMMAND-LINE-ARGUMENTS*)

  (write-line "Environment:") (finish-output)
  (dolist (e (environment))
    (write-line e))
  (write-line "Done.") (finish-output)

  ;; (apropos "*" "CL-LAUNCH")
  (dolist (v '("CL_LAUNCH_FILE" "PROG" "PROGBASE"))
    (format t "~20A = ~S~%" v (uiop:getenv v)))
  (let ((*package* (make-package "foo" :use '())))
    (dolist (f (sort (copy-list *features*) (function string<)))
      (print f)))

  ex-ok)

;;;; THE END ;;;;
