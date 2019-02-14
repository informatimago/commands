(in-package "COM.INFORMATIMAGO.COMMAND.UTILITY")

(defmacro command (&whole form &key name use-systems main)
  (declare (ignore form name use-systems main))
  '(progn))

(defvar *commands* (make-hash-table :test (function equal)))

(defun register-command (name command-form)
  (setf (gethash name *commands*) (list* :name name (rest command-form))))

(defun command-named (name)
  (gethash name *commands*))

(defun command-name        (command) (getf command :name))
(defun command-use-systems (command) (getf command :use-systems))
(defun command-main        (command) (or (getf command :main)
                                         (format nil "~:@(~A::MAIN~)"
                                                 (command-package-name
                                                  (command-name  command)))))

(defun command-package-name (command-name)
  (format nil "COMMAND.~:@(~A~)" command-name))

(defgeneric command-package (object)
  (:method ((command list))
    (command-package (command-name command)))
  (:method ((name string))
    (let ((package-name (command-package-name name)))
      (or (find-package package-name)
          (make-package package-name
                        :use '("COMMON-LISP"
                               "COM.INFORMATIMAGO.COMMAND.SCRIPT"
                               "COM.INFORMATIMAGO.COMMAND.UTILITY"))))))


(defun register-command-file (name pathname)
  (with-open-file (source pathname)
    (let* ((*package* (command-package name))
           (form      (read source)))
      (register-command name (when (and (listp form) (eql (first form) 'command))
                               form)))))


(defun dispatch-command (pname &rest arguments)
  "A toplevel to dispatch to commands."
  (let* ((name    (pathname-name pname))
         (command (command-named name)))
    (if command
        (progn
          (setf script:*program-name*         name
                script:*default-program-name* name
                script:*program-path*        pname
                script:*arguments*           arguments)
          (script:exit (funcall (read-from-string (command-main command)) arguments)))
        (error "No such command: ~S" name))
    (values)))

;;;; THE END ;;;;


