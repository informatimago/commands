(in-package "COM.INFORMATIMAGO.COMMAND.UTILITY")

(defmacro command (&whole form &key main use-systems use-packages)
  "This macro is a declaration; it doesn't 'DO' anything, but it's read
by the command generator script, to know the systems to be quickloaded,
and the packages to be used by the command package.
We can also specify a different main function than the default MAIN.
The rest of the file will be compiled and loaded in the command package."
  (declare (ignore form main use-systems use-packages))
  '(progn))

(defvar *commands* (make-hash-table :test (function equal)))

(defun register-command (name &rest attributes &key &allow-other-keys)
  (setf (gethash name *commands*) (list* :name name attributes)))

(defun command-named (name)
  (gethash name *commands*))

(defun command-name         (command) (getf command :name))
(defun command-pathname     (command) (getf command :pathname))
(defun command-use-systems  (command) (getf command :use-systems))
(defun command-use-packages (command) (getf command :use-packages))
(defun command-main         (command) (or (getf command :main)
                                          (format nil "~:@(~A::MAIN~)"
                                                  (command-package-name
                                                   (command-name  command)))))

(defun command-package-name (command-name)
  (format nil "COMMAND.~:@(~A~)" command-name))

(defparameter *default-package-use-list*
  '("COMMON-LISP"
    "COM.INFORMATIMAGO.COMMAND.SCRIPT"
    "COM.INFORMATIMAGO.COMMAND.UTILITY"))

(defun %command-package (name package-use-list)
  "Use PACKAGE-USE-LIST if not empty, otherwise use *DEFAULT-PACKAGE-USE-LIST*."
  (let ((package-name (command-package-name name)))
    (or (find-package package-name)
        (make-package package-name
                      :use (or package-use-list
                               *default-package-use-list*)))))

(defgeneric command-package (object)
  (:documentation "Returns the package of the command. Creates it if it doesn't already exist.
SEE: COMMAND-PACKAGE-NAME")
  (:method ((command list))
    (%command-package (command-name command) (command-use-packages command)))
  (:method ((name string))
    (let ((command (command-named name)))
      (if command
          (command-package command)
          (%command-package name nil)))))

(defun register-command-file (name pathname)
  (with-open-file (source pathname)
    (let* ((*package* (command-package name))
           (form      (read source)))
      (apply (function register-command) name
             :pathname pathname
             (when (and (listp form) (eql (first form) 'command))
               (rest form))))))

(defun dispatch-command (pname &rest arguments)
  "A toplevel to dispatch to commands."
  (let* ((name    (pathname-name pname))
         (command (command-named name)))
    (if command
        (progn
          (setf com.informatimago.command.script:*program-name*         name
                com.informatimago.command.script:*default-program-name* name
                com.informatimago.command.script:*program-path*         pname
                com.informatimago.command.script:*arguments*            arguments)
          (com.informatimago.command.script:exit
           (funcall (read-from-string (command-main command)) arguments)))
        (error "No such command: ~S" name))
    (values)))

;;;; THE END ;;;;
