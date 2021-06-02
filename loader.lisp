(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *load-truename*
    (error "This script must be loaded as source.")))

;;; --------------------------------------------------------------------
;;;

(defparameter *base-directory*    (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
(defparameter *source-directory*  (merge-pathnames #P"./sources/" *base-directory* nil))
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *source-directory* nil))
                                                  (directory (merge-pathnames "src/public/lisp/**/*.asd" (user-homedir-pathname) nil))
                                                  (list *source-directory*))))
(defparameter *release-directory* *base-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )

;;; --------------------------------------------------------------------

(load (merge-pathnames "generate.lisp" *base-directory*))
(configure-asdf-directories *asdf-directories*)
(ql:quickload :com.informatimago.command)

(load (merge-pathnames "builder.lisp" *base-directory*)) ; sets *failures*

(in-package "COM.INFORMATIMAGO.COMMAND.GENERATE")
(unless (zerop *failures*)
  (format t "~&;~D failures~%" *failures*)
  (finish-output))

