(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *load-truename*
    (error "This script must be loaded as source.")))

;;; --------------------------------------------------------------------

(defparameter *base-directory*    (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
(defparameter *source-directory*  (merge-pathnames #P"./sources/" *base-directory* nil))
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *source-directory* nil))
                                                  (directory (merge-pathnames "src/public/lisp/**/*.asd" (user-homedir-pathname) nil))
                                                  (list *source-directory*))))
(defparameter *release-directory* *base-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )

;;; --------------------------------------------------------------------

(load (merge-pathnames "generate.lisp" *base-directory*))
(load-quicklisp)
(configure-asdf-directories *asdf-directories*)
(ql:quickload :com.informatimago.command)

(load (merge-pathnames "builder.lisp" *base-directory*)) ; sets *failures*

(in-package "COM.INFORMATIMAGO.COMMAND.GENERATE")
#-testing
(unless (zerop *failures*)
  (format t "~&;~D failures~%" *failures*)
  (finish-output)
  (script:exit 1))

;;; Save the lisp image

#-testing
(cl-user::generate-program :program-name "commands"
                           :main-function "COM.INFORMATIMAGO.COMMAND.SCRIPT:DISPATCH-COMMAND"
                           :system-name nil
                           :system-list '()
                           :init-file "~/.commands.lisp"
                           :version "1.0.0"
                           :copyright #.(format nil "Copyright Pascal J. Bourguignon 2019 - ~A~%License: AGPL3"
                                              (nth-value 5 (decode-universal-time (get-universal-time))))
                           :source-directory  cl-user::*source-directory*
                           :asdf-directories  cl-user::*asdf-directories*
                           :release-directory cl-user::*release-directory*)

;; In most implementations, we should not reach this point, since saving the image exits.
#-testing (script:exit 0)
