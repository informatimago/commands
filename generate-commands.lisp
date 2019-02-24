(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless *load-truename*
    (error "This script must be loaded as source.")))

;;; --------------------------------------------------------------------
;;; Load the generator

(load (make-pathname :name "generate" :type "lisp" :version nil
                     :defaults (or *load-pathname* #P"./")))


;;; --------------------------------------------------------------------
;;;

(defparameter *base-directory*    (make-pathname :name nil :type nil :version nil :defaults *load-truename*))
(defparameter *source-directory*  (merge-pathnames #P"./sources/" *base-directory* nil))
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *source-directory* nil))
                                                  (directory (merge-pathnames "src/public/lisp/**/*.asd" (user-homedir-pathname) nil))
                                                  (list *source-directory*))))
(defparameter *release-directory* *base-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )

(load-quicklisp)
(configure-asdf-directories *asdf-directories*)
(ql:quickload :com.informatimago.command)
(in-package "COM.INFORMATIMAGO.COMMAND.GENERATE")

;;; --------------------------------------------------------------------
;;; generate the program
;;;

(defparameter *all-commands*
  '("add-cookie" "add-paths" "ansi-test" "batch-emerge"
    "bin-to-c-array" "buzzword" "capitalize"
    "cddb-to-tag" "check-surface" "clar"
    "clean-bd-archive" "clean-name" "clean-paths"
    "commands"
    "columnify" "cookie-diff" "cookie-loop"
    "cookie-merge" "cookie" "dedup"
    "departement" "diss" "downcase"
    "edit-comments-of-ogg" "entropy" "euronews"
    "extend-identifiers" "fetch-pop" "fpm"
    "get-directory" "group-files"
    "hexbin"
    "html-make-image-index" "insulte" "kwic" "lc" "llen"
    "lrev" "macosx-port-uninstall-recursively" "memo"
    "menu" "merge" "mfod" "new-password" "news-to-mbox"
    "nls" "one-of" "pic-resize" "pjb-diff" "programmer"
    "pseudo-pop" "radio" "random"
    "religion" "remove-duplicate-files" "revlines"
    "rotate" "rstuml"
    "script-test"
    "sleep-schedule" "split-dir"
    "split-merge" "substitute" "surveille-host"
    "surveille-web-pages" "svn-locate-revision" "text"
    "when")
  #-(and) '("box" "cpcd"  "shell"))

(dolist (name *all-commands*)
  (ignore-errors (delete-package (command-package-name name))))

;;; Scan command sources for command :use-systems forms.

(defun register-comamnds ()
  (let ((*default-pathname-defaults* (merge-pathnames (make-pathname :directory '(:relative "commands")
                                                                     :name nil
                                                                     :type "lisp"
                                                                     :version nil)
                                                      cl-user::*source-directory*)))
    (dolist (name *all-commands*)
      (register-command-file name (merge-pathnames name *default-pathname-defaults* nil)))))

;;; Quickload used systems:

(defun quickload-command-dependencies ()
  (ql:quickload (delete-duplicates
                 (mapcan (lambda (name) (copy-list (command-use-systems (command-named name))))
                         *all-commands*))))

;;; Compile and load all commands:

(defparameter *failures* 0)

(defun compile-and-load-command (name)
  (handler-case
      (let* ((package (command-package name))
             (command (command-named name)))
        (format t "~&;Processing ~A in package ~A~%" name  (package-name package)) (finish-output)
        (multiple-value-bind (fasl warnings-p failure-p)
            (let ((*program-name* name)
                  (*package* package))
              (compile-file (command-pathname command) :verbose t))
          (if (or warnings-p failure-p)
              (progn
                (format t "~&;Failed to compile ~S~%" (command-pathname command))
                (incf *failures*))
              (if (let ((*program-name* name)
                        (*package* package))
                    (load fasl :verbose t))
                  (format t "~&;Successfully loaded ~S~%" fasl)
                  (progn
                    (format t "~&;Failed to load ~S~%" (command-pathname command))
                    (incf *failures*))))))
    (error (err)
      (format *error-output* "~&~A~%" err) (finish-output *error-output*))))

;;; Generate link script:

(defun generate-link-script ()
  (with-open-file (*standard-output*
                   (merge-pathnames (make-pathname :name "symlink-commands" :type :unspecific :version :unspecific
                                                   :defaults cl-user::*release-directory*)
                                    cl-user::*release-directory*
                                    nil)
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (write-line "#!/bin/bash")
    (dolist (name *all-commands*)
      (format t "ln -s commands ~A~%" name))))


;;; Save the lisp image

(progn
  (register-comamnds)
  (quickload-command-dependencies)
  (generate-link-script)
  (setf *failures* 0)
  (dolist (name *all-commands*)
    (compile-and-load-command name)))

#-testing
(unless (zerop *failures*)
  (format t "~&;~D failures~%" *failures*)
  (finish-output)
  (script:exit 1))

#-testing
(cl-user::generate-program :program-name "commands"
                           :main-function "COM.INFORMATIMAGO.COMMAND.SCRIPT:DISPATCH-COMMAND"
                           :system-name nil
                           :system-list '()
                           :init-file "~/.commands.lisp"
                           :version "1.0.0"
                           :copyright (format nil "Copyright Pascal J. Bourguignon 2019 - 2019~%License: AGPL3")
                           :source-directory  cl-user::*source-directory*
                           :asdf-directories  cl-user::*asdf-directories*
                           :release-directory cl-user::*release-directory*)

;; In most implementations, we should not reach this point, since saving the image exits.
#-testing (script:exit 0)
