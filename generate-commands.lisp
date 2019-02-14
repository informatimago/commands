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

(setf *default-pathname-defaults* (merge-pathnames #P"./commands/.lisp" cl-user::*source-directory*))

(dolist (name *all-commands*)
  (register-command-file name (merge-pathnames name *default-pathname-defaults* nil)))

;;; Quickload used systems:

(ql:quickload (delete-duplicates
               (mapcan (lambda (name) (copy-list (command-use-systems (command-named name))))
                       *all-commands*)))

;;; Compile and load all commands:

(let ((commands '()))
  (dolist (name *all-commands*)
    (format t "~&;Processing ~A~%" name) (finish-output)
    (handler-case
        (let* ((package (command-package name)))
          (let ((*program-name* name)
                (*package* package))
            (load (let ((*program-name* name)
                        (*package* package))
                    (compile-file name :verbose t)) :verbose t))
          (let ((main (command-main (command-named name))))
            (when main (push (cons name main) commands))))
      (error (err)
        (format t "~&~A~%" err) (finish-output))))
  (pprint commands))

;;; Generate link script:

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
    (format t "ln -s commands ~A~%" name)))

;;; Save the lisp image

(cl-user::generate-program :program-name "commands"
                           :main-function "COM.INFORMATIMAGO.COMMAND.UTILITY:DISPATCH-COMMAND"
                           :system-name nil
                           :system-list '()
                           :init-file "~/.command.lisp"
                           :version "1.0.0"
                           :copyright (format nil "Copyright Pascal J. Bourguignon 2019 - 2019~%License: AGPL3")
                           :source-directory  cl-user::*source-directory*
                           :asdf-directories  cl-user::*asdf-directories*
                           :release-directory cl-user::*release-directory*)

