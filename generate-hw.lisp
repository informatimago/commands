(in-package "COMMON-LISP-USER")

;;; --------------------------------------------------------------------
;;; Load the generator

(load (make-pathname :name "generate" :type "lisp" :version nil
                     :defaults (or *load-pathname* #P"./")))

;;; --------------------------------------------------------------------
;;; generate the program
;;;

(defparameter *source-directory*  (or *load-pathname* (truename #P"./")))
(defparameter *asdf-directories*  (mapcar (lambda (path) (make-pathname :name nil :type nil :version nil :defaults path))
                                          (append (directory (merge-pathnames "**/*.asd" *source-directory* nil))
                                                  (list *source-directory*))))
(defparameter *release-directory* *source-directory* #|#P"HOME:bin;"|# "Where the executable will be stored." )

(generate-program :program-name "hw"
                  :main-function "HELLO-WORLD:HW"
                  :system-name "hw"
                  :system-list '()
                  :init-file "~/.hw.lisp"
                  :version "1.0.0"
                  :copyright (format nil "Copyright Pascal J. Bourguignon 2015 - 2018~%License: AGPL3")
                  :source-directory  *source-directory*
                  :asdf-directories  *asdf-directories*
                  :release-directory *release-directory*)

