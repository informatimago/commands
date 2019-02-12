;;;; -*- mode:lisp; coding:utf-8 -*-

;;------------------------------------------------------------------------
;; Ma petite magouille maniaque:

(SETQ *LOAD-VERBOSE* nil)
(LOAD (MERGE-PATHNAMES
       (MAKE-PATHNAME :NAME ".common" :TYPE "lisp") (user-homedir-pathname)))


;;; (SETQ *LOAD-VERBOSE* t)
;;;
;;; ;; clean the imported packages:
;;; (MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
;;;       (REMOVE (FIND-PACKAGE "COMMON-LISP")
;;;               (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))
;;;
;;; ;; Load COM.INFORMATIMAGO.COMMON-LISP.PACKAGE:
;;; (let ((+common+    "/local/share/lisp/packages/com/informatimago/common-lisp/"))
;;;   (macrolet ((SCONC (&REST ARGS) `(CONCATENATE 'STRING  ,@ARGS)))
;;;     (HANDLER-CASE (LOAD (SCONC +common+ "package"))
;;;       (ERROR ()   (LOAD (SCONC +common+ "package.lisp")))) ))
;;;
;;; ;; Import DEFINE-PACKAGE, and add translations:
;;; (let ((+SHARE+     "/local/share/lisp/"))
;;;   (macrolet ((SCONC (&REST ARGS) `(CONCATENATE 'STRING  ,@ARGS)))
;;;     (IMPORT 'PACKAGE:DEFINE-PACKAGE)
;;;     (PACKAGE:ADD-TRANSLATIONS
;;;      (LIST "**;*"              (SCONC +SHARE+ "packages/**/*"))
;;;      (LIST "**;*.*"            (SCONC +SHARE+ "packages/**/*.*"))
;;;      (LIST "**;*.*.*"          (SCONC +SHARE+ "packages/**/*.*.*"))) ))

(use-package "PACKAGE")

(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.LIST")
(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.STRING")
(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.HTML")
(PACKAGE:LOAD-PACKAGE "COM.INFORMATIMAGO.CLISP.SCRIPT")

(PACKAGE:ADD-NICKNAME "COM.INFORMATIMAGO.COMMON-LISP.HTML" "HTML")
(PACKAGE:ADD-NICKNAME "COM.INFORMATIMAGO.CLISP.SCRIPT"     "SCRIPT")

;;
;;------------------------------------------------------------------------


(SCRIPT:INITIALIZE)


(defun generate-image-page (previous current next)
  (let ((hprev (format nil "~A.html" previous))
        (hcurr (format nil "~A.html" current))
        (hnext (format nil "~A.html" next)))
    (format t "Processing ~A~%" current)
    (HTML:INITIALIZE)
    (html:doctype :strict
      (html:html ()
        (html:head ()
          (html:title () (html:insert-pcdata "~A" current)))
        (html:body ()
          (html:map (:name "leftright")
            (html:area (:shape "rect" :alt "left" :href hprev
                        :coords "0,0,357,874"))
            (html:area (:shape "rect" :alt "right" :href hnext
                        :coords "357,0,714,874")))
          (html:p (:align "center")
            (html:img (:src current :width "180%" :alt current :usemap "#leftright"))))))
    (with-open-file (out hcurr :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (HTML:FINALIZE out))
    (format t "Generated  ~A~%" hcurr))
  );;generate-image-page


(defun main (args)
  (HTML:INITIALIZE)
  (html:doctype :strict
    (html:html ()
      (html:head ()
        (html:title () (html:insert-pcdata "Index")))
      (html:body ()
        (html:h1 () (html:insert-pcdata "Index"))
        (html:p ()  (html:insert-pcdata "Please, browse these documents:"))
        (html:p ()
          (html:ul ()
            (do* ((args     (cddr args) (cdr args))
                  (previous nil         current)
                  (current  (car args)  next)
                  (next     (cadr args) (car args)))
                ((null current))
              (generate-image-page previous current next) ))))))
  (with-open-file (out "index.html" :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (HTML:FINALIZE out)));;main


(WHEN (SCRIPT:IS-RUNNING)
  (MAIN SCRIPT:*ARGUMENTS*))


;;;; html-make-image-index            --                     --          ;;;;
