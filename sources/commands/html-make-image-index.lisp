;;;; -*- mode:lisp; coding:utf-8 -*-

(command :use-systems (:com.informatimago.common-lisp)
         :main "HTML-MAKE-IMAGE-INDEX:MAIN")

(defpackage "HTML-MAKE-IMAGE-INDEX"
  (:use "COMMON-LISP"
        "SCRIPT"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:shadowing-import-from "SCRIPT" "CONCAT" "MAPCONCAT")
  (:export "MAIN"))
(in-package  "HTML-MAKE-IMAGE-INDEX")

;;------------------------------------------------------------------------


(defun generate-image-page (previous current next)
  (let ((hprev (format nil "~A.html" previous))
        (hcurr (format nil "~A.html" current))
        (hnext (format nil "~A.html" next)))
    (format t "Processing ~A~%" current)
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
      (html:finalize out))
    (format t "Generated  ~A~%" hcurr)))


(defun main (args)
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
    (html:finalize out))
  ex-ok)

;;;; THE END ;;;;
