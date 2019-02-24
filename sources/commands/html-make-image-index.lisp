;;;; -*- mode:lisp; coding:utf-8 -*-

(command :use-systems (:com.informatimago.common-lisp.cesarum
                       :com.informatimago.common-lisp.html-generator)
         :use-packages ("COMMON-LISP"
                        "SCRIPT"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"))

;;------------------------------------------------------------------------

(defun generate-image-page (previous current next)
  (let ((hprev (format nil "~A.html" previous))
        (hcurr (format nil "~A.html" current))
        (hnext (format nil "~A.html" next)))
    (format t "Processing ~A~%" current)
    (with-open-file (out hcurr :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
      (html:with-html-output (out)
        (html:doctype :strict
          (html:html ()
            (html:head ()
              (html:title () (html:pcdata "~A" current)))
            (html:body ()
              (html:map (:name "leftright")
                (html:area (:shape "rect" :alt "left" :href hprev
                            :coords "0,0,357,874"))
                (html:area (:shape "rect" :alt "right" :href hnext
                            :coords "357,0,714,874")))
              (html:p (:align "center")
                (html:img (:src current :width "180%" :alt current :usemap "#leftright"))))))))
    (format t "Generated  ~A~%" hcurr)))


(defun main (args)
  (with-open-file (out "index.html" :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)
    (html:with-html-output (out)
      (html:doctype :strict
        (html:html ()
          (html:head ()
            (html:title () (html:pcdata "Index")))
          (html:body ()
            (html:h1 () (html:pcdata "Index"))
            (html:p ()  (html:pcdata "Please, browse these documents:"))
            (html:p ()
              (html:ul ()
                (do* ((args     (cddr args) (cdr args))
                      (previous nil         current)
                      (current  (car args)  next)
                      (next     (cadr args) (car args)))
                     ((null current))
                  (generate-image-page previous current next)))))))))
  ex-ok)

;;;; THE END ;;;;
