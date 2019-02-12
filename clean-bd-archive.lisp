;; -*- mode:lisp;coding:utf-8 -*-

(in-package "COMMON-LISP-USER")
;; (load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
;; (use-package "SCRIPT")
(defparameter *program-name* "clean-bd-archive")
(defparameter *program-version* "1.0.2")

;; (push :debug *features*)

(defvar *just-print-p* nil)

;; (defconstant +max-command-length+
;;   #+#.(cl:if (cl:find-package :linux) '(and) '(or)) LINUX:ARG_MAX
;;   #-#.(cl:if (cl:find-package :linux) '(and) '(or)) 4096)
;;
;; (defun md5sum-files (flist)
;;   (loop
;;      :with sumtab = (make-hash-table)   ; sums are interned
;;      :for chunk = (and flist
;;                        (loop
;;                           :for size = (length "md5sum")
;;                           :then (+ size
;;                                    (if flist
;;                                        (length (format nil " ~S" (first flist)))
;;                                        0))
;;                           :while (and flist (< size +max-command-length+))
;;                           :collect (pop flist)))
;;      :while chunk
;;      :initially (format *trace-output* "~&  Checksumming...~%")
;;      :do (format *trace-output* "~&    ~6D files...~%" (length chunk))
;;      :do (with-open-stream (sums
;;                             (EXT:MAKE-PIPE-INPUT-STREAM
;;                              (format nil "md5sum~{ ~S~}"
;;                                      (mapcar (function namestring) chunk))))
;;            (loop
;;               :for line = (read-line sums nil nil)
;;               :for (sum file) = (when line
;;                                   (with-input-from-string
;;                                       (in (concatenate 'string "\\" line))
;;                                     (list (read in nil nil)
;;                                           (read-line in nil nil))))
;;               :while sum
;;               :do #+debug(format *trace-output* "received ~A~%" line)
;;                    (push (string-trim " " file) (gethash sum sumtab '()))))
;;      :finally (format *trace-output* "~&     done.~%")
;;      :finally (return sumtab)))

(defun md5sum-files (flist)
  (make-hash-table))

(defun delete-duplicate-files-in-directory (dir)
  (let ((sumtab (md5sum-files (directory (make-pathname :name :wild :type :wild
                                                        :defaults dir)))))
    (format *trace-output* "~&  Deleting doubles...~%")
    (maphash (lambda (k v)
               (when (< 1 (length v))
                 (setf v (cdr (sort v (function string>=))))
                 (dolist (f v)
                   (if *just-print-p*
                       (print `(delete-file ,f))
                       (delete-file f)))))
             sumtab)
    (format *trace-output* "~&     done.~%")))

(defun clean-bd-archive (base)
  (dolist (dir (directory
                (make-pathname
                 :directory (append (pathname-directory base) '(:wild))
                 :defaults base)))
    (format *trace-output* "Processing ~A~%" dir)
    (delete-duplicate-files-in-directory dir))
  (format *trace-output* "~&  done.~%"))

;; (clean-bd-archive "/d6/pjb/bd-archive/")
;; (clean-bd-archive "/tmp/get-bd/")
;; (delete-duplicate-files-in-directory "/d6/pjb/nanas/")

(defun main (arguments)
  (declare (ignore arguments))
  (error "not implemented yet")
  1)

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
