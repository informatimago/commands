#!/usr/local/bin/cl
;;;; -*- mode:lisp;coding:utf-8 -*-
(in-package "COMMON-LISP-USER")
;; (load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
;; (use-package "SCRIPT")
;; (defparameter *program-version* "1.0.0")

;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-list-text-file-contents (path &key (if-does-not-exist :error)
                                              (external-format :default))
  "
RETURN:  the list of lines collected from the file.
"
  (with-open-file (in path :direction :input
                           :if-does-not-exist if-does-not-exist
                           :external-format external-format)
    (stream-to-string-list  in)))

(defun split-list (separator list &key (test (function eql)))
  (loop
    :with result = '()
    :with subseq = '()
    :for item :in list
    :do (if (funcall test separator item)
            (progn
              (push (nreverse subseq) result)
              (setf subseq '()))
            (push item subseq))
    :finally (push (nreverse subseq) result)
             (return (nreverse result))))

(defun key (cookie)
  (map 'vector (function sxhash) cookie))

(defun read-cookies (pathname)
  (let ((c (make-hash-table :test (function equalp))))
    (dolist (cookie
             (delete nil
                     (split-list "%"
                                 (string-list-text-file-contents pathname)
                                 :test (function string=)))
             c)
      (if (and (gethash (key cookie) c)
               (not (equalp (gethash (key cookie) c) cookie)))
          (format t "collision ~%~S~%~S~%" (gethash (key cookie) c) cookie)
          (setf (gethash (key cookie) c) cookie)))))

(defun merge-cookies (a b)
  (let ((results (make-hash-table :test (function equalp))))
    (maphash (lambda (k c) (setf (gethash k results) c)) a)
    (maphash (lambda (k c) (setf (gethash k results) c)) b)
    results))

(defun write-cookies (pathname cookies)
  (with-open-file (stream pathname
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (dolist (c cookies)
      (write-line "%" stream)
      (dolist (l c)
        (write-line l stream)))
    (write-line "%" stream)))

(defun main (arguments)
  (let ((cookies (make-hash-table :test (function equalp))))
    (dolist (path arguments)
      (setf cookies (merge-cookies (read-cookies path) cookies)))
    (write-cookies "/tmp/cookies" cookies))
  0)

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
