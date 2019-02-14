;;;; -*- mode:lisp;coding:utf-8 -*-

(command :use-systems (:split-sequence :cl-ppcre)
         :use-packages ("COMMON-LISP" "SCRIPT" "SPLIT-SEQUENCE"
                                      "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun key (cookie)
  (map 'vector (function sxhash) cookie))

(defun read-cookies (pathname)
  (let ((c (make-hash-table :test (function equalp))))
    (dolist (cookie
             (delete nil
                     (split-sequence "%"
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
  ex-ok)

;;;; THE END ;;;;
