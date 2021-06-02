;;;; -*- mode:lisp;coding:utf-8 -*-

(command :use-systems (:com.informatimago.common-lisp.cesarum
                       :split-sequence)
         :use-packages ("COMMON-LISP"
                        "SCRIPT"
                        "SPLIT-SEQUENCE"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"))

(in-package "COMMAND.COOKIE-MERGE")

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
    (write-line "%" stream)
    (maphash (lambda (k c)
               (declare (ignore k))
               (dolist (l c)
                 (write-line l stream))
               (write-line "%" stream))
             cookies)))

(defun main (arguments)
 (let ((cookies (make-hash-table :test (function equalp))))
   (dolist (path arguments)
     (setf cookies (merge-cookies (read-cookies path) cookies)))
   (write-cookies "/tmp/cookies" cookies)
   (format t "Wrote /tmp/cookies~%")
   (finish-output))
  ex-ok)

;;;; THE END ;;;;



