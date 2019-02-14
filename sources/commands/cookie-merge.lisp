;;;; -*- mode:lisp;coding:utf-8 -*-


(defun stream-to-string-list (stream)
  "
RETURN:  the list of lines collected from stream.
"
  (typecase stream
    (stream    (loop
                  :for line = (read-line stream nil nil)
                  :while line :collect line))
    (string    (split-string stream (format nil "~C" #\newline)))
    (otherwise nil)))

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



