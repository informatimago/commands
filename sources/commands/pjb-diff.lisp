;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;
;;;; pjb-diff tries to locate each line in the other file, and
;;;; reports line movements in addition to line additions/suppressions.
;;;;


(defstruct options
  (remove-spaces t)
  (upcase        t))

(defun string-list-text-file-contents (path &key (if-does-not-exist :error)
                                       (external-format :default))
  "
RETURN:  the list of lines collected from the file.
"
  (with-open-file (inp path :direction :input
                       :if-does-not-exist if-does-not-exist
                       :external-format external-format)
    (loop
       :for line = (read-line inp nil nil)
       :while line :collect line)))

(defstruct line
  (text   "" :type string)
  (number  0 :type (integer 0))
  (key    "" :type string))

(defstruct (file (:copier nil))
  (path  ""  :type string)
  (lines #() :type (vector line)))

(defun load-file (path)
  (make-file
   :path (namestring path)
   :lines (coerce (loop
                     :for linum :from 1
                     :for text  :in (string-list-text-file-contents path)
                     :for key = (format nil "~(~{~2,'0X~}~)"
                                        (map 'list #'identity (md5sum-sequence (string-upcase (remove #\space text)))))
                     :collect (make-line :text text :number linum :key key))
                  'vector)))

(defun make-line-index (lines)
  (let ((index (make-hash-table :test (function equal))))
    (loop
       :for line :across lines
       :do (push line (gethash (line-key line) index '())))
    index))

(defun main (args)
  (destructuring-bind (left right) args
    (let* ((lfile (load-file left))
           (rfile  (load-file right))
           (rindex (make-line-index (file-lines rfile))))
      (loop
         :with num = 0
         :for line :across (file-lines lfile)
         :do (unless (gethash (line-key line) rindex)
               (if (= (1+ num) (line-number line))
                   (write-line (line-text line))
                   (format t "~%~A:~D: missing from ~A~%~A~%"
                           (file-path lfile)
                           (line-number line)
                           (file-path rfile)
                           (line-text line)))
               (setf num (line-number line)))))))

;;;; THE END ;;;;
