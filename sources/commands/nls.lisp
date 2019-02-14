;;;; -*- mode:lisp; coding:utf-8 -*-

(defun 1+/if-you-can (n)
  (if (numberp n) (1+ n) n))


(defun cut-index (name)
  (let* ((dot (position #\. name :from-end t))
         (digit (position-if (function digit-char-p) name :from-end t :end dot)))
    (subseq name 0
            (1+/if-you-can (position-if-not
                            (function digit-char-p) name
                            :from-end t :end digit)))))


(defun nls ()
  (with-open-stream (files (uiop:run-program '("/bin/ls" "-1")
                                             :output :stream
                                             :wait nil))
    (loop
      :with table = (make-hash-table :test (function equal))
      :for file = (read-line files nil nil)
      :while file
      :do (let* ((cut (cut-index file))
                 (ent (gethash cut table)))
            (if ent
                (incf (car ent))
                (setf (gethash cut table) (cons 1 file))))
      :finally (let ((res '()))
                 (maphash (lambda (k v) (push (cons k v) res)) table)
                 (dolist (item (sort res (function string<=) :key (function car)))
                   (destructuring-bind (cut num . nam) item
                     (format t "~A~%" (if (= 1 num) nam cut))))))))

(defun main (arguments)
  (declare (ignore arguments))
  (nls)
  ex-ok)
