;;;; -*- mode:lisp; coding:utf-8 -*-


(defun main (arguments)
  (declare (ignore arguments))
  (setf *read-base* 16.)
  (loop
    :with buffer = (make-array 8 :element-type '(unsigned-byte 8)  :fill-pointer 0 :adjustable t)
    :for byte = (read *standard-input* nil nil)
    :while  byte
    :do (vector-push-extend byte buffer (* 2 (length buffer)))
    :finally (with-open-file (out "binary"
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-does-not-exist :create
                                  :if-exists :supersede)
               (write-sequence buffer out)
               (format t "Written ~D bytes to ~S~%" (file-length out) (namestring out))))
  ex-ok)

;;;; THE END ;;;;
