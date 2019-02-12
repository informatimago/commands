#!/usr/local/bin/clisp -ansi -q -E iso-8859-1
;;;; -*- mode:lisp; coding:utf-8 -*-

(defun entropy (histogram total)
  (let ((entropy 0)
        (base (length histogram)))
    (map nil (lambda (count)
               (let ((p (/ count total)))
                 (when (plusp p)
                   (decf entropy (* p (log p base))))))
      histogram)
    entropy))

(defun byte-histogram (stream)
  (let ((total     0)
        (histogram (make-array 256 :initial-element 0))
        (next-byte (if (subtypep (stream-element-type stream) 'character)
                       (lambda ()
                         (let ((ch (read-char stream nil nil)))
                           (and ch (char-code ch))))
                       (lambda ()
                         (read-byte stream nil nil)))))
    (loop
      :for byte := (funcall next-byte)
      :while byte
      :do (incf total)
          (incf (aref histogram byte)))
    (values histogram total)))

;; entropy = 0
;; 
;; for count in byte_counts:
;;     # If no bytes of this value were seen in the value, it doesn't affect
;;     # the entropy of the file.
;;     if count == 0:
;;         continue
;;     # p is the probability of seeing this byte in the file, as a floating-
;;     # point number
;;     p = 1.0 * count / total
;;     entropy -= p * math.log(p, 256)

(format t "~A~%" (multiple-value-call (function entropy) (byte-histogram *standard-input*)))
