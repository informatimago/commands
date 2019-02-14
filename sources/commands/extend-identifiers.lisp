;;;; -*- mode:lisp; coding:utf-8 -*-

;; Converts a utf-8 C++ source into an ASCII one using extended characters.

(defun extend-characters (line)
  (with-output-to-string (*standard-output*)
    (loop
      :for ch :across line
      :do (if (< (char-code ch) 128)
              (princ ch)
              (format t "\\u~4,'0X" (char-code ch))))))

(defun main (arguments)
  (declare (ignore arguments))
  (loop
    :for line := (read-line *standard-input* nil nil)
    :while line
    :do (write-line (extend-characters line)))
  ex-ok)

;;;; THE END ;;;;
