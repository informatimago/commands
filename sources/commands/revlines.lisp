;;;; -*- mode:lisp; coding:iso-8859-1 -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; Warning: processes iso-8859-1 not utf-8 arguments! ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(defun slurp (stream)
  (loop
    :for line := (read-line stream nil nil)
    :while line :collect line))

(defun barf (lines stream)
  (dolist (line lines)
    (write-line line stream)))

(defun main (arguments)
  (declare (ignore arguments))
  (barf (mapcar (function reverse) (slurp *standard-input*)) *standard-output*)
  ex-ok)

;;;; THE END ;;;;
