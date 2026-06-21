;;;; -*- mode:lisp; coding:iso-8859-1 -*-

;; prefix each line of input with its length.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; Warning: processes iso-8859-1 not utf-8 arguments! ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(defun llen (stream)
  (loop
    :for line = (read-line stream nil nil)
    :while line
    :do (format t "~D ~A~%" (length line) line)))

(defun input-files (operands &key (standard-input t))
  "Return a generator (a function of no arguments) that yields successive
input streams for the file OPERANDS (\"-\" denotes *STANDARD-INPUT*), or
*STANDARD-INPUT* once when OPERANDS is empty and STANDARD-INPUT is true."
  (cond
    (operands
     (let ((files operands))
       (lambda ()
         (cond
           ((null files) nil)
           ((and standard-input (string= (first files) "-")) (pop files) *standard-input*)
           ((open (pop files)))))))
    (standard-input
     (let ((given nil))
       (lambda ()
         (if given
             nil
             (progn (setf given t) *standard-input*)))))
    (t
     (constantly nil))))

(options "llen" (standard-options))

(defun main (arguments)
  (let ((operands '()))
    (parse-options *command* arguments nil
                   (lambda (arg rest)
                     (if (string= arg "--")
                         (progn (setf operands (revappend rest operands)) '())
                         (progn (push arg operands) rest))))
    (let ((files (input-files (nreverse operands) :standard-input t)))
      (loop
        :for stream := (funcall files)
        :while stream
        :do (llen stream))))
  ex-ok)


