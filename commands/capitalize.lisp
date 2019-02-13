;; -*- mode:lisp; coding:utf-8 -*-


;; Replaces any sequences of non alphanumeric or dot character in the
;; arguments by a single dash.

(defun split-string-if (predicate string &key remove-empty-subseqs)
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (declare (ignore remove-empty-subseqs))
  (let ((chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)))
    (loop :while (< position strlen) :do
       (loop :while (and (< nextpos strlen)
                         (not (funcall predicate (aref string nextpos)))) :do
          (incf nextpos))
       (push (subseq string position nextpos) chunks)
       (setf position (1+ nextpos)
             nextpos  position))
    (nreverse chunks)))

(defun split-string (separators string &key remove-empty-subseqs)
  (split-string-if (lambda (ch) (find ch separators)) string
                   :remove-empty-subseqs remove-empty-subseqs))


;;;;--------------------------------------------------------------------

(defun main (arguments)
  (loop
    :for arg :in arguments
    :do (write-line (string-capitalize arg)))
  ex-ok)


;;;; THE END ;;;;
