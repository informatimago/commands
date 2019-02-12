#!/usr/local/bin/cl
;; -*- mode:lisp; coding:utf-8 -*-


;; Replaces any sequences of non alphanumeric or dot character in the
;; arguments by a single dash.

(defun split-string-if (predicate string &key remove-empty-subseqs)
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
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
  0)


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
