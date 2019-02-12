;; -*- mode:lisp;coding:utf-8 -*-

(in-package "COMMON-LISP-USER")

(defun split-string (string &optional (separators " ") (remove-empty nil))
  "
STRING:         A sequence.

SEPARATOR:      A sequence.

RETURN:         A list of subsequence of STRING, split upon any element of SEPARATORS.
                Separators are compared to elements of the STRING with EQL.

NOTE:           It's actually a simple split-sequence now.

EXAMPLES:       (split-string '(1 2 0 3 4 5 0 6 7 8 0 9) '(0))
                --> ((1 2) (3 4 5) (6 7 8) (9))
                (split-string #(1 2 0 3 4 5 0 6 7 8 0 9) #(0))
                --> (#(1 2) #(3 4 5) #(6 7 8) #(9))
                (split-string \"1 2 0 3 4 5 0 6 7 8\" '(#\space #\0))
                --> (\"1\" \"2\" \"\" \"\" \"3\" \"4\" \"5\" \"\" \"\" \"6\" \"7\" \"8\")
"
  (loop
    :with strlen = (length string)
    :for position = 0 :then (1+ nextpos)
    :for nextpos = (position-if (lambda (e) (find e separators)) string :start position)
    :unless (and remove-empty
                 (or (and (= position strlen) (null nextpos ))
                     (eql position nextpos)))
      :collect (subseq string position nextpos)
    :while (and nextpos (< position strlen))))

(defun string-justify-left (string &optional (width 72) (left-margin 0) (separators #(#\Space #\Newline)))
  "
RETURN:         A left-justified string built from string.

WIDTH:          The maximum width of the generated lines.  Default is 72 characters.

LEFT-MARGIN:    The left margin, filled with spaces.  Default is 0 characters.

SEPARATORS:     A sequence containing the characters on which to split the words.
                Default: #\(#\space #\newline).
"
  (check-type string string)
  (check-type width integer)
  (check-type left-margin integer)
  (let* ((margin    (make-string left-margin :initial-element (character " ")))
         (splited   (split-string string separators t))
         (col       left-margin)
         (justified (list (subseq margin 0 col)))
         (separator ""))
    (dolist (word splited)
      (if (<= width (+ col (length word)))
          (progn (push #(#\newline) justified)
                 (push margin justified)
                 (push word justified)
                 (setf col (+ left-margin (length word))))
          (progn (push separator justified)
                 (push word justified)
                 (incf col (+ 1 (length word)))))
      (setf separator " "))
    ;; ;; Pad with spaces up to width.
    ;; (when (< col width)
    ;;   (push (make-string (- width col) :initial-element (character " "))
    ;;         justified))
    (apply (function concatenate) 'string (nreverse justified))))

(defun fmt (s)
  (write-string (string-justify-left s))
  (terpri)
  (finish-output))

(defun one-of (s) (elt s (random (length s))))

(defun main (arguments)
  (declare (ignore arguments))
  (setf *random-state* (make-random-state t))
  (fmt (one-of #(
                 "You’ve baked a really lovely cake, but then you’ve used dog sh!t for frosting."
                 "You make some of the best products in the world — but you also make a lot of crap. Get rid of the crappy stuff."
                 "This company is in shambles, and I don’t have time to wet-nurse the board. So I need all of you to resign."
                 "Do you want to spend the rest of your life selling sugared water or do you want a chance to change the world?"
                 "Being the richest man in the cemetery doesn’t matter to me…"
                 "Be a yardstick of quality.  Some people aren’t used to an environment where excellence is expected."
                 "The products suck!  There's no sex in them anymore!"
                 "I’ve never known an HR person who had anything but a mediocre mentality."
                 "Everything you have done in your life is shit!  So why don't you come work for me?"
                 "My job is to say when something sucks rather than sugarcoat it."
                 "Look, I don’t know who you are, but no one around here is too important to fire besides me. And you’re fired!"
                 "You think I'm an arrogant ass -- who thinks he's above the law, and I think you're a slime bucket who gets most of his facts wrong."
                 "That's the kind of Porsche that dentists drive."
                 "Sometimes when you innovate, you make mistakes. It is best to admit them quickly, and get on with improving your other innovations.")))
  0)

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
