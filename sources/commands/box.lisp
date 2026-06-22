;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               box.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Draw a box (a frame) around the text read from standard input and
;;;;    write the result to standard output.
;;;;
;;;;    By default a plain ASCII box sized to the widest input line is drawn
;;;;    (like the classic shell version of this command).  A named decorative
;;;;    frame can be selected with -f/--frame, and ASCII-art heads and feet
;;;;    can be added with -H/--head and -F/--feet.  The available templates
;;;;    are listed by -l/--list.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2026-06-22 <PJB> Finished: replaced the stub and the load-time legacy
;;;;                     forms by a proper command; the templates are now
;;;;                     embedded and parsed lazily instead of read from the
;;;;                     command's own source file.
;;;;    2005-..-.. <PJB> Created (template data).
;;;;LEGAL
;;;;    GPL
;;;;    Copyright Pascal Bourguignon 2005 - 2026
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;****************************************************************************

(command :version "1.0.0"
         :documentation "
Draw a box around the text read from standard input.

With no options a plain ASCII box, sized to the widest input line, is drawn
around the text and written to standard output.  A named decorative frame may
be selected with -f/--frame, and ASCII-art heads and feet added with -H/--head
and -F/--feet.  Use -l/--list to print the names of the available frames,
heads and feet.

Examples:
    echo hello | box
    fortune | box -f bevel
    cowsay-like:  echo Moo | box -H cow")

;;; ------------------------------------------------------------------------
;;; Embedded templates.
;;;
;;; The data below is the historical template definition that the original
;;; command used to read back from its own source file.  It is kept here as a
;;; string and parsed lazily (see BOX-SECTIONS) so the command is
;;; self-contained and does not depend on the location of its source.
;;;
;;; Line format:
;;;     S NAME   start a section (HEADS, FEET or FRAMES)
;;;     T NAME   start a template (block) named NAME inside the section
;;;     F C      the fill character C marking the content area of a frame
;;;     <K> ...  a picture line; the key K is one of:
;;;                H head/picture line   I head base line   M repeated content
;;;                line   G end base line   E/O/N end lines  B decoration base
;;;

(defparameter *box-data* "S HEADS


T toto
H       \\\\\\|///
H     \\\\  - -  //
H      (  0-0  )
B  --oOOo-(_)-oOOo--


T teacher
H      ______
H      |___| \"
H      (o o)
B  -ooO--O--Ooo-


T bear
H      (_)-(_)
H       (o o)
B  -ooO--(_)--Ooo-


T mouse
H       ()_()
H       (o o)
B  -ooO--`o'--Ooo-

T cow
H       ((__))
H        (00)
B  -nn--(o__o)--nn-


T judge
H        ___
H       .|||.
H       (o o)
B  -ooO--(_)--Ooo-


T haut-de-forme
H        |\"|
H       _|_|_
H       (o o)
B  -ooO--(_)--Ooo-

T head-1
H       '/_\\
H       (o o)
B  -ooO--(_I--Ooo-

T head-2
H      ` /_\\ '
H     - (o o) -
B  -ooO--(_)--Ooo-

T asyrien
H    .  .:::.
H      :(o o):  .
B  -ooO-=(_)--Ooo-

T egyptian
H       ,,,,,
H      /(o o)\\
B  -ooO--(_)--Ooo-

T head-3
H       /\\#/\\
H      /(o o)\\
B  -ooO=-(_)--Ooo-

T et
H     '. ___ .'
H    '  (> <) '
B  -ooO--(_)--Ooo-

T head-4
H     `  _ _  '
H    -  (OXO)  -
B  -ooO--(_)--Ooo-

T head-5
H      '\\\\-//`
H       (o o)
B  -ooO--(_)--Ooo-

T head-5
H        vvv
H       (0~0)
B  -ooO--(_)--Ooo-

T head-5
H      ` /_\\ '
H     - (o o) -
B  -ooO--(_)--Ooo-

T head-6
H        /_\\ `*
H       (o o)
B  -ooO--(_)--Ooo-

T halterophile
H   #                 #
H   #=ooO=========Ooo=#
H   #  \\\\  (o o)  //  #
B  ---------(_)---------


T lancier
H    #   ___
H    #  <_*_>
H    #  (o o)
B  --8---(_)--Ooo-


T guard
H      .'_#_`.
H      |[o o]|
B  -ooO--(_)--Ooo-


T head-7
H        !!!
H     `  _ _  '
H    -  (OXO)  -
B  -ooO--(_)--Ooo-


T head-8
H       .|||.
H       (o o)
B  -ooO--(_)--Ooo-


T fou-du-roi
H      _     _
H    o' \\.=./ `o
H       (o o)
B  -ooO--(_)--Ooo-


T
H      |.===.
H      {}o o{}
B  -ooO--(_)--Ooo-

T
H       ,-_-|
H      ([o o])
B  -ooO--(_)--Ooo-


T canotier
H      __MMM__
H       (o o)
B  -ooO--(_)--Ooo-


S FEET

T small
B  ----/\\---/\\----
H      \\(   )/


T toto
E     ooo0
E    (    )   0ooo
B  ---\\  (----(   )--
E      \\_)     ) /
E             (_/



S FRAMES

T box
F *
I +---------------------------------------------------------------------+
M | ******************************************************************* |
G +---------------------------------------------------------------------+


T bevel
F *
I /---------------------------------------------------------------------\\
M | ******************************************************************* |
G \\---------------------------------------------------------------------/



T tape
F *
I   .-----------------------------------------------------------------.
H  /  .-.   ***************************************************   .-.  \\
H |  /   \\  ***************************************************  /   \\  |
H | |\\_.  | *************************************************** |    /| |
H |\\|  | /| *************************************************** |\\  | |/|
H | `---' | *************************************************** | `---' |
M |       | *************************************************** |       |
G |       |-----------------------------------------------------|       |
E \\       |                                                     |       /
E  \\     /                                                       \\     /
E   `---'                                                         `---'



T roll
F *
H                                                                .---.
H                                                               /  .  \\
H                                                              |\\_/|   |
H                                                              |   |  /|
I   .----------------------------------------------------------------' |
H  /  .-.   *********************************************************  |
H |  /   \\  *********************************************************  |
H | |\\_.  | *********************************************************  |
H |\\|  | /| *********************************************************  |
H | `---' | *********************************************************  |
M |       | *********************************************************  |
E |       | ********************************************************* /
G |       |----------------------------------------------------------'
E \\       |
E  \\     /
E   `---'


T directory

H     ___
H    /___\\_________
H   |              |
H   | ************ |
H   | ************ |
H   | ************ |
H   | ************ |
HVK |______________|


T hand
F *
I                                 ______________________________________
H                                |                                      |
H                     _.---------|.--.   ****************************** |
H                  .-'  `       .'/  ``  ****************************** |
H               .-'           .' |    /| ****************************** |
H            .-'         |   /   `.__//  ****************************** |
H         .-'           _.--/        /   ****************************** |
H        |        _  .-'   /        /    ****************************** |
H        |     ._  \\      /     `  /     ****************************** |
H        |        ` .    /     `  /      ****************************** |
H        |         \\ \\ '/        /       ****************************** |
H        |        - \\  /        /|       ****************************** |
H        |        '  .'        / |       ****************************** |
H        |          '         |.'|       ****************************** |
H        |                    |  |       ****************************** |
N                                |       ****************************** |
G        |                    |  |______________________________________|
F                                |______________________________________|
E        |                    |.'
E        |                    /
E        |                   /
E        |                  /
E        )                 /|
E     .A/`-.              / |
E    AMMMA. `-._         / /
E   AMMMMMMMMA. `-.     / /
E  AMMMMMMMMMMMMA. `.    /
E AMMMMMMMMMMMMMMMMA.`. /
E MMMMMMMMMMMMMMMMMMMA.`.
E MMMMMMMMMMMMMMMMMMMMMA.`.
E MMMMMMMMMMMMMMMMMMMMMMMA.
E MMMMMMMMMMMMMMMMMMMMMMMMMA.
E MMVKMMMMMMMMMMMMMMMMMMMMMMM
E MMMMMMMMMMMMMMMMMMMMMMMMMV'
O MMMMMMMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMMV
O MMMMMMMMMMMMMMV
O MMMMMMMMMMMMMV
O MMMMMMMMMMMMV
O MMMMMMMMMMMV
O MMMMMMMMMMV
O MMMMMMMMMV
O MMMMMMMMV
O MMMMMMMV
O MMMMMMV
O MMMMMV
O MMMMV
O MMMV
O MMV
O MV
O V")

(defun box-prefixp (prefix string)
  "Whether STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun parse-box-data (text)
  "Parse the embedded template TEXT into a list of sections.  A section is a
list (TITLE BLOCK...); a block is a list (TITLE (KEY STRING)...), where TITLE
is a string and KEY a keyword such as :H :I :M :G :E :B :F."
  (let ((sections '()) (section '()) (block '()))
    (labels ((end-block ()
               (when block (push (nreverse block) section) (setf block '())))
             (end-section ()
               (when section (push (nreverse section) sections) (setf section '()))))
      (with-input-from-string (in text)
        (loop :for line = (read-line in nil nil)
              :while line
              :do (cond
                    ((zerop (length line)))                 ; blank: separator
                    ((box-prefixp "S " line)
                     (end-block) (end-section)
                     (setf section (list (subseq line 2))))
                    ((box-prefixp "T " line)
                     (end-block)
                     (setf block (list (subseq line 2))))
                    ((string= line "T")
                     (end-block)
                     (setf block (list "")))
                    ((and (alpha-char-p (char line 0))
                          (upper-case-p (char line 0)))
                     (push (list (intern (string (char line 0)) "KEYWORD")
                                 (if (<= 2 (length line)) (subseq line 2) ""))
                           block)))))
      (end-block) (end-section)
      (nreverse sections))))

(defvar *box-sections* nil
  "The parsed templates, computed lazily by BOX-SECTIONS.")

(defun box-sections ()
  (or *box-sections* (setf *box-sections* (parse-box-data *box-data*))))

(defun section-title (section) (first section))
(defun section-blocks (section) (rest section))
(defun block-title (block) (first block))

(defun get-section (sections name)
  (assoc name sections :test (function string=)))

(defun get-block (section name)
  (assoc name (section-blocks section) :test (function string=)))

(defun fill-char (block)
  "The fill character of the frame BLOCK (the char of its F line), or NIL."
  (let ((f (second (assoc :f (rest block)))))
    (when (and f (plusp (length f))) (char f 0))))

(defun block-lines (block)
  "The picture lines of BLOCK (its (KEY STRING) entries, but not the F line)."
  (remove-if (lambda (item) (eq (car item) :f)) (rest block)))

;;; ------------------------------------------------------------------------
;;; Rendering.

(defun pad-or-truncate (string width)
  "STRING padded with spaces on the right (or truncated) to exactly WIDTH."
  (let ((len (length string)))
    (cond ((= len width) string)
          ((< len width) (concatenate 'string string
                                      (make-string (- width len)
                                                   :initial-element #\Space)))
          (t (subseq string 0 width)))))

(defun fill-span (string fill)
  "Return as two values the first and last column of FILL in STRING, or NIL."
  (when fill
    (let ((left (position fill string)))
      (when left (values left (position fill string :from-end t))))))

(defun blank-fills (string fill)
  "STRING with every FILL character replaced by a space."
  (if fill (substitute #\Space fill string) string))

(defun substitute-into (template left right text)
  "TEMPLATE with the columns [LEFT,RIGHT] replaced by TEXT (padded/truncated
to that width)."
  (let* ((width  (1+ (- right left)))
         (result (copy-seq template)))
    (when (< (length result) (1+ right))
      (setf result (pad-or-truncate result (1+ right))))
    (replace result (pad-or-truncate text width) :start1 left)
    result))

(defun render-frame (block text-lines)
  "Render the frame BLOCK around TEXT-LINES; return the list of output lines.
If the frame has a repeated content line (key M) that line is emitted once per
input line; otherwise the input lines are dropped into the successive picture
lines that contain the fill character."
  (let* ((fill  (or (fill-char block) #\*))
         (lines (block-lines block))
         (has-m (find :m lines :key (function car))))
    (if has-m
        (multiple-value-bind (left right) (fill-span (second has-m) fill)
          (loop :for (key content) :in lines
                :if (eq key :m)
                  :append (if (and left right)
                              (mapcar (lambda (tl)
                                        (substitute-into content left right tl))
                                      text-lines)
                              (list content))
                :else
                  :collect (blank-fills content fill)))
        (let ((texts text-lines))
          (loop :for (key content) :in lines
                :collect (multiple-value-bind (l r) (fill-span content fill)
                           (cond ((and l r texts)
                                  (substitute-into content l r (pop texts)))
                                 ((and l r) (blank-fills content fill))
                                 (t content))))))))

(defun art-lines (block)
  "The raw picture lines of BLOCK (used for heads and feet)."
  (mapcar (function second) (block-lines block)))

(defun auto-box (text-lines width pad)
  "A plain ASCII box around TEXT-LINES.  WIDTH is the content width (defaulting
to the widest line); PAD spaces are added on each side."
  (let* ((w   (or width (reduce (function max) (mapcar (function length) text-lines)
                                :initial-value 0)))
         (bar (concatenate 'string "+"
                           (make-string (+ w (* 2 pad)) :initial-element #\-)
                           "+"))
         (gap (make-string pad :initial-element #\Space)))
    (append (list bar)
            (mapcar (lambda (line)
                      (concatenate 'string "|" gap (pad-or-truncate line w) gap "|"))
                    text-lines)
            (list bar))))

;;; ------------------------------------------------------------------------
;;; Command.

(defvar *frame-name* nil)
(defvar *head-name*  nil)
(defvar *feet-name*  nil)
(defvar *box-width*  nil)
(defvar *box-pad*    1)
(defvar *list-requested* nil)

(defun find-template (section-name name)
  (let ((section (get-section (box-sections) section-name)))
    (and section (get-block section name))))

(defun parse-positive-integer (string what)
  (let ((n (ignore-errors (parse-integer string :junk-allowed nil))))
    (unless (and n (<= 0 n))
      (error "Invalid ~A: ~A" what string))
    n))

(defun list-templates ()
  (dolist (section (box-sections))
    (format t "~A:~%" (section-title section))
    (dolist (block (section-blocks section))
      (let ((title (block-title block)))
        (when (plusp (length title))
          (format t "    ~A~%" title))))))

(options "box"
         (standard-options)
         (option ("frame" "-f" "--frame") (name)
                 "Use the named decorative FRAME instead of a plain ASCII box."
                 (setf *frame-name* name))
         (option ("head" "-H" "--head") (name)
                 "Print the named ASCII-art HEAD above the box."
                 (setf *head-name* name))
         (option ("feet" "-F" "--feet") (name)
                 "Print the named ASCII-art FEET below the box."
                 (setf *feet-name* name))
         (option ("width" "-w" "--width") (columns)
                 "Content width, in COLUMNS, of the default ASCII box."
                 (setf *box-width* (parse-positive-integer columns "width")))
         (option ("pad" "-p" "--pad") (columns)
                 "Number of padding spaces on each side of the default box (default 1)."
                 (setf *box-pad* (parse-positive-integer columns "pad")))
         (option ("list" "-l" "--list") ()
                 "List the available frames, heads and feet, and exit."
                 (setf *list-requested* t)))

(defun resolve-or-die (section-name name what)
  (let ((block (find-template section-name name)))
    (unless block
      (format *error-output* "~A: no ~A named ~S~%" *program-name* what name)
      (exit ex-usage))
    block))

(defun main (arguments)
  (setf *frame-name* nil *head-name* nil *feet-name* nil
        *box-width* nil *box-pad* 1 *list-requested* nil)
  (let ((status (parse-options *command* arguments)))
    (unless (eql status 0)
      (return-from main status)))
  (when *list-requested*
    (list-templates)
    (return-from main ex-ok))
  (let* ((text  (loop :for line = (read-line *standard-input* nil nil)
                      :while line :collect line))
         (frame (when *frame-name*
                  (resolve-or-die "FRAMES" *frame-name* "frame")))
         (head  (when *head-name*
                  (resolve-or-die "HEADS" *head-name* "head")))
         (feet  (when *feet-name*
                  (resolve-or-die "FEET" *feet-name* "feet")))
         (boxed (if frame
                    (render-frame frame text)
                    (auto-box text *box-width* *box-pad*))))
    (dolist (line (and head (art-lines head)))   (write-line line))
    (dolist (line boxed)                          (write-line line))
    (dolist (line (and feet (art-lines feet)))   (write-line line))
    (finish-output))
  ex-ok)

;;;; THE END ;;;;
