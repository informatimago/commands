;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              rotate
;;;;LANGUAGE:          common lisp (clisp)
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    CLI
;;;;DESCRIPTION
;;;;    This script rotates lines vs. columns.
;;;;USAGE
;;;;
;;;;    rotate [-90|-180|-270|-0] < lines > columns
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2016-01-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************

(defparameter *program-version* "0.0.0")


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

(defun stream-to-string-list (stream)
  "
RETURN:  the list of lines collected from stream.
"
  (typecase stream
    (stream    (loop
                 :for line = (read-line stream nil nil)
                 :while line :collect line))
    (string    (split-string stream (format nil "~C" #\newline)))
    (otherwise nil)))

(defun string-list-text-file-contents (path &key (if-does-not-exist :error)
                                              (external-format :default))
  "
IF-DOES-NOT-EXIST:  Can be :error, :create, nil, or another value that
                    is returned instead of the content of the file if
                    it doesn't exist.

RETURN:             The list of lines collected from the file, or the
                    value of IF-DOES-NOT-EXIST when not :ERROR or
                    :CREATE and the file doesn't exist.
"
  (with-open-file (in path
                      :element-type 'character
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (stream-to-string-list in)))


(defun nudge-displaced-vector (displaced-vector
                               &key
                                 (start   nil startp)
                                 (start+  nil start+p)
                                 (start-  nil start-p)
                                 (end     nil endp)
                                 (end+    nil end+p)
                                 (end-    nil end-p)
                                 (length  nil lengthp)
                                 (length+ nil length+p)
                                 (length- nil length-p)
                                 (fill-pointer nil))
  "
DO:             Changes the displacement of the DISPLACED-VECTOR.
START:          Indicates the new absolute displacement offset.
START+:         Indicates an increment of the displacement offset.
START-:         Indicates a decrement of the displacement offset.
END:            Indicates the new absolute end of the DISPLACED-VECTOR.
END+:           Indicates an increment of the DISPLACED-VECTOR end.
END-:           Indicates a decrement of the DISPLACED-VECTOR end.
LENGTH:         Indicates the new absolute length of the DISPLACED-VECTOR.
LENGTH+:        Indicates an increment of the length of the DISPLACED-VECTOR.
LENGTH-:        Indicates a decrement of the length of the DISPLACED-VECTOR.
FILL-POINTER:   Indicates the new fill pointer for the DISPLACED-VECTOR.
NOTES:          START, START+ and START- are mutually exclusive.
                END, END+, END-, LENGTH, LENGTH+ and LENGTH- are mutually exclusive.
                START and END are expressed as indices in the displaced-to array.
RETURN:         The adjusted array.

EXAMPLE:        (let* ((s #(a door a window a big hole and a bucket))
                            (v (displaced-vector s 0 3 t)))
                       (show v)
                       (show (nudge-displaced-vector v :end+   1))
                       (show (nudge-displaced-vector v :fill-pointer 2))
                       (show (nudge-displaced-vector v :start+ 3 :end+ 3))
                       (show (nudge-displaced-vector v :start- 1 :end- 1))
                       (show (nudge-displaced-vector v :fill-pointer 1)))
                prints:
                    v = #(a door a)
                    (nudge-displaced-vector v :end+ 1) = #(a door a)
                    (nudge-displaced-vector v :fill-pointer 2) = #(a door)
                    (nudge-displaced-vector v :start+ 3 :end+ 3) = #(window a)
                    (nudge-displaced-vector v :start- 1 :end- 1) = #(a window)
                    (nudge-displaced-vector v :fill-pointer 1) = #(a)
                    #(a)
"
  ;; (print   (list :start start :start+ start+ :start- start-
  ;;                :end end :end+ end+ :end- end-
  ;;                :length length :length+ length+ :length- length-
  ;;                :fill-pointer fill-pointer))
  (assert (<= (count-if (function identity) (list startp start+p start-p)) 1))
  (assert (<= (count-if (function identity) (list endp end+p end-p
                                                  lengthp length+p length-p)) 1))
  (multiple-value-bind (vector old-start) (array-displacement displaced-vector)
    (let* ((old-length (array-dimension displaced-vector 0))
           (old-end    (+ old-start old-length))
           (new-start (cond (startp     start)
                            (start+p    (+ old-start start+))
                            (start-p    (- old-start start-))
                            (t          old-start)))
           (new-length (cond (endp      (- end new-start))
                             (end+p     (- (+ old-end end+) new-start))
                             (end-p     (- (- old-end end-) new-start))
                             (lengthp   length)
                             (length+p  (+ old-length length+))
                             (length-p  (- old-length length-))
                             (t         (- old-end new-start))))
           (fill-pointer (if (eq 't fill-pointer)
                             new-length
                             fill-pointer)))
      ;; (let ((*print-array* nil))
      ;;   (print (list 'adjust-array
      ;;                displaced-vector
      ;;                (list new-length)
      ;;                :element-type (array-element-type vector)
      ;;                :fill-pointer (and (array-has-fill-pointer-p displaced-vector)
      ;;                                   fill-pointer)
      ;;                :displaced-to vector
      ;;                :displaced-index-offset new-start)))
      (adjust-array
       displaced-vector
       (list new-length)
       :element-type (array-element-type vector)
       :fill-pointer (and (array-has-fill-pointer-p displaced-vector)
                          fill-pointer)
       :displaced-to vector
       :displaced-index-offset new-start))))

(defun slurp (input)
  (let* ((lines (stream-to-string-list input))
         (height (length lines))
         (width  (reduce (function max) lines :key (function length)))
         (matrix (make-array (list height width) :element-type 'character :initial-element #\space)))
    (loop
      :with row := (make-array (list width) :element-type 'character :displaced-to matrix :displaced-index-offset 0)
      :for line :in lines
      :for y :from 0
      :do (replace row line)
          (unless (= (1+ y) height)
            (setf row (nudge-displaced-vector row :start+ width :end+ width))))
    matrix))

(defun barf (output matrix)
  (let* ((height (array-dimension matrix 0))
         (width  (array-dimension matrix 1)))
    (loop
      :with row := (make-array (list width) :element-type 'character :displaced-to matrix :displaced-index-offset 0 :fill-pointer width)
      :for y :below height
      :for length := (position #\space row :from-end t :test-not (function char=))
      :do (setf (fill-pointer row) (or length (length row)))
          (write-line row output)
          (unless (= (1+ y) height)
            (setf row (nudge-displaced-vector row :start+ width :end+ width :fill-pointer width))))
    matrix))

(defun rotate-matrix (matrix)
  (let ((rotated (make-array (reverse (array-dimensions matrix))
                             :element-type (array-element-type matrix)))
        (width   (array-dimension matrix 0))
        (height  (array-dimension matrix 1)))
    (loop
      :for i :below width
      :do (loop
            :for j :below height
            :do (setf (aref rotated (- height j 1) i) (aref matrix i j))))
    rotated))

(defun rotate (input output rotation)
  (let ((matrix (slurp input)))
    (case rotation
      ((0)    (barf output matrix))
      ((-90)  (barf output (rotate-matrix matrix)))
      ((-180) (barf output (rotate-matrix (rotate-matrix matrix))))
      ((-270) (barf output (rotate-matrix (rotate-matrix (rotate-matrix matrix))))))))

(defun usage ()
  (format t "~A version ~A~%" *program-name* *program-version*)
  (format t "~A usage:~2%" *program-name*)
  (format t "~T~A  rotate  [-90|-180|-270|-0] < lines > columns~%" *program-name*))


(defun main (options)
  (handler-case
      (cond
        ((null options)
         (rotate *standard-input* *standard-output* -90)
         ex-ok)
        ((or (member "-h" options :test (function string=))
             (member "--help" options :test (function string=)))
         (usage)
         ex-ok)
        ((cdr options)
         (format *error-output* "~A: ~A~%" *program-name* "Too many options.")
         (usage)
         ex-usage)
        (t
         (let ((rotation (parse-integer (car options))))
           (check-type rotation (member 0 -90 -180 -270))
           (rotate *standard-input* *standard-output* rotation))
         ex-ok))
    (error (err)
      (format *error-output* "~A: ~A~%" *program-name* err)
      ex-software)))

;;;; THE END ;;;;
