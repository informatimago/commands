;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               religion.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Command Line Interface
;;;;DESCRIPTION
;;;;
;;;;    This  script is a religion code generator, explainer and editor.
;;;;    (See http://www.galactic-guide.com/articles/2R10.html)
;;;;
;;;;    Depends on ~/bin/script.lisp
;;;;
;;;;USAGE
;;;;    religion --help
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-05 <PJB> Converted to Common Lisp from bash.
;;;;    2002-03-22 <PJB> Creation.
;;;;BUGS
;;;;    Please report them to the author.
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2010
;;;;
;;;;    This program is free software; you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;;******************************************************************************
(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type nil :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(setf *debug* t)
(setf *random-state* (make-random-state t))

(set-documentation-text
 "See also: http://www.galactic-guide.com/articles/2R10.html

")

(defparameter *religion-pathname*
  (make-pathname :name "RELIGIONS" :type "DATA" :version NIL :case :common
                 :defaults (user-homedir-pathname)))
;; Could be a common file such as /usr/local/share/lib/religions or whatever..



;;;-----------------------------------------------------------------
;;;
;;;

(defgeneric enumeration-constants (enumeration)
  (:documentation "Return a list of (\"X\" \"label\")."))

(defgeneric normalize-enumeration-constant (enumeration constant)
  (:documentation "Returns the character corresponding to the constant designator."))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-predicate-name (name)
    (intern (format nil "~A~:[~;-~]P" name  (find #\- (string name))))))


(defmacro define-digit-type (name (&key (min 0) (max 9) (allow-unknown t) documentation))
  (assert (<= 0 min max 9) (min max) "MIN and MAX must be integers between 0 and 9, and MIN<MAX.")
  (let ((predicate (make-predicate-name name)))
   `(progn

      (deftype ,name () '(member ,@(loop
                                      :for i :from min :to max
                                      :collect (digit-char i))
                                 ,@(when allow-unknown '(#\?))))

      (defun ,predicate (object)
        (if (integerp object)
            (<= ,min object ,max)
            (and (or (characterp object)
                     (stringp object)
                     (symbolp object))
                 (= 1 (length (string object)))
                 (typep (character object) ',name))))

      (defmethod enumeration-constants ((self (eql ',name)))
        ',(let ((table (loop
                         :for n :from  1
                         :for i :from min :to max
                         :collect (list (digit-char i)
                                        (format nil "~:R ~(~A~) value." n name)
                                        ;; (if make-label
                                        ;;     (funcall (coerce make-label 'function) (code-char i))
                                        ;;     (format nil "~:R ~(~A~) value." i name))
                                        ))))
              (if allow-unknown
                  (append table  `((#\? "Unknown.")))
                  table)))

      (defmethod normalize-enumeration-constant ((self (eql ',name)) (constant t))
        (assert (,predicate constant) (constant)
                "CONSTANT must be between ~A and ~A." ,min ,max)
        (character constant))

      (defmethod normalize-enumeration-constant ((self (eql ',name)) (constant integer))
        (assert (,predicate constant) (constant)
                "CONSTANT must be between ~A and ~A." ,min ,max)
        (digit-char constant))

      ',name)))


(defmacro define-letter-type (name (&key (min #\A) (max #\Z) (allow-unknown t) documentation make-label))
  (assert (and (characterp min) (characterp max)
               (char<= min max))
          (min max) "MIN and MAX must be characters and MIN<MAX.")
  (let ((predicate (make-predicate-name name)))
    `(progn

       (deftype ,name () '(member ,@(loop
                                       :for i :from (char-code min) :to (char-code max)
                                       :collect (code-char i))
                                  ,@(when allow-unknown '(#\?))))

       (defun ,predicate (object)
         (and (or (characterp object)
                  (stringp object)
                  (symbolp object))
              (= 1 (length (string object)))
              (typep (character object) ',name)))

       (defmethod enumeration-constants ((self (eql ',name)))
         ',(let ((table (loop
                           :for n :from 1
                           :for i :from (char-code min) :to (char-code max)
                           :collect (list (code-char i)
                                          (format nil "~:R ~(~A~) value." n name)
                                          ;; (if make-label
                                          ;;     (funcall (coerce make-label 'function) (code-char i))
                                          ;;     (format nil "~:R ~(~A~) value." i name))
                                          ))))
                (if allow-unknown
                    (append table  `((#\? "Unknown.")))
                    table)))

       (defmethod normalize-enumeration-constant ((self (eql ',name)) (constant t))
         (assert (,predicate constant) (constant)
                 "CONSTANT must be between ~A and ~A." ,min ,max)
         (character constant))

       ',name)))


(defmacro define-enumeration (name (&key (allow-unknown t) documentation) &rest constants)
  (let ((predicate (make-predicate-name name)))
   `(progn

      (deftype ,name () '(member ,@(mapcar (lambda (constant) (character (first constant)))
                                           constants)
                                 ,@(when allow-unknown '(#\?))))

      (defun ,predicate (object)
        (and (or (characterp object)
                 (stringp object)
                 (symbolp object))
             (= 1 (length (string object)))
             (typep (character object) ',name)))

      (defmethod enumeration-constants ((self (eql ',name)))
        ',(append (mapcar (lambda (constant)
                           (list (character (first constant))
                                 (second constant)))
                         constants)
                 (when allow-unknown
                   `((#\? "Unknown.")))))

      (defmethod normalize-enumeration-constant ((self (eql ',name)) (constant t))
        (assert (,predicate constant) (constant)
                "CONSTANT must be one of ~{~A~^, ~}."
                ',(mapcar (function first) constants))
        (character constant))

      ',name)))


;;;-----------------------------------------------------------------
;;;
;;; Definitions of the encodings
;;;

(define-digit-type deity-count (:min 0 :max 9))

(define-enumeration deity ()
  ("A" "All powerful, all knowing, benevolent.")
  ("B" "All powerful, but one can usually pull pranks due to lack of all knowingness, benevolent.")
  ("C" "All knowing, but who the hell really cares due to lack of all powerfulness, benevolent.")
  ("D" "Neither all knowing nor all powerful, but just kind of there, benevolent.")
  ("E" "All powerful, all knowing, malevolent.")
  ("F" "All powerful, but one can usually pull pranks due to lack of all knowingness, malevolent.")
  ("G" "All knowing, but who the hell really cares due to lack of all powerfulness, malevolent.")
  ("H" "Neither all knowing nor all powerful, but just kind of there, malevolent.")
  ("I" "Whoever happens to be leader of the people at the time.")
  ("J" "Everything and everybody is part of the god.")
  ("K" "Everything and everybody, except for people members of the religion don't like, is part of god.")
  ("L" "God is a head of lettuce named Ralph."))


(define-enumeration after-death ()
  ("0" "Everybody goes to a nice place.")
  ("1" "Members of the religion go to a nice place, everybody else goes to an unpleasant place.")
  ("2" "Members of the religion go to an unpleasant place, everybody else goes to a nice place.")
  ("3" "Everybody goes to an unpleasant place.")
  ("4" "Nobody goes anywhere.")
  ("5" "Really bad people are forced to work in all night convenience stores in New Jersey.")
  ("6" "Everybody is reincarnated.")
  ("7" "Only people who deserve punishment are reincarnated."))


(define-letter-type organization (:min #\A :max #\Z :documentation "
A representing very organized, Z representing chaos. Follow this by a
hyphen to make it easier to find the next section."))


(define-digit-type bubba (:min 0 :max 9 :documentation "
The next number represents the percentage of members that are named
\"Bubba.\"  Do this on a scale from 0 to 9, with 0 meaning that nobody
is named Bubba and 9 meaning that everyone is named Bubba.  Southern
Baptists are rated a 5 and the First Congregational Church of Bubba is
a 9."))


(define-enumeration policy ()
  ("A" "Handed down from a single source.")
  ("B" "Voted upon by a collection of elders.")
  ("C" "Voted upon by everybody.")
  ("D" "Chosen by a random number generator.")
  ("E" "Determined by careful computer analysis.")
  ("F" "Determined by combatants representing each view playing Super Mario Brothers.")
  ("Z" "Nobody has ever tried to change the policy, so nobody knows just yet."))


(define-digit-type number-of-spellings (:min 0 :max 9 :allow-unknown t :documentation "
Then follows a digit representing the number of ways one can spell the name
of the religion."))


(define-enumeration place ()
  ("A" "No meetings.")
  ("B" "A building set aside for the purpose.")
  ("C" "A building which is also the gym for the local high school.")
  ("D" "Outside.")
  ("E" "In an airport or bus terminal.")
  ("F" "In a submarine.")
  ("G" "In a graveyard or mausoleum.")
  ("F" "In a bathtub or jacuzzi."))


;;;-----------------------------------------------------------------
;;;
;;; Definitions of the encodings
;;;

(defun line-code  (line) (elt line 0))
(defun line-label (line) (elt line 1))


(defun split-string (string &optional (separators " "))
  (loop
     :for start = 0 :then (and end (1+ end))
     :for end = (and start (position-if (lambda (ch) (find ch separators))
                                        string :start start))
     :while start
     :collect (subseq string start end)))


(defun read-answer (&optional (stream *standard-input*))
  (string-upcase (string-trim #(#\space #\tab) (read-line stream))))

(defun yeah-nay-p (&optional (stream *standard-input*) )
  (loop
     :for answer = (read-answer stream)
     :do (cond
           ((find answer '("1" "t" "true" "v" "vrai" "verdad" "y" "yes" "o"
                           "oui" "j" "ja" "d" "da" "s" "si" "p" "positif"
                           "a" "affirmative" "affirmatif" "afirmativo" "yep" "yeah"
                           "ouais")
                  :test (function string-equal))
            (return-from yeah-nay-p t))
           ((find answer '("0" "nil" "f" "false" "faux" "falso" "n" "no" "non"
                           "nein" "niet" "negatif" "negative" "negativo" "nay")
                  :test (function string-equal))
            (return-from yeah-nay-p nil)))))


;;;-----------------------------------------------------------------
;;;
;;; Religion database
;;;

(defun load-religion-database (path)
  (with-open-file (db path :if-does-not-exist nil)
    (and db
         (loop
            :for line = (read-line db nil nil)
            :while line
            :collect (split-string line ":")))))


(defun save-religion-database (db path)
  (with-open-file (stream path
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
    (format stream "~:{~A:~A~%~}" db)))

(defparameter *religion-db* (load-religion-database *religion-pathname*))

(defun lookup-names (code)
  (mapcar (function second)
          (remove code *religion-db*
                  :test-not (function string-equal)
                  :key (function first))))

(defun lookup-code (name)
  (first (find name *religion-db*
               :test (function string-equal)
               :key (function second))))


;;;-----------------------------------------------------------------
;;;
;;; Generation
;;;


(defun one-of (seq) (elt seq (random (length seq))))


(defun implode-religion (num dei aft org bub pol spe pla)
  (format nil "~A~:[~*~;~A~]~A~A-~A~A~A~A"
          num
          (char/= #\0 (normalize-enumeration-constant 'deity-count num))
          dei aft org bub pol spe pla))

(defun generate ()
  (apply (function implode-religion)
         (random 10)
         (mapcar (lambda (enum) (line-code (one-of (enumeration-constants enum))))
                 '(deity after-death organization
                   bubba policy number-of-spellings place))))

;;;-----------------------------------------------------------------
;;;
;;; Parsing
;;;

(defun imply (p q) (or (not p) q))
(defun <=> (a b) (or (and a b) (and (not a) (not b))))


(defun code-valid-p (code)
  (and (= 8 (length code))
       (char= #\- (aref code 3))
       (or (char= #\? (aref code 0)) (upper-case-p (aref code 0)))
       (or (char= #\? (aref code 1)) (digit-char-p (aref code 1)))
       (or (char= #\? (aref code 2)) (upper-case-p (aref code 2)))
       (or (char= #\? (aref code 4)) (digit-char-p (aref code 4)))
       (or (char= #\? (aref code 5)) (upper-case-p (aref code 5)))
       (or (char= #\? (aref code 6)) (digit-char-p (aref code 6)))
       (or (char= #\? (aref code 7)) (upper-case-p (aref code 7)))))


(defun explode-religion (rel)
  (flet ((invalid-religion-code (rel &rest additionnal-message)
           (cerror "Enter another religion code instead."
                   "Invalid religion code ~S~:[~;~%~A~]"
                   rel  additionnal-message
                   (and additionnal-message (apply (function format) nil additionnal-message)))
           (format *query-io* "New religion code: ")
           (finish-output  *query-io*)
           (setf rel (read-answer  *query-io*))
           (return-from explode-religion (explode-religion rel))))
    (let ((rel (handler-case (coerce rel 'string)
                 (error () (invalid-religion-code rel "rel = ~S; type: ~S" rel (type-of rel))))))
      (case (length rel)
        ((8 9) ; one more for the dash.
         (with-input-from-string (inp rel)
           (let ((num (read-char inp nil nil))
                 (dei nil))
             (unless (<=> (= (length rel) 8) ; 8 = one more for the dash
                          (char= #\0 num))
               (invalid-religion-code
                rel
                (if (char= #\0 num)
                    "When the number of deity is 0, there should be no deity code."
                    "When there is no deity code, the number of deity should be 0.")))
             (when (char/= #\0 num)
               (setf dei (read-char inp nil nil))
               (unless (deityp                dei) (invalid-religion-code rel "Invalid deity code: '~A'" dei)))
             (let ((aft (read-char inp))
                   (org (read-char inp))
                   (das (read-char inp))
                   (bub (read-char inp))
                   (pol (read-char inp))
                   (spe (read-char inp))
                   (pla (read-char inp)))
               (unless (after-death-p         aft) (invalid-religion-code rel "Invalid after-death code: '~A'" aft))
               (unless (organizationp         org) (invalid-religion-code rel "Invalid organization code: '~A'" org))
               (unless (char= #\-             das) (invalid-religion-code rel "Invalid religion code, it must have a dash in the middle, not '~A'" das))
               (unless (bubbap                bub) (invalid-religion-code rel "Invalid bubba code: '~A'" bub))
               (unless (policyp               pol) (invalid-religion-code rel "Invalid policy code: '~A'" pol))
               (unless (number-of-spellings-p spe) (invalid-religion-code rel "Invalid number of spellings code: '~A'" spe))
               (unless (placep                pla) (invalid-religion-code rel "Invalid place of cult code: '~A'" pla))
               (values num dei aft org bub pol spe pla)))))
        (otherwise
         (invalid-religion-code rel "Length is ~D, expected 8 or 9"))))))

(defun code-valid-p (rel)
  (handler-bind ((error (lambda (condi) (declare (ignore condi)) (return-from code-valid-p nil))))
    (explode-religion rel)
    t))

(defmacro with-religion-code ((num dei aft org bub pol spe pla) rel &body body)
  `(multiple-value-bind (,num ,dei ,aft ,org ,bub ,pol ,spe ,pla) (explode-religion ,rel) ,@body))

(defun get-label-for-code (code enum)
  (let ((line (find (normalize-enumeration-constant enum  code)
                    (enumeration-constants enum)
                    :key (function line-code)
                    :test (function equal))))
    (when line
      (line-label line))))

(defun wrap-long-string (long-string column-width)
  (let ((cw (list column-width)))
    (setf (cdr cw) cw)
    (format nil "~{~<~%~1,v:;~a~>~^ ~}"
            (mapcan (function list) cw (split-string long-string " ")))))

(defun print-section (indent line-width title text)
  (let ((lines (split-string (wrap-long-string text (- line-width indent 1)) #(#\newline))))
    (format t " ~VA ~A~%" indent title (first lines))
    (dolist (line (rest lines))
      (format t " ~VA ~A~%" indent "" line))))


(defun show-religion-names (indent line-width code names)
  (format t " ~VA ~A~%" indent "RELIGION CODE:" code)
  (when names
    (print-section indent line-width
                   (format nil "RELIGION NAME~:(~P~):" (length names))
                   (format nil "~{~A~^ ~}" names))))


(defun explain (code)
  (unless (code-valid-p code)
    (format t "~A: Invalid religion code '~A'.~%" *program-name* code)
    (format t "~VA  Please. use format: lnl-nlnl ~%~
               ~0@*~VA  with 'l' a majuscule letter and 'n' a digit.~%"
            (length *program-name*) "")
    (ext:exit 2))
  (with-religion-code
      (num dei aft org bub pol spe pla) code
      (let* ((line " -------------------------------------------------------------------")
             (line-width (length line))
             (indent     30))
        (format t "~A~%" line)
        (let ((code (implode-religion num dei aft org bub pol spe pla)))
          (show-religion-names indent line-width  code (lookup-names code)))
        (format t " RELIGION CHARACTERISTICS: ~%")
        (flet ((describe-code (title code enum errorfmt)
                 (let ((lab (get-label-for-code code enum)))
                   (if lab
                       (print-section indent line-width title lab)
                       (format t errorfmt code)))))
          (print-section indent line-width "NUMBER OF DEITIES:" (princ-to-string num))
          (when dei
           (describe-code "DEITY:"             dei  'deity       "INVALID DEITY CODE '~A'.~%"))
          (describe-code "AFTER DEATH:"       aft  'after-death "INVALID AFTER DEATH CODE '~A'.~%")
          (cond
            ((string= "?"  org) (format t " ~VA ~A~%"     indent "ORGANIZATION LEVEL:""Unknown."))
            ((upper-case-p org) (format t " ~VA ~A ~A~%"  indent "ORGANIZATION LEVEL:"org "(A = VERY ORGANIZED, Z = CHAOS)."))
            (t (format t  "INVALID ORGANIZATION LEVEL CODE '~A' (SHOULD BE AN UPPER CASE LETTER).~%"org)))
          (cond
            ((string= "?"  bub) (format t " ~VA ~A~%"     indent "% OF MEMBERS NAMED BUBBA:""Unknown."))
            ((digit-char-p bub) (format t " ~VA ~A %~%"   indent "% OF MEMBERS NAMED BUBBA:"(round (* 100/9 (digit-char-p bub)))))
            (t (format t  "INVALID BUBBA CODE '~A' (SHOULD BE A DIGIT).~%"bub)))
          (describe-code "POLICY:"            pol  'policy      "INVALID POLICY CODE '~A'.~%")
          (cond
            ((string= "?"  spe) (format t " ~VA ~A~%"     indent "NUM OF SPELLINGS OF NAME:""Unknown."))
            ((digit-char-p spe) (format t " ~VA ~A~%"     indent "NUM OF SPELLINGS OF NAME:"spe))
            (t (format t  "INVALID SPELLINGS NUMBER '~A' (SHOULD BE A DIGIT).~%"spe)))
          (describe-code "PLACE OF CULT:"     pla  'place       "INVALID PLACE OF CULT CODE '~A'.~%"))
        (format t "~A~%"line))))



(defun choose (message enum)
  (let* ((line " -----  ------------------------------------------------------------")
         (line-width (length line)))
    (loop
       (format t "~2%~A~%" line)
       (format t " CODE   DESCRIPTION~%")
       (format t "~A~%" line)
       (dolist (line (enumeration-constants enum))
         (print-section 8 line-width (string (line-code line)) (line-label line)))
       (format t "~A~%" line)
       (format t "~A" message)
       (finish-output)
       (let* ((code (read-answer))
              (lab (get-label-for-code code enum)))
         (when code
           (return-from choose code))
         (format t " INVALID CODE '~A'.~%" code)
         (finish-output)
         (sleep 1)))))

(defun choose-something (message what what-label)
  (loop
     (format t "~2%~A" message)
     (finish-output)
     (let* ((input (read-answer))
            (code  (funcall what input)))
       (when code
         (return-from choose-something code))
       (format t " INVALID CODE '~A'. SHOULD BE ~A OR '?'.~%" input what-label)
       (finish-output)
       (sleep 1))))

(defun choose-letter (message)
  (choose-something message
                    (lambda (input)
                      (cond ((string= "?" input)            #\?)
                            ((upper-case-p (aref input 0)) (aref input 0))
                            (t                             nil)))
                    "AN UPPER CASE LETTER"))

(defun choose-digit (message)
  (choose-something message
                    (lambda (input)
                      (cond ((string= "?" input)            #\?)
                            ((digit-char-p (aref input 0)) (aref input 0))
                            (t                             nil)))
                    "A DIGIT"))

(defun choose-bubba (message)
  (choose-something message
                    (lambda (input)
                      (cond ((string= "?" input) #\?)
                            ((let ((b (parse-integer input :junk-allowed t)))
                               (if (<= 0 b 100)
                                   (round (* 9/10 b) 10)
                                   (format t " PERCENTAGE TOO BIG ~A% !. SHOULD BE BETWEEN 0 and 100, OR '?'.~%" b))))
                            (t (format t " INVALID PERCENTAGE '~A'. SHOULD BE BETWEEN 0 and 100, OR '?'.~%" input))))
                    "A PERCENTAGE"))


(defun interactive ()
  (let* ((line " -------------------------------------------------------------------")
         (line-width (length line))
         (indent 30)
         code
         name)
    (loop :do
       (format t "~A~%" line)

       (format t " LET'S SEE WHAT KIND OF RELIGION YOU WANT...~%")
       (format t "~%")
       (let* ((num (choose-digit  " PLEASE CHOOSE A NUMBER OF DEITIES: "))
              (dei (unless (char= #\0 num)
                     (choose        " PLEASE CHOOSE A DEITY: "        'deity)))
              (aft (choose        " PLEASE CHOOSE AN AFTER DEATH: " 'after-death))
              (org (choose-letter " PLEASE CHOOSE AN ORGANIZATION LEVEL (A=VERY ORGANIZED, Z=CHAOS): "))
              (bub (choose-bubba  " PLEASE CHOOSE A BUBBA PERCENTAGE: "))
              (pol (choose        " PLEASE CHOOSE A POLICY: "       'policy))
              (spe (choose-digit  " PLEASE CHOOSE A NUMBER OF SPELLINGS: "))
              (pla (choose        " PLEASE CHOOSE A PLACE OF CULT: " 'place)))
         (setf code (implode-religion num dei aft org bub pol spe pla))
         (explain code)
         (finish-output))
       :until (yes-or-no-p  " ARE SATISFIED WITH YOUR RELIGION? "))
    (let ((names (lookup-names code)))
      (if (null names)
          (loop
             (format t " PLEASE ENTER A NAME FOR YOUR RELIGION: ")
             (finish-output)
             (setf name (read-answer))
             (show-religion-names indent line-width code (list name))
             (when (yes-or-no-p  " IS THAT CORRECT? ")
               (push (list code name) *religion-db*)
               (save-religion-database *religion-db* *religion-pathname*)
               (format t "~A~%" line)
               (format t " REGISTERED YOUR NEW RELIGION:~%")
               (show-religion-names indent line-width code (list name))
               (format t "~A~%" line)
               (return-from interactive)))
          (progn
            (format t "~A~%" line)
            (format t " YOU CHOSED THIS RELIGION:~%")
            (show-religion-names indent line-width code names)
            (format t "~A~%" line))))))



(define-option ("-g" "--generate") ()
  "Generate a random religion."
  (princ (generate)) (terpri)
  (finish-output))


(define-option ("-G" "--generate-and-explain") ()
  "Generate and explain random religion.
The code is generated and output to stderr,
while the explainations are written to stdout."
  (let ((religion-code (generate)))
    (princ religion-code *error-output*) (terpri *error-output*)
    (finish-output *error-output*)
    (explain religion-code)))

(define-option ("-e" "--explain") (religion-code)
  "Explain a religion code."
  (cond
    ((code-valid-p religion-code) (explain religion-code))
    ((lookup-code religion-code)  (explain (lookup-code religion-code)))
    (t                            (explain religion-code))))

(define-option ("-i" "--interactive") ()
  "Creates a religion interactively."
  (interactive))

(define-option ("-l" "--list") ()
  "List known religions."
  (loop
     :for (code name) :in (sort *religion-db* (function string<) :key (function line-code))
     :initially (progn
                  (format t " ~9D  ~:(~A~)~%" "---------" "----------------------------------------")
                  (format t " ~9D  ~:(~A~)~%" "Code" "Name")
                  (format t " ~9D  ~:(~A~)~%" "---------" "----------------------------------------"))
     :do (format t " ~9D  ~:(~A~)~%" code name)
     :finally (format t " ~9D  ~:(~A~)~%" "---------" "----------------------------------------")))


(define-option ("-V" "--version") ()
  "Prints the version of this script."
  (format t "~A version 2.0~%Running on ~A ~A~%"
          *program-name*
          (lisp-implementation-type) (lisp-implementation-version)))

(define-option ("-C" "--copyright") ()
  "Prints the version of this script."
  (format t "~A copyright and license:~%" *program-name*)
  (format t "
    Copyright Pascal J. Bourguignon 2002 - 2010

    mailto:pjb@informatimago.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"))


(parse-options ext:*args*)
;;;; THE END ;;;;
