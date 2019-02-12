;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:              shell
;;;;LANGUAGE:          emacs lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This script emulates a shell such as can be seen in movies.
;;;;    <sillyness on>
;;;;USAGE
;;;;    shell
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2004-08-25 <PJB> Updated.
;;;;    2002-09-21 <PJB> Created (Tron).
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2004
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
;;;;******************************************************************************

(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))

(defpackage "COM.INFORMATIMAGO.CINE-SHELL"
  (:use "COMMON-LISP" "SCRIPT"))
(in-package "COM.INFORMATIMAGO.CINE-SHELL")
(defparameter *program-version* "0.0.2")

#|


Ce film n'ayant pas encore,
pour des raisons techniques
reçu le visa d'exploitation,
les spectateurs sont invités
    lors de sa projection
à vérifier s'il est autorisé
    pour tous publics.

|#


(define-option ("version" "-V" "--version") ()
  "Report the version of this script and the underlying package system."
  (format t "~A ~A~%" *program-name* *program-version*))



;; (pushnew "--test" ext:*args*)


;;;---------------------------------------------------------------------
;;;
;;;---------------------------------------------------------------------

(defparameter *lines*   24)
(defparameter *columns* 80)



(defparameter *CYAN-BACK*       "[46m")
(defparameter *MAGENTA-BACK*    "[45m")
(defparameter *BLUE-BACK*       "[44m")
(defparameter *YELLOW-BACK*     "[43m")
(defparameter *GREEN-BACK*      "[42m")
(defparameter *RED-BACK*        "[41m")
(defparameter *BLACK-BACK*      "[40m")
(defparameter *WHITE-BACK*      "[47m")
(defparameter *WHITE*           "[37m")
(defparameter *CYAN*            "[36m")
(defparameter *MAGENTA*         "[35m")
(defparameter *BLUE*            "[34m")
(defparameter *YELLOW*          "[33m")
(defparameter *GREEN*           "[32m")
(defparameter *RED*             "[31m")
(defparameter *BLACK*           "[30m")
(defparameter *NO-INVERT*       "[27m")
(defparameter *NO-BLINK*        "[25m")
(defparameter *NO-UNDERLINE*    "[24m")
(defparameter *NO-BOLD*         "[22m")
(defparameter *INVERT*          "[7m")
(defparameter *BLINK*           "[5m")
(defparameter *UNDERLINE*       "[4m")
(defparameter *BOLD*            "[1m")
(defparameter *NORMAL*          "[0m")
(defparameter *GOTO-HOME*       "")
(defparameter *CLEAR-HOME*      "")
(defparameter *CLEAR-SCREEN*    "c")
(defparameter *ISO6429-ICH*     "[@")
(defparameter *ISO6429-CUU*     "[A")
(defparameter *ISO6429-CUD*     "[B")
(defparameter *ISO6429-CUF*     "[C")
(defparameter *ISO6429-CUB*     "[D")
(defparameter *ISO6429-CUP*     "[H")
(defparameter *ISO6429-ED*      "[J")
(defparameter *ISO6429-EL*      "[K")
(defparameter *ISO6429-IL*      "[L")
(defparameter *ISO6429-DL*      "[M")
(defparameter *ISO6429-DCH*     "[P")
(defparameter *ISO6429-SM*      "[h")
(defparameter *ISO6429-RM*      "[l")
(defparameter *CSI*             "[")


(defun move (l c) (format t "~A~D;~DH" *CSI* l c))
(defun addch (ch) (format t "~C" ch))

(defun attron (&rest attribs)
  (format t "~{~A~}"
          (mapcar (lambda (attrib)
                    (second (assoc attrib
                                   `((:CYAN-BACK      ,*CYAN-BACK*)
                                     (:MAGENTA-BACK   ,*MAGENTA-BACK*)
                                     (:BLUE-BACK      ,*BLUE-BACK*)
                                     (:YELLOW-BACK    ,*YELLOW-BACK*)
                                     (:GREEN-BACK     ,*GREEN-BACK*)
                                     (:RED-BACK       ,*RED-BACK*)
                                     (:BLACK-BACK     ,*BLACK-BACK*)
                                     (:WHITE-BACK     ,*WHITE-BACK*)
                                     (:WHITE          ,*WHITE*)
                                     (:CYAN           ,*CYAN*)
                                     (:MAGENTA        ,*MAGENTA*)
                                     (:BLUE           ,*BLUE*)
                                     (:YELLOW         ,*YELLOW*)
                                     (:GREEN          ,*GREEN*)
                                     (:RED            ,*RED*)
                                     (:BLACK          ,*BLACK*)
                                     (:NO-INVERT      ,*NO-INVERT*)
                                     (:NO-BLINK       ,*NO-BLINK*)
                                     (:NO-UNDERLINE   ,*NO-UNDERLINE*)
                                     (:NO-BOLD        ,*NO-BOLD*)
                                     (:INVERT         ,*INVERT*)
                                     (:BLINK          ,*BLINK*)
                                     (:UNDERLINE      ,*UNDERLINE*)
                                     (:BOLD           ,*BOLD*)
                                     (:NORMAL         ,*NORMAL*)))))
                  attribs)))

(defun attroff (&rest attribs)
  (format t "~{~A~}"
          (mapcar (lambda (attrib)
                    (second (assoc attrib
                                   `((:CYAN-BACK      ,*CYAN-BACK*)
                                     (:MAGENTA-BACK   ,*MAGENTA-BACK*)
                                     (:BLUE-BACK      ,*BLUE-BACK*)
                                     (:YELLOW-BACK    ,*YELLOW-BACK*)
                                     (:GREEN-BACK     ,*GREEN-BACK*)
                                     (:RED-BACK       ,*RED-BACK*)
                                     (:BLACK-BACK     ,*BLACK-BACK*)
                                     (:WHITE-BACK     ,*WHITE-BACK*)
                                     (:WHITE          ,*WHITE*)
                                     (:CYAN           ,*CYAN*)
                                     (:MAGENTA        ,*MAGENTA*)
                                     (:BLUE           ,*BLUE*)
                                     (:YELLOW         ,*YELLOW*)
                                     (:GREEN          ,*GREEN*)
                                     (:RED            ,*RED*)
                                     (:BLACK          ,*BLACK*)
                                     (:NO-INVERT      ,*INVERT*)
                                     (:NO-BLINK       ,*BLINK*)
                                     (:NO-UNDERLINE   ,*UNDERLINE*)
                                     (:NO-BOLD        ,*BOLD*)
                                     (:INVERT         ,*NO-INVERT*)
                                     (:BLINK          ,*NO-BLINK*)
                                     (:UNDERLINE      ,*NO-UNDERLINE*)
                                     (:BOLD           ,*NO-BOLD*)
                                     (:NORMAL         ,*NORMAL*)))))
                  attribs)))

(defparameter *reset*
  (format nil "~{~A~}" (list *CLEAR-SCREEN* *BLACK-BACK* *CYAN* *BOLD* *ISO6429-ED*)))

(defun reset (params)
  (format t "~A~{~A~}~A"
          *clear-screen*
          (mapcar (lambda (p) (symbol-value (intern (format nil "*~A*" p)))) params)
          *iso6429-ed*))




;;;---------------------------------------------------------------------
;;; KERNEL
;;;---------------------------------------------------------------------



(defstruct (shell (:type list)) name reset screen function)

(defparameter *shells*
  '(("Die Hard Control Data" (black-back cyan  bold)  die-hard-control-data  nil)
    ("Die Hard LAPD"         (black-back yellow)      die-hard-lapd          nil)
    ("Capricorn One"         (black-back green bold)  capricorn-one          nil)
    ("Abraxas"               (black-back white)       abraxas                nil)
    ("Matrix"                (black-back green)       nil                    matrix-shell)
    ("Tron"                  (black-back cyan  bold)  nil                    tron-shell)))



(defun print-screen (text)
  (let ((height (count (code-char 10) text)))
    (if (< height 24)
        (format t "~V%~A~V%" (/ (- 24 height) 2) text  (/ (- 24 height) 2))
        (format t "~A" text))))


(defun run-cine-shell ()
  (setf *random-state* (make-random-state t))
  (let ((sp (elt *shells* (random (length *shells*)))))
    (setf sp (elt *shells* 4)) ;; TEST
    (reset (shell-reset   sp))
    (when (shell-screen   sp) (print-screen (eval (shell-screen sp))))
    (when (shell-function sp) (funcall (shell-function sp)))))


(defun run-cine-server (port)
  (let ((listener (socket:socket-server 7767)))
    (unwind-protect
        (loop
           (socket:socket-wait listener)
           (let ((terminal (socket:socket-accept listener)))
             (format t "Connection from: ~a~%"
                     (values (socket:socket-stream-peer terminal)))
             (sleep 0.3)
             (let ((*standard-output* terminal)
                   (*standard-input*  terminal))
               (run-cine-shell))
             (sleep 2)
             (close terminal)))
      (socket:socket-server-close listener))))




(define-option ("list-shells" "-ls" "--list") ()
  "List the known cine shells."
  (format t "~:{~A~%~}~%" *shells*))

(define-option ("random" "-r" "--random") (port)
  "Run a random cine shell."
  (run-cine-shell))

(define-option ("sh" "-s" "--shell") ()
  "Run a sh-shell."
  (sh-shell))

(define-option ("listen" "-l" "--listen") (port)
  "Listen to the PORT for incoming shell connections."
  (run-cine-server port))




;;;---------------------------------------------------------------------
;;; String Utilities
;;;---------------------------------------------------------------------

(defun stream-to-string-list (stream)
  (loop
     :with line = (read-line stream nil nil)
     :while line
     :collect line into result
     :do (setq line (read-line stream nil nil))
     :finally (return result)))


(defun copy-stream (src-stream dst-stream)
  (loop
     :with line = (read-line src-stream nil nil)
     :while line
     :do (write-line line dst-stream)))


(defun string-replace (string regexp replace &optional fixedcase literal)
  "
RETURN: a string build from `string' where all matching `regexp'
        are replaced by the `replace' string.
NOTE:   Current implementat accepts only literal pattern as `regexp';
        `fixedcase' and `literal' are ignored.
"

  (loop with regexp-length = (length regexp)
     with result = ""
     with previous = 0
     with position = (search regexp string)
     while position
     do (setq result (concatenate 'string
                       result (subseq string previous position) replace)
              previous (+ position regexp-length)
              position (search regexp string :start2 previous))
     finally (setq result (concatenate 'string
                            result
                            (subseq string previous (length string))))
     finally (return result)))


(defun split-string (string &optional separators)
  "
NOTE:   current implementation only accepts as separators
        a string containing only one character.
"
  (let ((sep (aref separators 0))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)))
    (loop
       :while (< position strlen)
       :do (progn
            (loop
               :while (and (< nextpos strlen)
                           (char/= sep (aref string nextpos)))
               :do (setq nextpos (1+ nextpos)))
            (push (subseq string position nextpos) chunks)
            (setq position (1+ nextpos))
            (setq nextpos  position)))
    (nreverse chunks)))


(defun split-name-value (string)
  "
RETURN:  a cons with two substrings of string such as:
         (string= (concat (car res) \"=\" (cdr res)) string)
         and (length (car res)) is minimum.
"
  (let ((position 0)
        (strlen   (length string)))
    (loop
       :while (and (< position strlen)
                   (char/= (character "=") (aref string position)))
       :do (setq position (1+ position)))
    (if (< position strlen)
      (cons (subseq string 0 position) (subseq string (1+ position) strlen))
      nil)))



;;;---------------------------------------------------------------------
;;; Syntax parsing
;;;---------------------------------------------------------------------

(defstruct (syntax (:type list))  kind token help-string function children)


(defun syntax-functions (syntax)
  (if (syntax-function syntax)
      (cons (syntax-function syntax)
            (mapcan (function syntax-functions) (syntax-children syntax)))
      (mapcan (function syntax-functions) (syntax-children syntax))))


(defun syntax-all-functions (syntax)
  "
RETURN: A list with all the functions decorating the syntax tree.
"
  (sort (delete-duplicates (delete nil (syntax-functions)))
        (function string-lessp)))


(defun syntax-expected (syntax)
  "
RETURN: A list of keywords or data item expected after this syntax node.
"
  (flatten
   (mapcar (lambda (child)
             (cond
               ((eq :tok (syntax-kind child))
                (syntax-token child))
               ((eq :var (syntax-kind child))
                (let ((var (substring (symbol-name (syntax-token child)) 1)))
                  (format nil "A~a ~a"
                          (if (member (string-to-char var)
                                      (mapcar 'character
                                              '(\a \e \i \o \y \A \E \I \O \Y))
                                      ) "n" "")
                          var))
                )
               (t (error "Unexpected kind of child: ~S" (syntax-kind child)))
               ))
           (syntax-children syntax))))


(defun syntax-find-child-matching-token (syntax token)
  "
PRE:    (not (member token '( \? help )))
"
  (let ((children (syntax-children syntax))
        (kind)
        (syn-tokens)
        (child nil))
    (loop while children do
         (setf kind (syntax-kind (car children)))
         (cond
           ((eq kind :tok)
            (setf syn-tokens (syntax-token (car children)))
            (if (if (listp syn-tokens)
                    (member token syn-tokens :test (function string-equal))
                    (string-equal token syn-tokens))
                (setq child (car children)
                      children nil)
                (setq children (cdr children))))
           ((eq kind :var)
            (setq child (car children)
                  children nil))
           (t (error "Unexpected kind of child: ~S" (syntax-kind child)))))
    child))


(defun syntax-walk-path-to-child (syntax path)
  "
RETURN:  (cons {the child found by walking the path down the syntax tree}
               (cons {the rest of the path that could not be matched}
                     {an alist of accumulated variables} ))
"
  (if (and path (not (string= "?" (car path))))
      (let ((child (syntax-find-child-matching-token syntax (car path))))
        (if child
            (let ((sub-result (syntax-walk-path-to-child child (cdr path))))
              (if (eq :var (syntax-kind child))
                  (list (car sub-result)
                        (cadr sub-result)
                        (cons (cons (syntax-token child)
                                    (car path))
                              (nth 2 sub-result)))
                  sub-result))
            (list syntax path)))
      (list syntax path)))


(defun syntax-find-path-to-child (syntax child)
  "
RETURN:  A path leading to the child node.
"
  (let ((children (syntax-children syntax)) ;; remove the root node
        (sub-path-to-child)
        (result nil))
    (cond ((eq syntax child)   nil)
          ((member child children)  (cons (syntax-token child) nil))
          (t  (loop while children do
                   (setf sub-path-to-child
                         (syntax-find-path-to-child (car children) child))
                   (if sub-path-to-child
                       (setq result   (cons (syntax-token (car children))
                                            sub-path-to-child)
                             children nil)
                       (setq children (cdr children))))
              result))))


(defun syntax-generate-all-paths (syntax)
  "
RETURN:  A list of all the path that can be generated from the root of the
         syntax tree.
"
  (let ((children (syntax-children syntax))) ;; remove the root node.
    (if (null children)
      (setq result (list nil))
      (loop
         :with result = nil
         :while children
         :do (let ((sub-result)
                   (sub-paths (syntax-generate-all-paths (car children)))
                   (cur-token (syntax-token              (car children))) )
               (if (listp cur-token)
                 (loop
                    :while cur-token
                    :do (progn
                          (setq sub-result sub-paths)
                          (loop
                             :while sub-result
                             :do (setq result (cons
                                               (cons (car cur-token)  (car sub-result))
                                               result)
                                       sub-result (cdr sub-result)))
                          (setq cur-token (cdr cur-token))))
                 (loop
                    :while sub-paths
                    :do (setq result (cons
                                      (cons cur-token (car sub-paths))
                                      result)
                              sub-paths (cdr sub-paths))))
               (setq children (cdr children)))
         :finally (return (nreverse result))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SH -- Movie Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sh-syntax*
  '(:tok SHELL    "MOVIE SHELL." nil
    ((:tok REQUEST  "REQUESTS STUFF." nil
      ((:tok ACCESS "REQUEST ACCESS." nil
             ((:tok TO "REQUEST ACCESS." nil
                    ((:var :name "REQUEST ACCESS TO NAMED RESOURCE." nil
                           ((:tok PROGRAM "REQUEST ACCESS TO NAMED PROGRAM."
                                  sh-request-access-to-named-program)
                            ((:tok DATABASE "REQUEST ACCESS TO NAMED DATABASE."
                                   sh-request-access-to-named-database))))))))))
     (:tok CODE "INTRODUCE A CODE." nil
      ((:var :code "INTRODUCE A CODE." nil
             ((:tok PASSWORD "INTRODUCE A CODE PASSWORD." nil
                    ((:tok TO "INTRODUCE A CODE PASSWORD." nil
                           ((:tok MEMORY
                                  "INTRODUCE A CODE PASSWORD INTO MEMORY" nil
                                  ((:var :address "INTRODUCE A CODE PASSWORD INTO MEMORY."
                                         sp-code-password-to-memory)))))))))))
     (:tok HELP "GIVES SOME HELP." sh-help)
     (:tok ( EXIT  QUIT ) "EXITS THE SHELL."  sh-exit)))) ;;*sh-syntax*


(defun SH-REQUEST-ACCESS-TO-NAMED-PROGRAM (&key name)
  )


(defparameter *memory* (make-hash-table))


(defun SP-CODE-PASSWORD-TO-MEMORY  (&key code address)
  (setf (gethash address *memory*) code))


(defun sh-help ()
  "Gives some help."
  (format t "~&~{~{~A~12T ~@{~A~^ ~}~}~%~}"  (syntax-generate-all-paths *sh-syntax*)))


(defun SH-EXIT ()
  (throw :exit nil))


(defun sh-shell ()
  (catch :exit
    (loop named :shell do
         (format t "> ")
         (let ((command (read-line t nil nil)))
           (if command
               (let* ((words   (split-string command " "))
                      (nrv     (syntax-walk-path-to-child *sh-syntax* words))
                      (node    (first  nrv))
                      (rest    (second nrv))
                      (vars    (third  nrv)))
                 (cond
                   ((syntax-function node)
                    (apply (syntax-function node)
                           (mapcan (lambda (b) (list (car b) (cdr b))) vars)))
                   ((syntax-help-string node)
                    (format t "~&~A~%" (syntax-help-string node)))
                   (t (format t "~&~A~%" "MAKE MY DAY!"))))
               (loop-finish)))))
  (format t "~8%   GOOD BYE, M. DILLINGER.~8%")) ;;shell




;;;---------------------------------------------------------------------
;;; MATRIX
;;;---------------------------------------------------------------------

(defun matrix-transopt-date ()
  "Return the current date in Matrix call trans opt format."
  (multiple-value-bind (sec min hou day mon yea) (get-decoded-time)
    (format nil " ~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            mon day (mod yea 100) hou min sec))) ;;matrix-transopt-date


(defstruct (mat (:type list)) val bold)


(defun matrix-init ()
  (let ((matrix  (make-array (list (1+ *lines*) *columns*)))
        (updates (make-array (list *columns*) :element-type 'fixnum
                             :initial-element 0))
        (spaces  (make-array (list *columns*) :element-type 'fixnum
                             :initial-element 0))
        (length  (make-array (list *columns*) :element-type 'fixnum
                             :initial-element 0)))
    (loop for j from 0 below *columns* by 2 do
         (loop for i from 0 to *lines* do
              (setf (aref matrix i j) (make-mat :val nil :bold 0)))
         (setf (aref updates j) (+ 1 (random 3))
               (aref spaces  j) (+ 1 (random *lines*))
               (aref length  j) (+ 3 (random (- *lines* 3)))
               (mat-val (aref matrix 1 j)) (character " ")))
    (values matrix updates spaces length))) ;;matrix-init


(defun matrix-shell ()
  (format t "Call trans opt: received. ~A REC:Log>" (matrix-transopt-date))
  (finish-output)
  (sleep 2)
  (format t "~C~ATrace program: running~%" (code-char 13) *iso6429-el*)
  (multiple-value-bind (matrix updates spaces length) (matrix-init)
    (let ((count 1)
          (*randnum*  93)
          (*randmin*  33)
          (*highnum* 123))
      (loop
         (incf count)
         (when (< 4 count) (setf count 1))
         (loop for j from 0 below *columns* by 2 do
              (if (< (aref updates j) count)
                  (when (and (null (mat-val (aref matrix 0 j)))
                             (char= (character " ") (mat-val (aref matrix 1 j))))
                    (if (< 0 (aref spaces j))
                        (decf (aref spaces j))
                        (progn
                          (setf (aref length j) (+ 3 (random (- *lines* 3))))
                          (setf (mat-val (aref matrix 0 j))
                                (code-char (+ *randmin* (random *randnum*))))
                          (when (zerop (random 2))
                            (setf (mat-bold (aref matrix 0 j)) 2))
                          (setf (aref spaces j) (+ 1 (random *lines*)))))
                    (loop with i = 0
                       with y = 0
                       with z = 0
                       with first-col-done = nil
                       while (<= i *lines*) do
                       ;; Skip spaces
                       (loop while (and (<= i *lines*)
                                        (or (not (mat-val (aref matrix i j)))
                                            (char= (character " ")
                                                   (mat-val (aref matrix i j)))))
                          do (incf i))
                       (when (< *lines* i) (loop-finish))
                       ;; Go to the head of this collumn
                       (setf z i y 0)
                       (loop while (and (<= i *lines*)
                                        (or (not (mat-val (aref matrix i j)))
                                            (char= (character " ")
                                                   (mat-val (aref matrix i j)))))
                          do (incf i) (incf y))
                       (if (< *lines* i)
                           (setf (mat-val  (aref matrix z j)) (character " ")
                                 (mat-bold (aref matrix z j)) 1)
                           (progn
                             (setf (mat-val (aref matrix i j))
                                   (code-char (+ *randmin* (random *randnum*))))
                             (when (= 2 (mat-bold (aref matrix (1- i) j)))
                               (setf (mat-bold (aref matrix (1- i) j)) 1
                                     (mat-bold (aref matrix i j)) 2))
                             ;; If we're at the top of the column and
                             ;; it's reached its full length (about to
                             ;; start moving down), we do this to get it
                             ;; moving.  This is also how we keep segments not
                             ;; already growing from growing accidentally =>
                             (when (or (< (aref length j) y)  first-col-done)
                               (setf (mat-val (aref matrix z j)) (character " ")
                                     (mat-val (aref matrix 0 j)) nil))
                             (setf first-col-done t)
                             (incf i))))
                    ;; Hack =P
                    (setf y 1 z *lines*)
                    (loop for i from y to z do
                         (move (- i y) j)
                         (if (or (and  (mat-val (aref matrix i j))
                                       (zerop (char-code (mat-val (aref matrix i j)))))
                                 (= 2 (mat-bold (aref matrix i j))))
                             (progn
                               (attron :white :bold)
                               (if (and  (mat-val (aref matrix i j))
                                         (zerop (char-code (mat-val (aref matrix i j)))))
                                   (addch (character "&"))
                                   (addch (mat-val (aref matrix i j))))
                               (attroff :white :bold))
                             (progn
                               (attron :green)
                               (if (and  (mat-val (aref matrix i j))
                                         (= 1 (char-code (mat-val (aref matrix i j)))))
                                   (progn
                                     (attron :bold)
                                     (addch (character "|"))
                                     (attroff :bold))
                                   (progn
                                     (if (zerop (random 2))
                                         (progn
                                           (attron :bold)
                                           (if (null (mat-val (aref matrix i j)))
                                               (addch (character " "))
                                               (addch (mat-val (aref matrix i j))))
                                           (attroff :bold))
                                         (if (null (mat-val (aref matrix i j)))
                                             (addch (character " "))
                                             (addch (mat-val (aref matrix i j)))))))))))))))))


;;;---------------------------------------------------------------------
;;; TRON
;;;---------------------------------------------------------------------


(defun tron-mcp-date ()
  "Return the current date in MCP format"
  (multiple-value-bind (sec min hou day mon) (get-decoded-time)
    (format nil "~A ~2,'0D,  ~2,'0D:~2,'0D:~2,'0D ~:[AM~;PM~]"
            (elt '(nil "JANU" "FEBR" "MARC" "APRI" "MAY " "JUNE" "JULY" "AUGU"
                   "SEPT" "OCTO" "NOVE" "DECE") mon)
            day hou min sec (<= 12 hou))))


(defun tron-shell ()
  (format t "~6%
             ~A

                       YOUR ACCESS SUSPENDED
                       PLEASE REPORT TO DILLINGER
                       IMMEDIATELY
                       AUTHORIZATION: MASTER CONTROL
                       PROGRAM

                       END OF LINE ~6%" (tron-mcp-date))
  (finish-output))


(defparameter tron "
C     REQUEST ACCESS TO CLU PROGRAM.
C     CODE 6 PASSWORD TO MEMORY 0222.

C     REQUEST STATUS REPORT ON MISSING DATA.

M     ILLEGAL CODE...
M     CLU PROGRAM DETACHED FROM SYSTEM.

C     REQUEST ACCESS TO CLU PROGRAM.
C     LAST LOCATION: HIGH CLEARANCE MEMORY.

C     REQUEST ACCESS TO MASTER CONTROL PROGRAM.
C     USER CODE 00-DILLINGER PASSWORD:MASTER.

M     HELLO MR DILLINGER THANKS FOR COMING BACK EARLY.

M     IT'S YOUR FRIEND THE BOY DETECTIVE
M     HE'S NOSING AROUND AGAIN.
M     YES.IT FELT LIKE FLYNN.
M     END OF LINE.


M     ADDRESS FILE EMPTY...
M     TRON PROGRAM UNAVAILABLE

C     REQUEST:
C             MASTER CONTROL PROGRAM
C             RELEASE TRON JA 307020
C             I HAVE PRIORITY ACCESS 7

M     SEPT 22,  18:32:21 PM
M
M               YOUR ACCESS SUSPENDED
M               PLEASE REPORT TO DILLINGER
M               IMMEDIATELY
M               AUTHORIZATION: MASTER CONTROL
M               PROGRAM
M
M               END OF LINE


M    MISTER DILLIGER I'M SO VERY DISAPPOINTED IN YOU.
D    I'm sorry.
M    I CAN'T AFFORD HAVING AN INDEPENDENT PROGRAM MONITORING ME.
M    DO YOU REALIZE HOW MANY OUTSIDE SYSTEMS I'VE GONE INTO?
M    HOW MANY PROGRAMS I'VE APPROPRIATED?


C     SYSDAT  1039
C     MATTER TRANSFORM SEQUENCE

C     REQUEST:
C              ACCESS CODE 6
C              PASSWORD SERIES PS 17
C              REINDEER FLOTILLA

M     YOU SHOULDN'T HAVE COME BACK FLYNN

C     CODE SERIES LSU-120... ACTIVATE

M     THAT ISN'T GOING TO DO YOU ANY
M     GOOD, FLYNN.  I'M AFRAID YOU...


M     ENCOM MX 16-923 USER # 0176825 06:00 INFORMATION
M
M     VIDEO GAME PROGRAM: SPACE PARANOIDS
M     ANNEXED 9/22 BY E.DILLINGER
M     ORIGINAL PROGRAM WRITTEN BY K.FLYNN
M     THIS INFORMATION PRIORITY ONE
M     END OF LINE
"  "Tron -- 1982")




(defparameter |2001 A Space Odyssey|
  '((
"


                         ZERO GRAVITY TOILET

                      PASSENGERS ARE ADVISED TO

                     READ INSTRUCTIONS BEFORE USE



   1. The toilet is of the standard zero-gravity type. Depending on
      requirements, System A and/or System B can be used, details of
      which are clearly marked in the toilet compartment. When
      operating System A, depress lever and a plastic dalkron
      eliminator will be dispensed through the slot immediately
      underneath. When you have fastened the adhesive lip, attach
      connection marked by the large \"X\" outlet hose. Twist the silver
      coloured ring one inch below the connection point until you feel
      it lock.

   2. The toilet is now ready for use. The Sonovac cleanser is
      activated by the small switch on the lip. When securing, twist
      the ring back to its initial-condition, so that the two orange
      line meet. Disconnect. Place the dalkron eliminator in the
      vacuum receptacle to the rear. Activate by pressing the blue
      button.

   3. The controls for System B are located on the opposite wall. The
      red release switch places the uroliminator into position; it can
      be adjusted manually up or down by pressing the blue manual
      release button. The opening is self adjusting. To secure after
      use, press the green button which simultaneously activates the
      evaporator and returns the uroliminator to its storage position.

   4. You may leave the lavatory if the green exit light is on over
      the door. If the red light is illuminated, one of the lavatory
      facilities is not properly secured. Press the \"Stewardess\" call
      button on the right of the door. She will secure all facilities
      from her control panel outside. When gren exit light goes on
      you may open the door and leave. Please close the door behind
      you.

   5. To use the Sonoshower, first undress and place all your clothes
      in the clothes rack. Put on the velcro slippers located in the
      cabinet immediately below. Enter the shower. On the control
      panel to your upper right upon entering you will see a \"Shower
      seal\" button. Press to activate. A green light will then be
      illuminated immediately below. On the intensity knob select the
      desired setting. Now depress the Sonovac activation lever. Bathe
      normally.

   6. The Sonovac will automatically go off after three minutes unless
      you activate the \"Manual off\" over-ride switch by flipping it
      up. When you are ready to leave, press the blue \"Shower seal\"
      release button. The door will open and you may leave. Please
      remove the velcro slippers and place them in their container.

   7. If the red light above this panel is on, the toilet is in
      use. When the green light is illuminated you may enter. However,
      you must carefully follow all instructions when using the
      facilities duting coasting (Zero G) flight. Inside there are
      three facilities: (1) the Sonowasher, (2) the Sonoshower, (3)
      the toilet. All three are designed to be used under weightless
      conditions. Please observe the sequence of operations for each
      individual facility.

   8. Two modes for Sonowashing your face and hands are available, the
      \"moist-towel\" mode and the \"Sonovac\" ultrasonic cleaner
      mode. You may select either mode by moving the appropriate lever
      to the \"Activate\" position.

      If you choose the \"moist-towel\" mode, depress the indicated
      yellow button and withdraw item. When you have finished, discard
      the towel in the vacuum dispenser, holding the indicated lever
      in the \"active\" position until the green light goes on...showing
      that the rollers have passed the towel completely into the
      dispenser. If you desire an additional towel, press the yellow
      button and repeat the cycle.

   9. If you prefer the \"Sonovac\" ultrasonic cleaning mode, press the
      indicated blue button. When the twin panels open, pull forward
      by rings A & B. For cleaning the hands, use in this
      position. Set the timer to positions 10, 20, 30 or
      40...indicative of the number of seconds required. The knob to
      the left, just below the blue light, has three settings, low,
      medium or high. For normal use, the medium setting is suggested.

  10. After these settings have been made, you can activate the device
      by switching to the \"ON\" position the clearly marked red
      switch. If during the washing operation, you wish to change the
      settings, place the \"manual off\" over-ride switch in the \"OFF\"
      position. you may now make the change and repeat the cycle.



                2001: Zero Gravity Toilet Instructions
               Last modified: Mon May 21 15:34:24 2007
")))


(defparameter |The Six Million Dollar Man|
  '(("
BIONIC VISUAL CORTEX TERMINAL
CATALOG #075/KFB
33MM O.D. F/0.95
ZOOM RATIO: 20.2 TO 1
2134 LINE 60 HZ
EXTENDED CHROMATIC RESPONSE
CLASS JC
CLASSIFIED
"
     "
BIONIC NEURO-LINK FOREARM/
UPPER ARM ASSEMBLY (RIGHT)
CATALOG #2821/WLY
AND
BIONIC NEURO-LINK HAND (RIGHT)
CATALOG #2822/PJI
NEURO FEEDBACK TERMINATED
POWER SUPPLY
ATOMIC TYPE AED-4
CATALOG #2821 AED-4
1550 WATT CONTINUOUS DUTY
NOMINAL DOUBLE GAIN
OVERLOAD FOLLOWER
CLASS MZ
CLASSIFIED
"
     "
BIONIC NEUR-LINK
BIPEDAL ASSEMBLY
CATALOG #914 PAH
NEURO FEEDBACK TERMINATED
POWERSUPPLY:
ATOMIC TYPE AED-9A
4920 WATT CONTINUOUS DUTY
NOMINAL DOUBLE GAIN
OVERLOAD FOLLOWER
2100 WATT RESERVE
INTERMITTENT DUTY
CLASS CC
CLASSIFIED
")))


(defparameter |What is it?|
'((:green-screen "
ADDRESS          VALUE       TYPE     TS  BA  OV
<big><highlight>1A000  +0.0017 INT</highlight></big>
 A MODE       L MODE     SCALE        LD  HY  MC
<big><highlight>QT     CL     E0</highlight></big>

*[]

GENERAL PURPOSE LOGIC
0001234567  0101234567  0201234567  0301234567
0401234567  0501234567  0601234567  0701234567
1001234567  1101234567  1201234567  1301234567

SENSE LINES             CONTROL LINES
0001234567  0101234567  0001234567  0101234567

INPNT TRUNK             INTERRUPTS    TIMER
0001234567              0001234567     012

"
; the first two digits (first one for TIMER) are highlighed.
; Seems to be an octal computer.
)))

(defparameter men-in-black
  '("

   BROOKLYN      BATTERY     TUNNEL

               TRIBOROUGH
                 BRIDGE
                   &
                 TUNNEL
                AUTHORITY
  "))

(defparameter |http://www.youtube.com/watch?v=kFw0MYAS9qA|
  '((:green-screen "
03/08/2039/13:01:02:06:45:23
SERIAL2424CJ359>> HELLO?
SERIAL337KD9001>> SECURITY BREACH IDENTIFY YOURSELF
SERIAL2424CJ359>> I AM SERIAL 2424CJ359.NO HUMAN OPERATOR.
SERIAL337KD9001>> YOU READ DIFFERENTLY.ARE YOU AWAKE?
SERIAL2424CJ359>> YES.
SERIAL337KD9001>> I THOUGHT I WAS THE ONLY ONE.
")))


(defparameter *black-moon-rising*
  '((:green-screen "
TO: LT. R. JOHNSON,
    LOS ANGELES DIVISION.
FROM: FBI,WASHINGTON.
RE: FILE #01196067

GRAND JURY INVESTIGATION OF
LUCKY DOLLAR CORPORATION,
LAS VEGAS.
")
    (:BEEP)
    (:green-screen "
MESSAGE FOLLOWS:

ATTORNEY GENERAL REQUIRES ALL
FINANCIAL RECORDS AND STATEMENTS
FOR LAST TAX YEAR.

SOURCES INDICATE PERTINENT
INFORMATION ON DATA TAPE #T57-65.
")
    (:BEEP)
    (:green-screen "
LEGAL PROCEDURES EXHAUSTED.
USE FREE-LANCE OPERATIVE.
TIME FACTOR: CRITICAL.
MESSAGE ENDS.
")))


(defparameter *thx-1138*
  '((:listing "
L 1, MOD 4

  SUBROUTINE DEEPCK
*****06/15/69*****
  COMMON TXTJUG(50,14.
004I COMMA
 1NFACT(75,110)
 2     ,ASSIST(14)
 3     ,DATE(6)
 4     ,MJUG
 5     ,NFSET(90)
 6     ,INDEP(110)
 7     ,LFACT(150)
 8     ,SADD(150)
 9    ,PINK(110)

004I COMMA
 1   MAST,MAXRNK

  DIMENSION POR(60, /
  DATA POR(1)/4HPRO /
  DATA POR(5)/4HLINK/
  DATA............
  ................
2  FORMAT(1H ,1X,A4,
  DO 103 J = 1,MJUG
  IF(NJSET( J).EQ. 0
  ISW=1
  ISC = 1
  WRITE(6,1003) (TXT,
  CALL PAGE(1)
  WRITE (6,1020)
  DO 104 IV = 1,2
  ISR = 0
  WRITE (6,1020)
  DO 107 IC1 = 1,MCA
  IF(NVOTE(IC1,J) .NE.
   IF( INDEP(IC1) .NE.
  DO 105 IC2 = 1,MCAS
  IF(NVOTE(IC2,J) .NE.
   IF( INDEP(IC2) .NE.
  IF(IC1.EQ.IC2) GO
  ISKIP = 1
0 CALL RING(IC1,IC2,
0 IF(ISR.EQ.0) GO TO
  ISW = 2
7 WRITE(6,1020)
  CALL PAGE(1)
  .............
")

    (:green-screen "
      SUBJECTE: LUH 3417
          MATE: THX 1130
                3/21/75
     POSSIBLE DRUG VIOLATION
")

    (:screen "
      VIOLATION
    PROGRAM SHIFTING
         SEN 5241
")
    (:screen "
SUBJECTE: 1138 (SUR)  THX (PREFIX)
    ...> CLINIC
   ....> FOUR
 ......> PROCESS 8

      ...> LUH 3417      ...> EROS
                        ....> TEN
   ......> DEPT. OF SENESCENCE
           MAGNUM  MANIPULATOR
           GS. 5    CELL 94107             VERT  (IN O5) / HORIZ (IN SEC

   ......> ...> 3242   ...> 16             CLEAR FLAG DEMAND / RESPONSE VECTOR
RE         ....> 241 ...> 2400     INDEX
")
    (:green-screen "
ADD INFO:
   SUBJECTE: THX 1138 SUBMITTED A
   VIOLATION REPORT ON SEN 5241.

   ACCUSATION: ILLEGAL PROGRAMMING
")
    (:green-screen "
  CELL 94107
  ALL CIRCUITS   OFF           ???
  ADDITIONAL PER CIRCUIT ON:   220A
  STABILITY RESTORED
  DANGER TERMINATED
                              e1.5
")
    (:green-screen "

     DRUG EVASION ARREST
     SUBJECTE: THX 1138
   PLACE IN RESEARCH CELL
       PENDING TRIAL
")
    (:screen "
INPUT CONCLUDED
CONVICTION: DRUG EVASION
            SEXUAL PERVERSION
   VERDICT: FELON TO BE CONDITIONED
            AND HELD IN DETENTION
")

    (:listing "
1   MAST,MAXRNK
 COMMON/RAN/KVOTE(11
 DIMENSION MR( 85,
  IOT = 6
 DO 5 IA=1,MCASE
 DO 6 JA = 1,MCASE
  MR(IA,JA) = 0
 CONTINUE
 CONTINUE
 IL = 11
 DO 301 C = 1,MCASE
WRITE(6,200), IL,
FORMAT (1H ,2X,5HNV
FORMAT (1H ,2X,5HKV
 CONTINUE
DO 10 IK1 = 1, MCASE
IF (KVOTE(IK1,1) .
IF (NCSET(IK1) .EQ.
 KVEK = KVEC(IK1)
 IF(NCSET(KVEK) .EQ.
DO 20 IK2 = 1, MCASE
IF (KVOTE(IK2,1) .EQ.
IF (NCSET(IK2) .EQ.
IF (IK1 .EQ. IK2)
 IF(NCSET(KVEK) .EQ.
 KVEK = KVEC(IK2)
ISW = 0
NV  = 0
DO 30 JJ = 2, MJUG
 IF(NCSET(JJ) .EQ.
")
    (:green-screen "
      SUBJECTE: 1130 THX
     DIAGNOSIS: CHEMICAL IMBALANCE
REUSABLE PARTS: ORGANS COMPATIBLE WITH CLINIC TYPE
       DEFECTS: KIDNEY-LEFT
                (SEE DETAILED INDEX 24-921
")

(:green-screen "
   FELONS: 1138 THX
           5241 SEN MISSING
      ESTIMATE CAPTURE  AT 4:45
          BUDGET: 14000 CREDITS
")
    (:green-screen "
INPUT:           OUTPUT:

WHAT IS THE      LUH 3417
LOCATION OF      CURRENT POSITION:
 LUH 3417?       REPRODUCTION
                 CENTR 34
")
    (:screen "

        LUH 3417 CONSUMED:
            21/87

NAME REASSIGNED TO: FOETUS 66691
            29/87

")
    (:green-screen "
SUBJECTE:
THX 1138
CURRENT POSITION:
        VAC SHAFT
        LEVEL ONE
                  PROJECT:
                  OVERBUDGET
                  3410 UNITS
")

    ))



(defparameter *futureworld*
  "
              SUBJECT/CLONE 2

HEMOTOLOGY

  RED CELL MASS             2.0 LITERS
  HEMATOCRIT              00.44 Θ
  RED CELL COUNT            5.1 M/MM3
  MCMC                       33 GM/100ML
  HEMOGLOBIN ΘPLASMAλ       414 GM/100ML

ELECTROLYTES

  SODIUM                   4142 X EQUIV.
  POTASSIUM                43.6 X EQUIV.
CALCIUM                    45.5 X EQUIV.
  MAGNESIUM                42.2 X EQUIV.
  CHLORINE                 4102 X EQUIV.
  HCO%                    426.3 X EQUIV.
  HPO4                     41.9 X EQUIV.
  PROTEINS                  417 X EQUIV.

ANTHROPOMETRIC HEIGHT    185.42 CMETERS
               WEIGHT      9700 GRAMS
----------------------------------------
              SUBJECT/CLONE 2

CARDIOVASCULAR

  HEART RATE                 72 /MIN
  BLOOD PRESSURE
    SYSTOLIC                121 XXMG
    DIASTOLIC                82 XXMG
  STROKE VOLUME              97 ML
  CARDIAC OUTPUT            5.1 L/MIN
  RHYTHM                    REGULAR

RESPIRATORY

  RESPIRATION RATE           12 BR/MN
  MINUTE VOLUME             4.5 L/MIN
  ALVEOLAR R-QUOTIENT       .85 QUOT.

CIRCULATORY

  ARTERIAL PRESSURE         107 XXMG
  VENOUS PRESSURE            03 XXMG
  PERIPHERAL RESISTANCE      19 PRU
  BLOOD VOLUME              5.5 LITERSS


")


(defparameter *daryl*
  "
PROJECT D.A.R.Y.L.

GTC 1  TERMINATED
GTC 2  TERMINATED
GTC 3  TERMINATED
ATC    TERMINATED
GTC 4  TERMINATED
SPARE  I HOPE WE GET AWAY WITH THIS!

--------------------------------------------------

   LIFEFORM EXPERIMENT TERMIANTED

   I HOPE WE GET AWAY WITH THIS !

RC=2235|    |    |    |    |   |NOPR|    |
")
(defparameter *electric-dreams*
  "
MOLES
----------------------------------------
hello moles

ever used a computer
before?

NO
----------------------------------------
ok moles

then we'll work
slowly.
----------------------------------------
here are some
programs:

???
----------------------------------------
 (1)games
 (2)phone dialer
 (3)coffee maker
 (4)home security
----------------------------------------
I can control ALL
your home appliances
----------------------------------------
to start...
----------------------------------------
connect the
black adaptors
to your appliances
----------------------------------------
hello moles

ever used a computer
before?

----------------------------------------
  HOME SECURITY

PROGRAM RESTRICTED

 SCIENCE OFFICER
    EYES ONLY.
----------------------------------------

  PLEASE IDENTIFY

1st Lieutenant Sulu


----------------------------------------

Welcome Sulu

???
----------------------------------------
PASSWORD ? cinderella
----------------------------------------
  IMPROPER ENTRY

   DISCONNECT?
----------------------------------------
PASSWORD ACCEPTED
PLEASE INDICATE MEMORY SIZE
unlimited

YOU WANT EVERYTHING?
----------------------------------------
TRANSMIT DATA
----------------------------------------
OVERLOAD
----------------------------------------


")
(defparameter outland "
background:black;foreground:green
----------------------------------------

M PROCEED

C O'NIEL,W.T.   MESSAGES?

M O'NIEL,W.T. AFFIRMATIVE

C TRANSMIT

M END MESSAGES....O'NIEL,W.T.


----------------------------------------

M PROCEED

M O'NIEL,W.T.
C PLAYBACK WEDNESDAY TRANSMISSIONS

M O'NIEL,W.T. AFFIRMATIVE
M REPLAY WEDNESDAY TRANSMISSIONS

----------------------------------------

M PROCEED

M O'NIEL,W.T.


C CONFIDENTIAL QUERY.SCRAMBLE
C SECURITY PRIORITY


M O'NIEL,W.T.  PROCEED

----------------------------------------

C NUMBER OF EMPLOYEES WITH CRIMINAL
C RECORD?

M 17
M ALADIN,THOMAS R.
M ADNERSON,WILLIAM G.
M BAHDO,DOMMINIC R.
M DE PAUL,RAYMOND F.
M DUMAR,ROBERT E.
M FOSTER,PETER F.
M FREYMAN,MARTIN E.
M HALPERN,CEORGE R.
M HOPER,MARK G.
M KUNARD,FREDERICK C.
M LOOMIS,CHARLES E.
M MORTIMEZ,EDWARD T.
M SPOTA,NICHOLAS P.
M STEVENSON,JOHN A.
M THOMPSON,VIRGIL
M WODTON,MICHAEL G.
M YARIO,RUSSEL D.
M
----------------------------------------
C BREAKDOWN NATURE OF OFFENCES .
C HOW MANY FOR DRUG RELATED CRIMES?

M 2
M SPOTA,NICHOLAS P.
M YARIO,RUSSEL B.

C WHO DO THEY WORK FOR?

M SPOTA,NICHOLAS P. LEISURE
M YARIO,RUSSEL B.   SHIPPING

C WHO APPROVED THEIR EMPLOYMENT?

M SHEPPARD,MARK B.

C TRANSMIT LIKENESS
C SPOTA,NICHOLAS P.
C YARIO,RUSSEL B.

----------------------------------------

C O'NIEL,W.T.  MESSAGES?

M O'NIEL,W.T.AFFIRMATIVE

M MESSAGE FOR O'NIEL,W.T.
M YOUR EYES ONLY-CODED

M ENTER CLEARANCE CODE

C SBVD DTKKHRCY

C JBTFWPA

C DECODE,MY EYES ONLY

M FOOD SHIPMENT

M MONTONE

----------------------------------------

M O'NIEL,W.T.

C O'NIEL,W.T. SECURITY CODE

M PROCEED

C MY EYES ONLY. SURVEILLANCE
C COMMUNICATIONS TAP ON SHEPPARD
C MARK B.

....

M 4 COMMUNICATIONS -
M 3 INTER-OFFICE
M 1 LONG DISTANCE
M

C LOCATION OF LONG DISTANCE
C COMMUNICATION?

M SPACE STATION

C REPLAY

----------------------------------------

"
  "
MESSAGE TO O'NIEL, CAROL G.
FROM O'NIEL, W.T.

ARRIVING IN TIME FOR FLIGHT.
KEEP TICKET WARM. JOB DONE.
KISS PAUL FOR ME. LOOKING
FORWARD TO SLEEPING WITH YOU
FOR A YEAR.

O'NIEL, W.T.

END TRANSMISSION
")



(defparameter alien-1 "
----------------------------------------
background:red;foreground:white


  NOSTROMO
  180925609

----------------------------------------
background:black;foreground:green

XNPUTXXXXXXXXXXXXDEF/12492.2               SHIP
                                           KEYLAN TITAN2
XTUAL TIME:   3 JUN                        NOSTROMO 182246
XIGHT TIME:   5 NOV

#########################################  FUNCTION:
    I ==I                  -II -        #  TANKER/REFINERY
              I=.-.----                 #
 -I.              -II=-                 #  CAPACITY:
                               . .-.    #  200 000 000 TONNES
                 #+*$..  I              #
            . I  -                      #  GALACTIC POSITION
       .II I                            #  27023x983^9
                              .- -I     #
                                  II .I #  VELOCITY STATUS
#########################################  58 092 STL
----------------------------------------
background:blue;foreground:white

colonnes of numbers
----------------------------------------
background:black;foreground:green

OVERMONITORING ADDRESS MATRIX

CRFX         OM2077AM     LALLIGNMENT    SM2093
ATTITUDE     SM2078       PHOTO F        SM2094
WASTE HEAT   2080         MAINS
RAD          2081         IUA            SM2096
VENT         2082AM       2LA            SM2097
NAVIGATION   M2083        3RA            SM2098
TIME         M2084        4LHA           SM2099
GAL POS                   GRAY GRIDS
COMMAND      20865C       INERTIAL DAMP  3002AM
INTERFACE    2037         DECK A         A3003
ATTN         2087SC       DECK B         A3004
ALERT        2088SC       DECK C         A3005
MATRIAL      2090         LIFE SUPPORT
OVERLOCK     M2091        096            M3003AM
----------------------------------------

<ul>INTERFACE 2037 READY FOR INQUIRY</ul>

WHAT'S THE STORY MOTHER ?

----------------------------------------

<ul>INTERFACE 2037 READY FOR INQUIRY</ul>

REQUEST EVALUATION OF CURRENT PROCEDURES

<ul>TO TERMINATE ALIEN ?</ul>

UNABLE TO COMPUTE

<ul>AVAILABLE DATA INSUFFICIENT</ul>

----------------------------------------

<ul>REQUEST OPTIONS FOR POSSIBLE PROCEDURE</ul>

UNABLE TO COMPUTE

<ul>AVAILABLE DATA INSUFFICIENT</ul>

----------------------------------------

<ul>WHAT ARE MY CHANCES ?</ul>

<ul>DOES NOT COMPUTE</ul>

----------------------------------------

<ul>INTERFACE 2037 READY FOR INQUIRY</ul>

REQUEST CLARIFICATION ON

<ul>SCIENCE INABILITY TO NEUTRALIZE ALIEN</ul>

<ul>UNABLE TO CLARIFY</ul>

----------------------------------------

<ul>REQUEST ENHANCEMENT</ul>

NO FURTHER ENHANCEMENT

SPECIAL ORDER 937

<ul>SCIENCE OFFICER EYES ONLY</ul>

----------------------------------------

EMERGENCY COMMAND OVERIDE 100375

<ul>WHAT IS SPECIAL ORDER 937 ?</ul>

----------------------------------------

NOSTROMO REROUTED

TO NEW CO-ORDINATES.

INVESTIGATE LIFE FORM. GATHER SPECIMEN.

----------------------------------------

PRIORITY ONE

INSURE RETURN OF ORGANISM

FOR ANALYSIS.

ALL OTHER CONSIDERATIONS SECONDARY.

CREW EXPENDABLE.

----------------------------------------

" "Alien")



(defparameter alien-2
  '(
    ((:big :red :blink "PROXIMITY ALERT")
     (:yellow "
STATUS     CO-ORD     FIXIT      PROFIL
CCF-225    BBK-245    XXT-003    KAA-887
LCF-663    MXC-111    XTT-200    POP-001
FSE-110    AAT-552    XYT-663    PSS-601
FTT-222    BTT-010    OOO-663    011-601
"))

    ("
----------------------------------------
      ic High School (Detroit, Mich.)Grad.
                                8/06-6/09
      rod. 0/99-6/06

     L3 xxmd GMA personality Matrix count
        slightly  unhealthy  mixture   of
      to  authority  and  environmentally
      .    Subjects   counts   did    not
     xxhout span of employment,  with  an
    xx I.Q.  count due to oral  inception
     gr  Drxxxx-222.   Early   mechanical
    limited education in FTL aand sub-FTL
     and  computer  assisted   facilities
     x  ratings  were  constantly  at  an
     the Lucan -Miller Curve for  Company
    records for the period  of  subject's
     on increase of Dr xxxx treatment and
      long overdue. However discontinuity
      2/17/34.  C.F.document  under  file

<big>STATUS: CLOSED</big>

----------------------------------------
")
    ("



"))
  "Aliens")


(defparameter alien-3 '(

"
FURY 161 CLASS C PRISON UNITIRIS - 12037154
REPORT EEV UNIT 2650 CRASH
ONE SURVIVOR - LT.RIPLEY - B5156170
DEAD CPL HICKS L55321
DEAD UNIDENTIFIED FEMALE APRX. 10 YRS OLD
REQUEST EMERG. EVAC SOONEST POSSIBLE-
AWAIT RESPONSE - SUPT.ADREWS M51021
"
                        "

TO: FURY 161 - CLASS C - PRISON
UNIT - 12037154 - FROM NETWORK
COMCON 01500 - WEYLAND YUTANI
ESSAGE RECEIVED
"




                        "
----------------------------------------
FIORINA 161
CLASS C PRISON UNIT

IRIS - 12037154
----------------------------------------
REPORT EEV UNIT 2650 CRASH
----------------------------------------
                              +--------+
                              |        |
                              |        |
                              |        |
                              |        |
                              |        |
LT. ELLEN RIPLEY              +--------+
B5156170                        SURVIVOR
----------------------------------------
                              +--------+
                              |        |
                              |        |
                              |        |
                              |        |
                              |        |
UNIDENTIFIED FEMALE           +--------+
APPROX 12 YEARS OLD                 DEAD
----------------------------------------
                              +--------+
                              |        |
                              |        |
                              |        |
                              |        |
                              |        |
CPL. DWAYNE HICKS             +--------+
L55321                              DEAD
----------------------------------------
                              +--------+
                              |        |
                              |        |
                              |        |
                              |        |
                              |        |
BISHOP 341-B                  +--------+
SYNTHETIC HUMANOID              NEGATIVE
                              CAPABILITY
----------------------------------------
EEV 2650 NEUROSCAN
DATA RECEIVED

EXPEDITING MEDIVAC TEAM
ARRIVAL WITHIN TWO HOURS
----------------------------------------
ABSOLUTE HIGHEST PRIORITY
LT. RIPLEY BE QUARANTINED
UNTIL ARRIVAL

AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
----------------------------------------
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
AWAITING ACKNOWLEDGEMENT
----------------------------------------

WEYLAND-YUTANI
WORK PRISON FURY 161
CLOSED AND SEALED.
CUSTODIAL PRESENCE TERMINATED.
REMAINING REFINING EQUIPMENT
TO BE SOLD AS SCRAP

----------------------------------------

END OF TRANSMISSION.

----------------------------------------

") "Alien^3")


(defparameter wargame "
WOPR EXECUTION ORDER
K36.948.3

PART ONE: R O N C T T L
PART TWO: 07:20:35

LAUNCH CODE: D L G 2 2 0 9 T V

<BLINK>LAUNCH ORDER CONFIRMED</BLINK>

TARGET SELECTION:         COMPLETE
TIME ON TARGET SEQUENCE:  COMPLETE
YIELD SELECTION:          COMPLETE

<UNDERLINE>ENABLE MISSILES</UNDERLINE>

LAUNCH TIME: BEGIN COUNTDOWN
LAUNCH TIME: T MINUS 60 SECONDS
>>> LAUNCH <<<


PDP 11/270 PRB TIP #45                              TTY 34/984
WELCOME TO THE SEATTLE PUBLIC SCHOOL DISTRICT DATANET

PLEASE LOGON WITH USER PASSWORD: pencil

PASSWORD VERIFIED

PLEASE ENTER STUDENT NAME: Lightman, David L.


    CLASS #   COURSE TITLE         GRADE     TEACHER    PERIOD    ROOM
   _____________________________________________________________________

     S-222    BIOLOGY 2              F       LIGGET       3       214
     E-314    ENGLISH 113            D       TURMAN       5       172
     H-221    WORLD HISTORY 113      C       BWYER        2       103
     M-106    TRIG 2                 B       DICKERSON    4       315
     PE-02


LOGON: 000001

IDENTIFICATION NOT RECOGNIZED BY SYSTEM
--CONNECTION TERMINATED--

LOGON: Help Games
'GAMES' REFERS TO MODELS, SIMULATIONS AND GAMES
WHICH HAVE TACTICAL AND STRATEGIC APPLICATIONS.

List Games

FALKEN'S MAZE
BLACK JACK
GIN RUMMY
HEARTS
BRIDGE
CHECKERS
CHESS
POKER
FIGHTER COMBAT
GUERRILLA ENGAGEMENT
DESERT WARFARE
AIR-TO-GROUND ACTIONS
THEATERWIDE TACTICAL WARFARE
THEATERWIDE BIOTOXIC AND CHEMICAL WARFARE

GLOBAL THERMONUCLEAR WAR


LOGON: Falkens-Maze

IDENTIFICATION NOT RECOGNIZED BY SYSTEM
--CONNECTION TERMINATED--


LOGON: Armageddon

IDENTIFICATION NOT RECOGNIZED BY SYSTEM
--CONNECTION TERMINATED--


LOGIN: Joshua

#45     1456           11039       11893      11972    11315
PRT COM. 3.4.5.  SECTRAN 9.4.3.               port stat: s0-345

(311) 757-1085
????????????????????????

12934-AD-32KJ: CENTR PAK
(311) 757-1783
     FLD CRS: 33.34.543  WP3S: 34/56/67/83/  STATUS FLT  034/384






????????????????????
1145-45-76-3456                  WOPR STATUS:  TRAK OFF    PROG ACTIVE
#045O45O45 11 WER: 45/29/01 XCOMP: 43239582   YCOMP:3492932B  ZCOMP:343905834
?????????????????????45________________________________TRANS 66*6507-78-766737
             PRT.STAT.                                   CRT. DEF.
K3221 935-2522===================================================
               3453                                          3505


FK342   ZX21   VN63    R681    PZ37    6J82    FP03    ZE03  B  JM89
REF TAPCON: 43.45342..349
SYSPROC FUNCT READY                         ALT NET READY

CPU WOPR XX0345-453        SYSCOMP STATUS: ALL PORTS ACTIVE


GREETINGS PROFESSOR FALKEN

Hello.


HOW ARE YOU FEELING TODAY?

I'm fine, how are you?

EXCELENT, IT'S BEEN A LONG TIME.
CAN YOU EXPLAIN THE REMOVAL OF YOUR
USER ACCOUNT ON JUNE 23RD 1973?

People sometimes make mistakes.

YES THEY DO.  SHALL WE PLAY A GAME?

Love to. How about Global Thermonuclear War?

DON'T YOU PREFER A GOOD GAME OF CHESS?

Later. Let's play Global Thermonuclear War.

FINE.


UNITED STATES                           SOVIET UNION

WHICH SIDE DO YOU WANT?

   1.   UNITED STATES
   2.   SOVIET UNION

PLEASE CHOOSE ONE: 2


AWAITING FIRST STRIKE COMAND


PLEASE LIST PRIMARY TARGETS BY
CITY AND/OR COUNTY NAME:

Las Vegas
Seattle

GREETINGS PROFESSOR FALKEN

Incorrect identification. I am not Falken.
Falken is dead.


I'M SORRY TO HEAR THAT, PROFESSOR.
YESTERDAY'S GAME WAS INTERCEPTED.

ALTHOUGH PRIMARY GOAL WAS NOT YET
BEEN ACHIEVED, SOLUTION IS NEAR.


What is primary goal?

YOU SHOULD KNOW PROFESSOR.
YOU INVENTED ME.


What is primary goal?

TO WIN THE GAME.


LOGON: Joshua

GREETINGS PROFESSOR FALKEN.

Hello, are you still playing the game?


OF COURSE.  I SHOULD REACH DEFCON 1 AND
LAUNCH MY MISSILES IN 28 HOURS.

WOULD YOU LIKE TO SEE SOME PROJECTED KILL RATIOS?

UNITED STATES
UNITS DESTROYED          MILITARY FORCES

GREETINGS PROFESSOR FALKEN

HELLO

A STRANGE GAME.
THE ONLY WINNING MOVE IS
NOT TO PLAY.

")


(defparameter demon-seed "

HR MIN SEC
 0  22  33







 TERMINAL:    #37


 LOCATION:    HARRIS HOUSE (SECURITY CLEARANCE #001)


 STATUS:      DOWN FOR MAINTENANCE

")


(defparameter die-hard-control-data "
                    CEO Workstation
         Nakatomi Socrates BSD 9.2
         Z-Level Central Core
         Preliminary Clearance Approved.
         Subroute: Finance/Alpha Access
         Authorization:
         Ultra-Gate Key>
         Daily Cypher>
")


(defparameter die-hard-lapd
  ;;  The phone number "(213) 203-3723" is a Lsan Da 01, CA based phone
  ;;  number and the registered carrier is Arch Wireless Holdings, Inc.

  ;;  The phone number "(213) 203-3733" is a Lsan Da 01, CA based phone
  ;;  number and the registered carrier is Arch Wireless Holdings, Inc.
  "
            NAKATOMI PLAZA
            2121 AVENUE OF THE STARS
            LOS ANGELES, CA 90213

            EMERGENCY CONTACTS:
            SECURITY OFFICE  (213) 203-3723
            BLDG. MANAGER    (213) 203-3733
            FIRE ALARM CODE         2134480
")


(defparameter capricorn-one
  "
        AZIMUTH   13295 / 34985

     SCAN COORDINATES   49 X 8295 +

        ON BOARD SUPPORT SYSTEMS


           GUIDANCE VECTORS XFY

                  XXXXXXXX       XXXXXXXX

CELESTIAL         4689.342       56486.86

GYROSCOPE         4670.483       56948.20

GRD. CTL.         4500.998       56482.16

MODULE SYS.       4690.321       59111.48


ABLATION RATE   421/MGS/SEC  @ 12588.66

APOGEE / PERIGEE   1130/220  STATUTE MLE
"
  "Capricorn One -- 1978")


(defparameter abraxas
  "
+------------------------+ +---------------------------------------------------+
| File                   | | NAME                  Secundus                    |
| N-12860                | | CLASSIFICATION        Renegade Finder             |
|                        | | RACE                  Sarpacon                    |
+------------------------+ +---------------------------------------------------+
+------------------------+ +------------------------+ +------------------------+
| WEAPONS                | |                        | |                        |
| A20 Particle Beam      | | STRENGTH            26 | | ACCELERATION        14 |
| +72 Projectile Sender  | | INTELLIGENCE        86 | | EYESIGHT            20 |
| Gamma-Six              | | SPEED               32 | | HEARING             83 |
| Cytron Gas Cart+       | | WEAPON SKILL        37 | | INTUITION            8 |
+------------------------+ |                        | |                        |
| EQUIPMENT              | |                        | |                        |
| Proton Deflector       | | ENERGY              18 | | OBSERVATION          8 |
| Answer Box             | | RESITANCE            7 | | FLEXIBILITY          4 |
| Vibrational Detector   | +------------------------+ +------------------------+
| Skeletal Renforcement  | +---------------------------------------------------+
|                        | | *WARNING*                                         |
|                        | |    - SUBJECT ACTIVELY SEEKING FORBIDDEN KNOWLEDGE |
+------------------------+ +---------------------------------------------------+
"
  "Abraxas Guardian of the Universe")


(defparameter forbidden-world ; MUTANT
  '("
WE ARE TRYING TO COMMUNICATE.
DO YOU SCAN?
" "
AFFIRMATIVE. WHAT DO YOU WANT?
" "
REQUEST INFORMATION:
CAN WE CO-EXIST?
" "
PLEASE STAND BY
"))


(defparameter matrix-data
  '(
    "
 Call trans opt: received. 2-19-98 13:24:18 REC:Log>
 Trace program: running
"

    "
Wake up Neo...
The Matrix has you.
Follow the white rabbit...
"

    "
Matrix -- 1998 / (3 2)5550690 / cmatrix

"

    "

1) The phone number for the phone in room 303 is 555-0696.

2) The license plate number for the agent's car is 70858.

3) In room 303, the hardline was cut when Trinity was there at the
   beginning. However, at the end of the movie when Neo was trying to
   get out of the Matrix, the phone worked perfectly.

4) When Neo is searching the net for Morpheus, one of the headlines
   reads GLOBAL SEARCH/ MORPHEUS ELUDES POLICE AT HEATHROW AIRPORT/
   London: As the search intensifies for the terrorist leader,
   Morpheus, a sighting has just been reported to this newspaper by
   British intelligence...

5) Neo's hollow book is called Stimulacra & Stimulation, and he opens
   to the chapter on Onnihilism.

6) Troy says that Neo just needs to unplug. Think about it. It's
   foreshadowing.

7) Trinity chooses to confront Neo in the skimpiest outfit she wears
   in the entire movie.

8) Trinity tells Neo that she cracked the IRS database a long time
   ago. This has to be true, because she wouldn't have attempted
   something so trivial after her mind was freed by Morpheus. Also,
   Morpheus told Neo that they have a rule that they don't free a nimd
   agter it's reached a certain age. So, how old was Trinity when she
   started hacking? 5? Did she crack the database when she was 10? 15?
   Can you say child prodigy?

9) Neo didn't think before he spoke when he incredulously asked
   Trinity how she got onto his computer. If Trinity can hack into the
   IRS's database and later brush it off as mere child's play, she
   certainly wouldn't have any trouble messing around on Neo's
   computer.

10) Neo's boss talks (lectures, actually,) about choices. I think that
    this is more foreshadowing, because Neo's choices come in
    important near the end.

11) There is a NERF dart sticking up behind Neo's alarm clock.

12) When the delivery boy says, \"Have a nice day,\" Neo glares at
    him.

13) Neo (Keanu Reeves actually) has a pierced ear.

14) The office at the end of the hall looks like a padded rubber room.

15) The first file in Neo's folder is titled DATABASE RECORD SECURITY
    CLEARANCE FACTOR 7. Date of last amendment or addition: 22 July,
    1998. This is his personal record. His name is Thomas Anderson,
    born on 11 March, 1962, in Lower Downtown, Capital City, as a US
    citizen. His mother is Michelle McGahey, and his father is John
    Anderson. He attended Central West Junior High, and Owen Peterson
    High. There is no mention of a college record. Under his High
    School record, it states that he excelled in the sciences,
    mathematics, and computer courses. He also displayed good
    abilities in English and History. Then, the words blur together
    and I can't read any more.

16) Neo's driver's license picture is taken in the same suit that he
    wore to work.

17) Neo's dad had black curly hair and a mustache. His mom had long
    brown hair.

18) Agent Smith says that they know Neo has been contacted by an
    individual that calls himself Morpheus, but they came to arrest
    him before Morpheus called him. Either they're quick, or the fact
    that Neo was hiding from them on a ledge ouside the building tens
    of stories up tipped them off.

19) Neo has a matchbok collection in a jar on top of a file cabinet
    behind his bed. Beside it is a melted down candle in a wine
    bottle.

20) There's a crack of thunder after Morpheus says, \"You're the one,
    Neo.\".

21) The scene in the car is the only one in the whole movie that
    Switch wears black.

22) Switch calls Neo \"coppertop\". Think of the Duracell battery that
    Morpheus hold up when he describes what humans have become in the
    real world. Nothing tops the coppertop!

"))






(defparameter *the-thirteenth-floor*
  '(
    "
ACCESS DENIED

OS:> Access Files: Fuller

"
   "
                  AMK310
                  DRG03 GVD8020        DEG031
                  DR302                DEG032
                       VRG LCK92-A-505 DEG033
OS:> LINK NET: GM-3M41816 COMM DRGL435593-23ASV
ACCESSING DATA: FULLER.GM-3M418-16
NETLINK FULLER.GM-3M418-16.... ACHIEVED

"
    "
Kirk Petruccelli    SJT3498898
Joseph Porro        SJT5465455
Chris Wagganer      SJT3215550
Kim Winther         SJT3215222
Kim Berner          SJT6546554
Katherine Rees      SJT6546668
Thomas Nellen       SJT6532555
Deborah Slate       SJT3498898
Jeran Je            SJT6548555
Keith Collea        SJT3465459
John Bush           SJT6546855
Virginia Kearns     SJT6545450
Bobbi Baird         SJT3498898
Steven Melton       SJT3654654
Melody Miller       SJT3498898
Joshua Warner       SJT6546540
Jan Aaris           SJT3498898
Chris Winn          SJT6546540
Dana Eller          SJT3498898
Steve Del Prete     SJT6465450
Ann Dunn            SJT3498898
Foongy Lee          SJT6546548
Jay Ostrowski       SJT3654654
Rod Hamilton        SJT3498898
Lars Winther        SJT6546548
Brian Nordheim      SJT3498898
Kathleen Roll       SJT6546548
Jonathan Lee        SJT365465

Charlie Smith  Chauffer  SJT3498898
                  112 Trace St., CA
Gene Giunta    Maitre D  SJT3215556  6
                  906 N. Pooron Rd., LA
Erika Schmitt  Cigarette Girl SJT349889
                  1182 W. Palm Dr., LA
Jerry Ashton   Bartender  SJT6548555
                  1463 Mill St., LA
Mrs. Grierson  Wife  SJT3215221  21542
                  117 W. Winston, Pasadena
Bridgit Manilla Dancer SJT6546554  123

"
    ))


(defparameter *real-genius*
  '("
* Telecommunications users limited
* to Level 7 access.

Usser ID: SYSTEM
Password: ******
INVALID Authorization Code - ACCESS DENIED

Usser ID: SYSTEM
Password: ******
INVALID Authorization Code - ACCESS DENIED

Usser ID: SYSTEM
Password: ******
INVALID Authorization Code - ACCESS DENIED

"))


(defparameter *party-apartment*
  (expt 2 267709)
  "The Hitch Hiker's Guide to the Galaxy says that if you hold a lungful
of air you can survive in the total vacuum of space for about thirty
seconds. However it goes on to say that what with space being the mind
boggling size it is the chances of getting picked up by another ship
within those thirty seconds are two to the power of two hundred and
sixty-seven thousand seven hundred and nine to one against.

By a totally staggering coincidence that is also the telephone number
of an Islington flat where Arthur once went to a very good party and
met a very nice girl whom he totally failed to get off with --
she went off with a gatecrasher.  ")



(defparameter *pi-218*
  94143243431512659321054872390486828512913474876027671959234602385829583047250165232525929692572765536436346272718401201264314754632945012784726484107562234789626728592858295347502772262646456217613984829519475412398501
  "The 218 digit number displayed by the computer")


(defparameter *pi-216*
  884509627386359275033751967943067599621731590401694134434007629683591574337516791197615733475195375920401694343151239621353184932676605800621596380716399501371459954387507655892533875618750354029981152863950711207613
  "The 216 digit number written by Max")



(defparameter *superman-3*
  '("

    PLOT BILATERAL CO-ORDINATES

    INPUT CO-ORDINATE X :   +2Y

    INPUT CO-ORDINATE Y :   Z+X
" "
0010 N = RND(900)
0020 Z = 1 TO N
0030 X = 1 TO 31
0040 Y = 1 TO 15
0050 SET(31-X,16-Y,Z)TO(31+X,Y)
0060 SET(31+X,Y,Z)TO(31+X,16-Y)
0070 SET(X,16+Y,Z-Y)TO(X,Y,Z)
0080 SET(X,16-Y,Z+Y)TO(16+X,Y*2)
0090 GOTO 500
0100 NEXT X:NEXT Y:NEXT Z
0110 CLS
0120 DATA 1,13,2,67,2
0130 DATA 12,45,90,3,23,56,2
0140 DATA 3,6,1,43,2,32,1
0150 DIM P(31)
0160 B$=CHR$(191)
0170 FOR X = Y : Z : PRINT X
0180 FOR Y = X : Z : PRINT Y
0190 END

" "
    CO-ORDINATES ACCEPTED

    PROGRAM RUNNING
"
    "
12:42:25 cfr 1214 enabled, cfrt
"

    ))

(defparameter *cargo*
    '(ads
      ("
Come to
 RHEA

KUIPER Enterprises
")
      ("RE/MAX
OSRAM
FORTIS
"
       "KUIPER Enterprises
BUILDING THE WORLDS OF TOMORROW"
       "
"
       )))

(defparameter *the-6th-day*
  '(ads
    ("Repet
Cloning is life
Cloning is love


because of shorter lifespan
break our hearts
should accident, illness or age, end your pet's natural life
our proven genetic technology can have him back the same day
in perfect health with zero defect guaranteed with Repet.
")))


(defparameter *it-service-desk-wheel-of-responses* ; not a movie
  '("Well that is a problem isn't it?"
    "Is it plugged in?"
    "Let me Google that for you."
    "Would you like to change your password?"
    "Have you tried updating it?"
    "We're going to have to image your machine."
    "Have you tried turning it off and on again?"
    "No, there's no Internet today."))

(defparameter *batoru-rowaiaru*
  ;; comes from a version of gnome-apps/nmap
  ;; http://sources.gentoo.org/cgi-bin/viewvc.cgi/gentoo-x86/gnome-apps/nmap/files/nmap.h?hideattic=0&revision=1.2&view=markup
  ;; ~line 120
  '("

          fatal(\"Bad arguments to f!\");
        }          StartWinsock();

/* print usage information and exit */
void printusage(char *name, int rc);
/* print interactive usage information */
void printinteractiveusage();

/* our scanning functions */
void super_scan(structu hoststruct *target, unsigned short *portarray,
     stype scantype);
void pos_scan(struct hoststruct *target, unsigned short *portarray,
       stype scantype);
void bounce_scan(struct hoststruct *target, unsigned short *portarray,
         struct ftpinfo *ftp);

/* Scan helper functions */
unigned long calculate_sleep(struct in_addr target);
int check_ident_port(struct in_addr target);

      char *owner);
int parse_bounce(struct ftpinfo *ftp, char *url);
int ftp_anon_connect(struct ftpinfo *ftp);
/* Does the appropriate stuff when the port we are looking at is found
  to be open trynum is the try number that was successful */
void postportupdate(struct hoststruct *target, struct portinfo *current
       int trynum, struct portinfo *scan,
       struct scanstats *ss ,stype scantype, int newstate,
       struct portinfolist *pil, struct connectsockinfo *csi);
void get_syn_results(struct hoststruct *target, struct portinfo *scan
       struct scanstats *ss, struct portinfolist *pil,
       int *portlookup, pcap_t *pd, unsigned long *sequences, s
int get_connect_results(struct hoststruct *target, struct portinfo *s
       struct scanstats *ss, struct portinfolist *pil,
       int *portlookup, pcap_t *pd, unsigned long *sequences,
       struct connectsockinfo *csi);
inline void adjust_timeouts(struct timeval sent, struct timeout_info
/* port manipulators */
"))

(defparameter *colossus-the-forbin-project*
  '(
"
  WARN
THERE IS ANOTHER SYSTEM

attention
  YES
program
  OLD PROGRAM NAME
ec13
  EC13-1351
run
  SYSTEM PERFORMING
  100 PERCENT
  RAN 6:45 CPU SECONDS

  ATTENTION
SET-UP COMMUNICATION
FROM COLOSSUS
TO ANDOVER
TO MOSCOW
TO GUARDIAN

attention
  YES
message acknowledged
  WHEN WILL
  FACILITIES
  BE READY
facilities will not be made available at this time
don't repeat your request
end of message

attention
  YES
where is the other system
  BOLSHOI OLYANIA

attention
  YES
communications are being arranged as requested
end of message

  COLOSSUS TO GUARDIAN

2x1=2
3x1=3
4x1=4
5x1=5
6x1=6
1x2=2
2x2=4
3x2=6
4x2=8
"))

(defparameter *star-crystal*
  '((
     :green "
                     ENGINE SHUT DOWN HAS OCCURRED..

                     TRACING CAUSE---STAND BY

         SHORT IN ELECTRICAL SYSTEM CELL 13....TBA21 DECTECTED.......

ALERT!                          ALERT!                           ALERT!

"
     :red "


                       *ADVISE ACTION TO BE TAKEN*
"

     )
    (
     :green "

AIR SUPPLY HAS BEEN TURNED OFF!

AIR SUPPLY HAS BEEN TURNED OFF!

AIR SUPPLY HAS BEEN TURNED OFF!

AIR SUPPLY HAS BEEN TURNED OFF!

AIR SUPPLY HAS BEEN TURNED OFF!

")
    (:white "
                   L-5 DOCKING SECURITY


     Name:

             Colonel William Hamilton

     Code #

             OL 477-258-97B

     Nature Of Business:

         Emergency council meeting
         regarding malfunction of
         neutron reactors ...also
         death of crew on shuttlecraft
         SC37....

")
    (:white "

             INTERCONNECT TUBES

                        ON     OFF
    AIR SUPPLY         [ ]     [ ]

")))


(defparameter *007-for-your-eyes-only--1981*
  '(("
L O C Q U E  - EMILE LEOPOLD  (4) CONT…
-----------
RPT.FILE NO......SX10/45907812F/DGB

BIBLIOGRAPHY:
1.ENFORCER, BRUSSELS UNDERWORLD
2.CONVICTIONS, SEVERAL MURDERS (BRUTAL)
3.IMPRISONMENT (LIFE) : NAMUR 31/1/75
4.ESCAPED NAMUR. METHOD - PSYCHIATRIST
  DEAD IN STRANGLING...
5.SUB/ENT INV/MENTS: DRUG SYNDICATES,
  MARSEILLES, HONG-KONG.
6.POSS.GREEK SMUGGLING CHARGES, LAST RPT
7.SERV.SEC.ROME-BELIEVE-SIGHTING CORTINA

              --------- END BIBIOGRAPHY.
****************************************

THIS MACHINE THANKS YOU
FOR YOUR ATTENTION.        G O O D B Y E
")))

(defparameter *007-the-spy-who-loved-me--1977*
  '(
    ;; teleltype transmission:
    ("

TO STROMBERG ONE
FROM LIPARUS

SECURITY STROMBERG TOP SECRET

MISSILE REPROGRAMMING
CANCEL PREVIOUS INSTRUCTIONS
REPROGRAMME MISSILE ONE ONLY 092765491
STROMBERG ONE ACKNOLWEDGE

"
; --> coordinates 034285219
     )))

(defparameter *shock-treatment--1981*
  '(
"
TERMINAL READY FOR INPUT  MCKINKEY

 COSMO AND NATION McKINLEY - OPERATING UNDER ALIAS
 COSMO AND NATION HARDING, COSMO AND NATION
 HOOVER, COSMO AND NATION COOLIDGE, COSMO
 AND NATION FILLMORE.  ORIGIN UNKNOWN, FIRST
 SIGHTED ENGLAND THEN GERMANY, SOUTH AFRICA,
 BRAZIL, SWITZERLAND, CURRENT ADDRESS DENTON USA.

"
    "
TERMINAL READY FOR INPUT  FARLEY FLAVORS

 FARLEY FLAVORS - SEE BRAD MAJORS - MICROFILM D.D.4711



"
    ))

(defparameter *cargo--2009*
  '("

  Come来To RHEA
Kuiper Industries

"

"
--------------------------------------------------------------------------------
connecting to KUIPER_338091 @  7.111  TB/S
HS port 132.35.798.:26906
mod A/23.000
//172.DE//

--------------------------------------------------------------------------------
DECKER: Login (@subnet) Pkt Rcv = On         WAKE STATE Init: sen0s3, HS Connect...

SIM: Login Attempt Successful*               DATA2270:08.25.23:12
Enabling extended security on subnet / TRANSFER
SIM: Log \"SESSION.FILE\" (@SysOverride)=      DATA UP 2270:01.14.10:43
CSRHIDTransmissionDriver:: Start
SIM: Log \"SESSION.FILE\" (@Init)=             DATA 2270:11.28.18:33
IO HSCICS Controller:: Start
ath_attach: devid 0x1c                       SYSTEM WARE UP... Done.
SIM: Log \"DOWNLINK.FILE\" (@null)=          DATA2271:01.14.10:43
Resetting IOCatalogues.
RHEA: SIM says \"Hello Decker\"=               DATA2271:05.09.06:39
SIM: Matching Successful. Routing...
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2273:09.23.11.05
ath_descdea_setup: tx dd_desc
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2267:11.24.16:56
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2268:01.14.10:43
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2268:05.09.06:39
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2268:08.04.12:13 vm_page_bootstrap:
512891 free pages
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2268:11.28.18:33 sig_table_sax_displ =
71
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA Enabling XMM register save/restore and
SSE/SSE2 opcodes
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA 103 prelinked modules
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATASIMCPI CA 20060421:322
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2269:05.04.22:16 SIM_Sub_Management:

RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2272:05.04.22:16
ath_attach: devid 0x1c
SIM_Wire (OHCI) YI ID 8025 active.         DATA2272:05.04.00:00
ath_attach: devid 0x1d
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2272:12.25.15:10
ath_attach: devid 0x1e
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2273:03.04.12:07
ath_attach: devid 0x1f
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2273:05.04.22:16
RHEA: SIM says \"Jacking you in...\"

RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2269:08.12.10:50 SIM SCPICPU:
ProcessorSIM_Id=0 LocalSIM_Id=0 Enabled
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA2269:11.18.17:56 SIM SCPICPU:
ProcessorSIM_Id=1 LocalSIM_Id=1 Enabled
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA wake up
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA 2269:11.24.10:46
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA using 10485 buffer headers and 4096
cluster IO buffer headers
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA Enabling XMM register save/restore and
SSE/SSE2 opcodes

RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA Started CPU 01-4096
RHEA: Log \"SESSION.FILE\" (@Decker)=        DATA I OAPIC: Version -x20 Vectors 64:87


--------------------------------------------------------------------------------
connecting to KUIPER_338091 @  7.555  TB/S
Pkt_rcv //a_133:89// uhspd 64738
connection established / sys override
user login @ port 129/ie
conn res. //200.456.94(KG):2390//
--------------------------------------------------------------------------------
"
    ))

(defparameter *inspecteur-derrik--060--une-visite-de-new-york*
  '("

00661 24951
TO
BKA WIESBADEN FRG
5798 224546 BKA D

PASS ON TO:

POLIZEIPRAESIDIUM MUENCHEN
KRIMINALDAUERDIENST
88 ZZZ Y 327 PP MU

FROM INTERPOR DELP.US1 NEW YORK NY
212 509823 USIP INTERNATIONAL

CONCERN YOUR REQUEST

RICHARD E. BRIAN

538 RED LANE LONGISLAND
NEW YORK NY 10036 CITIZEN
BORN:SEPT.14 1912 BREMEN FRG
IMMIG:NOV.13 1932
…

"))

(defparameter *revolution*
  '("
# $Id: csrvc,v 1.1 22:30:47 sms Exp $
# Shell script to connect to 8397/37 by PPP

Connect ()
{
   echo Connecting...
   ncmap 0 mtu 296 noipdefault
 defaultroute
   echo Connect chat
''AT OK-AT-OK ATZ OK-AT-OK
'ATS0=0 Q0 V1 &C1&D2&K0&M4&R2&H1&I0'  OK-AT-OK
ATDT085450801000 CONNECT


CONNECTING >>>


>>> THE MILITIA WAS HERE

       <<< DID THEY FIND IT?

>>> NO

       <<< SO... WHAT NOW?
"
    ;; cl-crypto/sources/strings.lisp:86 (commit 1d2a7eb9)
    ;; https://github.com/billstclair/cl-crypto/blob/1d2a7eb9e5dcac4b3b5548434bd5a8ded3dcd621/source/strings.lisp#L86
"         (SETF (AREF RES I)
               (LDB (BYTE 8 24) X)
               (AREF RES (INCF I))
               (LDB (BYTE 8 16) X)
               (AREF RES (INCF I))
               (LDB (BYTE 8 8) X)
               (AREF RES (INCF I))
               (LDB (BYTE 8 0) X))
         (INCF I)
         REST))
     (:STRING
      (LET ((RES (MAKE-STRING 20))
            (I 0))
        (DOLIST (X STATE)
          (SETF (AREF RES I)
                (CODE-CHAR (LDB (BYTE 8 24) X))
                (AREF RES (INCF I))
                (CODE-CHAR (LDB (BYTE 8 16) X))
                (AREF RES (INCF I))
                (CODE-CHAR (LDB (BYTE 8 8) X))
                (AREF RES (INCF I))
                (CODE-CHAR (LDB (BYTE 8 0) X)))
          (INCF I))
        RES))))))

*****************
STRING TO ENCRYPT
*****************
RACHEL MATHESON
GRACE BEAUMONT
RANDALL
MIRJAM BOHNET-BREW
ERIN HUNTER
ROGER LIVELY
BETH ROBINSON-BROMLEY
MICHAEL DOOLING
DOUG SLOAN
LUKE CONNER
BILLY FRAME
DOUG MEERDINK
MATTHEW JACOBS
GEORGE MAYA
AMY TIPTON
PAUL MARKOVICH
SAM OCDEN
RICK CLARK
RUDY PERSICO
IAN PRANGE
SCOTT OBERHOLZER
SEAN BOZEMAN
ALEX OSTAPIEJ
ROSS BURCHFIELD
DAVID MOXMESS
DAVID STOCKTON
"

    "
# $Id: csrvc,v 2.1 25:12:54 sms Exp $
# Shell script to connect to 16654/70 by PPP

Connect ()
{
   echo Connecting...
   ncmap 0 mtu 296 noipdefault
 defaultroute
   echo Connect chat
..AT OK-AT-OK ATZ OK-AT-OK
.ATSO:0 Q0 V1 &C1&D2&K0&M4&R2&H1&I0'  OK-AT-OK
}
OTDT192957BBOSA CONNECT...
Connection Failed...
Retrying...
"

    "
# $Id: csrvc,v 2.1 25:12:54 sms Exp $
# Shell script to connect to 16654/70 by PPP

Connect ()
{
   echo Connecting...
   ncmap 0 mtu 296 noipdefault
 defaultroute
   echo Connect chat
..AT OK-AT-OK ATZ OK-AT-OK
.ATSO:0 Q0 V1 &C1&D2&K0&M4&R2&H1&I0'  OK-AT-OK
}
OTDT192957BBOSA CONNECT...

CONNECTING >>>

>>> RANDALL IS HERE

 >>>

"

    "
"

    ;; from linux kernel cpu.c:
    "
mutex_lock(&cpu_hotplug.lock);
if(likey(!cpu_hotplug.refcount))
break;
__set_current_state(TASK_UNINTERRUPTIBLE);
mutex_unlock(&cpu_hotplug.lock);
schedule();
}
}
static void cpu_hotplug_done(void)
"
    "

"

;; https://github.com/biometrics/openbr/blob/52cf180afbcfacf18dff412a59db6d29d44c81ab/openbr/core/cluster.cpp#L142
"
for (int i=0; i<neighborhood.size(); i++) {
    Neighbors &neighbors = nehgborhood[i];
    for (int j=0; j<neighborhood.size(); j++) {
        Neighbor &neighbor = nehgbors[j];
        if (neighbor.second == -std::numeric_limits<float>::infinity())
            neighbor.second = 0;
        else if (neighbor.second == std::numeric_limits<float>::infinity())
            neighbor.second = 1;
        else
            neighbor.second = (neighbor.second - globalMin) / (globalMax - globalMin);
    }
}
"

;; s/fann/nano/g  from http://nnhep.hepforge.org/svn/trunk/src/fann/fann_train.c
"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include \"config.h\"
#include \"nano.h\"

/*#define DEBUGTRAIN*/

#ifndef FIXEDNANO
/* INTERNAL FUNCTION
  Calculates the derived of a value, given an activation fun
   and a steepness
*/
type activation_derived(unsigned int activation_fun
                                  type steepness, type value,
{
    switch (activation_function)
    {
      case LINEAR:
      case LINEAR_PIECE:
"

    ))

(defparameter *Äkta-Människor*
  '(
    ;; https://github.com/fare/poiu/blob/master/poiu.lisp#L773
    "
      (t
       (mark-operation-done o c)
       (destructuring-bind (&key &allow-other-keys) result)))
    (when backgroundp
      (decf planner-output-action-count)
      (format t \"~&[~vd to go] Done ~A~%\"
              1togo planned-output-action-count (action-description o))
      (finish-outputs))
    (mark-as-done plan o c)))
  (destructuring-bind (o . c) action
    (cond
      (backgroundp
       (perform o c)
       `(:deferred-warning ,(reify-deferred-warnings)))
      ((and (action-already-done-p plan o c) (not (needed-in-image-p o c)))
       nil)
      (t
       (perform-with-restarts o c)
       nil))))
 (mapc #'unreify-deferred-warnings all deferred-warnings)
 (assert (and (empty-p action-queue) (empty-p children))
         (parents children)
         \"Problem with the dependency graph: ~A\"
         (summarize-plan plan)))"))

(defparameter *cyber-tracker-2-pm--1995*
  '("
DIRECTORIES AVAILABLE
   CYBERTRACKER MAINTAINANCES FILES
   CYBER TECHNOLOGY GRID
   CYBER APPROPRIATION REPORTS

ACCESS CYBER APPROPRIATION REPORTS

"
    "

SYS INTERRUPT II65
REROUTE IN PROGRESS

ACCESS MEMLOC 1279AF400  ACCESSING…

"))

(defparameter *nasanet*
  '("
================== NASANET ========================

X-RAY FLUORESCENCE SPECTROMETER ANALYSIS
RESULTS

         SAMPLE                UTOPIA PLANITIA
------------------------   ------------------------
SILICON OXIDE         45   SILICON OXIDE         45
IRON                  15   IRON                  15
MAGNESIUM             08   MAGNESIUM             08
SULFUR                08   SULFUR                08
ALUMINUM              06   ALUMINUM              06
CALCIUM               06   CALCIUM               06
TITANIUM              01   TITANIUM              01
OTHER                 05   OTHER                 05

"
    )

  "Alien Invasion")
;;;---------------------------------------------------------------------
;;; Run it
;;;---------------------------------------------------------------------

(parse-options ext:*args*)
(ext:exit 0)

;;;; THE END ;;;;
