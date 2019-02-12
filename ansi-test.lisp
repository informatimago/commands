;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               ansi-test
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Prints a test page for ANSI codes (ECMA-048) for text color
;;;;    and text rendering on a terminal.
;;;;
;;;;SYNOPSIS
;;;;    ansi-test
;;;;    xterm -xrm 'xterm*hold: on' -e ansi-test
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-12-24 <PJB> Updated for cesarum.
;;;;    2010-11-22 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2012
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************
(in-package "COMMON-LISP-USER")
(defpackage "COM.INFORMATIMAGO.COMMAND.ANSI-TEST"
  (:use "COMMON-LISP"))
(in-package "COM.INFORMATIMAGO.COMMAND.ANSI-TEST")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "1.0.2")
(defparameter *program-name* "ansi-test")
(define-option ("version" "-V" "--version") ()
  "Report the version of this script."
  (format t "~A ~A~%" *program-name* *program-version*))


(catch 'go-on
  (parse-options uiop:*command-line-arguments*
                 (lambda () (throw 'go-on t)))
  (uiop:quit 1))

(format t "~CcLoading...~%" (code-char 27))
(load (merge-pathnames (make-pathname :directory '(:relative "QUICKLISP")
                                      :name "SETUP" :type "LISP" :version NIL
                                      :case :common :defaults (user-homedir-pathname))
                       (user-homedir-pathname) nil))

(without-output
  (ql:quickload :com.informatimago.common-lisp))

(shadow 'concat)
(use-package "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")

(defpackage "E48" (:use))
(let ((*package* (find-package "E48")))
  (without-output
      (eval (funcall (or (find-symbol "GENERATE-ALL-FUNCTIONS"
                                      "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ECMA048")
                         (lambda (&rest args)
                           (declare (ignore args))
                           (error "Cannot generate the ECMA048 functions")))
                     :export  common-lisp:t
                     :8-bit   common-lisp:nil
                     :print   common-lisp:t
                     :result-type 'common-lisp:string))))



(defenum sgr-codes
  "
    0 normal (reset)
    1 bold or increased intensity
    2 faint, decreased intensity or second colour
    3 italicized
    4 singly underlined
    5 slowly blinking (less then 150 per minute)
    6 rapidly blinking (150 per minute or more)
    7 negative image
    8 concealed characters
    9 crossed-out (characters still legible but marked as to be deleted)
    10 primary (default) font
    11 first alternative font
    12 second alternative font
    13 third alternative font
    14 fourth alternative font
    15 fifth alternative font
    16 sixth alternative font
    17 seventh alternative font
    18 eighth alternative font
    19 ninth alternative font
    20 Fraktur (Gothic)
    21 doubly underlined
    22 normal colour or normal intensity (neither bold nor faint)
    23 not italicized, not fraktur
    24 not underlined (neither singly nor doubly)
    25 steady (not blinking)
    26 (reserved for proportional spacing as specified in CCITT Recommendation
       T.61)
    27 positive image
    28 revealed characters
    29 not crossed out
    30 black display
    31 red display
    32 green display
    33 yellow display
    34 blue display
    35 magenta display
    36 cyan display
    37 white display
    38 (reserved for future standardization; intended for setting
       character foreground colour as specified in ISO 8613-6 [CCITT
       Recommendation T.416])
    39 default display colour (implementation-defined)
    40 black background
    41 red background
    42 green background
    43 yellow background
    44 blue background
    45 magenta background
    46 cyan background
    47 white background
    48 (reserved for future standardization; intended
       for setting character background colour as specified in ISO 8613-6
       [CCITT Recommendation T.416])
    49 default background colour (implementation-defined)
    50 (reserved for cancelling the effect of the rendering aspect
       established by parameter value 26)
    51 framed
    52 encircled
    53 overlined
    54 not framed, not encircled
    55 not overlined
    56 (reserved for future standardization)
    57 (reserved for future standardization)
    58 (reserved for future standardization)
    59 (reserved for future standardization)
    60 ideogram underline or right side line
    61 ideogram double underline or double line on the right side
    62 ideogram overline or left side line
    63 ideogram double overline or double line on the left side
    64 ideogram stress marking
    65 cancels the effect of the rendition aspects established by
       parameter values 60 to 64.
"
  normal
  bold faint italic underline slow-blink fast-blink invert hidden
  crossed-out primary-font first-font second-font third-font fourth-font
  fifth-font sixth-font seventh-font eighth-font ninth-font gothic
  double-underline no-bold no-italic no-underline no-blink reserved-1
  no-invert no-hidden no-crossed-out black-foreground red-foreground
  green-foreground yellow-foreground blue-foreground magenta-foreground
  cyan-foreground white-foreground reseved-2 default-foreground
  black-background red-background green-background yellow-background
  blue-background magenta-background cyan-background white-background
  reserved-3 default-background reserved-4 framed encircled overlined
  not-framed not-verlined reserved-5 reserved-6 reserved-7 reserved-8
  ideogram-underline ideogram-double-underline ideogram-overline
  ideogram-double-overline ideogram-stress ideogram-cancel)

(defconstant blink slow-blink)


(defun ansi-test ()
  (e48:ris)

  (let ((y 0)
        (x 0)
        bb ff)
    (flet ((label (col)
                  (format nil "~8a"
                          (subseq (string (sgr-codes-label col)) 0
                                  (position (character "-")
                                            (string (sgr-codes-label col)))))))
      (dolist (b (list WHITE-background
                       CYAN-background MAGENTA-background YELLOW-background
                       BLUE-background GREEN-background RED-background
                       BLACK-background))
        (setf bb (label b))
        (dolist (f (list WHITE-foreground
                         CYAN-foreground MAGENTA-foreground YELLOW-foreground
                         BLUE-foreground GREEN-foreground RED-foreground
                         BLACK-foreground))
          (setf ff (label f))
          (e48:cup y x)
          (incf y)
          (when (= y 16) (setf y 0 x (+ x 20)))
          (e48:sgr b)
          (e48:sgr f)
          (format t "~A on ~A " ff bb)
          (e48:sgr normal)))))

  (let ((y 16)
        (x 0)
        jin jbl jun jbo)
    (flet ((label (mode)
                  (format nil "~8a"
                          (subseq (string (sgr-codes-label mode)) 0
                                  (min (length (string (sgr-codes-label mode)))
                                       8)))))
      (dolist (in (list no-invert invert))
        (setf jin (label in))
        (dolist (bl (list no-blink blink))
          (setf jbl (label bl))
          (dolist (un (list no-underline underline))
            (setf jun (label un))
            (dolist (bo (list no-bold bold))
              (setf jbo (label bo))
              (e48:cup y x)
              (incf y)
              (e48:sgr in)
              (e48:sgr bl)
              (e48:sgr un)
              (e48:sgr bo)
              (format t "  ~A ~A ~A ~A    " jin jbl jun jbo)
              (e48:sgr normal))))
        (setf y 16 x 40))))

  (finish-output))

(ansi-test)

;;;; THE END ;;;;
