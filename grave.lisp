#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              grave
;;;;LANGUAGE:          emacs lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This scripts simplify burning CDs.
;;;;USAGE
;;;;    grave --help
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-03-25 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
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

;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP")
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))


;; We only add logical pathname translations. This can be done from any package.
(let ((BASE (make-pathname :directory '(:absolute "USR" "LOCAL" "SHARE" "LISP")
                           :CASE :COMMON)))
  ((LAMBDA (&REST SPECS)
     (MAPCAR (LAMBDA (SPEC)
               (SETF (LOGICAL-PATHNAME-TRANSLATIONS (CAR SPEC))
                     (COPY-SEQ (CDR SPEC)))) SPECS))
   (LIST
    "PACKAGES"
    `("PACKAGES:**;*"      ,(merge-pathnames "packages/**/*"     BASE))
    `("PACKAGES:**;*.*"    ,(merge-pathnames "packages/**/*.*"   BASE))
    `("PACKAGES:**;*.*.*"  ,(merge-pathnames "packages/**/*.*.*" BASE)))))
;;(print  (LOGICAL-PATHNAME-TRANSLATIONS "PACKAGES"))
(load  "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE")
(use-package    "COM.INFORMATIMAGO.COMMON-LISP.PACKAGE")
(load  "PACKAGES:COM;INFORMATIMAGO;CLISP;SCRIPT")
(add-nickname   "COM.INFORMATIMAGO.CLISP.SCRIPT" "SCRIPT")
(SCRIPT:INITIALIZE)


(DEFPARAMETER VOLUME-LIST        "~/.cd-volumes"
  "Where (volset,volume) names are stored.");;VOLUME-LIST

(DEFPARAMETER DEFAULT-IMAGE-PATH "/tmp/cdrom.iso"
  "The path to the CD image.");;DEFAULT-IMAGE-PATH



(DEFUN CHECK-VALUE (NAME VALUE CHAR-SET MIN-LENGTH MAX-LENGTH)
  "
RETURN:     The VALUE if it is a string with only characters in CHAR-SET,
            and whose length is between MIN-LENGTH and MAX-LENGTH inclusive,
            or else NIL.
DO:         If the VALUE does not check then writes on *ERROR-OUTPUT*
            a message indicating the problem, tagged with NAME.
CHAR-SET:   A string containing the valid characters or character ranges:
                char-set ::= { character | range } .
                range ::= character '-' character .
            Since '-' is used for character range, it must be the first
            if it is to be explicitely listed (it may be included in a range).
NOTE:       ranges depend on the encoding used. It's probably better to list
            each characters ('-' first).
"
  (COND
   ((NULL VALUE)
    (SCRIPT:PERROR "No value given for ~A.~%" NAME)
    NIL)
   ((< (LENGTH VALUE) MIN-LENGTH)
    (SCRIPT:PERROR "The value \"~A\"~%
             for ~A is too short (minimum length is ~A).~%"
            VALUE NAME MIN-LENGTH)
    NIL)
   ((< MAX-LENGTH (LENGTH VALUE))
    (SCRIPT:PERROR "The value \"~A\"~%
             for ~A~% is too long (maximum length is ~A).~%"
            VALUE NAME MAX-LENGTH)
    NIL)
   (T
    (LET* ((PATTERN   (FORMAT NIL "[~A]" CHAR-SET))
           (BAD-CHARS (REGEXP:REGEXP-SPLIT PATTERN VALUE)))
      (IF BAD-CHARS
        (PROGN
         (SCRIPT:PERROR "The value \"~A\"~%
             for ~A~% contains an invalid character '~A' (valid set is ~A).~%"
            VALUE NAME (CAR BAD-CHARS) CHAR-SET)
         NIL)
        VALUE))))
  );;CHECK-VALUE


(DEFUN CHECK-VOLSET (VALUE)
  "
SEE-ALSO:   CHECK-VALUE.
"
  (CHECK-VALUE "volset" VALUE "A-Z0-9_" 0 278)
  );;CHECK-VOLSET


(DEFUN CHECK-VOLUME (VALUE)
  "
SEE-ALSO:   CHECK-VALUE.
"
  (CHECK-VALUE "volume" VALUE "A-Z0-9_" 0 32)
  );;CHECK-VOLUME



(DEFUN PROTECTED-PROBE-FILE (PATHSPEC)
  "
RETURN:     The result of (PROBE-FILE PATHSPEC) or NIL if it raises an error.
            CLISP PROBE-FILE raises an error when passed a PATHSPEC of a
            directory, I'm not sure this is legal!
"
  (LET ((RESULT NIL))
    (UNWIND-PROTECT
        (SETQ RESULT (PROBE-FILE PATHSPEC)))
    RESULT)
  );;PROTECTED-PROBE-FILE



(DEFUN FILE-IS-AVI (AVI-FILE-PATH)
  "
RETURN:     Whether the file command indicate that AVI-FILE-PATH
            is an AVI file.
"
  (= 0  (SCRIPT:EXECUTE
         "sh" "-c" (FORMAT NIL "file -b ~A | grep -q -s AVI"
                           (SCRIPT:SHELL-QUOTE-ARGUMENT AVI-FILE-PATH))))
  );;FILE-IS-AVI



(DEFUN CHECK-AVI-FILE-PATH (AVI-FILE-PATH)
  "
RETURN:     The AVI-FILE-PATH if it is a string containing the path
            to a readable AVI file, or else NIL.
DO:         If the AVI-FILE-PATH does not check then writes on *ERROR-OUTPUT*
            a message indicating the problem.
"
  (COND
   ((NOT (PROTECTED-PROBE-FILE AVI-FILE-PATH)) ;; The file does not exist.
    (SCRIPT:PERROR "There's no file named '~A'.~%" AVI-FILE-PATH)
    NIL)
   ((LET ((RESULT T))
      (UNWIND-PROTECT
          (SETQ RESULT (NULL (OPEN  AVI-FILE-PATH
                                    :DIRECTION :PROBE
                                    :IF-DOES-NOT-EXIST NIL)))))
    (SCRIPT:PERROR "Can't read '~A'.~%" AVI-FILE-PATH)
    NIL)
   ((NOT (FILE-IS-AVI AVI-FILE-PATH))
    (SCRIPT:PERROR "'~A' is not an AVI file.~%" AVI-FILE-PATH)
    NIL)
   (T
    AVI-FILE-PATH) )
  );;CHECK-AVI-FILE-PATH



(DEFUN CHECK-CD-IMAGE-PATH (CD-IMAGE-PATH EXISTS)
  "
RETURN:     If EXISTS then if CD-IMAGE-PATH is a string containing a path to
                    an existing readable file, or else NIL;
            else if  CD-IMAGE-PATH is a string containing a path to
                    an inexistant file in writeable directory, or else NIL.
DO:         If the CD-IMAGE-PATH does not check then writes on *ERROR-OUTPUT*
            a message indicating the problem.
"
  (IF EXISTS
    (COND
     ((NOT (PROTECTED-PROBE-FILE CD-IMAGE-PATH)) ;; The file does not exist.
      (SCRIPT:PERROR "There's no file named '~A'.~%" CD-IMAGE-PATH)
      NIL)
     ((LET ((RESULT T))
        (UNWIND-PROTECT
            (SETQ RESULT (NULL (OPEN  CD-IMAGE-PATH
                                      :DIRECTION :PROBE
                                      :IF-DOES-NOT-EXIST NIL)))))
      (SCRIPT:PERROR "Can't read '~A'.~%" CD-IMAGE-PATH)
      NIL)
     (T
      CD-IMAGE-PATH) )
    (IF (LET ((CAN-CREATE NIL))
          (UNWIND-PROTECT
              (SETQ CAN-CREATE (NOT (NULL
                                     (OPEN CD-IMAGE-PATH
                                           :DIRECTION :OUTPUT
                                           :IF-EXISTS NIL
                                           :IF-DOES-NOT-EXIST :CREATE))))
            (WHEN CAN-CREATE (DELETE-FILE CD-IMAGE-PATH)))
          (NOT CAN-CREATE))
      (PROGN
        (SCRIPT:PERROR "Can't create file '~A'.~%" CD-IMAGE-PATH)
        NIL)
      CD-IMAGE-PATH) )
  );;CHECK-CD-IMAGE-PATH



(DEFUN PRINT-USAGE ()
  "
DO:     Prints the usage on the standard output.
"
  (SCRIPT:PMESSAGE "usage:~%~
    ~&    ~A -h|--help | ( -b|--burn-again[-sam] ) [image] ~%~
    ~&    ~V,A | [volset [volume [path [image]]]]~%~
    ~&(default image path is /tmp/cdrom.iso).~%"
   SCRIPT:NAME SCRIPT:NAME (LENGTH SCRIPT:NAME) "")
  );;PRINT-USAGE


(DEFUN UPDATE-VOLUME-LIST (VOLSET VOLUME)
  "
DO:     Updates the volume-list file.
"
  (WITH-OPEN-FILE (OUT VOLUME-LIST :DIRECTION :OUTPUT :IF-EXISTS :APPEND)
    (FORMAT OUT "# ~43A  ~32A~%" VOLSET VOLUME))
  );;UPDATE-VOLUME-LIST


(DEFUN MAIN (ARGUMENTS)
  "
"
  (LET ((ONLY-BURN     NIL) ;; Whether we just burn an existing image.
        (VOLSET        NIL) ;; The name of the volume set.
        (VOLUME        NIL) ;; The name of the volume.
        (AVI-FILE-PATH NIL) ;; The path of the file to put on the CD.
        (IMAGE-PATH    NIL) ;; The path of the image to burn.
        (STATUS)
        )
    ;; PROCESS THE ARGUMENTS:
    (DOLIST (ARG ARGUMENTS)
      (COND
       ((OR (STRING= "-h" ARG) (STRING= "--help" ARG))
        (PRINT-USAGE)
        (SCRIPT:EXIT SCRIPT:EX-OK)
        )
       ((OR (STRING= "-b" ARG)
            (STRING= "--burn-again" ARG)
            (STRING= "--burn-again-sam" ARG))
        (SETQ ONLY-BURN T))
       ((STRING= (AREF ARG 0) (CHARACTER '\-))
        (SCRIPT:PERROR "invalid argument '~A'.~%" ARG)
        (PRINT-USAGE)
        (SCRIPT:EXIT SCRIPT:EX-USAGE))
       (ONLY-BURN
        (WHEN IMAGE-PATH
          (SCRIPT:PERROR "unexpected argument '~A'; ~
                  already got an image path '~A'.~%"
                  SCRIPT:NAME ARG IMAGE-PATH)
          (PRINT-USAGE)
          (SCRIPT:EXIT SCRIPT:EX-USAGE))
        (SETQ IMAGE-PATH (CHECK-CD-IMAGE-PATH ARG T))
        (UNLESS IMAGE-PATH
          (SCRIPT:EXIT SCRIPT:EX-DATAERR)) )
       ((NULL VOLSET)
        (SETQ VOLSET (CHECK-VOLSET ARG))
        (UNLESS VOLSET
          (SCRIPT:EXIT SCRIPT:EX-DATAERR)) )
       ((NULL VOLUME)
        (SETQ VOLUME (CHECK-VOLUME ARG))
        (UNLESS VOLUME
          (SCRIPT:EXIT SCRIPT:EX-DATAERR)) )
       ((NULL AVI-FILE-PATH)
        (SETQ AVI-FILE-PATH (CHECK-AVI-FILE-PATH ARG))
        (UNLESS AVI-FILE-PATH
          (SCRIPT:EXIT SCRIPT:EX-DATAERR)) )
       ((NULL IMAGE-PATH)
        (SETQ IMAGE-PATH (CHECK-CD-IMAGE-PATH ARG NIL))
        (UNLESS IMAGE-PATH
          (SCRIPT:EXIT SCRIPT:EX-DATAERR)) )
       (T
        (SCRIPT:PERROR "Too many argument: '~A'.~%" ARG)
        (PRINT-USAGE)
        (SCRIPT:EXIT SCRIPT:EX-USAGE))
       )) ;;DOLIST
    (UNLESS IMAGE-PATH (SETQ IMAGE-PATH DEFAULT-IMAGE-PATH))
    ;; PREPARE THE CD IMAGE:
    (UNLESS ONLY-BURN
      (DO () (VOLSET) (SETQ VOLSET (CHECK-VOLSET (SCRIPT:PQUERY "Enter volset: "))))
      (DO () (VOLUME) (SETQ VOLUME (CHECK-VOLUME (SCRIPT:PQUERY "Enter volume: "))))
      (DO () (AVI-FILE-PATH)
        (SETQ AVI-FILE-PATH (CHECK-AVI-FILE-PATH
                             (SCRIPT:PQUERY "Enter AVI file path: "))))
      (UPDATE-VOLUME-LIST VOLSET VOLUME)
      (SETQ STATUS (SCRIPT:EXECUTE
                    "mkisofs" "-v" "-r" "-T" "-J" "-pad"
                    "-volset-size" "1" "-volset-seqno" "1"
                    "-volset" VOLSET "-V" VOLUME "-O" IMAGE-PATH
                    AVI-FILE-PATH))
      (WHEN (/= 0 STATUS)
        (SCRIPT:PERROR "mkisofs failed with status ~A.~%" STATUS)
        (SCRIPT:EXIT SCRIPT:EX-IOERR))
      ) ;;UNLESS ONLY-BURN

    ;; BURN THE CD:
    (DO ((SUCCESS NIL))
        (SUCCESS)
      (SETQ STATUS (SCRIPT:EXECUTE "cdrecord" "-v" "-data" IMAGE-PATH))
      (WHEN (= 0 STATUS)
        (SETQ STATUS (SCRIPT:EXECUTE "mount" "/cdrom")))
      (IF (= 0 STATUS)
        (PROGN
          (SCRIPT:PMESSAGE "GOODY-GOOD!~%")
          (SCRIPT:EXECUTE "ls" "-l" "/cdrom/.")
          (SCRIPT:PMESSAGE "Comparing...~%")
          (SETQ STATUS (SCRIPT:EXECUTE "cmp" "/dev/sr0" IMAGE-PATH))
          (IF (= 0 STATUS)
            (PROGN
              (SCRIPT:PMESSAGE "SAME CD IMAGES ~%")
              (SCRIPT:EXECUTE "umount" "/cdrom")
              (SCRIPT:EXECUTE "eject")
              (SETQ SUCCESS T) )
            (PROGN
              (SCRIPT:PMESSAGE "CD IMAGES ARE DIFFERENT!~%")
              (SCRIPT:EXECUTE "umount" "/cdrom")
              (SCRIPT:EXECUTE "eject")
              (SCRIPT:PQUERY "PLEASE PUT A NEW CDR AND PRESS RETURN: ") )))
        (PROGN
          (SCRIPT:PMESSAGE "BADDY-BAD!~%")
          (SCRIPT:EXECUTE "eject")
          (SCRIPT:PQUERY "PLEASE PUT A NEW CDR AND PRESS RETURN: ") )))
    (SCRIPT:EXIT SCRIPT:EX-OK))
  );;MAIN


(WHEN (SCRIPT:IS-RUNNING) (MAIN SCRIPT:ARGUMENTS))


;;;; grave                            --                     --          ;;;;
