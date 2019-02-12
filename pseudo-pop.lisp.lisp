#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               pseudo-pop.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This is a POP-3 server that allows any user and password,
;;;;    and reports always an empty mailbox.
;;;;
;;;;    Can be installed in inetd.conf in place of pop-3 to
;;;;    temporarily disable the POP server without bothering the users.
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-04-05 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2006 - 2006
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

(UNUSE-PACKAGE "EXT")

(DEFUN SPLIT-STRING (STRING &OPTIONAL (SEPARATORS " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (UNLESS (SIMPLE-STRING-P STRING)     (SETQ STRING     (COPY-SEQ STRING)))
  (UNLESS (SIMPLE-STRING-P SEPARATORS) (SETQ SEPARATORS (COPY-SEQ SEPARATORS)))
  (LET ((CHUNKS  '())
        (POSITION 0)
        (NEXTPOS  0)
        (STRLEN   (LENGTH STRING)) )
    (DECLARE (TYPE SIMPLE-STRING STRING SEPARATORS))
    (LOOP WHILE (< POSITION STRLEN)
          DO
          (LOOP WHILE (AND (< NEXTPOS STRLEN)
                           (NOT (POSITION (CHAR STRING NEXTPOS) SEPARATORS)))
                DO (SETQ NEXTPOS (1+ NEXTPOS))
                );;loop
          (PUSH (SUBSEQ STRING POSITION NEXTPOS) CHUNKS)
          (SETQ POSITION (1+ NEXTPOS))
          (SETQ NEXTPOS  POSITION))
    (NREVERSE CHUNKS)))


(DEFPARAMETER CRLF (FORMAT NIL "~C~C" (CODE-CHAR 13) (CODE-CHAR 10)))


(DEFUN WRITELN (FMT &REST ARGS)
  (APPLY (FUNCTION FORMAT) T FMT ARGS)
  (PRINC CRLF)
  (FINISH-OUTPUT))


(DEFMACRO WITH-GENSYMS (SYMS &BODY BODY)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(LET ,(MAPCAR #'(LAMBDA (S) `(,S (GENSYM))) SYMS) ,@BODY))


(DEFMACRO EXPECT (&REST CLAUSES)
  (WITH-GENSYMS (TAG-LOOP TAG-END LINE WORDS)
    `(TAGBODY
      ,TAG-LOOP
      (LET* ((,LINE  (READ-LINE))
             (,WORDS (DELETE "" (SPLIT-STRING ,LINE " ")
                             :TEST (FUNCTION STRING=))))
        ,@(MAPCAR (LAMBDA (CLAUSE)
                    (LET* ((KEY  (POP CLAUSE))
                             (VAR  (CAR (POP CLAUSE)))
                             (LAST (CAR (LAST CLAUSE)))
                             (EXIT))
                        (IF (EQ :DONE LAST)
                          (PROGN
                            (SETF EXIT `(GO ,TAG-END))
                            (SETF CLAUSE (BUTLAST CLAUSE)))
                          (SETF EXIT `(GO ,TAG-LOOP)))
                        (COND
                         ((STRINGP KEY)
                          `(WHEN (STRING-EQUAL ,KEY (FIRST ,WORDS))
                             ,(IF VAR
                                `(LET ((,VAR ,LINE)) ,@CLAUSE)
                                `(PROGN              ,@CLAUSE))
                             ,EXIT))
                         ((NOT (EQ :OTHERWISE KEY))
                          (ERROR "Invalid expect string ~S." KEY))
                         (VAR `(LET ((,VAR ,LINE)) ,@CLAUSE ,EXIT))
                         (T   `(PROGN              ,@CLAUSE ,EXIT)))))
                  CLAUSES))
      ,TAG-END)))


;; +OK POP3 hermes.intra.afaa.asso.fr v7.64 server ready
;; USER pascal
;; USER pascal
;; +OK User name accepted, password please
;; PASS pari-fle
;; PASS pari-fle
;; +OK Mailbox open, 0 messages
;; LIST
;; LIST
;; +OK Mailbox scan listing follows
;; .
;; QUIT
;; QUIT
;; +OK Sayonara
;; Connection closed by foreign host.

(WRITELN "+OK POP3 hermes.intra.afaa.asso.fr v7.64 server ready")

(EXPECT
 ("USER" ()
  (WRITELN "+OK User name accepted, password please")
  :DONE)
 ("QUIT" ()
  (WRITELN "+OK Sayonara")
  (EXT:QUIT))
 (:OTHERWISE ()
  (WRITELN "-ERR Unknown AUTHORIZATION state command")))

(EXPECT
 ("PASS" ()
  (WRITELN "+OK Mailbox open, 0 messages")
  :DONE)
 ("QUIT" ()
  (WRITELN "+OK Sayonara")
  (EXT:QUIT))
 (:OTHERWISE ()
  (WRITELN "-ERR Unknown command")))

(LOOP
 (EXPECT
  ("LIST" ()
   (WRITELN "+OK Mailbox scan listing follows")
   (WRITELN "."))
  ("QUIT" ()
   (WRITELN "+OK Sayonara")
   (EXT:QUIT))
  (:OTHERWISE ()
   (WRITELN "-ERR Unknown command"))))


