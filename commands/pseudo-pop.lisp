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


(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (unless (simple-string-p string)     (setq string     (copy-seq string)))
  (unless (simple-string-p separators) (setq separators (copy-seq separators)))
  (let ((chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)) )
    (declare (type simple-string string separators))
    (loop :while (< position strlen)
          :do (loop :while (and (< nextpos strlen)
                                (not (position (char string nextpos) separators)))
                    :do (setq nextpos (1+ nextpos)))
              (push (subseq string position nextpos) chunks)
              (setq position (1+ nextpos))
              (setq nextpos  position))
    (nreverse chunks)))


(defparameter crlf (format nil "~C~C" (code-char 13) (code-char 10)))


(defun writeln (fmt &rest args)
  (apply (function format) t fmt args)
  (princ crlf)
  (finish-output))


(defmacro with-gensyms (syms &body body)
  "
DO:      Replaces given symbols with gensyms. Useful for creating macros.
NOTE:    This version by Paul Graham in On Lisp."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms) ,@body))


(defmacro expect (&rest clauses)
  (with-gensyms (tag-loop tag-end line words)
    `(tagbody
        ,tag-loop
        (let* ((,line  (read-line))
               (,words (delete "" (split-string ,line " ")
                               :test (function string=))))
          ,@(mapcar (lambda (clause)
                      (let* ((key  (pop clause))
                             (var  (car (pop clause)))
                             (last (car (last clause)))
                             (exit))
                        (if (eq :done last)
                            (progn
                              (setf exit `(go ,tag-end))
                              (setf clause (butlast clause)))
                            (setf exit `(go ,tag-loop)))
                        (cond
                          ((stringp key)
                           `(when (string-equal ,key (first ,words))
                              ,(if var
                                   `(let ((,var ,line)) ,@clause)
                                   `(progn              ,@clause))
                              ,exit))
                          ((not (eq :otherwise key))
                           (error "Invalid expect string ~S." key))
                          (var `(let ((,var ,line)) ,@clause ,exit))
                          (t   `(progn              ,@clause ,exit)))))
                    clauses))
        ,tag-end)))


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

(defun pseudo-pop3-server ()

  (writeln "+OK POP3 ~A v7.64 server ready" (machine-instance))

  (expect
   ("USER" ()
           (writeln "+OK User name accepted, password please")
           :done)
   ("QUIT" ()
           (writeln "+OK Sayonara")
           (exit ex-ok))
   (:otherwise ()
               (writeln "-ERR Unknown AUTHORIZATION state command")))

  (expect
   ("PASS" ()
           (writeln "+OK Mailbox open, 0 messages")
           :done)
   ("QUIT" ()
           (writeln "+OK Sayonara")
           (exit ex-ok))
   (:otherwise ()
               (writeln "-ERR Unknown command")))

  (loop
     (expect
      ("LIST" ()
              (writeln "+OK Mailbox scan listing follows")
              (writeln "."))
      ("QUIT" ()
              (writeln "+OK Sayonara")
              (exit ex-ok))
      (:otherwise ()
                  (writeln "-ERR Unknown command")))))

(defun main (arguments)
  (declare (ignore arguments))
  (pseudo-pop3-server)
  ex-ok)

;;;; THE END ;;;;




