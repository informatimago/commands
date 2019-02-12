#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               surveille-host
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             CLISP
;;;;USER-INTERFACE:     Common-Lisp
;;;;DESCRIPTION
;;;;
;;;;    This scripts pings regularly an IP address (or list of IP addresses)
;;;;    and signals (and/or sends an email) when a change occurs (remote
;;;;    going on-line or off-line).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-08-14 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1992 - 2003
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;*****************************************************************************

(SETQ *LOAD-VERBOSE*    NIL)
(LOAD "~/.common.lisp")
;;(SETQ PACKAGE:*VERBOSE* T)


(DECLARE-PACKAGE COM.INFORMATIMAGO.CLISP.SURVEILLE-HOST
  (:NICKNAMES SURVEILLE-HOST)
  (:DOCUMENTATION
   "This scripts pings regularly an IP address (or list of IP addresses)
    and signals (and/or sends an email) when a change occurs -- remote
    going on-line or off-line.
   ")
  (:FROM COMMON-LISP :IMPORT :ALL)
  (:USE  EXT)
  (:FROM COM.INFORMATIMAGO.COMMON-LISP.STRING   :IMPORT :ALL)
  (:FROM COM.INFORMATIMAGO.COMMON-LISP.ACTIVITY :IMPORT :ALL)
  (:USE  COM.INFORMATIMAGO.CLISP.SCRIPT)
  (:EXPORT REMOTE ADD-REMOTE MAIN ))
(IN-PACKAGE "COM.INFORMATIMAGO.CLISP.SURVEILLE-HOST")
(SCRIPT:INITIALIZE)


(DEFPARAMETER *FROM-EMAIL* "surveille-host <pjb@informatimago.com>")


(DEFPARAMETER *REMOTES* (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL))
  "A map: ip-address --> remotes.");;*REMOTES*


(DEFUN RESET-REMOTES ()
  "
POST:   There is no remote defintions in *REMOTES*.
"
  (SETQ *REMOTES* (MAKE-HASH-TABLE :TEST (FUNCTION EQUAL)))
  );;RESET-REMOTES



(DEFCLASS REMOTE NIL
  (
   (NAME
    :ACCESSOR NAME
    :INITARG :NAME
    :INITFORM "Unnamed"
    :TYPE     STRING
    :DOCUMENTATION "The name of the remote.")
   (IP-ADDRESS
    :ACCESSOR IP-ADDRESS
    :INITARG :IP-ADDRESS
    :INITFORM "127.0.0.1"
    :TYPE     STRING
    :DOCUMENTATION "The IP address of the remote.")
   (EMAIL
    :ACCESSOR EMAIL
    :INITARG :EMAIL
    :INITFORM NIL
    :TYPE     (OR NULL STRING)
    :DOCUMENTATION "The email address to be signaled when state change.")
   (CHECK-PERIOD
    :ACCESSOR CHECK-PERIOD
    :INITARG :CHECK-PERIOD
    :INITFORM 120
    :TYPE     NUMBER
    :DOCUMENTATION "The period in second of checking.")
   (STATE
    :ACCESSOR STATE
    :INITARG :STATE
    :INITFORM :UNKNOWN
    :DOCUMENTATION
    "Whether the remote state is :unknown, :off-line or :on-line.")
   (ACTIVITY
    :ACCESSOR ACTIVITY
    :INITFORM NIL
    :DOCUMENTATION "The activity running for this remote.")
   )
  (:DOCUMENTATION "A remote host definition.")
  );;REMOTE


(DEFPARAMETER *DO-LOGGING* NIL)

(DEFMACRO LOGGING (ACTION &BODY BODY)
  `(PROGN
     (WHEN *DO-LOGGING*
       (FORMAT *TRACE-OUTPUT* "~2%BEGINNING ~A~%" (STRING ,ACTION)))
     (PROG1 (PROGN ,@BODY)
       (WHEN *DO-LOGGING*
         (FORMAT *TRACE-OUTPUT* "~%COMPLETED ~A~%" (STRING ,ACTION)))))
  );;LOGGING


(DEFMETHOD GET-CURRENT-STATE ((SELF REMOTE))
  "
RETURN: Whether :ON-LINE or :OFF-LINE according to the status of ping.
"
  (LOGGING (FORMAT NIL "pinging ~A" (NAME SELF))
           (IF (= 0 (EXT:RUN-SHELL-COMMAND
                     (FORMAT NIL "ping -c 10 -w 15 -n -q ~S" (IP-ADDRESS SELF))
                     :INPUT NIL :OUTPUT NIL))
             :ON-LINE
             :OFF-LINE))
  );;GET-CURRENT-STATE


(DEFMETHOD NOTIFICATE-STATE-CHANGE ((SELF REMOTE))
  (WHEN (EMAIL SELF)
    (LOGGING (FORMAT NIL "sendmail ~A" (EMAIL SELF))
             (LET ((MSG-STREAM   (EXT:RUN-SHELL-COMMAND
                                  (FORMAT NIL "sendmail ~A" (EMAIL SELF))
                                  :INPUT :STREAM :OUTPUT NIL)))
               (FORMAT MSG-STREAM
                       (CONCATENATE 'STRING
                         "From: ~A~%"
                         "To: ~A~%"
                         "Subject: ~A going ~A~%"
                         "~%"
                         "I've the pleasure to inform you that ~%"
                         "the remote host ~A (~A)~%"
                         "just went ~A.~%"
                         "~%"
                         "-- ~%"
                         "The surveille-host script.~%")
                       *FROM-EMAIL*
                       (EMAIL SELF)
                       (NAME SELF)
                       (IF (EQ :ON-LINE (STATE SELF)) "on-line" "off-line")
                       (NAME SELF) (IP-ADDRESS SELF)
                       (IF (EQ :ON-LINE (STATE SELF)) "on-line" "off-line")
                       )
               (CLOSE MSG-STREAM))))
  );;NOTIFICATE-STATE-CHANGE


(DEFMETHOD CHECK ((SELF REMOTE))
  (LET ((NEW-STATE (GET-CURRENT-STATE SELF)))
    (UNLESS (EQ NEW-STATE (STATE SELF))
      (IF (EQ :UNKNOWN (STATE SELF))
        (PROGN
          (SETF (STATE SELF) NEW-STATE)
          (NOTIFICATE-STATE-CHANGE SELF))
        (SETF (STATE SELF) NEW-STATE))))
  );;CHECK





(DEFUN FIND-REMOTE-WITH-IP-ADDRESS (IP-ADDRESS)
  "
RETURN: The remote instance that has the given IP-ADDRESS, or NIL.
"
   (GETHASH IP-ADDRESS *REMOTES*)
   );;FIND-REMOTE-WITH-IP-ADDRESS


(DEFUN ADD-REMOTE (NAME IP-ADDRESS EMAIL)
  "
DO:    If there is already a remote with the same IP-ADDRESS
       then raise an error
       else create such a new remote.
"
  (IF (FIND-REMOTE-WITH-IP-ADDRESS IP-ADDRESS)
    (ERROR "There is already a remote with the same IP address ~S." IP-ADDRESS)
    (LET ((REMOTE (MAKE-INSTANCE 'REMOTE
                         :NAME NAME
                         :IP-ADDRESS IP-ADDRESS
                         :EMAIL EMAIL)))
      (SETF (GETHASH IP-ADDRESS *REMOTES*) REMOTE)
      (SETF (ACTIVITY REMOTE) (MAKE-INSTANCE 'ACTIVITY
                               :NAME (NAME REMOTE)
                               :PERIOD (CHECK-PERIOD REMOTE)
                               :ACTIVE T
                               :CLOSURE (LAMBDA () (CHECK REMOTE))))
      (SCHEDULE (ACTIVITY REMOTE))))
  );;ADD-REMOTE



(DEFUN MAIN (ARGV)
  (DECLARE (IGNORE ARGV))
  (ADD-REMOTE "hermes"  "195.114.85.131"  "pjb@informatimago.com")
  (ADD-REMOTE "janus-1" "195.114.85.145"  "pjb@informatimago.com")
  (ADD-REMOTE "janus-2" "195.114.85.146"  "pjb@informatimago.com")
  (ADD-REMOTE "janus-3" "195.114.85.147"  "pjb@informatimago.com")
  (ADD-REMOTE "janus-4" "195.114.85.148"  "pjb@informatimago.com")
  (ADD-REMOTE "bolet"   "bolet.no-ip.com" "pjb@informatimago.com")
  (LET ((PERIOD (* 30 (HASH-TABLE-SIZE *REMOTES*))))
    (MAPHASH (LAMBDA (KEY REMOTE)
               (DECLARE (IGNORE KEY))
               (SETF (CHECK-PERIOD REMOTE) PERIOD)
               (SETF (PERIOD (ACTIVITY REMOTE)) PERIOD)) *REMOTES*))
  (ACTIVITY-RUN :DEBUG NIL)
  );;MAIN


(WHEN (SCRIPT:IS-RUNNING)
  (MAIN SCRIPT:*ARGUMENTS*))

;;;; surveille-host                   --                     --          ;;;;
