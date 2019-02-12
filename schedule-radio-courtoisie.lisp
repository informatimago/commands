;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               radio-courtoisie-schedule
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Records radio-courtoisie broadcasts.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-05-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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

;; mms://viptvr.yacast.net/tvr_radiocourtoisie
;; mplayer -quiet -vc null -vo null -ao pcm:fast -ao pcm:file=/tmp/$(date +%Y%m%dT%H%M%S).pcm  mms://viptvr.yacast.net/tvr_radiocourtoisie


(defparameter *alternating-periods*
  '(week
    (semaine-a semaine-b semaine-c semaine-d)
    (day-in-week-p 2007-10-08 'semaine-a)))


(defparameter *schedule*
  '(((:name "Radio Courtoisie - La Radio Libre du Pays Réel et de la Francophonie")
     (:web-site-url "http://www.radiocourtoisie.net/")
     (:stream-url "mms://viptvr.yacast.net/tvr_radiocourtoisie")
     (:record-directory "/tmp/")
     (:date 2007-09-17))
    (semaine-a
     ("Livre du jour - Lacant"
      01.00.00 (monday    06.00.00))
     ("Livre du jour - Jacques Prouvost"
      01.00.00 (monday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 10.45.00)))
     ("Livre du jour - Collin et Paoli"
      01.00.00 (tuesday   10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 14.00.00)))
     ("Livre du jour - Pierre Mancheron"
      01.00.00 (wednesday 10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 18.00.00)))
     ("Livre du jour - Anne Brassié"
      01.00.00 (thirsday  10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 19.00.00)))
     ("Livre du jour - Hablot"
      01.00.00 (friday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 20.00.00)))
     ("Libre journal - Maxence"
      01.30.00 (monday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Lejeune"
      01.30.00 (tuesday   12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Média"
      01.30.00 (wednesday 12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Collectionneurs"
      01.30.00 (thirsday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal de l'Exploration"
      01.30.00 (friday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Lycéens"
      01.30.00 (saturday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal d'Henri de Lesquen"
      03.00.00 (monday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal d'Yves de Saint Robert"
      03.00.00 (tuesday   18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de Serge de Beketch"
      03.00.00 (wednesday 18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de François Giraud"
      03.00.00 (thirsday  18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de Sévillia"
      03.00.00 (friday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre Journal de la Nuit"
      02.00.00 (wednesday 21.30.00
                          (next friday 21.30.00)))
     ("Archives de Radio Courtoisie"
      03.00.00 (sunday  02.00.00))
     ("Chant Grégorien"
      01.00.00 (sunday 06.00.00
                       10.00.00
                       (next monday 05.00.00)))
     ("Saint du jour"
      01.00.00 (sunday 07.30.00
                       14.00.00))
     ("Scouts"
      01.30.00 (sunday 08.30.00
                       (next monday 09.00.00)))
     ("Images bibliques"
      01.00.00 (sunday 11.00.00))
     ("Lumière 101 - Dreyfus"
      01.30.00 (sunday 12.00.00
                       16.00.00
                       (next day 00.00.00)))
     ("Libre journal des Auditeurs et Musiciens"
      03.00.00 (sunday 18.00.00
                       02.00.00)))

    (semaine-b
     ("Livre du jour - Lacant"
      01.00.00 (monday    06.00.00))
     ("Livre du jour - Salon"
      01.00.00 (monday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 10.45.00)))
     ("Livre du jour - Collin et Paoli"
      01.00.00 (tuesday   10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 14.00.00)))
     ("Livre du jour - Helly"
      01.00.00 (wednesday 10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 18.00.00)))
     ("Livre du jour - Brassié"
      01.00.00 (thirsday  10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 19.00.00)))
     ("Livre du jour - Dehay"
      01.00.00 (friday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 20.00.00)))
     ("Libre journal - Bled"
      01.30.00 (monday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Beaux-arts"
      01.30.00 (tuesday   12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Sainte Marie"
      01.30.00 (wednesday 12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Kerros"
      01.30.00 (thirsday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Gourin"
      01.30.00 (friday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Lycéens"
      01.30.00 (saturday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal d'Henri de Lesquen"
      03.00.00 (monday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal - Le Méné"
      03.00.00 (tuesday   18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de Serge de Beketch"
      03.00.00 (wednesday 18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal des Historiens"
      03.00.00 (thirsday  18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal - Rouvier"
      03.00.00 (friday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre Journal de la Nuit"
      02.00.00 (wednesday 21.30.00
                          (next friday 21.30.00)))
     ("Archives de Radio Courtoisie"
      03.00.00 (sunday  02.00.00))
     ("Chant Grégorien"
      01.00.00 (sunday 06.00.00
                       10.00.00
                       (next monday 05.00.00)))
     ("Saint du jour"
      01.00.00 (sunday 07.30.00
                       14.00.00))
     ("Scouts"
      01.30.00 (sunday 08.30.00
                       (next monday 09.00.00)))
     ("Images bibliques"
      01.00.00 (sunday 11.00.00))
     ("Lumière 101 - Sureau"
      01.30.00 (sunday 12.00.00
                       16.00.00
                       (next day 00.00.00)))
     ("Libre journal des Auditeurs et Musiciens"
      03.00.00 (sunday 18.00.00
                       02.00.00)))

    (semaine-c
     ("Livre du jour - Lacant"
      01.00.00 (monday    06.00.00))
     ("Livre du jour - Prouvost"
      01.00.00 (monday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 10.45.00)))
     ("Livre du jour - Collin et Paoli"
      01.00.00 (tuesday   10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 14.00.00)))
     ("Livre du jour - Debray"
      01.00.00 (wednesday 10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 18.00.00)))
     ("Livre du jour - Brassié"
      01.00.00 (thirsday  10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 19.00.00)))
     ("Livre du jour - Hablot"
      01.00.00 (friday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 20.00.00)))
     ("Libre journal - Rostolan"
      01.30.00 (monday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Lejeune"
      01.30.00 (tuesday   12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Économistes"
      01.30.00 (wednesday 12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Idées Politiques"
      01.30.00 (thirsday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Éditeurs"
      01.30.00 (friday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Lycéens"
      01.30.00 (saturday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal d'Henri de Lesquen"
      03.00.00 (monday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal des Artisans"
      03.00.00 (tuesday   18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de Serge de Beketch"
      03.00.00 (wednesday 18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal - Bild"
      03.00.00 (thirsday  18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal - Level"
      03.00.00 (friday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre Journal de la Nuit"
      02.00.00 (wednesday 21.30.00
                          (next friday 21.30.00)))
     ("Archives de Radio Courtoisie"
      03.00.00 (sunday  02.00.00))
     ("Chant Grégorien"
      01.00.00 (sunday 06.00.00
                       10.00.00
                       (next monday 05.00.00)))
     ("Saint du jour"
      01.00.00 (sunday 07.30.00
                       14.00.00))
     ("Scouts"
      01.30.00 (sunday 08.30.00
                       (next monday 09.00.00)))
     ("Images bibliques"
      01.00.00 (sunday 11.00.00))
     ("Lumière 101 - Daniel Hamiche"
      01.30.00 (sunday 12.00.00
                       16.00.00
                       (next day 00.00.00)))
     ("Libre journal des Auditeurs et Musiciens"
      03.00.00 (sunday 18.00.00
                       02.00.00)))

    (semaine-d
     ("Livre du jour - Lacant"
      01.00.00 (monday    06.00.00))
     ("Livre du jour - Mourlet"
      01.00.00 (monday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 10.45.00)))
     ("Livre du jour - Collin et Paoli"
      01.00.00 (tuesday   10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 14.00.00)))
     ("Livre du jour - Cabanes"
      01.00.00 (wednesday 10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 18.00.00)))
     ("Livre du jour - Brassié"
      01.00.00 (thirsday  10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 19.00.00)))
     ("Livre du jour - Dehaye"
      01.00.00 (friday    10.45.00
                          14.00.00
                          (next day 06.00.00)
                          (next saturday 20.00.00)))
     ("Libre journal - Saboureau"
      01.30.00 (monday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal du Spectacle"
      01.30.00 (tuesday   12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal - Griotteray"
      01.30.00 (wednesday 12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal du Cinéma"
      01.30.00 (thirsday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Archives"
      01.30.00 (friday    12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal des Lycéens"
      01.30.00 (saturday  12.00.00
                          16.00.00
                          (next day 00.00.00)))
     ("Libre journal d'Henri de Lesquen"
      03.00.00 (monday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal des Contribuables"
      03.00.00 (tuesday   18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal de Serge de Beketch"
      03.00.00 (wednesday 18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal des Pioniers de la FM"
      03.00.00 (thirsday  18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre journal des Sciences"
      03.00.00 (friday    18.00.00
                          (next day 02.00.00)
                          (next day 07.30.00)))
     ("Libre Journal de la Nuit"
      02.00.00 (wednesday 21.30.00
                          (next friday 21.30.00)))
     ("Archives de Radio Courtoisie"
      03.00.00 (sunday  02.00.00))
     ("Chant Grégorien"
      01.00.00 (sunday 06.00.00
                       10.00.00
                       (next monday 05.00.00)))
     ("Saint du jour"
      01.00.00 (sunday 07.30.00
                       14.00.00))
     ("Scouts"
      01.30.00 (sunday 08.30.00
                       (next monday 09.00.00)))
     ("Images bibliques"
      01.00.00 (sunday 11.00.00))
     ("Lumière 101 - Dickès"
      01.30.00 (sunday 12.00.00
                       16.00.00
                       (next day 00.00.00)))
     ("Libre journal des Auditeurs et Musiciens"
      03.00.00 (sunday 18.00.00
                       02.00.00))))

  "
 *SCHEDULE* must be bound to a list.

 The first element is a list of options:
  ((:name         string)  ; name of the radio station.
   (:web-site-url string)  ; url of the web site of the station (http).
   (:stream-url   string)  ; url of the live stream (mms, or http redirector,
                           ; will be fed to mplayer).
   (:record-directory (or string pathname)) ; where to store the files.
   (:date         symbol)) ; a symbol in the YYYY-MM-DD format.
 The order of the options doesn't matter, and they don't need to be all present
 and other may be added.

 The other elements  are alternating schedules.
 BUG: we don't specify yet the period of each alternating schedule,
      it could be daily, weekly, monthly, yearly, etc.
 For now, each alternating schedule is a list whose first element is
 a symbol naming it, followed my a list of broadcast.

 Each broadcast is a list, whose first element is a string title of
 the broadcast, the second is a duration (a symbol in the HH.MM.SS
 format), and the third a list specifying the retransmissions of this
 broadcast.

 This retransmission list contains as first element the day (BUG: or
 month or ...), and the rest is a list of retransmission times. The
 first retransmission time is the original transmission time
 (usuallyin live, while the others are replays).  Each transmission
 time is either a symbol in the HH.MM.SS format, or a list specifying
 an transmission time in a relative day (BUG: month or ...).

 Such offset lists are of the form: (next day HH.MM.SS) or (next
 DAYNAME HH.MM.SS) where DAYNAME is a symbol naming a day of the week
 (BUG: month of the year, etc).
")

;;----------------------------------------------------------------------

(defclass broadcast ()
  ((title              :reader broadcast-title    :initarg :title
                       :type string)
   (duration           :reader broadcast-duration :initarg :duration)
   (first-transmission :reader broadcast-first-transmission
                       :initarg :first-transmission)
   (retransmissions    :reader broadcast-retransmissions
                       :initarg :retransmissions)))


(defmethod print-object ((self broadcast) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~S [~A] (~A~{ ~A~})"
            (broadcast-title self)
            (broadcast-duration self)
            (broadcast-first-transmission self)
            (broadcast-retransmissions self)))
  self)


;;----------------------------------------------------------------------

(defun same (list) (every (lambda (x) (equal x (first list))) (rest list)))
(defun set-equal-p (a b) (COM.INFORMATIMAGO.COMMON-LISP.GRAF:SET-EQUAL? a b))


(defparameter *schedule*
  (list
   (first *schedule*)
   (mapcar
    (lambda (broadcast)
      (assert (= 1 (length broadcast)))
      (let ((broadcast (first broadcast)))
        (destructuring-bind (name duration ((weeks day) &rest times)) broadcast
          (make-instance 'broadcast
           :title name
           :duration duration
           :first-transmission (if (and (listp weeks)
                                        (set-equal-p weeks
                                                     '(semaine-a semaine-b
                                                       semaine-c semaine-d)))
                                    (list 'every day)
                                    (list weeks  day))
           :retransmissions times))))
    (mapcar
     (lambda (class)
       (if (= 1 (length class))
           class
           (progn
             (assert (same (mapcar (function second) class)))
             (if (and (same (mapcar (lambda (x) (rest  (third x))) class))
                      (same (mapcar (lambda (x) (cadar (third x))) class)))
                 (destructuring-bind (name duration ((week day) &rest times))
                     (first class)
                   (list (list name duration
                               (list* (list (mapcar (lambda (x) (caar (third x)))
                                                    class) day) times))))
                 class))))
     (COM.INFORMATIMAGO.COMMON-LISP.LIST:EQUIVALENCE-CLASSES
      (mapcan
       (lambda (week)
         (destructuring-bind (week-name &rest broadcasts) week
           (mapcar
            (lambdA (broadcast)
              (destructuring-bind (name duration (day &rest times)) broadcast
                (list name duration (list* (list week-name day) times))))
            broadcasts)))
       (rest *schedule*))
      :test (function string=)
      :key (function first)))))
  "
 The *SCHEDULE* is reformated as a list containing a list of options in
 the first element:
  ((:name         string)  ; name of the radio station.
   (:web-site-url string)  ; url of the web site of the station (http).
   (:stream-url   string)  ; url of the live stream (mms, or http redirector,
                           ; will be fed to mplayer).
   (:record-directory (or string pathname)) ; where to store the files.
   (:date         symbol)) ; a symbol in the YYYY-MM-DD format.
 The order of the options doesn't matter, and they don't need to be all present
 and other may be added.

 The rest of the *SCHEDULE* is a list of BROADCAST instances.
")



;;----------------------------------------------------------------------
;;----------------------------------------------------------------------
;;----------------------------------------------------------------------



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||

CONCEPTS

- time relative to the planet (or the sun, etc) = double-float
- date in a calendar (eg. gregorian calendar)
- date in a cyclic calendar (eg. week)


+ mapping cyclic date to calendar date.
  ==> when the cycle belongs to the calendar.
  ==> beware of irregular cycles: months (28/29/30/31),
      leap years, leap seconds, etc.

+ mapping one calendar to another calendar dates.
  ==> depends on the time and place.

+ mapping a calendar date to a relative time.
  ==> careful with the DST (and leap seconds)
      ==> depends on the time and place too.


Computer clocks are aproximations of relative time.
Schedules are given usually in cyclic date.


See:  ~/src/public/lisp/common-lisp/date.lisp


cyclic calendars are cursors over other calendards (cyclic or other).


next/this/previous

next day
next week
next month
next quarter
next year
next century

next/previous <DAYNAME>


in a day/week/month/quarter/year/century
in <N> days/weeks/months/quarters/years/centuries


<N> day/week/month/quarter/year/century  ago
<N> days/weeks/months/quarters/years/centuries ago



||#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Classes:
;;   day-of-the-week    Monday to Sunday
;;   time-of-the-day    00:00 to 23:59
;;   date-in-the-week   {Monday to Sunday}x{00:00 to 23:59}
;;   duration
;;   time-offset


;;;-----------------------------------------------------------

(defclass day-of-the-week ()
  ((index :accessor index :initarg :index :type (integer 0 6))
   (name  :accessor name  :initarg :name  :type symbol)))


(defmethod initialize-instance :after ((self day-of-the-week)
                                       &key (date nil datep) &allow-other-keys)
  (when datep
    (setf (index self) (index (in-the-week date))))
  (setf (name self) (aref #(monday tuesday wednesday thirsday
                            friday saturday sunday) (index self)))
  self)


(defmethod print-object ((self day-of-the-week) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :identity nil :type t)
        (let ((*print-escape* nil))
          (print-object self stream)))
      (princ (name self) stream))
  self)


(defparameter monday    (make-instance 'day-of-the-week :index 0))
(defparameter tuesday   (make-instance 'day-of-the-week :index 1))
(defparameter wednesday (make-instance 'day-of-the-week :index 2))
(defparameter thirsday  (make-instance 'day-of-the-week :index 3))
(defparameter friday    (make-instance 'day-of-the-week :index 4))
(defparameter saturday  (make-instance 'day-of-the-week :index 5))
(defparameter sunday    (make-instance 'day-of-the-week :index 6))

(defparameter *days-of-the-week*
  (vector monday tuesday wednesday thirsday friday saturday sunday)
  "The days of the week in DECODE-UNIVERSAL-TIME order.")


(defun dow-from-index (index) (aref *days-of-the-week* index))

(defgeneric dow-to-index (self)
  (:method ((self day-of-the-week))  (index self))
  (:method ((self (eql 'monday)))    0)
  (:method ((self (eql 'tuesday)))   1)
  (:method ((self (eql 'wednesday))) 2)
  (:method ((self (eql 'thirsday)))  3)
  (:method ((self (eql 'friday)))    4)
  (:method ((self (eql 'saturday)))  5)
  (:method ((self (eql 'sunday)))    6))


;; (defclass periodic-sequence () ())
;;
;;
;; (defmacro define-periodic-sequence (name-and-options &rest items)
;;   (let ((name    (if (atom name-and-options)
;;                      name-and-options
;;                      (first name-and-options)))
;;         (options (if (atom name-and-options)
;;                      '()
;;                      (rest  name-and-options))))
;;     `(progn
;;        (defclass ,name (periodic-sequence)
;;          ((period :initform ,(length items)
;;                   :reader (symbol-concat name '-period)))
;;          ((items :initform ',items
;;           :reader (symbol-concat name '-
;;                                  (or (getf options :item-name) 'item) 's))))
;;
;;        )))
;;
;;
;; (define-periodic-sequence (week (:item-name day))
;;     monday tuesday wednesday thirsday friday saturday sunday)
;;
;;
;; (define-periodic-sequence (julian-year (:item-name month))
;;     january february march april may june
;;     july august september october november december)



;;;-----------------------------------------------------------

(defclass time-of-the-day ()
  ((hour    :accessor hour    :initarg :hour     :type (integer 0 23))
   (minute  :accessor minute  :initarg :minute   :type (integer 0 59))
   (seconde :accessor seconde :initarg :seconde  :type (integer 0 60))))


(defmethod initialize-instance :after ((self time-of-the-day)
                                       &key (time nil timep) (hour 0 hop)
                                       (minute 0 mip) (seconde 0 sep)
                                       &allow-other-keys)
  (when timep (setf (hour    self) (hour    time)
                    (minute  self) (minute  time)
                    (seconde self) (seconde time)))
  (when hop   (setf (hour    self) hour))
  (when mip   (setf (minute  self) minute))
  (when sep   (setf (seconde self) seconde))
  self)

(defparameter *midnight*
  (make-instance 'time-of-the-day :hour  0 :minute 0 :seconde 0))
(defparameter *noon*
  (make-instance 'time-of-the-day :hour 12 :minute 0 :seconde 0))


(defclass duration ()
  ((secondes :accessor secondes :initarg :secondes :type integer)))

(defmethod initialize-instance :after ((self duration)
                                       &key (hour 0 hop) (minute 0 mip)
                                       (seconde 0 sep)
                                       (year 1900 yep) (month 0 mop) (day 0 dap)
                                       &allow-other-keys)
  (when (or yep mop dap hop mip sep)
    (setf (secondes self)
          (encode-universal-time seconde minute hour (1+ day) (1+ month) year)))
  self)


(defmethod print-object ((self duration) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :type t :identity nil)
        (let ((*print-escape* nil))
          (print-object self stream)))
      (multiple-value-bind (se mi ho da mo ye)
          (decode-universal-time (secondes self))
        (format stream "~[~A secondes ~~;~]~
                      ~[~;~:*~A year~:*~P ~]~
                      ~[~;~:*~A month~:*~P ~]~
                      ~[~;~:*~A day~:*~P ~]~
                      ~2,'0D:~2,'0D:~2,'0D"
                (secondes self) ye (1- mo) (1- da) ho mi se)))
  self)


;;;-----------------------------------------------------------

(defclass date-in-the-week (day-of-the-week time-of-the-day)
  ())

(defmethod print-object ((self date-in-the-week) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :identity nil :type t)
        (let ((*print-escape* nil))
          (print-object self stream)))
      (format stream "~A ~2,'0D:~2,'0D:~2,'0D"
              (name self) (hour self) (minute self) (seconde self)))
  self)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BROADCAST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We convert the *schedule* into a list of broadcasts *time-table*.

(defstruct broadcast
  title
  original-transmission-date
  retransmission-index
  start-date
  duration)




(defun expand-days (days)
  (mapcar
   (function dow-from-index)
   (sort
    (delete-duplicates
     (loop
        :while days
        :nconc (if (eq '- (cadr days))
                   (prog1 (loop
                             :for d :from (dow-to-index (car days))
                             :to (dow-to-index (caddr days))
                             :collect d)
                     (setf days (nthcdr 3 days)))
                   (prog1 (list (dow-to-index (car days)))
                     (pop days)))))
    (function <=))))



(defun parse-duration (token)
  (apply (function make-instance) 'duration
         (loop
            :with tok = (substitute #\space #\. (string token))
            :with start = 0
            :for key :in '(:hour :minute :seconde)
            :collect key
            :collect (multiple-value-bind (value next)
                         (parse-integer tok :start start :junk-allowed t)
                       (setf start next)
                       start))))


(defun parse-time (token)
  (with-input-from-string (tok (substitute #\space #\/
                                           (substitute #\space #\.
                                                       (string token))))
    (let ((a (read tok)))
      (if (eq 'nd a)
          (let ((ho (read tok))
                (mi (read tok))
                (se (read tok)))
            (make-instance 'time-offset
                :label (format nil "Next day at ~2,'0D:~2,'0D:~2,'0D" ho mi se)
                :operator (lambda (date)
                            (add-time (next-day date)
                                      :hour ho :minute mi :seconde se))))
          (let ((ho a)
                (mi (read tok))
                (se (read tok)))
            (make-instance 'time-offset
                :label (format nil "Same day at ~2,'0D:~2,'0D:~2,'0D" ho mi se)
                :operator (lambda (date)
                            (add-time date
                                      :hour ho :minute mi :seconde se))))))))


(defun schedule-to-time-table (schedule)
  (sort
   (mapcan
    (lambda (line)
      (destructuring-bind (name days duration start-times) line
        (let ((duration    (parse-duration duration))
              (start-times (mapcar (function parse-time) start-times)))
          (mapcan
           (lambda (day)
             (let ((original (increment day (first start-times))))
               (mapcar
                (let ((i -1))
                  (lambda (start-time)
                    (make-broadcast
                     :title name
                     :original-transmission-date original
                     :retransmission-index (incf i)
                     :start-date  (increment day start-time)
                     :duration    duration)))
                start-times)))
           (expand-days days)))))
    schedule)
   (function time<=) :key (function broadcast-start-date)))


(defparameter *time-table* (schedule-to-time-table *schedule*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;-----------------------------------------------------------

(defclass day () ())
(defclass date (day time-in-day) ())

(defgeneric %add-hms (self hour minute seconde)
  (:method ((self day) hour minute seconde)
    (make-instance 'date
        :year (year self) :month (month self) :day (day self)
        :hour hour :minute minute :seconde seconde))
  (:method ((self day-of-the-week) hour minute seconde)
    (make-instance 'date-in-the-week
        :index (index self)
        :hour hour :minute minute :seconde seconde)))


(defgeneric %add-time (self time)
  (:method ((self day) (time time-of-the-day))
    (%add-hms self (hour time) (minute time) (seconde time)))
  (:method ((self day-of-the-week) (time time-of-the-day))
    (%add-hms self (hour time) (minute time) (seconde time))))


(defun add-time (self &rest args)
  (cond ((and (null (rest args)) (typep (first args) 'time-of-the-day))
         (%add-time self (first args)))
        ((and (evenp (length args))
              (loop
                 :for (k v) :on args :by (function cddr)
                 :always (member k '(:hour :minute :seconde :second))))
         (%add-hms self (getf args :hour 0)
                   (getf args :minute 0)
                   (or (getf args :seconde nil) (getf args :second 0))))
        (t (error "Invalid arguments to ADD-TIME"))))





(defgeneric universal-time (self)
  (:documentation "Express SELF as a universal-time."))

(defmethod universal-time ((self time-of-the-day))
    (encode-universal-time (seconde self) (minute self) (hour self)
                           1 1 1900))

(defmethod universal-time ((self day))
    (encode-universal-time 0 0 0 (day self) (month self) (year self)))

(defmethod universal-time ((self date))
    (encode-universal-time (seconde self) (minute self) (hour self)
                           (day self) (month self) (year self)))


(defgeneric secondes- (a b)
  (:method ((a day)  (b day))
    (- (universal-time a) (universal-time b)))
  (:method ((a day-of-the-week)  (b day-of-the-week))
    (- (universal-time a) (universal-time b))))

(defgeneric increment (when increment)
  (:method ((self day) (seconds integer))
    (make-instance 'date
        :xxx (+ (universal-time a) seconds)))
  (:method ((self day-of-the-week) (seconds integer))
    (make-instance 'date-in-the-week
        :xxx (+ (universal-time self) seconds))))


(defgeneric at-time (day time)
  (:method ((self day) (time time-of-the-day))
    (if (zerop (univeral-time time))
        self
        (make-instance 'date :date day :time time)))
  (:method ((self date) (time time-of-the-day))
    (if (zerop (univeral-time time))
        (make-instance 'date :date self)
        (make-instance 'date :date day :time time)))
  (:method ((self day-of-the-week) (time time-of-the-day))
    (if (zerop (univeral-time time))
        self
        (make-instance 'date-in-the-week :date day :time time)))
  (:method ((self day-of-the-week) (time time-of-the-day))
    (if (zerop (univeral-time time))
        (make-instance 'date-in-the-week :date self)
        (make-instance 'date-in-the-week :date day :time time))))



(defgeneric next-day (self)
  (:method ((self day-of-the-week))
    (dow-from-index (mod (1+ (dow-to-index self)) (length *days-of-the-week*))))
  (:method ((self day))
    (multiple-value-bind (se mi ho da mo ye)
        (decode-universal-time (+ #.(* 24 60 60) (universal-time self)))
      (make-instance 'day :year ye :month mo :day da)))
  (:method ((self date))
    (multiple-value-bind (se mi ho da mo ye)
        (decode-universal-time (+ #.(* 24 60 60) (universal-time self)))
      (make-instance 'date
          :year ye :month mo :day da
          :hour ho :minute mo :second se))))


(defgeneric monday-before (day)
  (:method ((self day))
    () (universal-time (in-the-week (at-time day *midnight*)))))
(defgeneric day-following (dow day)
  (:documentation "RETURN: the day (or date) matching dow, following day.")
  (:method ((self day-of-the-week) (day day))

    ))

(defgeneric in-the-week (self)
  (:method ((self day))
    (dow-from-index (nth-value 6 (decode-universal-time
                                  (universal-time self)))))
  (:method ((self date))
    (make-instance 'date-in-the-week
        :index (nth-value 6 (decode-universal-time
                             (universal-time self)))
        :hour (hour self) :minute (minute self) :seconde (seconde self)))
  (:method ((self date-in-the-week))
    self))



(defgeneric next-date (day dow)
  (:documentation "RETURN: The next date corresponding to DOW following DAY.")
  (:method ((day day) (dow date-in-the-week))
    (add-time) (secondes-between (in-the-week day) monday)
    ))




(defclass time-offset ()
  ((label    :accessor label    :initarg :label)
   (operator :accessor operator :initarg :operator)))

(defmethod print-object ((self time-offset) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :identity t :type t)
        (let ((*print-escape* nil))
          (print-object self stream)))
      (princ (label self) stream))
  self)


(defmethod increment (date (self time-offset))
  (funcall (operator self) date))



(defgeneric time<= (a b)
  (:method ((a day) (b day))
    (or (< (year a) (year b))
        (and (= (year a) (year b))
             (or (< (month a) (month b))
                 (and (= (month a) (month b))
                      (<= (day a) (day b)))))))
  (:method ((a date) (b date))
    (or (< (year a) (year b))
        (and (= (year a) (year b))
             (or (< (month a) (month b))
                 (and (= (month a) (month b))
                      (or (< (day a) (day b))
                          (and (= (day a) (day b))
                               (or (< (hour a) (hour b))
                                   (and (= (hour a) (hour b))
                                        (or (< (minute a) (minute b))
                                            (and (= (minute a) (minute b))
                                                 (<= (seconde a)
                                                     (seconde b)))))))))))))
  (:method ((a day-of-the-week) (b day-of-the-week))
    (<= (index a) (index b)))
  (:method ((a date-in-the-week) (b date-in-the-week))
    (or (< (index a) (index b))
        (and (= (index a) (index b))
             (or (< (hour a) (hour b))
                 (and (= (hour a) (hour b))
                      (or (< (minute a) (minute b))
                          (and (= (minute a) (minute b))
                               (<= (seconde a)
                                   (seconde b)))))))))
  (:method (a b)
    (error "Cannot time compare ~A with ~A"
           (class-name (class-of a))
           (class-name (class-of b)))))


(defgeneric time>  (a b) (:method (a b) (not (time<= a b))))
(defgeneric time=  (a b) (:method (a b) (and (time<= a b) (time<= b a))))
(defgeneric time/= (a b) (:method (a b) (not (time= a b))))
(defgeneric time<  (a b) (:method (a b) (and (time<= a b) (time/= a b))))
(defgeneric time>= (a b) (:method (a b) (not (time< b a))))



(defun find-next-broadcast (now)
  (loop
     :with btime = (in-the-week now)
     :for broadcast :in *time-table*
     :while (time< (broadcast-start-date broadcast) btime)
     :finally (return (if (time<= btime (broadcast-start-date broadcast))
                           broadcast
                           (first *time-table*)))))


(defun record (url duration filepath)
  "
DO:     Records from URL for DURATION into FILEPATH.
RETURN: The duration actually recorded ; the status returned by mplayer.
"
  (let ((cr (code-char 13))
        (start (get-universal-time))
        (end (+ duration (get-universal-time)))
        (msg (namestring filepath)))
    (format t "Record  ~4D seconds of broadcast into ~A~%"
            duration (namestring filepath))
    (with-open-stream (mplayer
                       (ext:run-shell-command
                        (format nil
                                "mplayer -quiet ~
                               -vc null -vo null -ao pcm:fast ~
                               -ao pcm:file=~S ~S ~
                               >/dev/null 2>&1 ; echo $?"
                                (namestring filepath)
                                url)
                        :input :stream :output :stream :wait nil))
      (loop
         :do (format t "~CRemains ~4D seconds to record..."
                     cr (- end (get-universal-time)))
         :do (finish-output)
         :do (sleep 1)
         :while (and (not (listen mplayer)) (<= (get-universal-time) end))
         :finally (terpri))
      (if (listen mplayer)
          (let ((status (read mplayer))
                (end    (get-universal-time)))
            (format t "Recording aborted with status ~D after only ~D seconds~%"
                    status (- end start))
            (values (- end start) status))
          (progn
            (format mplayer "q~%")
            (finish-output mplayer)
            (let ((status (read mplayer))
                  (end    (get-universal-time)))
              (format t "Completed successfully recording ~D seconds~%"
                      (- end start))
              (values (- end start) status)))))))


(defun now ()
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time (get-universal-time)
                             #|by default, local time zone with DST|#)
    (make-instance 'date
        :seconde se :minute mi :hour ho :day da :month mo :year ye)))

(defun wait (seconds &rest args)
  (let ((cr (code-char 13))
        (msg (apply (function format) nil args))
        (end (+ seconds (get-universal-time))))
    (format t "~%")
    (loop
       :for now = (now)
       :do (format t "~CNow is ~A  ~
                        Waiting ~4D seconds ~:[~;~:*for ~A~]"
                   cr now (- end (get-universal-time)) msg)
       :do (finish-output)
       :do (sleep 1)
       :while (< (get-universal-time) end)
       :finally (terpri))))


(defun compress (filepath)
  (ext:run-shell-command
   (format nil "lame -S -V 7 ~S ~S"
           (namestring filepath)
           (namestring (make-pathname :type "MP3" :case :common
                                      :defaults filepath)))
   :input nil :output nil :wait nil))


(defun approximatively= (d1 d2)
  (< (abs (/ (- d1 d2) (+ d1 d2))) 0.01))


(defun main ()
  (loop
     (let* ((now (now))
            (broadcast   (find-next-broadcast now))
            (start-time  (this-week now (broadcast-start-date broadcast))))
       (wait (max 0 (- (mod (- (universal-time start-time)
                               (encode-universal-time
                                (date-second now)
                                (date-minute now)
                                (date-hour now)
                                (date-day-of-week now) 1 1900))
                            (* 7 24 60 60))
                       30))
             "next broadcast: ~A ~2,'0D:~2,'0D:~2,'0D ~A" dn h m s n)
       (let ((now (now))
             (file
              (make-pathname
               :name (format nil
                             "~4,'0,D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D-~A-~A-~A"
                             (date-year now) (date-month now) (date-day now)
                             oh om os n dn seq)
               :type "WAV" :case :common :defaults *record-directory*))
             (duration (+ ds (* 60 (+ dm (* 60 dh))))))
         (multiple-value-bind (actual-duration status)
             (record *url* duration file)
           (if (and (zerop status)
                    (approximatively= actual-duration duration))
               (progn
                 (format t "Successfully recorded broadcast ~S.~%"
                         (file-namestring file))
                 (compress file))
               (progn
                 (format t "Failed to record broadcast ~S.~%"
                         (file-namestring file))
                 (ignore-errors (delete-file file)))))))))


;; Testing for a TESTING-SCRIPT feature is more portable.
#-(and)
(when (EQ (GET-DISPATCH-MACRO-CHARACTER #\# #\!) #'SYS::UNIX-EXECUTABLE-READER)
  (main))

#-testing-script
(main)

;;;; THE END ;;;;
