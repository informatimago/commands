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

(command :use-systems (:com.informatimago.common-lisp)
         :use-packages ("COMMON-LISP" "SCRIPT"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ACTIVITY"))


(defparameter *from-email* "surveille-host <pjb@informatimago.com>")

(defparameter *remotes* (make-hash-table :test (function equal))
  "A map: ip-address --> remotes.")

(defun reset-remotes ()
  "
POST:   There is no remote defintions in *REMOTES*.
"
  (setf *remotes* (make-hash-table :test (function equal))))

(defclass remote ()
  ((name
    :accessor name
    :initarg :name
    :initform "Unnamed"
    :type     string
    :documentation "The name of the remote.")
   (ip-address
    :accessor ip-address
    :initarg :ip-address
    :initform "127.0.0.1"
    :type     string
    :documentation "The IP address of the remote.")
   (email
    :accessor email
    :initarg :email
    :initform nil
    :type     (or null string)
    :documentation "The email address to be signaled when state change.")
   (check-period
    :accessor check-period
    :initarg :check-period
    :initform 120
    :type     number
    :documentation "The period in second of checking.")
   (state
    :accessor state
    :initarg :state
    :initform :unknown
    :documentation
    "Whether the remote state is :unknown, :off-line or :on-line.")
   (activity
    :accessor activity
    :initform nil
    :documentation "The activity running for this remote."))
  (:documentation "A remote host definition."))

(defparameter *do-logging* nil)

(defmacro logging (action &body body)
  `(progn
     (when *do-logging*
       (format *trace-output* "~2%BEGINNING ~A~%" (string ,action)))
     (prog1 (progn ,@body)
       (when *do-logging*
         (format *trace-output* "~%COMPLETED ~A~%" (string ,action))))))

(defmethod get-current-state ((self remote))
  "
RETURN: Whether :ON-LINE or :OFF-LINE according to the status of ping.
"
  (logging (format nil "pinging ~A" (name self))
           (if (= 0 (uiop:run-program (format nil "ping -c 10 -w 15 -n -q ~S" (ip-address self))
                                      :input nil :output nil))
               :on-line
               :off-line)))

(defmethod notificate-state-change ((self remote))
  (when (email self)
    (logging (format nil "sendmail ~A" (email self))
             (let ((msg-stream   (uiop:run-program (format nil "sendmail ~A" (email self))
                                                   :force-shell t :input :stream :output nil)))
               (format msg-stream
                       (concatenate 'string
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
                       *from-email*
                       (email self)
                       (name self)
                       (if (eq :on-line (state self)) "on-line" "off-line")
                       (name self) (ip-address self)
                       (if (eq :on-line (state self)) "on-line" "off-line"))
               (close msg-stream)))))

(defmethod check ((self remote))
  (let ((new-state (get-current-state self)))
    (unless (eq new-state (state self))
      (if (eq :unknown (state self))
        (progn
          (setf (state self) new-state)
          (notificate-state-change self))
        (setf (state self) new-state)))))

(defun find-remote-with-ip-address (ip-address)
  "
RETURN: The remote instance that has the given IP-ADDRESS, or NIL.
"
   (gethash ip-address *remotes*))

(defun add-remote (name ip-address email)
  "
DO:    If there is already a remote with the same IP-ADDRESS
       then raise an error
       else create such a new remote.
"
  (if (find-remote-with-ip-address ip-address)
    (error "There is already a remote with the same IP address ~S." ip-address)
    (let ((remote (make-instance 'remote
                         :name name
                         :ip-address ip-address
                         :email email)))
      (setf (gethash ip-address *remotes*) remote)
      (setf (activity remote) (make-instance 'activity
                               :name (name remote)
                               :period (check-period remote)
                               :active t
                               :closure (lambda () (check remote))))
      (schedule (activity remote)))))

(defun main (argv)
  (declare (ignore argv))

  (add-remote "hubble"   "hubble.informatimago.com"   "pjb@informatimago.com")
  (add-remote "proteus"  "proteus.informatimago.com"  "pjb@informatimago.com")
  (add-remote "kuiper"   "kuiper.informatimago.com"   "pjb@informatimago.com")
  (add-remote "despina"  "despina.informatimago.com"  "pjb@informatimago.com")
  (add-remote "larissa"  "larissa.informatimago.com"  "pjb@informatimago.com")

  (let ((period (* 30 (hash-table-size *remotes*))))
    (maphash (lambda (key remote)
               (declare (ignore key))
               (setf (check-period remote) period)
               (setf (period (activity remote)) period)) *remotes*))
  (activity-run :debug nil)
  ex-ok)

;;;; THE END ;;;;
