;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               radio
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Script to listen to various radio stations.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-14 <PJB> Translated from bash.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
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

(defparameter *program-version* "0.1.2")

;;;---------------------------------------------------------------------

(defparameter *radio-urls*
  ;; "http://www.radiocourtoisie.fr/courtoisie.m3u"
  '(("courtoisie"     "http://direct.radiocourtoisie.fr")
    ("esradio"        "mms://streaming.esradio.fm/EsRadio")
    ("reinfo"         "http://www.radiocourtoisie.net/tempo/public/reinformation/reinformation.mp3")
    ("treffpunkt"     "http://www.cibersoft.com.ar/radio/radio.m3u")
    ("esperance"      "http://www.streamakaci.com/radios/esperance.m3u")))

;;;------------------------------------------------------------------------

(defvar *verbose* nil
  "Whether the underlying commands run should be written to stdout.")

(defun print-command (command)
  (when *verbose*
   (format t "~A~%" command)) (finish-output)
  command)

(define-condition command-exit-status (error)
  ((command :initform "" :initarg :command :accessor command-exit-status-command)
   (status  :initform 0  :initarg :status  :accessor command-exit-status-status))
  (:report  (lambda (condition stream)
              (format stream "Command ~S exited with status ~D"
                      (command-exit-status-command condition)
                      (command-exit-status-status  condition)))))

(define-condition command-aborted-on-signal (error)
  ((command :initform "" :initarg :command :accessor command-aborted-on-signal-command)
   (signal  :initform 0  :initarg :signal  :accessor command-aborted-on-signal-signal))
  (:report  (lambda (condition stream)
              (format stream "Command ~S was killed by signal ~D"
                      (command-aborted-on-signal-command condition)
                      (command-aborted-on-signal-signal  condition)))))

(defvar *run-output* :terminal)

(defun run (control-string &rest arguments)
  "Runs the specified shell commands.
Signals an error if they exit with an error status or are killed by a signal."
  (let* ((command (print-command (format nil "{ ~? ; } 2>&1" control-string arguments)))
         (status  (uiop:run-program (list "/bin/bash" "-c" command)
                                    :force-shell t
                                    :output *run-output*
                                    :if-output-exists :append
                                    :wait t)

          ;; (let ((pid (linux:|fork|)))
          ;;   (case pid
          ;;     ((-1) (linux:perror "fork") EX-UNAVAILABLE)
          ;;     ((0) (unwind-protect
          ;;               (nth-value 1 (linux:|waitpid| pid 0))
          ;;            (linux:|kill| pid)))
          ;;     (otherwise
          ;;      (ext:run-program "/bin/bash"
          ;;                       :arguments (list "-c" command)
          ;;                       :output *run-output*
          ;;                       :if-output-exists :append
          ;;                       :wait t)
          ;;      (exit ex-ok))))

          ))
    (cond
      ((null   status)
       #|normal exit|#)
      ((not (integerp status))
       (error "Command ~S exited with strange status ~S" command status))
      ((zerop  status)
       #|normal exit|#)
      ((plusp  status)
       (error 'command-exit-status :command command :status status))
      (t
       (error 'command-aborted-on-signal :command command :signal (abs status))))))

;;;---------------------------------------------------------------------

(defun s (command &rest arguments)
  (let ((arguments  (mapcar (lambda (arg) (script:shell-quote-argument (princ-to-string arg)))
                            arguments)))
   (apply (function run) (format nil "~A~~@{ ~~A~~}" command) arguments)))

(defun e (&rest args)
  (with-output-to-string (out)
    (with-open-stream (in (uiop:run-program (format nil "~{~A ~}" args)
                                            :force-shell t
                                            :output :stream :wait nil))
      (loop
        :for line = (read-line in nil nil)
        :while line :do (write-line line out)))))

;;;------------------------------------------------------------

(defun compose-sexp (functions var)
  (if (null functions)
      var
      (list (car functions) (compose-sexp (cdr functions) var))))

(defmacro COMPOSE (&rest functions)
  `(lambda (x) ,(compose-sexp functions 'x)))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun prefixp (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun suffixp (suffix string)
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

;;;------------------------------------------------------------

(defvar *do-record*             nil)
(defvar *stations-to-listen-to* '())

(defun get-radio-station (name)
  (assoc  name *radio-urls* :test (function string-equal)))

(defun get-radio-station-names ()
   (mapcar (function first) *radio-urls*))


(defvar *radio-list-format* "~:{~20A ~S~%~}")

(define-option ("version" "-V" "--version") ()
  "Report the version of this script."
  (format t "~A ~A~%" *program-name* *program-version*))

(define-option ("verbose" "-v" "--verbose") ()
  "Report writes the underlying commands that are run."
  (setf *verbose* t))

(define-option ("url" "-U" "-url" "--url") (radio-station)
  "Prints the URL of the radio station and exit."
  (let ((radio (get-radio-station radio-station)))
    (if radio
        (format t "~A" (second radio))
        (error "There is no radio station named ~S" radio-station)))
  (exit ex-ok))

(define-option ("record" "-r" "--record") ()
  "Records the stream while listening."
  (setf *do-record* t))

(define-option ("list-stations" "list"  "-l" "-ls" "--list" "--list-stations") ()
  "Prints the list of radio stations and exit."
  (format t *radio-list-format* *radio-urls*)
  (exit ex-ok))

(defun completion-station-prefix (prefix)
  (format t "~(~{~A~%~}~)"  (remove-if-not (lambda (key)
                                             (and (<= (length prefix) (length key))
                                                  (string= prefix key :end2 (length prefix))))
                                           (get-radio-station-names)))
  (finish-output))

(defun completion-all-stations ()
  (format t "~(~{~A~%~}~)" (get-radio-station-names))
  (finish-output))

(defun main (args)
  (setf *debug* nil)
  (setf *bash-completion-hook*
        (lambda (index words)
          (if index
              (completion-station-prefix (elt words index))
              (completion-all-stations))
          nil))
  (parse-options args
                 (lambda () (call-option-function "help" '()))
                 (lambda (name arguments)
                     (let* ((radio (get-radio-station name)))
                       (if radio
                         (progn
                           (setf *stations-to-listen-to* (append *stations-to-listen-to* (list radio)))
                           arguments)
                         (error "Unknown radio station: ~S" name)))))
  (let ((base-options '( "-nojoystick" "-ontop" "-quiet")))
    (loop
      :for (name url) :in  *stations-to-listen-to*
      :for options = (when *do-record*
                       (let ((date  (multiple-value-bind (se mi ho da mo ye)
                                        (decode-universal-time (get-universal-time))
                                      (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D"
                                              ye mo da ho mi se))))
                         (setf options (list "-ao" (format nil "pcm:file=/tmp/~A-~A.wav" name date)))))
      :do (apply (function s) "mplayer" (append base-options options (list url)))))
  ex-ok)


;;;; THE END ;;;;
