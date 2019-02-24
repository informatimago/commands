;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mfod.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Shows all the emacs servers available, and let the user select one
;;;;    on which to open a new frame.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-08-30 <PJB> Translated from bash...
;;;;    2009-09-12 <PJB> Created as a bash script.
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


(defun regexp-compile (regexp)
  (cl-ppcre:create-scanner regexp :extended-mode t))

(defun regexp-exec (regexp string &key (start 0) (end nil))
  (let* ((results (multiple-value-list (cl-ppcre:scan regexp string
                                                      :start start
                                                      :end (or end (length string))))))
    (if (equal '(nil) results)
        nil
        (destructuring-bind (as ae ss es) results
          (declare (ignore as ae))
          (map 'list (function list) ss es)))))

(defun match (regexp string) (regexp-exec regexp string))
(defun match-start (range) (first  range))
(defun match-end   (range) (second range))

(defun match-string (string range)
  (subseq string (first range) (second range)))

(eval-when (:compile-toplevel :load-toplevel :execute) (shadow 'run-program))
(defun run-program (command &rest arguments &key &allow-other-keys)
  (if (listp command)
      (apply (function ccl::run-program) (first command) (rest command) arguments)
      (let ((command (split-sequence #\space command)))
        (apply (function ccl::run-program) (first command) (rest command) arguments))))
(defun external-process-output-stream (ep)
  (ccl:external-process-output-stream ep))

(defparameter *program-version* "1.0.2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun function-named (sname pname)
  (let ((pack (find-package pname)))
    (and pack
         (let ((sym (find-symbol sname pack)))
           (and sym
                (fboundp sym)
                sym)))))

(defvar *sockets* nil)

(defun emacsclient-22/process-value (value)
  "
&_ --> #\space
&n --> #\newline
&x --> #\x
and read-from-string
"
  (read-from-string
   (with-output-to-string (out)
     (loop
       :with i = 0
       :while (< i (length value))
       :do (let ((ch (aref value i)))
             (if (char= #\& ch)
                 (progn
                   (incf i)
                   (let ((ch (aref value i)))
                     (case ch
                       ((#\_) (princ #\space out))
                       ((#\n) (princ #\newline out))
                       (otherwise (princ ch out)))))
                 (princ ch out))
             (incf i))))))


(defun emacsclient-22/read-output (stream)
  (loop
    :for line := (read-line stream nil)
    :for items := (split-sequence #\space line)
    :for key := (first items)
    :for value := (rest items)
    :while line
    :collect (cons key (emacsclient-22/process-value (format nil "~{~A~^ ~}" value)))))

(defun emacsen ()
  (let ((emacsen '()))
    (dolist (socket *sockets* (reverse emacsen))
      (let ((frames
              (with-open-stream (frames (external-process-output-stream
                                         (run-program
                                          (list "emacsclient"
                                                (format nil "--socket-name=~A" socket)
                                                "--eval"
                                                "(mapcar (lambda (f) (list (frame-parameter f 'name) (frame-parameter f 'display))) (frame-list))")
                                          :input nil :output :stream :wait nil)))
                ;; Output from /Applications/Emacs.app/Contents/MacOS/bin/emacsclient 24.2 on MacOSX:
                ;; ((#1="EMACS" #2="iMac-Core-i5.local")
                ;;  (#1# #2#))
                ;;
                ;; Output from emacsclient 23.2 on linux:
                ;; ((" emacs at voyager.informatimago.com" nil))
                ;;
                ;; Output from /usr/bin/emacsclient 22.1 on MacOSX:
                ;; -emacs-pid 30294
                ;; -print ((#1="EMACS"&_#2="iMac&-Core&-i5.local")&n&_(#1#&_#2#))&n
                (let ((ch (peek-char nil frames)))
                  (if (char= #\- ch)
                      (cdr (assoc '-print (emacsclient-22/read-output frames)))
                      (read frames nil))))))
        (if frames
            (push (list socket frames) emacsen)
            (multiple-value-bind (all pid) (match "^.*server-([0-9]+)$" socket)
              (if all
                  (let ((pid (match-string socket  pid)))
                    (with-open-stream (ps (external-process-output-stream
                                           (run-program (list "ps" "-p" pid) :input nil :output :stream :wait nil)))
                      (unless (loop
                                :named search-emacs
                                :for line = (read-line ps nil nil)
                                :while line
                                :if (match "emacs" line)
                                  :do (return-from search-emacs t)
                                :finally  (return-from search-emacs nil))
                        (delete-file socket)
                        (setf *sockets* (delete socket *sockets*))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *emacsen*)

(command :use-systems (:cl-ppcre :split-sequence)
         :use-packages ("COMMON-LISP" "SCRIPT" "SPLIT-SEQUENCE")
         :options (list* (option ("list" "-l" "--list") ()
                                 "List the available emacs servers."
                                 (handler-case
                                     (loop
                                       :for i :from 1
                                       :for (server frames) :in *emacsen*
                                       :do (format t "~2D) ~30A ~:{~1@*~16A ~0@*~S~:^~%~35T~}~%"
                                                   i server frames))
                                   (error (err) (invoke-debugger err))))

                         (option ("select" "-s" "--select") (index)
                                 "Select the server at the given index (from 1 up) as the default server."
                                 (let* ((index (parse-integer index))
                                        (uid    (getuid))
                                        (server (ignore-errors (nth (1- index) *emacsen*))))
                                   (if server
                                       (uiop:run-program (list "ln" "-sf"
                                                               (first server)
                                                               (format nil "/tmp/emacs~A/server" uid)))
                                       (error "~A is not a server index. Please give an index between 1 and ~A"
                                              index (length *emacsen*)))))

                         (option ("open" "-o" "--open") (index)
                                 "Make a new frame from the server at the given index (from 1 up) on the current DISPLAY."
                                 (let* ((index   (parse-integer index))
                                        (server  (ignore-errors (nth (1- index) *emacsen*)))
                                        (display (uiop:getenv "DISPLAY")))
                                   (cond
                                     ((null server)
                                      (error "~A is not a server index. Please give an index between 1 and ~A"
                                             index (length *emacsen*)))
                                     ((null display)
                                      (error "There is no DISPLAY environment variable."))
                                     (t
                                      (make-frame (first server) :on-display display)))))

                         (option ("terminal" "-t" "--open-on-terminal") (index)
                                 "Make a new frame from the server at the given index (from 1 up) in the terminal."
                                 (let* ((index (parse-integer index))
                                        (server (ignore-errors (nth (1- index) *emacsen*))))
                                   (cond
                                     ((null server)
                                      (error "~A is not a server index. Please give an index between 1 and ~A"
                                             index (length *emacsen*)))
                                     (t
                                      (make-frame (first server) :on-terminal t)))))

                         (help-option)
                         (bash-completion-options)))

(defun xor (a b) (or (and a (not b)) (and (not a) b)))

(defun make-frame (socket-name &key on-display on-terminal)
  (assert (xor on-display on-terminal))
  (uiop:run-program
   (cons "emacsclient" (cond
                         (on-display
                          (list
                           (format nil "--socket-name=~A" socket-name)
                           "--no-wait"
                           ;; "--eval" (format nil "(make-frame-on-display \"~A\")"
                           ;;                  )
                           "--create-frame"
                           "--display" on-display))
                         (on-terminal
                          (list
                           (format nil "--socket-name=~A" socket-name)
                           "--tty"))))))

(defun main (arguments)
  (setf *sockets* (let ((uid (getuid)))
                    (sort
                     (mapcar (function namestring)
                             (remove-if-not (function probe-file)
                                            (remove-duplicates
                             ;; getting server is not so useful since directory will
                             ;; return the truename...
                                             (append (directory (format nil "/tmp/emacs~A/server"   uid))
                                                     (directory (format nil "/tmp/emacs~A/server-*" uid)))
                                             :test (function equalp))))
                     (function string-lessp))))
  (setf *emacsen* (emacsen))
  (if (null *emacsen*)
      (progn
        (format t "There is no emacs server~%")
        ex-unavailable)
      (parse-options *command* arguments
                     (lambda ()
                       (call-option-function *command* "help" '())
                       ex-noinput))))

;;;; THE END ;;;;
