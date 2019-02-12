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
(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "1.0.2")


;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun function-named (sname pname)
  (let ((pack (find-package pname)))
    (and pack
         (let ((sym (find-symbol sname pack)))
           (and sym
                (fboundp sym)
                sym)))))

(defun getuid ()
  (funcall (or (function-named "UID" "POSIX")
               (function-named "getuid" "LINUX")
               (function-named "GETUID" "LINUX")
               (error "Cannot get the UID."))))

(defparameter *sockets*
  (let ((uid (getuid)))
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
    :for key = (read stream nil nil)
    :for value = (when key (read-line stream))
    :while key
    :collect (cons key (emacsclient-22/process-value value))))


(defparameter *emacsen*
  (let ((emacsen '()))
    (dolist (socket *sockets* (reverse emacsen))
      (let ((frames
             (with-open-stream (frames (ext:run-program
                                        "emacsclient"
                                        :arguments (list (format nil "--socket-name=~A" socket)
                                                         "--eval"
                                                         "(mapcar (lambda (f) (list (frame-name f) (frame-display f))) (frame-list))")
                                        :output :stream))
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
                     (read frames nil nil))))))
        (if frames
            (push (list socket frames) emacsen)
            (multiple-value-bind (all pid) (regexp:match "^.*server-\\([0-9]\\+\\)$" socket)
              (if all
                  (let ((pid (regexp:match-string socket  pid)))
                    (with-open-stream (ps (ext:run-program "ps" :arguments (list "-p" pid) :output :stream))
                      (unless (loop
                                 :named search-emacs
                                 :for line = (read-line ps nil nil)
                                 :while line
                                 :if (regexp:match "emacs" line)
                                 :do (return-from search-emacs t)
                                 :finally  (return-from search-emacs nil))
                        (delete-file socket)
                        (setf *sockets* (delete socket *sockets*))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-option ("list" "-l" "--list") ()
  "List the available emacs servers."
  (handler-case
   (loop
     :for i :from 1
     :for (server frames) :in *emacsen*
     :do (format t "~2D) ~30A ~:{~1@*~16A ~0@*~S~:^~%~35T~}~%"
                 i server frames))
    (error (err) (invoke-debugger err))))


(define-option ("select" "-s" "--select") (index)
  "Select the server at the given index (from 1 up) as the default server."
  (let* ((index (parse-integer index))
         (uid    (getuid))
         (server (ignore-errors (nth (1- index) *emacsen*))))
    (if server
        (ext:run-program "ln" :arguments (list "-sf"
                                               (first server)
                                               (format nil "/tmp/emacs~A/server" uid)))
        (error "~A is not a server index. Please give an index between 1 and ~A"
               index (length *emacsen*)))))



(defun xor (a b) (or (and a (not b)) (and (not a) b)))

(defun make-frame (socket-name &key on-display on-terminal)
  (assert (xor on-display on-terminal))
  (ext:run-program "emacsclient"
                   :arguments  (cond
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
                                   "--tty")))))


(define-option ("open" "-o" "--open") (index)
  "Make a new frame from the server at the given index (from 1 up) on the current DISPLAY."
  (let* ((index   (parse-integer index))
         (uid     (getuid))
         (server  (ignore-errors (nth (1- index) *emacsen*)))
         (display (ext:getenv "DISPLAY")))
    (cond
      ((null server)
       (error "~A is not a server index. Please give an index between 1 and ~A"
              index (length *emacsen*)))
      ((null display)
       (error "There is no DISPLAY environment variable."))
      (t
       (make-frame (first server) :on-display display)))))


(define-option ("terminal" "-t" "--open-on-terminal") (index)
  "Make a new frame from the server at the given index (from 1 up) in the terminal."
  (let* ((index (parse-integer index))
         (uid    (getuid))
         (server (ignore-errors (nth (1- index) *emacsen*))))
    (cond
      ((null server)
       (error "~A is not a server index. Please give an index between 1 and ~A"
              index (length *emacsen*)))
      (t
       (make-frame (first server) :on-terminal t)))))



(ext:exit
 (if (null *emacsen*)
     (progn
       (format t "There is no emacs server~%")
       EX-UNAVAILABLE)
     (parse-options ext:*args*
                    (lambda ()
                      (call-option-function "help" '())
                      EX-NOINPUT))))

;;;; THE END ;;;;



