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
;;;;    2021-06-01 <PJB> Improved slurping of stream, added verbose option.
;;;;    2010-08-30 <PJB> Translated from bash...
;;;;    2009-09-12 <PJB> Created as a bash script.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2021
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

(in-package "SCRIPT")

(command :use-systems (:cl-ppcre :split-sequence)
         :use-packages ("COMMON-LISP" "SCRIPT" "SPLIT-SEQUENCE")
         :shadow ("RUN-PROGRAM")
         :version "1.0.2")

(defun run-program (command &rest arguments &key &allow-other-keys)
  (apply (function uiop:run-program) command arguments))

(defun slurp-stream (stream)
  "Read text from the output and error streams until both EOF or external-process exits."
  (uiop/stream:slurp-stream-string stream))

(defun regexp-compile (regexp)
  (cl-ppcre:create-scanner regexp :extended-mode t))

(defun regexp-exec (regexp string &key (start 0) (end nil))
  (let* ((results (multiple-value-list (cl-ppcre:scan regexp string
                                                      :start start
                                                      :end (or end (length string))))))
    (if (equal '(nil) results)
        nil
        (destructuring-bind (as ae ss es) results
          (values-list (cons (list as ae) (map 'list (function list) ss es)))))))

(defun match (regexp string) (regexp-exec regexp string))
(defun match-start (range) (first  range))
(defun match-end   (range) (second range))

(defun match-string (string range)
  (subseq string (first range) (second range)))

(defvar *verbose* nil)

(defun print-if-verbose (object)
  (when *verbose*
    (prin1 object)
    (terpri))
  object)

(defun verbose-option-p (arguments)
  (member-if (lambda (argument)
               (member argument '("verbose" "-v" "--verbose") :test (function string=)))
             arguments))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun function-named (sname pname)
  (let ((pack (find-package pname)))
    (and pack
         (let ((sym (find-symbol sname pack)))
           (and sym
                (fboundp sym)
                sym)))))

(defvar *sockets* nil)

(defun getenv-non-empty (name)
  (let ((value (uiop:getenv name)))
    (and value
         (< 0 (length value))
         value)))

(defun pathname-directory-pathname (pathname)
  (make-pathname :name nil :type nil :version nil :defaults pathname))

(defun socket-pathnames-from-patterns (&rest patterns)
  (remove-if-not (function probe-file)
                 (mapcan (lambda (pattern)
                           (ignore-errors (directory pattern)))
                         patterns)))

(defun emacs-socket-candidates ()
  (let* ((uid (getuid))
         (explicit-server (getenv-non-empty "EMACS_SERVER_FILE"))
         (xdg-runtime-dir (getenv-non-empty "XDG_RUNTIME_DIR"))
         (tmpdir          (or (getenv-non-empty "TMPDIR") "/tmp/"))
         (patterns        '()))
    (when explicit-server
      (let* ((server (pathname explicit-server))
             (server-name (file-namestring server))
             (server-dir  (pathname-directory-pathname server)))
        (push server patterns)
        (push (merge-pathnames (make-pathname :name :wild :type nil :version nil
                                              :defaults (merge-pathnames
                                                         (make-pathname :name server-name :type nil :version nil)
                                                         server-dir))
                               server-dir)
              patterns)))
    (when xdg-runtime-dir
      (setf patterns (nconc (list (format nil "~A/emacs/server" xdg-runtime-dir)
                                  (format nil "~A/emacs/server-*" xdg-runtime-dir))
                            patterns)))
    (setf patterns (nconc (list (format nil "~A/emacs*/server" tmpdir)
                                (format nil "~A/emacs*/server-*" tmpdir)
                                (format nil "/tmp/emacs~A/server" uid)
                                (format nil "/tmp/emacs~A/server-*" uid))
                          patterns))
    (sort (mapcar (function namestring)
                  (remove-duplicates (apply (function socket-pathnames-from-patterns) patterns)
                                     :test (function equalp)))
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
    :for line := (read-line stream nil)
    :for items := (split-sequence #\space line)
    :for key := (first items)
    :for value := (rest items)
    :while line
    :collect (cons key (emacsclient-22/process-value (format nil "~{~A~^ ~}" value)))))

(defun emacsen ()
  (let ((emacsen '()))
    (dolist (socket *sockets* (reverse emacsen))
      (multiple-value-bind (output error status)
          (run-program
           (print-if-verbose
            (list "emacsclient"
                  (format nil "--socket-name=~A" socket)
                  "--eval"
                  "(mapcar (lambda (f) (list (frame-parameter f 'name) (frame-parameter f 'display))) (frame-list))"))
           :input nil :output :string :error nil :ignore-error-status t :wait nil)
        (declare (ignore error))
        (let ((frames (if (eql 0 status)
                          (with-input-from-string (frames output)
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
                                  (handler-case
                                      (read frames nil)
                                    (error (err)
                                      (format *error-output* "~&While reading: ~S~%~A~%" output err)
                                      (finish-output *error-output*)
                                      nil)))))
                          (progn
                            (format t "~&~:@{emacsclient with status ~A~%~}" status)
                            (write-string output *error-output*)
                            nil))))
          (if frames
              (push (list socket frames) emacsen)
              (multiple-value-bind (all pid) (match "^.*server-([0-9]+)$" socket)
                (if all
                    (let ((pid (match-string socket  pid)))
                      (with-input-from-string (ps (run-program (print-if-verbose (list "ps" "-p" pid))
                                                               :input nil :output :string :wait nil))
                        (unless (loop
                                  :named search-emacs
                                  :for line = (read-line ps nil nil)
                                  :while line
                                  :if (match "emacs" line)
                                  :do (return-from search-emacs t)
                                  :finally  (return-from search-emacs nil))
                          (delete-file socket)
                          (setf *sockets* (delete socket *sockets*)))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *emacsen*)

(options "mfod"
         (option ("verbose" "-v" "--verbose") ()
                 "Add printing how things are done."
                 (setf *verbose* t))

         (option ("list" "-l" "--list") ()
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
                       (uiop:run-program (print-if-verbose
                                          (list "ln" "-sf"
                                                (first server)
                                                (format nil "/tmp/emacs~A/server" uid)))))
                   (error "~A is not a server index. Please give an index between 1 and ~A"
                          index (length *emacsen*))))

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

         (version-option)
         (help-option)
         (bash-completion-options))

(defun xor (a b) (or (and a (not b)) (and (not a) b)))

(defun make-frame (socket-name &key on-display on-terminal)
  (assert (xor on-display on-terminal))
  (uiop:run-program
   (print-if-verbose
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
                            "--tty")))))))

(defun main (arguments)
  (let ((*verbose* (and arguments (verbose-option-p arguments))))
    (setf *sockets* (emacs-socket-candidates))
    (setf *emacsen* (emacsen))
    ;; Parse the options first, so --help/--version/--verbose are honoured
    ;; even when no emacs server is running.  With no arguments, report the
    ;; server status (or show help when a server is available).
    (parse-options *command* arguments
                   (lambda ()
                     (if (null *emacsen*)
                         (progn
                           (format t "There is no emacs server~%")
                           (exit ex-unavailable))
                         (progn
                           (call-option-function *command* "help" '())
                           (exit ex-noinput)))))
    ex-ok))

;;;; THE END ;;;;
