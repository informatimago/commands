;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               script.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     CLI
;;;;DESCRIPTION
;;;;
;;;;    This file defines utilities for lisp scripts.
;;;;
;;;;    Note: cl-launch scripts are not "scripts" but lisp programs that are
;;;;    compiled first.  So we cannot sequence different times like in a script
;;;;    when using cl-launch.  Instead, wrap functionalities in functions that
;;;;    will be called at *run-time*.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2019-02-12 <PJB> Adapted for cl-launch.
;;;;    2009-11-29 <PJB> Extracted from logs scripts.
;;;;    2009-07-27 <PJB> Merged log-to-script in here.
;;;;    2009-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2009 - 2019
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

(in-package "COM.INFORMATIMAGO.COMMAND.SCRIPT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From /usr/include/sysexits.h (Linux)
;;;

(defconstant ex-ok            0
  "successful termination")

(defconstant ex--base         64
  "base value for error messages")

(defconstant ex-usage         64
  "command line usage error
The command was used incorrectly, e.g., with
the wrong number of arguments, a bad flag, a bad
syntax in a parameter, or whatever.")

(defconstant ex-dataerr       65
  "data format error
The input data was incorrect in some way.
This should only be used for user's data & not
system files.")

(defconstant ex-noinput       66
  "cannot open input
An input file (not a system file) did not
exist or was not readable.  This could also include
errors like \"No message\" to a mailer (if it cared
to catch it).")

(defconstant ex-nouser        67
  "addressee unknown
The user specified did not exist.  This might
be used for mail addresses or remote logins.")

(defconstant ex-nohost        68
  "host name unknown
The host specified did not exist.  This is used
in mail addresses or network requests.")

(defconstant ex-unavailable   69
  "service unavailable
A service is unavailable.  This can occur
if a support program or file does not exist.  This
can also be used as a catchall message when something
you wanted to do doesn't work, but you don't know
why.")

(defconstant ex-software      70
  "internal software error
An internal software error has been detected.
This should be limited to non-operating system related
errors as possible.")

(defconstant ex-oserr         71
  "system error (e.g., can't fork)
An operating system error has been detected.
This is intended to be used for such things as \"cannot
fork\", \"cannot create pipe\", or the like.  It includes
things like getuid returning a user that does not
exist in the passwd file.")

(defconstant ex-osfile        72
  "critical OS file missing
Some system file (e.g., /etc/passwd, /etc/utmp,
etc.) does not exist, cannot be opened, or has some
sort of error (e.g., syntax error).")

(defconstant ex-cantcreat     73
  "can't create (user) output file
A (user specified) output file cannot be created.")

(defconstant ex-ioerr         74
  "input/output error
An error occurred while doing I/O on some file.")

(defconstant ex-tempfail      75
  "temp failure; user is invited to retry
temporary failure, indicating something that
is not really an error.  In sendmail, this means
that a mailer (e.g.) could not create a connection,
and the request should be reattempted later.")

(defconstant ex-protocol      76
  "remote error in protocol
the remote system returned something that
was \"not possible\" during a protocol exchange.")

(defconstant ex-noperm        77
  "permission denied
You did not have sufficient permission to
perform the operation.  This is not intended for
file system problems, which should use NOINPUT or
CANTCREAT, but rather for higher level permissions.")

(defconstant ex-config        78
  "configuration error")

(defconstant ex--max          78
  "maximum listed value")

(defun exit (&optional (code ex-ok))
  (uiop:quit code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun clean-package/common-lisp-user ()
  (mapc (lambda (package) (unuse-package package "COMMON-LISP-USER"))
        (set-difference
         (copy-seq (package-use-list "COMMON-LISP-USER"))
         (delete nil (list ;; A list of all possible "CL" packages:
                           (find-package "COMMON-LISP")
                           (find-package "IMAGE-BASED-COMMON-LISP"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defvar *default-program-name* "untitled")

(defun program-path ()
  #+ccl   (first ccl:*command-line-argument-list*)
  #+clisp (let* ((argv  (ext:argv))
                 (largv (length argv))
                 (args  ext:*args*)
                 (largs (length args))
                 (index (- largv largs 1))
                 (path  (and (<= 0 index largv) (elt argv index))))
            (cond
              (path
               path)
              #-cl-launch
              ((and *load-truename*
                    (string/= (file-namestring *load-truename*) "script.lisp"))
               (namestring *load-truename*))
              (t
               *default-program-name*)))
  #-(or ccl clisp) *default-program-name*)

(defvar *program-path* *default-program-name*
  "A namestring of the path to the program.
This may be a relative pathname, when it's obtained from argv[0].
If available we use the actual program name (from (EXT:ARGV) or
*LOAD-TRUENAME*), otherwise we fallback to *DEFAULT-PROGRAM-NAME*.")

(defvar *program-name* "untitled"
  "Name of the program.")

(defun arguments ()
  #+ccl   (rest ccl:*command-line-argument-list*)
  #+clisp ext:*args*
  #-(or ccl clisp) '())

(defvar *arguments* '()
  "A list of command line arguments (strings).")

(defvar *verbose* nil
  "Adds some trace output.")

(defvar *debug* nil
  "Errors break into the debugger.")

#-(or use-ppcre use-regexp) (pushnew (progn #-(and) :use-ppcre
                                            #+(and) :use-regexp)
                                     *features*)

;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#+clisp
(defun relaunch-with-kfull-linkset-if-needed (thunk)
  ;; If the version of clisp requires -Kfull to have linux, then let's call it with -Kfull…
  (multiple-value-bind (res err) (ignore-errors (funcall thunk))
    (when err
      (if (find "-Kfull" (ext:argv) :test (function string=))
          (error err)
          (ext:exit
           (or  (ext:run-program "/usr/local/bin/clisp"
                                 :arguments (append '("-ansi" "-q" "-E" "utf-8" "-Kfull")
                                                    (cons *program-path* *arguments*))
                                 :wait t
                                 #+linux :may-exec  #+linux t
                                 #+win32 :indirectp #+win32 nil)
                0))))))

#+(and clisp (not linux))
(when (find "linux" *modules* :test (function string=))
  (push :linux *features*))

#+(and clisp (not (or linux macos win32 #|what else is not linux?|#)))
(relaunch-with-kfull-linkset-if-needed
 (lambda ()
   ;; But sometimes, it's not enough, linux is just not there…
   (handler-case
       (require "linux")
     (error (err)
       (format *error-output* "~A: ~A~%Missing the Linux module.  Abort.~%" *program-name* err)
       (ext:quit 69)))))


(defun getenv (var)
  #+ccl (ccl:getenv var)
  #-ccl (uiop:getenv var))

(defun getuid ()
  #+ccl (ccl::getuid)
  #+clisp (funcall (or (function-named "getuid" "LINUX")
                       (function-named "GETUID" "LINUX")
                       (function-named "UID"    "POSIX")
                       (error "How to get the process UID in ~A?" (lisp-implementation-type))))
  #-(or ccl clisp) (error "How to get the process UID in ~A?" (lisp-implementation-type)))

(defun getpid ()
  #+ccl   (ccl::getpid)
  #+clisp (funcall (or (ignore-errors (find-symbol "getpid"     "LINUX"))
                       (ignore-errors (find-symbol "PROCESS-ID" "OS"))
                       (ignore-errors (find-symbol "PROCESS-ID" "SYSTEM"))
                       (error "How to get the process PID in ~A?" (lisp-implementation-type))))
  #-(or ccl clisp) (error "How to get the process PID in ~A?" (lisp-implementation-type)))

(defun report-the-error (err string-stream)
  (let ((log-path (format nil "/tmp/~A.~D.errors" *program-name*
                          (let ((getpid (getpid)))
                            (if getpid
                                (funcall getpid)
                                "nopid")))))
    (with-open-file (log-stream log-path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
      (format log-stream "~A GOT AN ERROR: ~A~%~80,,,'-<~>~%~A~%"
              *program-name* err (get-output-stream-string string-stream)))
    (format *error-output* "~A: ~A~%  See ~A~%" *program-name* err log-path)
    (finish-output *error-output*)
    (exit ex-software)))

(defmacro without-output (&body body)
  `(prog1 (values)
     (with-output-to-string (net)
       (handler-case
           (let ((*standard-output* net)
                 (*error-output*    net)
                 (*trace-output*    net))
             ,@body)
         (error (err) (report-the-error err net))))))

(defmacro redirecting-stdout-to-stderr (&body body)
  (let ((verror  (gensym))
        (voutput (gensym)))
    `(let* ((,verror  nil)
            (,voutput (with-output-to-string (stream)
                        (let ((*standard-output* stream)
                              (*error-output*    stream)
                              (*trace-output*    stream))
                          (handler-case (progn ,@body)
                            (error (err) (setf ,verror err)))))))
       (when ,verror
         (terpri *error-output*)
         (princ ,voutput *error-output*)
         (terpri *error-output*)
         (princ ,verror *error-output*)
         (terpri *error-output*)
         (terpri *error-output*)
         #-testing (exit ex-software)))))

(defmacro with-pager ((&key lines) &body body)
  "
Executes the BODY, redirecting *STANDARD-OUTPUT* to a pager.

If no option is given, use the system pager obtained from the
environment variable PAGER.  If none is defined, then no pager is
used.

The following is NOT IMPLEMENTED YET:

If an option is given, then it defines a local pager.

LINES     Number of line to output in a single chunk.
          After this number of line has been written,
          some user input is required to further display lines.
"
  (when lines
    (error "~S: Sorry :LINES is not implemented yet." 'with-pager))
  ;; (print (list (version:clisp-version)
  ;;              (version:rt-version<= "2.48" (version:clisp-version))))
  #-clisp
  `(progn ,@body)
  #+clisp
  `(progn
     #+#.(rt-version<= "2.44" (version))
     ,`(progn ,@body)
     #-#.(rt-version<= "2.44" (version))
     ,(let ((pager (uiop:getenv "PAGER")))
        (if pager
            (let ((pager-stream (gensym)))
              `(let ((,pager-stream (ext:make-pipe-output-stream
                                     ,pager
                                     :external-format charset:utf-8
                                     :buffered nil)
                                    ;; (ext:run-program ,pager
                                    ;;                  :input :stream
                                    ;;                  :output :terminal
                                    ;;                  :wait nil)
                                    ))
                 (unwind-protect
                      (let ((*standard-output* ,pager-stream))
                        ,@body)
                   (close ,pager-stream))))
            `(progn ,@body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defun shell-quote-argument (argument)
  "Quote ARGUMENT for passing as argument to an inferior shell."
  #+(or MSWINDOWS WIN32)
  ;; Quote using double quotes, but escape any existing quotes in
  ;; the argument with backslashes.
  (let ((result "")
        (start 0)
        (end)
        (match-beginning)
        (match-end))
    (when (or (null (setf match-end (position #\" argument)))
              (< match-end (length argument)))
      (loop
        :while (setf match-beginning (position #\" argument :start start))
        :do (setf end (1+ match-beginning)
                  result (concatenate 'string result (subseq argument start end)
                                      "\\" (subseq argument end (1+ end)))
                  start (1+ end))))
    (concatenate 'string "\"" result (subseq argument start) "\""))
  #-(or MSWINDOWS WIN32)
  (if (equal argument "")
      "''"
      ;; Quote everything except POSIX filename characters.
      ;; This should be safe enough even for really weird shells.
      (let ((result "")
            (start 0)
            (end)
            (match-end))
        (loop
          :while (setf match-end (position-if-not (lambda (ch) (or (alphanumericp ch) (position ch "-_./")))  argument :start start))
          :do (setf end match-end
                    result (concatenate 'string result (subseq argument start end)
                                        "\\" (subseq argument end (1+ end)))
                    start (1+ end)))
        (concatenate 'string result (subseq argument start)))))

(defun shell   (command &rest arguments)
  "
SEE ALSO:    EXECUTE.
"
  (uiop:run-program  (cons command arguments) :force-shell t))

(defun execute (&rest command)
  "
RETURN:     The status returned by the command.
SEE ALSO:   SHELL
"
  (uiop:run-program command :input nil :output t))

(defun cp (file newname &optional ok-if-already-exists keep-time)
  "
IMPLEMENTATION: The optional argument is not implemented.

Copy FILE to NEWNAME.  Both args must be strings.
If NEWNAME names a directory, copy FILE there.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
A prefix arg makes KEEP-TIME non-nil.
"
  (declare (ignore ok-if-already-exists keep-time))
  (execute "cp" (shell-quote-argument file)  (shell-quote-argument newname)))



#+(and clisp linux) ;; Should probably move away.
(defun make-symbolic-link (filename linkname &optional ok-if-already-exists)
  "
IMPLEMENTATION: The optional argument is not implemented.

Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.
Signals a `file-already-exists' error if a file LINKNAME already exists
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.
A number as third arg means request confirmation if LINKNAME already exists.
"
  (declare (ignore ok-if-already-exists))
  (/= 0 (linux:|symlink| filename linkname)))


#+(and clisp linux) ;; Should probably move away.
(defun make-directory (*path* &optional (parents nil))
  "
Create the directory *PATH* and any optionally nonexistent parents dirs.
The second (optional) argument PARENTS says whether
to create parents directories if they don't exist.
"
  (if parents
      (ensure-directories-exist (concatenate 'string *path* "/.") :verbose nil)
      (linux:|mkdir| *path*  511 #| #o777 |# ))
  (ext:probe-directory (if (char= (char *path* (1- (length *path*)))
                                  (character "/"))
                           *path* (concatenate 'string *path* "/"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perror (format-string &rest args)
  "
DO:     Writes a message on the error output in the name of the script.
"
  (format *error-output* "~&~A: ~?" *program-name*  format-string args)
  (finish-output *error-output*))


(defun pmessage (format-string &rest args)
  "
DO:     Writes a message on the standard output in the name of the script.
"
  (format *standard-output* "~&~A: ~?" *program-name* format-string args)
  (finish-output *standard-output*))


(defun pquery (format-string &rest args)
  "
DO:     Writes a message on the query I/O in the name of the script, and
        read a response line.
RETURN: A string containing the response line.
"
  (format *query-io* "~&~A: ~?" *program-name* format-string args)
  (finish-output *query-io*)
  (clear-input *query-io*)
  (read-line *query-io*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMMANDS
;;;

;;; Note: The command structure is used at compilation-time, and at run-time:
;;; - At compilation-time, the generate-commands.lisp script reads each
;;;   command source file and search for the command form, using the
;;;   :use-systems, :use-packages and :main options to compile the
;;;   command source file.
;;; - At run-time, the command form registers the options and documentation
;;;   of the command for option parsing and help.

(defstruct (option (:predicate optionp))
  keys arguments documentation function)

(defclass command ()
  ((name                 :initarg  :name
                         :initform nil
                         :accessor command-name)
   (documentation        :initarg  :documentation
                         :initform nil
                         :accessor command-documentation)
   (bash-completion-hook :initarg  :bash-completion-hook
                         :initform nil
                         :accessor command-bash-completion-hook
                         :documentation "A function (lambda (index words) ...) that will print the completion and return true, or do nothing and return nil.")
   (options              :initform (make-hash-table :test (function equal))
                         :reader   command-options)
   ;; compilation-time:
   (pathname             :initarg  :pathname      :initform nil    :accessor command-pathname)
   (use-systems          :initarg  :use-systems   :initform '()    :accessor command-use-systems)
   (use-packages         :initarg  :use-packages  :initform '()    :accessor command-use-packages)
   (main                 :initarg  :main          :initform nil)))

(defun command-package-name (command-name)
  (format nil "COMMAND.~:@(~A~)" command-name))

(defgeneric command-main (command)
  (:method ((command command))
    (or (slot-value command 'main)
        (setf (slot-value command 'main) (format nil "~:@(~A::MAIN~)"
                                                 (command-package-name
                                                  (command-name  command)))))))

(defun commandp (object) (typep object 'command))

(defgeneric add-option (command option)
  (:method ((command command) option)
    (dolist (name (option-keys option) command)
      (setf (gethash name (command-options command)) option))))

(defvar *commands* (make-hash-table :test (function equal)))
(defvar *command* nil "The current command.")

(defun command-named (name)
  (gethash name *commands*))

(defun register-command (&key name pathname use-systems use-packages main
                           documentation bash-completion-hook)
  (setf (gethash name *commands*)
        (make-instance 'command
                       :name name
                       :pathname pathname
                       :use-systems use-systems
                       :use-packages use-packages
                       :main main
                       :bash-completion-hook bash-completion-hook
                       :documentation documentation)))

(defmacro command (&key name use-systems use-packages main
                     documentation options bash-completion-hook)
  "
This macro registers a command, and is also used as a declaration:
it's read by the command generator script, to know the systems to be
quickloaded,and the packages to be used by the command package.
We can also specify a different main function than the default MAIN.
The rest of the file will be compiled and loaded in the command package.

NAME:          a string, the name of the command.

Compilation-time slots:

MAIN:          a string, containing the name of the symbol fbound
               to the main function.
USE-SYSTEMS:   a list (not evaluated) of system names.
USE-PACKAGES:  a list (not evaluated) of package names.

Run-time slots:

DOCUMENTATION: a string containing the documentation of the commands.
OPTIONS:       an expression that should return a list of clauses,
               each clause is a list of the form:
               ((option-name …) parsed-option) as returned by the
               OPTION macro.

RETURN:        a new command structure.
"
  (let ((name (or name (pathname-name (or *compile-file-truename* *load-truename*)))))
    `(eval-when (:load-toplevel :execute)
       (let ((command  (register-command :name ',name
                                         :main ',main
                                         :use-systems ',use-systems
                                         :use-packages ',use-packages
                                         :documentation ',documentation
                                         :bash-completion-hook ,bash-completion-hook)))
         (dolist (option ,options)
           (add-option command option))
         command))))

(defparameter *default-package-use-list*
  '("COMMON-LISP" "COM.INFORMATIMAGO.COMMAND.SCRIPT"))

(defun package-set-equal-p (p q)
  (and (subsetp p q :key (function package-name) :test (function string=))
       (subsetp q p :key (function package-name) :test (function string=))))

(defun %command-package (name package-use-list)
  "Use PACKAGE-USE-LIST if not empty, otherwise use *DEFAULT-PACKAGE-USE-LIST*."
  (let ((package-name (command-package-name name)))
    (or (let ((package (find-package package-name)))
          (when (and package package-use-list)
            (unless (package-set-equal-p (package-use-list package) package-use-list)
              (unuse-package (set-difference (package-use-list package) package-use-list
                                             :key (function package-name) :test (function string=))
                             package)
              (use-package package-use-list package)))
          package)
        (make-package package-name
                      :use (or package-use-list
                               *default-package-use-list*)))))

(defgeneric command-package (object)
  (:documentation "Returns the package of the command. Creates it if it doesn't already exist.
SEE: COMMAND-PACKAGE-NAME")
  (:method ((command command))
    (%command-package (command-name command) (command-use-packages command)))
  (:method ((name string))
    (let ((command (command-named name)))
      (if command
          (command-package command)
          (%command-package name nil)))))

(defun command-form-p (form)
  (and (consp form) (symbolp (first form)) (string= (first form) 'command)))

(defun read-command-form (source)
  (let ((package (command-package (format nil "read command form temp package ~X" (random (expt 2 32))))))
    (unwind-protect
         (let ((*package* package))
           (loop
             :for form := (ignore-errors (read source nil source))
             :until (eql form source)
             :when (command-form-p form)
               :do (return-from read-command-form form)
             :finally (return nil)))
      (delete-package package))))

(defun register-command-file (name pathname)
  (with-open-file (source pathname)
    (let ((form (read-command-form source)))
      (apply (function register-command)
             :name name
             :pathname pathname
             :allow-other-keys t
             (when (command-form-p form)
               (rest form))))))

(defun dispatch-command (pname &rest arguments)
  "A toplevel to dispatch to commands."
  (let* ((name    (pathname-name pname))
         (command (command-named name)))
    (if command
        (progn
          (setf com.informatimago.command.script:*command*              command
                com.informatimago.command.script:*program-name*         name
                com.informatimago.command.script:*default-program-name* name
                com.informatimago.command.script:*program-path*         pname
                com.informatimago.command.script:*arguments*            arguments)
          (com.informatimago.command.script:exit
           (handler-case
               (funcall (read-from-string (command-main command)) arguments)
             (error (err)
               (format *error-output* "~&~A: ~A~%" name err)
               (finish-output *error-output*)
               com.informatimago.command.script:ex-software))))
        (error "No such command: ~S" name))
    (values)))

;; Options:

(defun q&d-parse-parameters (parameters)
  "Parses (mandatory &optional optionals... &rest rest &key key...)"
  (loop
     :with mandatories = '()
     :with optionals   = '()
     :with rest        = nil
     :with keys        = '()
     :with state       = :mandatory
     :with params      = parameters
     :for param = (first params)
     :while params
     :do (ecase state
           ((:mandatory)
            (case param
              ((&optional) (setf state :optional))
              ((&rest)     (setf state :rest))
              ((&key)      (setf state :key))
              (otherwise (push param mandatories)))
            (pop params))
           ((:optional)
            (case param
              ((&optional) (error "&OPTIONAL given more than once in ~S" parameters))
              ((&rest)     (setf state :rest))
              ((&key)      (setf state :key))
              (otherwise (push param optionals)))
            (pop params))
           ((:rest)
            (case param
              ((&optional) (error "&OPTIONAL given after &REST in ~S" parameters))
              ((&rest)     (error "&REST given twice in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (setf state :after-rest
                                 rest param)))
            (pop params))
           ((:after-rest)
            (case param
              ((&optional) (error "&OPTIONAL given after &REST in ~S" parameters))
              ((&rest)     (error "&REST given after &REST in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (error "Several &REST parameters given in ~S" parameters)))
            (pop params))
           ((:key)
            (case param
              ((&optional) (error "&OPTIONAL given after &KEY in ~S" parameters))
              ((&rest)     (error "&REST given after &KEY in ~S" parameters))
              ((&key)      (setf state :key))
              (otherwise   (push param keys)))
            (pop params)))
     :finally (return (values (nreverse mandatories)
                              (nreverse optionals)
                              rest
                              (nreverse keys)))))

(defun q&d-arguments (mandatories optionals rest keys)
  "
BUG: when the optionals or keys have a present indicator,
     we just ignore it and build a list that will pass
     the default value anyways...
"
  (assert (every (function symbolp) mandatories))
  (append mandatories
          (mapcar (lambda (opt)
                    (etypecase opt
                      (cons   (first opt))
                      (symbol opt)))
                  optionals)
          (when rest (list rest))
          (mapcan (lambda (key)
                    (etypecase key
                      (cons  (etypecase (first key)
                               (symbol (list (keywordize (first key)) (first key)))
                               (cons   (list (second (first key)) (first (first key))))))
                      (symbol (list (keywordize key) key))))
                  keys)))

(defun wrap-option-function (keys option-arguments docstring option-function)
  (let ((vargs (gensym)))
    (multiple-value-bind (mandatories optionals rest keys-args) (q&d-parse-parameters option-arguments)
      (make-option
       :keys keys
       :arguments option-arguments
       :function (compile (make-symbol (format nil "~@(~A-WRAPPER~)" (first keys)))
                          `(lambda (option-key ,vargs)
                             (let ((nargs (length ,vargs)))
                               (if (<= ,(length mandatories) nargs)
                                   ,(cond
                                     (rest
                                      `(destructuring-bind ,option-arguments ,vargs
                                         (funcall ',option-function ,@(q&d-arguments mandatories
                                                                                     optionals
                                                                                     rest
                                                                                     keys-args))
                                         nil))
                                     (keys-args
                                      (error "An option cannot have &key parameters without a &rest parameter. ~@
                                              Invalid option parameters: ~S" option-arguments))
                                     (t
                                      (let ((vremaining (gensym)))
                                        `(destructuring-bind (,@option-arguments &rest ,vremaining) ,vargs
                                           (funcall ',option-function ,@(q&d-arguments mandatories
                                                                                       optionals
                                                                                       rest
                                                                                       keys-args))
                                           ,vremaining))))
                                   (let ((missing-count (- ,(length mandatories) nargs))
                                         (missing-args  (subseq ',mandatories nargs)))
                                     (error "option ~A is missing ~:[an ~*~;~A ~]argument~:*~p: ~{~A ~}"
                                            option-key
                                            (< 1 missing-count) missing-count
                                            missing-args))))))
       :documentation (split-string docstring (string #\newline))))))

(defun call-option-function (command option-key arguments &optional undefined-argument)
  (let ((option (gethash option-key (command-options command))))
    (cond
      (option             (funcall (option-function option) option-key arguments))
      (undefined-argument (funcall undefined-argument option-key arguments))
      (t                  (error "Unknown option ~A ; try: ~A help" option-key *program-name*)))))

(defmacro option (names parameters &body body)
  "
DO:         Define a new option for the scirpt.
NAMES:      A list designator of option names (strings
            such as \"-a\" \"--always\").
PARAMETERS: A list of option parameters.  The names of
            these parameters must be descriptive as they
            are used to build the usage help text.
BODY:       The code implementing this option.
RETURN:     The lisp-name of the option (this is a symbol
            named for the first option name).
"
  (let* ((main-name   (if (listp names)
                          (first names)
                          names))
         (other-names (if (listp names)
                          (rest names)
                          '()))
         (lisp-name   (intern (string-upcase main-name)))
         (docstring   (if (and (stringp (first body)) (rest body))
                          (first body)
                          nil))
         (body        (if (and (stringp (first body)) (rest body))
                          (rest body)
                          body)))
    `(wrap-option-function ',(cons main-name other-names)
                                 ',parameters
                                 ',docstring
                                 (lambda ,(remove '&rest parameters)
                                   ,docstring
                                   (block ,lisp-name
                                     ,@body)))))

(defgeneric option-list (command)
  (:method ((command command))
    (let ((options '()))
      (maphash (lambda (key option)
                 (declare (ignore key))
                 (pushnew option options))
               (command-options command))
      options)))

;; TODO: See if we couldn't simplify it, perhaps with complete -C.

(defgeneric list-all-option-keys (command)
  (:method ((command command))
    (let ((keys '()))
      (dolist (option (option-list command))
        (dolist (key (option-keys option))
          (push key keys)))
      keys)))

(defun help-option ()
  (option ("help" "-h" "--help") ()
          "Give this help."
          (with-pager ()
            (let ((options (option-list *command*)))
              (format t "~2%~A options:~2%" *program-name*)
              (dolist (option (sort options (function string<)
                                    :key (lambda (option) (first (option-keys option)))))
                (format t "    ~{~A~^ | ~}  ~:@(~{~A ~}~)~%~@[~{~%        ~A~}~]~2%"
                        (option-keys option)
                        (option-arguments option)
                        (option-documentation option)))
              (format t "~@[~A~%~]" (command-documentation *command*))))))

(defun completion-option-prefix (command prefix)
  (dolist (key (remove-if-not (lambda (key)
                                (and (<= (length prefix) (length key))
                                     (string= prefix key :end2 (length prefix))))
                              (list-all-option-keys command)))
    (format t "~A~%" key))
  (finish-output))

(defun completion-all-options (command)
  (dolist (key (list-all-option-keys command))
    (format t "~A~%" key))
  (finish-output))

(defun bash-completion-options ()
  (list (option ("--bash-completions") (index &rest words)
                "Implement the auto-completion of arguments.
This option is designed to be invoked from the function generated by
the '--bash-completion-function' option.  There should be no need to
use directly.
"
                (let ((index (parse-integer index :junk-allowed t)))
                  (unless (and (command-bash-completion-hook *command*)
                               (funcall (command-bash-completion-hook *command*) index words))
                    (if index
                        (completion-option-prefix *command* (elt words index))
                        (completion-all-options *command*))))
                (exit 0))

        (option ("--bash-completion-function") ()
                "Write two bash commands (separated by a semi-colon) to create a
bash function used to do auto-completion of command arguments.
Use it with:

       eval $($COMMAND  --bash-completion-function)

and then typing TAB on the command line after the command name will
autocomplete argument prefixes.
"
                (format t "function completion_~A(){ ~
COMPREPLY=( $(~:*~A --bash-completions \"$COMP_CWORD\" \"${COMP_WORDS[@]}\") ) ; } ;~
complete -F completion_~:*~A ~:*~A~%"
                        *program-name*)
                (exit 0))))

(defun parse-options (command arguments &optional default undefined-argument)
  (flet ((process-arguments ()
           (cond
             (arguments
              (loop
                 :while arguments
                 :do (setf arguments (call-option-function command (pop arguments) arguments undefined-argument))))
             (default
              (funcall default)))))
    (if *debug*
        (process-arguments)
        (handler-case (process-arguments)
          (error (err)
            (perror "~A~%" err)
            ;; TODO: select different sysexits codes depending on the error class.
            (return-from parse-options ex-software)))))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defun not-implemented-here (function-name)
  (error "How to implement ~S in ~S"
         function-name
         (lisp-implementation-type)))

(defun prepare-options (options)
  (mapcar (lambda (option)
            (typecase option
              (keyword (format nil "-~(~A~)" option))
              (symbol  (string-downcase option))
              (string  option)
              (t       (prin1-to-string option))))
          options))

(defun run-program (program-and-arguments &key (input :terminal) (output :terminal)
                    (if-output-exists :error) (wait t))
  "
RETURN:     The status returned by the command.
SEE ALSO:   SHELL
"
  (uiop:run-program program-and-arguments :input input :output output :if-output-exists if-output-exists :wait wait))


(defun uname (&rest options)
  "Without OPTIONS, return a keyword naming the system (:LINUX, :DARWIN, etc).
With options, returns the first line output by uname(1)."
  (with-open-stream (uname #+ccl (ccl:external-process-output-stream
                                  (ccl:run-program "uname" (prepare-options options)
                                                   :input nil :output :stream :wait nil))
                           #-ccl (error "run-program not implemented yet"))
    (values (if options
                (read-line uname)
                (intern (string-upcase (read-line uname))
                        "KEYWORD")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Some emacs style functions.
;;;

(defun find-directories (rootpath)
  "Return a list of recursive subdirectories starting from ROOTPATH
that are accessible by the user."
  (directory (merge-pathnames (make-pathname :directory '(:relative :wild-inferiors)
                                             :name nil :type nil :version nil)
                              rootpath nil)))

(defun concat (&rest items) (apply (function concatenate) 'string items))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;; The main program, definition of the options
;; ;;;
;;
;;
;; (in-package "COMMON-LISP-USER")
;; (load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common
;;                      :defaults *load-pathname*))
;; (use-package "SCRIPT")
;;
;; ;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))

(defun initialize ()
  (setf *program-path* (program-path)
        *program-name* (file-namestring *program-path*)
        *arguments*    (arguments))
  (values))

(defun run-main (main)
  (handler-case (uiop:quit (funcall main uiop:*command-line-arguments*))
    (error (err)
      (format *error-output* "~&~A~%" err)
      (finish-output *error-output*)
      (uiop:quit 1))))



;;;; THE END ;;;;
