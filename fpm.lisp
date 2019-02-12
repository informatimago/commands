;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;
;;;; fpm = Front-end Package Management
;;;;
;;;;
;;;; The purpose of this command is to provide a unique user interface
;;;; to all the various and gratuituously distinct package management
;;;; user interfaces.
;;;;
;;;; Supported package managers:
;;;;
;;;; RedHat:   rpm  (Not implemented yet)
;;;; SuSE:     rpm  (Not implemented yet)
;;;; Debian:   apt
;;;; MacOSX:   macport
;;;; Gentoo:   portage
;;;;
;;;; Some commands may be not implemented yet, since I don't
;;;; necessarily know how to do everything with each package
;;;; management system (hence the need of this tool!).
;;;;

(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "0.0.3")

(defpackage "FRONT-END-UNIX-PACKAGE-MANAGEMENT"
  (:use "COMMON-LISP" "SCRIPT")
  (:nicknames "FPM")
  (:shadow "PACKAGE" "PACKAGEP" "PACKAGE-NAME")
  (:documentation "

fpm = Front-end Package Management


The purpose of this command is to provide a unique user interface
to all the various and gratuituously distinct package management
user interfaces.

Supported package managers:

RedHat:   rpm  (Not implemented yet)
SuSE:     rpm  (Not implemented yet)
Debian:   apt
MacOSX:   macport
Gentoo:   portage

Some commands may be not implemented yet, since I don't
necessarily know how to do everything with each package
management system (hence the need of this tool!).

"))
(in-package "FRONT-END-UNIX-PACKAGE-MANAGEMENT")


(defparameter *usage*
  "
~A~:* version

    Report the version of this script and the underlying package system.

~A~:* install     <package-name>

    Install the package named <package-name>.
    <package-name> may include some specific version according to the
    underlying package system syntax.


~A~:* update      <package-name>

    Updates the package named <package-name>.
    <package-name> may include some specific version or version constraint
    according to the underlying package system syntax.


~A~:* remove      <package-name>

    Uninstalls the package named <package-name>.
    <package-name> may include some specific version or version constraint
    according to the underlying package system syntax.


~A~:* [show] info <package-name>

    Displays information about the package named <package-name>.
    <package-name> may include some specific version or version constraint
    according to the underlying package system syntax.
    This package may be installed or not.


~A~:* [list] [installed|available|all|not installed] packages [<package-pattern>]

    Lists the packages and versions matching the <package-pattern> or
    all the packages if omited.   The keywords installed, available,
    all or not installed restrict the listing to the correponding
    package.  available and all are synonyms and the default.


~A~:* [list] files [in] <package-name>

    Lists the full pathnames of the files in the package named <package-name>.
    <package-name> may include some specific version or version constraint
    according to the underlying package system syntax.
    This package may be installed or not.


~A~:* package [containing] [file] <file-path>

    Lists the package(s) containing the file <file-path>.  If file path
    is not an absolute pathname, then it's taken as a pattern for the
    file paths.


~A~:* search [package] [info] <pattern>

    Lists the packages that have the <pattern> in their package information.


~A~:* [show] dependencies [of] [package] <package-name>

    List the packages on which the package named <package-name> depends.
    <package-name> may include some specific version according to the
    underlying package system syntax.
    [installed|newest]



~A~:* [who] depends [on] [package] <package-name>

    List the packages who depend on the package named <package-name>.
    <package-name> may include some specific version according to the
    underlying package system syntax.


~A~:* lowly cleanup

    Some package managers need some cleanup.

")

(defun print-usage ()
  (format t *usage* *program-name*))


(defmacro alt (&body body)
  #+ (or) (mapcar (lambda (item)
            (typecase item
              (null)
              (symbol)
              (cons))
            )
          body)
  '(progn))



;;;---------------------------------------------------------------------

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
         (status  (unwind-protect
                       ;; TODO: fork/exec to collect the PID.
                       (ext:run-program "/bin/bash"
                                        :arguments (list "-c" command)
                                        :output *run-output*
                                        :if-output-exists :append)
                    ;; TODO: kill the subprocess
                    (progn))))
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
  (let ((arguments  (mapcar (lambda (arg) (shell-quote-argument (princ-to-string arg)))
                            arguments)))
   (apply (function run) (format nil "~A~~@{ ~~A~~}" command) arguments)))


(defun e (&rest args)
  (with-output-to-string (out)
    (with-open-stream (in (ext:run-shell-command (format nil "~{~A ~}" args)
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
(defclass package-manager ()
  ((name :accessor package-manager-name :initarg :name :type string)))


(defgeneric pm-version (pm))
(defgeneric pm-install (pm package-designator))
(defgeneric pm-update  (pm package-designator))
(defgeneric pm-remove  (pm package-designator))
(defgeneric pm-info    (pm package-designator))

(defgeneric pm-list-packages (pm &key installed all available not-installed required
                                 packages pattern))

(defgeneric pm-list-files                    (pm package-designator))
(defgeneric pm-find-package-containing-file  (pm file-path))
(defgeneric pm-find-package-with-info        (pm pattern))
(defgeneric pm-dependencies                  (pm package-designator))
(defgeneric pm-dependants                    (pm package-designator))



(defgeneric pm-package-designator (pm package-designator)
  (:documentation
   "Return a single package, a list of packages,
or an expression such as (= <package>)  (<= <package>) etc."))

;;;------------------------------------------------------------
(defclass package ()
  ((category :initarg :category :initform "" :accessor package-category)
   (name     :initarg :name     :initform "" :accessor package-name)
   (version  :initarg :version  :initform "" :accessor package-version)))

(defun packagep (x) (typep x 'package))

(defmethod print-object ((self package) stream)
  (print-unreadable-object (self stream :type t)
    (format stream ":CATEGORY ~S :NAME ~S :VERSION ~S"
            (package-category self)
            (package-name self)
            (package-version self)))
  self)


(defgeneric package-file-pathname (package))

(defmethod existp ((self package))
   (probe-file (package-file-pathname self)))

(defmethod check-exists ((self package))
  "Returns SELF if it exists, or signals an error otherwise."
  (if (existp self)
      self
      (error "Package ~A doesn't exist." self)))



;;;------------------------------------------------------------
(defclass rpm (package-manager)
  ())

;;;------------------------------------------------------------
(defclass apt (package-manager)
  ())
  ;; http://www.debian-administration.org/articles/249
;; apt-cache search pound
;; pound - reverse proxy, load balancer and https front-end for web-servers

;; snowball:534$ dpkg -S /bin/cp
;; coreutils: /bin/cp
;; snowball:535$ apt-get source coreutils


(defmethod pm-version ((self apt))
  (s "apt-get --version"))

(defmethod pm-install ((self apt) package-designator)
  (s "apt-get install" package-designator))

(defmethod pm-update  ((self apt) package-designator)
  (s "apt-get upgrade" package-designator))

(defmethod pm-remove  ((self apt) package-designator)
  (s "apt-get remove" package-designator))

(defmethod pm-info    ((self apt) package-designator)
  (s "dpkg-query --show" package-designator))


(defmethod pm-list-packages ((self apt)
                             &key installed all available not-installed required
                             packages pattern)
  (cond
    (packages            (s "dpkg --status"  packages))
    (installed           (s "dpkg --list"    pattern))
    (required            (s "aptitude search '~~i !~~M'" pattern))
    (not-installed       (s "apt-cache search" pattern))
    (t                   (s "dpkg --list " pattern)
                         (s "apt-cache search" pattern))))

;; apt-cache search gnustep|sort -u|awk -F' - ' '{printf "%-30s  %s\n",$1,$2;}'

(defmethod pm-list-files                    ((self apt) package-designator)
  (s "dpkg --listfiles" package-designator))

(defmethod pm-find-package-containing-file  ((self apt) file-path)
  (format t "# Did you run: #   sudo apt-file update   # ?~%")
  (s "apt-file search" file-path))

(defmethod pm-find-package-with-info        ((self apt) pattern)
  (s "apt-cache search" pattern))

(defmethod pm-dependencies                  ((self apt) package-designator)
  (s "apt-cache depends" package-designator))

(defmethod pm-dependants                    ((self apt) package-designator)
  (s "apt-cache rdepends" package-designator))


;;;------------------------------------------------------------
(defclass macport (package-manager)
  ())

(defmethod pm-version ((self macport))
  (s "echo quit | port -v | head -1")) ; verbose

(defmethod pm-install ((self macport) package-designator)
  (s "port install" package-designator))

(defmethod pm-update  ((self macport) package-designator)
  (s "port update -u" package-designator))

(defmethod pm-remove  ((self macport) package-designator)
  (s "port uninstall -u" package-designator))

(defmethod pm-info    ((self macport) package-designator)
  (s "port info" package-designator))


(defmethod pm-list-packages ((self macport) &key installed all available required
                             not-installed packages pattern)
  (error "not implemented yet")
  (s "port list")
  )

(defmethod pm-list-files                    ((self macport) package-designator)
  (s "port contents" package-designator))

(defmethod pm-find-package-containing-file  ((self macport) file-path)
  (s "port provides" file-path))

(defmethod pm-find-package-with-info        ((self macport) pattern)
  (error "not implemented yet"))

(defmethod pm-dependencies                  ((self macport) package-designator)
  (s "port deps" package-designator))

(defmethod pm-dependants                    ((self macport) package-designator)
  (s "port dependents" package-designator))





;;;------------------------------------------------------------
(defclass portage (package-manager)
  ()
  (:documentation "This is the package manager of gentoo."))


(defclass portage-package (package)
  ())

(defmethod package-file-pathname ((self portage-package))
  (pathname (format nil "/usr/portage/~A/~A/~:*~A-~A.ebuild"
                    (package-category self)
                    (package-name self)
                    (package-version self))))

(defmethod package-atom ((self portage-package))
  (format nil "~A/~A-~A"
          (package-category self)
          (package-name self)
          (package-version self)))

;; (defparameter *portage-package-regexp*
;; "^\\(/usr/portage/\\)\\?\\([^/]*\\)/\\([^/]*\\)/\\([-_+A-Za-z0-9]*\\)-\\([0-9][.0-9]*[-_a-z0-9]*\\)\\(-r[0-9][0-9]*\\)\\?\\(\\.ebuild\\)\\?$"

;; /usr/portage/cat/pac/pac-vers.ebuild
;;              cat/pac/pac-vers

;;              cat/pac-vers
;;             =cat/pac-vers
;;             <=cat/pac-vers
;;             >=cat/pac-vers
;;             <cat/pac-vers
;;             >cat/pac-vers


;; /usr/portage/\([-a-zA-Z0-9]\)/\([-A-Za-z0-9]\)/\([-A-Za-z0-9]\)-\([0-9][.0-9]


(defparameter *portage-category-regexp*         "\\([A-Za-z0-9][-A-Za-z0-9]*\\)")
(defparameter *portage-name-regexp*             "\\([A-Za-z0-9][-_+A-Za-z0-9]*[+A-Za-z0-9]\\)")
(defparameter *portage-optional-version-regexp* "\\(\\(-\\([0-9][.0-9]*[-_a-z0-9]*\\)\\(-r[0-9][0-9]*\\)\\?\\)\\?\\)")

(defparameter *portage-pv-regexp*   (format nil "^~A~A$"
                                           *portage-name-regexp*
                                           *portage-optional-version-regexp*))

(defparameter *portage-cpv-regexp*  (format nil "^~A/~A~A$"
                                           *portage-category-regexp*
                                           *portage-name-regexp*
                                           *portage-optional-version-regexp*))

(defparameter *portage-cppv-regexp* (format nil "^~A/~A/~:*~A~A$"
                                           *portage-category-regexp*
                                           *portage-name-regexp*
                                           *portage-optional-version-regexp*))


(defun parse-portage-package-designator (package-designator &optional version-must-exist-p)
  (let ((designator (princ-to-string package-designator)))
    (if (zerop (length designator))
        '()
        (labels ((unambiguous (packages)
                   ;; (print `(unambiguous ,packages))
                   (if (= 1 (length packages))
                       (first packages)
                       (error "Ambiguous package designator: ~S~& could be any of:~{  ~A~^~%~}"
                              package-designator packages)))
                 (ambiguous (packages)
                   ;; (print `(ambiguous ,packages))
                   (cond
                     ((null packages) '())
                     ((null (rest packages))
                      (first packages))
                     (t
                      packages)))
                 (cppve (designator)
                   ;; (print `(cppve ,designator))
                   (if (suffixp ".ebuild" designator)
                       (cppv (subseq designator 0 (- (length designator) (length ".ebuild"))))
                       (cppv designator)))
                 (make-cpv (category name version)
                   (cond
                     ((string= version "")
                      (mapcan (compose ensure-list parse-portage-package-designator)
                              (directory (make-pathname :directory (list :absolute "usr" "portage" category name)
                                                        :name (format nil "~A-*" name)
                                                        :type "ebuild"
                                                        :case :local))))
                     ((not version-must-exist-p) ; note: version must not be empty, but it may not exist.
                      (if (directory (make-pathname :directory (list :absolute "usr" "portage" category name)
                                                    :name (format nil "~A-*" name)
                                                    :type "ebuild"
                                                    :case :local))
                          (list (make-instance 'portage-package
                                               :category category
                                               :name name
                                               :version (subseq version 1)))
                          (error "There is no such package: ~S" package-designator)))
                     (t
                      (list (check-exists (make-instance 'portage-package
                                                         :category category
                                                         :name name
                                                         :version (subseq version 1)))))))
                 (cppv (designator)
                   ;; (print `(cppv ,designator))
                   (multiple-value-bind (all category name1 name2 version)
                       (regexp:match *portage-cppv-regexp* designator)
                     ;; (print (list all category name1 name2 version))
                     (if all
                         (let ((category (regexp:match-string designator category))
                               (name1    (regexp:match-string designator name1))
                               (name2    (regexp:match-string designator name2))
                               (version  (regexp:match-string designator version)))
                           (if (string= name1 name2)
                                 (make-cpv category name1 version)
                              (error "Invalid package designator, inconsistent package names: ~S"
                                     package-designator)))
                         (error "Invalid package designator: ~S" package-designator))))
                 (cpv (designator)
                   ;; (print `(cpv ,designator))
                   (multiple-value-bind (all category name version)
                       (regexp:match *portage-cpv-regexp* designator)
                     ;; (print (list all category name version))
                     (if all
                         (let ((category (regexp:match-string designator category))
                               (name     (regexp:match-string designator name))
                               (version  (regexp:match-string designator version)))
                           (make-cpv category name version))
                         (error "Invalid package designator: ~S" package-designator))))
                 (pv (designator)
                   ;; (print `(pv ,designator))
                   (multiple-value-bind (all name version)
                       (regexp:match *portage-pv-regexp* designator)
                     ;; (print (list all name version))
                     (if all
                         (let ((name     (regexp:match-string designator name))
                               (version  (regexp:match-string designator version)))
                           (mapcan (compose ensure-list parse-portage-package-designator)
                                   (directory (make-pathname :directory (list :absolute "usr" "portage" :wild name)
                                                             :name (format nil "~A~A" name
                                                                           (if (string= version "")
                                                                               "-*"
                                                                               version))
                                                             :type "ebuild"
                                                             :case :local))))
                         (error "Invalid package designator: ~S" package-designator)))))
          (cond
            ((position (char designator 0) "=")
             (list '= (unambiguous (cpv (subseq designator 1)))))
            ((position (char designator 0) "<>")
             (if (and (< 2 (length designator))
                      (char= (char designator 1) #\=))
                 (list (intern (subseq designator 0 2))
                       (unambiguous (cpv (subseq designator 2))))
                 (list (intern (subseq designator 0 1))
                       (unambiguous (cpv (subseq designator 1))))))
            (t
             (ambiguous
              (let ((slashes (count #\/ designator)))
                (case slashes
                  ((0)   (pv designator))
                  ((1)   (cpv designator))
                  ((2)   (cppv designator))
                  ((5)   (cond
                           ((prefixp "/usr/portage/" designator)
                            (cppve (subseq designator (length "/usr/portage/"))))
                           ((suffixp ".ebuild" designator)
                            (cppv  (subseq designator 0 (- (length designator) (length ".ebuild")))))
                           (t
                            (error "Invalid package designator: ~S" package-designator))))
                  (otherwise
                   (error "Invalid package designator: ~S" package-designator)))))))))))


(defmethod pm-package-designator ((self portage) package-designator)
  (parse-portage-package-designator package-designator))


(defmethod pm-version ((self portage))
  (s "emerge --version"))

(defmethod pm-install ((self portage) package-designator)
  (s "emerge" package-designator))

(defmethod pm-update  ((self portage) package-designator)
  (s "emerge --update" package-designator))

(defmethod pm-remove  ((self portage) package-designator)
  (s "emerge --unmerge" package-designator))

(defmethod pm-info    ((self portage) package-designator)
  (flet ((info (package)
           ;; (s "equery meta" (package-atom package))
           (run  "for EBUILD in ~A ; do ( ~
                                cd $(dirname $EBUILD) ; EBUILD_PHASE=depend ; ECLASSDIR=/usr/portage/eclass ; PORTAGE_TMPDIR=/tmp ; ~
                                ~A ~
                                . $EBUILD ; ~
                                printf \"File:        %-20s\\nDescription: %s\\nHomepage:    %s\\n\\n\" \"$EBUILD\" \"$DESCRIPTION\" \"$HOMEPAGE\"  ~
                              ) ; done"
                 (package-file-pathname package)
                 (format nil ". ~{/~A~}/fpm-portage-functions.sh ; "
                         (rest (pathname-directory (or *load-pathname* "/home/pjb/bin/")))))))
    (let ((packages (ensure-list (pm-package-designator self package-designator))))
      (cond
        ((null packages) (format *error-output* "No package specified.~%"))
        ((packagep (first packages))
         (dolist (package packages)
           (info package)))
        ((eq '= (first packages))
         (info (second packages)))
        (t
         (error "Ambiguous package designator: ~S" package-designator))))))


(defmethod pm-list-packages ((self portage)
                             &key installed all available not-installed required
                             packages pattern)
  (cond
    (packages            (s "equery list" packages))
    (installed           (s "equery list    -i -f" pattern))
    (not-installed       (s "equery list -p -I -f" pattern))
    (required            (error "Not implemented yet"))
    (t                   (s "equery list -p    -f" pattern))))

(defmethod pm-list-files                    ((self portage) package-designator)
  (s "equery files" package-designator))

(defmethod pm-find-package-containing-file  ((self portage) file-path)
  (s "equery belongs" file-path))

(defmethod pm-find-package-with-info        ((self portage) pattern)
  (s "echo 'This is not good.'")
  (s "equery list" pattern))

(defmethod pm-dependencies                  ((self portage) package-designator)
  (s "equery depgraph" package-designator))

(defmethod pm-dependants                    ((self portage) package-designator)
  (s "equery depends" package-designator))



;;;---------------------------------------------------------------------

(defclass unimplemented-pms (package)
  ((pms :initarg :package-management-system
        :reader unimplemented-package-management-system)))

(defmethod unimplemented-pms ((self unimplemented-pms))
  (error "~:(~A~) has no implementation for the ~A package management system used in the ~A distribution of the ~A system."
         'fpm
         (unimplemented-package-management-system self)
         (distribution)
         (uname)))

(defmethod pm-package-designator ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))


(defmethod pm-version ((self unimplemented-pms))
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-install ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-update  ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-remove  ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-info    ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))


(defmethod pm-list-packages ((self unimplemented-pms)
                             &key installed all available not-installed required
                             packages pattern)
  (declare (ignore installed all available not-installed packages pattern))
  (unimplemented-pms self))

(defmethod pm-list-files                    ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-find-package-containing-file  ((self unimplemented-pms) file-path)
  (declare (ignore file-path))
  (unimplemented-pms self))

(defmethod pm-find-package-with-info        ((self unimplemented-pms) pattern)
  (declare (ignore pattern))
  (unimplemented-pms self))

(defmethod pm-dependencies                  ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))

(defmethod pm-dependants                    ((self unimplemented-pms) package-designator)
  (declare (ignore package-designator))
  (unimplemented-pms self))



;;;------------------------------------------------------------

;; (defun uname ()
;;   #+clisp
;;   (intern (string-upcase (posix:uname-sysname (posix:uname)))
;;           (load-time-value (find-package "KEYWORD")))
;;   #-clisp
;;   (let ((*package* (find-package "KEYWORD")))
;;     (read-from-string (e "uname"))))


(defun distribution ()
  (cond
    ((ignore-errors (probe-file "/etc/gentoo-release"))     :gentoo)
    ((ignore-errors (probe-file "/etc/SuSE-release"))       :suse)
    ((ignore-errors (probe-file "/etc/mandrake-release"))   :mandrake)
    ((ignore-errors (probe-file "/etc/redhat-release"))     :redhat)
    ((ignore-errors (probe-file "/etc/debian_version"))     :debian)
    ((with-open-file (release "/etc/lsb-release" :if-does-not-exist nil)
       (when release
         (loop
           :with distrib-id = "DISTRIB_ID="
           :for line = (read-line release nil)
           :while line
           :when (string= distrib-id line :end2 (length distrib-id))
           :do (return (intern (string-upcase (read-from-string line t nil :start (length distrib-id))) "KEYWORD"))))))
    (t                                                      :unknown)))


(defun distribution-default-package-management-system (sysname distribution)
  (case sysname
    ((:darwin) 'macport)
    ((:linux)
     (case distribution
       ((:suse :mandrake :redhat) 'rpm)
       ((:gentoo)                 'portage)
       ((:debian)                 'apt)
       (otherwise
        (error "~:(~A~) doesn't know the package management system used in the ~A distribution of the ~A system."
               'fpm distribution sysname))))
    ((:cygwin_nt-5.1 :cygwin_nt-6.1-wow64)
     'cygwin)
    (otherwise
     (error "~:(~A~) doesn't know the package management system used on the ~A system."
            'fpm sysname))))


(defparameter *package-management-system*
  ;; Notice that we don't signal an error just because we don't have a
  ;; package management system class, since the script may be loaded
  ;; without a need for it (eg. for bash completion).  The error is
  ;; deferred to the UNIMPLEMENTED-PMS class if it receives a message.
  (let ((pms (distribution-default-package-management-system (uname) (distribution))))
    (cond
      ((ignore-errors (find-class pms))
       (make-instance pms))
      (t
       (make-instance 'unimplemented-pms :package-management-system pms)))))


(defun fpm-operate (key &rest arguments)
  (if (member key '(version install update remove info
                    list-packages list-files
                    find-package-containing-file find-package-with-info
                    dependencies dependants))
      (apply (intern (format nil "PM-~A" key))
             *package-management-system* arguments)
      (error "Invalid key ~A" key)))


(defmacro fpm (key &rest arguments)
  `(fpm-operate (quote ,key) ,@arguments))


;;;------------------------------------------------------------



(define-option ("version" "-V" "--version") ()
  "Report the version of this script and the underlying package system."
  (format t "~A ~A~%" *program-name* *program-version*)
  (fpm version))


(define-option ("verbose" "-v" "--verbose") ()
  "Report writes the underlying commands that are run."
  (setf *verbose* t))


(define-option ("install" "-I" "--install") (package-name)
 "Install the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version according to the
underlying package system syntax."
 (fpm install package-name))


(define-option ("update" "-U" "--update") (package-name)
  "Updates the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version or version constraint
according to the underlying package system syntax."
  (fpm update package-name))


(define-option ("remove" "-R" "--remove" "uninstall" "--uninstall") (package-name)
  "Uninstalls the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version or version constraint
according to the underlying package system syntax.
"
  (fpm remove package-name))


(define-option ("show-info" "info" "-i" "--show-info" "--info") (package-name)
  ;; [show] info <package-name>
  "Displays information about the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version or version constraint
according to the underlying package system syntax.
This package may be installed or not.
"
  (fpm info package-name))


(define-option  ("list-packages" "-p" "--list-packages" "list-package"
                                 "list" "packages" "package")
    (&rest qualifier-and-package-pattern)
  ;; [list] [installed|available|all|not-installed|required] packages [<package-pattern>]
  "Lists the packages and versions matching the PACKAGE-PATTERN or
all the packages if omited.   The keywords installed, available,
all or not installed restrict the listing to the correponding
package.  available and all are synonyms and the default."
  (let ((valid-qualifiers  '(installed available all not-installed required)))
    (when (null qualifier-and-package-pattern)
      (error "Missing a PACKAGE-PATTERN"))
    (let ((qualifier
           (cond
             ((member (first qualifier-and-package-pattern) valid-qualifiers
                      :test (function string-equal))
              (pop  qualifier-and-package-pattern))
             ((< 1 (length qualifier-and-package-pattern))
              (error "Too many arguments: ~{~A~^ ~}" qualifier-and-package-pattern))
             (t
              'all)))
          (package-pattern (first qualifier-and-package-pattern)))
      (fpm list-packages
           (intern (string-upcase qualifier) "KEYWORD") t
           :pattern package-pattern))))


(define-option ("list-files" "files" "-l" "--list-files" "--files" "list-file" "file"
                             "contents" "content") (package-name)
  ;; [list] files [in] <package-name>
  "Lists the full pathnames of the files in the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version or version constraint
according to the underlying package system syntax.
This package may be installed or not.
"
  (fpm list-files package-name))


(define-option ("package-containing" "package" "-c" "--package-containing"
                                     "who-owns" "owns" "owner") (file-path)
  ;; package [containing] [file] <file-path>
  "Lists the package(s) containing the file FILE-PATH.  If file path
is not an absolute pathname, then it's taken as a pattern for the
file paths."
  (fpm find-package-containing-file file-path))



(define-option ("search-package-info" "search-package" "search"
                "-s"
                "--search-package-info" "--search-package" "--search") (pattern)
  ;; search [package] [info] <pattern>
  "Lists the packages that have the <pattern> in their package information."
  (fpm find-package-with-info pattern))



(define-option ("show-dependencies" "dependencies" "-d" "--show-dependencies" "--dependencies") (package-name)
;; [show] dependencies [of] [package] <package-name>
  "List the packages on which the package named PACKAGE-NAME depends.
PACKAGE-NAME may include some specific version according to the
underlying package system syntax.
[installed|newest]
"
  (fpm dependencies package-name))



(define-option ("who-depends-on" "depends" "--who-depends-on" "--who-depends" "--depends") (package-name)
  ;; [who] depends [on] [package] <package-name>
  "List the packages who depend on the package named PACKAGE-NAME.
PACKAGE-NAME may include some specific version according to the
underlying package system syntax.
"
  (fpm dependants package-name))




;;  "
;;
;; ~A~:* lowly cleanup
;;
;;     Some package managers need some cleanup.
;;
;; "

;; (trace run)
;; (trace s)


(parse-options ext:*args*)

;; #-testing-script
;; (ext:exit (main ext:*args*))

(ext:exit 0)
;;;------------------------------------------------------------


(alt
 (seq (function print-version)  version)
 (seq (function pkg-install)    install     <package-name>)
 (seq (function pkg-update)     update      <package-name>)
 (seq (function pkg-remove)     remove      <package-name>)
 (seq (function pkg-info)       (opt show) info <package-name>)
 (seq (function pkg-list-packages)
      (opt list) (opt (alt installed available all (seq not installed)))
      packages (opt <package-pattern>))
 (seq (function pkg-list-files) (opt list) files (opt in) <package-name>)
 (seq (function pkg-find-file) package (opt containing) (opt file) <file-path>)
 (seq (function pkg-find-info) search (opt package) (opt info) <pattern>)
 (seq (function pkg-dependencies)
      (opt show) dependencies (opt of) (opt package) <package-name>)
 (seq (function pkg-dependants)
      (opt who) depends (opt on) (opt package) <package-name>))

(format t "~&Not implemented yet~%")
;; (ext:exit 1)
