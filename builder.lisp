;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               builder.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Builder script for the commands executable.
;;;;    This scripts compiles and load all commands
;;;;    and their dependencies.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-06-02 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.COMMAND.GENERATE")

;;; --------------------------------------------------------------------
(ql:quickload :com.informatimago.common-lisp.cesarum)

(cl:defpackage "COM.INFORMATIMAGO.COMMAND.GENERATE.E48"
  (:use)
  (:export "ACK" "APC" "BEL" "BPH" "BS" "CAN" "CBT" "CCH" "CHA" "CHT"
           "CMD" "CNL" "CPL" "CPR" "CR" "CSI" "CTC" "CUB" "CUD" "CUF"
           "CUP" "CUU" "CVT" "DA" "DAQ" "DC1" "DC2" "DC3" "DC4" "DCH"
           "DCS" "DL" "DLE" "DMI" "DSR" "DTA" "EA" "ECH" "ED" "EF"
           "EL" "EM" "EMI" "ENQ" "EOT" "EPA" "ESA" "ESC" "ETB" "ETX"
           "FF" "FNK" "FNT" "GCC" "GSM" "GSS" "HPA" "HPB" "HPR" "HT"
           "HTJ" "HTS" "HVP" "ICH" "IDCS" "IGS" "IL" "INT" "IS1" "IS2"
           "IS3" "IS4" "JFY" "LF" "LS0" "LS1" "LS2" "LS3" "MC" "MW"
           "NAK" "NBH" "NEL" "NP" "NUL" "OSC" "PEC" "PFS" "PLD" "PLU"
           "PM" "PP" "PPA" "PPB" "PPR" "PTX" "PU1" "PU2" "QUAD" "REP"
           "RI" "RIS" "RM" "SACS" "SAPV" "SCI" "SCO" "SCP" "SCS" "SD"
           "SDS" "SEE" "SEF" "SGR" "SHS" "SI" "SIMD" "SL" "SLH" "SLL"
           "SLS" "SM" "SO" "SOH" "SOS" "SPA" "SPD" "SPH" "SPI" "SPL"
           "SPQR" "SR" "SRCS" "SRS" "SS2" "SS3" "SSA" "SSU" "SSW" "ST"
           "STAB" "STS" "STX" "SU" "SUB" "SVS" "SYN" "TAC" "TALE"
           "TATE" "TBC" "TCC" "TSR" "TSS" "VPA" "VPB" "VPR" "VT"
           "VTS"))
(cl:in-package "COM.INFORMATIMAGO.COMMAND.GENERATE.E48")

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.ecma048:define-all-functions
      :export cl:t :8-bit cl:nil :print cl:nil :result-type cl:string))

;;; --------------------------------------------------------------------
;;; generate the program
;;;
(cl:in-package "COM.INFORMATIMAGO.COMMAND.GENERATE")
(import 'com.informatimago.command.generate.e48:sgr)

(defconstant normal       0)
(defconstant black        30)
(defconstant red          31)
(defconstant green        32)
(defconstant yellow       33)
(defconstant blue         34)
(defconstant magenta      35)
(defconstant cyan         36)
(defconstant white        37)
(defconstant black-back   40)
(defconstant red-back     41)
(defconstant green-back   42)
(defconstant yellow-back  43)
(defconstant blue-back    44)
(defconstant magenta-back 45)
(defconstant cyan-back    46)
(defconstant white-back   47)


(defparameter *all-commands*
  '("add-cookie" "add-paths" "ansi-test" "batch-emerge"
    "bin-to-c-array" "buzzword" "capitalize"
    "cddb-to-tag" "check-surface" "clar"
    "clean-bd-archive" "clean-name" ;; "clean-paths"
    "commands"
    "columnify" "cookie-diff" "cookie-loop"
    "cookie-merge" "cookie" "dedup"
    "departement" "diss" "downcase"
    "edit-comments-of-ogg" "entropy" "euronews"
    "extend-identifiers" "fetch-pop" "fpm"
    "get-directory" "group-files"
    "hexbin"
    "html-make-image-index" "insulte" "kwic" "lc" "llen"
    "lrev" "macosx-port-uninstall-recursively" "memo"
    "menu" "merge" "mfod" "new-password" "news-to-mbox"
    "nls" "one-of" "pic-resize" "pjb-diff" "programmer"
    "pseudo-pop" "radio" "random"
    "religion" "remove-duplicate-files" "revlines"
    "rotate" "rstuml"
    "script-test"
    "sleep-schedule" "split-dir"
    "split-merge" "substitute" "surveille-host"
    "surveille-web-pages" "svn-locate-revision" "text"
    "when")
  #-(and) '("box" "cpcd"  "shell"))

(dolist (name *all-commands*)
  (ignore-errors (delete-package (command-package-name name))))

;;; Scan command sources for command :use-systems forms.

(defun register-commands ()
  (let ((*default-pathname-defaults* (merge-pathnames (make-pathname :directory '(:relative "commands")
                                                                     :name nil
                                                                     :type "lisp"
                                                                     :version nil)
                                                      cl-user::*source-directory*)))
    (dolist (name *all-commands*)
      (register-command-file name (merge-pathnames name *default-pathname-defaults* nil)))))

;;; Quickload used systems:

(defun quickload-command-dependencies ()
  (ql:quickload (delete-duplicates
                 (mapcan (lambda (name) (copy-list (command-use-systems (command-named name))))
                         *all-commands*))))

;;; Compile and load all commands:

(defparameter *failures* 0)

(defun compile-and-load-command (name)
  (handler-case
      (let* ((package (command-package name))
             (command (command-named name)))
        (if (let ((type (and (find-package "SWANK/GRAY")
                             (find-symbol "SLIME-OUTPUT-STREAM" "SWANK/GRAY"))))
              (and type (typep *standard-output* type)))
            (format t "~&;Processing ~A in package ~A~%"
                    name (package-name package))
            (format t "~&;Processing ~A~A~A in package ~A~A~A~%"
                    (sgr blue) name (sgr normal)
                    (sgr cyan) (package-name package) (sgr normal)))
        (finish-output)
        (multiple-value-bind (fasl warnings-p failure-p)
            (let ((*program-name* name)
                  (*package* package))
              (with-compilation-unit (:override t)
                (compile-file (command-pathname command) :verbose t)))
          (if (or warnings-p failure-p)
              (progn
                (format t "~&;Failed to compile ~S~%" (command-pathname command))
                (incf *failures*))
              (if (let ((*program-name* name)
                        (*package* package))
                    (load fasl :verbose t))
                  (format t "~&;Successfully loaded ~S~%" fasl)
                  (progn
                    (format t "~&;Failed to load ~S~%" (command-pathname command))
                    (incf *failures*))))))
    (error (err)
      (format *error-output* "~&~A~%" err) (finish-output *error-output*))))

;;; Generate link script:

(defun generate-link-script ()
  (with-open-file (*standard-output*
                   (merge-pathnames (make-pathname :name "symlink-commands" :type :unspecific :version :unspecific
                                                   :defaults cl-user::*release-directory*)
                                    cl-user::*release-directory*
                                    nil)
                   :direction :output
                   :if-does-not-exist :create
                   :if-exists :supersede)
    (write-line "#!/bin/bash")
    (dolist (name *all-commands*)
      (unless (string= name "commands")
        (format t "ln -s commands ~A~%" name)))))



(progn
  (register-commands)
  (quickload-command-dependencies)
  (generate-link-script)
  (setf *failures* 0)
  (dolist (name *all-commands*)
    (compile-and-load-command name)))

;;;; THE END ;;;;
