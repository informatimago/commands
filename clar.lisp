#!/usr/local/bin/cl
;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              clar
;;;;LANGUAGE:          common lisp (clisp)
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    CLI
;;;;DESCRIPTION
;;;;    This script joins or splits lisp sources between a single file
;;;;    and several files.
;;;;USAGE
;;;;
;;;;    clar single.clar a.lisp ... z.lisp
;;;;            -- creates a single.clar as a concatenation of a.lisp ... z.lisp
;;;;
;;;;    clar single.lisp
;;;;            -- splits single.clar into the original a.lisp ... z.lisp files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2010-09-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2010 - 2010
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************
(in-package "COMMON-LISP-USER")
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (load (make-pathname :name "SCRIPT" :type "LISP" :version nil :case :common :defaults *load-pathname*)))
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (use-package "SCRIPT"))

(defparameter *program-name*    "clar")
(defparameter *program-version* "0.1.2")

(defparameter *escape-constr*  ";;;; -%- CLAR ~A~%")
(defparameter *escape-regexp* "^;;;; -%- CLAR \\(.*\\)")
(defparameter *file-constr*    ";;;; -%- CLAR FILE -%- ~A~%")
(defparameter *file-regexp*   "^;;;; -%- CLAR FILE -%- \\(.*\\)")
(defparameter *end-constr*     ";;;; -%- CLAR END -%-~%")
(defparameter *end-regexp*    "^;;;; -%- CLAR END -%-")

;;;; -%- CLAR FILE -%- It's a trap!

(defun match (regexp string)
  #-clisp (declare (ignore regexp string))
  #+clisp (regexp:match regexp string)
  #-clisp (error "regexp not implemented yet on ~A" (lisp-implementation-type)))

(defun match-string (string range)
  #-clisp (declare (ignore string range))
  #+clisp (regexp:match-string string range)
  #-clisp (error "regexp not implemented yet on ~A" (lisp-implementation-type)))



(defun valid-file-namestring-p (namestring)
  (every (lambda (ch) (or (alphanumericp ch) (position ch "-._"))) namestring))

(defparameter *external-format*
  #+clisp charset:iso-8859-1
  #-clisp :iso-8859-1)

(defun join (output inputs)
  (with-open-file (out output
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede
                       :external-format *external-format*)
    (dolist (input inputs)
      (if (valid-file-namestring-p (file-namestring input))
        (with-open-file (inp input
                             :direction :input
                             :if-does-not-exist :error
                             :external-format *external-format*)
          (format out *file-constr* (file-namestring input))
          (loop
            :for line = (read-line inp nil nil)
            :while line
            :do (if (match *escape-regexp* line)
                  (format out *escape-constr* line)
                  (write-line line out))))
        (warn "Invalid file namestring ~S -- rejected." input)))
    (format out *end-constr*)))


(defun split (archive)
  (with-open-file (inp archive
                       :direction :input
                       :if-does-not-exist :error
                       :external-format *external-format*)
    (let ((*print-pretty*       nil)
          (*print-right-margin* nil)
          (out          nil)
          (regexps      (list *file-regexp* *end-regexp* *escape-regexp*)))
      (unwind-protect
          (loop :for line = (read-line inp nil nil) :while line :do
            (let ((matches (mapcar (lambda (regexp)
                                       (mapcar (lambda (range) (match-string line range))
                                        (multiple-value-list (match regexp line))))
                                   regexps)))
              (cond
                ((elt matches 0)
                 (when out (close out))
                 (let ((name (second (elt matches 0))))
                   (setf out (if (valid-file-namestring-p name)
                               (open name
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede
                                     :external-format *external-format*)
                               (progn
                                 (warn "Invalid file namestring ~S -- ignored." name)
                                 (make-broadcast-stream))))))
                ((elt matches 1)
                 (close out)
                 (setf out nil)
                 (loop-finish))
                ((elt matches 2)
                 (write-line (second (elt matches 2)) out))
                (out
                 (write-line line out))
                (t
                 (warn "Prefix line: ~S" line)))))
        (when out (close out))))))


(defun usage ()
  (format t "~A version ~A~%" *program-name* *program-version*)
  (format t "~A usage:~2%" *program-name*)
  (format t "~T~A  single.clar   a.lisp .... z.lisp~%" *program-name*)
  (format t "~T~T# create a single file from several sources.~2%")
  (format t "~T~A  single.clar~%" *program-name*)
  (format t "~T~T# split out several sources from a single file.~2%"))


(defun main (files)
  (handler-case
      (cond
        ((null files)
         (usage)
         1)
        ((some (lambda (file) (or (zerop (length file))
                               (char= #\- (aref file 0)))) files)
         (usage)
         2)
        ((null (rest files))
         (split (first files))
         0)
        (t
         (join (first files) (rest files))
         0))
    (error (err)
           (format t "~A: ~A~%" *program-name* err)
           3)))

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
