#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               get-directory
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Search for the given directory tag in the ~/directories.txt file,
;;;;    and outputs a directory made appending the given optional subdirectory.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-12-04 <PJB> Added this header; convert to CL.
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




;;;----------------------------------------------------------------------
;;;
;;; Directories.
;;;

(defvar *directories*  '())

(defun list-directories ()
  "Returns the list of named directories."
  (copy-seq *directories*))

(defun get-directory (key &optional (subpath ""))
  "
Caches the ~/directories.txt file that contains a map of
directory keys to pathnames, into *DIRECTORIES*.

Then builds and returns a pathname made by merging the directory
selected by KEY, and the given SUBPATH.
"
  (unless *directories*
    (with-open-file (dirs (merge-pathnames
                           (make-pathname :name "DIRECTORIES" :type "TXT"
                                          :version nil :case :common)
                           (user-homedir-pathname)
                           nil))
      (loop
         :for k = (read dirs nil dirs)
         :until (eq k dirs)
         :do (push (string-trim " " (read-line dirs)) *directories*)
         :do (push (intern (substitute #\- #\_ (string k))
                           "KEYWORD") *directories*))))
  (unless (getf *directories* key)
    (error "~S: No directory keyed ~S" 'get-directory key))
  (merge-pathnames subpath (getf *directories* key) nil))


;;;----------------------------------------------------------------------

(ext:exit
 (handler-case
     (destructuring-bind (key &optional (subpath "")) ext:*args*
       (princ (get-directory (intern (substitute #\- #\_ key) "KEYWORD") subpath))
       (terpri)
       (finish-output)
       0)
   (error (err)
     (princ err *error-output*)
     (terpri *error-output*)
     (finish-output *error-output*)
     1)))

;;;; THE END ;;;;
