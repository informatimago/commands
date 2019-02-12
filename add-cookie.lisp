;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               addcookie
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Add a cookie to a cookie file.
;;;;
;;;;SYNOPSIS
;;;;    addcookie [cookie-file] < cookie-text
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-22 <PJB> Added this header.
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

(defparameter *cookie-file* "/data/cookies/bopcs.cookies")

(defun main (arguments)

  (case (length arguments)
    ((0))
    ((1) (setf *cookie-file* (first arguments)))
    (otherwise
     (format t "~&Usage: ~A [ cookie-file ] < cookie-data ~&"
             (file-namestring *load-pathname*))
     (format t "Default cookie file is ~A~&"
             *cookie-file*)
     (uiop:quit 1)))

  (handler-case
      (with-open-file (out *cookie-file*
                           :direction :output
                           :if-does-not-exist :error
                           :if-exists :append)
        (loop
          :for line = (read-line *standard-input* nil nil)
          :while line
          :do (format out "~A~%" line)
          :finally  (format out "~%#~%")))
    (error (err) (format t "~%~A~%" err)))

  0)

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
