;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               add-paths
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Prepends new elements to a colon-separated list of elemnts.
;;;;    Typically: add-paths "$PATH" "/new/path" "/other/new/path"
;;;;
;;;;SYNOPSIS
;;;;    PATH="$(add-paths "$PATH" "$newpath" ...)"
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

;; add-path d1:d0 d2 d3 --> d3:d2:d1:d0 ; but only for new paths.
;; add-path d1:d0 d0 d3 --> d3:d1:d0

(defun split (separator string)
  (delete ""
	  (loop
	   :for start = 0 :then (and next (1+ next))
	   :for next = (and start (position separator string :start start))
	   :while start
	   :collect (subseq string start next))
	  :test (function string=)))

(defun main (arguments)
  (let ((old-paths (split #\: (pop arguments)))
        (new-paths (reverse arguments)))
    (if (endp old-paths)
        (setf old-paths new-paths)
        (dolist (new-path new-paths)
          (unless (member new-path old-paths :test (function string=))
	        (push new-path old-paths))))
    (format t "~{~A~^:~}~%" old-paths))
  0)

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
