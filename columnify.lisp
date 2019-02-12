#!/usr/local/bin/cl
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               columnify
;;;;LANGUAGE:           text
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Columnify the input.
;;;;
;;;;    By default, columnify as much as possible given the line lengths
;;;;    and the terminal width as given by the environment variable COLUMNS.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-04-19 <PJB>
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2007 - 2007
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
;;;;****************************************************************************


(defvar *width* (or #+clisp (nth-value 1 (screen:with-window (screen:window-size screen:*window*)))
                    (and (uiop:getenv "COLUMNS") (parse-integer (uiop:getenv "COLUMNS")))
                    (block :found
                      (dolist (path (list #+clisp (format nil "/proc/~D/fd/1" (posix:process-id))
                                          "/dev/tty")
                                    nil)
                        (with-open-stream (stty (uiop:run-program (format nil "stty -a < ~S" path)
                                                                  :force-shell t
                                                                  :input :terminal
                                                                  :output :stream))
                          (loop
                            :for line = (read-line stty nil nil)
                            :while line
                            :do (let* ((tag " columns ")
                                       (pos (search tag line)))
                                  (when pos
                                    (return-from :found
                                      (parse-integer line :start (+ pos (length tag))
                                                          :junk-allowed t))))))))
                    80))



(defun columnify (lines width)
  (let* ((colwid (1+ (or (loop :for line :in lines :maximize (length line)) 0)))
         (ncols  (truncate width colwid)))
    (if (<= ncols 1)
        (loop :for line :in lines :do (princ line) (terpri))
        ;; columnify:
        (let* ((nrows     (ceiling (length lines) ncols))
               (cols      (make-array (list ncols nrows) :initial-element ""))
               (longcols  (mod (length lines) ncols))
               (next-line lines))
          (loop
            :for col :from 0 :below ncols
            :do (loop
                  :for row :from 0 :below (- nrows (if (< col longcols) 0 1))
                  :do (setf (aref cols col row) (pop next-line))))
          (loop :for row :from 0 :below nrows
            :do (loop
                  :for col :from 0 :below ncols
                  :do (format t "~VA" colwid (aref cols col row))
                  :finally (format t "~%")))))))


(defun main (arguments)
  (declare (ignore arguments))
  (columnify (loop
               :for line = (read-line *standard-input* nil nil)
               :while line :collect line)
             *width*)
  0)


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
