;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               merge
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Merge the files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-06-28 <PJB> Created.
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
;;;;**************************************************************************

(defpackage "MERGE"
  (:use "COMMON-LISP")
  (:shadow "FILE-STREAM" "MERGE"))
(in-package "MERGE")


(defstruct file
  stream
  line)


(defun file-open (path)
  "
DO:     Open the text file at PATH and read the first line.
RETURN: The FILE structured.
"
  (let ((result (make-file :stream (open path))))
    (file-read-line result)
    result))


(defun file-read-line (file)
  "
RETURN: The line read, or NIL upon EOF.
"
  (setf (file-line file) (read-line (file-stream file) nil nil)))


(defun nsort (vector test &key (key (function identity) keyp))
  "
RETURN: VECTOR
DO:     Same as CL:SORT, but returns VECTOR itself, keeping
        the other attributes of the vector as the fill-pointer, etc.
"
  (let ((sorted (if keyp
                    (sort vector test :key key)
                    (sort vector test))))
    (unless (eq sorted vector)
      (replace vector sorted))
    vector))


(defun insert (item vector test &key (key (function identity)))
  "
DO:     Modify VECTOR, shifting elements smaller than ITEM (as by TEST)
        from 1 below (length vector) down by one, and insert ITEM in
        VECTOR.
PRE:    (subseq VECTOR 1) is sorted.
POST:   VECTOR is sorted.
RETURN: VECTOR
"
  (loop
     :for i :from 1 :below (length vector)
     :while (funcall test (funcall key (aref vector i)) (funcall key item))
     :do (setf (aref vector (1- i)) (aref vector i))
     :finally (setf (aref vector (1- i)) item) (return vector)))


(defun vpop (vector)
  "

"
  (replace vector vector :start1 0 :start2 1)
  (decf (fill-pointer vector))
  vector)


(defun merge (inpaths)
  (let* ((data  (remove-if-not (function file-line) ; skip empty files.
                               (mapcar (function file-open) inpaths)))
         (len   (length data))
         (files (nsort (make-array len :fill-pointer len :initial-contents data)
                       (function string<=)
                       :key (function file-line))))
    (loop
       :while (plusp (length files))
       :do (let ((smallest (aref files 0)))
             (princ (file-line smallest)) (terpri)
             (if (file-read-line smallest)
                 (progn
                   (insert smallest files
                           (function string<=) :key (function file-line))
                   (nsort  files
                           (function string<=) :key (function file-line)))
                 (vpop files))))))


(when ext:*args*
  (merge ext:*args*))

;;;; THE END ;;;;
