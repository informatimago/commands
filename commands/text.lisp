;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;***************************************************************************
;;;;FILE:              text
;;;;LANGUAGE:          Common Lisp (clisp)
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This script filters out non ASCII characters.
;;;     The only control code that is left is the newline.
;;;;USAGE
;;;;    text < bytes > text
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2009-12-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2009 - 2009
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
;;;;***************************************************************************

(defun main (arguments)
  (declare (ignore arguments))
  (unwind-protect
       (progn
         #+clisp
         (setf (stream-element-type *standard-input*)   '(unsigned-byte 8)
               (stream-element-type *standard-output*)  '(unsigned-byte 8))
         (loop
           :with inbuffer  = (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0)
           :with outbuffer = (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 0
                                               :fill-pointer 0)
           :do (let ((pos (read-sequence inbuffer *standard-input*)))
                 (when (zerop pos) (loop-finish))
                 (setf (fill-pointer outbuffer) (array-dimension outbuffer 0))
                 (loop
                   :with j := 0
                   :for i :from 0 :below pos
                   :for code := (aref inbuffer i)
                   :do (cond
                         ((= code 10)   (setf (aref outbuffer j) code) (incf j))
                         ((< code 32))
                         ((<= 127 code))
                         (t  (setf (aref outbuffer j) code) (incf j)))
                   :finally (setf (fill-pointer outbuffer) j))
                 (write-sequence outbuffer *standard-output*))))

    #+clisp
    (setf (stream-element-type *standard-input*)   'character
          (stream-element-type *standard-output*)  'character))
  ex-ok)
