;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               substitute
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A strange substitution:
;;;;
;;;;    'ab*c' 'xyz'
;;;;    dacdddddd     -->   dxydddddd
;;;;    dabcddddd     -->   dxyzddddd
;;;;    dabbbcddd     -->   dxyzxyddd
;;;;    dabbbbbcd     -->   dxyzxyzxd
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-10-30 <PJB> Added this header.
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



(defun non-empty-string-p (value)
  (and (stringp value)
       (string> value "")))


(defun substitute-expression (text regexp substitution)
  "
DO:      Modifies text, replacing all parts matching regexp with
         the substitution strings, possibly repeated.
RETURN:  A string containing the repeated substitution string.
"
  (loop
     :with start-search = 0
     :for match = (regexp:regexp-exec regexp text :start start-search)
     :while match
     :do (let* ((match-start  (regexp:match-start match))
                (match-end    (regexp:match-end match))
                (match-length (-  match-end match-start))
                (substitution (loop :while (< (length substitution) match-length)
                                 :do (setf substitution (concatenate 'string substitution substitution))
                                 :finally (return substitution))))
           (setf start-search match-end)
           (replace text substitution :start1 match-start :end1 match-end))
     :finally (return substitution)))



(defmacro check (default-program-name usage &rest clauses)
  (let ((messages (gensym)))
    `(let ((,messages '()))
       ,@(mapcar (lambda (clause)
                   (destructuring-bind (assertion error-message) clause
                     `(unless ,assertion (push ,error-message ,messages))))
                 clauses)
       (when ,messages
         (format *error-output* "~&Error~P:~{~%  ~A~}" (length ,messages) ,messages)
         (format *error-output* "~&Usage:~%   ~A ~A~2%"
                 (or *load-pathname* ,default-program-name)
                 ,usage)))))


(defun main (args)
  (destructuring-bind (&optional input-filename output-filename regexp substitution &rest others) args

    (check "substitute-expression" "$inputfile $outputfile $regexp $substitution"
           ((null others)                        "Too many arguments")
           ((non-empty-string-p substitution)    "Missing substitution string")
           ((non-empty-string-p regexp)          "Missing regular expression")
           ((non-empty-string-p output-filename) "Missing output file argument")
           ((non-empty-string-p input-filename)  "Missing input file argument"))

    (let ((regexp (regexp:regexp-compile regexp)))
      (with-open-file (input-stream input-filename)
        (with-open-file (output-stream output-filename
                                       :direction :output
                                       :if-exists :supersede)
          (loop
             :for current-line = (read-line input-stream nil)
             :while current-line
             :do (setf substitution (substitute-expression current-line regexp substitution))
                 (write-line current-line output-stream)))))))


(main ext:*args*)

