;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               one-of
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Choose an argument randomly.
;;;;
;;;;    With a given probability, will choose several of them with the
;;;;    given probability.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-11-14 <PJB> Translated from bash.
;;;;    2011-03-20 <PJB> Added -c option.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2010 - 2011
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
(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "0.1.2")

(defpackage "COM.INFORMATIMAGO.RANDOM"
  (:use "COMMON-LISP" "SCRIPT")
  (:documentation "
Choose an argument randomly.
"))
(in-package "COM.INFORMATIMAGO.RANDOM")

(setf *debug* t)
(setf *random-state* (make-random-state t))

(define-option ("version" "-V" "--version") ()
  "Report the version of this script."
  (format t "~A ~A~%" *program-name* *program-version*))


(defvar *probability* nil)
(defvar *count*       nil)
(defvar *items*  '())


(define-option ("-p" "--probability") (probability)
  "
PROBABILITY is a number between 0.0 and 1.0.

Prints a random selection of the remaining arguments, each having
probability PROBABILITY of being selected (which means that a mean
of PROBABILITY*{nb of arguments} arguments are printed).
"
  (setf *probability*
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (argument  probability))
            (let ((probability (read-from-string probability)))
              (assert (realp probability)
                      (probability)
                      "The probability must be a real number, not ~S" argument)
              (assert (<= 0.0 probability 1.0)
                      (probability)
                      "The probability must be a real number between 0.0 and 1.0,  ~A is too ~:[big~;small~]."
                      probability (< probability 0.0))
              probability)))))


(define-option ("-c" "--count") (count)
  "
COUNT is an integer.

Prints COUNT items selected at random from the remaining arguments.

Each item is taken only once, if COUNT is greater than the number of
remaining arguments, then they're all printed in random order.
"
  (setf *count*
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (argument count))
            (let ((count (read-from-string count)))
              (assert (integerp count)
                      (count)
                      "The COUNT must be an integer, not ~S" argument)
              (assert (< 0 COUNT)
                      (count)
                      "The COUNT must be a strictly-positive number,  ~A is too ~:[big~;small~]."
                      count (<= count 0))
              count)))))



(parse-options ext:*args*
               nil ; (lambda () (call-option-function "help" '()))
               (lambda (name arguments)
                 (push name *items*)
                 arguments))

(defun one-of (sequence)
  (elt sequence (random (length sequence))))

(defun shuffle (sequence)
  (let ((result (coerce sequence 'vector)))
    (loop
       :for i :from (length result) :downto 1
       :do (rotatef (aref result (1- i)) (aref result (random i))))
    result))

(defun shuffle-list (list)
  (do* ((list (copy-list list) list)
        (length  (length list) (1- length))
        (current list (cdr list)))
       ((< 2 length) list)
   (rotatef (first current) (elt current (random length)))))


(unless *items*
  (setf *items*
        (mapcar (function namestring)
                (remove-if ; remove backup files and dot-files.
                 (lambda (path)
                   (let ((name (file-namestring path)))
                     (or (char= #\. (aref name 0))
                         (char= #\~ (aref name (1- (length name))))
                         (and (char= #\# (aref name 0))
                              (char= #\# (aref name (1- (length name))))))))
                 (remove-duplicates (append (directory "*") (directory "*.*"))
                                    :test (function equalp))))))

(when *items*
  (cond
    ((and *probability* *count*)
     (error "Specifying a probability and a count are mutually exclusive."))
    (*probability*
     (loop
        :for arg :in *items*
        :when (<= (random 1.0) *probability*)
        :do (write-line arg) (finish-output)))
    (*count*
     (loop
        :for arg :across (subseq (shuffle *items*) 0 (min *count* (length *items*)))
        :do (write-line arg) (finish-output)))
    (t ; a single one
     (write-line (one-of *items*))
     (finish-output))))


;; #-testing-script
;; (ext:exit (main ext:*args*))

(ext:exit 0)
;;;; THE END ;;;;
