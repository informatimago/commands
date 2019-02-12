;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               new-password
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Unix Command Line
;;;;DESCRIPTION
;;;;
;;;;    Generate a random password.
;;;;
;;;;    Depends on ~/bin/script.lisp
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2010-01-06 <PJB> Created. (Rewrote from a bit-rotten bash script).
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
(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type nil :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(setf *program-version* "1.0.1")
(setf *random-state* (make-random-state t))

;; (defparameter *index-pathname*
;;   (make-pathname :name "NEWPASSWORD" :type "INDEX" :version NIL :case :common
;;                  :defaults *load-pathname*))


;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

People use those symbols in order of exponential decreasing preference
in their passwords:

         !@#*.#&-?>"./)+=~%(;^`[]'>

         @!$*#.-&_

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct options
  (case          :mix-case   :type (member :mix-case :up-case :low-case))
  (no-special-p  nil         :type boolean)
  (no-digit-p    nil         :type boolean)
  (length        8           :type (integer 1)))

(defparameter *options* (make-options))


(define-option ("-l" "--low-case") ()
  "Generate only low-case letters in the password."
  (setf (options-case *options*) :low-case))

(define-option ("-u" "--up-case") ()
  "Generate only up-case letters in the password."
  (setf (options-case *options*) :up-case))

(define-option ("-m" "--mix-case") ()
  "Generate mix-case letters in the password (default)."
  (setf (options-case *options*) :mix-case))

(define-option ("+s" "--no-special") ()
  "Generate a password without any special character."
  (setf (options-no-special-p *options*) t))

(define-option ("+d" "--no-digit") ()
  "Generate a password without any digit character."
  (setf (options-no-digit-p *options*) t))

(define-option ("-L" "--length") (length)
  "Specifies the password length."
  (let ((length (parse-integer length)))
   (check-type length (integer 1))
   (setf (options-length *options*) length)))

;; (define-option ("-I" "--build-trigram-index") (thesaurus-pathname)
;;   "Analyses the thesaurus and save tri-gram statistics for readable
;; password generation."
;;   (build-trigram-index thesaurus-pathname *index-pathname*))

(parse-options ext:*args*)

;; (defparameter *probability-distribution*
;;   (load-probability-distribution *index-pathname*))
;; (setf *random-state* (make-random-state t))
;; (loop
;;    :repeat (options-length *options*)
;;    :do (princ (get-trigram *probability-distribution*))
;;    :finally (terpri))




(defparameter *consonants* "BCDFGHJKLMNPQRSTVWXZ")
(defparameter *vowels*     "AEIOUY")
(defparameter *specials*   "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~")
(defparameter *digits*     "0123456789")

(defun generate-random-syllabe ()
  (format nil "~C~C~:[~;~C~]"
          (aref *consonants* (random (length *consonants*)))
          (aref *vowels*     (random (length *vowels*)))
          (< 0.5 (random 1.0))
          (aref *consonants* (random (length *consonants*)))))

(define-modify-macro concatf (other) concat)

(loop
   :with password = ""
   :while (< (length password) (options-length *options*))
   :do (concatf password (generate-random-syllabe))
   :do (case (random 3)
         ((0)
          (unless (options-no-special-p *options*)
            (concatf password
                     (string (aref *specials* (random (length *specials*)))))))
         ((1)
          (unless (options-no-digit-p *options*)
            (concatf password
                     (string (aref *digits* (random (length *digits*))))))))
   :finally
   (princ (case (options-case *options*)
            ((:low-case) (string-downcase password))
            ((:up-case)  (string-upcase password))
            ((:mix-case) (with-output-to-string (out)
                           (loop :for ch :across password
                              :do (princ (if (zerop (random 2))
                                             (char-upcase ch)
                                             (char-downcase ch))
                                         out))))))
   (terpri))


;; #-testing-script
;; (ext:exit (main ext:*args*))




;; (defun split-sequence-if (predicate sequence &key remove-empty-subseqs)
;;   "
;; RETURN: A list of subsequences split whatever element for which predicates.
;; "
;;   (let ((chunks  '())
;;         (position 0)
;;         (nextpos  0)
;;         (strlen   (length sequence)))
;;     (loop :while (< position strlen) :do
;;        (loop :while (and (< nextpos strlen)
;;                          (not (funcall predicate (elt sequence nextpos)))) :do
;;           (incf nextpos))
;;        (unless (and remove-empty-subseqs (eql position nextpos))
;;          (push (subseq sequence position nextpos) chunks))
;;        (setf position (1+ nextpos)
;;              nextpos  position))
;;     (nreverse chunks)))
;;
;;
;; (defun split-sequence (separators sequence &key remove-empty-subseqs)
;;   "
;; RETURN: A list of subsequences split on the elements in SEPARATORS.
;; "
;;   (let ((chunks  '())
;;         (position 0)
;;         (nextpos  0)
;;         (strlen   (length sequence)))
;;     (loop :while (< position strlen) :do
;;        (loop :while (and (< nextpos strlen)
;;                          (not (position (elt sequence nextpos) separators))) :do
;;           (setq nextpos (1+ nextpos)))
;;        (unless (and remove-empty-subseqs (eql position nextpos))
;;          (push (subseq sequence position nextpos) chunks))
;;        (setf position (1+ nextpos)
;;              nextpos  position))
;;     (nreverse chunks)))
;;
;; (defun dichotomy (vector value compare &key
;;                   (start 0) (end (length vector))
;;                   (key (function identity)))
;;   "
;; PRE:	entry is the element to be searched in the table.
;;         (<= start end)
;; RETURN: (values found index order)
;; POST:	(<= start index end)
;;         +-------------------+----------+-------+----------+----------------+
;;         | Case              |  found   | index |  order   |     Error      |
;;         +-------------------+----------+-------+----------+----------------+
;;         | x < a[min]        |   FALSE  |  min  |  less    |      0         |
;;         | a[i] < x < a[i+1] |   FALSE  |   i   |  greater |      0         |
;;         | x = a[i]          |   TRUE   |   i   |  equal   |      0         |
;;         | a[max] < x        |   FALSE  |  max  |  greater |      0         |
;;         +-------------------+----------+-------+----------+----------------+
;; "
;;   (let* ((curmin start)
;;          (curmax end)
;;          (index    (truncate (+ curmin curmax) 2))
;;          (order  (funcall compare value (funcall key (aref vector index)))) )
;;     (loop :while (and (/= 0 order) (/= curmin index)) :do
;;        (if (< order 0)
;;            (setf curmax index)
;;            (setf curmin index))
;;        (setf index (truncate (+ curmin curmax) 2))
;;        (setf order (funcall compare value (funcall key (aref vector index)))))
;;     (when (and (< start index) (< order 0))
;;       (setf order 1)
;;       (decf index))
;;     (assert
;;      (or (< (funcall compare value (funcall key (aref vector index))) 0)
;;          (and (> (funcall compare value (funcall key (aref vector index))) 0)
;;               (or (>= (1+ index) end)
;;                   (< (funcall compare value
;;                               (funcall key (aref vector (1+  index)))) 0)))
;;          (= (funcall compare value (funcall key (aref vector index))) 0)))
;;     (values (= order 0) index order)))
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; (defun build-trigram-index (thesaurus-pathname index-pathname)
;;  (let ((tri (make-array '(26 26 26) :element-type 'float :initial-element 0.0))
;;        (total 0.0)
;;        (nzc 0))
;;    (labels ((letter (code)
;;               (cond
;;                 ((<= #.(char-code #\a) code #.(char-code #\z))
;;                  (- code #.(char-code #\a)))
;;                 ((<= #.(char-code #\A) code #.(char-code #\Z))
;;                  (- code #.(char-code #\A)))
;;                 (t nil)))
;;             (register (a b c)
;;               (let ((i (letter a))
;;                     (j (letter b))
;;                     (k (letter c)))
;;                 (when (and i j k)
;;                   (incf (aref tri i j k))
;;                   (incf total))))
;;             (average ()
;;               (loop
;;                  :for i :from 0 :below (array-total-size tri)
;;                  :for prob = (row-major-aref tri i)
;;                  :initially (setf nzc 0)
;;                  :do (when (plusp prob)
;;                        (setf (row-major-aref tri i) (/ prob total))
;;                        (incf nzc)))))
;;
;;      (with-open-file (words thesaurus-pathname :element-type '(unsigned-byte 8))
;;        (loop
;;           :with buffer = (make-array 65536 :element-type '(unsigned-byte 8)
;;                                      :adjustable t
;;                                      :fill-pointer 65536)
;;           :while (plusp (read-sequence buffer words))
;;           :do (loop
;;                  :for i :from 0 :below (- (length buffer) 3)
;;                  :do (register (aref buffer i)
;;                                (aref buffer (+ 1 i))
;;                                (aref buffer (+ 2 i))))))
;;      (average))
;;
;;    (with-open-file (index index-pathname
;;                           :direction :output
;;                           :if-does-not-exist :create
;;                           :if-exists :supersede)
;;      (print tri index))))
;;
;;
;; (defun load-probability-distribution (index-pathname)
;;   (let* ((tri (with-open-file (index index-pathname)
;;                 (read index)))
;;          (nzc (loop
;;                  :for i :from 0 :below (array-total-size tri)
;;                  :for prob = (row-major-aref tri i)
;;                  :when (plusp prob) :count 1))
;;          (distribution (make-array nzc))
;;          (d -1))
;;     (loop :for i :from 0 :below 26 :do
;;        (loop :for j :from 0 :below 26 :do
;;           (loop :for k :from 0 :below 26 :do
;;              (let ((prob (aref tri i j k)))
;;                (when (plusp prob)
;;                  (setf (aref distribution (incf d)) (vector prob i j k)))))))
;;     (let ((distribution (sort distribution (function >) :key (lambda (v) (aref v 0)))))
;;       (loop
;;          :with s = 0.0
;;          :for i :from 0 :below (length distribution)
;;          :for trigram = (aref distribution i)
;;          :do (let ((p (aref trigram 0)))
;;                (setf (aref trigram 0) (incf s p)))
;;          ;; :finally (print s)
;;          )
;;       distribution)))
;;
;; (defun get-trigram (distribution)
;;   (let ((trigram (aref distribution
;;                        (nth-value 1 (dichotomy distribution (random 1.0)
;;                                                (lambda (a b)
;;                                                  (cond ((< a b) -1)
;;                                                        ((= a b) 0)
;;                                                        (t       1)))
;;                                                :key (lambda (v) (aref v 0)))))))
;;     (format nil "~C~C~C"
;;             (code-char (+ #.(char-code #\a) (aref trigram 1)))
;;             (code-char (+ #.(char-code #\a) (aref trigram 2)))
;;             (code-char (+ #.(char-code #\a) (aref trigram 3))))))

;;;; THE END ;;;;
