;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               split-merge
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Separate a merged file with conflict into the two unmerged originals.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-11-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
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

(defparameter *program-version* "0.1.0")

;;;------------------------------------------------------------------------

(defvar *verbose* nil
  "Whether the underlying commands run should be written to stdout.")

(defun verbose (control-string &rest arguments)
  (when *verbose*
    (format t "~?" control-string arguments)
    (finish-output)))


;;;---------------------------------------------------------------------

;;;------------------------------------------------------------

(defun compose-sexp (functions var)
  (if (null functions)
      var
      (list (car functions) (compose-sexp (cdr functions) var))))

(defmacro COMPOSE (&rest functions)
  `(lambda (x) ,(compose-sexp functions 'x)))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun prefixp (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun suffixp (suffix string)
  (and (<= (length suffix) (length string))
       (string= suffix string :start2 (- (length string) (length suffix)))))

;;;------------------------------------------------------------

;;;------------------------------------------------------------

(defparameter *left-prefix*  "<<<<<<< ")
(defparameter *right-prefix* "=======")
(defparameter *both-prefix*  ">>>>>>> ")
(defparameter *external-format*  #+clisp charset:iso-8859-1 #-clisp :iso-8859-1)

(defun read-file (path)
  (with-open-file (input path :external-format *external-format*)
    (verbose "Reading file ~S~%" (pathname input))
    (loop :for line := (read-line input nil) :while line :collect line)))

(defun write-file (path lines)
  (with-open-file (output path :external-format *external-format*
                               :direction :output
                               :if-does-not-exist :create
                               :if-exists :supersede)
    (verbose "Writing file ~S~%" (pathname output))
    (loop :for line :in lines :do (write-line line output))))

(defun extract-stem (which old-stem new-stem)
  (or old-stem
      (progn
        (verbose "~@(~A~) stem is ~S~%" which new-stem)
        new-stem)))

(defun split-merge (lines)
  (loop
    :with state := :both
    :with hunk-count := 0
    :with left-stem  := nil  :with left-lines  := '()
    :with right-stem := nil  :with right-lines := '()
    :for line :in lines
    :do (ecase state
          ((:both)  (if (prefixp "<<<<<<< " line)
                        (progn
                          (setf left-stem (extract-stem :left  left-stem (subseq line (length *left-prefix*))))
                          (incf hunk-count)
                          (setf state :left))
                        (progn
                          (push line left-lines)
                          (push line right-lines))))
          ((:left)  (if (string= *right-prefix* line)
                        (setf state :right)
                        (push line left-lines)))
          ((:right) (if (prefixp *both-prefix* line)
                        (progn
                          (setf right-stem (extract-stem :right right-stem (subseq line (length *both-prefix*))))
                          (setf state :both))
                        (push line right-lines))))
    :finally (progn
               (unless (eq state :both)
                 (error "Unfinished hunk (~A)" state))
               (verbose "Read ~D hunk~:*~P~%" hunk-count)
               (return (values (nreverse left-lines)  left-stem
                               (nreverse right-lines) right-stem)))))

(defun compose-pathname (base stem)
  (let ((base (pathname base)))
    (make-pathname :name (concatenate 'string (pathname-name base) "-" stem) :defaults base)))

(defun split-merge-file (input-path left-path right-path)
  (multiple-value-bind (left-lines left-stem right-lines right-stem) (split-merge (read-file input-path))
    (if (and left-stem  (plusp (length left-stem))
             right-stem (plusp (length right-stem)))
        (progn
          (write-file (or left-path  (compose-pathname input-path left-stem))  left-lines)
          (write-file (or right-path (compose-pathname input-path right-stem)) right-lines))
        (error "Merged file didn't specify both stems (~S ~S)" left-stem right-stem))))


;;;------------------------------------------------------------

;;;------------------------------------------------------------

(defvar *left-path* nil)
(defvar *right-path* nil)

(command :options (list*

                   (option ("version" "-V" "--version") ()
                     "Report the version of this script."
                     (format t "~A ~A~%" *program-name* *program-version*))

                   (option ("verbose" "-v" "--verbose") ()
                     "Report writes the underlying commands that are run."
                     (setf *verbose* t))

                   (option ("left" "-l" "--left") (path)
                     "Specifies the path of the left output file."
                     (setf *left-path* path))

                   (option ("right" "-l" "--right") (path)
                     "Specifies the path of the right output file."
                     (setf *right-path* path))

                   (help-option)
                   (bash-completion-options)))




;; (defun completion-station-prefix (prefix)
;;   (format t "~(~{~A~%~}~)"  (remove-if-not (lambda (key)
;;                                              (and (<= (length prefix) (length key))
;;                                                   (string= prefix key :end2 (length prefix))))
;;                                            (get-radio-station-names)))
;;   (finish-output))
;;
;; (defun completion-all-stations ()
;;   (format t "~(~{~A~%~}~)" (get-radio-station-names))
;;   (finish-output))
;;
;; (setf *bash-completion-hook*
;;       (lambda (index words)
;;         (if index
;;             (completion-station-prefix (elt words index))
;;             (completion-all-stations))
;;         nil))

(defvar *input-path* nil)

(defun main (arguments)
  (parse-options *command* arguments
                 (lambda ()
                   (call-option-function *command* "help" '()))
                 (lambda (input-path arguments)
                   (setf *input-path* input-path)
                   arguments))
  (split-merge-file *input-path* *left-path* *right-path*)
  ex-ok)


;;;; THE END ;;;;
