;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               group-files
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     CLI
;;;;DESCRIPTION
;;;;
;;;;    Group files by name.
;;;;
;;;;    Example:
;;;;       group-files the-ventures the-surfaris the-surf-coasters the-chantays dario-moreno/compilation-sorted
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2009-07-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 2009 - 2009
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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro redirecting-stdout-to-stderr (&body body)
  (let ((verror  (gensym))
        (voutput (gensym)))
   `(let* ((,verror  nil)
           (,voutput (with-output-to-string (stream)
                       (let ((*standard-output* stream)
                             (*error-output*    stream)
                             (*trace-output*    stream))
                         (handler-case (progn ,@body)
                           (error (err) (setf ,verror err)))))))
      (when ,verror
        (terpri *error-output*)
        (princ ,voutput *error-output*)
        (terpri *error-output*)
        (princ ,verror *error-output*)
        (terpri *error-output*)
        (terpri *error-output*)
        #-testing-script (ext:exit 1)))))


;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
(redirecting-stdout-to-stderr
 (let ((*load-verbose* nil)
       (*compile-verbose* nil))
   (load (make-pathname :name ".clisprc" :type "lisp"
                        :defaults (user-homedir-pathname)))
   ;; (setf *features* (delete :testing-script *features*))
   ))
;; (redirecting-stdout-to-stderr
;;  (asdf:oos 'asdf:load-op :split-sequence)
;;  (asdf:oos 'asdf:load-op :cl-ppcre))


;; (cd "/home/pjb/music/")
(dolist (class (mapcar
                (lambda (class) (mapcar (function cdr) class))
                (remove-if
                 (lambda (class) (= 1 (length class)))
                 (COM.INFORMATIMAGO.COMMON-LISP.LIST:EQUIVALENCE-CLASSES
                  (mapcar
                   (lambda (path)
                     (cons (let* ((name  (namestring path))
                                  (slash (position #\/ name :from-end t))
                                  (dot   (position #\. name :start slash))
                                  (live  (search "--live" name)))
                             (subseq name (+ 4 slash) (or live dot)))
                           path))
                   (mapcan
                    (lambda (dir) (copy-seq (directory (format nil "~A/**/*.mp3" dir))))
                    ext:*args*))
                  :key (function car)
                  :test (function equalp)))))
  (dolist (file class)
    (princ file)
    (terpri)))


;; find the-* -name \*.mp3|while read f ; do echo $(echo $f | sed -e 's^.*/[0-9][0-9]-\([^/]*\)\.mp3$^\1^'|sed -e 's%--live%%')  $f ; done|sort
;;;; THE END ;;;;
