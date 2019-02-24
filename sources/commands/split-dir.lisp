;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               split-dir
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Make hard-links from a source directory to a sequence of destination
;;;;    directories such as the disk usage of each destination directory is
;;;;    less than a maximum size.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-00 <PJB> Merged  spread-files with split-dir.
;;;;    2004-11-15 <PJB> Created split-dir
;;;;    2003-10-22 <PJB> Created spread-files
;;;;BUGS
;;;;    Lack an option to use symbolic links, or to copy instead of hard links.
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 2004 - 2004
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


(defun print-help ()
  (format t "~&~A usage:~%" *program-name*)
  (format t "    ~A [-h|--help]~%" *program-name*)
  (format t "    ~A [--preview] max-size src-dir dst-dir~%" *program-name*)
  (format t "max-size :== number ['K'|'M'|'G']~%"))

(defun disk-usage (dir-upath)
  (let ((directories
          (append (directory (concatenate 'string dir-upath "/*"))
                  (directory (concatenate 'string dir-upath "/*/")))))
    (unless directories
      (error "Cannot find files or directories in ~A/" dir-upath))
    (with-open-stream
        (in (uiop:run-program
             (list* "du" "-s" "-k" directories)
             :input nil
             :output :stream
             :wait nil))
      (loop
        with result = '()
        for line = (read-line in nil nil)
        while line
        do (let ((tab (position (code-char 9) line)))
             (when tab
               (push (cons (subseq line (1+ tab))
                           (parse-integer (subseq line 0 tab))) result)))
        finally (return result)))))

(defun factor (string)
  (let ((pos (position (char string (1- (length string))) "KMGkmg")))
    (if pos
        (expt 1024 (mod pos 3))
        1)))

(defun mantissa (string)
  (parse-integer
   string
   :junk-allowed nil
   :end  (when (position (char string (1- (length string))) "KMGkmg")
           (1- (length string)))))

(defun test-mantissa-factor ()
  (dolist (test '(("123"   123 1)
                  ("123K"  123 1)
                  ("123k"  123 1)
                  ("123M"  123 1024)
                  ("123m"  123 1024)
                  ("123G"  123 1048576)
                  ("123g"  123 1048576)
                  ("123x"  :error)
                  ("123 k" 123 1)
                  ("x"     :error)
                  ("k"     :error)
                  ))
    (let ((m nil) (f nil))
      (handler-case
          (setf m (mantissa (first test))
                f (factor (first test)))
        (error () (setf m :error f :error)))
      (if (eq :error (second test))
          (assert (eq :error m))
          (assert (and (= m (second test))
                       (= f (third test))))))))

(defun split-groups (list max-size)
  (do ((list list (cdr list))
       (groups '())
       (chunk  '())
       (chunk-size 0))
      ((null list)
       (when chunk (push (nreverse chunk) groups))
       (nreverse groups))
    (when (< max-size (+ chunk-size (cdr (car list))))
      (push (nreverse chunk) groups)
      (setf chunk '()
            chunk-size 0))
    (push (car list) chunk)
    (incf chunk-size (cdr (car list)))))

(defun hardlink (src dst) (uiop:run-program "ln" :arguments (list "-f" src dst)))
(defun symlink  (src dst) (uiop:run-program "ln"
                            :arguments (list "-f" "-s" src dst)))
(defun move     (src dst) (uiop:run-program "mv" :arguments (list src dst)))

(defun contains-option-p (arg options) (member arg options :test (function string=)))

(defun main (arguments)
  (let ((max-size nil)
        (src-dir  nil)
        (dst-dir  nil)
        (preview  nil)
        (collate  (function symlink)))
    (dolist (arg arguments)
      (cond
        ((contains-option-p arg '("-h" "--help"))     (print-help) (return-from main))
        ((contains-option-p arg '("-p" "--preview"))  (setf preview t))
        ((contains-option-p arg '("-L" "--symlink"))  (setf collate (function symlink)))
        ((contains-option-p arg '("-M" "--move"))     (setf collate (function move)))
        ((contains-option-p arg '("-H" "--hardlink")) (setf collate (function hardlink)))
        ((char= #\- (char arg 0)) (error "Invalid option ~A" arg))
        ((null max-size) (setf max-size (* (mantissa arg) (factor arg))))
        ((null src-dir)  (setf src-dir arg))
        ((null dst-dir)  (setf dst-dir arg))
        (t (error "Too many arguments."))))
    (when (or (null max-size) (null src-dir) (null dst-dir))
      (error "Missing arguments."))
    (let* ((du (sort (disk-usage src-dir) (lambda (a b) (string< (car a) (car b)))))
           (groups (split-groups du max-size)))
      (if preview
          (loop
            :for group :in groups
            :for n :from 0
            :do (format t "~&Group #~2D  size=~D~%" n
                        (reduce (function +)
                                (mapcar (function cdr) group)))
                (dolist (item group)
                  (format t "    ~A~%" (car item))))
          (loop
            :for group :in groups
            :for n :from 0
            :for dst := (format nil "~A-~3,'0D/" dst-dir n)
            :do (format t "~&Group #~2D  size=~D --> ~A~%" n
                        (reduce (function +)
                                (mapcar (function cdr) group))
                        dst)
                (ensure-directories-exist dst)
                (dolist (item group) (funcall collate (car item) dst))))))
  ex-ok)

;;; (load "package:com;informatimago;clisp;disk")
;;; (defun spread-files (max-size  file-list  dir-list)
;;;   (ensure-directories-exist (format nil "~A/toto" (car dir-list)))
;;;   (do ((file-list file-list (cdr file-list))
;;;        (dir-list  dir-list))
;;;       ((or (null file-list) (null dir-list)))
;;;     (format t "~S~%   in ~S~2%" (car file-list) (car dir-list))
;;;     (hardlink (car file-list) (car dir-list))
;;;     (let ((size (* 1024 (com.informatimago.clisp.disk:du (car dir-list)))))
;;;       (when (<= max-size size)
;;;         (delete-file
;;;          (format nil "~A/~A" (car dir-list)
;;;                  (subseq (namestring (car file-list))
;;;                          (position  (character "/")
;;;                                     (namestring (car file-list)) :from-end t))))
;;;         (pop dir-list)
;;;         (when dir-list
;;;           (ensure-directories-exist (format nil "~A/toto" (car dir-list)))
;;;           (hardlink (car file-list) (car dir-list))) )))
;;;   );;spread-files
;;;
;;;
;;; (when nil
;;;
;;; (setq file-list
;;;       (directory
;;;        "/data/mirrors/publications.ai.mit.edu/ai-publications/0-499/*.gz")
;;;       dummy nil)
;;;
;;; (setq file-list
;;;       (directory
;;;        "/data/mirrors/publications.ai.mit.edu/ai-publications/1000-1499/*.gz")
;;;       dummy nil)
;;;
;;; (setq file-list
;;;       (directory
;;;        "/data/mirrors/publications.ai.mit.edu/ai-publications/500-999/*.gz")
;;;       dummy nil)
;;;
;;;
;;; (setq sorted-file-list
;;;       (sort file-list
;;;             (lambda (a b)
;;;               (string-lessp
;;;                (subseq (namestring a) (position (character "-") (namestring a) :from-end t))
;;;                (subseq (namestring b) (position (character "-") (namestring b) :from-end t)))))
;;;       dummy nil)
;;;
;;;
;;; (spread-files (* 680 1024 1024)
;;;               sorted-file-list
;;;               '( "/data/mirrors/publications.ai.mit.edu/ai-publications/a"
;;;                  "/data/mirrors/publications.ai.mit.edu/ai-publications/b"
;;;                  "/data/mirrors/publications.ai.mit.edu/ai-publications/c"
;;;                  "/data/mirrors/publications.ai.mit.edu/ai-publications/d"
;;;                  "/data/mirrors/publications.ai.mit.edu/ai-publications/e"
;;;                  "/data/mirrors/publications.ai.mit.edu/ai-publications/f"
;;;                  ))
;;;
;;; )
