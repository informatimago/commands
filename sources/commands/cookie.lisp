;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               cookie
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This program search a cookie in the cookie files and print it.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-10-03 <PJB> Use system fortune files when available.
;;;;    2004-12-17 <PJB> Created (converted from cookie.c)
;;;;    2003-12-01 <PJB> Some changes.
;;;;    1993-09-08 <PJB> Implemented the lookup of the file "cookie.files".
;;;;    1993-03-28 <PJB> Began updating to lookup the file "cookie.files" before
;;;;                     using the hard-coded files.
;;;;    1990-12-20 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;
;;;;    Copyright Pascal Bourguignon 1990 - 2016
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


(defparameter *cookie-environment-variable-name* "COOKIE_FILES")

(defparameter *default-table-path* "/data/cookies/ALL.files")
(defparameter *default-files*
  '("/data/cookies/fortune.cookies"
    "/data/cookies/mit.cookies"
    "/data/cookies/cmirh.cookies"
    "/data/cookies/litp.cookies"
    "/data/cookies/kenny-tilton.cookies"
    "/home/pjb/pjb.cookies"))


(defun configure-cookie-files ()
  (setf *default-table-path* nil
        *default-files*
        (remove-duplicates (append
                            (mapcan (lambda (dir)
                                      (set-difference (ignore-errors (directory (merge-pathnames "*" dir)))
                                                      (append (ignore-errors (directory (merge-pathnames "*.dat" dir)))
                                                              (ignore-errors (directory (merge-pathnames "*.u8" dir)))
                                                              (ignore-errors (directory (merge-pathnames "*~" dir))))
                                                      :test (function equal)))
                                    (list "/usr/share/games/fortune/"
                                          "/opt/local/share/games/fortune/"))
                            (ignore-errors (directory "/data/cookies/*.cookies"))
                            (ignore-errors (directory (merge-pathnames "*.cookies" (user-homedir-pathname)))))
                           :test (function equal))))

(defun load-file-names (table-path)
  (and table-path
       (with-open-file (input table-path :direction :input :if-does-not-exist nil)
         (when input
           (loop for line = (read-line input nil nil)
                 for path = (and line (string-trim " " line))
                 while line
                 when (plusp (length path))
                   collect path)))))


(defun cookie-separator-p (ch)
  (or (char= #\# ch)
      (char= #\% ch)))

(defun cookie-from-file (file)
  (with-open-file (input file :direction :input :if-does-not-exist :error)
    (loop repeat 3 do
      (loop repeat 3
            until (file-position input (random (file-length input))))
      (let* ((line (and
                    (loop for line = (read-line input nil nil)
                          while (and line
                                     (or (zerop (length line))
                                         (not (cookie-separator-p (char line 0)))))
                          finally (return line))
                    (loop for line = (read-line input nil nil)
                          while (and line
                                     (or (zerop (length line))
                                         (cookie-separator-p (char line 0))))
                          finally (return line))))
             (cookie (list line)))
        (when line
          (loop for line = (read-line input nil nil)
                while (and line (or (zerop (length line))
                                    (not (cookie-separator-p (char line 0)))))
                do (push line cookie)
                finally (when line
                          (dolist (line (nreverse cookie))
                            (princ line) (terpri))
                          (return-from cookie-from-file))))))))

(defun list-files ()
  (flet ((explain-value (value name)
           (format t "~A is ~S~%" name value)
           value)
         (explain-load-file-names (path)
           (if path
               (progn
                 (format t "Trying to load cookie file list from file ~A~%" path)
                 (load-file-names path))
               (progn
                 (format t "No file list file.~%")
                 (load-file-names path))))
         (explain-cookie-files (files)
           (if files
               (progn
                 (format t "Cookie file list contains:~%")
                 (dolist (file files)
                   (format t "~:[Does not exist~;              ~] ~A~%"
                           (probe-file file) file)))
               (format t "Cookie file list is empty.~%"))))
    (explain-cookie-files
     (or (explain-load-file-names
          (or (explain-value (getenv *cookie-environment-variable-name*)
                             (format nil "The environment variable ~A"
                                     *cookie-environment-variable-name*))
              (explain-value *default-table-path* '*default-table-path*)))
         (explain-value *default-files* '*default-files*)))))



(defun starts-with-option-p (options arguments)
  (if (atom options)
      (member options arguments :test (function string-equal))
      (some (lambda (option) (member option arguments :test (function string-equal)))
            options)))

(defun usage ()
  (format t "~A usage:~%~
             ~&    ~:*~A [-h|--help|-l|--list-files] \\~%~
             ~&    ~VA [-f|--file cookie-file]~%" *program-name* (length *program-name*) ""))

(defun main (argv)
  (setf *random-state* (make-random-state t))
  (cond
    ((starts-with-option-p '("--help" "-h") argv)
     (usage))
    ((starts-with-option-p '("--list-files" "-l") argv)
     (configure-cookie-files)
     (list-files))
    (t
     (let ((file (starts-with-option-p '("--file" "-f") argv)))
       (format t "~&")
       (if file
           (cookie-from-file (second file))
           (progn
             (configure-cookie-files)
             (let* ((total-size 0)
                    (files
                      (mapcan (lambda (file)
                                (with-open-file (stream file :direction :input
                                                             :if-does-not-exist nil)
                                  (when stream
                                    (let ((size (file-length stream)))
                                      (incf total-size size)
                                      (list (cons size file))))))
                              (or (load-file-names
                                   (or (getenv *cookie-environment-variable-name*)
                                       *default-table-path*))
                                  *default-files*)))
                    (arrow (if (zerop total-size)
                               (progn (error "~A: not a good cookie file! ~
                                       (This is not a cookie).~%"
                                             *program-name*)
                                      (return-from main 1))
                               (random total-size))))
               (cookie-from-file (loop with total = 0
                                       for file in files
                                       while (< (+ total (car file)) arrow)
                                       do (incf total (car file))
                                       finally (return (cdr file))))))))))
  ex-ok)

;; (print (configure-cookie-files))

;;;; THE END ;;;;
