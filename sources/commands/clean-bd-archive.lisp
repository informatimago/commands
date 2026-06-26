;; -*- mode:lisp;coding:utf-8 -*-

(command :version "1.0.2"
         :documentation "
Dispose of duplicate files inside a bd-archive tree.

For the BASE directory argument, each of its immediate subdirectories is
scanned and the regular files it directly contains are compared by their md5
checksum (computed with md5sum(1)); within each group of identical files the
first one (in directory order) is kept and the others are disposed of.  By
default disposed files are moved to the Trash rather than deleted; see --trash,
--delete and --empty-trash.

Usage: clean-bd-archive [options] BASE")

(defvar *base* nil
  "The bd-archive base directory whose subdirectories are cleaned.")

(defun files-md5-groups (files)
  "Returns the groups (lists) of FILES that share an md5 checksum, keeping only
the groups with more than one member.  Within each group the files keep their
order in FILES, so the first one is the one DISPOSE-OF-DUPLICATES keeps.  The
checksums are computed by md5sum(1), invoked with the files as arguments (no
shell), and matched back to FILES by position so that file names with spaces or
other special characters are handled correctly."
  (when files
    (let ((table (make-hash-table :test 'equal))
          (order '()))
      (with-input-from-string
          (out (uiop:run-program (list* "md5sum" (mapcar (function namestring) files))
                                 :output :string
                                 :ignore-error-status t))
        (loop :for file :in files
              :for line  = (read-line out nil nil)
              :while line
              :for end   = (or (position #\Space line) (length line))
              :for hash  = (subseq line 0 end)
              :do (unless (gethash hash table) (push hash order))
                  (setf (gethash hash table)
                        (nconc (gethash hash table) (list file)))))
      (loop :for hash :in (nreverse order)
            :for group = (gethash hash table)
            :when (rest group) :collect group))))

(defun bd-archive-duplicate-groups (base)
  "Returns all duplicate-file groups found among the regular files directly
contained in each immediate subdirectory of BASE."
  (loop :for dir :in (uiop:subdirectories (pathname-as-directory base))
        :do (when *verbose* (format *trace-output* "; processing ~A~%" (namestring dir)))
        :nconc (files-md5-groups (uiop:directory-files dir))))

(options "clean-bd-archive"
         (trash-disposal-options))

(defun main (arguments)
  (setf *base* nil)
  (let ((operands '()))
    (parse-options *command* arguments
                   (lambda () nil)
                   (lambda (argument rest)
                     (push argument operands)
                     rest))
    (setf operands (nreverse operands))
    (when (and (null operands) (not *empty-trash-requested*))
      (print-command-help *command*)
      (exit ex-usage))
    (setf *base* (first operands))
    (dispose-duplicates-command
     (lambda () (and *base* (bd-archive-duplicate-groups *base*))))))

;;;; THE END ;;;;
