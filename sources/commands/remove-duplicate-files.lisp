;;;; -*- mode:lisp; coding:utf-8 -*-

(command :documentation "
Remove duplicate files from the given directories.

For each DIRECTORY argument, the regular files it directly contains are
compared by their md5 checksum (computed with md5sum(1)); within each group
of identical files the first one (in directory order) is kept and the others
are removed.  By default removed files are moved to the Trash rather than
deleted; see --trash, --delete and --empty-trash.

Usage: remove-duplicate-files [options] DIRECTORY...")

(defvar *directories* '()
  "The directory arguments to scan, in order.")

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

(defun md5-duplicate-groups (directories)
  "Returns all duplicate-file groups found among the regular files directly
contained in each of the DIRECTORIES."
  (loop :for base :in directories
        :for dir = (pathname-as-directory base)
        :do (when *verbose* (format *trace-output* "; scanning ~A~%" (namestring dir)))
        :nconc (files-md5-groups (uiop:directory-files dir))))

(options "remove-duplicate-files"
         (trash-disposal-options))

(defun main (arguments)
  (setf *directories* '())
  (parse-options *command* arguments
                 (lambda () nil)
                 (lambda (argument rest)
                   (push argument *directories*)
                   rest))
  (setf *directories* (nreverse *directories*))
  (when (and (null *directories*) (not *empty-trash-requested*))
    (print-command-help *command*)
    (exit ex-usage))
  (dispose-duplicates-command
   (lambda () (md5-duplicate-groups *directories*))))

;;;; THE END ;;;;
