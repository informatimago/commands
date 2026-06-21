;; -*- mode:lisp;coding:utf-8 -*-

(command :use-systems (:com.informatimago.common-lisp.cesarum)
         :use-packages ("COMMON-LISP"
                        "SCRIPT"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
         :documentation "
Remove duplicate files listed on standard input.

Each input line must have the form KEY<two spaces>PATH, as produced for
example by:

    md5sum * | dedup

Lines sharing the same KEY denote duplicate files.  For each group of
duplicates the first PATH read is kept and the others are removed.  By
default the removed files are moved to the Trash rather than deleted; see
--trash, --delete and --empty-trash.  Malformed lines (without the
two-space separator) are ignored.")

(options "dedup"
         (trash-disposal-options))

(defun stdin-duplicate-groups ()
  "Reads lines of the form KEY<two spaces>PATH from *STANDARD-INPUT* and returns
the groups (lists of PATHs sharing a KEY) that have more than one member.  Within
each group the first PATH read comes first (and is the one kept).  Lines without
the two-space separator are ignored."
  (let ((table (make-hash-table :test 'equal))
        (order '()))
    (dolist (line (stream-to-string-list *standard-input*))
      (let ((p (search "  " line)))
        (when p
          (let ((key  (subseq line 0 p))
                (path (subseq line (+ p 2))))
            (unless (gethash key table) (push key order))
            (setf (gethash key table)
                  (nconc (gethash key table) (list path)))))))
    (loop :for key :in (nreverse order)
          :for group = (gethash key table)
          :when (rest group) :collect group)))

(defun main (arguments)
  (parse-options *command* arguments)
  (dispose-duplicates-command (function stdin-duplicate-groups)))

;;;; THE END ;;;;
