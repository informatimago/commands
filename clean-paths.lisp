;; -*- mode:lisp;coding:utf-8 -*-
(defun main (arguments)
  (declare (ignore arguments))
  (error "com.informatimago.clisp.susv3 needs works to switch to 64-bit libraries…"))

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;

#|

;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               clean-paths
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     Common-Lisp
;;;;DESCRIPTION
;;;;
;;;;    This scripts builds an isomorphe directory hierarchy with the names
;;;;    cleaned up (removing most special and non ASCII characters).
;;;;
;;;;    A second entry point issues mkdir & ln commands to reproduce
;;;;    a target directory hierarchy from a source directory hierarchy
;;;;    on the same file system (using the inode as file identification).
;;;;
;;;;    Entry points: clean-paths
;;;;                  reproduce-changes
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-03-28 <PJB> Converted from elisp to Common-Lisp.
;;;;    2003-02-08 <PJB> Merged bash script in here and use emacs-script.
;;;;    2002-09-16 <PJB> Creation.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;
;;;;    Copyright Pascal J. Bourguignon 1992 - 2003
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    Lesser General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Lesser General Public
;;;;    License along with this library; if not, write to the Free Software
;;;;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
;;;;
;;;;*****************************************************************************



(in-package "COMMON-LISP-USER")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (make-pathname :name "SCRIPT" :type "LISP" :version nil :case :common :defaults *load-pathname*)))
(eval-when (:compile-toplevel :load-toplevel :execute)
 (use-package "SCRIPT"))
(defparameter *program-version* "1.0.2")
(eval-when (:compile-toplevel :load-toplevel :execute)
 (load (merge-pathnames (make-pathname :directory '(:relative "QUICKLISP")
                                       :name "SETUP" :type "LISP" :version NIL
                                       :case :common :defaults (user-homedir-pathname))
                        (user-homedir-pathname) nil)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (without-output
      (ql:quickload :com.informatimago.common-lisp)
    (ql:quickload :com.informatimago.clisp)
    (ql:quickload :com.informatimago.susv3)
    (ql:quickload :split-sequence)))
(eval-when (:compile-toplevel :load-toplevel :execute)
 (com.informatimago.common-lisp.cesarum.package:add-nickname
  "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PACKAGE" "PACKAGE")
 (package:add-nickname "COM.INFORMATIMAGO.CLISP.SUSV3"  "SUSV3")
 (package:add-nickname "COM.INFORMATIMAGO.SUSV3.DIRENT" "DIRENT"))

(defpackage "COM.INFORMATIMAGO.CLISP.CLEAN-PATH"
  (:nicknames "CLEAN-PATH")
  (:documentation "
This scripts builds an isomorphe directory hierarchy with the names
cleaned up (removing most special and non ASCII characters).
")
  (:use "COMMON-LISP")
  (:use "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "REMOTE" "ADD-REMOTE" "MAIN"))
(in-package "COM.INFORMATIMAGO.CLISP.CLEAN-PATH")


(defun memq (element list)
  (member element list :test (function eq)))


(defun file-directory-p (path)
  "
RETURN:     Whether PATH is the path of an existing directory.
"
  (handler-case
      (ext:probe-directory (if (char= (char path (1- (length path)))
                                      (character "/"))
                             path (concatenate 'string path "/")))
    (error () nil)))



(defun file-exists-p (path)
  (or (file-directory-p path)
      (probe-file path)))



(defun mode-to-string (mode)
  (format nil "~A~A~A~A~A~A~A~A~A~A"
          (char "?pc?d?b?-?l?s???" (truncate (logand mode susv3:s-ifmt) 4096))
          (if (zerop (logand mode susv3:s-irusr)) "-" "r")
          (if (zerop (logand mode susv3:s-iwusr)) "-" "w")
          (if (zerop (logand mode susv3:s-isuid))
              (if (zerop (logand mode susv3:s-ixusr)) "S" "s")
              (if (zerop (logand mode susv3:s-ixusr)) "-" "x"))
          (if (zerop (logand mode susv3:s-irgrp)) "-" "r")
          (if (zerop (logand mode susv3:s-iwgrp)) "-" "w")
          (if (zerop (logand mode susv3:s-isuid))
              (if (zerop (logand mode susv3:s-ixgrp)) "S" "s")
              (if (zerop (logand mode susv3:s-ixgrp)) "-" "x"))
          (if (zerop (logand mode susv3:s-iroth)) "-" "r")
          (if (zerop (logand mode susv3:s-iwoth)) "-" "w")
          (if (zerop (logand mode susv3:s-isuid))
              (if (zerop (logand mode susv3:s-ixoth)) "T" "t")
              (if (zerop (logand mode susv3:s-ixoth)) "-" "x"))))

(defun file-attributes (filepath)
  "
RETURN:    A list of attributes of file FILEPATH.
           Value is NIL if specified file cannot be opened.
           If file does not exist, returns NIL.

Otherwise, list elements are:
 0. t for directory, string (name linked to) for symbolic link, or nil.
 1. Number of links to file.
 2. File uid.
 3. File gid.
 4. Last access time, as an integers.
 5. Last modification time, likewise.
 6. Last status change time, likewise.
 7. Size in bytes.
 8. File modes, as a string of ten letters or dashes as in ls -l.
 9. t iff file's gid would change if file were deleted and recreated.
10. inode number.
11. Device number.

IMPLEMENTATION: Field 9 is not implemented and always T.
                Symbolic link handling is not implemented.
"
  (multiple-value-bind (res sbuf)  (susv3:stat filepath)
    (if (/= 0 res)
      nil
      (list
       ;;  0. t for directory, string (name linked to) for symbolic link,
       ;;     or nil.
       (cond
        ((susv3:s-isdir (susv3:stat-mode sbuf)) t)
        ((susv3:s-islnk (susv3:stat-mode sbuf)) "")
        (t nil))
       ;;  1. Number of links to file.
       (susv3:stat-nlink sbuf)
       ;;  2. File uid.
       (susv3:stat-uid sbuf)
       ;;  3. File gid.
       (susv3:stat-gid sbuf)
       ;;  4. Last access time, as an integer.
       (susv3:stat-atime sbuf)
       ;;  5. Last modification time, likewise.
       (susv3:stat-mtime sbuf)
       ;;  6. Last status change time, likewise.
       (susv3:stat-ctime sbuf)
       ;;  7. Size in bytes.
       (susv3:stat-size sbuf)
       ;;  8. File modes, as a string of ten letters or dashes as in ls -l.
       (mode-to-string (susv3:stat-mode sbuf))
       ;;  9. t iff file's gid would change if file were deleted and recreated.
       ;; TODO: iff file's gid would change if file were deleted and recreated.
       t
       ;; 10. inode number.
       (susv3:stat-ino sbuf)
       ;; 11. Device number.
       (susv3:stat-dev sbuf)  ))))


(defun file-inode (path)
  "
RETURN: The inode of the file at `path'.
"
  (nth 10 (file-attributes path)))


(defun file-symlink-p (path)
  "
RETURN: Whether PATH refers to a symbolic-link.
"
  (stringp (nth 0 (file-attributes path))))


(defun file-symbolic-link (path)
  "
PRE:    (file-symlink-p path)
"
  (nth 0 (file-attributes path)))



(defun append-slash-to-path (path)
  "
RETURN:  If PATH terminates with a '/' then PATH
         else a string with a '/' appended to PATH.
"
  (unless (stringp path) (setq path (namestring path)))
  (if (char= (character "/") (char path (1- (length path))))
    path
    (concatenate 'string path "/")))


(defun absolute-path (path)
  "
RETURN: A string containing an absolute path leading to PATH.
"
  (if (char= (character "/") (char path 0))
    path
    (concatenate 'string (namestring (ext:cd)) path)))


(defun directory-files (directory &optional full match nosort)
  "
IMPLEMENTATION: The options are not implemented.

Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
"
  (let ((dirpath (if full (append-slash-to-path (absolute-path directory)) ""))
        (dir     (dirent:opendir directory)))
    (unwind-protect
        (do* ((entry (dirent:readdir dir) (dirent:readdir dir))
              (files '()))
            ((null entry) (if nosort files (sort files (function string<=))))
          (when (or (not match) (regexp:match match (dirent:dirent-name entry)))
            (script:pmessage "dirent: ~S~%dirent-name: ~S~%"
                             entry (dirent:dirent-name entry))
            (if (string= "" (dirent:dirent-name entry))
              (script:perror "There's a bug with DIRENT:READDIR directory = ~S" directory)
              (push (concatenate 'string dirpath (dirent:dirent-name entry)) files))))
      (dirent:closedir dir))))


(defun find-file-with-inode (directory inode)
  "
RETURN: The path to a file found under `directory' whose inode is `inode',
        or nil if none found.
"
  (car
   (split-sequence:split-sequence
    (ext:run-program "find"
      :arguments (list (script:shell-quote-argument directory)
                       "-inum" inode "-print")
      :output :stream)
    #\newline)))


(defun print-usage ()
  (format t
    "~A usage:~%~%~
    ~:*~A [--symbolic-links|-L|--hard-links|-H|--copy|-C] \\~%~
    ~V<~> [--print-commands|-n] [--help|-h] \\~%~
    ~:*~V<~> $srcdir $dstdir~%~%"
    script:*program-name* (length script:*program-name*)))


(defvar *print-commands* nil
  "Whether commands should be printed instead of executed.")

(defvar cp-paths (make-hash-table :size 127)
  "An table of paths of new items.")


(defun cp-path-register (path)
  (setf (gethash path cp-paths) path))

(defun cp-path-exists-p (path)
  (gethash path cp-paths))



(defun cp-hard-link (src-path dst-path)
  "
DOES:    add an hard link from the item at src-path to dst-path.
"
  (cp-path-register dst-path)
  (if *print-commands*
    (format t "/bin/ln ~A ~A~%"
            (script:shell-quote-argument src-path)
            (script:shell-quote-argument dst-path))
    (script:execute  "/bin/ln"  src-path dst-path)))


(defun cp-symbolic-link (src-path dst-path)
  "
DOES:    add a symbolic link from the item at src-path to dst-path.
"
  (cp-path-register dst-path)
  (if *print-commands*
    (format t "/bin/ln -s ~A ~A~%"
            (script:shell-quote-argument src-path)
            (script:shell-quote-argument dst-path))
    (script:make-symbolic-link src-path dst-path)))


(defun cp-copy-file (src-path dst-path)
  "
DOES:    copy the item at src-path to dst-path.
"
  (cp-path-register dst-path)
  (if *print-commands*
    (format t "/bin/cp -p ~A ~A~%"
            (script:shell-quote-argument src-path)
            (script:shell-quote-argument dst-path))
    (script:copy-file src-path dst-path nil t)))


(defun cp-make-directory (path &optional parents)
  (cp-path-register path)
  (if *print-commands*
    (format t "/bin/mkdir ~:[~;-p~] ~A~%" parents (script:shell-quote-argument path))
    (script:make-directory path parents)))




(defun unique-name (dir name &optional root counter  extension)
  "
PRE:    (or (null counter)
            (string= name (format \"%s-%d%s\" root counter extension)))
RETURN: a unique name in the directory dir, formated as root-counter.extension
"
  (if (cp-path-exists-p (concatenate 'string dir "/" name))
    (if counter
      (unique-name dir (format nil "~A-~D~A" root counter extension)
                   root (1+ counter) extension)
      ;; first time: lets split the name.
      (if (regexp:match "^\\(.+\\)\\(\\.[^\\.][^\\.]*\\)$" name)
        (let ((root (regexp:match-string 1 name))
              (extension (regexp:match-string 2 name)))
          (unique-name dir (format nil "~A-~D~A" root 0 extension)
                       root 0 extension))
        (unique-name dir (format nil "~A-~D~A" name 0 "") name 0 "")))
    name))

(defun clean-character-p (ch)
  (or (alphanumericp ch) (find ch "._-")))

(defun name-is-clean-p (name)
  (every (function clean-character-p) name))

(defun clean-name (name)
  (format nil "~(~{~A~^-~}~)"
          (split-sequence:split-sequence-if (complement (function clean-character-p)) name
                                            :remove-empty-subseqs t)))
(trace clean-name)

(defun clean-path-directory (srcdir dstdir copy-action)
  "
DOES:    recursively clean the paths from srcdir into dstdir.
"
  (dolist (item (directory-files srcdir))
    (let ((fitem (concatenate 'string srcdir "/" item))
          cname)
      (when (and (string/= "." item) (string/= ".." item))
        (setq cname (unique-name dstdir
                                 (if (name-is-clean-p item)
                                     item
                                     (clean-name item))))
        (if (file-directory-p fitem)
            ;; a subdirectory
            (let ((srcsub (concatenate 'string srcdir "/" item))
                  (dstsub (concatenate 'string dstdir "/" cname)))
              (cp-make-directory dstsub)
              (clean-path-directory srcsub dstsub copy-action))
            ;; a leaf node.
            (let ((srcitem (concatenate 'string srcdir "/" item))
                  (dstitem (concatenate 'string dstdir "/" cname)))
              (funcall copy-action srcitem dstitem)))))))


(defun last-char (string)
  (aref string (1- (length string))))

(defun clean-paths (srcdir dstdir keys)
  "
DOES:    builds a directory hierarchy dstdir similar to srcdir,
         with file and directory names cleaned (removing spaces and other
         special characters).

keys:    may contain one of:
         :hard-link           The files are hard linked.
         :symbolic-link       The files are symbolically linked. (default)
         :file-copy           The files are copied.
"
  (when (char= #\/ (last-char srcdir))
    (setf srcdir (subseq srcdir 0 (1- (length srcdir)))))
  (when (char= #\/ (last-char dstdir))
    (setf dstdir (subseq dstdir 0 (1- (length dstdir)))))
  (unless (file-directory-p srcdir)
    (error "First argument (~A) should be the path to the source directory." srcdir))
  (when (file-exists-p dstdir)
    (error "Destination path (~A) already exist." dstdir))
  (cp-make-directory dstdir t)
  (let ((copy-action (cond
                       ((memq :hard-link     keys) 'cp-hard-link)
                       ((memq :symbolic-link keys) 'cp-symbolic-link)
                       ((memq :file-copy     keys) 'cp-copy-file)
                       (t                          'cp-symbolic-link))))
    (let ((*print-commands* (memq :print-commands keys)))
      (clean-path-directory srcdir dstdir copy-action))))


(defun reproduce-changes-directory (srcdir dstdir copy-action)
  "
DOES:    scan dstdir and issue commands.
"
  (cp-make-directory dstdir)
  (dolist (item (directory-files dstdir))
    (let ((fitem (concatenate 'string dstdir "/" item)))
      (when (and (string/= "." item) (string/= ".." item))
        (cond
         ((file-directory-p fitem)
          ;; a subdirectory
          (reproduce-changes-directory
           srcdir (concatenate 'string dstdir "/" item) copy-action))
         ((file-symlink-p fitem)
          ;; a symbolic-link
          (cp-symbolic-link (file-symbolic-link fitem) fitem))
         (t
          ;; a leaf node.
          (let* ((dstitem (concatenate 'string dstdir "/" item))
                 (srcitem (find-file-with-inode srcdir (file-inode dstitem))))
            (when srcitem
              (funcall copy-action srcitem dstitem)))))))))


(defun reproduce-changes (srcdir dstdir keys)
  "
DOES:    issues commands to rebuild the directory hierarchy dstdir from srcdir,
         (using the inodes of the files as identifiers).

keys:    may contain one of:
         :hard-link           The files are hard linked.
         :symbolic-link       The files are symbolically linked. (default)
         :file-copy           The files are copied.
"
  (when (char= #\/ (last-char srcdir))
    (setq srcdir (subseq srcdir 0 (1- (length srcdir)))))
  (when (char= #\/ (last-char dstdir))
    (setq dstdir (subseq dstdir 0 (1- (length dstdir)))))
  (unless (file-directory-p srcdir)
    (error "First argument (%s) should be the path to the source directory."
           srcdir))
  (unless (file-directory-p srcdir)
    (error "Second argument (%s) should be the path to the source directory."
           srcdir))
  (let ((copy-action (cond
                       ((memq :hard-link keys)     'cp-hard-link)
                       ((memq :symbolic-link keys) 'cp-symbolic-link)
                       ((memq :file-copy keys)     'cp-copy-file)
                       (t                          'cp-symbolic-link)))
        (*print-commands* t))
    (reproduce-changes-directory srcdir dstdir copy-action)))




(defun main (argv)
  (let ((src nil)
        (dst nil)
        (options ()))
    (format t "~&")
    ;; process arguments
    (dolist (arg argv)
      (cond
       ((member arg '("-L" "--symbolic-links") :test (function string-equal))
        (push :symbolic-link options))
       ((member arg '("-H" "--hard-links")     :test (function string-equal))
        (push :hard-link options))
       ((member arg '("-C" "--copy")           :test (function string-equal))
        (push :file-copy options))
       ((member arg '("-n" "--print-commands") :test (function string-equal))
        (push :print-commands options))
       ((member arg '("-h" "--help")           :test (function string-equal))
        (print-usage)
        (script:exit script:ex-ok))
       (t
        (cond
         ((null src) (setq src arg))
         ((null dst) (setq dst arg))
         (t
          (script:perror "'~A' is superfluous~%"  arg)
          (script:perror "I already got source & destination directories.~%")
          (print-usage)
          (script:exit script:ex-usage)))))
      ;;;(FORMAT t "arg=~W~%src=~W~%dst=~W~%options=~W~%" arg src dst options)
      )
    ;; check if src & dst are present
    (when (null src)
      (script:perror "source directory argument is missing.~%")
      (print-usage)
      (script:exit script:ex-usage))
    (when (not (file-directory-p src))
      (script:perror "invalid source directory '~A'.~%" src)
      (print-usage)
      (script:exit script:ex-dataerr))
    (when (null dst)
      (script:perror "destination directory argument is missing.~%")
      (print-usage)
      (script:exit script:ex-usage))
    ;; execute the command
    (if (string= script:*program-name* "reproduce-changes")
      (progn ;; just output a script to reproduce $dst from $src
        (when (not (file-directory-p dst))
          (script:perror "invalid destination directory '~A'.~%" dst)
          (print-usage)
          (script:exit script:ex-dataerr))
        (reproduce-changes src dst options))
      (progn
        ;; actually produce a cleaned-up $dst from $src
        ;; (or just output a script to this effect
        ;;  if --print-commands  is given).
        (when (file-exists-p dst)
          (script:pmessage "destination directory '~A' already exists.~%" dst)
          (print-usage)
          (script:exit script:ex-dataerr))
        (clean-paths src dst options)))))

#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
|#
