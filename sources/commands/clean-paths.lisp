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

(command :use-systems (:com.informatimago.common-lisp
                       :split-sequence
                       :cffi
                       :cl-ppcre)
         :use-packages ("COMMON-LISP"
                        "SCRIPT"
                        "CFFI"
                        "SPLIT-SEQUENCE"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
                        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
         :documentation  "
This scripts builds an isomorphe directory hierarchy with the names
cleaned up (removing most special and non ASCII characters).
")

(in-package "COMMAND.CLEAN-PATHS")


(defconstant s-ifmt  #o0170000)
(defconstant s-ifdir  #o040000)
(defconstant s-ifchr  #o020000)
(defconstant s-ifblk  #o060000)
(defconstant s-ifreg  #o100000)
(defconstant s-ififo  #o010000)
(defconstant s-iflnk  #o120000)
(defconstant s-ifsock #o140000)

(defconstant s-isuid  #o004000)
(defconstant s-isgid  #o002000)
(defconstant s-isvtx  #o001000)

(define-symbol-macro s-iread  s-irusr)
(define-symbol-macro s-iwrite s-iwusr)
(define-symbol-macro s-iexec  s-ixusr)

(defconstant s-irusr  #o000400)
(defconstant s-iwusr  #o000200)
(defconstant s-ixusr  #o000100)
(defconstant s-irwxu  (logior s-irusr s-iwusr s-ixusr))
(defconstant s-irgrp  #o000040)
(defconstant s-iwgrp  #o000020)
(defconstant s-ixgrp  #o000010)
(defconstant s-irwxg  (logior s-irgrp s-iwgrp s-ixgrp))
(defconstant s-iroth  #o000004)
(defconstant s-iwoth  #o000002)
(defconstant s-ixoth  #o000001)
(defconstant s-irwxo  (logior s-iroth s-iwoth s-ixoth))

(defmacro s-isdir  (m) `(= (logand ,m s-ifmt) s-ifdir))
(defmacro s-ischr  (m) `(= (logand ,m s-ifmt) s-ifchr))
(defmacro s-isblk  (m) `(= (logand ,m s-ifmt) s-ifblk))
(defmacro s-isreg  (m) `(= (logand ,m s-ifmt) s-ifreg))
(defmacro s-isfifo (m) `(= (logand ,m s-ifmt) s-iffifo))
(defmacro s-islnk  (m) `(= (logand ,m s-ifmt) s-iflnk))
(defmacro s-issock (m) `(= (logand ,m s-ifmt) s-ifsock))


#+64-bit-target
(progn
  (defctype dev_t             :int32)
  (defctype ino_t             :int64)
  (defctype mode_t            :uint16)
  (defctype nlink_t           :uint16)
  (defctype uid_t             :int32)
  (defctype gid_t             :int32)
  (defctype off_t             :int64)
  (defctype quad_t            :int64)
  (defctype u_long            :uint64)
  (defctype blkcnt_t          :uint64)
  (defctype blksize_t         :uint32)
  (defctype uint32_t          :uint32)
  (defctype int32_t           :int32)
  (defctype int64_t           :int64)
  (defctype __darwin_time_t   :int64)
  (defctype long              :int64))

#-64-bit-target
(progn
  (defctype dev_t             :int32)
  (defctype ino_t             :int64)
  (defctype mode_t            :uint16)
  (defctype nlink_t           :uint16)
  (defctype uid_t             :int32)
  (defctype gid_t             :int32)
  (defctype off_t             :int64)
  (defctype quad_t            :int64)
  (defctype u_long            :uint64)
  (defctype blkcnt_t          :uint64)
  (defctype blksize_t         :uint32)
  (defctype uint32_t          :uint32)
  (defctype int32_t           :int32)
  (defctype int64_t           :int64)
  (defctype __darwin_time_t   :int64)
  (defctype long              :int64))


(defcstruct timespec
  (tv_sec  __darwin_time_t)
  (tv_nsec            long))

(defcstruct stat
  ;; when _DARWIN_FEATURE_64_BIT_INODE is defined
  (dev            dev_t)             ; ID of device containing file
  (mode           mode_t)            ; Mode of file (see below)
  (nlink          nlink_t)           ; Number of hard links
  (ino            ino_t)             ; File serial number
  (uid            uid_t)             ; User ID of the file
  (gid            gid_t)             ; Group ID of the file
  (rdev           dev_t)             ; Device ID
  (atimespec      (:struct timespec)) ; time of last access
  (mtimespec      (:struct timespec)) ; time of last data modification
  (ctimespec      (:struct timespec)) ; time of last status change
  (birthtimespec  (:struct timespec)) ; time of file creation(birth)
  (size           off_t)              ; file size, in bytes
  (blocks         blkcnt_t)           ; blocks allocated for file
  (blksize        blksize_t)          ; optimal blocksize for I/O
  (flags          uint32_t)           ; user defined flags for file
  (gen            uint32_t)           ; file generation number
  (lspare         int32_t)            ; RESERVED: DO NOT USE!
  (qspare0        int64_t)            ; RESERVED: DO NOT USE!
  (qspare1        int64_t))

#-64-bit-target
(defcstruct stat
  ;; when _DARWIN_FEATURE_64_BIT_INODE is defined
  (dev            dev_t)             ; ID of device containing file
  (ino            ino_t)             ; File serial number
  (mode           mode_t)            ; Mode of file (see below)
  (nlink          nlink_t)           ; Number of hard links
  (uid            uid_t)             ; User ID of the file
  (gid            gid_t)             ; Group ID of the file
  (rdev           dev_t)             ; Device ID
  (atimespec      (:struct timespec)) ; time of last access
  (mtimespec      (:struct timespec)) ; time of last data modification
  (ctimespec      (:struct timespec)) ; time of last status change
  (size           off_t)              ; file size, in bytes
  (blocks         quad_t)             ; blocks allocated for file
  (blksize        u_long)             ; optimal blocksize for I/O
  (flags          u_long)             ; user defined flags for file
  (gen            u_long))           ; file generation number


(defcfun stat :int
  (path :string)
  (stat (:pointer (:struct stat))))


#-64-bit-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant MAXNAMLEN+1 256))

#-64-bit-target
(defcstruct dirent
  (ino ino_t)                           ; file number of entry
  (reclen :uint16)                      ; length of this record
  (type   :uint8)                       ; file type, see below
  (namlen :uint8)                       ; length of string in d_name
  (name   :char :count #.MAXNAMLEN+1)) ; name must be no longer than this


#+64-bit-target
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant MAXNAMLEN+1 1024))

#+64-bit-target
(defcstruct dirent
  (ino     ino_t)          ; file number of entry
  (seekoff :uint64)        ; seek offset (optional, used by servers)
  (reclen  :uint16)        ; length of this record
  (namlen  :uint16)        ; length of string in d_name
  (type    :uint8)         ; file type, see below
  (name    :char :count #.MAXNAMLEN+1)) ; name must be no longer than this

;; (foreign-slot-value ptr 'point 'x)

(defcfun opendir :pointer                    (dirpath :string))
(defcfun readdir (:pointer (:struct dirent)) (dir :pointer))
(defcfun closedir :int                       (dir :pointer))
(defcfun getcwd :string)

;; (let ((p (opendir "/tmp")))
;;   (let ((d (progn (readdir p) (readdir p) (readdir p))))
;;     (list (dirent-ino d)
;;           (dirent-seekoff d)
;;           (dirent-reclen d)
;;           (dirent-namlen d)
;;           (dirent-name d))))



;; File types
(defconstant DT_UNKNOWN       0)
(defconstant DT_FIFO          1)
(defconstant DT_CHR           2)
(defconstant DT_DIR           4)
(defconstant DT_BLK           6)
(defconstant DT_REG           8)
(defconstant DT_LNK          10)
(defconstant DT_SOCK         12)
(defconstant DT_WHT          14)

;; Convert between stat structure types and directory types.
(defun IFTODT (mode)    (ash (logand mode #o170000) -12))
(defun DTTOIF (dirtype) (ash dirtype 12))



(defparameter *program-version* "1.0.2")

(defun memq (element list)
  (member element list :test (function eq)))

(defun file-directory-p (path)
  "
RETURN:     Whether PATH is the path of an existing directory.
"
  (handler-case
      (ql-impl-util:probe-directory
       (if (char= (char path (1- (length path)))
                  (character "/"))
           path
           (concatenate 'string path "/")))
    (error () nil)))


(defun file-exists-p (path)
  (or (file-directory-p path)
      (probe-file path)))

(defun mode-to-string (mode)
  (format nil "~A~A~A~A~A~A~A~A~A~A"
          (char "?pc?d?b?-?l?s???" (truncate (logand mode s-ifmt) 4096))
          (if (zerop (logand mode s-irusr)) "-" "r")
          (if (zerop (logand mode s-iwusr)) "-" "w")
          (if (zerop (logand mode s-isuid))
              (if (zerop (logand mode s-ixusr)) "S" "s")
              (if (zerop (logand mode s-ixusr)) "-" "x"))
          (if (zerop (logand mode s-irgrp)) "-" "r")
          (if (zerop (logand mode s-iwgrp)) "-" "w")
          (if (zerop (logand mode s-isuid))
              (if (zerop (logand mode s-ixgrp)) "S" "s")
              (if (zerop (logand mode s-ixgrp)) "-" "x"))
          (if (zerop (logand mode s-iroth)) "-" "r")
          (if (zerop (logand mode s-iwoth)) "-" "w")
          (if (zerop (logand mode s-isuid))
              (if (zerop (logand mode s-ixoth)) "T" "t")
              (if (zerop (logand mode s-ixoth)) "-" "x"))))


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
  (with-foreign-object (sbuf '(:struct stat))
    (let ((res (stat filepath sbuf)))
      (if (/= 0 res)
          nil
          (list
           ;;  0. t for directory, string (name linked to) for symbolic link,
           ;;     or nil.
           (cond
             ((s-isdir (stat-mode sbuf)) t)
             ((s-islnk (stat-mode sbuf)) "")
             (t nil))
           ;;  1. Number of links to file.
           (stat-nlink sbuf)
           ;;  2. File uid.
           (stat-uid sbuf)
           ;;  3. File gid.
           (stat-gid sbuf)
           ;;  4. Last access time, as an integer.
           (stat-atime sbuf)
           ;;  5. Last modification time, likewise.
           (stat-mtime sbuf)
           ;;  6. Last status change time, likewise.
           (stat-ctime sbuf)
           ;;  7. Size in bytes.
           (stat-size sbuf)
           ;;  8. File modes, as a string of ten letters or dashes as in ls -l.
           (mode-to-string (stat-mode sbuf))
           ;;  9. t iff file's gid would change if file were deleted and recreated.
           ;; TODO: iff file's gid would change if file were deleted and recreated.
           t
           ;; 10. inode number.
           (stat-ino sbuf)
           ;; 11. Device number.
           (stat-dev sbuf)  )))))


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
  (check-type path string)
  (if (char= (character "/") (char path 0))
      path
      (namestring (format nil "~A/~A" (getcwd) path))))


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
        (dir     (opendir directory)))
    (unwind-protect
        (do* ((entry (readdir dir) (readdir dir))
              (files '()))
            ((null entry) (if nosort files (sort files (function string<=))))
          (when (or (not match) (cl-ppcre:scan match (dirent-name entry)))
            (script:pmessage " ~S~%dirent-name: ~S~%"
                             entry (dirent-name entry))
            (if (string= "" (dirent-name entry))
              (script:perror "There's a bug with READDIR directory = ~S" directory)
              (push (concatenate 'string dirpath (dirent-name entry)) files))))
      (closedir dir))))


(defun find-file-with-inode (directory inode)
  "
RETURN: The path to a file found under `directory' whose inode is `inode',
        or nil if none found.
"
  (car
   (split-sequence
    (uiop:run-program (format nil "find ~A -inum ~A -print"
                              (script:shell-quote-argument directory)
                              inode)
                      :output :stream
                      :wait nil)
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
    (copy-file src-path dst-path :if-exists :supersede)))

(defun cp-make-directory (path &optional parents)
  (cp-path-register path)
  (if *print-commands*
    (format t "/bin/mkdir ~:[~;-p~] ~A~%" parents (script:shell-quote-argument path))
    (ensure-directories-exist (format nil "~A/foo.txt" path))))


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
          (let ((dot (position #\. name :from-end t)))
            (if dot
                (let* ((dash (position #\- name :end dot :from-end t))
                       (root      (subseq name 0 (or dash dot)))
                       (extension (subseq name
                                          (if dash
                                              (1+ dash)
                                              dot)
                                          dot)))
                  (unique-name dir (format nil "~A-~D~A" root 0 extension)
                               root 0 extension))
                (unique-name dir (format nil "~A-~D~A" name 0 "") name 0 ""))))
      name))

(defun clean-character-p (ch)
  (or (alphanumericp ch) (find ch "._-")))

(defun name-is-clean-p (name)
  (every (function clean-character-p) name))

(defun clean-name (name)
  (format nil "~(~{~A~^-~}~)"
          (split-sequence-if (complement (function clean-character-p)) name
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
    (error "First argument (~s) should be the path to the source directory."
           srcdir))
  (unless (file-directory-p srcdir)
    (error "Second argument (~s) should be the path to the source directory."
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

;;;; THE END ;;;;

