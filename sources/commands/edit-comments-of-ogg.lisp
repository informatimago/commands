;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:              edit-comments-of-ogg.lisp
;;;;LANGUAGE:          common lisp (clisp)
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    This script helps editing ogg vorbis comments of a set of ogg files.
;;;;    Once comments are edited in separate .inf files, they're written
;;;;    to the ogg files with vorbiscomment.
;;;;USAGE
;;;;    edit-comments-of-ogg --help
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2002-04-04 <PJB> Created.
;;;;    2002-04-14 <PJB> Fine tuned some variable references to handle spaces
;;;;                     in directory and file names.
;;;;    2002-09-20 <PJB> Added l)ast command.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2002
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;*****************************************************************************

(defparameter *program-version* "1.0.2")

(defun stream-to-string-list (stream)
  (loop
    :with line := (read-line stream nil nil)
    :while line
    :collect line :into result
    :do (setf line (read-line stream nil nil))
    :finally (return result)))


(defun copy-stream (src-stream dst-stream)
  (loop
    :with line := (read-line src-stream nil nil)
    :while line
    :do (write-line line dst-stream)))


(defun string-replace (string regexp replace &optional fixedcase literal)
  "
RETURN: a string build from `string' where all matching `regexp'
        are replaced by the `replace' string.
NOTE:   Current implementat accepts only literal pattern as `regexp';
        `fixedcase' and `literal' are ignored.
"
  (declare (ignore fixedcase literal))
  (loop
    :with regexp-length := (length regexp)
    :with result := ""
    :with previous := 0
    :with position := (search regexp string)
    :while position
    :do (setf result (concatenate 'string
                                  result (subseq string previous position) replace)
              previous (+ position regexp-length)
              position (search regexp string :start2 previous))
    :finally (setf result (concatenate 'string
                                       result
                                       (subseq string previous (length string))))
             (return result)))


(defun split-string (string &optional separators)
  "
NOTE:   current implementation only accepts as separators
        a string containing only one character.
"
  (let ((sep (aref separators 0))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)))

    (loop while (< position strlen)
       do
         (loop while (and (< nextpos strlen)
                          (char/= sep (aref string nextpos)))
            do (setf nextpos (1+ nextpos))
              );;loop
         (push (subseq string position nextpos) chunks)
         (setf position (1+ nextpos))
         (setf nextpos  position))
    (nreverse chunks)))


(defun split-name-value (string)
  "
RETURN:  a cons with two substrings of string such as:
         (string= (concat (car res) \"=\" (cdr res)) string)
         and (length (car res)) is minimum.
"
  (let ((position 0)
        (strlen   (length string))
        )
    (loop while (and (< position strlen)
                     (char/= (character "=") (aref string position)))
       do (setf position (1+ position)))
    (if (< position strlen)
        (cons (subseq string 0 position) (subseq string (1+ position) strlen))
        nil)))




(defconstant comext ".inf")

(defconstant fields '(artist album title version tracknumber organization genre
                      description date location copyright));;FIELDS
(defvar fields-names (mapcar 'symbol-name fields))

(defun usage ()
  (format
   t (concatenate
         'string
       "~%"
       "~a~:* usage:~%"
       "~%"
       "    ~a [-h|--help|-e|--edit|-w|--write]... DIR_OR_OGG_FILE... ~%"
       "~%"
       " -e  edits the attribute files of the .ogg found in DIRECTORY.~%"
       " -w  writes the attributes data to the .ogg files. (unfortunately ~%"
       "     this means copying the .ogg file to a new version ~%"
       "     per vorbis-comment).~%"
       "~%")
   *program-name*))


(defvar artist       "")
(defvar album        "")
(defvar title        "")
(defvar version      "")
(defvar tracknumber  "")
(defvar organization "")
(defvar genre        "")
(defvar description  "")
(defvar date         "")
(defvar location     "")
(defvar copyright    "")

(defvar last-artist       "")
(defvar last-album        "")
(defvar last-title        "")
(defvar last-version      "")
(defvar last-tracknumber  "")
(defvar last-organization "")
(defvar last-genre        "")
(defvar last-description  "")
(defvar last-date         "")
(defvar last-location     "")
(defvar last-copyright    "")


(defun display (index max file)
  (format t
          (concatenate
              'string
            "~Cc~%"
            "    INDEX        = ~a/~a~%"
            "    FILE         = ~a~%"
            ""
            "1)  ARTIST       = ~a~%"
            "2)  ALBUM        = ~a~%"
            "3)  TITLE        = ~a~%"
            "4)  VERSION      = ~a~%"
            "5)  TRACKNUMBER  = ~a~%"
            "6)  ORGANIZATION = ~a~%"
            "7)  GENRE        = ~a~%"
            "8)  DESCRIPTION  = ~a~%"
            "9)  DATE         = ~a~%"
            "a)  LOCATION     = ~a~%"
            "b)  COPYRIGHT    = ~a~%"
            )
          (code-char 27)
          index max
          file artist album title version tracknumber organization genre
          description date location copyright))

(defun info-load (txt)
  (if (probe-file txt)

      (dolist (line (with-open-file (stream txt :direction :input)
                      (stream-to-string-list stream)))
        (let* ((nv (split-name-value line))
               (name (car nv))
               (value (cdr nv)))
          (when (member name fields-names :test 'string=)
            (set (intern name) value))
          ))

      ;;else there's no existing .inf file.
      (setf artist       ""
            album        ""
            title        ""
            version      ""
            tracknumber  ""
            organization ""
            genre        ""
            description  ""
            date         ""
            location     ""
            copyright    "")))

(defun info-save (txt)
  (with-open-file (out txt :direction :output :if-exists :supersede)
    (format out
            (concatenate
                'string
              "ARTIST=~a~%"
              "ALBUM=~a~%"
              "TITLE=~a~%"
              "VERSION=~a~%"
              "TRACKNUMBER=~a~%"
              "ORGANIZATION=~a~%"
              "GENRE=~a~%"
              "DESCRIPTION=~a~%"
              "DATE=~a~%"
              "LOCATION=~a~%"
              "COPYRIGHT=~a~%")
            artist album title version tracknumber organization genre
            description date location copyright))
  (setf last-artist           artist)
  (setf last-album            album)
  (setf last-title            title)
  (setf last-version          version)
  (setf last-tracknumber      tracknumber)
  (setf last-organization     organization)
  (setf last-genre            genre)
  (setf last-description      description)
  (setf last-date             date)
  (setf last-location         location)
  (setf last-copyright        copyright))

(defun title (file)
  (when (string= ".ogg" (subseq file (- (length file) 4) (length file)))
    (setf file (subseq file 0 (- (length file) 4))))
  (loop for i from 0 to (1- (length file))
     when (member (aref file i) '( #\- #\_ ) :test 'eq)
     do   (aset file i 32))
  file)

(defun edit (files)
  (let ((index 0)
        (state 'editing)
        (flist (sort
                (stream-to-string-list
                 (uiop:run-program
                  (cons "/usr/bin/find" (append files '("-name" "*.ogg" "-print")))
                  :input     nil
                  :output    :stream
                  :wait nil))
                'string<)))

    (when (= 0 (length flist))
      (format *error-output*
              "~a: I could not find any .ogg file in ~a.~%" *program-name* files)
      (exit ex-dataerr))


    (loop until (eq state 'done) do
         (let* ((fogg (nth index flist))
                (fcom (string-replace fogg ".ogg" comext)))
           (info-load fcom)

           (setf state 'editing)
           (loop while (eq state 'editing) do
                (display index (length flist) fogg)
                (format t "~%")
                (when (< index (1- (length flist)))
                  (format t "n)ext,  "))
                (when (< 0 index)
                  (format t "p)revious,  "))
                (format t (concatenate
                              'string
                            "q)quit,  copy from l)ast,  S)earch,  "
                            "s)et all,  ~%"
                            "or:  digit) set one field.  ? "
                            ))
                (let ((cmd (let ((line (read-line nil "q")))
                             (if (< 0 (length line)) (aref line 0) 0))))

                  (cond

                    ((eq (character '\n) cmd)
                     (info-save fcom)
                     (when (< index (1- (length flist)))
                       (setf index (1+ index)))
                     (setf state 'next))

                    ((eq (character '\p) cmd)
                     (info-save fcom)
                     (when (< 0 index)
                       (setf index (1- index)))
                     (setf state 'next))

                    ((eq (character '\q) cmd)
                     (info-save fcom)
                     (setf state 'done))

                    ((eq (character '\l) cmd)
                     (setf
                      artist         last-artist
                      album          last-album
                      title          last-title
                      version        last-version
                      tracknumber    last-tracknumber
                      organization   last-organization
                      genre          last-genre
                      description    last-description
                      date           last-date
                      location       last-location
                      copyright      last-copyright))

                    ((eq (character '\S) cmd)
                     (info-save fcom)
                     (format t "Search for: ")
                     (let ((pattern (read-line)))
                       (setf index
                             (loop
                                for sindex = (mod (+ 1 index) (length flist))
                                then (mod (+ 1 sindex) (length flist))
                                for fogg = (nth sindex flist)
                                for fcom = (string-replace fogg ".ogg" comext)
                                while (/= sindex index)
                                until (search pattern
                                              (concatenate 'string
                                                fogg         "**"
                                                artist       "**"
                                                album        "**"
                                                title        "**"
                                                version      "**"
                                                tracknumber  "**"
                                                organization "**"
                                                genre        "**"
                                                description  "**"
                                                date         "**"
                                                location     "**"
                                                copyright    "**"))
                                do (info-load fcom)
                                finally (return sindex)))
                       );;let
                     (setf state 'next)
                     )

                    ((eq (character '\s) cmd)

                     (mapc (lambda (sym)
                             (format t "~a=" sym)
                             (set sym (read-line)))
                           fields)
                     )

                    ((eq (character '\1) cmd)
                     (format t "~a=" 'artist)
                     (setf artist (read-line)))

                    ((eq (character '\2) cmd)
                     (format t "~a=" 'album)
                     (setf album (read-line)))

                    ((eq (character '\3) cmd)
                     (format t "~a=" 'title)
                     (setf title (read-line)))

                    ((eq (character '\4) cmd)
                     (format t "~a=" 'version)
                     (setf version (read-line)))

                    ((eq (character '\5) cmd)
                     (format t "~a=" 'tracknumber)
                     (setf tracknumber (read-line)))

                    ((eq (character '\6) cmd)
                     (format t "~a=" 'organization)
                     (setf organization (read-line)))

                    ((eq (character '\7) cmd)
                     (format t "~a=" 'genre)
                     (setf genre (read-line)))

                    ((eq (character '\8) cmd)
                     (format t "~a=" 'description)
                     (setf description (read-line)))

                    ((eq (character '\9) cmd)
                     (format t "~a=" 'date)
                     (setf date (read-line)))

                    ((eq (character '\a) cmd)
                     (format t "~a=" 'location)
                     (setf location (read-line)))

                    ((eq (character '\b) cmd)
                     (format t "~a=" 'copyright)
                     (setf copyright (read-line)))

                    )))))))

(defun commit-comments (files)
  (dolist (fogg (stream-to-string-list
                 (uiop:run-program
                  (cons "/usr/bin/find" (append files '("-name" "*.ogg" "-print")))
                  :input     nil
                  :output    :stream
                  :wait nil)))
    (let ((fcom (string-replace fogg ".ogg" comext)))
      (if (probe-file fcom)
          (progn
            (format t "~a: Writting comments to '~a'...~%" *program-name* fogg)
            (copy-stream (uiop:run-program
                          (list "/usr/local/bin/vorbiscomment" "-w" fogg "-c" fcom)
                          :input  nil
                          :output :stream
                          :wait nil)
                         *standard-output*))
          (format t "~a: Missing '~a'.~%" *program-name* fcom)))))


(defun main (arguments)

  (let ((files '())
        (do_edit nil)
        (do_write nil))

    (dolist (arg arguments)
      (cond

        ((or (string= "-h" arg) (string= "--help" arg))
         (usage)
         (exit ex-ok))

        ((or (string= "-e" arg) (string= "--edit" arg))
         (setf do_edit t))

        ((or (string= "-w" arg) (string= "--write" arg))
         (setf do_write t))

        ((string= (aref arg 0) (character '\-))
         (format *error-output* "~a: invalid argument '~a'.~%" *program-name* arg)
         (usage)
         (exit ex-usage))

        (t
         (push arg files))))

    (when (and (not do_edit) (not do_write))
      (format *error-output* "~a: Nothing to do. Please specify -e or -w. Aborting.~%" *program-name*)
      (usage)
      (exit ex-usage))

    (when (= 0 (length files))
      (format *error-output* "~a: No directory, no file to work on. Aborting.~%" *program-name*)
      (usage)
      (exit ex-usage))


    (when do_edit
      (edit files))

    (when do_write
      (commit-comments files)))

  ex-ok)


(defun m () (main '("-e" ".")))
;;;; THE END ;;;;
