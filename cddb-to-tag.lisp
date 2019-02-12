;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cddb-to-tag
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Reads cddb.txt, rename .flac or .mp3 files and add tags, according
;;;;    to the information found in it.
;;;;
;;;;USAGE
;;;;
;;;;    cddb-to-tag flac-or-mp3-directory/ …
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2017-05-23 <PJB> Created.
;;;;BUGS
;;;;    Doesn't process any option yet. We'd need --help, --dry-run, etc.
;;;;    Does not read cd-info version 0.82 cddb.txt files yet (only cd-info 0.83).
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2017
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

;; (push :testing *features*)

#-testing
(eval-when (:compile-toplevel)
  (let* ((*standard-output* (make-broadcast-stream)))
    (load #P"~/quicklisp/setup.lisp")))
#-testing
(eval-when (:compile-toplevel)
  (let* ((*standard-output* (make-broadcast-stream)))
    (ql:quickload :uiop                                  :verbose nil)
    (ql:quickload :split-sequence                        :verbose nil)
    (ql:quickload :cl-ppcre                              :verbose nil)
    (ql:quickload :com.informatimago.common-lisp.cesarum :verbose nil)))

(defpackage "CDDB-TO-TAG"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export "MAIN"))
(in-package "CDDB-TO-TAG")

(defparameter *dry-run* nil)

;;;
;;; System Exit Codes
;;;

;; From /usr/include/sysexits.h (Linux)
(defconstant _sysexits_h      1)
(defconstant ex-ok            0)
(defconstant ex--base        64)
(defconstant ex-usage        64)
(defconstant ex-dataerr      65)
(defconstant ex-noinput      66)
(defconstant ex-nouser       67)
(defconstant ex-nohost       68)
(defconstant ex-unavailable  69)
(defconstant ex-software     70)
(defconstant ex-oserr        71)
(defconstant ex-osfile       72)
(defconstant ex-cantcreat    73)
(defconstant ex-ioerr        74)
(defconstant ex-tempfail     75)
(defconstant ex-protocol     76)
(defconstant ex-noperm       77)
(defconstant ex-config       78)
(defconstant ex--max         78)


;;;
;;; Tags
;;;

(defparameter *tag-keys*
  '(
    ;; (keyword    VORBIS/FLAC-tagname  mp3-id3v2-frame)
    (:title        "TITLE"              "TIT2")
    (:version      "VERSION"            nil)
    (:album        "ALBUM"              "TALB")
    (:tracknumber  "TRACKNUMBER"        "TRCK")
    (:artist       "ARTIST"             "TCOM")
    (:performer    "PERFORMER"          "TPE1")
    (:copyright    "COPYRIGHT"          "TCOP")
    (:license      "LICENSE"            nil)
    (:organization "ORGANIZATION"       nil)
    (:description  "DESCRIPTION"        "COMM")
    (:genre        "GENRE"              "genre") ;; !!!
    (:date         "DATE"               "TDAT")
    (:location     "LOCATION"           nil)
    (:contact      "CONTACT"            nil)
    (:isrc         "ISRC"               "TSRC")))

;; id3v2 frames:
;;
;; --AENC    Audio encryption
;; --APIC    Attached picture
;; --COMM    Comments
;; --COMR    Commercial frame
;; --ENCR    Encryption method registration
;; --EQUA    Equalization
;; --ETCO    Event timing codes
;; --GEOB    General encapsulated object
;; --GRID    Group identification registration
;; --IPLS    Involved people list
;; --LINK    Linked information
;; --MCDI    Music CD identifier
;; --MLLT    MPEG location lookup table
;; --OWNE    Ownership frame
;; --PRIV    Private frame
;; --PCNT    Play counter
;; --POPM    Popularimeter
;; --POSS    Position synchronisation frame
;; --RBUF    Recommended buffer size
;; --RVAD    Relative volume adjustment
;; --RVRB    Reverb
;; --SYLT    Synchronized lyric/text
;; --SYTC    Synchronized tempo codes
;; --TALB    Album/Movie/Show title
;; --TBPM    BPM (beats per minute)
;; --TCOM    Composer
;; --TCON    Content type
;; --TCOP    Copyright message
;; --TDAT    Date
;; --TDLY    Playlist delay
;; --TENC    Encoded by
;; --TEXT    Lyricist/Text writer
;; --TFLT    File type
;; --TIME    Time
;; --TIT1    Content group description
;; --TIT2    Title/songname/content description
;; --TIT3    Subtitle/Description refinement
;; --TKEY    Initial key
;; --TLAN    Language(s)
;; --TLEN    Length
;; --TMED    Media type
;; --TOAL    Original album/movie/show title
;; --TOFN    Original filename
;; --TOLY    Original lyricist(s)/text writer(s)
;; --TOPE    Original artist(s)/performer(s)
;; --TORY    Original release year
;; --TOWN    File owner/licensee
;; --TPE1    Lead performer(s)/Soloist(s)
;; --TPE2    Band/orchestra/accompaniment
;; --TPE3    Conductor/performer refinement
;; --TPE4    Interpreted, remixed, or otherwise modified by
;; --TPOS    Part of a set
;; --TPUB    Publisher
;; --TRCK    Track number/Position in set
;; --TRDA    Recording dates
;; --TRSN    Internet radio station name
;; --TRSO    Internet radio station owner
;; --TSIZ    Size
;; --TSRC    ISRC (international standard recording code)
;; --TSSE    Software/Hardware and settings used for encoding
;; --TXXX    User defined text information
;; --TYER    Year
;; --UFID    Unique file identifier
;; --USER    Terms of use
;; --USLT    Unsynchronized lyric/text transcription
;; --WCOM    Commercial information
;; --WCOP    Copyright/Legal infromation
;; --WOAF    Official audio file webpage
;; --WOAR    Official artist/performer webpage
;; --WOAS    Official audio source webpage
;; --WORS    Official internet radio station homepage
;; --WPAY    Payment
;; --WPUB    Official publisher webpage
;; --WXXX    User defined URL link


;; TITLE
;;
;;     Track/Work name
;;
;; VERSION
;;
;;     The version field may be used to differentiate multiple versions
;;     of the same track title in a single collection. (e.g. remix info)
;;
;; ALBUM
;;
;;     The collection name to which this track belongs
;;
;; TRACKNUMBER
;;
;;     The track number of this piece if part of a specific larger
;;     collection or album
;;
;; ARTIST
;;
;;     The artist generally considered responsible for the work. In
;;     popular music this is usually the performing band or singer. For
;;     classical music it would be the composer. For an audio book it
;;     would be the author of the original text.
;;
;; PERFORMER
;;
;;     The artist(s) who performed the work. In classical music this
;;     would be the conductor, orchestra, soloists. In an audio book it
;;     would be the actor who did the reading. In popular music this is
;;     typically the same as the ARTIST and is omitted.
;;
;; COPYRIGHT
;;
;;     Copyright attribution, e.g., '2001 Nobody's Band' or '1999 Jack
;;     Moffitt'
;;
;; LICENSE
;;
;;     License information, eg, 'All Rights Reserved', 'Any Use
;;     Permitted', a URL to a license such as a Creative Commons license
;;     ("www.creativecommons.org/blahblah/license.html") or the EFF Open
;;     Audio License ('distributed under the terms of the Open Audio
;;     License. see http://www.eff.org/IP/Open_licenses/eff_oal.html for
;;     details'), etc.
;;
;; ORGANIZATION
;;
;;     Name of the organization producing the track (i.e. the 'record
;;     label')
;;
;; DESCRIPTION
;;
;;     A short text description of the contents
;;
;; GENRE
;;
;;     A short text indication of music genre
;;
;; DATE
;;
;;     Date the track was recorded
;;
;; LOCATION
;;
;;     Location where track was recorded
;;
;; CONTACT
;;
;;     Contact information for the creators or distributors of the
;;     track. This could be a URL, an email address, the physical address
;;     of the producing label.
;;
;; ISRC
;;
;;     ISRC number for the track; see the ISRC intro page for more
;;     information on ISRC numbers. http://isrc.ifpi.org/


(defun tag-flac-file (file disk tags)
  (declare (ignore disk))
  (funcall (if *dry-run*
               (function print)
               (function uiop:run-program))
           (append (list "metaflac")
                   (mapcan (lambda (tag)
                             (let ((flac-tag (second (find (first tag) *tag-keys* :key (function first)))))
                               (when flac-tag
                                 (list
                                  (format nil "--set-tag=~A=~A"
                                          flac-tag
                                          (second tag))))))
                           tags)
                   (list (namestring file)))))

(defun tag-mp3-file  (file disk tags)
  (funcall (if *dry-run*
               (function print)
               (function uiop:run-program))
           (append (list "id3v2")
                   (mapcan (lambda (tag)
                             (let ((frame (third (find (first tag) *tag-keys* :key (function first)))))
                               (when frame
                                 (list (format nil "--~A" frame)
                                       (if (eq :tracknumber (first tag))
                                           (format nil "~A/~A" (second tag) (length (cddb-disk-tracks disk)))
                                           (second tag))))))
                           tags)
                   (list  (namestring file)))))


;;;
;;; Cleaning up file names
;;;

;; Replaces any sequences of non alphanumeric or dot character in the
;; arguments by a single dash; remove accents.

(defparameter +character-foldings+
  '(("A" "ÀÁÂÃÄÅ") ("AE" "Æ") ("C" "Ç") ("E" "ÈÉÊË") ("I" "ÌÍÎÏ")
    ("ETH" "Ð") ("N" "Ñ") ("O" "ÒÓÔÕÖØ") ("U" "ÙÚÛÜ") ("Y" "Ý")
    ("TH" "Þ") ("ss" "ß") ("a" "àáâãäå") ("ae" "æ") ("c" "ç")
    ("e" "èéêë") ("i" "ìíîï") ("eth" "ð") ("n" "ñ") ("o" "òóôõöøº")
    ("u" "ùúûü") ("u" "ýÿ") ("th" "þ")))

(defun character-folding (character)
  (car (member (character character) +character-foldings+
               :test (function position) :key (function second))))

(defun character-fold (character)
  "
RETURN: A string containing the character without accent
        (for accented characters), or a pure ASCII form of the character.
"
  (car (character-folding character)))

(defun string-fold (string)
  (apply (function concatenate) 'string
         (map 'list
           (lambda (ch) (let ((conv (character-folding ch)))
                          (if conv
                              (first conv)
                              (string ch))))
           string)))

(defun clean-name (name)
  (format nil "~{~A~^-~}"
          (split-sequence-if (complement (function alphanumericp))
                             (string-fold (string-downcase name))
                             :remove-empty-subseqs t)))


;;; Convert HH:MM:SS strings into seconds.

(defgeneric dms (o)
  (:method ((s string))
    (dms (list (parse-integer s :start 0 :end 2)
               (parse-integer s :start 3 :end 5)
               (parse-integer s :start 6 :end 8))) )
  (:method ((s list))
    (+ (* 60 (+ (* 60 (first s)) (second s))) (third s))))


;;;
;;; CDDB disk structure
;;;

(defstruct cddb-disk
  media-catalog-number
  disk-id
  performer
  title
  tracks
  %current-track)

(defstruct cddb-track
  number
  offset
  isrc
  performer
  title)

(defun cddb-track-normalize-file-name (track disk)
  (let ((width (max 2 (ceiling (log (1+ (length (cddb-disk-tracks disk))) 10)))))
    (format nil "~V,'0D--~A"
            width (cddb-track-number track)
            (clean-name (cddb-track-title track)))))

(defun cddb-track-tags (track disk)
  (list (list :title       (cddb-track-title track))
        ;; :version
        (list :album       (cddb-disk-title disk))
        (list :tracknumber (cddb-track-number track))
        (list :artist      (cddb-disk-performer disk))
        (list :performer   (cddb-track-performer track))
        ;; :copyright :license :organization
        ;; :description :genre :date :location :contact
        (list :isrc        (cddb-track-isrc track))))

;;;
;;; Loading (read and parse) cddb.txt files.
;;;

(defun update-disk (disk key values)
  (case key
    (:track-list-header  (setf (cddb-disk-tracks disk) (make-array 8 :adjustable t :fill-pointer 0)))
    (:track-list         (destructuring-bind (number offset) values
                           (vector-push-extend (make-cddb-track :number number :offset offset)
                                               (cddb-disk-tracks disk))))
    (:disk-mcn           (setf (cddb-disk-media-catalog-number disk) (first values)))
    (:track-isrc         (destructuring-bind (number isrc) values
                           (setf (cddb-track-isrc (aref (cddb-disk-tracks disk) (1- number))) isrc)))
    (:disk-text-header   (setf (cddb-disk-%current-track disk) nil))
    (:track-text-header  (setf (cddb-disk-%current-track disk) (1- (first values))))
    (:disk-id            (setf (cddb-disk-disk-id disk) (first values)))
    (:performer          (if (cddb-disk-%current-track disk)
                             (setf (cddb-track-performer (aref (cddb-disk-tracks disk)
                                                               (cddb-disk-%current-track disk)))
                                   (first values))
                             (setf (cddb-disk-performer disk) (first values))))
    (:title              (if (cddb-disk-%current-track disk)
                             (setf (cddb-track-title (aref (cddb-disk-tracks disk)
                                                           (cddb-disk-%current-track disk)))
                                   (first values))
                             (setf (cddb-disk-title disk) (first values))))))

;; cd-info 0.82
;; cd-info 0.83 -- cdparanoia III release 10.2 (September 11, 2008)

(defparameter *regexps*
  '(
    (:version           "^cd-info version ([0-9]+\\.[0-9]+) " real)
    (:track-list-header "^CD-ROM Track List")
    (:track-list        "^ *([0-9]*): ([0-9][0-9]:[0-9][0-9]:[0-9][0-9])  .* audio .*$" integer dms)
    (:disk-mcn          "^Media Catalog Number (MCN): (.*)$" string)
    (:track-isrc        "^TRACK (.*) ISRC: (.*)$" integer string)
    (:disk-text-header  "^CD-TEXT for Disc:$")
    (:track-text-header "^CD-TEXT for Track (.*):" integer)
    (:disk-id           "^	DISC_ID: (.*)$" string)
    (:performer         "^	PERFORMER: (.*)$" string)
    (:title             "^	TITLE: (.*)$" string)))

(defun parse-line (line regexps)
  (loop
    :for (key re . types) :in regexps
    :do (multiple-value-bind (start end groups-start groups-end)
            (cl-ppcre:scan re line)
          (declare (ignore end))
          (when start
            (return (values key
                            (loop
                              :for type  :in types
                              :for start :across groups-start
                              :for end   :across groups-end
                              :for text  := (subseq line start end)
                              :collect (ecase type
                                         (string  text)
                                         (dms     (dms text))
                                         (real    (read-from-string text))
                                         (integer (parse-integer text))))))))))

(defun cddb-load (path)
  (with-open-file (input path :external-format #+clisp charset:iso-8859-1 #-clisp :iso-8859-1)
    (loop
      :with disk := (make-cddb-disk)
      :for line := (read-line input nil nil)
      :while line
      :do (multiple-value-bind (key values) (parse-line line *regexps*)
            (when key
              (update-disk disk key values)))
      :finally (return disk))))


;;;
;;; Renaming and tagging files.
;;;

(defun validate-cddb-for-files (files disk)
  (flet ((check (expression message)
           (unless expression
             (format *error-output* "~&ERROR: ~A~%" message)
             (return-from validate-cddb-for-files nil))))
    (check files "No .flac or .mp3 files.")
    (check disk "Invalid cddb.txt file.")
    (check (= (length (cddb-disk-tracks disk)) (length files)) "Different number of tracks and files.")
    (check (cddb-disk-title disk) "Empty disk title.")
    (check (cddb-disk-performer disk) "Empty disk performer.")
    (loop :for track :across (cddb-disk-tracks disk)
          :do (check (cddb-track-title track)     "Empty track title.")
              (check (cddb-track-performer track) "Empty track performer."))
    t))

(defun rename-files (files disk)
  (map 'list
    (lambda (old-path track)
      (let ((new-name (cddb-track-normalize-file-name track disk)))
        (if (string= (pathname-name old-path)
                     (pathname-name new-name))
            old-path
            (if *dry-run*
                (let ((new-path (merge-pathnames new-name old-path)))
                  (format t "~&Would rename ~A to ~A~%" (file-namestring old-path) (file-namestring new-path))
                  (finish-output)
                  new-path)
                (let ((new-path (rename-file old-path new-name)))
                  (format t "~&Renamed ~A to ~A~%" (file-namestring old-path) (file-namestring new-path))
                  (finish-output)
                  new-path)))))
    files (cddb-disk-tracks disk)))

(defun tag-file (file disk tags)
  (cond
    ((string-equal "flac" (pathname-type file))  (tag-flac-file file disk tags))
    ((string-equal "mp3"  (pathname-type file))  (tag-mp3-file  file disk tags))
    (t (error "Cannot tag: unknown file type ~S" (pathname-type file)))))

(defun tag-files (files disk)
  (map nil
    (lambda (file track)
      (let ((tags (cddb-track-tags track disk)))
        (when *dry-run*
          (format t "~&Would tag file ~A~:{~%    ~30S ~S~}~%"
                  file
                  tags))
        (tag-file file disk tags)))
    files (cddb-disk-tracks disk)))

(defun rename-and-tag-files (directory)
  (let* ((cddb-file (merge-pathnames #P"cddb.txt" directory))
         (flacs     (sort (directory (merge-pathnames #P"*.flac" directory))
                          (lambda (a b)
                            (string< (file-namestring a) (file-namestring b)))))
         (mp3s      (sort (directory (merge-pathnames #P"*.mp3" directory))
                          (lambda (a b)
                            (string< (file-namestring a) (file-namestring b)))))
         (files     (or flacs mp3s)))

    (unless (probe-file cddb-file)
      (format *error-output* "~&ERROR: Missing a ~A file.~%" cddb-file)
      (return-from rename-and-tag-files ex-noinput))

    (unless files
      (format *error-output* "~&ERROR: No .flac or .mp3 files in ~A.~%" directory)
      (return-from rename-and-tag-files ex-noinput))

    (let ((disk (cddb-load cddb-file)))

      (unless (validate-cddb-for-files files disk)
        (format *error-output* "~&ERROR: Invalid ~A file for ~S.~%" cddb-file files)
        (return-from rename-and-tag-files ex-dataerr))

      (handler-case
          (tag-files (rename-files files disk) disk)
        (error (err)
          (format *error-output* "~&ERROR: while processing ~A: ~A~%"
                  directory
                  (string-trim #(#\newline #\space #\tab)
                               (princ-to-string err)))
          ex-software)
        (:no-error (&rest ignore)
          (declare (ignore ignore))
          ex-ok)))))


;;;
;;; The main function.
;;;

(defun main (arguments)
  (let ((result ex-ok))
    (dolist (directory arguments result)
      (let ((directory (if (string= "/" (subseq directory (1- (length directory))))
                           directory
                           (concatenate 'string directory "/"))))
        (handler-case
            (let ((new-result (rename-and-tag-files directory)))
              (setf result (if (zerop result)
                               new-result
                               result)))
          (error (err)
            (format *error-output* "~&ERROR: while processing ~A: ~A~%"
                    directory
                    (string-trim #(#\newline #\space #\tab)
                                 (princ-to-string err)))
            (setf result ex-software)))))))


;;;
;;; Legacy garbage.
;;;


(defun get-disc-and-track-from-pathname (pathname)
  "Extract the disc and track numbers from a pathname such as:
/music/$album/cd01/track42\\.cdda.flac"
  (let ((name (file-namestring pathname))
        (dir  (first (last (pathname-directory pathname)))))
    (values (cond
              ((and (prefixp "cd" dir)
                    (digit-char-p (aref dir 2)))
               (parse-integer dir :start 2 :junk-allowed t)))
            (cond
              ((and (prefixp "track" name)
                    (digit-char-p (aref name 5)))
               (parse-integer name :start 5 :junk-allowed t))))))



;; (defun title-to-filename (title)
;;   ())

(defun set-flac-metadata (file plist)
  (warn "Not implemented yet.")
  (print file)
  (print plist))

(defun verbose-rename-file (old new)
  (declare (ignore old))
  (warn "Not implemented yet.")
  (print new))


(defun rename-eric-satic-files ()
  (dolist (dir '("/movies/sound/flac/eric-satie/the-complete-solo-piano-music--thibaudet/"
                 "/movies/sound/flac/eric-satie/satie-piano-works--ciccolini--tacchino/"))
    (let ((index (com.informatimago.common-lisp.cesarum.file:sexp-file-contents
                  (merge-pathnames "index.sexp" dir))))
      (dolist (file (directory (merge-pathnames #P"**/*.flac" dir)))
        (multiple-value-bind (discnum tracknum) (get-disc-and-track-from-pathname file)
          (let* ((dirkeys   (mapcan (lambda (key)
                                      (let ((value (getf index key)))
                                        (when value (list key value))))
                                    (mapcar (function first) *tag-keys*)))
                 (discindex (find discnum (getf index :discs) :key (lambda (entry) (getf entry :number))))
                 (trackinfo (find tracknum (getf (or discindex index) :tracks) :key (function first)))
                 (trackno   (getf trackinfo :tracknum))
                 (title     (error "Not implemented yet.")))
            (set-flac-metadata file (list* :album (if discnum
                                                      (format nil "~A, disc ~A"
                                                              (getf dirkeys :album)
                                                              discnum)
                                                      (getf dirkeys :album))
                                           (append
                                            (progn (remf dirkeys :album) dirkeys)
                                            trackinfo)))
            (verbose-rename-file file (format nil "~2,'0D-~A" trackno (clean-name title)))))))))


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
