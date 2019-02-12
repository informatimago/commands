#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-

(defparameter *ARCHIVE* "/a5/rc/")
(defparameter *URL*     "http://www.tv-radio.com/cgi-bin/tagger.pl?tag=site&metafile=courtoisie/courtoisie-20k.asx")

(setf (ext:getenv "DISPLAY")            ":0.0"
      (ext:getenv "WRITE_ASF"           "1")
      (ext:getenv "AVIPLAY_MUTE_AUDIO") "1"
      (ext:getenv "AVIPLAY_MUTE_VIDEO") "1")


(defparameter *schedule*
  '((everyday  (00 00 01 30))
    (everyday  (02 00 05 00))
    (everyday  (07 30 10 30))
    (everyday  (12 00 13 30))
    (everyday  (18 00 21 00))
    (wednesday (21 30 00 00))))

(defstructure)
aviplay "$URL" > $LOG 2>&1 &
PID=$?
sleep $(( $MINUTES * 60 ))
kill $PID
FILE="$(sed -n -e 's-.*Writing ASF file: \([^ ]*\).*-\1-p' < $LOG )"
mv "$FILE" "$ARCHIVE/${NAME/.asf}.asf"
