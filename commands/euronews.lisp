;;;; -*- mode: lisp; coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:              eurnews
;;;;LANGUAGE:          Common-Lisp
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;    Note: we compile on load only to check syntax errors faster.
;;;;USAGE
;;;;    euronews --help
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2004-03-24 <PJB> Added user selectable language.
;;;;    2002-09-30 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2002 - 2004
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
;;;;******************************************************************************

(defparameter *program-version* "1.0.2")

(defvar do-clear    t "Whether clear screen actually works.")
(defun clear () (when do-clear (uiop:run-program "clear 2>/dev/null || true"
                                                 :force-shell t)))

(defvar language    "ge" "The language selected for euronews.")
(defvar index       0)
(defvar last-index  0)
(defvar previous    nil)
(defvar urls        '())
(defvar length-urls 0)
(defvar played      nil)
(defvar played-last nil)
(defvar menu-items  '())

(defconstant +available-languages+ '(de fr en it es ru))

(defun language-to-euronews (lang)
  (cond
     ((member lang '("ge" "de") :test 'string-equal) "ge")
     ((member lang '("es" "sp") :test 'string-equal) "sp")
     ((member lang +available-languages+ :test 'string-equal) lang)
     (t nil)))

(defun stream-to-string-list (stream)
  (loop with line = (read-line stream nil nil)
        while line
        collect line into result
        do (setf line (read-line stream nil nil))
        finally (return result)))

(defun played-indicator      (index)  (if (aref played index) "*" " "))
(defun played-last-indicator (index)  (if (= index played-last) "#" " "))
(defun set-played (index  rest)
  (when rest (setf played-last index))
  (setf (aref played index) rest))

(defun get-urls ()
  (setf previous nil)
  (setf urls
        (loop for item in (sort
                           (loop for page in '( "accueil_info" "acceuil_eco" "euro" "lemag" "hitech" )
                                 for new-urls = (stream-to-string-list
                                                 (uiop:run-program
                                                  (format nil "{ lynx -source 'http://www.euronews.net/create_html.php?page=~A&langue=~A'|tr '<' '\\012'|grep ramgen|sed -e 's/.*lien=\\(.*hostname\\).*/http:\\/\\/\\1/' ;}" page language)
                                                  :input     nil
                                                  :output    :stream
                                                  :force-shell t))
                                 ;;do (format t "new-urls=~S~%" new-urls)
                                 append new-urls into all-urls
                                 finally (return all-urls))
                           'string<=)
              ;;do (format t "item=~S~%" item)
              when (and previous (string/= previous item))
              collect item into all-urls
              do (setf previous item)
              finally (return all-urls)))
  (setf length-urls (length urls)) ;;get-urls
  (setf played (make-array (list length-urls) :initial-element nil))
  (setf played-last -1)
  (setf last-index 1))


(defun get-file-name (url)
  (let ((question nil)
        (slash nil))
  (loop for index from (1- (length url)) downto 0
        until slash
        when (char= (char url index) (character '\?)) do (setf question index)
        when (char= (char url index) (character '\/)) do (setf slash index)
        finally (return (subseq url (1+ slash) question)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main (arguments)
  (dolist (arg arguments)
    (setf language (language-to-euronews arg))
    (unless language
      (format t "~%usage:~%")
      (format t "   euronews  [de|fr|en|it|es|po|ru]~%")
      (exit ex-usage)))
  (get-urls)
  (loop while (and (/= 0 last-index) (<= last-index length-urls))
        do
           ;; index is counted between 1 and (length urls)
           ;; index==0 means quit
           ;; urls list and played array are  0 indexed though.
           (clear)

           (setf menu-items
                 (loop :for index := 0 :then (1+ index)
                       :for url :in urls
                       :collect (format nil "~2D ~1A~1A ~34A"
                                        (1+ index)
                                        (played-indicator        index)
                                        (played-last-indicator   index)
                                        (get-file-name url))
                         :into menu-items
                       :finally (return menu-items)))
           (loop :for line :from 0 :to 21
                 :do (loop :for menu := line :then (+ 22 menu)
                           :while (< menu (length menu-items))
                           :do (format t "~40A" (elt menu-items menu))
                           :finally (format t "~%")))

           (format t "~%Number to play (0 to quit) or language ~S: "
                   +available-languages+)
           (setf index (read))
           (cond
             ((member index +available-languages+)
              (setf language (language-to-euronews index))
              (get-urls))
             ((numberp index)
              (if (and (<= 0 index) (<= index length-urls))
                  (setf last-index index)
                  (progn
                    (format t "Invalid option!")
                    (setf last-index (- 0 last-index)))))
             ((eq 'n index)
              (setf last-index (1+ (mod last-index length-urls))))
             ((eq 'p index)
              (setf last-index (1+ (mod (- last-index 2) length-urls))))
             ((eq 'q index)
              (setf last-index 0))
             ((eq 'r index)
              ;; replay - no change
              )
             (t
              (format t "Invalid option!")
              (setf last-index (- 0 last-index))))

           (when (< 0 last-index)
             (error "What player?")
             #-(and) (uiop:run-program (format nil "/local/apps/RealPlayer8/realplay '~A' &"
                                               (elt urls (1- last-index)))
                                       :force-shell t)
             (set-played (1- last-index) t)
             (setf index (1+ (mod last-index length-urls))))
           (setf last-index (abs last-index)))
  ex-ok)

;;;; THE END ;;;;
