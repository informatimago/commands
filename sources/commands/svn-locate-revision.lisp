;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               svn-locate-revision
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Locates a revision in the svn:mergeinfo properties.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pascal.bourguignon@trustonic.com>
;;;;MODIFICATIONS
;;;;    2016-12-13 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2016 - 2016
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

(command :use-systems (:xmls :com.informatimago.common-lisp.cesarum)
         :main "SVN-LOCATE-REVISION:MAIN")


(defpackage "SVN-LOCATE-REVISION"
  (:use "COMMON-LISP"
        "SCRIPT"
        "XMLS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export "MAIN"))
(in-package "SVN-LOCATE-REVISION")

(defparameter *verbose* t)


(defun make-pipe-input-stream  (command &key (external-format :default)
                                          (element-type 'character)
                                          (buffered t))
  (declare (ignore command external-format element-type buffered))
  (error "Not implemented yet."))

(defun make-pipe-output-stream (command &key (external-format :default)
                                          (element-type 'character)
                                          (buffered t))
  (declare (ignore command external-format element-type buffered))
  (error "Not implemented yet."))


(defun candidate-branches (url revision)
  "Return a list of branch names that have a range covering the REVISION."
  (with-open-stream (in (make-pipe-input-stream
                         (format nil "svn propget svn:mergeinfo ~S" url)
                         :buffered t))
    (loop
      :with result := '()
      :for line := (read-line in nil nil)
      :while line
      :do (destructuring-bind (branch revisions) (split-string line ":")
            (when (some (lambda (range)
                          (destructuring-bind (min &optional max) (split-string range "-")
                            (let ((min (parse-integer (string-right-trim "*"  min)))
                                  (max (and max (parse-integer (string-right-trim "*"  max)))))
                              (if max
                                  (<= min revision max)
                                  (= min revision)))))
                        (split-string revisions ","))
              (push branch result)))
      :finally (return result))))

(defun branch-url (base-url branch-name)
  (concatenate 'string base-url
               (if (and (plusp (length branch-name))
                        (char= #\^ (aref branch-name 0)))
                   (subseq branch-name 1)
                   branch-name)))



(defun svn-info (url)
  ;; xmls:parse parses the xml stream into a sexp.
  (parse (make-pipe-input-stream
          (format nil "svn info --xml ~S" url)
          :buffered t)))

(defun info-root (info)
  (let ((entry (xmlrep-find-child-tag "entry" info)))
   (unless (string= "dir" (xmlrep-attrib-value "kind" entry))
     (error "Expected the svn info of a directry entry, not ~S info" info))
    (first (xmlrep-children (xmlrep-find-child-tag "root" (xmlrep-find-child-tag "repository" entry))))))


(defun svn-revision (url revision)
  (ignore-errors
   (parse (make-pipe-input-stream
           (format nil "svn log ~S -r ~A --xml" url revision)
           :buffered t))))

(defun revision-logentry (revision) (and (xmlrep-children revision)
                                         (xmlrep-find-child-tag "logentry" revision)))
(defun logentry-revision (entry)    (parse-integer (xmlrep-attrib-value "revision" entry)))
(defun logentry-date     (entry)    (first (xmlrep-children (xmlrep-find-child-tag "date"   entry))))
(defun logentry-author   (entry)    (first (xmlrep-children (xmlrep-find-child-tag "author" entry))))
(defun logentry-message  (entry)    (first (xmlrep-children (xmlrep-find-child-tag "msg"    entry))))


(defun locate-revision (url revision)
  "Indicate whether the given REVISION exists in the branch at URL.
If it does, then return a list of merged branches where this revision come from."
  (let ((info (info-root (svn-info url))))
    (remove-if-not (lambda (candidate)
                     (revision-logentry (svn-revision (branch-url info candidate) revision)))
                   (candidate-branches url revision))))


;; locate a set of revisions (53007 has not been merged anywhere)) :
;; (mapcar (lambda (revision)
;;           (cons revision (locate-revision  "/Users/pjb/src/trustonic/tbase/branches/dev_kinibi_wb_sdk/" revision)))
;;         '(50233 50950 51012 53007))
;; ((50233 "/users/pasbou01/dev_kinibi_wb_sdk-LP33172863")
;;  (50950 "/users/pasbou01/dev_kinibi_wb_sdk-LP33172863")
;;  (51012 "/users/pasbou01/dev_kinibi_wb_sdk-LP33172863")
;;  (53007))

;; Problem: find in  /users/pasbou01/dev_kinibi_wb_sdk-LP33172863
;; all the revisions that have not found their way (merged) into
;; /users/pasbou01/merge-dev_kinibi_wb_sdk-LP33172863 (which is a
;; copy of /branches/dev_kinibi_wb_sdk).
;;

;; (let ((revisions  (let ((src-url "https://svn.trustonic.internal/svn/tbase/users/pasbou01/dev_kinibi_wb_sdk-LP33172863"))
;;                     ;; 1- filter revisions where author=pasbou01 (the others have been merged in and are already in dst-url)
;;                     ;;    between HEAD:47559.
;;                     (mapcar (function logentry-revision)
;;                             (remove "pasbou01"
;;                                     (xmlrep-children (parse (make-pipe-input-stream
;;                                                              (format nil "svn log ~S -r ~A --xml" src-url "HEAD:47559")
;;                                                              :buffered t)))
;;                                     :test (function string/=)
;;                                     :key (function logentry-author)))))
;;       (dst-url ;; "https://svn.trustonic.internal/svn/tbase/users/pasbou01/merge-dev_kinibi_wb_sdk-LP33172863"
;;         "https://svn.trustonic.internal/svn/tbase/branches/dev_kinibi_wb_sdk"))
;;   ;; 2- remove the revisions that are already in dst-url.
;;   (remove-if (lambda (revision)
;;                (locate-revision dst-url revision))
;;              revisions))

;; --> (53007 52698 52614 51667)



(defun main (arguments)
  (loop
    :for srevision :in arguments
    :for revision = (parse-integer srevision :junk-allowed t
                                             :start (if (and (plusp (length srevision))
                                                             (eql #\r (aref srevision 0)))
                                                        1
                                                        0))
    :when revision
      :do (map nil (function write-line) (locate-revision "." revision)))
  ex-ok)

;;;; THE END ;;;;
