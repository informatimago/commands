#!/usr/local/bin/clisp -ansi -q -E utf-8
;; -*- mode:lisp; coding:iso-8859-1 -*-

(defparameter *base-url* "http://hubble:3000/diagram/")
(defparameter *base-url* "http://yuml.me/diagram/")

(defun yuml-url (lines &key (style  :nofunky) (scale 100) (direction :lr)
                 (kod :class))
  (check-type style (member :nofunky :plain :scruffy))
  (check-type scale (integer 10 200))
  (check-type direction (member :lr :tb :bt :rl))
  (check-type kod (member :class :activity :usecase))
  (check-type lines list) ; of strings
  (format nil "~A~(~A~);scale:~D;dir:~A/~(~A~)/~{~A~^,~}"
          *base-url* style scale direction kod lines))

(defvar *style*     :plain)
(defvar *scale*     100)
(defvar *direction* :lr)

(defun uml-line-p (line)
  (regexp:match "^\\.\\.  *UML  *\\([^ ][^ ]*\\)" line))

(defun get-resource (file url)
  (let ((cached-url (make-pathname :type "URL" :case :COMMON :defaults file)))
    (if (and (probe-file file)
             (probe-file cached-url)
             (string=  url (with-open-file (stream cached-url) (read-line stream))))
      file
      (progn
        (ignore-errors (delete-file file))
        (print `(ext:run-program "wget" :arguments ,(list "-O" file  url) :wait t :output nil)) (finish-output)
        (or (ext:run-program "wget" :arguments (list "-O" file  url) :wait t
                             :output nil)
            (progn
              (with-open-file (stream cached-url
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (write-line url stream))
              file))))))


(defun process-uml (line dir src dst)
  (multiple-value-bind (all kod name options)
      (regexp:match "^\\.\\.  *UML  *\\([a-z][a-z]*\\)/\\([^ ][^ ]*\\)\\( .*\\)" line)
    (when all
      (let* ((kod     (regexp:match-string line kod))
             (name    (regexp:match-string line name))
             (options (with-input-from-string (input  (regexp:match-string line options))
                        (loop
                           :for item = (read input nil input)
                           :until (eq item input)
                           :collect item)))
             (file (make-pathname :name (format nil "~A-~A" kod name)
                                  :type "png"
                                  :version nil
                                  :defaults dir)))
        (destructuring-bind (&key (style *style*) (scale *scale*) (direction *direction*)) options
         (format dst ".. |~A/~A| image:: ~A~%" kod name
                 (namestring
                  (get-resource
                   file
                   (yuml-url
                    (loop
                       :for line = (read-line src nil nil)
                       :for sline = (and line (string-trim " " line))
                       :while (and line (not (string= "" sline)))
                       :collect sline)
                    :kod (intern (string-upcase kod) "KEYWORD")
                    :style style :scale scale :direction direction)))))))))


(defun process-stream (dir src dst)
(print dir) (finish-output)
  (loop
     :for line = (read-line src nil nil)
     :while line
     :do (if (uml-line-p line)
           (process-uml line dir src dst)
           (write-line line dst))))


(if (null ext:*args*)
  (process-stream #P"./"  *standard-input* *standard-output*)
  (dolist (src ext:*args*)
    (let ((dir (make-pathname :name nil :type nil   :version nil :case :common :defaults src))
          (rst (make-pathname           :type "RST" :version nil :case :common :defaults src)))
      (with-open-file (input src)
        (with-open-file (output rst
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (process-stream dir input output))))))
