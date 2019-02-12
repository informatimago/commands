#!/usr/local/bin/clisp -q -ansi -norc
;; -*- mode:lisp;coding:utf-8 -*-

(defun make-pathname* (&key (host nil hostp) (device nil devicep) (directory nil directoryp)
                       (name nil namep) (type nil typep) (version nil versionp)
                       (defaults nil defaultsp) (case :local casep))
  (declare (ignorable casep))
  #+ (or abcl ccl allegro)
  (labels ((localize (object)
             (typecase object
               (list   (mapcar (function localize) object))
               (string (string-downcase object))
               (t      object)))
           (parameter (indicator key value)
             (when indicator
               (list key (if (eql case :common)
                             (localize value)
                             value)))))
    (apply (function make-pathname)
           (append (parameter hostp      :host      host)
                   (parameter devicep    :device    device)
                   (parameter directoryp :directory directory)
                   (parameter namep      :name      name)
                   (parameter typep      :type      type)
                   (parameter versionp   :version   version)
                   (parameter defaultsp  :defaults  defaults)
                   (list :case :local))))
  #-(or abcl ccl allegro)
  (apply (function make-pathname)
         (append
          (when hostp      (list :host      host))
          (when devicep    (list :device    device))
          (when directoryp (list :directory directory))
          (when namep      (list :name      name))
          (when typep      (list :type      type))
          (when versionp   (list :version   version))
          (when defaultsp  (list :defaults  defaults))
          (when casep      (list :case      case)))))

(let* ((root "/Users/pjb/" #-(and) (user-homedir-pathname))
      (quicklisp (merge-pathnames
                  (make-pathname* :directory '(:relative "QUICKLISP")
                                  :name "SETUP"
                                  :type "LISP"
                                  :version :newest
                                  :case :common
                                  :defaults root)
                  root
                  nil)))
  (if (probe-file quicklisp)
      (load quicklisp)
      (error "Please install quicklisp.  I expect it in ~S" quicklisp)))

(ql:quickload :split-sequence)
(use-package :split-sequence)

(defvar *processing* '())

(defun port (options packs)
  (format *trace-output* "### port ~{~A~^ ~}~%" (append options packs))
  (force-output *trace-output*)
  (let ((*processing* (append (loop :for (p v) :on packs :by (function cddr)
                                 :if v :collect (format nil "~A ~A" p v)
                                 :else :collect p)
                              *processing*))
        (before '()))
    (with-open-stream (pout (ext:run-program "port" :arguments (append options packs) :input nil :output :stream))
      (loop
         :with prefix = "--->  	"
         :for line = (read-line pout nil nil)
         :while line
         :do (write-line line)
         :do (when (and (< (length prefix) (length line))
                        (string= line prefix :end1 (length prefix)))
               (push (subseq line (length prefix)) before))))
    (when before
      (dolist (pack before)
        (format *trace-output* "pack = ~S~% *processing* = ~S~2%" pack *processing*)
        (if (member pack *processing* :test (function equalp))
            (port '("uninstall" "-f")  (split-sequence #\space pack))
            (port '("uninstall")       (split-sequence #\space pack))))
      (port options packs))))

(catch 'done
  (port '("uninstall") EXT:*ARGS*))

