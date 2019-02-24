;; -*- mode:lisp;coding:utf-8 -*-

(defun main (arguments)
  (declare (ignore arguments))
  (error "Not implemented yet.")
  ex-usage)


#|
(in-package "COMMON-LISP-USER")
(load (make-pathname :name "SCRIPT" :type "LISP" :version NIL :case :common :defaults *load-pathname*))
(use-package "SCRIPT")
(defparameter *program-version* "1.0.2")

(defpackage "CPCD"
  (:use "CL" "SCRIPT"))
(in-package "CPCD")


;; (redirecting-stdout-to-stderr (load #p"/etc/gentoo-init.lisp"))
;; (redirecting-stdout-to-stderr
;;  (let ((*load-verbose* nil)
;;        (*compile-verbose* nil))
;;    (load (make-pathname :name ".clisprc" :type "lisp" :case :local
;;                         :defaults (user-homedir-pathname)))
;;    ;; (setf *features* (delete :testing-script *features*))
;;    ))
;; (redirecting-stdout-to-stderr (asdf:oos 'asdf:load-op :split-sequence)
;;                               (asdf:oos 'asdf:load-op :cl-ppcre))


;; default implementations

(defun eject ()
  (format *query-io* "Please eject the CDROM, and type RET")
  (read-line  *query-io*)
  (values))


(defun wait-cd (&optional (name ""))
  (format *query-io* "Waiting for the CD ~S~%" name)
  (format *query-io* "Please insert a CDROM, and type RET when it's ready.")
  (read-line *query-io*)
  (values))


;; system specific override:

(case (uname)
  ((:DARWIN)
   (defun eject ()
     (shell "drutil ~A" "eject"))
   (defun wait-cd (&optional (name ""))
     (loop
        :do (if (not (shell "df | grep -q -s /dev/disk1"))
                (loop-finish)
                (progn
                  (format *query-io* "Waiting for the CD ~s~%" name)
                  (sleep 1))))))
  ((:linux)
   (defun eject ()
     (shell "eject"))))

(defun cpcd (cddir)
  (wait-cd cddir)
  (ensure-directories-exist (make-pathname :name "PROBE"  :case :common :defaults cddir))
  (let ((curdir (ext:cd)))
    (unwind-protect
         (progn
           (ext:cd cddir)
           (shell "cd-info > cddb.txt")
           (shell "cdparanoia --query 2> cdparanoia.txt")
           (shell "cdparanoia --output-wav --batch")
           (eject)
           (format *trace-output*   "Compressing in background.~%")
           (when (zerop (linux:fork))
             (dolist (file (directory "*.wav"))
               (when (not (shell "flac --silent -V --compression-level-8 ~S >> flac.log" (namestring file)))
                 (delete-file file)))
             (ext:exit 0)))
      (ext:cd curdir))))


(defparameter *start-index*      nil)
(defparameter *directory-format* "cd~2,'0D")

(command :options (list
(option ("-b" "--batch") (start-index)
  ""
  (setf *start-index* (parse-integer start-index)))

(option ("-f" "--directory-format") (format)
  (setf *directory-format* format))

(option ("-o" "--one-shoot") (directory)
  (cpcd directory)))


#|
if [ "$start" != '' ] ; then
    case "$start" in
    *[^0-9]*)
        printf "Invalid start index, it must be an integer, not '%s'\n" "$start"
        usage
        exit 1
        ;;
    esac
    index=$start
    while true ; do
        cpcd "$(printf "$format" "$index")"
        start=$(( $index + 1 ))
    done
else
    cpcd "${dir}"
fi
|#


(defun main (arguments)
  (parse-options arguments)
  ex-ok)


|#

;;;; THE END ;;;;
