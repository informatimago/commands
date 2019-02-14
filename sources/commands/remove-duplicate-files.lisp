;;;; -*- mode:lisp; coding:utf-8 -*-

(defun main (arguments)
  (when (null arguments)
    (error "Usage: ~A directory~%" *program-name*))
  (dolist (*base* arguments)
    (when (and (stringp *base*)
               (char/= #\/ (char *base* (1- (length *base*)))))
      (setf *base* (format nil "~A/" *base*)))
    (dolist (dir (directory
                  (make-pathname
                   :directory (append (pathname-directory *base*) '(:wild))
                   :defaults *base*)))
      (format *trace-output* "Processing ~A~%" dir)
      (let ((sumtab (make-hash-table)))
        (format *trace-output* "  Checksumming...~%")
        (with-open-stream (sums (uiop:run-program
                                 (format nil "md5sum ~A"
                                         (namestring
                                          (make-pathname :name :wild
                                                         :type :wild
                                                         :defaults dir)))
                                 :input nil :output :stream :wait nil
                                 :force-shell t))
          (loop :for (sum file) := (list (read sums nil nil)
                                       (read-line sums nil nil))
                :while sum
                :do (push (string-trim " " file) (gethash sum sumtab))))
        (format *trace-output* "  Deleting doubles...~%")
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (when (< 1 (length v))
                     (pop v)
                     (dolist (f v) (delete-file f))))
                 sumtab))))
  ex-ok)

;;;; THE END ;;;;



