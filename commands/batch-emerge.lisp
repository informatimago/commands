;; -*- mode:lisp;coding:utf-8 -*-

;; emerge all the packages specified in /home/pjb/portage-packages.txt, in batch(1).
(defun main (arguments)
  (declare (ignore arguments))
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt)  :preserve)
    (with-open-file (f (merge-pathnames "portage-packages.txt"
                                        (user-homedir-pathname)))
      (let ((*readtable* rt))
        (loop
          :for package = (read f nil nil)
          :while package
          :do (let ((batch #+ccl (ccl:run-program "batch" '() :wait nil :input :stream :output :stream)
                           #-ccl (error "Missing run-program, uiop:run-program fails.")))
                (with-open-stream (input (ccl:external-process-input-stream batch))
                  (format input "emerge ~A~%" package)))))))
  ex-ok)

;;;; THE END ;;;;
