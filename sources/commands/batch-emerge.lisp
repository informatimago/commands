;; -*- mode:lisp;coding:utf-8 -*-

;; emerge all the packages specified in /home/pjb/portage-packages.txt, in batch(1).
(options "batch-emerge" (standard-options))

(defun main (arguments)
  (parse-options *command* arguments)
  #-(or ccl sbcl) (error "This command can only work when compiled with ccl or sbcl.")
  #+(or ccl sbcl)
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt)  :preserve)
    (with-open-file (f (merge-pathnames "portage-packages.txt"
                                        (user-homedir-pathname)))
      (let ((*readtable* rt))
        (loop
          :for package = (read f nil nil)
          :while package
          :do #+ccl (let ((batch (uiop:run-program "batch" :input :stream :output :stream :wait nil)))
                      (with-open-stream (input (ccl:external-process-input-stream batch))
                        (format input "emerge ~A~%" package)))
              #+sbcl (uiop:run-program (format nil "bash -c 'echo emerge ~A|batch'" package) :wait nil)))))
  ex-ok)

;;;; THE END ;;;;
