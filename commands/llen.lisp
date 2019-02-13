;;;; -*- mode:lisp; coding:iso-8859-1 -*-

;; prefix each line of input with its length.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;
;; Warning: processes iso-8859-1 not utf-8 arguments! ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;


(defun llen (stream)
  (loop
    :for line = (read-line stream nil nil)
    :while line
    :do (format t "~D ~A~%" (length line) line)))

(defun process-arguments (argv options &key (standard-input t))
  (flet ((input-files (argv)
           (cond
             (argv
              (let ((files argv))
                (lambda ()
                  (cond
                    ((null files)
                     nil)
                    ((and standard-input (string= (first files) "-"))
                     (pop files)
                     *standard-input*)
                    ((open (pop files)))))))
             (standard-input
              (let ((given nil))
                (lambda ()
                  (if given
                      nil
                      (progn
                        (setf given t)
                        *standard-input*)))))
             (t
              (constantly nil))))
         (optionp (arg options)
           (find-if (lambda (option)
                      (cond
                        ((atom option)         (string= arg option))
                        ((atom (first option)) (string= arg (first option)))
                        ((member arg (first option) :test (function string=)))))
                    options))
         (option-canonical (option)
           (cond
             ((atom option)          option)
             ((atom (first option)) (first option))
             (t                     (first (first option)))))
         (option-argument-count (option)
           (if (atom option)
               0
               (or (second option) 0))))
    (loop
      :with arguments := '()
      :with option
      :while argv
      :do (cond ((string= "--" (first argv))
                 (pop argv)
                 (loop-finish))
                ((and (<= 1 (length (first argv)))
                      (char= #\- (aref (first argv) 0))
                      (setf option (optionp (first argv) options)))
                 (let ((argument (pop argv)))
                   (push (cons (option-canonical option)
                               (if (<= (option-argument-count option) (length argv))
                                   (loop :repeat (option-argument-count option)
                                         :collect (pop argv))
                                   (error "Missing arguments after option ~S" argument)))
                         arguments)))
                (t
                 (loop-finish)))
      :finally (return (values (nreverse arguments)
                               (input-files argv))))))

(defun main (argv)
  (let ((meta-options  '(((:help      "-h"  "--help")))))
    (multiple-value-bind (options files)
        (process-arguments argv
                           meta-options
                           :standard-input t)
      (when (member '(:help) options :test (function equal))
        (loop :for option :in meta-options
                :initially (format t "~A usage:~2%    ~:*~A {option} [--] {file}~2%" *program-name*)
              :do (destructuring-bind ((ignore &rest options) &optional typep) option
                    (declare (ignore ignore typep))
                    (format t "         ~{~A~^|~}~%" options))
              :finally (terpri))
        (return-from main))
      (loop
        :for stream := (funcall files)
        :while stream
        :do (llen stream))))
  ex-ok)


