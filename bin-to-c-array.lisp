#!/usr/local/bin/cl
;; -*- mode:lisp; coding:utf-8 -*-

;; Note: xxd -i does about the same.

(defparameter *program-name* "bin-to-c-array")

(defparameter *element-type* "uint8")
(defparameter *size-type*    "usize_t")

(defmethod generate-c-array ((in stream) name
                             &key (radix 16)
                               (byte-type "unsigned char")
                               (size-type "unsigned int"))
  (format t "~%~A ~A[]={" byte-type name)
  (loop
    :with sep := ""
    :with r   := (case radix (8 0) (10 1) (otherwise 2))
    :with i   := 0
    :for byte := (read-byte in nil nil)
    :while byte
    :do (format t "~A~:[~;~%    ~]~[0~3,'0O~;~3D~;0x~2,'0X~]" sep  (zerop (mod i 8)) r byte)
        (incf i)
        (setf sep ", ")
    :finally (format t "};~%")
             (format t "~A ~A_size=~D;~%" size-type name i)))




#-(and)
(let ((paths '("a" "b" "c")))
  (unwind-protect
       (progn
         (map nil (lambda (path)
                    (close (open path :if-does-not-exist :create)))
           paths)
         (multiple-value-bind (arguments files)
             (process-arguments paths '())
           (assert (= 3 (loop :for stream := (funcall files)
                              :for path :in paths
                              :while stream
                              :do (assert (equal (pathname path)
                                                 (pathname stream)))
                              :do (close stream)
                              :count 1)))))
    (map nil (function delete-file) paths)))

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

(defun main (arguments)
  (let ((meta-options  '(((:decimal   "-d" "--decimal"))
                         ((:octal     "-o" "--octal"))
                         ((:hexa      "-x" "--hexa" "--hexadecimal"))
                         ((:byte-type "-bt" "--byte-type") 1)
                         ((:size-type "-st" "--size-type") 1)
                         ((:help      "-h"  "--help")))))
    (multiple-value-bind (options files)
        (process-arguments arguments
                           meta-options
                           :standard-input nil)
      (when (member '(:help) options :test (function equal))
        (loop :for option :in meta-options
              :initially (format t "~A usage:~2%    ~:*~A {option} [--] {file}~2%" *program-name*)
              :do (destructuring-bind ((ignore &rest options) &optional typep) option
                    (declare (ignore ignore))
                    (format t "         ~{~A~^|~}~:[~; type~]~%" options typep))
              :finally (terpri))
        (return-from main))
      (let ((radix     (cond
                         ((member '(:decimal) options :test (function equal)) 10)
                         ((member '(:octal)   options :test (function equal))  8)
                         (t                                                   16)))
            (byte-type (let ((type (find :byte-type options :key (function first))))
                         (if type
                             (second type)
                             "unsigned char")))
            (size-type (let ((type (find :size-type options :key (function first))))
                         (if type
                             (second type)
                             "unsigned int"))))
        (loop
          :for stream := (funcall files)
          :for path   := (and stream (pathname stream))
          :for name   := (and stream
                              (string-downcase
                               (remove-if-not
                                (lambda (ch) (or (alphanumericp ch) (char= ch #\_)))
                                (substitute #\_ #\- (pathname-name path)))))
          :while stream
          :do (close stream)
              (with-open-file (in path :element-type '(unsigned-byte 8))
                (generate-c-array in name
                                  :radix radix
                                  :byte-type byte-type
                                  :size-type size-type))))))
  0)


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
