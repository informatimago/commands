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


(defun input-files (operands &key (standard-input t))
  "Return a generator (a function of no arguments) that yields successive
input streams for the file OPERANDS (\"-\" denotes *STANDARD-INPUT*), or
*STANDARD-INPUT* once when OPERANDS is empty and STANDARD-INPUT is true."
  (cond
    (operands
     (let ((files operands))
       (lambda ()
         (cond
           ((null files) nil)
           ((and standard-input (string= (first files) "-")) (pop files) *standard-input*)
           ((open (pop files)))))))
    (standard-input
     (let ((given nil))
       (lambda ()
         (if given
             nil
             (progn (setf given t) *standard-input*)))))
    (t
     (constantly nil))))

(defvar *radix*     16             "Output radix for the array bytes.")
(defvar *byte-type* "unsigned char" "C type used for the array elements.")
(defvar *size-type* "unsigned int"  "C type used for the array size constant.")

(options "bin-to-c-array"
         (standard-options)
         (option ("decimal" "-d" "--decimal") ()
                 "Write the array bytes in decimal."
                 (setf *radix* 10))
         (option ("octal" "-o" "--octal") ()
                 "Write the array bytes in octal."
                 (setf *radix* 8))
         (option ("hexa" "-x" "--hexa" "--hexadecimal") ()
                 "Write the array bytes in hexadecimal (default)."
                 (setf *radix* 16))
         (option ("byte-type" "-bt" "--byte-type") (type)
                 "C type used for the array elements (default: unsigned char)."
                 (setf *byte-type* type))
         (option ("size-type" "-st" "--size-type") (type)
                 "C type used for the array size constant (default: unsigned int)."
                 (setf *size-type* type)))

(defun main (arguments)
  (setf *radix* 16  *byte-type* "unsigned char"  *size-type* "unsigned int")
  (let ((operands '()))
    (parse-options *command* arguments nil
                   (lambda (arg rest)
                     (if (string= arg "--")
                         (progn (setf operands (revappend rest operands)) '())
                         (progn (push arg operands) rest))))
    (let ((files (input-files (nreverse operands) :standard-input nil)))
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
                                :radix *radix*
                                :byte-type *byte-type*
                                :size-type *size-type*)))))
  ex-ok)

;;;; THE END ;;;;
