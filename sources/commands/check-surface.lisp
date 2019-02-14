;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:              check-surface
;;;;LANGUAGE:          common lisp (clisp)
;;;;SYSTEM:            UNIX
;;;;USER-INTERFACE:    UNIX
;;;;DESCRIPTION
;;;;
;;;;    This script tests a block device, block by block, creating a map
;;;;    of bad sectors.
;;;;
;;;;USAGE
;;;;    check-surface /dev/sda
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2007-09-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Copyright Pascal J. Bourguignon 2007 - 2007
;;;;
;;;;    This script is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU  General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This script is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;    General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this library; see the file COPYING.LIB.
;;;;    If not, write to the Free Software Foundation,
;;;;    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;****************************************************************************

(command :use-systems (:com.informatimago.clmisc)
         :use-packages ("COM.INFORMATIMAGO.CLMISC.RESOURCE-UTILIZATION"))

(defun usage ()
  (format
   t (concatenate
         'string
       "~%"
       "~a usage:~%"
       "~%"
       "    ~:*~a [options...] BLOCK_DEVICE ~%"
       "~%"
       "Options:~%"
       " -h|--help    Prints this usage documentation.~%"
       " --block-size=SIZE |~%"
       " -b SIZE      Indicate a block size. By default, stat -c %o is used.~%"
       " --start=POSITION |~%"
       " -s POSITION  Indicate the start block number. By default: 0.~%"
       " --end=POSITION |~%"
       " -e POSITION  Indicate the end block number. By default: NIL (end of device).~%"
       "~%"
       "Tries to read all the blocks of the device, then if no error is encountered,~%"
       "tries to write all the blocks of the device, then if no error is encountered,~%"
       "tries to read the written blocks, and reports a list of bad blocks.~%"
       "~%"
       "Output bad block list is given in block units.~%"
       "~%")
   *program-name*))


(defun report-error (message &optional print-usage-p (status nil statusp))
  (format *error-output*
          "~%~a: ~A~:[~; Aborting.~]~%"
          *program-name* message (and statusp (not (zerop status))))
  (when print-usage-p
    (usage))
  (when statusp
    #+testing (throw 'exit status)
    #-testing (exit status)))


(defun geometry (device)
  "
RETURN: number of sectors per disk;
        number of cylinders per disk;
        number of tracks per cylinder;
        number of sector per track;
        start sector.
"
    (with-open-stream (inp (uiop:run-program
                            (list "hdparm" "-g"
                                           (etypecase device
                                             (string device)
                                             (pathname (namestring device))))
                          :output :stream))
    (let ((*read-eval* nil)
          (*read-base* 10.))
      (loop
         :with cylinders = nil
         :with heads = nil
         :with sectors = nil
         :with total-sectors = nil
         :with start = nil
         :for line = (read-line inp nil nil)
         :while line
         :until (eq 'geometry (ignore-errors (read-from-string line)))
         :finally (let ((pos (position #\= line)))
                    (unless pos
                      (error "unexpected format for hdparm -g output: ~S" line))
                    (incf pos)
                    (multiple-value-setq (cylinders pos)
                      (parse-integer line :start pos :junk-allowed t))
                    (incf pos)
                    (multiple-value-setq (heads pos)
                      (parse-integer line :start pos :junk-allowed t))
                    (incf pos)
                    (multiple-value-setq (sectors pos)
                      (parse-integer line :start pos :junk-allowed t))
                    (setf pos (position #\= line :start pos))
                    (unless pos
                      (error "unexpected format for hdparm -g output: ~S" line))
                    (incf pos)
                    (multiple-value-setq (total-sectors pos)
                      (parse-integer line :start pos :junk-allowed t))
                    (incf pos)
                    (setf pos (position #\= line :start pos))
                    (unless pos
                      (error "unexpected format for hdparm -g output: ~S" line))
                    (incf pos)
                    (multiple-value-setq (start pos)
                      (parse-integer line :start pos :junk-allowed t))
                    (return-from geometry
                      (values total-sectors
                              cylinders heads sectors start)))))))


(defun block-size (device)
  (with-open-stream (inp (uiop:run-program
                          (list "stat" "-c" "%o"
                                (etypecase device
                                  (string device)
                                  (pathname (namestring device))))
                          :output :stream))
    (let ((*read-eval* nil)
          (*read-base* 10.))
      (or (read inp nil nil)
          (report-error "Cannot get the block-size from stat(1)."
                        nil  ex-unavailable)))))


(defun parse-block-size (string)
  ;; (print string)
  (multiple-value-bind (block-size end)
      (parse-integer string :junk-allowed t :radix 10.)
    (* block-size
       (block factor
         (if (< end (length string))
             (when (<= (length string) (1+ end))
               (let ((factor (position (aref string end) "KMGTPEZY"
                                       :test (function char-equal))))
                 (when factor
                   (return-from factor (expt 1024 (1+ factor))))
                 (error "Invalid block factor: ~A" (subseq string end))))
             (return-from factor 1))))))


(defparameter *element-type*    '(unsigned-byte 8))
(defparameter *external-format* :default)



(defun test!read-all (device buffer
                      fpos-generator block-checker
                      reporter)
  "
DEVICE:           A STREAM, open on the device to be checked.

BUFFER:           A vector used as a block buffer.

FPOS-GENERATOR:   A (FUNCTION ((OR NULL INTEGER)) (OR NULL INTEGER))
                  returning a sequence of file position. The first time,
                  it's called as (fpos-generator nil), and then it's
                  called as (fpos-generator previous-fpos), until it
                  returns NIL.  The whole sequence can then be repeated.

BLOCK-CHECKER:    A (FUNCTION (INTEGER INTEGER SEQUENCE) (or null string))
                  called to check that the buffer read back is the
                  same as the buffer that was written at the given
                  file position. The second argument is the result of
                  READ-SEQUENCE: END-OF-FILE => 0, <block-size => error.
                  Note: some file positions can be skipped (BLOCK-CHECKER
                  not called) if no block could be read from there.
                  The result is passed to the reporter.

REPORTER:         A (FUNCTION (INTEGER (OR NULL STRING)))
                  called to report on the success of the block
                  at the given file position. The second argument is
                  NIL when the block is valid, and contains an error
                  message when the block failled.

RESULTS:          The number of failed blocks;
                  the number of valid blocks;
                  the total number of blocks;
                  the number of seek failed;
                  the number of read failed.
"
  (let ((valid-blocks  0)
        (failed-reads  0)
        (failed-seeks  0)
        (total-number-of-blocks 0))
    ;; read all:
    (loop
       :for i :from 0
       :for fpos = (funcall fpos-generator i)
       :while fpos
       :do (incf total-number-of-blocks)
       :do (multiple-value-bind (success error)
               (ignore-errors (file-position device fpos))
             ;; (print `(file-position ,fpos -> ,success ,error))
             (if success
                 (multiple-value-bind (size-read error)
                     (ignore-errors (read-sequence buffer device))
                   ;; (print `(read-sequence -> ,size-read ,error))
                   (if error
                       (progn
                         (incf failed-reads)
                         (funcall reporter fpos
                                  (format nil
                                          "READ-SEQUENCE failed~:[~;~:*~%~A~]"
                                          error)))
                       (progn
                         (incf valid-blocks)
                         (funcall reporter fpos
                                  (funcall block-checker
                                           fpos size-read buffer)))))
                 (progn
                   (incf failed-seeks)
                   (funcall reporter fpos
                            (format nil "FILE-POSITION failed~:[~;~:*~%~A~]"
                                    error))))))
    (values (- total-number-of-blocks valid-blocks)
            valid-blocks
            total-number-of-blocks
            failed-seeks
            failed-reads)))


(defun test!write-all/read-all (device buffer
                                fpos-generator block-generator block-checker
                                write-reporter read-reporter
                                &optional (phase :both))
  "
DEVICE:           A STREAM, open on the device to be checked.

BUFFER:           A vector used as a block buffer.

FPOS-GENERATOR:   A (FUNCTION ((OR NULL INTEGER)) (OR NULL INTEGER))
                  returning a sequence of file position. The first time,
                  it's called as (fpos-generator nil), and then it's
                  called as (fpos-generator previous-fpos), until it
                  returns NIL.  The whole sequence can then be repeated.

BLOCK-GENERATOR:  A (FUNCTION (INTEGER SEQUENCE))
                  called to fill the buffer to be writen at the
                  given file position.  The result is ignored.
                  Note: some file positions can be skipped if
                  the file position cannot be reached on the device.

BLOCK-CHECKER:    A (FUNCTION (INTEGER INTEGER SEQUENCE) (or null string))
                  called to check that the buffer read back is the
                  same as the buffer that was written at the given
                  file position. The second argument is the result of
                  READ-SEQUENCE: END-OF-FILE => 0, <block-size => error.
                  Note: some file positions can be skipped (BLOCK-CHECKER
                  not called) if no block could be read from there.
                  The result is passed to the reporter.

REPORTER:         A (FUNCTION (INTEGER (OR NULL STRING)))
                  called to report on the success of the block
                  at the given file position. The second argument is
                  NIL when the block is valid, and contains an error
                  message when the block failled.

RESULTS:          The number of failed blocks;
                  the number of valid blocks;
                  the total number of blocks;
                  the number of seek failed;
                  the number of read failed;
                  the number of write failed.
"
  (let ((failed-seeks                0)
        (failed-writes               0)
        (read/failed-blocks          0)
        (read/valid-blocks           0)
        (read/total-number-of-blocks 0)
        (read/failed-seeks           0)
        (read/failed-reads           0))
    (declare (ignorable read/failed-blocks))
    (when (member phase '(:both :write))
      ;; write all:
      (loop
         :for i :from 0
         :for fpos = (funcall fpos-generator i)
         :while fpos
         :do (multiple-value-bind (success error)
                 (ignore-errors (file-position device fpos))
               (if success
                   (progn
                     (funcall block-generator fpos buffer)
                     (multiple-value-bind (result error)
                         (ignore-errors (write-sequence buffer device))
                       (declare (ignore result))
                       (if error
                           (progn
                             (incf failed-writes)
                             (funcall write-reporter fpos
                                      (format nil
                                              "WRITE-SEQUENCE failed~:[~;~:*~%~A~]"
                                              error)))
                           (funcall write-reporter fpos nil))))
                   (progn
                     (incf failed-seeks)
                     (funcall write-reporter fpos
                              (format nil "FILE-POSITION failed~:[~;~:*~%~A~]"
                                      error)))))))
    (when (member phase '(:both :read))
      (multiple-value-setq (read/failed-blocks
                            read/valid-blocks
                            read/total-number-of-blocks
                            read/failed-seeks
                            read/failed-reads)
        (test!read-all device buffer fpos-generator block-checker read-reporter)))
    (values (- read/total-number-of-blocks read/valid-blocks)
            read/valid-blocks
            read/total-number-of-blocks
            (+ read/failed-seeks failed-seeks)
            read/failed-reads
            failed-writes)))




(defun make-fpos-generator/sequential (block-size start end)
  (if end
      (let ((endi (- end start)))
        (lambda (i)
          (when (< i endi)
            (* (+ start i) block-size))))
      (lambda (i)
         (* (+ start i) block-size))))


(defun zeros (length)
  (make-array length
              :element-type '(unsigned-byte 32)
              :initial-element 0))

(defun iota (vector)
  (dotimes (i (length vector) vector)
    (setf (aref vector i) i)))

(defun random-permutation (vector)
  (dotimes (i (- (length vector) 2) vector)
    (rotatef (aref vector i)
             (aref vector (+ i (random (- (length vector) i)))))))

(defun make-fpos-generator/random (block-size start end)
  (let* ((endi (- end start))
         (p (let ((indices (random-permutation (iota (zeros endi)))))
              (map-into indices (lambda (x)  (* (+ start x) block-size)) indices))))
    (lambda (i)
      (when (< i endi)
        (aref p i)))))


(defun make-reporter (block-size start end
                      my-failure bad good add-to-bad-block-list)
  (let ((ubad (string-upcase bad))
        (dbad (string-downcase bad)))
    (lambda (fpos failure)
      (let ((block-no (- (truncate fpos block-size) start)))
        (when (zerop (mod (+ start block-no) (* 64 16)))
          (format t "~%[~D -> ~D -> ~D[ ;  ~D/~D = ~2$ %"
                  start (+ start block-no) end
                  block-no (- end start) (* 100 (/ block-no (- end start)))))
        (if failure
            (progn
              (funcall add-to-bad-block-list block-no)
              (when (zerop (mod (+ start block-no) 64))
                (format t "~%~8D: " (+ start block-no)))
              (if (eq failure my-failure)
                  (princ dbad)
                  (princ ubad)))
            (progn
              (when (zerop (mod (+ start block-no) 64))
                (format t "~%~8D: " (+ start block-no)))
              (princ good)))))))




(defun test$read-all$?access (device-path block-size buffer
                              access-constructor
                              &key (start 0) (end nil))
  (with-open-file (device device-path
                          :direction         :io
                          :element-type      *element-type*
                          :external-format   *external-format*
                          :if-does-not-exist :error)
    (let ((bad-blocks '())
          (my-failure (copy-seq "Block could not be read entirely.")))
      (test!read-all
       device buffer
       (funcall access-constructor block-size start end)
       (lambda (fpos size-read buffer)       ; BLOCK-CHECKER
         (declare (ignore fpos buffer))
         (when (< size-read block-size)
           my-failure))
       (make-reporter block-size start end my-failure "r" "." ; READ-REPORTER
                      (lambda (block-no) (push block-no bad-blocks)))))))


(defun test$read-all$sequential (device-path block-size buffer
                                 &key (start 0) (end nil))
  (test$read-all$?access device-path block-size buffer
                         (function make-fpos-generator/sequential)
                         :start start :end end))


(defun test$read-all$random (device-path block-size buffer
                             &key (start 0) (end nil))
  (test$read-all$?access device-path block-size buffer
                         (function make-fpos-generator/random)
                         :start start :end end))


(defun test$write-all/read-all$sequential (device-path block-size buffer
                                           &key (start 0) (end nil)
                                           (phase :both))
  (with-open-file (device device-path
                          :direction         :io
                          :element-type      *element-type*
                          :external-format   *external-format*
                          :if-does-not-exist :error)
    (let ((bad-blocks '())
          (my-failure (copy-seq "Block could not be read entirely.")))
      (test!write-all/read-all
       device buffer
       (make-fpos-generator/sequential block-size start end)

       (lambda (fpos buffer)                 ; BLOCK-GENERATOR
         (loop
            :for i :from 0 :below block-size
            :do (setf (aref buffer i) (mod (+ i fpos) 256)))
         (replace buffer
                  (map 'vector (function char-code)
                       (format nil "*** BLOCK AT POSITION: ~8,'0X ***" fpos))
                  :start1 128)
         buffer)

       (lambda (fpos size-read buffer)       ; BLOCK-CHECKER
         (block checker
           (unless (= size-read block-size)
             (return-from checker "Buffer not read back completely."))
           (let ((pattern
                  (map 'vector (function char-code)
                       (format nil "*** BLOCK AT POSITION: ~8,'0X ***" fpos))))
             (unless (equalp (subseq buffer 128 (+ 128 (length pattern)))
                             pattern)
               (return-from checker
                 (format nil "Identifying pattern not present in block at ~D"
                         fpos)))
             (loop
                :for i :from 128 :below  (+ 128 (length pattern))
                :do (setf (aref buffer i) (mod (+ i fpos) 256))))
           (unless (loop
                      :for i :from 0 :below (length buffer)
                      :always (= (aref buffer i) (mod (+ i fpos) 256)))
             (return-from checker
               (format nil "Pattern doesn't match for block at ~D" fpos)))))

       (make-reporter block-size start end my-failure "w" "."
                      (lambda (block-no) (push block-no bad-blocks)))
       (make-reporter block-size start end my-failure "r" "."
                      (lambda (block-no) (push block-no bad-blocks)))
       phase))))


(defun check-surface (device block-size start end phase)
  (let ((buffer (make-array block-size :element-type *element-type*
                                       :initial-element 0))
        (end (or end (truncate (* 512 (geometry device)) block-size))))
    (print
     (multiple-value-list
      #-(and)
      (test$read-all$sequential device block-size buffer
                                :start (- end 512) :end (1+ end))
      #-(and)
      (test$read-all$sequential device block-size buffer :start 0 :end 1024)
      #-(and)
      (test$read-all$random device block-size buffer :start 0 :end 1024)
      (test$write-all/read-all$sequential device block-size buffer
                                          :start start :end end
                                          :phase phase)))))


(defun main (arguments)
  (reporting-sru
      (:job-origin
       (format nil "~A@~A"
               (or (uiop:getenv "USER")
                   (first (last (pathname-directory
                                 (user-homedir-pathname)))))
               (short-site-name))
       :stream *standard-output*)
    (catch 'exit

      (let ((files      '())
            (block-size nil)
            (device     nil)
            (start      0)
            (end        nil)
            (phase      :both))
        (loop
          :with args = arguments
          :for arg = (car args)
          :while args
          :do (cond ((or (string= "-h" arg) (string= "--help" arg))
                     (usage)
                     (throw 'exit ex-ok))
                    ((string= arg "-b")
                     (setf block-size (parse-block-size (cadr args)))
                     (pop args) (pop args))
                    ((string= arg #1="--block-size=")
                     (setf block-size (parse-block-size (subseq arg (length #1#))))
                     (pop args))
                    ((string= arg "-s")
                     (setf start (parse-block-size (cadr args)))
                     (pop args) (pop args))
                    ((string= arg #2="--start=")
                     (setf start (parse-block-size (subseq arg (length #2#))))
                     (pop args))
                    ((string= arg "-e")
                     (setf end (parse-block-size (cadr args)))
                     (pop args) (pop args))
                    ((string= arg #3="--end=")
                     (setf end (parse-block-size (subseq arg (length #3#))))
                     (pop args))
                    ((string= arg "--only-write")
                     (setf phase :read)
                     (pop args))
                    ((string= arg "--check")
                     (setf phase :write)
                     (pop args))
                    ((string= arg "--read")
                     (error "not implemented yet")
                     (pop args))
                    (t
                     (push arg files)
                     (pop args))))
        (unless files
          (report-error "Nothing to do. Please specify -h or --help." t ex-usage))
        (case  (length files)
          ((0)
           (report-error "No device to work on." t ex-usage))
          ((1)
           (setf device (pop files))
           (setf block-size (or block-size (block-size device)))
           (check-surface device block-size start end phase))
          (otherwise
           (report-error "Too many device arguments given, only one expected."
                         t ex-usage)))))))

(defun m () (main '("-e" ".")))
;;;; THE END ;;;;;
