;;;; -*- mode:lisp; coding:utf-8 -*-

;;----------------------------------------------------------------------

(defun optionp (options key)
  (if (atom options)
      (string= options key)
      (member key options :test (function string=))))

(defun opts (name)
  (list (format nil "-~A" (char (string name) 0))
        (format nil "--~(~A~)" name)
        (format nil "~(~A~)" name)))

;;----------------------------------------------------------------------

(defconstant +day+ (* 24 60 60))

(defun ut-from-date (yyyy-mm-dd)
  (multiple-value-bind (y p)
      (parse-integer yyyy-mm-dd :junk-allowed t :start 0)
    (multiple-value-bind (m p)
        (parse-integer yyyy-mm-dd :junk-allowed t :start (1+ p))
      (multiple-value-bind (d p)
          (parse-integer yyyy-mm-dd :junk-allowed nil :start (1+ p))
        (encode-universal-time 0 0 0 d m y)))))

(defun date-label (ut)
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time ut)
    (format nil "~4,'0D-~2,'0D-~2,'0D" ye mo da)))

(defun days-from-duration (n units)
  (* n
     (cond
       ((optionp '("day"   "days")   units)   1)
       ((optionp '("week"  "weeks")  units)   7)
       ((optionp '("month" "months") units)  30)
       ((optionp '("year"  "years")  units) 365)
       (t (error "Invalid time unit: ~S" units)))))

(defun duration-label (days)
  (cond
    ((zerop (mod days 365)) (format nil "~4D year~:*~P"   (truncate days 365)))
    ((zerop (mod days  30)) (format nil "~4D month~:*~P"  (truncate days  30)))
    ((zerop (mod days   7)) (format nil "~4D week~:*~P"   (truncate days   7)))
    (t                      (format nil "~4D day~:*~P"    days))))

(defun test-durations ()
  (dolist (test
            (mapcan (lambda (n) (mapcar (lambda (u) (list n u))
                                   '("day" "days" "week" "weeks" "month" "months"
                                     "year" "years" "furlong")))
                    '(0 1 6 7 8 13 14 15 29 30 31 59 60 61 364 365 366
                      729 730 731)))
    (let* ((r1 (ignore-errors
                 (days-from-duration (first test) (Second test))))
           (r2 (when r1 (duration-label r1))))
      (print `(,test ,r1 ,r2)))))


;;----------------------------------------------------------------------


(defparameter *memo-dir* (merge-pathnames ".memo/" (user-homedir-pathname))
  "Where the memos are stored.")


(defun print-usage ()
  (format t
    "~&usage:~
     ~%~:{~%    memo ~{~A~^|~} ~A~}~
     ~%~
     ~%  date:     YYYY-MM-DD or now~
     ~%  duration: N  (day|week|month|year)[s]~
     ~2%"
    `((,(opts "help")   "")
      (,(opts "list")   "")
      (,(opts "show")   "")
      (,(opts "add")    "[from $date [for $duration]] $text $of $the $memo.")
      (,(opts "remove") "$memonum")
      (,(opts "purge")  ""))))


;;memo add from now do something
;;memo add from 2006-03-15 aniversaire
;;memo add from 2006-03-15 for 1 (day|week|month|year)[s]  aniversaire


(defstruct (memo (:type list))
  pathname
  date
  duration
  lines)


(defun memo-path (num)
  (make-pathname :name num :type "MEMO" :case :common :defaults *memo-dir*))
(defun memo-files ()
  (directory (memo-path :wild)))
(defun memo-new-path ()
  (loop
     :for num :from 0 :to 999
     :do (let ((path  (memo-path (format nil "~3,'0D" num))))
           (unless (probe-file path)
             (return-from memo-new-path path))))
  (error "Too many memos. Purge or remove some."))


(defun load-memos ()
  (mapcar (lambda (file)
            (with-open-file (memo file)
              (make-memo :pathname file
                         :date (read memo)
                         :duration (read memo)
                         :lines (loop :for line = (read-line memo nil nil)
                                   :while line :collect line))))
          (memo-files)))


(defun save-memo (memo)
  (with-open-file (file (memo-pathname memo)
                        :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (format file "~S ~S~%" (memo-date memo) (memo-duration memo))
    (format file "~&~{~A~%~}" (memo-lines memo))))


(defun list-memos ()
  (dolist (memo (sort (load-memos) (function string<=)
                      :key (lambda (memo) (namestring (memo-pathname memo)))))
    (format t "~3:A~:[~; from ~:*~10A~]~:[~; for ~:*~10A~]~
               ~2:*~:[~;~%   ~]~* ~A~:[~;...~]~%"
            (pathname-name (memo-pathname memo))
            (when (memo-date memo) (date-label (ut-from-date (memo-date memo))))
            (when (memo-duration memo) (duration-label (memo-duration memo)))
            (first (memo-lines memo))
            (rest  (memo-lines memo)))))


(defun show-memos ()
  (dolist (memo (sort
                 (let ((today (get-universal-time)))
                   (remove-if
                    (lambda (memo)
                      (and (memo-date memo)
                           (or (< today (ut-from-date (memo-date memo)))
                               (and (memo-duration memo)
                                    (< (+ (ut-from-date (memo-date memo))
                                          (* +day+ (memo-duration memo)))
                                       today)))))
                    (load-memos)))
                 (function string<=)
                 :key (lambda (memo) (namestring (memo-pathname memo)))))
    (format t "~3:A ~{~A~^~%    ~}~%"
            (pathname-name (memo-pathname memo))
            (memo-lines memo))))


(defun purge-memos ()
  (mapc (lambda (memo) (delete-file (memo-pathname memo)))
        (let ((today (get-universal-time)))
          (remove-if
           (lambda (memo)
             (not (and (memo-date memo)
                       (memo-duration memo)
                       (< (+ (ut-from-date (memo-date memo))
                             (* +day+ (memo-duration memo)))
                          today))))
           (load-memos)))))


(defun remove-memo (num)
  (let ((num (handler-case (parse-integer num :junk-allowed nil)
               (error (err) (error "Invalid memo number ~S" num)))))
    (delete-file (memo-path (format nil "~3,'0D" num)))))

(defun add-memo (text)
  (let (date duration)
    (when (optionp "from" (first text))
      (pop text)
      (setf date (pop text))
      (ut-from-date date)
      (when (optionp "for" (first text))
        (pop text)
        (setf duration (days-from-duration (parse-integer (pop text)
                                                          :junk-allowed nil)
                                           (pop text)))))
    (let ((text (format nil "~{~A~^ ~}" text))
          (path (memo-new-path)))
      (save-memo (make-memo :pathname path
                            :date date
                            :duration duration
                            :lines (list text))))))

(defun main (argv)
  (format t "~&")
  (ensure-directories-exist (make-pathname :name "TEST" :defaults *memo-dir*))
  (let ((key (first argv)))
    (handler-case
        (COND
          ((or (null argv) (optionp (opts "help") key))  (print-usage))
          ((optionp (opts "list")   key)     (list-memos))
          ((optionp (opts "show")   key)     (show-memos))
          ((optionp (opts "add")    key)     (add-memo     (rest   argv)))
          ((optionp (opts "remove") key)     (remove-memo  (second argv)))
          ((optionp (opts "purge")  key)     (purge-memos))
          (t  (format *error-output* "Invalid option: ~S~%" key)
              (print-usage)))
      (error (err)
        (format *error-output* "~&~A~%" err)))))


(main ext:*args*)

