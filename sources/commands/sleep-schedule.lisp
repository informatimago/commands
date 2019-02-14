;;;; -*- mode:lisp; coding:utf-8 -*-

(defvar *schedule-file*)


(defconstant 24h (* 24 60 60))


(defun days (d) (* d 24h))

(defun date-  (d1 d2)  (-  d1 d2))
(defun date=  (d1 d2)  (=  d1 d2))
(defun date>= (d1 d2)  (>= d1 d2))
(defun date<= (d1 d2)  (<= d1 d2))


(defun sunday-p (dt)
  (= 6 (nth-value 6 (decode-universal-time dt))))

(defstruct (entry  (:type list))
  switch year month day hour minute second zone comment)

(defun entry-universal-time (entry)
  (encode-universal-time (entry-second entry)
                         (entry-minute entry)
                         (entry-hour   entry)
                         (entry-day    entry)
                         (entry-month  entry)
                         (entry-year   entry)
                         (entry-zone   entry)))

(defun entry-time (entry)    (mod      (entry-universal-time entry) 24h))
(defun entry-date (entry) (* (truncate (entry-universal-time entry) 24h) 24h))


(defun last-date (schedule)
  (reduce (function max) (mapcar (function entry-date) schedule)))


(defun print-schedule (schedule &key (stream *standard-output*)
                       (height 72) (days nil) (append-date nil))
  "
DO:     Prints a graph HEIGHT characters wide, for the whole schedule
        if DAYS is nil, or only for the DAYS last days.
"
  (when days
    (setf schedule (remove (date- (last-date schedule) (days days))
                           schedule
                           :key  (function entry-date)
                           :test (function date>=))))
  (loop
     :initially (format t "~%~VA~VA~VA~VA~%"
                        (/ height 4) "UTC:" (/ height 4) "6H"
                        (/ height 4) "12H" (/ height 4) "18H")
     :with line = (make-string height)
     :for date = (and schedule (entry-date (car schedule))) then (+ date 24h)
     :while schedule
     :do (loop
            :initially (fill line #\ )
            :with start = 0
            :with next-date = (entry-date (car schedule))
            :for sleep-p = (if schedule
                               (eq :stop (entry-switch (car schedule)))
                               (not sleep-p))
            :while (and schedule
                        (date= date next-date)
                        (date= next-date (entry-date (car schedule))))
            :do (let ((end (round (/ (* height (entry-time (car schedule)))
                                     24h))))
                  ;; (print (list (car schedule) start end sleep-p)) (terpri)
                  (fill line (if sleep-p #\Z #\ ) :start start :end end)
                  (setf start end)
                  (pop schedule))
            :finally (progn
                       ;;(setf sleep-p (not sleep-p))
                       ;; (print (list start sleep-p)) (terpri)
                       (fill line (if sleep-p #\Z #\ ) :start start)
                       (let ((mark (if (sunday-p date) #\+ #\|)))
                         (setf (aref line (round (* 1/4 height))) mark
                               (aref line (round (* 2/4 height))) mark
                               (aref line (round (* 3/4 height))) mark))
                       (princ line stream)
                       (when append-date
                         (multiple-value-bind (sec min hou day mon yea)
                             (decode-universal-time date)
                           (declare (ignore sec min hou))
                           (format stream "~4,'0D~2,'0D~2,'0D" yea mon day)))
                       (terpri stream)))
     :finally (terpri stream)))


(defun read-schedule (file)
  (sort (with-open-file (input file)
          (loop
             :for entry = (read input nil nil)
             :while entry
             :collect entry))
        (function <=)
        :key (function entry-universal-time)))


(defun square (x) (* x x))

(defun sum (sequence &key (key (function identity)))
  (if (listp sequence)
      (loop :for item :in     sequence :sum (funcall key item))
      (loop :for item :across sequence :sum (funcall key item))))

(defun mean (sequence &key (key (function identity)))
  (/ (sum sequence :key key) (length sequence)))

(defun variance (sequence &key (key (function identity)))
  (let ((mean (mean sequence :key key)))
    (/ (sum sequence :key (lambda (item) (square (- (funcall key item) mean))))
       (length sequence))))

(defun ecart-type  (sequence &key (key (function identity)))
  (sqrt (variance sequence :key key)))


(defun covariance (sequence &key (x (function first)) (y (function second)))
  (let ((mean-x (mean sequence :key x))
        (mean-y (mean sequence :key y)))
    (/ (sum sequence :key (lambda (item) (* (- (funcall x item) mean-x)
                                       (- (funcall y item) mean-y))))
       (length sequence))))

(defun regression-lineaire
    (sequence &key (x (function first)) (y (function second)))
  (let ((cov (covariance sequence :x x :y y))
        (var (variance   sequence :key x))
        (mean-x (mean sequence :key x))
        (mean-y (mean sequence :key y)))
    (list (/ cov var) (- mean-y (* mean-x (/ cov var))))))


#||
(mapcar
 (lambda (x) (coerce x 'float))
 (regression-lineaire
  (mapcar
   (lambda (time) (cons (truncate (cdr time) 24h) (mod (cdr time) 24h)))
   (delete :stop
           (mapcar
            (lambda (sched) (cons (car sched) (entry-universal-time sched)))
            (last (read-schedule  *schedule-file*) (* 8 2)))
           :key (function car)))
  :x (function car) :y (function cdr)))
||#

(defun main (arguments)
  (declare (ignore arguments))

  (setf *schedule-file* (merge-pathnames "./.sleep-schedule" (user-homedir-pathname) nil))

  (print-schedule (read-schedule  *schedule-file*) :append-date t)

  (let* ((schedule  (mapcar
                     (lambda (sched) (cons (car sched) (entry-universal-time sched)))
                     (read-schedule  *schedule-file*)))
         (times    (mapcar (function cdr) schedule))
         (start    (truncate (apply (function min) times) 24h))
         (end      (1+ (truncate (apply (function max) times) 24h))))
    (print `(day length
                 ,@(mapcar
                    (lambda (x) (/ x 60.0 60.0))
                    ((lambda (s)
                       (list (/ (reduce (function +) s) (length s))
                             (apply (function min) s)
                             (apply (function max) s)))
                     ((lambda (x) (mapcar (function -) (cdr x) x))
                      (mapcar (function cdr)
                              (delete :stop schedule :key (function car))))))))
    (print `(sleep time
                   ,@((lambda (s)
                        (list (/ (reduce (function +) s) (- end start))
                              (apply (function min) s)
                              (apply (function max) s)))
                      (mapcar
                       (lambda (x) (/ x 60.0 60.0))
                       (loop
                         :for (s e) :on times :by (function cddr)
                         :when e :collect (- e s)))))))

  ex-ok)

;;;; THE END ;;;;
