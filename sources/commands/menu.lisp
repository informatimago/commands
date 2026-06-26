;;;; -*- mode:lisp; coding:utf-8 -*-

(command :version "0.0.2"
         :documentation "Display a numbered menu of the ITEM arguments on stderr,
read a selection number from stdin, and print the chosen item on stdout.
With this separation the chosen item can be captured: choice=$(menu a b c).")

(options "menu" (standard-options))

(defun display-menu (items)
  "Print the numbered ITEMS and a prompt on *ERROR-OUTPUT*."
  (loop :for i :from 1
        :for item :in items
        :do (format *error-output* "~2D) ~A~%" i item))
  (format *error-output* "Choice? ")
  (finish-output *error-output*))

(defun read-choice (items)
  "Display ITEMS and read a 1-based selection number from *STANDARD-INPUT*,
retrying on invalid input.  Return the chosen item, or NIL on end of input."
  (loop
    (display-menu items)
    (let ((line (read-line *standard-input* nil nil)))
      (cond
        ((null line)
         (return nil))
        (t
         (let ((n (parse-integer line :junk-allowed t)))
           (if (and n (<= 1 n (length items)))
               (return (nth (1- n) items))
               (format *error-output* "~&Invalid choice: ~A~%" line))))))))

(defun main (arguments)
  (let ((items '()))
    (parse-options *command* arguments nil
                   (lambda (arg rest)
                     (cond
                       ((string= arg "--")
                        (setf items (revappend rest items)) '())
                       ((and (plusp (length arg)) (char= (char arg 0) #\-))
                        (error "Invalid argument: ~A" arg))
                       (t (push arg items) rest))))
    (setf items (nreverse items))
    (when (null items)
      (format *error-output* "~A: No menu item given. Aborting.~%" *program-name*)
      (return-from main ex-usage))
    (let ((choice (read-choice items)))
      (cond
        (choice (write-line choice) (finish-output) ex-ok)
        (t      ex-noinput)))))

;;;; THE END ;;;;
