;;;; -*- mode:lisp; coding:utf-8 -*-
(let* ((*standard-output* (make-broadcast-stream)))
 (load #P"~/quicklisp/setup.lisp"))
(let* ((*standard-output* (make-broadcast-stream)))
 (ql:quickload :split-sequence :verbose nil)
 (ql:quickload :com.informatimago.common-lisp.cesarum :verbose nil))
(in-package "COMMON-LISP-USER")
(defpackage "KWIC"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "SPLIT-SEQUENCE"))
(in-package "KWIC")

(defun wl-lessp (a b)
  (cond
    ((null a)
     b)
    ((null b)
     nil)
    ((string= (first a) (first b))
     (wl-lessp (rest a) (rest b)))
    ((string< (first a) (first b))
     t)
    (t
     nil)))

(defun test/wl-lessp ()
  (assert (not (wl-lessp '() '())))
  (assert (wl-lessp '() '("abc")))
  (assert (not (wl-lessp '("abc") '())))
  (assert (not (wl-lessp '("abc") '("abc"))))
  (assert (wl-lessp '("aaa") '("abc")))
  (assert (not (wl-lessp '("abc") '("aaa"))))

  (assert (wl-lessp '("aaa" "bbb") '("bbb" "ccc")))
  (assert (wl-lessp '("aaa" "bbb") '("bbb" "aaa")))
  (assert (not (wl-lessp '("bbb" "ccc")  '("aaa" "bbb"))))
  (assert (not (wl-lessp '("bbb" "aaa")  '("aaa" "bbb"))))

  (assert (not (wl-lessp '("zzz" ) '("zzz" ))))
  (assert (wl-lessp '("zzz" ) '("zzz" "abc")))
  (assert (not (wl-lessp '("zzz" "abc") '("zzz" ))))
  (assert (not (wl-lessp '("zzz" "abc") '("zzz" "abc"))))
  (assert (wl-lessp '("zzz" "aaa") '("zzz" "abc")))
  (assert (not (wl-lessp '("zzz" "abc") '("zzz" "aaa"))))

  (assert (wl-lessp '("a" "b" "a") '("a" "b" "c")))
  (assert (not (wl-lessp '("a" "b" "c") '("a" "b" "a"))))
  (assert (not (wl-lessp '("a" "b" "c") '("a" "b" "c"))))
  (assert (wl-lessp '("a" "b") '("a" "b" "c")))
  (assert (not (wl-lessp '("a" "b" "c")  '("a" "b"))))
  (assert (wl-lessp '("a" "b" "c")  '("b" "c")))
  (assert (not (wl-lessp  '("b" "c")  '("a" "b" "c"))))
  :success)

(test/wl-lessp)

(dolist (line (sort
               (mapcan (lambda (line)
                         (loop
                           :with len = (length line)
                           :with 2line = (append line line)
                           :for tail :on (nthcdr len 2line)
                           :for head :on 2line
                           :collect (ldiff head tail)))
                       (remove nil
                               (mapcar (lambda (line)
                                         (split-sequence #\space line :remove-empty-subseqs t))
                                       (stream-to-string-list *standard-input*))))
               (function wl-lessp)))
  (format t "~{~A~^ ~}~%" line))
