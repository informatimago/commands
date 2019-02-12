;;;; -*- mode:lisp; coding:utf-8 -*-

;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP")
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))



(defparameter *sentences*
  '("It works on my machine."
    "It’s not a bug, it’s a feature."
    "How hard can it be?"
    "How did that even work in the first place?"
    "I don’t see anything in the logs."
    "I’m guessing it’s an issue on their side."
    "Probably some kind of permissions issue."
    "That’s weird."
    "That’s a known issue."
    "We don’t support that."
    "No, I don’t know how long it’s going to take."
    "Actually, it has always been like that."
    "Have you tried it with Chrome or Firefox?"
    "It should work now."
    "But that’s not how you told me it should work."
    "Well there’s your problem right there."
    "Working as intended."
    "I mean, everything’s *possible*…"
    "That will never happen."
    "Shouldn’t take too long."
    "Cheap, fast, high quality. Pick any two."
    "It’s 90% done."
    "This is just a temporary fix"
    "You’re doing it wrong"
    "That’s a code smell."
    "I thought we fixed this!"
    "Yes, but does it scale?"))

(defparameter *gn*
  '(
    "foobar"
    "Grok"
    "Rabbit hole"
    "Turtles all the way down"
    "Yak shaving"
    "Bikeshedding"
    "Automagically"
    "Performant"
    "+1"
    "TODO"
    ))




(defun one-of (s) (elt s (random (length s))))

(setf *random-state* (make-random-state t))
(princ (one-of *sentences*)) (terpri)

;;;; THE END ;;;;
