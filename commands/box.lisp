;; -*- mode:lisp; coding:utf-8 -*-

(defun main (arguments)
  (declare (ignore arguments))
  (format t "Not implemented yet.~%")
  (finish-output)
  ex-usage)



;; "─═┌╔┬╦┐╗├╠┼╬┤╣│║└╚┴╩┘╝"
;;
;; ┌────────────────────────┬────────┬──────┐
;; │                        │        │      │
;; ├────────────────────────┼────────┼──────┤
;; │                        │        │      │
;; └────────────────────────┴────────┴──────┘
;;
;; box drawing
;; ╭──────────────────────────────────────────────────────────────────────────────╮
;; │                                                                              │
;; │                                                                              │
;; │                                                                              │
;; ╰──────────────────────────────────────────────────────────────────────────────╯

;; spaces='                                                                               '
;;
;; if [ "$1" = "-toto" ] ; then
;; 	echo '                                 \\\\\\|///                                       '
;; 	echo '                               \\\  - -  //                                     '
;; 	echo '                                (  0-0  )                                      '
;; 	echo '+-----------------------------oOOo-(_)-oOOo-----------------------------------+'
;; else
;; 	echo '+-----------------------------------------------------------------------------+'
;; fi
;;
;; while read line ; do
;; 	echo "$line$spaces$spaces" \
;; 	| expand \
;; 	| sed -e 's/^\(............................................................................\).*/| \1|/'
;; done
;;
;; if [ "$1" = "-toto" ] ; then
;; 	echo '|                             ooo0                                            |'
;; 	echo '|                            (    )   0ooo                                    |'
;; 	echo '+-----------------------------\  (----(   )-----------------------------------+'
;; 	echo '                               \_)     ) /                                     '
;; 	echo '                                      (_/                                      '
;; else
;; 	echo '+-----------------------------------------------------------------------------+'
;; fi
;;
;; exit 0

;; (load (make-pathname :name ".clisprc" :type "lisp"
;;                      :defaults (user-homedir-pathname)))

(setf (logical-pathname-translations "PACKAGES") nil)
(setf (logical-pathname-translations "PACKAGES")
      `(("PACKAGES:COM;INFORMATIMAGO;**;*.*"
         ,(merge-pathnames "src/public/lisp/**/*.*" (user-homedir-pathname) nil))
        ("PACKAGES:COM;INFORMATIMAGO;**;*"
         ,(merge-pathnames "src/public/lisp/**/*"   (user-homedir-pathname) nil))))

(load "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PACKAGE.LISP")

(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.LIST")
(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.STRING")
(import '(COM.INFORMATIMAGO.COMMON-LISP.STRING:prefixp))
(package:load-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE")
(use-package "COM.INFORMATIMAGO.COMMON-LISP.PICTURE")


(defparameter *box-path*
  (or *load-pathname*
      (make-pathname :directory (append (pathname-directory
                                         (user-homedir-pathname)) '("bin"))
                     :name "box"
                     :defaults (user-homedir-pathname)
                     :case :default)))

(defparameter *templates*
  (with-open-file (in *box-path* :direction :input :if-does-not-exist :error)
    ;; scan up to ";; DATA"
    (loop do
          (let ((line (read-line in nil nil)))
            (when (or (null line) (prefixp line ";; DATA"))  (loop-finish))))
    (macrolet ((end-block
                ()
                `(when block
                   (push (nreverse block) section)
                   (setf block nil)))
               (end-section
                ()
                `(when section
                   (push (nreverse section) sections)
                   (setf section nil))))
      (do ((line (read-line in nil nil) (read-line in nil nil))
           (sections ())
           (section ())
           (block ()))
          ((or (null line) (prefixp line ";; S END"))
           (end-block) (end-section) (nreverse sections))
        (cond
         ((prefixp line ";;  ")) ;; ignore comment
         ((prefixp line ";; S ") ;; a new section
          (end-block) (end-section)
          (push (list :title (subseq line 5)) section))
         ((prefixp line ";; T ") ;; a block
          (end-block)
          (push (list :title (subseq line 5)) block))
         ((and (prefixp line ";; ") (< 5 (length line)))
          (push (list (intern (subseq line 3 4) "KEYWORD")
                      (subseq line 5)) block)))))));;*templates*


;; Sections:
;;     HEADS
;;     FEET
;;     FRAMES


(defun get-section (templates name)
  (assoc name templates :key (function second) :test (function string=)))


(defun get-block (section name)
  (assoc name (cdr section) :key (function second) :test (function string=)))


(defun fill-char (block)
  (second (assoc :f block)))


(defun block-lines (block)
  (remove-if (lambda (item) (member (car item) '(:title :f))) block))


(defun enclosing-box (lines &optional fill-char)
  "
RETURN:  left right first last
         left and right are the maximum non-white-space or fill-char
         columns in the lines.
         first and last are the index in lines of the first and last lines
         containing non-white-space or fill-char characters.
"
  (let ((pred (function char/=))
        (first most-positive-fixnum)
        (last  0)
        (left most-positive-fixnum)
        (right 0))
    (if fill-char
      (setf pred (function char=)
            fill-char (character fill-char))
      (setf fill-char (character " ")))
    (do ((i 0 (1+ i))
         (lines lines (cdr lines))
         (p))
        ((null lines) (values left right first last))
      (setf p (position fill-char (car lines) :test pred))
      (when p
        (setf first (min first i)
              last (max last i)
              left (min left p)
              right (max right (position fill-char (car lines)
                                         :test pred :from-end t))))
      )));;enclosing-box


(defun list-all-frames ()
  (mapcar (function second) *templates)
)


#||

;; left right of - in base header line
(enclosing-box
 (mapcar (function second)
         (remove-if (lambda (line) (not (eq :i (car line))))
         (BLOCK-LINES
          (GET-BLOCK (get-section templates "FRAMES") "roll"))))
 "-")


;; enclosing box of "*"
(enclosing-box
 (mapcar (function second)
         (BLOCK-LINES
          (GET-BLOCK (get-section templates "FRAMES") "roll")))
 "*")

;; enclosig box of sprite
(enclosing-box
 (mapcar (function second)
         (BLOCK-LINES
          (GET-BLOCK (get-section templates "FEET") "toto"))))





ext:*args*


||#



;;     S section
;;       comment
;;     T title
;;     H picture
;;     B base-line picture
;;
;;
;; DATA
;; S HEADS
;;
;;
;; T toto
;; H       \\\|///
;; H     \\  - -  //
;; H      (  0-0  )
;; B  --oOOo-(_)-oOOo--
;;
;;
;; T teacher
;; H      ______
;; H      |___| "
;; H      (o o)
;; B  -ooO--O--Ooo-
;;
;;
;; T bear
;; H      (_)-(_)
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;;
;; T mouse
;; H       ()_()
;; H       (o o)
;; B  -ooO--`o'--Ooo-
;;
;; T cow
;; H       ((__))
;; H        (00)
;; B  -nn--(o__o)--nn-
;;
;;
;; T judge
;; H        ___
;; H       .|||.
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;;
;; T haut-de-forme
;; H        |"|
;; H       _|_|_
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;; T head-1
;; H       '/_\
;; H       (o o)
;; B  -ooO--(_I--Ooo-
;;
;; T head-2
;; H      ` /_\ '
;; H     - (o o) -
;; B  -ooO--(_)--Ooo-
;;
;; T asyrien
;; H    .  .:::.
;; H      :(o o):  .
;; B  -ooO-=(_)--Ooo-
;;
;; T egyptian
;; H       ,,,,,
;; H      /(o o)\
;; B  -ooO--(_)--Ooo-
;;
;; T head-3
;; H       /\#/\
;; H      /(o o)\
;; B  -ooO=-(_)--Ooo-
;;
;; T et
;; H     '. ___ .'
;; H    '  (> <) '
;; B  -ooO--(_)--Ooo-
;;
;; T head-4
;; H     `  _ _  '
;; H    -  (OXO)  -
;; B  -ooO--(_)--Ooo-
;;
;; T head-5
;; H      '\\-//`
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;; T head-5
;; H        vvv
;; H       (0~0)
;; B  -ooO--(_)--Ooo-
;;
;; T head-5
;; H      ` /_\ '
;; H     - (o o) -
;; B  -ooO--(_)--Ooo-
;;
;; T head-6
;; H        /_\ `*
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;; T halterophile
;; H   #                 #
;; H   #=ooO=========Ooo=#
;; H   #  \\  (o o)  //  #
;; B  ---------(_)---------
;;
;;
;; T lancier
;; H    #   ___
;; H    #  <_*_>
;; H    #  (o o)
;; B  --8---(_)--Ooo-
;;
;;
;; T guard
;; H      .'_#_`.
;; H      |[o o]|
;; B  -ooO--(_)--Ooo-
;;
;;
;; T head-7
;; H        !!!
;; H     `  _ _  '
;; H    -  (OXO)  -
;; B  -ooO--(_)--Ooo-
;;
;;
;; T head-8
;; H       .|||.
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;;
;; T fou-du-roi
;; H      _     _
;; H    o' \.=./ `o
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;;
;; T
;; H      |.===.
;; H      {}o o{}
;; B  -ooO--(_)--Ooo-
;;
;; T
;; H       ,-_-|
;; H      ([o o])
;; B  -ooO--(_)--Ooo-
;;
;;
;; T canotier
;; H      __MMM__
;; H       (o o)
;; B  -ooO--(_)--Ooo-
;;
;;
;; S FEET
;;
;; T small
;; B  ----/\---/\----
;; H      \(   )/
;;
;;
;; T toto
;; E     ooo0
;; E    (    )   0ooo
;; B  ---\  (----(   )--
;; E      \_)     ) /
;; E             (_/
;;
;;
;;
;; S FRAMES
;;    T title
;;    F fill char
;;    H head
;;    M repeat line
;;    N repeat portion (overiding E)
;;    F repeat end (overiding E)
;;    E end
;;    O optional end
;;    I head base line
;;    G end base line
;;
;; T box
;; F *
;; I +---------------------------------------------------------------------+
;; M | ******************************************************************* |
;; G +---------------------------------------------------------------------+
;;
;;
;; T bevel
;; F *
;; I /---------------------------------------------------------------------\
;; M | ******************************************************************* |
;; G \---------------------------------------------------------------------/
;;
;;
;;
;; T tape
;; F *
;; I   .-----------------------------------------------------------------.
;; H  /  .-.   ***************************************************   .-.  \
;; H |  /   \  ***************************************************  /   \  |
;; H | |\_.  | *************************************************** |    /| |
;; H |\|  | /| *************************************************** |\  | |/|
;; H | `---' | *************************************************** | `---' |
;; M |       | *************************************************** |       |
;; G |       |-----------------------------------------------------|       |
;; E \       |                                                     |       /
;; E  \     /                                                       \     /
;; E   `---'                                                         `---'
;;
;;
;;
;; T roll
;; F *
;; H                                                                .---.
;; H                                                               /  .  \
;; H                                                              |\_/|   |
;; H                                                              |   |  /|
;; I   .----------------------------------------------------------------' |
;; H  /  .-.   *********************************************************  |
;; H |  /   \  *********************************************************  |
;; H | |\_.  | *********************************************************  |
;; H |\|  | /| *********************************************************  |
;; H | `---' | *********************************************************  |
;; M |       | *********************************************************  |
;; E |       | ********************************************************* /
;; G |       |----------------------------------------------------------'
;; E \       |
;; E  \     /
;; E   `---'
;;
;;
;; T directory
;;  directory symbol, folder symbol]
;;
;; H     ___
;; H    /___\_________
;; H   |              |
;; H   | ************ |
;; H   | ************ |
;; H   | ************ |
;; H   | ************ |
;; HVK |______________|
;;
;;
;; T hand
;; F *
;; I                                 ______________________________________
;; H                                |                                      |
;; H                     _.---------|.--.   ****************************** |
;; H                  .-'  `       .'/  ``  ****************************** |
;; H               .-'           .' |    /| ****************************** |
;; H            .-'         |   /   `.__//  ****************************** |
;; H         .-'           _.--/        /   ****************************** |
;; H        |        _  .-'   /        /    ****************************** |
;; H        |     ._  \      /     `  /     ****************************** |
;; H        |        ` .    /     `  /      ****************************** |
;; H        |         \ \ '/        /       ****************************** |
;; H        |        - \  /        /|       ****************************** |
;; H        |        '  .'        / |       ****************************** |
;; H        |          '         |.'|       ****************************** |
;; H        |                    |  |       ****************************** |
;; N                                |       ****************************** |
;; G        |                    |  |______________________________________|
;; F                                |______________________________________|
;; E        |                    |.'
;; E        |                    /
;; E        |                   /
;; E        |                  /
;; E        )                 /|
;; E     .A/`-.              / |
;; E    AMMMA. `-._         / /
;; E   AMMMMMMMMA. `-.     / /
;; E  AMMMMMMMMMMMMA. `.    /
;; E AMMMMMMMMMMMMMMMMA.`. /
;; E MMMMMMMMMMMMMMMMMMMA.`.
;; E MMMMMMMMMMMMMMMMMMMMMA.`.
;; E MMMMMMMMMMMMMMMMMMMMMMMA.
;; E MMMMMMMMMMMMMMMMMMMMMMMMMA.
;; E MMVKMMMMMMMMMMMMMMMMMMMMMMM
;; E MMMMMMMMMMMMMMMMMMMMMMMMMV'
;; O MMMMMMMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMMV
;; O MMMMMMMMMMMMMV
;; O MMMMMMMMMMMMV
;; O MMMMMMMMMMMV
;; O MMMMMMMMMMV
;; O MMMMMMMMMV
;; O MMMMMMMMV
;; O MMMMMMMV
;; O MMMMMMV
;; O MMMMMV
;; O MMMMV
;; O MMMV
;; O MMV
;; O MV
;; O V
;;
;; S END
;;
;;        if you are a legitimat__email operator that has be victimized by spews
;;     >  please contact m_ to (  ) a __ass ac____ lawsuit.
;;     >                 ( )    \ (  (  \    (    )
;;     >  although free __)\ch __) \rt) (ly __)  / defamation is not. We are
;;     >  not su__g s__(   (__/     )(   \ (     )sue God. they exist but are
;;     >  facel(  \a(              (__)  (d )     \fi_ing a class action by
;;     >  the IS)  \t)                    )/       )( )se spews subscribers
;;     >  who_s(__                        '       (_/(__ be___ior.
;;     >    (_) __)     S   P   L   O   R   F           ) (   )
;;     >  if yo(___                                  _ /m t)  )
;;     >          (       ___                   ____/ ))  (__/
;;     >  spew_(\ta) _   (t a)  __      ____  __)s _o( \_came the KKK/Gestapo
;;     >  of (___)(_/r)   \T(  (ar\    (imin)( co_/ )r\__)with no face, and
;;     >  unlimited p(_____)t\ey) (   __) t/ /se(  (_up to be the judge, jury,
;;     >  and executio_er. In )(eir\ (urt,(_/u ar)   )lty by association,
;;     >  guilty unti(_)roven(__)oc(  ) b_t then(___/l punished even though
;;     >  you ARE innocent.         )/  (_)
;;     >                            '

;;;; box                              --                     --          ;;;;
