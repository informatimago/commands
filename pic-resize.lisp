#!/usr/local/bin/clisp -ansi -q -E utf-8
;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;******************************************************************************
;;;;FILE:				resize
;;;;LANGUAGE:			Common-Lisp
;;;;SYSTEM:			Clisp
;;;;USER-INTERFACE:	None
;;;;DESCRIPTION
;;;;	This script is a driver for the GIMP scheme script resize.scm.
;;;;USAGE
;;;;	resize [-h|--help] max-width max-height [-] pic-file...
;;;;
;;;;  # resize the pic-file in place!
;;;;
;;;;AUTHORS
;;;;	<PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2003-09-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    LGPL
;;;;    Copyright Pascal J. Bourguignon 2003 - 2003
;;;;
;;;;    This  library is  free software;  you can  redistribute  it and/or
;;;;    modify  it under  the  terms  of the  GNU  Library General  Public
;;;;    License  as  published by  the  Free  Software Foundation;  either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    This library  is distributed in the  hope that it  will be useful,
;;;;    but  WITHOUT ANY WARRANTY;  without even  the implied  warranty of
;;;;    MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the GNU
;;;;    Library General Public License for more details.
;;;;
;;;;    You should have received a  copy of the GNU Library General Public
;;;;    License  along with this  library; see  the file  COPYING.LIB.  If
;;;;    not,  write to  the Free  Software Foundation,  59 Temple  Place -
;;;;    Suite 330, Boston, MA 02111-1307, USA.
;;;;******************************************************************************

;; Clean the packages imported into COMMON-LISP-USER:
(MAPC (LAMBDA (USED) (UNUSE-PACKAGE USED "COMMON-LISP-USER"))
      (REMOVE (FIND-PACKAGE "COMMON-LISP")
              (COPY-SEQ (PACKAGE-USE-LIST "COMMON-LISP-USER"))))



(DEFCONSTANT +PNAME+ "resize")


(DEFUN PRINT-USAGE ()
  (FORMAT T
    "~A usage:~%~4T~A [-h|--help] max-width max-height [-] pic-file...~%"
    +PNAME+ +PNAME+)
  );;PRINT-USAGE


(DEFUN MAIN (ARGUMENTS)
  (LET ((FILES      NIL)
        (PIC-FILES  '())
        (MAX-WIDTH  NIL)
        (MAX-HEIGHT NIL)
        (ABORT      NIL))
    (LABELS ((CHECK-INTEGER
              (ARG)
              (MULTIPLE-VALUE-BIND (VALUE POS)
                  (PARSE-INTEGER ARG :JUNK-ALLOWED T)
                (IF (= POS (LENGTH ARG))
                  VALUE
                  (PROGN (FORMAT T "~A: Invalid number: ~S.~%" +PNAME+ ARG)
                         (SETQ ABORT T)
                         NIL))))
             (PROCESS-ARGUMENT
              (ARG)
              (COND
               ((NULL MAX-WIDTH)
                (SETQ MAX-WIDTH (CHECK-INTEGER ARG)))
               ((NULL MAX-HEIGHT)
                (SETQ MAX-HEIGHT (CHECK-INTEGER ARG)))
               (T
                (IF (OPEN ARG :DIRECTION :PROBE :IF-DOES-NOT-EXIST NIL)
                  (PUSH ARG PIC-FILES)
                  (PROGN
                    (FORMAT T "~A: There is no file ~S.~%" +PNAME+ ARG)
                    (SETQ ABORT T)))))))
      (DOLIST (ARG ARGUMENTS)
        (COND
         ((OR (STRING-EQUAL "-h" ARG) (STRING-EQUAL "--help" ARG))
          (PRINT-USAGE)
          (EXT:EXIT 0))
         ((STRING-EQUAL "-" ARG)
          (SETQ FILES T))
         ((CHAR= (CHARACTER "=") (CHAR ARG 0))
          (IF FILES
            (PROCESS-ARGUMENT ARG)
            (PROGN
              (FORMAT T "~A: Invalid option ~S.~%" +PNAME+ ARG)
              (PRINT-USAGE)
              (EXT:EXIT 1))))
         (T (PROCESS-ARGUMENT ARG))))
      (UNLESS PIC-FILES
        (FORMAT T "~A: Missing arguments.~%" +PNAME+)
        (PRINT-USAGE)
        (EXT:EXIT 1))
      (WHEN ABORT
        (EXT:EXIT 1))
      (EXT:RUN-PROGRAM "gimp"
        :ARGUMENTS
        `(d
          "--no-data"
          "--console-messages"
          "--no-interface"
          "--batch"
          ,@(mapcar
             (lambda (fname)
               (remove (code-char 10)
                       (FORMAT NIL
                         "(let* ((max-width ~D)
                        (max-height ~D)
                        (fname ~S)
                        (image  (car (gimp-file-load run-mode fname fname)))
                        (width  (car (gimp-image-width  image)))
                        (height (car (gimp-image-height image)))
                        (fx     (/ width  max-width  ))
                        (fy     (/ height max-height ))
                        (ratio  1.0) )
                   (cond ((and (<= width max-width) (<= height max-height)))
                         (t (cond ((< fx fy) (set! ratio fy))
                                  (t         (set! ratio fx)))
                            (set! width  (/ width  ratio))
                            (set! height (/ height ratio))
                            (gimp-image-scale image width height)
                            (file-jpeg-save
                             run-mode image
                             (aref (cadr (gimp-image-get-layers image)) 0)
                             fname fname 0.75 0.80 1 1
                             \"Resized with pic-resize & gimp\"
                             0 1 0 0)
                            ))
                   (gimp-image-delete image)
                   )"
                         MAX-WIDTH MAX-HEIGHT fname)))
             PIC-FILES)
          "(gimp-quit 0)"
          )
        :INPUT  NIL
        :OUTPUT :TERMINAL
        :WAIT T)
      (EXT:EXIT 0)
      )));;MAIN
(MAIN EXT:*ARGS*)







;;(gimp-display-new image)
;;(car (gimp-image-get-active-layer image))
;;(resize-internal run-mode max-width max-height . pic-files)
(resize-internal RUN-NONINTERACTIVE 128 256 "~/tmp/test.jpg" )


(file-jpeg-save run-mode image (car (gimp-image-get-active-layer image)) fname fname 0.75 0.80 1 1 "Resized with pic-resize & gimp" 0 1 0 0)

(begin
 (set! run-mode RUN-NONINTERACTIVE)
 (set! max-width 128)
 (set! max-height 256)
 (set! fname "/home/pascal/tmp/test.jpg")
 (set! image  (car (gimp-file-load run-mode fname fname)))
 (gimp-display-new image)
 (set! width  (car (gimp-image-width  image)))
 (set! height (car (gimp-image-height image)))
 (set! fx     (/ width  max-width  ))
 (set! fy     (/ height max-height ))
 (set! ratio  fx)
 (set! width  (/ width  ratio))
 (set! height (/ height ratio))
 )

(gimp-file-save run-mode image
                                (car (gimp-image-get-active-layer image))
                                fname fname)

;;(car (gimp-image-get-active-layer image))

;;(resize-internal RUN-INTERACTIVE 128 256 "/home/pascal/tmp/test.jpg")

;;(resize-internal RUN-NONINTERACTIVE 128 256 "/home/pascal/tmp/test.jpg")

;;;; pic-resize                       --                     --          ;;;;
(define (resize-internal run-mode max-width max-height . pic-files)
              (while pic-files
(print pic-files)
                (let* ((fname (car pic-files))
                       (image  (car (gimp-file-load run-mode fname fname)))
                       (width  (car (gimp-image-width  image)))
                       (height (car (gimp-image-height image)))
                       (fx     (/ width  max-width  ))
                       (fy     (/ height max-height ))
                       (ratio  1.0))
                  (set! pic-files (cdr pic-files))
                  (gimp-display-new image)
                  (cond ((and (<= width max-width) (<= height max-height)))
                        (t (cond ((< fx fy) (set! ratio fy))
                                 (t         (set! ratio fx)))
                           (set! width  (/ width  ratio))
                           (set! height (/ height ratio))
                           (gimp-image-scale image width height)
                           (file-jpeg-save run-mode image
                                (aref (cadr (gimp-image-get-layers image)) 0)
                                fname fname 0.75 0.80 1 1
                                "Resized with pic-resize & gimp"
                                0 1 0 0)
                           ))
                  (gimp-image-delete image)
                  nil)))
