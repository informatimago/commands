
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


(defconstant +pname+ "resize")


(defun print-usage ()
  (format t
    "~A~:* usage:~%~4T~A [-h|--help] max-width max-height [-] pic-file...~%"
    *program-name*))


(defun main (arguments)
  (let ((files      nil)
        (pic-files  '())
        (max-width  nil)
        (max-height nil)
        (abort      nil))
    (labels ((check-integer
                 (arg)
               (multiple-value-bind (value pos)
                   (parse-integer arg :junk-allowed t)
                 (if (= pos (length arg))
                     value
                     (progn (format t "~A: Invalid number: ~S.~%" +pname+ arg)
                            (setq abort t)
                            nil))))
             (process-argument
                 (arg)
               (cond
                 ((null max-width)
                  (setq max-width (check-integer arg)))
                 ((null max-height)
                  (setq max-height (check-integer arg)))
                 (t
                  (if (open arg :direction :probe :if-does-not-exist nil)
                      (push arg pic-files)
                      (progn
                        (format t "~A: There is no file ~S.~%" +pname+ arg)
                        (setq abort t)))))))
      (dolist (arg arguments)
        (cond
          ((or (string-equal "-h" arg) (string-equal "--help" arg))
           (print-usage)
           (exit ex-ok))
          ((string-equal "-" arg)
           (setq files t))
          ((char= (character "=") (char arg 0))
           (if files
               (process-argument arg)
               (progn
                 (format t "~A: Invalid option ~S.~%" +pname+ arg)
                 (print-usage)
                 (exit 1))))
          (t (process-argument arg))))
      (unless pic-files
        (format t "~A: Missing arguments.~%" +pname+)
        (print-usage)
        (exit ex-usage))
      (when abort
        (exit ex-usage))
      (uiop:run-program `("gimp"
                          "--no-data"
                          "--console-messages"
                          "--no-interface"
                          "--batch"
                          ,@(mapcar
                             (lambda (fname)
                               (remove (code-char 10)
                                       (format nil
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
                                               max-width max-height fname)))
                             pic-files)
                          "(gimp-quit 0)")
                        :input  nil
                        :output :terminal
                        :wait t)))
  ex-ok)

#|


;;(gimp-display-new image)
;;(car (gimp-image-get-active-layer image))
;;(resize-internal run-mode max-width max-height . pic-files)
(resize-internal run-noninteractive 128 256 "~/tmp/test.jpg" )


(file-jpeg-save run-mode image (car (gimp-image-get-active-layer image)) fname fname 0.75 0.80 1 1 "Resized with pic-resize & gimp" 0 1 0 0)

(begin
 (set! run-mode run-noninteractive)
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
|#

;;;; THE END ;;;;
