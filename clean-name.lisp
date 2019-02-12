;; -*- mode:lisp;coding:utf-8 -*-

(defpackage "CLEAN-NAME"
  (:use "COMMON-LISP"))
(in-package "CLEAN-NAME")

;; Replaces any sequences of non alphanumeric or dot character in the
;; arguments by a single dash.

(defun split-string-if (predicate string &key remove-empty-subseqs)
  (declare (ignore remove-empty-subseqs))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (let ((chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)))
    (loop :while (< position strlen) :do
       (loop :while (and (< nextpos strlen)
                         (not (funcall predicate (aref string nextpos)))) :do
          (incf nextpos))
       (push (subseq string position nextpos) chunks)
       (setf position (1+ nextpos)
             nextpos  position))
    (nreverse chunks)))

(defun split-string (separators string &key remove-empty-subseqs)
  (split-string-if (lambda (ch) (find ch separators)) string
                   :remove-empty-subseqs remove-empty-subseqs))


(defparameter *character-foldings*
  '(("A" "ÀÁÂÃÄÅ") ("AE" "Æ") ("C" "Ç") ("E" "ÈÉÊË") ("I" "ÌÍÎÏ")
    ("ETH" "Ð") ("N" "Ñ") ("O" "ÒÓÔÕÖØ") ("U" "ÙÚÛÜ") ("Y" "Ý")
    ("TH" "Þ") ("ss" "ß") ("a" "àáâãäå") ("ae" "æ") ("c" "ç")
    ("e" "èéêë") ("i" "ìíîï") ("eth" "ð") ("n" "ñ") ("o" "òóôõöø")
    ("u" "ùúûü") ("u" "ýÿ") ("th" "þ")

    ("-lt-" "<")
    ("-gt-" ">")
    ("-exclaim-" "!")
    ("-question-" "?")
    ("-percent-" "%")))

(defun character-folding (character)
  (car (member (character character) *character-foldings*
               :test (function position) :key (function second))))

(defun character-fold (character)
  "
RETURN: A string containing the character without accent
        (for accented characters), or a pure ASCII form of the character.
"
  (car (character-folding character)))

(defun string-fold (string)
  (apply (function concatenate) 'string
         (map 'list (lambda (ch) (let ((conv (character-folding ch)))
                                   (if conv
                                       (first conv)
                                       (list ch))))
              string)))


(defun clean-name (name)
  (format nil "~{~A~^-~}"
          (split-string-if (lambda (char) (not (or (alphanumericp char)
                                                   (char= char #\/)
                                                   (char= char #\.))))
                           (string-trim "-" (string-fold (string-downcase name)))
                           :remove-empty-subseqs t)))





(defparameter *diacritics* '((#x80
                              "AÀ" "EÈ" "IÌ" "OÒ" "UÙ"
                              "aà" "eè" "iì" "oò" "uù")
                             (#x81
                              "AÁ" "EÉ" "IÍ" "OÓ" "UÜ" "YÝ"
                              "aá" "eé" "ií" "oó" "uü" "yý")
                             (#x82
                              "AÂ" "EÊ" "IÎ" "OÔ" "UÛ"
                              "aâ" "eê" "iî" "oô" "uû")
                             (#x88
                              "AÄ" "EË" "IÏ" "OÖ" "UÜ"
                              "aä" "eë" "iï" "oö" "uü")
                             (#xa7
                              "CÇ" "cç")))

(defun combine-diacritic (ch diacritic)
  (let ((dia (find ch diacritic :key (lambda (dia) (aref dia 0)))))
    (if dia
        (aref dia 1)
        ch)))

(defun translate-diacritics (text)
  (let ((escape (code-char #xcc)))
    (if (find escape text)
        (let ((out (make-array (length text) :fill-pointer 0 :element-type 'character)))
          (loop :with state = :normal
                :for ch :across text
                :do (case state
                      ((:normal) (if (char= escape ch)
                                     (setf state :escaped)
                                     (vector-push ch out)))
                      ((:escaped) (let ((diacritic (cdr (assoc (char-code ch) *diacritics*))))
                                    (when diacritic
                                      (setf (aref out (1- (length out)))
                                            (combine-diacritic (aref out (1- (length out))) diacritic)))
                                    (setf state :normal)))))
          out)
        text)))

(defun escape (string)
  (with-output-to-string (*standard-output*)
    (loop :for ch :across string
            :initially (princ "'")
          :do (if (char= #\' ch)
                  (princ "'\\''")
                  (princ ch))
          :finally (princ "'"))))

(defun process-name (name)
  (write-line (clean-name (translate-diacritics name))))



(defun process-item (path)
  (let* ((components  (split-string "/" path :remove-empty-subseqs t))
         (new-path    (format nil "~{~A~^/~}"
                              (append (butlast components)
                                      (list (clean-name
                                             (translate-diacritics
                                              (string-trim "- ." (first (last components))))))))))
    (unless (string-equal path new-path)
      (format t "mv -v ~A ~A~%" (escape path) (escape new-path)))))

(defun process-items (path)
  (with-open-file (input path)
    (loop
      :for item-path := (read-line input nil nil)
      :while item-path
      :do (process-item item-path))))

;;;;--------------------------------------------------------------------

(defparameter *program-name* "clean-name"
  "Name of the program.")

(defun print-usage ()
  (format t "~%~A usage:~%~
             ~%    ~:*~A [-f|--list-file file]... [name...]~2%"
          *program-name*))

;;;;--------------------------------------------------------------------

(defun main (args)
  (let ((files    nil)
        (names    '()))

    (loop :while args
          :do (let ((arg (pop args)))
                (cond
                  ((member arg '("-f" "--list-file") :test (function string-equal))
                   (push (pop args) files))
                  ((member arg '("-h" "--help")           :test (function string-equal))
                   (print-usage)
                   (return-from main 0))
                  ((string= arg "-" :end1 (min (length arg) 1))
                   (error "Unknown option: ~A" arg))
                  (t (push arg names)))))
    (when (and (null files) (null names))
      (print-usage)
      (return-from main 0))
    (dolist (file files)
      (process-items file))
    (dolist (name names)
      (process-name name))
    0))


#-testing (handler-case (uiop:quit (main uiop:*command-line-arguments*))
            (error (err)
              (format *error-output* "~&~A~%" err)
              (finish-output *error-output*)
              (uiop:quit 1)))
;;;; THE END ;;;;
