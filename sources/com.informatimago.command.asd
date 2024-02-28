(asdf:defsystem "com.informatimago.command"
  ;; system attributes:
  :description "This system gathers command utilities."
  :long-description "

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.1"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2019")
               ((#:albert #:output-dir)          . "../documentation/commands/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("com.informatimago.common-lisp.cesarum"
               ;; commands dependencies:
               "split-sequence"
               "babel"
               "cffi"
               "cl-ppcre"
               "com.informatimago.clmisc"
               "com.informatimago.common-lisp"
               "com.informatimago.common-lisp.cesarum"
               "md5"
               "split-sequence"
               "uiop"
               "usocket"
               "xmls")
  :components ((:file "packages" :depends-on ())
               (:file "script"   :depends-on ("packages")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

