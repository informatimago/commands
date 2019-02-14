(asdf:defsystem "com.informatimago.command"
  ;; system attributes:
  :description "This system gathers command utilities."
  :long-description "

"
  :author     "Pascal J. Bourguignon <pjb@informatimago.com>"
  :maintainer "Pascal J. Bourguignon <pjb@informatimago.com>"
  :licence "AGPL3"
  ;; component attributes:
  :version "1.0.0"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2019")
               ((#:albert #:output-dir)          . "../documentation/commands/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))
  :depends-on ("com.informatimago.common-lisp.cesarum")
  :components ((:file "packages" :depends-on ())
               (:file "utility"  :depends-on ("packages" "script"))
               (:file "script"   :depends-on ("packages")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

