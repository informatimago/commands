(defun one-of (argument &rest options)
  (member argument options :test (function string=)))

(defun main (arguments)
  (let ((arguments (or arguments  '("-h"))))
    (loop
      :for arg := (pop arguments)
      :do (cond
            ((one-of arg "-r" "--repl")
             (com.informatimago.common-lisp.interactive.interactive:repl))

            ((one-of arg "-Q" "--quit")
             (loop-finish))

            ((one-of arg "-l" "--list-all-commands")
             (format t "~{~A~%~}" com.informatimago.command:*all-commands*))

            ((one-of arg "-h" "--help")
             (format t "~%~A options:~%~
                      ~%    -h|--help                  prints this text.~
                      ~%    -V|--version               prints the version.~
                      ~%    -r|--repl                  enters a lisp REPL.~
                      ~%    -Q|--quit                  quit.~
                      ~%    -l|--list-all-commands     prints a list of all the command names.~
                      ~%                               that can be symlinked to ~0@*~A~
                      ~%" *program-name*)
             (format t "Options are optional, and may be followed by the name of ~
                    ~%a command and its own arguments and options.  For example: ~
                   ~2%    ~A --version  buzzword~
                   ~2%This will print the commands executable version, and run the buzzword command.~
                   ~2%" *program-name*))

            ((one-of arg "-V" "--version")
             (format t "~%Commands version 1.0~%~
                      ~%Copyright Pascal J. Bourguignon 1990 - 2021~
                      ~%License: AGPL3~
                      ~%Sources: https://gitlab.com/informatimago/commands~2%"))

            ((com.informatimago.common-lisp.cesarum.sequence:prefixp "-" arg)
             (format *error-output* "Invalid option: ~A~%" arg)
             (main '("-h"))
             (loop-finish))

            ((member arg com.informatimago.command:*all-commands*
                     :test (function string=))
             ;; does not return:
             (apply (function dispatch-command) arg arguments))

            (t
             (format *error-output* "Unknown command: ~A~%" arg)
             (main '("-h"))
             (loop-finish)))

      :while arguments))
  0)
