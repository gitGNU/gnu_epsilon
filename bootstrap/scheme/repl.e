;;;;; epsilon1 REPL

;;;;; Copyright (C) 2013, 2014, 2016  Luca Saiu
;;;;; Updated in 2015 by Luca Saiu

;;;;; This file is part of GNU epsilon.

;;;;; GNU epsilon is free software: you can redistribute it and/or modify
;;;;; it under the terms of the GNU General Public License as published by
;;;;; the Free Software Foundation, either version 3 of the License, or
;;;;; (at your option) any later version.

;;;;; GNU epsilon is distributed in the hope that it will be useful,
;;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;; GNU General Public License for more details.

;;;;; You should have received a copy of the GNU General Public License
;;;;; along with GNU epsilon.  If not, see <http://www.gnu.org/licenses/>.


;;;;; Define the scratch file full path to be used later
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define repl:scratch-file-path
  (string:append configuration:abs_top_srcdir
                 configuration:dir_separator
                 "bootstrap/scheme/scratch.e"))


;;;;; Define the command-line options needed for the REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Of course we want --help, --version and -- to finalize option processing.
(command-line:set-common-options)

;;; Add the other options we need
(command-line:add-options (("--no-repl") "don't execute the interactive REPL")
                          (("--no-scratch") "don't load the scratch file")
                          #;(("--optimization-level" "--optimize" "-O") fixnum 10 "set optimization level"))

;;; Set the information displayed by --version and --help .
(command-line:set-info!
    #:introduction "The GNU epsilon interpreter."
    #:usage (string:append (vector:get (command-line:get-argv) 0)
                           " [option-or-file]...")
    #:closing (string:append "Interpret "
                             repl:scratch-file-path
                             "\n" ;; Breaking here helps Emacs font lock in epsilon mode.
"(unless --no-scratch is given), then the epsilon1 source files named in
the command line, if any, in order.  Then run an interactive Read-Eval-Print
Loop (unless --no-repl is given).")
    #:program-name configuration:package_name
    #:program-version configuration:package_version
    #:bug-email configuration:package_bugreport
    #:copyright (string:append "Copyright (C) Luca Saiu 2012-2016\nCopyright (C) Universit"
                               (vector:vector 233)
                               " Paris 13 2012")
    #:authors "Luca Saiu <http://ageinghacker.net>")


;;;;; REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We already implemented the "E" part in core.e.  Of course in
;;; epsilon1 "executing" an s-expressions involves macroexpanding it
;;; into an expression and then transforming the expression, before we
;;; interpreting the resulting epsilon0 expression.

(e1:define (repl:repl)
  (e1:let* ((input-port (input-port:readline-input-port))
            (backtrackable-input-port
             (backtrackable-port:input-port->backtrackable-port input-port
                                                                (option:option-none))))
    (command-line:process-args)
    (repl:load-sources-from-the-command-line)
    (e1:unless (command-line:option-supplied? "--no-scratch")
      (repl:load-scratch))
    (e1:unless (command-line:option-supplied? "--no-repl")
      (repl:print-banner)
      (repl:repl-helper backtrackable-input-port))))
(e1:define (repl:repl-helper bp)
  (e1:if (backtrackable-port:eof? bp)
    (e1:bundle)
    (e1:let ((sexpression (reader:read-bp bp)))
      (e1:when (box:get repl:debug)
        (fio:write "[You wrote: " (se sexpression) "]\n"))
      (e1:if (sexpression:eof-object? sexpression)
        (e1:when (box:get repl:debug)
          (fio:write "Goodbye.\n"))
        (e1:begin
          ;;(fio:write "Macroexpanding, transforming and interpreting... ")
          (e1:let ((expression (repl:macroexpand-and-transform sexpression)))
            ;;(fio:write "The macroexpand and tranformation part is done.\n")
            (e1:let ((results (e0:eval-ee expression)))
              ;;(fio:write "There are " (i (list:length results)) " results\n")
              (e1:dolist (result results)
                (e1:primitive io:write-value (io:standard-output) result)
                (fio:write "\n"))
              (repl:repl-helper bp))))))))

(e1:define (repl:print-banner)
  (fio:write "GNU epsilon " (st version:string) "
Copyright (C) 2012-2016  Luca Saiu
Copyright (C) 2012  Universit" (c 233) ;; FIXME: do it the obvious way after bootstrapping away from Guile -- actually, Guile 1.8
" Paris 13

GNU epsilon comes with ABSOLUTELY NO WARRANTY; enter `,show no-warranty' for
details.  This program is free software and you are welcome to redistribute it
under the terms of the GNU General Public License, version 3 or later.  Enter
`,show license' or see the file named COPYING for the full license text.

"))

(e1:define repl:debug
  (box:make #f))

;;; Load the scratch file.  This is to be done automatically when the REPL
;;; starts, not now.
(e1:define (repl:load-scratch)
  (e1:load repl:scratch-file-path))

(e1:define (repl:load-sources-from-the-command-line)
  (e1:dolist (filename (command-line:non-option-list))
    (e1:load filename)))


;;;;; REPL commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define a REPL command named ,NAME-AS-STRING, calling PROCEDURE-NAME
;;; with one s-expression parameter.
(e1:define-macro (repl:define-command name-as-string procedure-name)
  (e1:let ((macro-name (sexpression:fresh-symbol-with-prefix (string:append "command-macro-"
                                                                            (sexpression:eject-string name-as-string)))))
    `(e1:begin
       (e1:define-macro (,macro-name argument)
         `(,',procedure-name ',argument)) ;; Pass argument as a literal s-expression.
       (reader:define-simple-prefix
          ,(sexpression:inject-string
              (string:append "," (sexpression:eject-string name-as-string)))
          ,macro-name))))

;;; After unexec'ing these two globals are just bound to ordinary
;;; strings, and files from the source tree are not accessed at runtime.
(e1:define repl:gpl-text
  (io:file-content-as-byte-vector (string:append configuration:abs_top_srcdir
                                                 configuration:dir_separator
                                                 "COPYING")))
(e1:define repl:no-warranty-text
  (io:file-content-as-byte-vector (string:append configuration:abs_top_srcdir
                                                 configuration:dir_separator
                                                 "NO-WARRANTY")))

(e1:toplevel (repl:define-command "show" repl:show))
(e1:define (repl:show what)
  (e1:case (sexpression:eject-symbol what)
    ((license copying gpl)
     (fio:write (st repl:gpl-text) "\n"))
    ((no-warranty warranty)
     (fio:write (st repl:no-warranty-text) "\n"))
    (else
     (e1:error "Unknown ,show argument\n"))))
