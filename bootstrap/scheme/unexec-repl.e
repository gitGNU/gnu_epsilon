;;;;; Load advanced features and unexec an -*- epsilon -*- REPL.  No Guile.

;;;;; Copyright (C) 2014, 2015, 2016  Luca Saiu

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


(fio:write "Still alive in unexec-repl.e\n")


;;; -----------------------------------------------------------
;;; FIXME: comment on why this is important -- FIXME: is it?
(e1:load (string:append configuration:abs_top_builddir "/bootstrap/scheme/configuration.e"))
(fio:write "configuration:abs_top_builddir is "
           (st configuration:abs_top_builddir)
           "\n")
#;(e1:define bootstrap:repl-image-file-name "/tmp/foo.u")
;;; -----------------------------------------------------------

;;;;; Syntax for loading predefined files from the right path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The argument is a string, holding a file name with no directories.  The file
;;; is taken from the source directory.  [FIXME: I should probably generalize it
;;; to use the installation path when installed].
(e1:define (e1:load-predefined-file file-name)
  (e1:load (string:append configuration:abs_top_srcdir
                          configuration:dir_separator
                          "bootstrap/scheme"
                          configuration:dir_separator
                          file-name)))

;;; Load every file given as a parameter.  Every parameter should be an
;;; expression evaluating to a string, to be passed to e1:load-predefined-file .
(e1:define-macro (e1:load-predefined . file-names)
  (e1:if (sexpression:null? file-names)
    '(e1:begin) ;; some code expects a 0-dimension expression in the end.
    `(e1:begin
       (e1:load-predefined-file ,(sexpression:car file-names))
       (e1:load-predefined ,@(sexpression:cdr file-names)))))


;;;;; Load advanced epsilon1 features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:load-predefined "fixed-point.e"
                    "command-line.e"
                    "repl.e"
                    "compiler.e"
                    "stream.e"
                    "analyses-and-optimizations.e"
                    "csp.e"
                    "new-compiler.e")


;;;;; Unexec a non-Guile REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(e1:toplevel
  (fio:write "Unexecing an epsilon1 REPL into "
             (st bootstrap:repl-image-file-name)
             "\n")
  (e1:unexec bootstrap:repl-image-file-name
    (repl:repl))
;;)
