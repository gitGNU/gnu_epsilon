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

;;;;; Load advanced epsilon1 features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/fixed-point.e"))
(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/command-line.e"))
(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/repl.e"))
(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/compiler.e"))
(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/analyses-and-optimizations.e"))
(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/new-compiler.e"))


;;;;; Unexec a non-Guile REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(e1:toplevel
  (fio:write "Unexecing an epsilon1 REPL into "
             (st bootstrap:repl-image-file-name)
             "\n")
  (e1:unexec bootstrap:repl-image-file-name
    (repl:repl))
;;)
