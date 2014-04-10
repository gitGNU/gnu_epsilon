;;;;; Unexec a native REPL.  This is executed late after initialization.

;;;;; Copyright (C) 2014  Luca Saiu

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


;;; Load the scratch file, which is not included into our
;;; automatically-built unexec dumps.
(e1:define (repl:load-scratch)
  (e1:load (string:append configuration:abs_top_srcdir
                          configuration:dir_separator
                          "bootstrap/scheme/scratch.e")))

(e1:toplevel
   (e1:let ((file-name (string:append configuration:abs_top_builddir
                                      configuration:dir_separator
                                      "dumps/repl.u")))
     (fio:write "Unexecing an epsilon1 REPL into " (st file-name) "\n")
     (e1:unexec file-name
       (repl:load-scratch)
       (repl:repl))))
