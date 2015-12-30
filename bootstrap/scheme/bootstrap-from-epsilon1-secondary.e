;;;;; This is -*- epsilon -*-.
;;;;; Bootstrapping code: generate REPL-generating image, from secondary:load.

;;;;; Copyright (C) 2015 Luca Saiu

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


(e1:define bootstrap:unexec-repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/unexec-repl-from-epsilon1.u"))
(e1:define bootstrap:unexec-repl-source-file-name
  (string:append configuration:abs_top_srcdir
                 configuration:dir_separator
                 "bootstrap/scheme/unexec-repl.e"))
(e1:define bootstrap:repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/repl-from-epsilon1.u"))
(fio:write "Unexecing an epsilon1 image loading the rest into "
           (st bootstrap:unexec-repl-image-file-name) "...\n")

;;; Invalidate the secondary symbol table, which shouldn't be visible after unexec
;;; and shouldn't be dumped.
(e1:define secondary:symbol-table 12345)

(e1:unexec bootstrap:unexec-repl-image-file-name
  (fio:write "Still alive, working to unexec a REPL.  Hello from "
             (st bootstrap:unexec-repl-source-file-name)
             "\n")
  (fio:write "About to load " (st bootstrap:unexec-repl-source-file-name) "\n")
  (e1:load bootstrap:unexec-repl-source-file-name))
(fio:write "Now please run "
           (st bootstrap:unexec-repl-image-file-name)
           " to generate "
           (st bootstrap:repl-image-file-name)
           " .\n")
