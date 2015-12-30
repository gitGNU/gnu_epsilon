;;;;; Bootstrap driver in Guile Scheme, of course with some -*- epsilon -*-.

;;;;; Copyright (C) 2015 Luca Saiu
;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Updated in 2013 and 2014 by Luca Saiu
;;;;; Written by Luca Saiu

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


;;;;; Bootstrap: make epsilon1 usable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "epsilon0-in-scheme.scm")
(load "core.e")
(load "unexec.e")
(load "fill-reflective-structures.scm")
(load "toplevel-in-scheme.scm")


;;; This is the heavyweight definition of epsilon1 built on epsilon0.  It should
;;; contain everything needed to unexec an image to be execed in an image
;;; interpreter, with no Guile.
(load "epsilon1.scm")

;;; Load configuration-dependant stuff.  This needs the
;;; EPSILON_BUILD_PATH environment variable to be defined.
(define epsilon-build-path (getenv "EPSILON_BUILD_PATH"))
(unless epsilon-build-path
  (error "the environment variable EPSILON_BUILD_PATH is not defined"))
(load (string-append epsilon-build-path "/bootstrap/scheme/configuration.e"))

;;; Unexec, so that the rest can be run faster after execing from the image
;;; interpreter, without using Guile.

(e1:define bootstrap:unexec-repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/unexec-repl-from-guile.u"))
(e1:define bootstrap:unexec-repl-source-file-name
  (string:append configuration:abs_top_srcdir
                 configuration:dir_separator
                 "bootstrap/scheme/unexec-repl.e"))
(e1:define bootstrap:repl-image-file-name
  (string:append configuration:abs_top_builddir
                 configuration:dir_separator
                 "dumps/repl-from-guile.u"))
(e1:toplevel
   (fio:write "Unexecing an epsilon1 image loading the rest into "
              (st bootstrap:unexec-repl-image-file-name) "...\n")
   (e1:unexec bootstrap:unexec-repl-image-file-name
     (fio:write "Still alive, working to unexec a REPL.  Hello from "
                (st bootstrap:unexec-repl-source-file-name)
                "\n")
     (fio:write "About to load " (st bootstrap:unexec-repl-source-file-name) "\n")
     (e1:load bootstrap:unexec-repl-source-file-name))
   (fio:write "... done: " (st bootstrap:repl-image-file-name) " should now contain a working REPL image.\n"))

(display "Unexeced.  We no longer need Guile from now on.\n")
