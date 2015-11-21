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

(e1:toplevel
 (e1:let ((file-name (string:append configuration:abs_top_builddir
                                    configuration:dir_separator
                                    "dumps/unexec-repl.u")))
   (fio:write "Unexecing an epsilon1 image loading the rest into " (st file-name) "...\n")
   (e1:unexec file-name
     (fio:write "Still alive, working to unexec a REPL.  Hello from unexec-repl.e...\n")
     (fio:write "About to load "
                (st (string:append configuration:abs_top_srcdir
                                   configuration:dir_separator
                                   "bootstrap/scheme/unexec-repl.e"))
                "\n")
     (e1:load (string:append configuration:abs_top_srcdir
                             configuration:dir_separator
                             "bootstrap/scheme/unexec-repl.e")))
   (fio:write "... done.\n")))

(display "Unexeced epsilon1.u.  We no longer need Guile from now on.\n")
