;;;;; Bootstrap driver in Guile -*- Scheme -*-.

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


;;;;; Bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "epsilon0-in-scheme.scm")
(load "core.e")
(load "unexec.e")
(load "fill-reflective-structures.scm")
(load "toplevel-in-scheme.scm")
(load "epsilon1.scm")
(load "compiler.e")

;; Load configuration-dependant stuff.  This needs the
;; EPSILON_BUILD_PATH environment variable to be defined.
(define epsilon-build-path (getenv "EPSILON_BUILD_PATH"))
(unless epsilon-build-path
  (error "the environment variable EPSILON_BUILD_PATH is not defined"))
(load (string-append epsilon-build-path "/bootstrap/scheme/configuration.e"))

;; Load the configuration-dependant stuff again, this time into the
;; epsilon1 state environments.  Now we can use configuration:abs_top_builddir,
;; which was defined in the epsilon state environment from Guile.
(e1:toplevel (e1:load (string:append configuration:abs_top_builddir
                                     "/bootstrap/scheme/configuration.e")))


;;;;; Save the current state, to make quick-start.scm work:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format #t "Saving the state to make quick-start.scm work later...\n")
(e1:toplevel (marshal:marshal (string:append configuration:abs_top_builddir
                                             (e0:value "/repl/quick-start.dump"))
                              symbol:table))
(format #t "...done\n")


;;;;; Unexec a native REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "unexec-repl.e")


;;;;; Load the scratch file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "scratch.e")
