;;;;; Quickstart driver in -*- Scheme -*-, using unexec

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Updated in 2014 by Luca Saiu
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


;;; Load our quick-n-dirty epsilonzero-in-Guile implementation:
(load "epsilon0-in-scheme.scm")

;;; Define the main reflective data structures, so that we can at
;;; least use symbols and global definitions.  This is rather quick:
(load "core.e")

;;; Now run unexec on our epsilonzero-in-Guile implementation, so that
;;; we rebuild the saved symbol table:
(load "unexec.e")
(format #t "Loading the saved symbol table...\n")
(define epsilon-build-path (getenv "EPSILON_BUILD_PATH"))
(unless epsilon-build-path
  (error "the environment variable EPSILON_BUILD_PATH is not defined"))
(define symbol:table
  (marshal:unmarshal (string:append2 (guile-string->string epsilon-build-path)
                                     (e0:value "/dumps/quick-start.dump"))))
(format #t "Done.\n")

;;; Make the system usable from Guile
(load "toplevel-in-scheme.scm")
(load "export-toplevel-forms-to-guile.scm")


;;;;; Unexec a native REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "unexec-repl.e")


;;;;; Load the scratch file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "scratch.e")
