;;;;; Bootstrap driver in Guile -*- Scheme -*-.

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Updated in 2013 by Luca Saiu
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

;;(load "compiler.e")

;;;;; Save the current state, to make quick-start.scm work:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format #t "Saving the state to make quick-start.scm work later...\n")
;;(e1:toplevel (unexec:quick-save) 57)
(e1:toplevel (marshal:marshal (e0:value "quick-start.m") symbol:table))
(format #t "...done\n")


;;;;; Load the scratch file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notice the changed performed in scratch.scm are *not* saved.  This
;;; is intentional.

(load "scratch.scm")
