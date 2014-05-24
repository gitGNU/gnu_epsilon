;;;;; Export epsilon1 toplevel forms to Guile -*- Scheme -*-

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


;;; This simple facility lets us save some ugly e1:toplevel forms
;;; at the top level in Guile.

(export-to-guile-toplevel e1:define)
(export-to-guile-toplevel e1:define-macro)

(export-to-guile-toplevel e1:define-variadic-left-deep)
(export-to-guile-toplevel e1:define-variadic-right-deep)
(export-to-guile-toplevel e1:define-variadic-associative)

(export-to-guile-toplevel e1:define-record)
(export-to-guile-toplevel e1:define-sum)
(export-to-guile-toplevel e1:define-sum-open)
(export-to-guile-toplevel e1:extend-sum)

(export-to-guile-toplevel e1:define-with-keywords)
(export-to-guile-toplevel e1:define-keyword-syntax)
(export-to-guile-toplevel e1:define-keyword-syntax)

(export-to-guile-toplevel e1:define-regexp)

(export-to-guile-toplevel c64:sprite)
(export-to-guile-toplevel c64:parse-sprite)
(export-to-guile-toplevel e1:define-c64-single-color-sprite)
(export-to-guile-toplevel e1:define-c64-multi-color-sprite)
