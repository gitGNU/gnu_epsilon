;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2014 Luca Saiu

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


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (fact n)
  (e1:if (fixnum:zero? n)
    1
    (fixnum:* n (fact (fixnum:1- n)))))

(e1:define (gauss n)
  (e1:if (fixnum:zero? n)
    0
    (fixnum:+ n (gauss (fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2);;(e1:if-in n (0 1)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))

(e1:define (wait-till limit)
  (e1:let ((current (c64:get-jiffies)))
    ;;(fio:write "current: " (i current) ", limit: " (i limit) "\n")
    (e1:if (fixnum:< current limit)
      (wait-till limit)
      current)))

(e1:define (every limit)
  (e1:let ((now (wait-till limit)))
    (fio:write ".")
    (every (fixnum:+ now 50))))
