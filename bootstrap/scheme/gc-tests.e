;;;;; This is -*- epsilon -*-
;;;;; GC tests and microbenchmarks

;;;;; Copyright (C) 2013 Luca Saiu

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

(e1:define (sharealot n)
  (e1:if (fixnum:zero? n)
    0
    (e1:let* ((subtree (sharealot (fixnum:1- n))))
      (tuple:make subtree subtree))))

(e1:define (dontshare n)
  (e1:if (fixnum:zero? n)
    0
    (tuple:make (dontshare (fixnum:1- n))
                (dontshare (fixnum:1- n)))))

(e1:define (complexity q)
  (e1:if (fixnum:zero? q)
    1
    (fixnum:+ (complexity (tuple:get q 0))
              (complexity (tuple:get q 0))
              1)))

(e1:define (complexityp q)
  (e0:if-in q (0)
    1
    (e0:primitive fixnum:1+
                  (e0:primitive fixnum:+
                                (complexityp (e0:primitive buffer:get q 0))
                                (complexityp (e0:primitive buffer:get q 0))))))

(e1:define global-datum
  ;;(list:iota 4)
  symbol:table
  )

(e1:define (fact n)
  (e1:if (fixnum:zero? n)
    1
    (fixnum:* n (fact (fixnum:1- n)))))
(e1:define (mm)
  (e1:dotimes (n 12)
    (fio:write "The factorial of " (i n) " is " (i (fact n)) "\n")))
