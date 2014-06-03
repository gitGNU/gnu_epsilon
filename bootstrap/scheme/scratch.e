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
  (e1:if-in n (0)
    1
    (fixnum:* n (fact (fixnum:1- n)))))

(e1:define (gauss n)
  (e0:if-in n (0)
    0
    (e0:primitive fixnum:+ n (gauss (e0:primitive fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))
(e1:define (test n)
  (e1:dotimes (i n)
    (fio:write "fibo(" (i i) ") = " (i (fibo i)) "\n")))

(e1:define (iter a b)
  (e1:if-in a (0)
    b
    (iter (e0:primitive fixnum:1- a)
          (e0:primitive fixnum:1+ b))))


;;;;; What follows is tentative code: Commodore 64 tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (stripes-once n)
  (e1:primitive io:store-byte! 53280 n)
  (e1:primitive io:store-byte! 53281 n)
  (e0:if-in n (0)
    (e1:bundle)
    (stripes-once (e1:primitive fixnum:1- n))))
(e1:define (stripes)
  (stripes-once 15)
  (stripes))

(e1:define v 53248)

(e1:define (show-sprite! n)
  (set-sprite-visibility! n #t))
(e1:define (hide-sprite! n)
  (set-sprite-visibility! n #f))

(e1:define (set-sprite-block! n block)
  (io:store-byte! (fixnum:+ 2040 n)
                  block))
(e1:define (set-sprite-configuration! n address)
  (set-sprite-block! n
                     (fixnum:non-primitive-logic-right-shift address 6)))
(e1:define (set-sprite-visibility! n visibility)
  (e1:let* ((enable-address (fixnum:+ v 21))
            (old-byte (io:load-byte enable-address))
            (new-byte
             (e1:if visibility
               (fixnum:bitwise-or old-byte (fixnum:non-primitive-left-shift 1 n))
               (fixnum:bitwise-and old-byte (fixnum:bitwise-not (fixnum:non-primitive-left-shift 1 n))))))
    (io:store-byte! enable-address new-byte)))
(e1:define (move-sprite! n x y)
  (e1:let* ((x-address (fixnum:+ v (fixnum:double n)))
            (y-address (fixnum:+ v 1 (fixnum:double n))))
    (io:store-byte! x-address x)
    (io:store-byte! y-address y)))

(e1:define (go-sprites)
  (set-sprite-configuration! 0 0)
  (move-sprite! 0 100 100)
  (show-sprite! 0)
  (move-sprite! 2 200 150)
  (show-sprite! 2)
  (move-sprite! 1 255 200)
  (show-sprite! 1)
  (animate-sprite 0 20 1 100)
  )
(e1:define x-min 10)
(e1:define x-max 245);;120)
(e1:define (animate-sprite n x dx y)
  (move-sprite! n x y)
  (e1:cond ((fixnum:> x x-max)
            ;;(fio:write "R")
            (animate-sprite n x-max -1 y))
           ((fixnum:< x x-min)
            ;;(fio:write "L")
            (animate-sprite n x-min 1 y))
           (else
            (e1:if (fixnum:> dx 0)
              (fio:write "r")
              (fio:write "l"))
            (animate-sprite n (fixnum:+ x dx) dx y))))
(e1:toplevel
(e1:when #f
  (can "/tmp/q.a" (stripes))
  (can "/tmp/q.a" (go-sprites))
  ))
(e1:toplevel (e1:when #f
  ;;(e1:define configuration:bits-per-word 16)
  (e1:define (fixnum:* a b) (fixnum:non-primitive-* a b))
  (e1:define (fixnum:/% a b) (fixnum:non-primitive-/% a b))
  (e1:define (fixnum:/ a b) (fixnum:non-primitive-/ a b))
  (e1:define (fixnum:% aq bq) (fixnum:non-primitive-% aq bq))
  (e1:define (fixnum:left-shift a b) (fixnum:non-primitive-left-shift a b))
  (e1:define (fixnum:arithmetic-right-shift a b) (fixnum:non-primitive-arithmetic-right-shift a b))
  (e1:define (fixnum:logic-right-shift a b) (fixnum:non-primitive-logic-right-shift a b))
  ))

;; FIXME: test (fixnum:> x 200) or (fixnum:< x 10).  I guess it gives an incorrect result
;; for x > 128more than 7-bit x's.
;; FIXME: I think I've found it: (fixnum:< 140 10) ==> #t.  It's a bug in fixnum:<

;; (can "/tmp/q.a" (e1:dolist (s (list:list "foo" "bar")) (fio:write s "\n")))
