;;;;; -*- Scheme -*-, plus something close enough for Emacs
;;;;; Minor debugging and test stuff, to be used while bootstrapping.

;;;;; Copyright (C) 2012 Université Paris 13
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


;;;;;; Fixed-point numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fixedpoint:scale
  (fixnum:left-shift (e0:value 1) fixedpoint:fractional-bit-no))
(define fixedpoint:scale-for-scheme
  (whatever->guile-fixnum fixedpoint:scale))
(define (fixedpoint:float->fixedpoint float)
  (guile-sexpression->whatever (inexact->exact (truncate (* float fixedpoint:scale-for-scheme)))))
(define (fixedpoint:fixedpoint->float a)
  (exact->inexact (: (whatever->guile-fixnum a) fixedpoint:scale-for-scheme)))
;;; For testing fixed-point precision.  This is pretty cool!
(define (fixedpoint:error original-float)
  (let ((fixed (fixedpoint:float->fixedpoint original-float)))
    (abs (- original-float (fixedpoint:fixedpoint->float fixed)))))
(define (fixedpoint:maximum-error)
  (apply max (map-reversed (lambda whatever
                             (let ((q (random 1.0)))
                               (fixedpoint:error q)))
                           (iota 10000))))
;;(dump (fixedpoint:maximum-error))
;(exit 0)


(define (fibo-guile n) (if (< n 2) n (+ (fibo-guile (- n 2)) (fibo-guile (- n 1)))))
(e1:define (fibo n) (e0:if-in n (0 1) n (fixnum:+ (fibo (fixnum:- n (e0:value 2))) (fibo (fixnum:- n (e0:value 1))))))
(e1:define (fibo-a t n) (e0:if-in n (0 1) n (fixnum:+ (fibo-a t (fixnum:- n (e0:value 2))) (fibo-a t (fixnum:- n (e0:value 1))))))

(e1:define (fact n) (e0:if-in n (0) (e0:value 1) (fixnum:* n (fact (fixnum:1- n)))))
(e1:define (fact-acc n a) (e0:if-in n (0) a (fact-acc (fixnum:1- n) (fixnum:* n a))))
(e1:define (fact-acc-a t n a) (e0:if-in n (0) a (fact-acc-a t (fixnum:1- n) (fixnum:* n a))))

