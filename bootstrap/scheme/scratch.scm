;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2012, 2013 Luca Saiu
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


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(load "cps.scm")
;;(load "compiler.scm")
;;(load "pattern-matching.e")

(e1:define-macro (n-times-in-parallel n . forms)
  (e1:let* ((formal-name (sexpression:fresh-symbol))
            (list-name (sexpression:fresh-symbol)))
    `(e1:let* ((,list-name (list:map (e1:lambda (,formal-name)
                                       (e1:future ,@forms))
                                     (list:iota ,n))))
       (vector:list->vector (list:map (e1:lambda (,formal-name)
                                        (e1:join ,formal-name))
                                      ,list-name)))))

(define (fibo-guile n) (if (< n 2) n (+ (fibo-guile (- n 2)) (fibo-guile (- n 1)))))
(e1:define (fibo n) (e0:if-in n (0 1) n (fixnum:+ (fibo (fixnum:- n (e0:value 2))) (fibo (fixnum:1- n)))))
(e1:define (fibo-a t n) (e0:if-in n (0 1) n (fixnum:+ (fibo-a t (fixnum:- n (e0:value 2))) (fibo-a t (fixnum:1- n)))))
(e1:define (fibop n) (e0:if-in n (0 1) n (e0:primitive fixnum:+ (fibop (e0:primitive fixnum:- n (e0:value 2))) (fibop (e0:primitive fixnum:1- n)))))

(e1:define (fact n) (e0:if-in n (0) (e0:value 1) (fixnum:* n (fact (fixnum:1- n)))))
(e1:define (fact-acc n a) (e0:if-in n (0) a (fact-acc (fixnum:1- n) (fixnum:* n a))))
(e1:define (fact-acc-a t n a) (e0:if-in n (0) a (fact-acc-a t (fixnum:1- n) (fixnum:* n a))))
(e1:define (factp n) (e0:if-in n (0) (e0:value 1) (e0:primitive fixnum:* n (fact (e0:primitive fixnum:1- n)))))
(e1:define (return-constant) 42)
(e1:define (return-local) (e0:let (a) 25 a))
(e1:define a-global 47)
(e1:define (return-global) a-global)
;;(e1:define (f n) (fibo n))
(e1:define (f n) (fixnum:+ n 500 500))

(e1:define (zerotozero) (e0:bundle))
(e1:define (onetozero x) (e1:let* ((q x)) (e0:bundle)))
(e1:define (zerotoone) 42)
(e1:define (zerototwo) (e0:bundle 10 20))
(e1:define (twotozero x y) (e1:let* ((q1 x) (q2 y)) (e0:bundle)))
(e1:define (simplertwotozero x y) (e0:let (q1) x (e0:let (q2) y (e0:bundle))))
(e1:define (average a b) (fixnum:/ (fixnum:+ a b) 2))
(e1:define (call-indirect-1 f x) (e0:primitive fixnum:1+ (e0:call-indirect f x)))
(e1:define (call-indirect-2 f x) (e0:call-indirect f x))
