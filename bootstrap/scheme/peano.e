;;;;; This is -*- epsilon -*-
;;;;; Peano naturals: a test and micro-benchmark

;;;;; Copyright (C) 2013 Luca Saiu
;;;;; Updated in 2014 by Luca Saiu

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


;;;;; Peano naturals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel (sum:define peano:peano
               (zero)
               (successor peano)))

(e1:define (peano:fixnum->peano n)
  (peano:fixnum->peano-acc n (peano:peano-zero)))
(e1:define (peano:fixnum->peano-acc n a)
  (e1:if (fixnum:zero? n)
    a
    (peano:fixnum->peano-acc (fixnum:1- n)
                             (peano:peano-successor a))))

(e1:define (peano:peano->fixnum p)
  (peano:peano->fixnum-acc p 0))
(e1:define (peano:peano->fixnum-acc p a)
  (e1:match p
    ((peano:peano-zero)
     a)
    ((peano:peano-successor p-minus-1)
     (peano:peano->fixnum-acc p-minus-1 (fixnum:1+ a)))))

(e1:define peano:0
  (peano:peano-zero))

(e1:define peano:1
  (peano:peano-successor peano:0))

(e1:define (peano:zero? a)
  (e1:match a
    ((peano:peano-zero)
     #t)
    (_
     #f)))

(e1:define (peano:< a b)
  (e1:cond ((peano:zero? a)
            (e1:not (peano:zero? b)))
           ((peano:zero? b)
            #f)
           (else
            (peano:< (peano:1- a) (peano:1- b)))))

(e1:define (peano:= a b)
  (e1:cond ((peano:zero? a)
            (peano:zero? b))
           ((peano:zero? b)
            #f)
           (else
            (peano:= (peano:1- a) (peano:1- b)))))

(e1:define (peano:<= a b)
  (peano:< a (peano:1+ b)))

(e1:define (peano:> a b)
  (peano:< b a))

(e1:define (peano:>= a b)
  (peano:<= b a))

(e1:define (peano:1+ a)
  (peano:peano-successor a))

(e1:define (peano:1- a)
  (e1:match a
    ((peano:peano-zero)
     (e1:error "peano:1-: the argument is zero"))
    ((peano:peano-successor a-minus-1)
     a-minus-1)))

(e1:define (peano:+ a b)
  (e1:match a
    ((peano:peano-zero)
     b)
    ((peano:peano-successor a-minus-1)
     (peano:+ a-minus-1
              (peano:peano-successor b)))))

(e1:define (peano:- a b)
  (e1:match b
    ((peano:peano-zero)
     a)
    ((peano:peano-successor b-minus-1)
     (peano:- (peano:1- a)
              b-minus-1))))

(e1:define (peano:* a b)
  (e1:if (peano:< a b)
    (peano:*-acc a b peano:0)
    (peano:*-acc b a peano:0)))
(e1:define (peano:*-acc a b acc)
  (e1:match b
    ((peano:peano-zero)
     acc)
    ((peano:peano-successor b-minus-1)
     (peano:*-acc a
                  b-minus-1
                  (peano:+ a acc)))))

(e1:define (peano:** a b)
  (peano:**-acc a b peano:1))
(e1:define (peano:**-acc a b acc)
  (e1:match b
    ((peano:peano-zero)
     acc)
    ((peano:peano-successor b-minus-1)
     (peano:**-acc a
                   b-minus-1
                   (peano:* a acc)))))

(e1:define (peano:/% a b)
  (peano:/%-acc a b peano:0))
(e1:define (peano:/%-acc a b quotient-acc)
  (e1:if (peano:< a b)
    (e0:bundle quotient-acc
               a)
    (peano:/%-acc (peano:- a b)
                  b
                  (peano:1+ quotient-acc))))

(e1:define (peano:/ a b)
  (e0:let (quotient remainder) (peano:/% a b)
    quotient))
(e1:define (peano:% a b)
  (e0:let (quotient remainder) (peano:/% a b)
    remainder))


;;;;; User interface: more convenient testing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (p->f a)
  (peano:peano->fixnum a))
(e1:define (f->p a)
  (peano:fixnum->peano a))

(e1:define (peano:test a b)
  (peano:peano->fixnum (peano:% (peano:fixnum->peano a)
                                (peano:fixnum->peano b))))
(e1:define (peano:test- a b)
  (peano:peano->fixnum (peano:- (peano:fixnum->peano a)
                                (peano:fixnum->peano b))))
(e1:define (peano:test** a b)
  (peano:peano->fixnum (peano:** (peano:fixnum->peano a)
                                 (peano:fixnum->peano b))))
(e1:define (peano:test/ a b)
  (peano:peano->fixnum (peano:/ (peano:fixnum->peano a)
                                (peano:fixnum->peano b))))
(e1:define (peano:test% a b)
  (peano:peano->fixnum (peano:% (peano:fixnum->peano a)
                                (peano:fixnum->peano b))))
