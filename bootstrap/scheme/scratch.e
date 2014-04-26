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

(e1:define (fibo n)
  (e1:if (fixnum:< n 2)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))
(e1:define (test n)
  (e1:dotimes (i n)
    (fio:write "fibo(" (i i) ") = " (i (fibo i)) "\n")))

;;; Long division.  This implementation is useful for very small
;;; machines as it only requires sum, subtraction, bitwise and and
;;; or, comparison and 1-bit shifts.
(e1:define (fixnum:/-unsigned n d)
  (e1:let ((initial-i (fixnum:- (fixnum:* configuration:sizeof_void_p 8)
                                2))) ;; FIXME: make it 1 on untagged runtime
    (fixnum:/-unsigned-helper n d (fixnum:left-shift 1 initial-i) 0 0)))
(e1:define (fixnum:/-unsigned-helper n d mask q r)
  (e1:if (fixnum:zero? mask)
    (e1:bundle q r)
    (e1:let ((r (e1:if (fixnum:bitwise-and n mask)
                  (fixnum:bitwise-or (fixnum:left-shift r 1) 1)
                  (fixnum:left-shift r 1)))
             (next-mask (fixnum:logic-right-shift mask 1)))
      (e1:if (fixnum:>= r d)
        (fixnum:/-unsigned-helper n d next-mask (fixnum:bitwise-or q mask) (fixnum:- r d))
        (fixnum:/-unsigned-helper n d next-mask q r)))))
;; (e1:define (fixnum:/ n d)
;;   (e1:cond ((fixnum:zero? d)
;;             (e1:error "division by zero"))
;;            ((fixnum:< d 0)
;;             (e1:if (fixnum:< n 0)
;;               (e0:let (q r) (fixnum:/-unsigned (fixnum:negate n) (fixnum:negate d))
;;                 (e1:bundle 
;;               (fixnum:negate (fixnum:/-unsigned n (fixnum:negate d)))))
;;            (else
;;             (e1:if (fixnum:< n 0)
;;               (fixnum:negate (fixnum:/-unsigned (fixnum:negate n) d))
;;               (fixnum:/-unsigned n d)))))
