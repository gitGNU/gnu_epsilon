;;;;; Fixed-point numbers in -*- epsilon -*-.

;;;;; Copyright (C) 2013, 2014, 2015  Luca Saiu

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


;;; Fixed-point numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We reserve this many bits for the fractional part of each
;;; fixed-point number, in the the least significant part of the word.
;;; All the other bits will be used for the integer part and the sign
;;; bit; we assume two's complement representation for integers.
;;; Warning: using more than 30 bits causes the bitmask to be
;;; undumptable in the current system based on 32-bit words for all
;;; architactures.
(e1:define fixedpoint:fractional-bit-no
  (e0:value 16))

;;; This is used for some conversions:
(e1:define fixedpoint:fractional-bitmask
  (fixnum:1- (fixnum:left-shift (e0:value 1) fixedpoint:fractional-bit-no)))

(e1:define fixedpoint:fractional-bit-no/2
  (fixnum:half fixedpoint:fractional-bit-no))

(e1:define (fixedpoint:fixnum->fixedpoint fixnum)
  (fixnum:left-shift fixnum fixedpoint:fractional-bit-no))

;;; Basic arithmetic:
(e1:define (fixedpoint:+ a b)
  (fixnum:+ a b))
(e1:define (fixedpoint:1+ a)
  (fixnum:+ a fixedpoint:1))
(e1:define (fixedpoint:1- a)
  (fixnum:- a fixedpoint:1))
(e1:define (fixedpoint:negate a)
  (fixnum:negate a))
(e1:define (fixedpoint:- a b)
  (fixnum:- a b))

(e1:define (fixedpoint:* a b)
  (fixnum:arithmetic-right-shift (fixnum:* a b) fixedpoint:fractional-bit-no))

;; FIXME: this version is less subject to overflow but loses more information in
;; the fractional part.  Shall I keep both?
;; (e1:define (fixedpoint:* a b)
;;   (fixnum:* (fixnum:arithmetic-right-shift a fixedpoint:fractional-bit-no/2)
;;             (fixnum:arithmetic-right-shift b fixedpoint:fractional-bit-no/2)))

(e1:define (fixedpoint:/ a b)
  (fixnum:/ (fixnum:left-shift a fixedpoint:fractional-bit-no) b))
(e1:define (fixedpoint:sign a)
  (fixnum:sign a))
(e1:define (fixedpoint:half a)
  (fixnum:half a))
(e1:define (fixedpoint:double a)
  (fixnum:double a))
(e1:define (fixedpoint:square x)
  (fixedpoint:* x x))

;;; Comparisons:
(e1:define (fixedpoint:zero? a)
  (fixnum:zero? a))
(e1:define (fixedpoint:< a b)
  (fixnum:< a b))
(e1:define (fixedpoint:> a b)
  (fixnum:> a b))
(e1:define (fixedpoint:<= a b)
  (fixnum:<= a b))
(e1:define (fixedpoint:>= a b)
  (fixnum:>= a b))
(e1:define (fixedpoint:= a b)
  (fixnum:= a b))
(e1:define (fixedpoint:<> a b)
  (fixnum:<> a b))

;;; Conversion:
(e1:define (fixedpoint:fixedpoint->fixnum fixed)
  (fixnum:arithmetic-right-shift fixed fixedpoint:fractional-bit-no))

;;; More computation:
(e1:define (fixedpoint:mod a)
  (fixnum:mod a))

(e1:define (fixedpoint:get-integer-part fixed)
  (fixnum:arithmetic-right-shift fixed fixedpoint:fractional-bit-no))
(e1:define (fixedpoint:get-fractional-part fixed digit-no)
  (e0:let (possibly-negative-result)
          (fixnum:arithmetic-right-shift (fixnum:* (fixnum:bitwise-and fixed fixedpoint:fractional-bitmask)
                                                   (fixnum:** (e0:value 10) digit-no))
                                         fixedpoint:fractional-bit-no)
    (fixnum:mod possibly-negative-result)))


;;;;; Fixed-point reading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (reader:string-in-simple-dot-notation->fixed-point s)
  (e1:let ((c (string:get s 0)))
    (e1:case c
      ((#\+)
       (reader:string-in-simple-dot-notation->fixed-point-helper s 1 0))
      ((#\-)
       (fixedpoint:negate
        (reader:string-in-simple-dot-notation->fixed-point-helper s 1 0))
       )
      (else
       (reader:string-in-simple-dot-notation->fixed-point-helper s 0 0)))))
(e1:define (reader:string-in-simple-dot-notation->fixed-point-helper s i acc)
  (e1:if (fixnum:= i (string:length s))
    (fixedpoint:fixnum->fixedpoint acc) ;; also recognize an integer with no dot
    (e1:let ((c (string:get s i)))
      (e1:if (whatever:eq? c #\.)
        (e1:let ((integer-part (fixedpoint:fixnum->fixedpoint acc))
                 (fractional-part (fixedpoint:fractional-digits-string-to-bitmask s (fixnum:1+ i))))
          (fixnum:bitwise-or integer-part fractional-part))
        (e1:let ((c-digit (reader:character-value c 10)))
          (reader:string-in-simple-dot-notation->fixed-point-helper
           s
           (fixnum:1+ i)
           (fixnum:+ c-digit (fixnum:* acc 10))))))))

;;; This implementation is quite inefficient and not particularly beautiful.  It
;;; follows the usual doubling algorithm.

;;; The result is reversed
(e1:define (fixedpoint:increment-reversed-decimal-digits dd)
  (e1:cond ((list:null? dd)
            (list:list 1))
           ((fixnum:= (list:head dd) 9)
            (list:cons 0
                       (fixedpoint:increment-reversed-decimal-digits (list:tail dd))))
           (else
            (list:cons (fixnum:1+ (list:head dd))
                       (list:tail dd)))))

;;; The result is reversed
(e1:define (fixedpoint:double-reversed-decimal-digits dd)
  (e1:match dd
    ((list:list-nil)
     list:nil)
    ((list:list-cons first rest)
     (e1:let ((doubled-first (fixnum:* first 2)))
       (e1:if (fixnum:>= doubled-first 10)
          (list:cons (fixnum:- doubled-first 10)
                     (fixedpoint:increment-reversed-decimal-digits (fixedpoint:double-reversed-decimal-digits rest)))
          (list:cons doubled-first
                     (fixedpoint:double-reversed-decimal-digits rest)))))))

(e1:define (fixedpoint:double-decimal-digits dd)
  (list:reverse (fixedpoint:double-reversed-decimal-digits (list:reverse dd))))

(e1:define (fixedpoint:fractional-decimal-digits-to-binary-digits-helper dd remaining-digits)
  (e1:if (fixnum:zero? remaining-digits)
    list:nil
    (e1:let ((doubled-dd (fixedpoint:double-decimal-digits dd)))
      (e1:if (fixnum:= (list:length dd) (list:length doubled-dd))
        (list:cons 0
                   (fixedpoint:fractional-decimal-digits-to-binary-digits-helper doubled-dd
                                                                                 (fixnum:1- remaining-digits)))
        (list:cons 1
                   (fixedpoint:fractional-decimal-digits-to-binary-digits-helper (list:tail doubled-dd)
                                                                                 (fixnum:1- remaining-digits)))))))

(e1:define (fixedpoint:fractional-decimal-digits->binary-digits dd)
  (fixedpoint:fractional-decimal-digits-to-binary-digits-helper dd fixedpoint:fractional-bit-no))

(e1:define (fixedpoint:fractional-decimal-digits->bitmask dd)
  (e1:let ((bb (fixedpoint:fractional-decimal-digits->binary-digits dd)))
    (fixedpoint:fractional-binary-digits->bitmask bb)))

(e1:define (fixedpoint:fractional-binary-digits->bitmask bb)
  (fixedpoint:reversed-fractional-binary-digits->bitmask (list:reverse bb)
                                                         1
                                                         0))
(e1:define (fixedpoint:reversed-fractional-binary-digits->bitmask rbb value acc)
  (e1:if (list:null? rbb)
    acc
    (e1:let ((new-rbb (list:tail rbb))
             (new-value (fixnum:left-shift-1-bit value))
             (new-acc (e1:case (list:head rbb)
                        ((0) acc)
                        ((1) (fixnum:bitwise-or acc value))
                        (else (e1:error "unreachable")))))
      (fixedpoint:reversed-fractional-binary-digits->bitmask new-rbb
                                                             new-value
                                                             new-acc))))

(e1:define (fixedpoint:fractional-digits-string-to-digits s index)
  (e1:if (fixnum:= index (string:length s))
    list:nil
    (list:cons (character:character->fixnum (string:get s index))
               (fixedpoint:fractional-digits-string-to-digits s (fixnum:1+ index)))))

(e1:define (fixedpoint:fractional-digits-string-to-bitmask digits-string after-dot-index)
  (e1:let ((dd (fixedpoint:fractional-digits-string-to-digits digits-string after-dot-index)))
    (fixedpoint:fractional-decimal-digits->bitmask dd)))


;;;;; Fixed-point printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (printer:write-fixed-point port fp)
  (e1:if (fixnum:= (fixedpoint:sign fp) -1)
    (e1:begin
      (io:write-character port #\-)
      (printer:write-fixed-point port (fixedpoint:negate fp)))
    (e1:begin
      (printer:write-fixnum port (fixedpoint:get-integer-part fp))
      (io:write-character port #\.)
      (printer:write-fixed-point-fractional-part port (fixnum:bitwise-and fixedpoint:fractional-bitmask fp)))))
(e1:define (printer:write-fixed-point-fractional-part port fractional-part-only)
  (e1:if (fixnum:zero? fractional-part-only)
    (e1:bundle)
    (e1:let* ((decimal-digit
               (fixnum:bitwise-and (fixnum:bitwise-not fixedpoint:fractional-bitmask)
                                   (fixnum:* fractional-part-only 10)))
              (digit-as-fixnum
               (fixnum:logic-right-shift decimal-digit fixedpoint:fractional-bit-no)))
      (io:write-character port (fixnum:+ #\0 digit-as-fixnum))
      (printer:write-fixed-point-fractional-part port (fixnum:bitwise-and fixedpoint:fractional-bitmask (fixnum:* fractional-part-only 10))))))


;;;;; Load machine-generated tables for fixed-point procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:load (string:append configuration:abs_top_srcdir
                        configuration:dir_separator
                        "bootstrap/scheme/fixed-point-tables.e"))


;;;;; Fixed-point constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define fixedpoint:0
  0)
(e1:define fixedpoint:1
  (reader:string-in-simple-dot-notation->fixed-point "1.0"))
(e1:define fixedpoint:-1
  (fixnum:- fixedpoint:1))
(e1:define fixedpoint:2
  (reader:string-in-simple-dot-notation->fixed-point "2.0"))
(e1:define fixedpoint:10
  (reader:string-in-simple-dot-notation->fixed-point "10.0"))

;; These are machine-generated, with a lot of digits.
(e1:define fixedpoint:e
  fixedpoint-tables:e)
(e1:define fixedpoint:pi
  fixedpoint-tables:pi)

(e1:define fixedpoint:-pi
  (fixnum:- fixedpoint:pi))
(e1:define fixedpoint:2pi
  (fixnum:* 2 fixedpoint:pi))
(e1:define fixedpoint:-2pi
  (fixnum:- fixedpoint:2pi))
(e1:define fixedpoint:pi/2
  (fixnum:/ fixedpoint:pi 2))
(e1:define fixedpoint:-pi/2
  (fixnum:- fixedpoint:pi/2))
(e1:define fixedpoint:3pi/2
  (fixnum:* 3 fixedpoint:pi/2))
(e1:define fixedpoint:-3pi/2
  (fixnum:- fixedpoint:3pi/2))


;;;;; Fixed-point sine by quintic interpolation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Let x = t pi/2, to make sin(t) pass thru (0, 0) and (1, 1).  I manually
;;; interpolated at⁵ + bt³ + cx = 0 ; it's an odd function, like sine, passing
;;; thru (0, 0) and therefore not needing a zero-degree term.  I took t = 0 and
;;; t = 1 as nodes.  Notice that the derivative in t = 0 is pi/2, not 1.

(e1:define sine:a
  (fixnum:/ (fixedpoint:- fixedpoint:pi
                          (reader:string-in-simple-dot-notation->fixed-point "3.0"))
            2))
(e1:define sine:b
  (fixnum:/ (fixedpoint:- (reader:string-in-simple-dot-notation->fixed-point "5.0")
                          fixedpoint:2pi)
            2))
(e1:define sine:c
  fixedpoint:pi/2)

(e1:define (sine:sinet-in-0-1 t)
  (e1:let* ((t2 (fixedpoint:* t t))
            (t3 (fixedpoint:* t2 t))
            (t5 (fixedpoint:* t3 t2)))
    (fixnum:+ (fixedpoint:* sine:a t5)
              (fixedpoint:* sine:b t3)
              (fixedpoint:* sine:c t))))

;; It's faster to multiply by 2/pi than to divide by pi/2.
(e1:define fixedpoint:2/pi
  (fixedpoint:/ (reader:string-in-simple-dot-notation->fixed-point "2.0")
                fixedpoint:pi))
(e1:define (sine:sine-in-0-pi/2 x)
  (sine:sinet-in-0-1 (fixedpoint:* x fixedpoint:2/pi)))

(e1:define (sine:normalize-angle-in-0-2pi x-anywhere)
  (e1:let* ((x (fixnum:% x-anywhere fixedpoint:2pi)))
    (e1:if (fixnum:< x 0)
      ;; Unfortunately I currently can't rely on the sign of %.
      (sine:normalize-angle-in-0-2pi (fixedpoint:+ x fixedpoint:2pi))
      x)))

(e1:define (fixedpoint:sin x-anywhere)
  (e1:let* ((x (sine:normalize-angle-in-0-2pi x-anywhere)))
    ;; (fio:write "  ?" (f x-anywhere)
    ;;            " -> " (f x)
    ;;            ": range " (i (fixnum:/ x fixedpoint:pi/2))
    ;;            "\n")
    (e1:case ;; (fixnum:/ x fixedpoint:pi/2)
             (fixnum:/ (fixnum:* x 2) fixedpoint:pi) ;; more precise
      ((0)
       (sine:sine-in-0-pi/2 x))
      ((1)
       (sine:sine-in-0-pi/2 (fixnum:- fixedpoint:pi x)))
      ((2)
       (fixnum:- (sine:sine-in-0-pi/2 (fixnum:- x fixedpoint:pi))))
      ((3)
       (fixnum:- (sine:sine-in-0-pi/2 (fixnum:- fixedpoint:2pi x))))
      (else
       (fio:write "The interval index is "
                  (i (fixnum:/ x fixedpoint:pi/2))
                  "\n")
       (e1:error "fixedpoint:sin: unreachable")))))


;;;;; Fixed-point logarithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This silently returns wrong results if x < 1.0.
(e1:define (fixedpoint:log-of-at-least-1 x)
  (fixedpoint:log-loop x 1 fixedpoint:1 fixedpoint:0))

;;; Slower error-checking procedure.
(e1:define (fixedpoint:log x)
  (e1:cond ((fixedpoint:>= x fixedpoint:1)
            (fixedpoint:log-of-at-least-1 x))
           ((fixedpoint:> x fixedpoint:0)
            (fixedpoint:negate
                (fixedpoint:log-of-at-least-1 (fixedpoint:/ fixedpoint:1 x))))
           (else
            (e1:error "logarithm: argument <= 0"))))

;;; Shift-and-add algorithm.
(e1:define fixedpoint:log-steps
  fixedpoint:fractional-bit-no) ;; FIXME: is this always a good value?
(e1:define (fixedpoint:log-loop x k e acc)
  ;;(fio:write "k: " (i k) "; e: " (f e) "; acc: " (f acc) "\n")
  (e1:if (fixnum:>= k fixedpoint:log-steps)
    acc
    (e0:let (e acc) (fixedpoint:log-inner-loop x k e acc)
      (fixedpoint:log-loop x (fixnum:1+ k) e acc))))
(e1:define (fixedpoint:log-inner-loop x k e acc)
  (e1:let* ((u (fixedpoint:+ e (fixnum:logic-right-shift e k))))
    ;;(fio:write "k: " (i k) "; e: " (f e) "; acc: " (f acc) "; u: " (f u) "\n")
    (e1:if (fixedpoint:> u x)
      (e1:bundle e acc)
      (fixedpoint:log-inner-loop
          x
          k
          u
          (fixedpoint:+ acc (buffer:get fixedpoint-tables:logarithm-table k))))))

;;; Version with a generic base.
(e1:define (fixedpoint:log-with-base b x)
  (fixedpoint:/ (fixedpoint:log x)
                (fixedpoint:log b)))

;; FIXME: these could be computed in a different scale, so that they can be
;; multiplied with fixnum:*, which avoids a shift.
(e1:define fixedpoint:1/log2
  (fixedpoint:/ fixedpoint:1 (fixedpoint:log fixedpoint:2)))
(e1:define fixedpoint:1/log10
  (fixedpoint:/ fixedpoint:1 (fixedpoint:log fixedpoint:10)))

;;; Versions for common bases, optimized to only have a product instead
;;; of a logarithm and a division.
(e1:define (fixedpoint:log2 x)
  (fixedpoint:* (fixedpoint:log x)
                fixedpoint:1/log2))
(e1:define (fixedpoint:log10 x)
  (fixedpoint:* (fixedpoint:log x)
                fixedpoint:1/log10))

;;; Convenient variadic syntax.
(e1:define-macro (fixedpoint:log . args)
  (e1:case (sexpression:length args)
    ((1)
     `(e1:call fixedpoint:log ,@args))
    ((2)
     `(e1:call fixedpoint:log-with-base ,@args))
    (else
     `(e1:error "log: argument number not 1 or 2"))))


;;;;; Fixed-point exponential
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This silently returns incorrect results for x < 0.
(e1:define (fixedpoint:exp-of-at-least-0 x)
  (fixedpoint:exp-loop x 1 fixedpoint:0 fixedpoint:1))

;;; Slower more general version.
(e1:define (fixedpoint:exp x)
  (e1:if (fixedpoint:< x 0)
    (fixedpoint:/ fixedpoint:1
                  (fixedpoint:exp-of-at-least-0 (fixedpoint:negate x)))
    (fixedpoint:exp-of-at-least-0 x)))

;;; Another shift-and-add algorithm.
(e1:define fixedpoint:exp-steps
  fixedpoint:fractional-bit-no) ;; FIXME: is this always a good value?
(e1:define (fixedpoint:exp-loop x k t acc)
  (e1:if (fixnum:>= k fixedpoint:exp-steps)
    acc
    (e0:let (t acc) (fixedpoint:exp-inner-loop x k t acc)
      (fixedpoint:exp-loop x (fixnum:1+ k) t acc))))
(e1:define (fixedpoint:exp-inner-loop x k t acc)
  (e1:let* ((u (fixedpoint:+ t (buffer:get fixedpoint-tables:logarithm-table k))))
    (e1:if (fixedpoint:> u x)
      (e1:bundle t acc)
      (fixedpoint:exp-inner-loop
          x
          k
          u
          (fixedpoint:+ acc (fixnum:logic-right-shift acc k))))))

;;; Exponentiation with a generic base.
(e1:define (fixedpoint:pow b x)
  (fixedpoint:exp (fixedpoint:* (fixedpoint:log b) x)))

;;; Exponentiation with common bases, optimized to avoid the logarithm.
(e1:define fixedpoint:log2 (fixedpoint:log fixedpoint:2))
(e1:define fixedpoint:log10 (fixedpoint:log fixedpoint:10))
(e1:define (fixedpoint:pow2 x)
  (fixedpoint:exp (fixedpoint:* fixedpoint:log2 x)))
(e1:define (fixedpoint:pow10 x)
  (fixedpoint:exp (fixedpoint:* fixedpoint:log10 x)))

;;; Aliases.
(e1:define (fixedpoint:** b x)
  (fixedpoint:pow b x))

;;; Other aliases.  FIXME: do I really want these?
(e1:define (fixedpoint:exp2 x)
  (fixedpoint:pow2 x))
(e1:define (fixedpoint:exp10 x)
  (fixedpoint:pow10 x))

;;; It might be convenient to be able to call exp with two arguments, just
;;; like log.  The base comes first.
(e1:define-macro (fixedpoint:exp . args)
  (e1:case (sexpression:length args)
    ((1)
     `(e1:call fixedpoint:exp ,@args))
    ((2)
     `(e1:call fixedpoint:pow ,@args))
    (else
     `(e1:error "fixedpoint:exp: argument number not 1 or 2"))))


;;;;; Variadic versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
  (variadic:define-associative fixedpoint:+ fixedpoint:+ fixedpoint:0)
  (variadic:define-associative fixedpoint:* fixedpoint:* fixedpoint:1)
  (variadic:define-right-deep fixedpoint:** fixedpoint:** fixedpoint:1))
;;; FIXME: factor the logic of fixnum:- and fixnum:/ into a more general macro
;;; and use it here as well for fixedpoint:- and fixedpoint:/

;;; I don't even need to define min and max procedures for fixed-point numbers.
;;; Shall I do the same for + and -?
(e1:define-macro (fixedpoint:min . args)
  `(fixnum:min ,@args))
(e1:define-macro (fixedpoint:max . args)
  `(fixnum:max ,@args))


;;;;; Fixed-point average
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: there's another way avoiding the possible overflow of the
;;; intermediate result.
(e1:define (fixedpoint:average x y)
  (fixedpoint:half (fixedpoint:+ x y)))


;;;;; Fixed-point hyperbolic functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (fixedpoint:sinh x)
  (fixedpoint:half (fixedpoint:- (fixedpoint:exp x)
                                 (fixedpoint:exp (fixedpoint:negate x)))))

(e1:define (fixedpoint:cosh x)
  (fixedpoint:average (fixedpoint:exp x)
                      (fixedpoint:exp (fixedpoint:negate x))))

(e1:define (fixedpoint:tanh x)
  (e1:let* ((expx (fixedpoint:exp x))
            (exp-x (fixedpoint:exp (fixedpoint:negate x))))
    (fixedpoint:/ (fixedpoint:- expx exp-x)
                  (fixedpoint:+ expx exp-x))))


;;;;; Other easy fixed-point trascendental functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (fixedpoint:sqrt x)
  (fixnum:sqrt (fixnum:left-shift x fixedpoint:fractional-bit-no)))

(e1:define (fixedpoint:cos x)
  (fixedpoint:sin (fixnum:+ x fixedpoint:pi/2)))

(e1:define (fixedpoint:tan x)
  (fixedpoint:/ (fixedpoint:sin x)
                (fixedpoint:cos x)))
