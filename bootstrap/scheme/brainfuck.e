;;;;; This is -*- epsilon -*- (with some Scheme).
;;;;; Brainfuck interpreter (to be partially evaluated)

;;;;; Copyright (C) 2014 Luca Saiu
;;;;; Written by Luca Saiu

;;;;; Copyright (C) 2013 Jérémie Koenig

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


;;;;; Brainfuck interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (bf:wrap n size)
  (e1:cond ((fixnum:< n 0)
            (bf:wrap (fixnum:+ n size) size))
           ((fixnum:>= n size)
            (bf:wrap (fixnum:- n size) size))
           (else
            n)))
(e1:define bf:cell-modulo 256)
(e1:define bf:tape-size 5);;1000);;30000) ;;30000)

(e1:define (bf:eval program)
  (bf:eval-with program
                 0
                 (string:length program)
                 (vector:make-initialized bf:tape-size 0)
                 0)
  ;; I don't want to be distracted by the return value of bf:eval-with.
  (e1:bundle))

;;; The result is the last value of t-index.
(e1:define (bf:eval-with p p-index p-limit t t-index)
  (e1:if (fixnum:= p-index p-limit)
    t-index
    (e1:let ((c (string:get p p-index)))
      (e1:case c
        ((#\< #\>)
         (e1:let ((delta (e1:case c
                           ((#\<) -1)
                           (else +1))))
           (bf:eval-with p (fixnum:1+ p-index) p-limit t (fixnum:+ t-index delta))))
        ((#\- #\+)
         (e1:let ((delta (e1:case c
                           ((#\-) -1)
                           (else +1))))
           (vector:set! t t-index (bf:wrap (fixnum:+ (vector:get t t-index) delta)
                                           bf:cell-modulo)))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((#\.)
         ;;(fio:write "Printing " (C (bf:wrap (vector:get t t-index) 256)) "\n")
         (fio:write (c (bf:wrap (vector:get t t-index) 256)))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((44) ;; #\,
         (e1:let ((c (character:read)))
           (e1:if (io:eof-object? c)
             (vector:set! t t-index 0))
             (vector:set! t t-index c))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((#\[)
         (e1:let* ((next-p-index (fixnum:1+ p-index))
                   (loop-end-index (bf:loop-end p next-p-index))
                   (new-t-index
                    (bf:eval-loop p next-p-index loop-end-index t t-index)))
           (bf:eval-with p (fixnum:1+ loop-end-index) p-limit t new-t-index)))
        ((#\])
         (e1:error "this shouldn't happen"))
        (else
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))))))

(e1:define (bf:loop-end p i)
  ;;(fio:write "Matching the [ at " (i (fixnum:1- i)) "...\n")
  (bf:find-matching-closed-bracket p i 0))
(e1:define (bf:find-matching-closed-bracket p i open-bracket-no)
  (e1:if (fixnum:>= i (string:length p))
    (e1:error "no matching ] for [")
    (e1:case (string:get p i)
      ((#\[)
       (bf:find-matching-closed-bracket p (fixnum:1+ i) (fixnum:1+ open-bracket-no)))
      ((#\])
       (e1:if (fixnum:zero? open-bracket-no)
         (e1:begin
           ;;(fio:write "The ] at " (i i) ".\n")
           i)
         (bf:find-matching-closed-bracket p (fixnum:1+ i) (fixnum:1- open-bracket-no))))
      (else
       (bf:find-matching-closed-bracket p (fixnum:1+ i) open-bracket-no)))))
(e1:define (bf:eval-loop p p-index p-limit t t-index)
  (e1:if (vector:get t t-index)
    (e1:let ((new-t-index (bf:eval-with p p-index p-limit t t-index)))
      ;;(fio:write "AGAIN (" (i (string:get t new-t-index)) ")\n")
      (bf:eval-loop p p-index p-limit t new-t-index))
    t-index))
