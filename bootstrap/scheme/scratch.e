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


;;;;; What follows is tentative code: Commodore 64 tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (fixnum:absolute-value n)
  (e1:if (fixnum:< n 0)
    (fixnum:negate n)
    n))

(e1:define fixnum:random-seed
  (box:make 1234))
(e1:define (fixnum:random)
  (e1:let* ((old-seed (box:get fixnum:random-seed))
            (old-seed old-seed)
            (new-seed (e0:if-in old-seed (0)
                        1
                        (fixnum:non-primitive-% (fixnum:+ old-seed
                                                          (fixnum:non-primitive-* old-seed
                                                                                  213))
                                                32323)))
            (new-seed (fixnum:absolute-value new-seed)))
    (box:set! fixnum:random-seed new-seed)
    new-seed))

(e1:define (fixnum:2* n)
  (e1:primitive fixnum:left-shift-1-bit n))
(e1:define (fixnum:4* n)
  (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit n)))
(e1:define (fixnum:8* n)
  (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit n))))
(e1:define (fixnum:16* n)
  (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit n)))))
(e1:define (fixnum:32* n)
  (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit n))))))
(e1:define (fixnum:64* n)
  (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit (e1:primitive fixnum:left-shift-1-bit n)))))))

;; n * 320 = n * 256 + n * 64 = n * 64 + n * 64 * 4
(e1:define (fixnum:320* n)
  (e1:let* ((64n (fixnum:64* n))
            (256n (fixnum:4* 64n)))
    (fixnum:+ 64n 256n)))

(e1:define (fixnum:2/ n)
  (e1:primitive fixnum:arithmetic-right-shift-1-bit n))
(e1:define (fixnum:4/ n)
  (e1:primitive fixnum:arithmetic-right-shift-1-bit (e1:primitive fixnum:arithmetic-right-shift-1-bit n)))
(e1:define (fixnum:8/ n)
  (e1:primitive fixnum:arithmetic-right-shift-1-bit (e1:primitive fixnum:arithmetic-right-shift-1-bit (e1:primitive fixnum:arithmetic-right-shift-1-bit n))))

;; An efficient way of left-shifting 1.
(e1:define fixnum:left-shifts
  (tuple:make 1 2 4 8 16 32 64 128))
(e1:define (fixnum:left-shift-1 n)
  (e1:primitive buffer:get fixnum:left-shifts n))

(e1:define (io:or! address byte)
  (io:store-byte! address
                  (fixnum:bitwise-or (io:load-byte address) byte)))
(e1:define (io:and! address byte)
  (io:store-byte! address
                  (fixnum:bitwise-and (io:load-byte address) byte)))

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

;; An 8 fixnum array storing (1 << i) at index i.  Useful for setting
;; sprite coordinate high bits.
(e1:define sprite-bit-array
  fixnum:left-shifts)
;; A way to avoid negating the previous array elements at run time.
(e1:define sprite-not-bit-array
  (tuple:make (fixnum:bitwise-not 1) (fixnum:bitwise-not 2)
              (fixnum:bitwise-not 4) (fixnum:bitwise-not 8)
              (fixnum:bitwise-not 16) (fixnum:bitwise-not 32)
              (fixnum:bitwise-not 64) (fixnum:bitwise-not 128)))
(e1:define (move-sprite! n x y)
  (e1:let* ((2n (fixnum:double n))
            (x-address (fixnum:+ v 2n))
            (x-255 (fixnum:- x 255))
            (y-address (fixnum:1+ x-address))
            (old-high-x-bits (io:load-byte 53264)))
    (e1:if (fixnum:< x-255 0)
      (e1:begin
        (io:store-byte! 53264
                        (fixnum:bitwise-and old-high-x-bits
                                            (buffer:get sprite-not-bit-array n)))
        (io:store-byte! x-address x))
      (e1:begin
        (io:store-byte! 53264
                        (fixnum:bitwise-or old-high-x-bits
                                           (buffer:get sprite-bit-array n)))
        (io:store-byte! x-address x-255)))
    (io:store-byte! y-address y)))

(e1:define (joystick-address j)
  (fixnum:+ 56320 j))

(e1:define (joystick-up? j)
  (e1:not (fixnum:bitwise-and (io:load-byte (joystick-address j)) 1)))
(e1:define (joystick-down? j)
  (e1:not (fixnum:bitwise-and (io:load-byte (joystick-address j)) 2)))
(e1:define (joystick-left? j)
  (e1:not (fixnum:bitwise-and (io:load-byte (joystick-address j)) 4)))
(e1:define (joystick-right? j)
  (e1:not (fixnum:bitwise-and (io:load-byte (joystick-address j)) 8)))
(e1:define (joystick-fire? j)
  (e1:not (fixnum:bitwise-and (io:load-byte (joystick-address j)) 16)))

(e1:define (make-sprites)
  (set-sprite-configuration! 0 0)
  (move-sprite! 0 100 100)
  (show-sprite! 0)
  (move-sprite! 2 200 150)
  (show-sprite! 2)
  (move-sprite! 1 255 200)
  (show-sprite! 1))

(e1:define (test-sprites)
  (make-sprites)
  (animate-sprite 0 100 100)
  )
(e1:define x-min 10)
(e1:define x-max 245);;120)
(e1:define joystick 1)
(e1:define (animate-sprite n x y)
  (move-sprite! n x y)
  (e1:let ((dx (e1:cond ((joystick-left? joystick)  -1)
                        ((joystick-right? joystick) 1)
                        (else                       0)))
           (dy (e1:cond ((joystick-up? joystick)    -1)
                        ((joystick-down? joystick)  1)
                        (else                       0))))
    (e1:when (joystick-fire? joystick)
      (fio:write "f"))
    (animate-sprite n (fixnum:+ x dx) (fixnum:+ y dy))))

(e1:define base
  (fixnum:double 4096))

(e1:define (disable-high-resolution-mode)
  (io:store-byte! 53265 27)
  (io:store-byte! 53272 21)
  (io:store-byte! 56576 199))
(e1:define (enable-high-resolution-mode)
  (io:store-byte! 53265 59)
  (io:store-byte! 53272 29)
  (io:store-byte! 56576 198)
  ;; ;; ;; Put the bit map at base.
  ;; ;; (io:store-byte! 53272 (fixnum:bitwise-or (io:load-byte 53272) 8))
  ;; ;; Enter bit map mode.
  ;; (io:store-byte! 53265 (fixnum:bitwise-or (io:load-byte 53265) 32))
  ;; ;; Clear the bit map.
  ;; (io:fill-from-to! 24576 32575);base 7999)
  ;; ;; Set color to cyan and black.
  ;; (io:fill-from-to! 1024 (fixnum:- 2023 1024 -1) 3)
  )
(e1:define (io:fill-from-to! from how-many byte)
  (e1:when how-many
    (io:store-byte! from byte)
    (io:fill-from-to! (fixnum:1+ from) (fixnum:1- how-many) byte)))

(e1:define (plot x y c)
  (e1:let* ((row (fixnum:non-primitive-/ y 8))
            (column (fixnum:non-primitive-/ x 8))
            (line (fixnum:bitwise-and y 7))
            (bit (fixnum:- 7 (fixnum:bitwise-and x 7)))
            (byte (fixnum:+ 24576
                            (fixnum:non-primitive-* row 320)
                            (fixnum:non-primitive-* column 8)
                            line))
            (cbyte (fixnum:+ 17408
                             (fixnum:non-primitive-* row 40)
                             col)))
    #;(io:store-byte! byte
                    (fixnum:bitwise-or (io:load-byte byte)
                                       (fixnum:non-primitive-left-shift 1 bit)))
    (io:store-byte! cbyte c)
    ))

(e1:define vic2:color-memory 55296)
(e1:define (vic2:set-screen-memory address)
  ;; FIXME: shall I add the bank address to 648?
  ;;; FIXME: I should also poke 53272 to choose where screen memory is:
  ;; POKE53272,(PEEK(53272)AND15)OR A
  ;; POKE 53272,(PEEK(53272)AND240)OR A   REM for character memory
  ;; The following is only for the kernal's screen editor
  (io:store-byte! 648 (fixnum:non-primitive-/ address 64)))
(e1:define (vic2:set-bank b)
  ;;POKE 56578,PEEK(56578)OR 3: REM MAKE SURE BITS 0 AND 1 ARE OUTPUTS
  ;;POKE 56576,(PEEK(56576)AND 252)OR A: REM CHANGE BANKS
  (io:or! 56578 3)
  (io:or! 56576 (fixnum:- 3 b)))

(e1:define (vic2:set-bit-map)
  (io:or! 53265 32))
(e1:define (vic2:unset-bit-map)
  (io:and! 53265 223))

;; Lookup the value for a given pixel within a byte, indexed from the left.
(e1:define fixnum:lookup-bit-value-values
  (tuple:make 128 64 32 16 8 4 2 1))
(e1:define (fixnum:lookup-bit-value n)
  (e1:primitive buffer:get fixnum:lookup-bit-value-values n))

(e1:define (vic2:plot-pixel x y)
  ;; 70 CH=INT(X/8)
  ;; 80 RO=INT(Y/8)
  ;; 85 LN=YAND7
  ;; 90 BY=BASE+RO*320+8*CH+LN
  ;; 100 BI=7-(XAND7)
  ;; 110 POKEBY,PEEK(BY)OR(2^BI)
  (e1:let* ((ch (fixnum:8/ x))
            (ro (fixnum:8/ y))
            (ln (fixnum:bitwise-and y 7))
            (by (fixnum:+ base
                          (fixnum:320* ro)
                          (fixnum:8* ch)
                          ln))
            (bi (fixnum:bitwise-and x 7)))
    (io:or! by (fixnum:lookup-bit-value bi))))

(e1:define (plot-line x y)
  (e1:when (e1:and (fixnum:< x 320)
                   (fixnum:< y 200))
    (vic2:plot-pixel x y)
    (plot-line (fixnum:+ x 1) (fixnum:+ y 1))))

(e1:define (plot-random x)
  (e1:when (e1:and (fixnum:< x 320))
    (vic2:plot-pixel x (fixnum:non-primitive-% (fixnum:random) 200))
    (plot-random (fixnum:+ x 1))))

(e1:define (plot-everything x)
  (e1:when (fixnum:< x 320)
    (plot-column x 0)
    (plot-everything (fixnum:+ x 1))))
(e1:define (plot-column x y)
  (e1:when (fixnum:< y 200)
    (vic2:plot-pixel x y)
    (plot-column x (fixnum:1+ y))))
(e1:define (go)
  ;;(make-sprites)
  ;; (enable-high-resolution-mode)
  ;; (io:fill-from-to! 17408 1000 94)
  ;; (io:fill-from-to! 24576 8000 0)
  ;; ;; "lighthouse"
  ;; (io:store-byte! 18090 0)
  ;; (io:store-byte! 18130 17)
  ;; (io:store-byte! 18170 17)
  ;; (io:store-byte! 18210 17)
  ;;(plot 10 20 10)
  ;(plot 10 21 10)
  ;; (plot 11 30 5)
  ;; (plot 12 40 4)
  ;; (plot 13 50 3)
  ;; (plot 14 70 2)

  ;;(disable-high-resolution-mode)
  ;;POKE 53270,PEEK(53270)OR 16
  ;;(io:or! 53270 16)
  ;;POKE53272,PEEK(53272)OR8:REM PUT BIT MAP AT 8192
  (io:or! 53272 8) ;; put the bit map at 8192
  (vic2:set-bit-map)
  ;;(io:fill-from-to! color-memory 500 1)
  (io:fill-from-to! base 8000 0)
  ;;(vic2:unset-bit-map)
  (io:fill-from-to! 1024 1024 3)
  ;;(vic2:set-bit-map)
  (vic2:plot-pixel 100 100)
  (vic2:plot-pixel 100 110)
  (vic2:plot-pixel 100 120)
  (vic2:plot-pixel 100 130)
  (vic2:plot-pixel 100 150)
  ;;(plot-line 0 0)
  (plot-everything 0)
  ;;(vic2:unset-bit-map)
  ;;(loop)
  )

(e1:define (loop) (loop))

(e1:define-macro (g)
  '(can "/tmp/q.a" (go)))
(e1:toplevel
(e1:when #f
  (can "/tmp/q.a" (stripes))
  (can "/tmp/q.a" (test-sprites))
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

;; (can "/tmp/q.a" (e1:dolist (s (list:list "foo" "bar")) (fio:write s "\n")))
(e1:toplevel (g))
