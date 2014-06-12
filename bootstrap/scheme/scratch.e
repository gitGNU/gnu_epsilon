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


;;;;; FIXME: move this back to compiler.e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (compiler:c64-compile-procedure f procedure-name data-graph)
  (e1:let* ((formals (state:procedure-get-formals procedure-name))
            (body (state:procedure-get-body procedure-name))
            (procedure (trivial-compiler:compile-procedure procedure-name formals body data-graph)))
    (fio:write-to f ";;;;; " (sy procedure-name) "\n")
    (fio:write-to f ";;; io-no is "
                  (i (trivial-compiler:procedure-get-io-no procedure)) "
;;; leaf is "
                  (s (e1:if (trivial-compiler:procedure-get-leaf procedure) "#t" "#f")) "
;;; local-no is "
                  (i (trivial-compiler:procedure-get-local-no procedure)) "
;;; scratch-no is "
                  (i (trivial-compiler:procedure-get-scratch-no procedure))
                  "\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_in:\n  !pet \"" (sy procedure-name) ": begin\", 0\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_calling:\n  !pet \"  c " (sy procedure-name) "\", 0\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_tail_calling:\n  !pet \"  t-c " (sy procedure-name) "\", 0\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_out:\n  !pet \"" (sy procedure-name) ": return\", 0\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f ":\n")
    (fio:write-to f ";;;;;;;;;;;;;;;; BEGIN\n")
    (fio:write-to f ";  +print_string ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_in\n")
    (fio:write-to f
                  "  +absolute_to_stack_16bit return_address, "
                  (i (compiler:c64-return-stack-index procedure))
                  " ;; save return address on the stack\n")
    (compiler:c64-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (fio:write-to f ";;;;;;;;;;;;;;;; END
  ;;rts ; FIXME: is this needed?\n\n")))

(e1:define (compiler:c64-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (e1:let ((procedure-name (trivial-compiler:procedure-get-name procedure)))
         (fio:write-to f ";  +print_string ")
         (trivial-compiler:emit-symbol-identifier f procedure-name)
         (fio:write-to f "_out\n"))
       (fio:write-to f "  +jump_to_stack_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     " ;; return\n"))
      ((trivial-compiler:instruction-tail-call name)
       (fio:write-to f ";  +print_string ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f "_tail_calling\n")
       (fio:write-to f "  +stack_to_absolute_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     ", return_address ;; copy return address\n")
       (fio:write-to f "  jmp ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f " ;; tail-call " (sy name) "\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (fio:write-to f "  +stack_to_absolute_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     ", return_address ;; copy return address\n")
       (fio:write-to f "  +jump_indirect_stack_16bit "
                     (i (compiler:c64-local->stack-index procedure local-index))
                     " ;; tail-call-indirect\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (e1:let ((return-label (trivial-compiler:fresh-label "return")))
         (fio:write-to f "  ;; nontail-call: begin\n")
         (fio:write-to f "  ;+print_string ")
         (trivial-compiler:emit-symbol-identifier f name)
         (fio:write-to f "_calling\n")
         (fio:write-to f "  +literal_to_16bit "
                       (st return-label)
                       ", return_address ;; pass return address\n")
         (fio:write-to f "  +adjust_frame_pointer_16bit "
                       (i (compiler:c64-scratch->stack-index procedure scratch-index))
                       " ;; advance frame pointer\n")
         (fio:write-to f "  jmp ")
         (trivial-compiler:emit-symbol-identifier f name)
         (fio:write-to f " ;; non-tail call " (sy name) "\n")
         (fio:write-to f (st return-label) ":\n")
         (fio:write-to f "  +adjust_frame_pointer_16bit -"
                       (i (compiler:c64-scratch->stack-index procedure scratch-index))
                       " ;; reset frame pointer\n")
         (fio:write-to f "  ;; nontail-call: end\n")))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       ;; BBBBBBBBBBBBBBBBBBBBBBBBBB
       (e1:let ((return-label (trivial-compiler:fresh-label "return")))
         (fio:write-to f "  ;; nontail-call-indirect: begin\n")
         (fio:write-to f "  +literal_to_16bit "
                       (st return-label)
                       ", return_address ;; copy return address\n")
         (fio:write-to f "  +symbol_native_code_to_absolute_stack_16bit "
                       (i (compiler:c64-local->stack-index procedure local-index))
                       ", zeropage_arg2 ;; save nontail indirect call destination\n")
         (fio:write-to f "  +adjust_frame_pointer_16bit "
                       (i (compiler:c64-scratch->stack-index procedure scratch-index))
                       " ;; advance frame pointer\n")
         (fio:write-to f "  jmp (zeropage_arg2) ; nontail indirect call\n")
         (fio:write-to f (st return-label) ":\n")
         (fio:write-to f "  +adjust_frame_pointer_16bit -"
                       (i (compiler:c64-scratch->stack-index procedure scratch-index))
                       " ;; reset frame pointer\n")
         (fio:write-to f "  ;; nontail-call-indirect: end\n")))
       ;; EEEEEEEEEEEEEEEEEEEEEEEEEE
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (fio:write-to f "  +stack_to_stack_16bit "
                     (i (compiler:c64-io->stack-index procedure io-index))
                     ", "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     " ;; get-io\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (fio:write-to f "  +stack_to_stack_16bit "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     ", "
                     (i (compiler:c64-io->stack-index procedure io-index))
                     " ;; set-io\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (fio:write-to f "  +stack_to_stack_16bit "
                     (i (compiler:c64-local->stack-index procedure local-index))
                     ", "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     " ;; get-local\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (fio:write-to f "  +stack_to_stack_16bit "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     ", "
                     (i (compiler:c64-local->stack-index procedure local-index))
                     " ;; set-local\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       (e1:let ((global-value (buffer:get global-name 2))) ;; third symbol slot
         (fio:write-to f "  +literal_to_stack_16bit ")
         (compiler:c64-emit-value f procedure global-value)
         (fio:write-to f
                       ", "
                       (i (compiler:c64-scratch->stack-index procedure scratch-index))
                       " ;; get-global " (sy global-name) "\n")))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (fio:write-to f "  +literal_to_stack_16bit ")
       (compiler:c64-emit-value f procedure value)
       (fio:write-to f
                     ", "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     " ;; get-value\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (compiler:c64-compile-primitive f procedure name scratch-index))
      ((trivial-compiler:instruction-fork name scratch-index)
       (e1:error "fork: unimplemented\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (e1:error "join: unimplemented\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after_if")))
         (fio:write-to f "  ;; if-in: begin\n")
         (e1:dolist (v values)
           (e1:if (fixnum:zero? v)
             ;; We can be slightly more efficient in this (common) case.
             (fio:write-to f "  +jump_when_zero_stack_16bit "
                           (i (compiler:c64-scratch->stack-index procedure scratch-index))
                           ", " (st then-label) "\n")
             (e1:begin ;; nonzero v
               (fio:write-to f "  +literal_to_stack_16bit ")
               (compiler:c64-emit-value f procedure v)
               (fio:write-to f ", "
                             (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                             " ;; candidate discriminator [FIXME: use a designed scratch slot or, better, write a jump_when_equal_stack_immediate_16bit macro]\n")
               (fio:write-to f "  +equal_stack_16bit "
                             (i (compiler:c64-scratch->stack-index procedure scratch-index))
                             ", "
                             (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                             ", "
                             (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                             " ;; compare\n")
               (fio:write-to f "  +jump_unless_zero_stack_8bit "
                             (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                             ", " (st then-label) "\n"))))
         (fio:write-to f ";; else branch\n")
         (compiler:c64-compile-instructions f procedure else-instructions)
         (fio:write-to f "  jmp " (st after-label) "\n")
         (fio:write-to f (st then-label) ":\n")
         (compiler:c64-compile-instructions f procedure then-instructions)
         (fio:write-to f (st after-label) ":\n")
         (fio:write-to f "  ;; if-in: end\n")))
      (_
       (e1:error "impossible")))))

(e1:define (compiler:c64-compile-primitive f p name scratch-index)
  (e1:case name
    ((debug:fail)
     (fio:write-to f "  +debug_fail"))

    ((fixnum:+)
     (compiler:c64-emit-binary-primitive f "sum_stack_16bit" p scratch-index))
    ((fixnum:-)
     (compiler:c64-emit-binary-primitive f "subtract_stack_16bit" p scratch-index))
    ((fixnum:bitwise-and)
     (compiler:c64-emit-binary-primitive f "bitwise_and_stack_16bit" p scratch-index))
    ((fixnum:bitwise-or)
     (compiler:c64-emit-binary-primitive f "bitwise_or_stack_16bit" p scratch-index))
    ((fixnum:bitwise-xor)
     (compiler:c64-emit-binary-primitive f "bitwise_xor_stack_16bit" p scratch-index))

    ((whatever:zero?)
     (fio:write-to f
                   "  +literal_to_stack_16bit 0, "
                   (i (compiler:c64-scratch->stack-index p (fixnum:1+ scratch-index)))
                   ";; [FIXME: use a designed scratch slot]\n")
     (compiler:c64-emit-binary-primitive f "equal_stack_16bit" p scratch-index))
    ((fixnum:1+)
     (compiler:c64-emit-unary-primitive f "increment_stack_16bit" p scratch-index))
    ((fixnum:1-)
     (compiler:c64-emit-unary-primitive f "decrement_stack_16bit" p scratch-index))
    ((fixnum:left-shift-1-bit)
     (compiler:c64-emit-unary-primitive f "left_shift_1_stack_16bit" p scratch-index))
    ((fixnum:arithmetic-right-shift-1-bit)
     (compiler:c64-emit-unary-primitive f "arithmetic_right_shift_1_stack_16bit" p scratch-index))
    ((fixnum:logic-right-shift-1-bit)
     (compiler:c64-emit-unary-primitive f "logic_right_shift_1_stack_16bit" p scratch-index))

    ((whatever:eq?)
     (compiler:c64-emit-binary-primitive f "equal_stack_16bit" p scratch-index))
    ((fixnum:<)
     (compiler:c64-emit-binary-primitive f "less_than_stack_16bit" p scratch-index))
    ((fixnum:<=)
     (compiler:c64-emit-binary-primitive f "less_than_or_equal_to_stack_16bit" p scratch-index))

    ((fixnum:negate)
     (compiler:c64-emit-unary-primitive f "negate_stack_16bit" p scratch-index))
    ((fixnum:bitwise-not)
     (compiler:c64-emit-unary-primitive f "bitwise_not_stack_16bit" p scratch-index))

    ((buffer:make)
     (compiler:c64-emit-unary-primitive f "buffer_make_stack_16bit" p scratch-index))
    ((buffer:get)
     (compiler:c64-emit-binary-primitive f "buffer_get_stack_16bit" p scratch-index))
    ((buffer:set! buffer:initialize!)
     ;; a ternary primitive with no results.  FIXME: generalize?
     (compiler:c64-emit-ternary-no-result-primitive f "buffer_set_stack_16bit" p scratch-index))

    ((io:load-byte)
     (compiler:c64-emit-unary-primitive f "io_load_byte_8bit" p scratch-index))
    ((io:store-byte!)
     (compiler:c64-emit-binary-no-result-primitive f "io_store_byte_8bit" p scratch-index))

    ((io:standard-output)
     (fio:write-to f "  ;; Return an undefined value for io:standard-output\n"))
    ((io:write-character) ;; FIXME: no file support yet: ignore the first parameter
     (fio:write-to f "  +stack_to_a_8bit "
                   (i (compiler:c64-scratch->stack-index p (fixnum:1+ scratch-index)))
                   "\n")
     (fio:write-to f "  jsr io_write_ASCII_character"))
    (else
     (fio:write "About the primitive " (sy name) ":\n")
     (e1:error "unsupported primitive")))
  (fio:write-to f " ;; primitive " (sy name) "\n"))


;;;;; What follows is tentative code: Commodore 64 tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (list:sum xs)
  (list:sum-acc xs 0))
(e1:define (list:sum-acc xs a)
  (e1:if (list:null? xs)
    a
    (list:sum-acc (list:tail xs)
                  (fixnum:+ (list:head xs) a))))

;; (e1:define (fixnum:absolute-value n)
;;   (e1:if (fixnum:< n 0)
;;     (fixnum:negate n)
;;     n))

;; (e1:define fixnum:random-seed
;;   (box:make 1234))
;; (e1:define (fixnum:random)
;;   (e1:let* ((old-seed (box:get fixnum:random-seed))
;;             (old-seed old-seed)
;;             (new-seed (e0:if-in old-seed (0)
;;                         1
;;                         (fixnum:non-primitive-% (fixnum:+ old-seed
;;                                                           (fixnum:non-primitive-* old-seed
;;                                                                                   213))
;;                                                 32323)))
;;             (new-seed (fixnum:absolute-value new-seed)))
;;     (box:set! fixnum:random-seed new-seed)
;;     new-seed))

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

;; n * 40 = n * 32 + n * 8 = n * 8 + n * 8 * 4
(e1:define (fixnum:40* n)
  (e1:let* ((8n (fixnum:8* n))
            (32n (fixnum:4* 8n)))
    (fixnum:+ 32n 8n)))

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
  (e1:let* ((row (fixnum:8/ y))
            (column (fixnum:8/ x))
            (line (fixnum:bitwise-and y 7))
            (bit (fixnum:- 7 (fixnum:bitwise-and x 7)))
            (byte (fixnum:+ 24576
                            (fixnum:320* row 320)
                            (fixnum:8* column 8)
                            line))
            (cbyte (fixnum:+ 17408
                             (fixnum:40* row 40)
                             col)))
    ;; (io:store-byte! byte
    ;;                 (fixnum:bitwise-or (io:load-byte byte)
    ;;                                    (fixnum:non-primitive-left-shift 1 bit)))
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
  (e1:let* ((by (fixnum:+ base
                          (fixnum:320* (fixnum:8/ y))
                          (fixnum:8* (fixnum:8/ x))
                          (fixnum:bitwise-and y 7)))
            (bi (fixnum:bitwise-and x 7)))
    (io:or! by (fixnum:lookup-bit-value bi))))

(e1:define (plot-line x y)
  (e1:when (e1:and (fixnum:< x 320)
                   (fixnum:< y 200))
    (vic2:plot-pixel x y)
    (plot-line (fixnum:+ x 1) (fixnum:+ y 1))))

;; (e1:define (plot-random x)
;;   (e1:when (e1:and (fixnum:< x 320))
;;     (vic2:plot-pixel x (fixnum:non-primitive-% (fixnum:random) 200))
;;     (plot-random (fixnum:+ x 1))))

(e1:define (plot-everything x)
  (e1:when (fixnum:< x 320)
    (plot-column x 0)
    (plot-everything (fixnum:+ x 1))))
(e1:define (plot-column x y)
  (e1:when (fixnum:- y 200)
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
  '(c (go)))
(e1:toplevel
(e1:when #f
  (c (stripes))
  (c (test-sprites))
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

;; (c (e1:dolist (s (list:list "foo" "bar")) (fio:write s "\n")))
;; (e1:toplevel (g))

;; (c (list:fold (e1:lambda (a b) (fixnum:+ a b)) (list:iota 5)))
