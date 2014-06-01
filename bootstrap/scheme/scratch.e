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

(e1:define (fixnum:* a b) (fixnum:non-primitive-* a b))
(e1:define (fixnum:left-shift a b) (fixnum:non-primitive-left-shift a b))
(e1:define (fixnum:arithmetic-right-shift a b) (fixnum:non-primitive-arithmetic-right-shift a b))
(e1:define (fixnum:logic-right-shift a b) (fixnum:non-primitive-logic-right-shift a b))

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


;;;;; C64 compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define compiler:c64
  (compiler:metadata  #:procedure-closure
                      (e1:lambda (procedure-name target-file-name)
                        (compiler:c64-compile-to-assembly procedure-name target-file-name))
                      #:assembly-file-extension ".a"
                      #:object-file-extension ".o"
                      #:CCAS "acme"
                      #:ASFLAGS (string:append "-v9" ;; FIXME: remove this
                                               " --format cbm"
                                               " ")
                      #:CCLD configuration:CC
                      #:LDFLAGS (string:append "-L'" configuration:abs_top_builddir "/lib'"
                                               " "
                                               configuration:LDFLAGS)
                      #:LIBS (string:append configuration:LIBS
                                            " "
                                            "-lepsilondriver-native-untagged -lepsilonruntime-untagged -lepsilonutility")))

(e1:define (compiler:c64-compile-to-assembly main target-file-name)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file target-file-name io:write-mode)))
    (fio:write-to f ";;;;; This is machine-generated -*- asm -*- for ACME.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Built by the GNU epsilon compiler.
;;;;; http://www.gnu.org/software/epsilon


;;;;; Runtime library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Include assembly runtime library.
!source \"" (st configuration:abs_top_srcdir) "/runtime/backend-specific/c64/runtime.a\"\n")
    (compiler:c64-compile-data f data-graph)
    (fio:write-to f "

;;;;; Machine-language entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!zone machine_language_entry_point

;;; This is called from the machine-language driver, which is called from
;;; the BASIC driver.
epsilon_main_entry_point:
  +literal_to_16bit epsilon_end, return_address
  jmp ")
    (trivial-compiler:emit-symbol-identifier f main)
(fio:write-to f " ;; call main procedure
epsilon_end:
  ;; FIXME: this is test code
  +print_string string_slot0
  +print_stack 0
  ;; jsr print_return
  ;; +print_string string_slot1
  ;; +print_stack 1
  ;; jsr print_return
  rts
string_slot0:
  !pet \"first:  \", 0
;; string_slot1:
;;   !pet \"second: \", 0


;;;;; Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!zone compiled_procedures

")
    (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
      (compiler:c64-compile-procedure f procedure-name data-graph))
    (io:close-file f)))

;;; FIXME: factor, sharing code with trivial-compiler:c64-compile-data.
(e1:define (compiler:c64-compile-data f data-graph)
  (e1:let* ((hash (data-graph:graph-get-pointer-hash data-graph))
            (pointers (data-graph:graph-get-pointers data-graph))
            (symbol-hash (data-graph:graph-get-symbol-hash data-graph)))
    (fio:write-to f "

;;;;; Global data.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
!zone compiled_globals

;;; By convention we render a pointer to data whose content is not compiled
;;; as a pointer to this datum:
p0:
  !16 $ff

;;; Globals.
global_data_beginning:

")
    (e1:dolist (pointer pointers)
      (e1:let* ((index (unboxed-hash:get hash pointer))
                (is-symbol (unboxed-hash:has? symbol-hash pointer))
                (is-procedure-name (e1:and is-symbol (symbol:procedure-name? pointer))))
        (e1:unless (fixnum:zero? index)
          (fio:write-to f "p" (i index) ":")
          (e1:when is-symbol
            (fio:write-to f " ; the symbol " (sy pointer)))
          (fio:write-to f "\n")
          (fio:write-to f "  !16")
          (e1:dotimes (j (e0:primitive buffer:length pointer))
            (e1:unless (fixnum:zero? j)
              (io:write-string f ","))
            (io:write-string f " ")
            (e1:if (e1:and is-procedure-name (fixnum:= j 9)) ;; native code slot
              (trivial-compiler:emit-symbol-identifier f pointer)
              (e1:let* ((element (buffer:get pointer j)))
                (e1:if (e0:primitive whatever:atom? element)
                  (e1:cond ((fixnum:< element -32768)
                            (fio:write-to f "out_of_bounds_low"))
                           ((fixnum:> element 32767)
                            (fio:write-to f "out_of_bounds_high"))
                           (else
                            (fio:write-to f (i element))))
                  (e1:begin
                    (fio:write-to f "p")
                    (fio:write-to f (i (unboxed-hash:get hash element))))))))
          (fio:write-to f "\n"))))
    (fio:write-to f "
global_data_end:
  !16 0\n")))

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
    (fio:write-to f "  +print_string ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_in\n")
    (fio:write-to f
                  "  +to_stack_16bit return_address, "
                  (i (compiler:c64-return-stack-index procedure))
                  " ;; save return address on the stack\n")
    (compiler:c64-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (fio:write-to f ";;;;;;;;;;;;;;;; END
  ;;rts ; FIXME: is this needed?\n\n")))

(e1:define (compiler:c64-io->stack-index procedure io)
  io)
(e1:define (compiler:c64-return-stack-index procedure)
  (trivial-compiler:procedure-get-io-no procedure))
;; (e1:define (compiler:c64-frame-pointer-stack-index procedure)
;;   (fixnum:1+ (trivial-compiler:procedure-get-io-no procedure)))
(e1:define (compiler:c64-local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            local))
(e1:define (compiler:c64-scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))

;;; FIXME: factor?
(e1:define (compiler:c64-limit n)
  (e1:cond ((fixnum:< n -32768)
            -32768)
           ((fixnum:> n 32767)
            32767)
           (else
            n)))

;;; FIXME: factor, using this more often.
(e1:define (compiler:c64-emit-literal f n)
  (e1:cond ((fixnum:< n -32768)
            (fio:write-to f "out_of_bounds_low"))
           ((fixnum:> n 32767)
            (fio:write-to f "out_of_bounds_high"))
           (else
            (fio:write-to f (i n)))))

(e1:define (compiler:c64-emit-value f procedure value)
  (e1:let* ((data-graph (trivial-compiler:procedure-get-data-graph procedure))
            (pointer-hash (data-graph:graph-get-pointer-hash data-graph)))
    (e1:if (e0:primitive whatever:atom? value)
      (compiler:c64-emit-literal f value)
      (e1:let ((index (unboxed-hash:get pointer-hash value)))
        (fio:write-to f "p" (i index))))))

(e1:define (compiler:c64-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (e1:let ((procedure-name (trivial-compiler:procedure-get-name procedure)))
         (fio:write-to f "  +print_string ")
         (trivial-compiler:emit-symbol-identifier f procedure-name)
         (fio:write-to f "_out\n"))
       (fio:write-to f "  +jump_to_stack_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     " ;; return\n"))
      ((trivial-compiler:instruction-tail-call name)
       (fio:write-to f "  +print_string ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f "_tail_calling\n")
       (fio:write-to f "  +stack_to_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     ", return_address ;; copy return address\n")
       (fio:write-to f "  jmp ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f " ;; tail-call " (sy name) "\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (fio:write-to f "  ;; tail-call-indirect: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (e1:let ((return-label (trivial-compiler:fresh-label "return")))
         (fio:write-to f "  ;; nontail-call [tentative]: BEGIN\n")
         (fio:write-to f "  +print_string ")
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
         (fio:write-to f "  ;; nontail-call [tentative]: END\n")))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (fio:write-to f "  ;; nontail-call-indirect: FIXME: unimplemented\n"))
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
       (fio:write-to f "  +literal_to_stack_16bit "
                     (i (compiler:c64-limit value))
                     ", "
                     (i (compiler:c64-scratch->stack-index procedure scratch-index))
                     " ;; get-value\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (compiler:c64-compile-primitive f procedure name scratch-index))
      ((trivial-compiler:instruction-fork name scratch-index)
       (fio:write-to f "  ;; fork: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (fio:write-to f "  ;; join: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after_if")))
         (fio:write-to f "  ;; if-in: FIXME: begin\n")
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
         (fio:write-to f "  ;; if-in: FIXME: unimplemented\n")))
      (_
       (e1:error "impossible")))))

(e1:define (compiler:c64-emit-unary-primitive f name procedure scratch-index)
  (fio:write-to f "  +" (st name) " "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))
                ", "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))))

(e1:define (compiler:c64-emit-binary-primitive f name procedure scratch-index)
  (fio:write-to f "  +" (st name) " "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                ", "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))))

;;; A ternary primitive with no results.  FIXME: generalize?
(e1:define (compiler:c64-emit-store-primitive f name procedure scratch-index)
  (fio:write-to f "  +" (st name) " "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:+ scratch-index 2)))))

(e1:define (compiler:c64-compile-primitive f p name scratch-index)
  (e1:case name
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

    ((fixnum:1+)
     (fio:write-to f
                   "  +literal_to_stack_16bit 1, "
                   (i (compiler:c64-scratch->stack-index p (fixnum:1+ scratch-index)))
                   ";; [FIXME: use a designed scratch slot]\n")
     (compiler:c64-emit-binary-primitive f "sum_stack_16bit" p scratch-index))
    ((fixnum:1-)
     (fio:write-to f
                   "  +literal_to_stack_16bit 1, "
                   (i (compiler:c64-scratch->stack-index p (fixnum:1+ scratch-index)))
                   ";; [FIXME: use a designed scratch slot]\n")
     (compiler:c64-emit-binary-primitive f "subtract_stack_16bit" p scratch-index))
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
     (compiler:c64-emit-binary-primitive f "less_than_or_equal_stack_16bit" p scratch-index))

    ((fixnum:negate)
     (compiler:c64-emit-unary-primitive f "negate_stack_16bit" p scratch-index))
    ((fixnum:bitwise-not)
     (compiler:c64-emit-unary-primitive f "bitwise_not_stack_16bit" p scratch-index))

    ((buffer:get)
     (compiler:c64-emit-binary-primitive f "buffer_get_stack_16bit" p scratch-index))
    ((buffer:set! buffer:initialize!)
     ;; a ternary primitive with no results.  FIXME: generalize?
     (compiler:c64-emit-store-primitive f "buffer_set_stack_16bit" p scratch-index))

    (else
     (fio:write "About the primitive " (sy name) ":\n")
     (e1:error "unsupported primitive")))
  (fio:write-to f "  ;; primitive " (sy name) "\n"))


;;;;; Scratch convenience macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (test . stuff)
  (e1:cond ((sexpression:null? stuff)
            '(test 10))
           ((fixnum:= (sexpression:length stuff) 1)
            `(e1:call test ,@stuff))
           (else
            (e1:error "test arity"))))


;;;;; Scratch convenience syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (ca #;64:compile-to-assembly executable-file-name . forms)
  `(compiler:compile-to-assembly-with compiler:c64 ,executable-file-name ,@forms))
(e1:define-macro (c #;64:compile executable-file-name . forms)
  `(compiler:compile-with compiler:c64 ,executable-file-name ,@forms))

;;; Scratch version not using closures (hence not depending on memory primitives)
(e1:define-macro (can assembly-file-name . forms)
  `(e1:begin
     (e1:define (main)
       ,@forms)
     (compiler:compile-procedure-to-assembly-with compiler:c64
                                                  (e1:value main)
                                                  ,assembly-file-name)))


;; FIXME: compiled code apparently loops, in a complex way:
;; (can "/tmp/q.a" (fact 7))
;;
;; Notice that (can "/tmp/q.a" (fixnum:* 1 2 3 4 5 6 7)) works fine;
;; and so does (can "/tmp/q.a" (fixnum:* 7 6 5 4 3 2 1)).

