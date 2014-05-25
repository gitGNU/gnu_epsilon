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
  jmp ")
    (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f " ; tail-call the main procedure " (sy main) "


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

;;;;; Special global definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conventional values for constants not fitting in 16 bits.
;;; If we start really using them we're probably in trouble.
out_of_bounds_low = -32768
out_of_bounds_high = 32767


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
    (fio:write-to f ":\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (fio:write-to f "_TAIL:\n")
    (fio:write-to f ";;;;;;;;;;;;;;;; BEGIN\n")
    (compiler:c64-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (fio:write-to f ";;;;;;;;;;;;;;;; END
  rts ; FIXME: is this needed?\n\n")))

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
(e1:define (compiler:c64-emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 4))
  (io:write-string f "($16)"))

(e1:define (compiler:c64-emit-load-value-to-$2 f procedure value)
  (e1:if (e0:primitive whatever:atom? value)
    (e1:begin
      (io:write-string f "  li $2, ")
      (io:write-fixnum f value))
    (e1:let* ((data-graph (trivial-compiler:procedure-get-data-graph procedure))
              (pointer-hash (data-graph:graph-get-pointer-hash data-graph))
              (symbol-hash (data-graph:graph-get-symbol-hash data-graph))
              (index (unboxed-hash:get pointer-hash value)))
      (io:write-string f "  la $2, p")
      (io:write-fixnum f index)
      (e1:when (unboxed-hash:has? symbol-hash value)
        (io:write-string f " ; ")
        (io:write-symbol f value))))
  (io:write-string f "\n"))

(e1:define (compiler:c64-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (io:write-string f "  ; return: BEGIN\n")
       (io:write-string f "  lw $31, ")
       (compiler:c64-emit-stack-access f (compiler:c64-return-stack-index procedure))
       (io:write-string f "\n")
       (io:write-string f "  jr $31 ; return\n")
       (io:write-string f "  nop ; empty delay slot\n")
       (io:write-string f "  ; return: END\n"))
      ((trivial-compiler:instruction-tail-call name)
       (io:write-string f "  ; tail-call: BEGIN\n")
       (io:write-string f "  la $2, ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f " ; load destination (may be large)\n")
       (io:write-string f "  jr $2 ; jump\n")
       (io:write-string f "  lw $31, ")
       (compiler:c64-emit-stack-access f (compiler:c64-return-stack-index procedure))
       (io:write-string f " ; pass the return address via $31 (delay slot)\n")
       (io:write-string f "  ; tail-call: END\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (io:write-string f "  ; tail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (compiler:c64-emit-stack-access f (compiler:c64-local->stack-index procedure local-index))
       (io:write-string f " ; load code address\n")
       (io:write-string f "  lw $25, 9*4($25) ; Load native code address from symbol\n")
       (io:write-string f "  jr $25\n")
       (io:write-string f "  lw $31, ")
       (compiler:c64-emit-stack-access f (compiler:c64-return-stack-index procedure))
       (io:write-string f " ; delay slot: pass the return address via $31\n")
       (io:write-string f "  ; tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (io:write-string f "  ; nontail-call: BEGIN\n")
       (io:write-string f "  la $2, ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "\n")
       (io:write-string f "  jalr $2\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:c64-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " ; delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (compiler:c64-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " ; restore frame pointer\n")
       (io:write-string f "  ; nontail-call: END\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (io:write-string f "  ; nontail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (compiler:c64-emit-stack-access f (compiler:c64-local->stack-index procedure local-index))
       (io:write-string f " ; load symbol address\n")
       (io:write-string f "  lw $25, 9*4($25) ; Load native code address from symbol\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:c64-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " ; delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (compiler:c64-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " ; restore frame pointer\n")
       (io:write-string f "  ; nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (io:write-string f "  ; get-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  ; get-io: END\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (io:write-string f "  ; set-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  ; set-io: END\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (io:write-string f "  ; get-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  ; get-local: END\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (io:write-string f "  ; set-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  ; set-local: END\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       ;;; FIXME: this is correct, but I could generate better code by exploiting global immutability
       (io:write-string f "  ; get-global: BEGIN\n")
       (compiler:c64-emit-load-value-to-$2 f procedure global-name)
       (io:write-string f "  lw $2, 2*4($2) ; Load global binding from symbol\n")
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  ; get-global: END\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;;  (io:write-string f "  # set-global [FIXME: IMPLEMENT]\n"))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (io:write-string f "  ; get-value: BEGIN\n")
       (compiler:c64-emit-load-value-to-$2 f procedure value)
       (io:write-string f "  sw $2, ")
       (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  ; get-value: END\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (io:write-string f "  ; primitive: BEGIN\n")
       (io:write-string f "  lw $25, (4 * ")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f ")($22) ; Load primitive address\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $4, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:c64-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " ; delay slot: pass the frame pointer\n")
       (io:write-string f "  move $28, $21 # Restore $gp, trashed by C functions under o32\n")
       (io:write-string f "  ; primitive: END\n"))
      ((trivial-compiler:instruction-fork name scratch-index)
       (io:write-string f "  ; fork\n")
       (io:write-string f "  ; [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (io:write-string f "  ; join\n")
       (io:write-string f "  ; [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (io:write-string f "  ; if-in: BEGIN\n")
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after")))
         (io:write-string f "  lw $3, ")
         (compiler:c64-emit-stack-access f (compiler:c64-scratch->stack-index procedure scratch-index))
         (io:write-string f " ; load the discriminand\n")
         (e1:dolist (value values)
           (compiler:c64-emit-load-value-to-$2 f procedure value)
           (io:write-string f "  beq $3, $2, ")
           (io:write-string f then-label)
           (io:write-string f " ; branch if equal\n")
           (io:write-string f "  nop\n"))
         ;;(io:write-string f "  # The next instruction should be an lw, harmless as a delay slot; anyway I want to avoid an assembler warning in case the next command is a multi-instruction macro\n")
         (compiler:c64-compile-instructions f procedure else-instructions)
         (io:write-string f "  j ")
         (io:write-string f after-label)
         (io:write-string f " ; skip the \"else\" branch\n")
         (io:write-string f "  nop ; Delay slot\n")
         (io:write-string f then-label)
         (io:write-string f ":\n")
         (compiler:c64-compile-instructions f procedure then-instructions)
         (io:write-string f after-label)
         (io:write-string f ":\n"))
       (io:write-string f "  ; if-in: END\n"))
      (_
       (e1:error "impossible")))))


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

(e1:define-macro (can assembly-file-name . forms)
  `(e1:begin
     (e1:define (main)
       ,@forms)
     (compiler:compile-procedure-to-assembly-with compiler:c64
                                                  (e1:value main)
                                                  ,assembly-file-name)))
