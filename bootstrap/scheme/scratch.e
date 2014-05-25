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
epsilon_main_entry_point:")
    ;; (fio:write-to f "  jmp ")
    ;; (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f "  jsr ")
    (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f "
  ;; FIXME: this is test code
print_string
  +print_string string_slot0
  +print_stack 0
  jsr print_return
  +print_string string_slot1
  +print_stack 1
  jsr print_return
  rts
string_slot0:
  !pet \"first:  \", 0
string_slot1:
  !pet \"second: \", 0



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

;;; FIXME: nothing similar is probably applicable to the 6502
(e1:define (compiler:c64-emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 4))
  (io:write-string f "($16)"))

;;; FIXME: nothing similar is probably applicable to the 6502
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

(e1:define (compiler:c64-limit n)
  (e1:cond ((fixnum:< n -32768)
            -32768)
           ((fixnum:> n 32767)
            32767)
           (else
            n)))

(e1:define (compiler:c64-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (fio:write-to f "  ;; return: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-tail-call name)
       (fio:write-to f "  ;; tail-call: FIXME: BEGIN (tentative)\n")
       (fio:write-to f "  jmp ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f " ;; " (sy name) "\n")
       (fio:write-to f "  ;; tail-call: FIXME: END (tentative)\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (fio:write-to f "  ;; tail-call-indirect: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (fio:write-to f "  ;; nontail-call: FIXME: unimplemented\n"))
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
       (fio:write-to f "  ;; get-local: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (fio:write-to f "  ;; set-local: FIXME: unimplemented\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       (fio:write-to f "  ;; get-local: FIXME: unimplemented\n"))
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
       (fio:write-to f "  ;; if-in: FIXME: unimplemented\n"))
      (_
       (e1:error "impossible")))))

(e1:define (compiler:c64-compile-primitive f procedure primitive-name scratch-index)
  (fio:write-to f "  ;; primitive " (sy primitive-name) ": FIXME begin\n")
  (e1:case primitive-name
    ((fixnum:+)
     (fio:write-to f "  +sum_stack_16bit "
                   (i (compiler:c64-scratch->stack-index procedure scratch-index))
                   ", "
                   (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                   ", "
                   (i (compiler:c64-scratch->stack-index procedure scratch-index))
                   "\n"))
    (else
     (fio:write-to f "  ;; FIXME: UNIMPLEMENTED\n")))
  (fio:write-to f "  ;; primitive " (sy primitive-name) ": FIXME END\n"))

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
