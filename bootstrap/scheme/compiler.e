;;;;; This is -*- epsilon -*-
;;;;; Trivial compiler

;;;;; Copyright (C) 2013, 2014 Luca Saiu

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


;;;;; Compiler metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A compiler matadata record contains information about how to
;;; compile for a given target, in terms of assembler and linker
;;; command-line interface.

(e1:define-record compiler:metadata
  procedure-closure
  ;; the following fields are all strings
  assembly-file-extension
  object-file-extension
  CCAS
  ASFLAGS
  CCLD
  LDFLAGS
  LIBS)


;;;;; Compiler internal machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return true iff the given body does not call any procedure; forks
;;; are not considered calls, since such calls happen in different
;;; threads.
(e1:define (e0:leaf-expression? e)
  (e1:match e
    ((e0:expression-variable h x)
     #t)
    ((e0:expression-value v)
     #t)
    ((e0:expression-bundle h items)
     (e0:leaf-expressions? items))
    ((e0:expression-primitive h name actuals)
     (e0:leaf-expressions? actuals))
    ((e0:expression-let h bound-variables bound-expression body)
     (e1:and (e0:leaf-expression? bound-expression)
             (e0:leaf-expression? body)))
    ((e0:expression-call h procedure-name actuals)
     #f)
    ((e0:expression-call-indirect h procedure-expression actuals)
     #f)
    ((e0:expression-if-in h discriminand values then-branch else-branch)
     (e1:and (e0:leaf-expression? discriminand)
             (e0:leaf-expression? then-branch)
             (e0:leaf-expression? else-branch)))
    ((e0:expression-fork h procedure-name actuals)
     (e0:leaf-expressions? actuals)) ;; actuals are evaluated in foreground
    ((e0:expression-join h future)
     (e0:leaf-expression? future))))

(e1:define (e0:leaf-expressions? ee)
  (e1:if (list:null? ee)
    #t
    (e1:and (e0:leaf-expression? (list:head ee))
            (e0:leaf-expressions? (list:tail ee)))))


;;;;; Trivial compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This solution is crude and generates inefficient code: we should
;;; run a liveness analysis and reuse indices for a good-performance
;;; solution.

;;; An alternative form of e0 expressions, where each variable is
;;; statically recognized as parameter, local or global; parameters
;;; and locals (including locals which shadow other variables) are
;;; assigned a unique index.
(e1:toplevel (sum:define-open trivial-compiler:expression
               (parameter handle index)
               (local handle index)
               (global handle name)
               ;; The remaining cases are just identical to e0:expression
               ;; except let and call-indirect.
               (value handle content)
               (bundle handle items)
               (primitive handle name actuals)
               (let handle local-from new-local-no bound-expression body)
               (call handle procedure-name actuals)
               (call-indirect handle local-index actuals)
               (if-in handle discriminand values then-branch else-branch)
               (fork handle procedure-name actuals)
               (join handle future)))

(e1:define (trivial-compiler:resolve-expression e formals locals)
  (e1:match e
    ((e0:expression-variable h x)
     (e1:cond ((list:has? locals x)
               (trivial-compiler:expression-local h (list:last-index-of locals x)))
              ((list:has? formals x)
               (trivial-compiler:expression-parameter h (list:index-of formals x)))
              (else
               (trivial-compiler:expression-global h x))))
    ((e0:expression-value h c)
     (trivial-compiler:expression-value h c))
    ((e0:expression-bundle h items)
     (trivial-compiler:expression-bundle h (trivial-compiler:resolve-expressions items formals locals)))
    ((e0:expression-primitive h name actuals)
     (trivial-compiler:expression-primitive h name (trivial-compiler:resolve-expressions actuals formals locals)))
    ((e0:expression-let h bound-variables bound-expression body)
     (trivial-compiler:expression-let h
                                      (list:length locals)
                                      (list:length bound-variables)
                                      (trivial-compiler:resolve-expression bound-expression formals locals)
                                      (trivial-compiler:resolve-expression body formals (list:append locals bound-variables))))
    ((e0:expression-call h procedure-name actuals)
     (trivial-compiler:expression-call h procedure-name (trivial-compiler:resolve-expressions actuals formals locals)))
    ((e0:expression-call-indirect h procedure-expression actuals)
     (e1:let ((name (symbol:fresh))
              (h1 (e0:fresh-handle)))
       (trivial-compiler:expression-let
          h1
          (list:length locals)
          1 ; one more bound variable
          (trivial-compiler:resolve-expression procedure-expression formals locals)
          (trivial-compiler:expression-call-indirect
             h
             (list:length locals)
             (trivial-compiler:resolve-expressions actuals formals (list:append locals (list:list name)))))))
    ;; ((e0:expression-call-indirect h procedure-expression actuals)
    ;;  (trivial-compiler:expression-call-indirect h
    ;;                                             (trivial-compiler:resolve-expression procedure-expression formals locals)
    ;;                                             (trivial-compiler:resolve-expressions actuals formals locals)))
    ((e0:expression-if-in h discriminand values then-branch else-branch)
     (trivial-compiler:expression-if-in h
                                        (trivial-compiler:resolve-expression discriminand formals locals)
                                        values
                                        (trivial-compiler:resolve-expression then-branch formals locals)
                                        (trivial-compiler:resolve-expression else-branch formals locals)))
    ((e0:expression-fork h procedure-name actuals)
     (trivial-compiler:expression-fork h procedure-name (trivial-compiler:resolve-expressions actuals formals locals)))
    ((e0:expression-join h future)
     (trivial-compiler:expression-join h (trivial-compiler:resolve-expression future formals locals)))))

(e1:define (trivial-compiler:resolve-expressions ee formals locals)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (trivial-compiler:resolve-expression (list:head ee) formals locals)
               (trivial-compiler:resolve-expressions (list:tail ee) formals locals))))

(e1:toplevel (sum:define trivial-compiler:instruction ;; from -> to
               (return)
               (tail-call name)
               (tail-call-indirect local-index)
               (nontail-call name scratch-index)
               (nontail-call-indirect local-index scratch-index)
               (get-io io-index scratch-index)
               (set-io scratch-index io-index)
               (get-local local-index scratch-index)
               (set-local scratch-index local-index)
               (get-global global-name scratch-index)
               ;;(set-global scratch-index global-name)
               (get-value value scratch-index)
               (primitive name scratch-index)
               (fork name scratch-index)
               (join scratch-index)
               (if-in scratch-index values then-instructions else-instructions))) ;; tailness doesn't matter

(e1:define trivial-compiler:label-box
  (box:make-0-initialized))
(e1:define (trivial-compiler:fresh-label prefix)
  (string:append prefix (string:fixnum->string (box:bump-and-get! trivial-compiler:label-box))))

;;; Return the maximum io index used in the given instructions, or -1
(e1:define (trivial-compiler:max-io ii)
  (trivial-compiler:max-io-acc ii -1))
(e1:define (trivial-compiler:max-io-acc ii acc)
  (e1:match ii
    ((list:list-nil) acc)
    ((list:list-cons (trivial-compiler:instruction-get-io io _) rest)
     (trivial-compiler:max-io-acc rest (fixnum:max io acc)))
    ((list:list-cons (trivial-compiler:instruction-set-io _ io) rest)
     (trivial-compiler:max-io-acc rest (fixnum:max io acc)))
    ((list:list-cons (trivial-compiler:instruction-if-in _ _ then-instructions else-instructions) rest)
     (trivial-compiler:max-io-acc (list:append-reversed then-instructions (list:append-reversed else-instructions rest))
                                  acc))
    ((list:list-cons _ rest)
     (trivial-compiler:max-io-acc rest acc))))

;;; Return the maximum local index used in the given instructions, or -1
(e1:define (trivial-compiler:max-local ii)
  (trivial-compiler:max-local-acc ii -1))
(e1:define (trivial-compiler:max-local-acc ii acc)
  (e1:match ii
    ((list:list-nil) acc)
    ((list:list-cons (trivial-compiler:instruction-get-local local _) rest)
     (trivial-compiler:max-local-acc rest (fixnum:max local acc)))
    ((list:list-cons (trivial-compiler:instruction-set-local _ local) rest)
     (trivial-compiler:max-local-acc rest (fixnum:max local acc)))
    ((list:list-cons (trivial-compiler:instruction-if-in _ _ then-instructions else-instructions) rest)
     (trivial-compiler:max-local-acc (list:append-reversed then-instructions (list:append-reversed else-instructions rest))
                                     acc))
    ((list:list-cons (trivial-compiler:instruction-tail-call-indirect local-index) rest)
     (trivial-compiler:max-local-acc rest (fixnum:max local-index acc)))
    ((list:list-cons (trivial-compiler:instruction-nontail-call-indirect local-index _) rest)
     (trivial-compiler:max-local-acc rest (fixnum:max local-index acc)))
    ((list:list-cons _ rest)
     (trivial-compiler:max-local-acc rest acc))))

;;; Return the maximum stack height used in the given instructions, or -1
(e1:define (trivial-compiler:max-height ii)
  42) ;; FIXME: implement

;;; e is a trivial-compiler:expression, with qualified variables.  Return instructions in reversed order.
(e1:define (trivial-compiler:compile-expression e tail height)
  (e1:match e
    ((trivial-compiler:expression-parameter h i)
     (trivial-compiler:compile-parameter i tail height))
    ((trivial-compiler:expression-local h i)
     (trivial-compiler:compile-local i tail height))
    ((trivial-compiler:expression-global h x)
     (trivial-compiler:compile-global x tail height))
    ((trivial-compiler:expression-value h c)
     (trivial-compiler:compile-value c tail height))
    ((trivial-compiler:expression-bundle h items)
     (trivial-compiler:compile-bundle items tail height))
    ((trivial-compiler:expression-primitive h name actuals)
     (trivial-compiler:compile-primitive name actuals tail height))
    ((trivial-compiler:expression-let h local-from new-local-no bound-expression body)
     (trivial-compiler:compile-let local-from new-local-no bound-expression body tail height))
    ((trivial-compiler:expression-call h procedure-name actuals)
     (trivial-compiler:compile-call procedure-name actuals tail height))
    ((trivial-compiler:expression-call-indirect h local-index actuals)
     (trivial-compiler:compile-call-indirect local-index actuals tail height))
    ((trivial-compiler:expression-if-in h discriminand values then-branch else-branch)
     (trivial-compiler:compile-if-in discriminand values then-branch else-branch tail height))
    ((trivial-compiler:expression-fork h procedure-name actuals)
     (trivial-compiler:compile-fork procedure-name actuals tail height))
    ((trivial-compiler:expression-join h future)
     (trivial-compiler:compile-join future tail height))))
(e1:define (trivial-compiler:compile-nontail-expressions ee height)
  (e1:if (list:null? ee)
    list:nil
    (list:append (trivial-compiler:compile-nontail-expressions (list:tail ee) (fixnum:1+ height))
                 (trivial-compiler:compile-expression (list:head ee) #f height))))
(e1:define (trivial-compiler:compile-expressions ee tail height)
  (trivial-compiler:prepend-return-if-needed tail height (list:length ee)
     (trivial-compiler:compile-nontail-expressions ee height)))

(e1:define (trivial-compiler:set-ios height how-many)
  (e1:if (fixnum:zero? how-many)
    list:nil
    (list:cons (trivial-compiler:instruction-set-io (fixnum:+ height how-many -1)
                                                    (fixnum:1- how-many))
               (trivial-compiler:set-ios height (fixnum:1- how-many)))))

(e1:define (trivial-compiler:set-locals height local-from how-many)
  (e1:if (fixnum:zero? how-many)
    list:nil
    (list:cons (trivial-compiler:instruction-set-local (fixnum:+ height how-many -1)
                                                       (fixnum:+ local-from how-many -1))
               (trivial-compiler:set-locals height local-from (fixnum:1- how-many)))))

(e1:define (trivial-compiler:prepend-return-if-needed tail height out-dimension instructions)
  (list:append
    (e1:if tail
      (list:cons (trivial-compiler:instruction-return)
                 (trivial-compiler:set-ios height out-dimension))
      list:nil)
    instructions))

(e1:define (trivial-compiler:compile-parameter i tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:list (trivial-compiler:instruction-get-io i height))))
(e1:define (trivial-compiler:compile-local i tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:list (trivial-compiler:instruction-get-local i height))))
(e1:define (trivial-compiler:compile-global x tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:list (trivial-compiler:instruction-get-global x height))))
(e1:define (trivial-compiler:compile-value c tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:list (trivial-compiler:instruction-get-value c height))))
(e1:define (trivial-compiler:compile-bundle items tail height)
  (trivial-compiler:compile-expressions items tail height))
(e1:define (trivial-compiler:compile-primitive name actuals tail height)
  (trivial-compiler:prepend-return-if-needed tail height (state:primitive-get-out-dimension name)
     (list:cons (trivial-compiler:instruction-primitive name height)
                (trivial-compiler:compile-expressions actuals #f height))))
(e1:define (trivial-compiler:compile-let local-from new-local-no bound-expression body tail height)
  (list:append (trivial-compiler:compile-expression body tail height)
               (trivial-compiler:set-locals height local-from new-local-no)
               (trivial-compiler:compile-expression bound-expression #f height)))
(e1:define (trivial-compiler:compile-call procedure-name actuals tail height)
  (e1:if tail
    (list:cons (trivial-compiler:instruction-tail-call procedure-name)
               (list:append (trivial-compiler:set-ios height (list:length actuals))
                            (trivial-compiler:compile-expressions actuals #f height)))
    (list:cons (trivial-compiler:instruction-nontail-call procedure-name height)
               (trivial-compiler:compile-expressions actuals #f height))))
(e1:define (trivial-compiler:compile-call-indirect local-index actuals tail height)
  (e1:if tail
    (list:cons (trivial-compiler:instruction-tail-call-indirect local-index)
               (list:append (trivial-compiler:set-ios height (fixnum:1+ (list:length actuals)))
                            (trivial-compiler:compile-expressions actuals #f 0)))
    (list:cons (trivial-compiler:instruction-nontail-call-indirect local-index height)
               (trivial-compiler:compile-expressions actuals #f height))))
(e1:define (trivial-compiler:compile-if-in discriminand values then-branch else-branch tail height)
  (list:cons
    (trivial-compiler:instruction-if-in height
                                        values
                                        (trivial-compiler:compile-expression then-branch tail height)
                                        (trivial-compiler:compile-expression else-branch tail height))
    (trivial-compiler:compile-expression discriminand #f height)))
(e1:define (trivial-compiler:compile-fork procedure-name actuals tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:cons (trivial-compiler:instruction-fork procedure-name height)
                (trivial-compiler:compile-expressions actuals #f height))))
(e1:define (trivial-compiler:compile-join future tail height)
  (trivial-compiler:prepend-return-if-needed tail height 1
     (list:cons (trivial-compiler:instruction-join height)
                (trivial-compiler:compile-expression future #f height))))


;;;;; Imperative stacks.  FIXME: move to epsilon1.e.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: reimplement in a smarter way

(e1:define (imperative-stack:make)
  (box:make list:nil))

(e1:define (imperative-stack:null? s)
  (list:null? (box:get s)))

(e1:define (imperative-stack:push! s x)
  (box:set! s (list:cons x (box:get s))))

(e1:define (imperative-stack:pop! s)
  (e1:let* ((list (box:get s)))
    (box:set! s (list:tail list))
    (list:head list)))

(e1:define (imperative-stack:top s)
  (list:head (box:get s)))

;;; Return a list containing all the elements of the given stack in the same order,
;;; so that the stack top corresponds to the list head.
(e1:define (imperative-stack:imperative-stack->list s)
  (box:get s))

;;; Return a stack containing all the elements of the given in the
;;; same order, so that the list head is on the top.
(e1:define (imperative-stack:list->imperative-stack list)
  (e1:let* ((result (imperative-stack:make)))
    (e1:dolist (element (list:reverse list))
      (imperative-stack:push! result element))))

(e1:define (imperative-stack:imperative-stack->vector s)
  (vector:list->vector (imperative-stack:imperative-stack->list s)))

(e1:define (imperative-stack:vector->imperative-stack v)
  (imperative-stack:list->imperative-stack (vector:vector->list v)))


;;;;; Data graph visit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel (record:define data-graph:graph
               procedures    ;; reachable procdures - a symbol list
               globals       ;; reachable globals - a symbol list
               pointers      ;; non-omitted pointer list, also including the symbols above
               pointer-hash  ;; pointer -> index (1-based; 0 for omitted pointers)
               symbol-hash)) ;; symbol -> whatever

;;; FIXME: optionally, we should find used code *syntactically*, by
;;; recursion over procedure bodies but omitting the procedures as
;;; data structures.  This would make static non-reflective compiled
;;; programs smaller and slightly more efficient, mostly because of GC
;;; roots.

(e1:define (data-graph:graph-from-excluding main-object symbol-indices-to-exclude)
  (e1:let* ((symbol-hash (string-hash:invert-into-unboxed-hash symbol:table)) ;; symbol -> whatever
            (hash (unboxed-hash:make)) ;; pointer -> index
            (stack (imperative-stack:make))
            (procedures (imperative-stack:make))
            (globals (imperative-stack:make))
            (pointers (imperative-stack:make))
            ;; FIXME: turn omitted-indices-hash into a buffer; factor away the functionality turning
            ;; a list or an unboxed hash with small fixnum keys into an efficient buffer (+ length)
            (omitted-indices-hash (unboxed-hash:make))
            (omitted-pointers (unboxed-hash:make)))
    (e1:dolist (i symbol-indices-to-exclude)
      (unboxed-hash:set! omitted-indices-hash i #t))
    (e1:when (e0:primitive whatever:buffer? main-object)
      (imperative-stack:push! stack main-object)
      (unboxed-hash:set! hash main-object 1))
    (e1:while (e1:not (imperative-stack:null? stack))
      (e1:let* ((pointer (imperative-stack:pop! stack))
                (is-symbol (unboxed-hash:has? symbol-hash pointer)))
        (imperative-stack:push! pointers pointer)
        (e1:when is-symbol
          (e1:when (symbol:procedure-name? pointer)
            (imperative-stack:push! procedures pointer))
          (e1:when (symbol:global-name? pointer)
            (imperative-stack:push! globals pointer)))
        (e1:dotimes (i (e0:primitive buffer:length pointer))
          (e1:let* ((element (buffer:get pointer i)))
            (e1:if (e1:and is-symbol (unboxed-hash:has? omitted-indices-hash i))
              (unboxed-hash:set! hash element 0)
              (e1:when (e1:and (e0:primitive whatever:buffer? element)
                               (e1:not (unboxed-hash:has? hash element)))
                (unboxed-hash:set! hash element (fixnum:1+ (unboxed-hash:element-no hash)))
                (imperative-stack:push! stack element)))))))
    (e1:dolist (p (box:get procedures))
      (fio:write "* Procedure to compile: " (sy p) "\n"))
    (data-graph:graph
       (box:get procedures);(imperative-stack:imperative-stack->vector procedures)
       (box:get globals);(imperative-stack:imperative-stack->vector globals)
       (box:get pointers);(imperative-stack:imperative-stack->vector pointers)
       hash
       symbol-hash)))

(e1:define (data-graph:graph-from-no-macros main-object)
  (data-graph:graph-from-excluding main-object
                                   (list:list 5 6    ; macro, macro procedure
                                              8 9))) ; bytecode, native

;;; FIXME: this doesn't keep its promise not to visit bodies yet,
;;; because of the FIXME note above: when we omit procedure bodies as
;;; data structures we have to scan them anyway to find used
;;; procedures and globals.
(e1:define (data-graph:graph-from-compiled-only main-object)
  (data-graph:graph-from-excluding main-object
                                   (list:list 3 ;;4    ; formals, ;;body
                                              5 6    ; macro, macro procedure
                                              7      ; primitive descriptor
                                              8 9))) ; bytecode, native
(e1:define (data-graph:graph-from main-object)
  (data-graph:graph-from-excluding main-object
                                   (list:list 8 9))) ; bytecode, native


;;;;; Procedure compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel (record:define trivial-compiler:procedure
                name
                formals
                instructions
                leaf
                io-no
                local-no
                scratch-no
                ;;pointer-hash ;; pointer->index
                ;;pointers)) ;; pointers in order
                data-graph))
(e1:define (trivial-compiler:compile-procedure name formals body data-graph)
  (e1:let* ((resolved-body (trivial-compiler:resolve-expression body formals list:nil))
            (instructions (trivial-compiler:compile-expression resolved-body #t 0)))
    (trivial-compiler:procedure name
                                formals
                                instructions
                                (e0:leaf-expression? body)
                                (fixnum:1+ (trivial-compiler:max-io instructions))
                                (fixnum:1+ (trivial-compiler:max-local instructions))
                                (fixnum:1+ (trivial-compiler:max-height instructions))
                                data-graph)))

;;; Emit the mangled version of the given identifier.
(e1:define (trivial-compiler:emit-string-identifier f string)
  ;; (io:write-string f "/* ")
  ;; (io:write-string f string)
  ;; (io:write-string f " */")
  (e1:dotimes (i (string:length string))
    (e1:let* ((c (string:get string i)))
      (e1:if (e1:or (e1:and (fixnum:<= 48 c) (fixnum:<= c 57))   ; digit
                    (e1:and (fixnum:<= 65 c) (fixnum:<= c 90))   ; upper-case letter
                    (e1:and (fixnum:<= 97 c) (fixnum:<= c 122))) ; lower-case letter
        (io:write-character f c)
        (e1:begin
          (io:write-string f "_")
          (io:write-fixnum f c)
          (io:write-string f "_"))))))

(e1:define (trivial-compiler:emit-symbol-identifier f symbol)
  (trivial-compiler:emit-string-identifier f (symbol:symbol->string symbol)))

(e1:define (trivial-compiler:compile-data f data-graph 64bit)
  (e1:let* ((hash (data-graph:graph-get-pointer-hash data-graph))
            (pointers (data-graph:graph-get-pointers data-graph))
            (symbol-hash (data-graph:graph-get-symbol-hash data-graph))
            (word-or-quad (e1:if 64bit ".quad" ".word")))
    (io:write-string f "# Global data\n")
    (io:write-string f "  .data\n")
    (io:write-string f "  .balign 8# .align 2\n")
    (io:write-string f "  .type global_data_beginning, @object\n")
    (io:write-string f "global_data_beginning:\n")
    (io:write-string f "  #.section .data.rel\n")
    (io:write-string f "  .globl global_data_beginning\n")
    (io:write-string f "p0:\n")
    (io:write-string f "  .quad 0xbad # omitted from compilation\n")
    (e1:dolist (pointer pointers)
      (e1:let* ((index (unboxed-hash:get hash pointer))
                (is-symbol (unboxed-hash:has? symbol-hash pointer))
                (is-procedure-name (e1:and is-symbol (symbol:procedure-name? pointer))))
        (e1:unless (fixnum:zero? index)
          (io:write-string f "p")
          (io:write-fixnum f index)
          (io:write-string f ":")
          (e1:when is-symbol
            (io:write-string f " # the symbol ")
            (io:write-symbol f pointer))
          (io:write-string f "\n  ")
          (io:write-string f word-or-quad)
          (e1:dotimes (j (e0:primitive buffer:length pointer))
            (e1:unless (fixnum:zero? j)
              (io:write-string f ","))
            (io:write-string f " ")
            (e1:if (e1:and is-procedure-name (fixnum:= j 9)) ;; native code slot
              (trivial-compiler:emit-symbol-identifier f pointer)
              (e1:let* ((element (buffer:get pointer j)))
                (e1:if (e0:primitive whatever:atom? element)
                  (io:write-fixnum f element)
                  (e1:begin
                    (io:write-string f "p")
                    (io:write-fixnum f (unboxed-hash:get hash element)))))))
          (io:write-string f "\n"))))
    (io:write-string f "global_data_end:\n")
    (io:write-string f "  #.section .data.rel\n")
    (io:write-string f "  .globl global_data_end\n")
    (io:write-string f "  .quad 0\n")
    (io:write-string f "\n\n")))


;;;;; C-generating compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define compiler:c
  (compiler:metadata  #:procedure-closure
                      (e1:lambda (procedure-name target-file-name)
                        (compiler:c-compile-to-assembly procedure-name target-file-name))
                      #:assembly-file-extension ".c"
                      #:object-file-extension ".o"
                      #:CCAS configuration:CC
                      #:ASFLAGS (string:append configuration:CPPFLAGS
                                               " "
                                               "-DEPSILON_RUNTIME_UNTAGGED"
                                               " "
                                               "-I'" configuration:abs_top_builddir "'"
                                               " "
                                               "-I'" configuration:abs_top_srcdir "'"
                                               " "
                                               "-c")
                      #:CCLD configuration:CC
                      #:LDFLAGS (string:append "-L'" configuration:abs_top_builddir "/lib'"
                                               " "
                                               configuration:LDFLAGS)
                      #:LIBS (string:append configuration:LIBS
                                            " "
                                            "-lepsilondriver-native-untagged -lepsilonruntime-untagged -lepsilonutility")))

(e1:define (compiler:c-compile-to-assembly main target-file-name)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file target-file-name io:write-mode)))
    (fio:write-to f "#include \"runtime/runtime.h\"\n\n")
    (compiler:c-emit-forward-declarations f data-graph)
    (io:write-string f "\n")
    (fio:write-to f "/* Driver. */
void epsilon_main_entry_point(epsilon_value *stack){")
    (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f "(stack);}\n\n")
    (compiler:c-compile-data f data-graph)
    (io:write-string f "\n")
    (compiler:c-compile-procedures f data-graph)
    (io:close-file f)))

(e1:define (compiler:c-emit-forward-declarations f data-graph)
  (e1:let* ((procedure-names (data-graph:graph-get-procedures data-graph))
            (pointers (data-graph:graph-get-pointers data-graph))
            (pointer-hash (data-graph:graph-get-pointer-hash data-graph)))
    (e1:dolist (procedure-name procedure-names)
      (io:write-string f "void ")
      (trivial-compiler:emit-symbol-identifier f procedure-name)
      (io:write-string f "(epsilon_value *stack);\n"))
    (e1:dolist (pointer pointers)
      (io:write-string f "extern epsilon_value p")
      (io:write-fixnum f (unboxed-hash:get pointer-hash pointer))
      (io:write-string f "[];\n"))))

(e1:define (compiler:c-compile-data f data-graph)
  (e1:let* ((hash (data-graph:graph-get-pointer-hash data-graph))
            (pointers (data-graph:graph-get-pointers data-graph))
            (symbol-hash (data-graph:graph-get-symbol-hash data-graph)))
    (io:write-string f "char global_data_beginning;\n")
    (io:write-string f "epsilon_value p0[] = { EPSILON_LONG_TO_EPSILON_WORD(0xbad) }; // omitted from compilation\n")
    (e1:dolist (pointer pointers)
      (e1:let* ((index (unboxed-hash:get hash pointer))
                (is-symbol (unboxed-hash:has? symbol-hash pointer))
                (is-procedure-name (e1:and is-symbol (symbol:procedure-name? pointer))))
        (e1:unless (fixnum:zero? index)
          (io:write-string f "epsilon_value p")
          (io:write-fixnum f index)
          (io:write-string f "[] = {")
          (e1:when is-symbol
            (io:write-string f " // the symbol ")
            (io:write-symbol f pointer))
          (io:write-string f "\n ")
          (e1:dotimes (j (e0:primitive buffer:length pointer))
            (e1:unless (fixnum:zero? j)
              (io:write-string f ","))
            (io:write-string f " ")
            (e1:if (e1:and is-procedure-name (fixnum:= j 9)) ;; native code slot
              (trivial-compiler:emit-symbol-identifier f pointer)
              (e1:let* ((element (buffer:get pointer j)))
                (e1:if (e0:primitive whatever:atom? element)
                  (e1:begin
                    (io:write-string f "EPSILON_LONG_TO_EPSILON_WORD(")
                    (io:write-fixnum f element)
                    (io:write-string f ")"))
                  (e1:begin
                    (io:write-string f "p")
                    (io:write-fixnum f (unboxed-hash:get hash element)))))))
          (io:write-string f " };\n"))))
    (io:write-string f "char global_data_end;\n")))


(e1:define (compiler:c-compile-procedures f data-graph)
  (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
    (compiler:c-compile-procedure f procedure-name data-graph)))

(e1:define (compiler:c-compile-procedure f procedure-name data-graph)
  (e1:let* ((formals (state:procedure-get-formals procedure-name))
            (body (state:procedure-get-body procedure-name))
            (procedure (trivial-compiler:compile-procedure procedure-name formals body data-graph)))
    (io:write-string f "// ")
    (io:write-symbol f procedure-name)
    (io:write-string f "\n")
    (io:write-string f "void ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "(epsilon_value *stack){\n")
    (io:write-string f "// io-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-io-no procedure))
    (io:write-string f "\n")
    (io:write-string f "// leaf is ")
    (io:write-string f (e1:if (trivial-compiler:procedure-get-leaf procedure) "#t" "#f"))
    (io:write-string f "\n")
    (io:write-string f "// local-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-local-no procedure))
    (io:write-string f "\n")
    (io:write-string f "// scratch-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-scratch-no procedure))
    (io:write-string f "\n")
    (compiler:c-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "// This should be unreachable\n  return;\n}\n\n")))

(e1:define (compiler:c-io->stack-index procedure io)
  io)
(e1:define (compiler:c-local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            ;;(e1:if (trivial-compiler:procedure-get-leaf procedure) 0 1) ; return slot
            local))
(e1:define (compiler:c-scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            ;;(e1:if (trivial-compiler:procedure-get-leaf procedure) 0 1) ; return slot
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))

(e1:define (compiler:c-emit-io f procedure io)
  (io:write-string f "stack[")
  (io:write-fixnum f (compiler:c-io->stack-index procedure io))
  (io:write-string f "]"))
(e1:define (compiler:c-emit-local f procedure local)
  (io:write-string f "stack[")
  (io:write-fixnum f (compiler:c-local->stack-index procedure local))
  (io:write-string f "]"))
(e1:define (compiler:c-emit-scratch f procedure scratch)
  (io:write-string f "stack[")
  (io:write-fixnum f (compiler:c-scratch->stack-index procedure scratch))
  (io:write-string f "]"))

(e1:define (compiler:c-emit-value f procedure value)
  (e1:if (e0:primitive whatever:atom? value)
    (e1:begin
      (io:write-string f "epsilon_int_to_epsilon_value(")
      (io:write-fixnum f value)
      (io:write-string f ")"))
    (e1:let* ((data-graph (trivial-compiler:procedure-get-data-graph procedure))
              (pointer-hash (data-graph:graph-get-pointer-hash data-graph))
              (symbol-hash (data-graph:graph-get-symbol-hash data-graph))
              (index (unboxed-hash:get pointer-hash value)))
      (io:write-string f "p")
      (io:write-fixnum f index)
      (e1:when (unboxed-hash:has? symbol-hash value)
        (io:write-string f " /* ")
        (io:write-symbol f value)
        (io:write-string f " */ ")))))

(e1:define (compiler:c-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (io:write-string f "  return;\n"))
      ((trivial-compiler:instruction-tail-call name)
       (io:write-string f "// tail-call ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "  ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "(stack);\n")
       (io:write-string f "  return;\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (io:write-string f "// tail-call-indirect: BEGIN\n")
       (io:write-string f "  ((epsilon_compiled_c_function)(((epsilon_value*)(")
       (compiler:c-emit-local f procedure local-index)
       (io:write-string f "))[9]))(stack);\n")
       (io:write-string f "  return;\n")
       (io:write-string f "// tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (io:write-string f "// nontail-call ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "  ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "(stack + ")
       (io:write-fixnum f (compiler:c-scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (io:write-string f "// nontail-call-indirect: BEGIN\n")
       (io:write-string f "  ((epsilon_compiled_c_function)(((epsilon_value*)(")
       (compiler:c-emit-local f procedure local-index)
       (io:write-string f "))[9]))(stack + ")
       (io:write-fixnum f (compiler:c-scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n")
       (io:write-string f "// nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (io:write-string f "  ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (compiler:c-emit-io f procedure io-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (io:write-string f "  ")
       (compiler:c-emit-io f procedure io-index)
       (io:write-string f " = ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (io:write-string f "  ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (compiler:c-emit-local f procedure local-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (io:write-string f "  ")
       (compiler:c-emit-local f procedure local-index)
       (io:write-string f " = ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       (io:write-string f "  ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (compiler:c-emit-value f procedure global-name)
       (io:write-string f "[2];\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;; FIXME: implement if ever needed)
      ((trivial-compiler:instruction-get-value value scratch-index)
       (io:write-string f "  ")
       (compiler:c-emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (compiler:c-emit-value f procedure value)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (io:write-string f "// primitive ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "  //epsilon_call_c_primitive_by_index(")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f ", stack + ")
       (io:write-fixnum f (compiler:c-scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n")
       (io:write-string f "  epsilon_c_primitive_functions[")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f "](stack + ")
       (io:write-fixnum f (compiler:c-scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n"))
      ((trivial-compiler:instruction-fork name scratch-index)
       (io:write-string f "// fork ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "volatile int a = 1; a /= 0; // die horribly\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (io:write-string f "// join\n")
       (io:write-string f "volatile int a = 1; a /= 0; // die horribly\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after")))
         (io:write-string f "// if-in, tail or not\n")
         (e1:dolist (value values)
           (io:write-string f "  if(epsilon_value_to_epsilon_int(")
           (compiler:c-emit-scratch f procedure scratch-index)
           (io:write-string f ") == ")
           (io:write-fixnum f value)
           (io:write-string f ") goto ")
           (io:write-string f then-label)
           (io:write-string f ";\n"))
         (compiler:c-compile-instructions f procedure else-instructions)
         (io:write-string f "  goto ")
         (io:write-string f after-label)
         (io:write-string f ";\n")
         (io:write-string f then-label)
         (io:write-string f ":\n")
         (compiler:c-compile-instructions f procedure then-instructions)
         (io:write-string f after-label)
         (io:write-string f ":\n")))
      (_
       (e1:error "impossible")))))


;;;;; MIPS compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define compiler:mips
  (compiler:metadata  #:procedure-closure
                      (e1:lambda (procedure-name target-file-name)
                        (compiler:mips-compile-to-assembly procedure-name target-file-name))
                      #:assembly-file-extension ".s"
                      #:object-file-extension ".o"
                      #:CCAS configuration:CCAS
                      #:ASFLAGS (string:append "-c"
                                               " "
                                               configuration:CCASFLAGS)
                      #:CCLD configuration:CC
                      #:LDFLAGS (string:append "-L'" configuration:abs_top_builddir "/lib'"
                                               " "
                                               configuration:LDFLAGS)
                      #:LIBS (string:append configuration:LIBS
                                            " "
                                            "-lepsilondriver-native-untagged -lepsilonruntime-untagged -lepsilonutility")))

(e1:define (compiler:mips-compile-to-assembly main target-file-name)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file target-file-name io:write-mode)))
    (compiler:mips-compile-data f data-graph)
    (io:write-string f "  .text\n")
    (io:write-string f "  .set noreorder\n")
    (io:write-string f "  .set macro #  .set nomacro\n")
    (io:write-string f "  .set nomips16\n\n")
    (fio:write-to f "# Driver
  .balign 8 #.align 2
  .globl epsilon_main_entry_point
  .ent epsilon_main_entry_point
  .type epsilon_main_entry_point, @function
epsilon_main_entry_point:
  j ")
    (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f "
  .end epsilon_main_entry_point
  .size epsilon_main_entry_point, .-epsilon_main_entry_point\n\n")
    (io:write-string f "# Procedures\n")
    (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
      (compiler:mips-compile-procedure f procedure-name data-graph))
    (io:close-file f)))

(e1:define (compiler:mips-compile-data f data-graph)
  (trivial-compiler:compile-data f data-graph #f))

(e1:define (compiler:mips-compile-procedure f procedure-name data-graph)
  (e1:let* ((formals (state:procedure-get-formals procedure-name))
            (body (state:procedure-get-body procedure-name))
            (procedure (trivial-compiler:compile-procedure procedure-name formals body data-graph)))
    (io:write-string f "\n")
    (io:write-string f "######### ")
    (io:write-symbol f procedure-name)
    (io:write-string f " (mangled as \"")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\")\n")
    (io:write-string f "# io-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-io-no procedure))
    (io:write-string f "\n")
    (io:write-string f "# leaf is ")
    (io:write-string f (e1:if (trivial-compiler:procedure-get-leaf procedure) "#t" "#f"))
    (io:write-string f "\n")
    (io:write-string f "# local-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-local-no procedure))
    (io:write-string f "\n")
    (io:write-string f "# scratch-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-scratch-no procedure))
    (io:write-string f "
  .balign 8 #.align 2
  .globl ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "
  .ent ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "
  .type ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", @function\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ":\n")
    (io:write-string f "  sw $31, ")
    (io:write-fixnum f (fixnum:* 4 (compiler:mips-return-stack-index procedure)))
    (io:write-string f "($16) # Save return address\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "_TAIL:\n")
    (io:write-string f "################ BEGIN\n")
    (compiler:mips-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "################ END
  .end ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "
  .size ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", .-")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\n\n")))

(e1:define (compiler:mips-io->stack-index procedure io)
  io)
(e1:define (compiler:mips-return-stack-index procedure)
  (trivial-compiler:procedure-get-io-no procedure))
;; (e1:define (compiler:mips-frame-pointer-stack-index procedure)
;;   (fixnum:1+ (trivial-compiler:procedure-get-io-no procedure)))
(e1:define (compiler:mips-local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            local))
(e1:define (compiler:mips-scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))
(e1:define (compiler:mips-emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 4))
  (io:write-string f "($16)"))

(e1:define (compiler:mips-emit-load-value-to-$2 f procedure value)
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
        (io:write-string f " # ")
        (io:write-symbol f value))))
  (io:write-string f "\n"))

(e1:define (compiler:mips-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (io:write-string f "  # return: BEGIN\n")
       (io:write-string f "  lw $31, ")
       (compiler:mips-emit-stack-access f (compiler:mips-return-stack-index procedure))
       (io:write-string f "\n")
       (io:write-string f "  jr $31 # return\n")
       (io:write-string f "  nop # empty delay slot\n")
       (io:write-string f "  # return: END\n"))
      ((trivial-compiler:instruction-tail-call name)
       (io:write-string f "  # tail-call: BEGIN\n")
       (io:write-string f "  la $2, ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f " # load destination (may be large)\n")
       (io:write-string f "  jr $2 # jump\n")
       (io:write-string f "  lw $31, ")
       (compiler:mips-emit-stack-access f (compiler:mips-return-stack-index procedure))
       (io:write-string f " # pass the return address via $31 (delay slot)\n")
       (io:write-string f "  # tail-call: END\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (io:write-string f "  # tail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (compiler:mips-emit-stack-access f (compiler:mips-local->stack-index procedure local-index))
       (io:write-string f " # load code address\n")
       (io:write-string f "  lw $25, 9*4($25) # Load native code address from symbol\n")
       (io:write-string f "  jr $25\n")
       (io:write-string f "  lw $31, ")
       (compiler:mips-emit-stack-access f (compiler:mips-return-stack-index procedure))
       (io:write-string f " # delay slot: pass the return address via $31\n")
       (io:write-string f "  # tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (io:write-string f "  # nontail-call: BEGIN\n")
       (io:write-string f "  la $2, ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "\n")
       (io:write-string f "  jalr $2\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:mips-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (compiler:mips-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # restore frame pointer\n")
       (io:write-string f "  # nontail-call: END\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (io:write-string f "  # nontail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (compiler:mips-emit-stack-access f (compiler:mips-local->stack-index procedure local-index))
       (io:write-string f " # load symbol address\n")
       (io:write-string f "  lw $25, 9*4($25) # Load native code address from symbol\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:mips-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (compiler:mips-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # restore frame pointer\n")
       (io:write-string f "  # nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (io:write-string f "  # get-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-io: END\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (io:write-string f "  # set-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  # set-io: END\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (io:write-string f "  # get-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-local: END\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (io:write-string f "  # set-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  # set-local: END\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       ;;; FIXME: this is correct, but I could generate better code by exploiting global immutability
       (io:write-string f "  # get-global: BEGIN\n")
       (compiler:mips-emit-load-value-to-$2 f procedure global-name)
       (io:write-string f "  lw $2, 2*4($2) # Load global binding from symbol\n")
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-global: END\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;;  (io:write-string f "  # set-global [FIXME: IMPLEMENT]\n"))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (io:write-string f "  # get-value: BEGIN\n")
       (compiler:mips-emit-load-value-to-$2 f procedure value)
       (io:write-string f "  sw $2, ")
       (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-value: END\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (io:write-string f "  # primitive: BEGIN\n")
       (io:write-string f "  lw $25, (4 * ")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f ")($22) # Load primitive address\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $4, $16, ")
       (io:write-fixnum f (fixnum:* (compiler:mips-scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # delay slot: pass the frame pointer\n")
       (io:write-string f "  move $28, $21 # Restore $gp, trashed by C functions under o32\n")
       (io:write-string f "  # primitive: END\n"))
      ((trivial-compiler:instruction-fork name scratch-index)
       (io:write-string f "  # fork\n")
       (io:write-string f "  # [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (io:write-string f "  # join\n")
       (io:write-string f "  # [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (io:write-string f "  # if-in: BEGIN\n")
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after")))
         (io:write-string f "  lw $3, ")
         (compiler:mips-emit-stack-access f (compiler:mips-scratch->stack-index procedure scratch-index))
         (io:write-string f " # load the discriminand\n")
         (e1:dolist (value values)
           (compiler:mips-emit-load-value-to-$2 f procedure value)
           (io:write-string f "  beq $3, $2, ")
           (io:write-string f then-label)
           (io:write-string f " # branch if equal\n")
           (io:write-string f "  nop\n"))
         ;;(io:write-string f "  # The next instruction should be an lw, harmless as a delay slot; anyway I want to avoid an assembler warning in case the next command is a multi-instruction macro\n")
         (compiler:mips-compile-instructions f procedure else-instructions)
         (io:write-string f "  j ")
         (io:write-string f after-label)
         (io:write-string f " # skip the \"else\" branch\n")
         (io:write-string f "  nop # Delay slot\n")
         (io:write-string f then-label)
         (io:write-string f ":\n")
         (compiler:mips-compile-instructions f procedure then-instructions)
         (io:write-string f after-label)
         (io:write-string f ":\n"))
       (io:write-string f "  # if-in: END\n"))
      (_
       (e1:error "impossible")))))


;;;;; x86_64 compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define compiler:x86_64
  (compiler:metadata  #:procedure-closure
                      (e1:lambda (procedure-name target-file-name)
                        (compiler:x86_64-compile-to-assembly procedure-name target-file-name))
                      #:assembly-file-extension ".s"
                      #:object-file-extension ".o"
                      #:CCAS configuration:CCAS
                      #:ASFLAGS (string:append "-c"
                                               " "
                                               configuration:CCASFLAGS)
                      #:CCLD configuration:CC
                      #:LDFLAGS (string:append "-L'" configuration:abs_top_builddir "/lib'"
                                               " "
                                               configuration:LDFLAGS)
                      #:LIBS (string:append configuration:LIBS
                                            " "
                                            "-lepsilondriver-native-untagged -lepsilonruntime-untagged -lepsilonutility")))

(e1:define (compiler:x86_64-compile-to-assembly main target-file-name)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file target-file-name io:write-mode)))
    (compiler:x86_64-compile-data f data-graph)
    (io:write-string f "# Procedures\n")
    (io:write-string f "  .text\n")
    (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
      (compiler:x86_64-compile-procedure f procedure-name data-graph))
    (fio:write-to f "# Driver
  .balign 8 #.align 2
  .globl epsilon_main_entry_point
  .type epsilon_main_entry_point, @function
epsilon_main_entry_point:
  jmp ")
    (trivial-compiler:emit-symbol-identifier f main)
    (fio:write-to f "
  .size epsilon_main_entry_point, .-epsilon_main_entry_point\n\n")
    (io:close-file f)))

(e1:define (compiler:x86_64-compile-data f data-graph)
  (trivial-compiler:compile-data f data-graph #t))

(e1:define (compiler:x86_64-compile-procedure f procedure-name data-graph)
  (e1:let* ((formals (state:procedure-get-formals procedure-name))
            (body (state:procedure-get-body procedure-name))
            (procedure (trivial-compiler:compile-procedure procedure-name formals body data-graph)))
    (io:write-string f "\n")
    (io:write-string f "######### ")
    (io:write-symbol f procedure-name)
    (io:write-string f " (mangled as \"")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\")\n")
    (io:write-string f "# io-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-io-no procedure))
    (io:write-string f "\n")
    (io:write-string f "# leaf is ")
    (io:write-string f (e1:if (trivial-compiler:procedure-get-leaf procedure) "#t" "#f"))
    (io:write-string f "\n")
    (io:write-string f "# local-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-local-no procedure))
    (io:write-string f "\n")
    (io:write-string f "# scratch-no is ")
    (io:write-fixnum f (trivial-compiler:procedure-get-scratch-no procedure))
    (io:write-string f "
  .balign 8 #.align 2
  .globl ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    ;;(io:write-string f "\n  .ent ")
    ;;(trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\n  .type ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", @function\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ":\n")
    (io:write-string f "  #[...]\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "_TAIL:\n")
    (io:write-string f "################ BEGIN\n")
    (compiler:x86_64-compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "################ END\n")
    ;;(io:write-string f "  .end ")
    ;;(trivial-compiler:emit-symbol-identifier f procedure-name)
    ;;(io:write-string f "\n")
    (io:write-string f "  .size ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", .-")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\n\n")))

(e1:define (compiler:x86_64-io->stack-index procedure io)
  io)
(e1:define (compiler:x86_64-return-stack-index procedure)
  (trivial-compiler:procedure-get-io-no procedure))
;; (e1:define (compiler:x86_64-frame-pointer-stack-index procedure)
;;   (fixnum:1+ (trivial-compiler:procedure-get-io-no procedure)))
(e1:define (compiler:x86_64-local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            local))
(e1:define (compiler:x86_64-scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))
(e1:define (compiler:x86_64-emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 8))
  (io:write-string f "(%rbx)"))
(e1:define (compiler:x86_64-emit-load-value-to-%rax f procedure value)
  (io:write-string f "  movq ")
  (e1:if (e0:primitive whatever:atom? value)
    (e1:begin
      (io:write-string f "$")
      (io:write-fixnum f value)
      (io:write-string f ", %rax\n"))
    (e1:let* ((data-graph (trivial-compiler:procedure-get-data-graph procedure))
              (pointer-hash (data-graph:graph-get-pointer-hash data-graph))
              (symbol-hash (data-graph:graph-get-symbol-hash data-graph))
              (index (unboxed-hash:get pointer-hash value)))
      (io:write-string f "$p")
      (io:write-fixnum f index)
      (io:write-string f ", %rax")
      (e1:when (unboxed-hash:has? symbol-hash value)
        (io:write-string f " # ")
        (io:write-symbol f value))
      (io:write-string f "\n"))))

(e1:define (compiler:x86_64-compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (fio:write-to f "  # return: BEGIN\n")
       (fio:write-to f "  retq $0\n")
       (fio:write-to f "  # return: END\n"))
      ((trivial-compiler:instruction-tail-call name)
       (fio:write-to f "  # tail-call: BEGIN\n")
       (fio:write-to f "  jmp ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f "\n")
       (fio:write-to f "  # tail-call: END\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (fio:write-to f "  # tail-call-indirect: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-local->stack-index procedure local-index))
       (fio:write-to f ", %rax # load symbol address\n")
       (fio:write-to f "  mov 9*8(%rax), %rax # Load native code address from symbol\n")
       (fio:write-to f "  jmp *%rax\n")
       (fio:write-to f "  # tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (fio:write-to f "  # nontail-call: BEGIN\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (compiler:x86_64-scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # pass frame pointer\n")
       (fio:write-to f "  callq ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f "\n")
       (fio:write-to f "  addq $-")
       (fio:write-to f (i (fixnum:* (compiler:x86_64-scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # restore frame pointer\n")
       (fio:write-to f "  # nontail-call: END\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (fio:write-to f "  # nontail-call-indirect: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-local->stack-index procedure local-index))
       (fio:write-to f ", %rax # load symbol address\n")
       (fio:write-to f "  movq 9*8(%rax), %rax # Load native code address from symbol\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (compiler:x86_64-scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # pass frame pointer\n")
       (fio:write-to f "  callq *%rax\n")
       (fio:write-to f "  addq $-")
       (fio:write-to f (i (fixnum:* (compiler:x86_64-scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # restore frame pointer\n")
       (fio:write-to f "  # nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (fio:write-to f "  # get-io: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-io->stack-index procedure io-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-io: END\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (fio:write-to f "  # set-io: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-io->stack-index procedure io-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # set-io: END\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (fio:write-to f "  # get-local: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-local->stack-index procedure local-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-local: END\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (fio:write-to f "  # set-local: BEGIN\n")
       (fio:write-to f "  movq ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-local->stack-index procedure local-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # set-local: END\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       ;;; FIXME: this is correct, but I could generate better code by exploiting global immutability
       (fio:write-to f "  # get-global: BEGIN\n")
       (compiler:x86_64-emit-load-value-to-%rax f procedure global-name)
       (fio:write-to f "  movq 2*8(%rax), %rax # Load global binding from symbol\n")
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-global: END\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;;  (fio:write-to f "  # set-global [FIXME: IMPLEMENT]\n"))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (fio:write-to f "  # get-value: BEGIN\n")
       (compiler:x86_64-emit-load-value-to-%rax f procedure value)
       (fio:write-to f "  movq %rax, ")
       (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-value: END\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (fio:write-to f "  # primitive: BEGIN\n")
       (fio:write-to f "  movq 8*")
       (fio:write-to f (i (state:primitive-get-index name)))
       (fio:write-to f "(%r12), %rax # Load primitive address\n")
       (fio:write-to f "  movq %rbx, %rdi\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (compiler:x86_64-scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rdi # pass the frame pointer\n")
       (fio:write-to f "  callq *%rax\n")
       (fio:write-to f "  # primitive: END\n"))
      ((trivial-compiler:instruction-fork name scratch-index)
       (fio:write-to f "  # fork\n")
       (fio:write-to f "  # [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-join scratch-index)
       (fio:write-to f "  # join\n")
       (fio:write-to f "  # [NOT IMPLEMENTED YET]\n"))
      ((trivial-compiler:instruction-if-in scratch-index values then-instructions else-instructions)
       (fio:write-to f "  # if-in: BEGIN\n")
       (e1:let ((then-label (trivial-compiler:fresh-label "then"))
                (after-label (trivial-compiler:fresh-label "after")))
         (fio:write-to f "  movq ")
         (compiler:x86_64-emit-stack-access f (compiler:x86_64-scratch->stack-index procedure scratch-index))
         (fio:write-to f ", %r11 # load the discriminand\n")
         (e1:dolist (value values)
           (compiler:x86_64-emit-load-value-to-%rax f procedure value)
           (fio:write-to f "  cmpq %r11, %rax\n")
           (fio:write-to f "  je ")
           (fio:write-to f (st then-label))
           (fio:write-to f " # branch if equal\n"))
         ;;(fio:write-to f "  # The next instruction should be an lw, harmless as a delay slot; anyway I want to avoid an assembler warning in case the next command is a multi-instruction macro\n")
         (compiler:x86_64-compile-instructions f procedure else-instructions)
         (fio:write-to f "  jmp ")
         (fio:write-to f (st after-label))
         (fio:write-to f " # skip the \"else\" branch\n")
         (fio:write-to f (st then-label))
         (fio:write-to f ":\n")
         (compiler:x86_64-compile-instructions f procedure then-instructions)
         (fio:write-to f (st after-label))
         (fio:write-to f ":\n"))
       (fio:write-to f "  # if-in: END\n"))
      (_
       (e1:error "impossible")))))


;;;;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define a zero-argument procedure executing the given forms, which
;;; may refer nonlocals.  Return the procedure name.
(e1:define (macroexpand:procedure-name-using-nonlocals forms-as-sexpression-list)
  (e1:let ((procedure-name (symbol:fresh)))
    (state:procedure-set! procedure-name
                          list:nil
                          (macroexpand:expression-using-nonlocals forms-as-sexpression-list))
    procedure-name))

(e1:define (compiler:compile-procedure-to-assembly-with metadata procedure-name assembly-file-name)
  (e1:call-closure (compiler:metadata-get-procedure-closure metadata)
                    procedure-name
                    assembly-file-name))

(e1:define-macro (system:fail-unless-zero1 form)
  `(e1:unless (fixnum:zero? ,form)
     (fio:write "When executing " (se ',form) ":\n")
     (e1:error "failed")))
(e1:define-macro (system:fail-unless-zero . forms)
  (e1:if (sexpression:null? forms)
    '(e1:bundle)
    `(e1:begin
       (system:fail-unless-zero1 ,(sexpression:car forms))
       (system:fail-unless-zero ,@(sexpression:cdr forms)))))

(e1:define (compiler:assemble-with metadata assembly-file-name object-file-name)
  (e1:let ((assemble-command-line
            (string:append (compiler:metadata-get-CCAS metadata)
                           " "
                           (compiler:metadata-get-ASFLAGS metadata)
                           " "
                           "'" assembly-file-name "'"
                           " -o '" object-file-name "'")))
    ;;(fio:write "ASSEMBLE COMMAND LINE: " (st assemble-command-line) "\n")
    (system:fail-unless-zero
      (unix:system assemble-command-line)
      (unix:unlink assembly-file-name))))

(e1:define (compiler:link-with metadata object-file-name executable-file-name)
  (e1:let ((link-command-line
            (string:append (compiler:metadata-get-CCLD metadata)
                           " "
                           (compiler:metadata-get-LDFLAGS metadata)
                           " "
                           (compiler:metadata-get-LIBS metadata)
                           " "
                           "'" object-file-name "'"
                           " "
                           "-o '" executable-file-name "'")))
    ;;(fio:write "LINK COMMAND LINE: " (st link-command-line) "\n")
    (system:fail-unless-zero
      (unix:system link-command-line)
      (unix:unlink object-file-name))))

(e1:define (compiler:compile-procedure-with metadata procedure-name executable-file-name)
  (e1:let ((assembly-file-name (string:append executable-file-name
                                            (compiler:metadata-get-assembly-file-extension metadata)))
           (object-file-name (string:append executable-file-name
                                            (compiler:metadata-get-object-file-extension metadata))))
    (compiler:compile-procedure-to-assembly-with metadata procedure-name assembly-file-name)
    (compiler:assemble-with metadata assembly-file-name object-file-name)
    (compiler:link-with metadata object-file-name executable-file-name)))


;;;;; Native compiler metadata
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
  (e1:cond ((string:equal? configuration:host_cpu "x86_64")
            (e1:define compiler:native compiler:x86_64))
           ((e1:or (string:equal? configuration:host_cpu "mips")
                   (string:equal? configuration:host_cpu "mipsel")
                   (string:equal? configuration:host_cpu "mips64")
                   (string:equal? configuration:host_cpu "mips64el"))
            (e1:define compiler:native compiler:mips))
           (else
            (e1:define compiler:native compiler:c))))


;;;;; Compiler convenience macros, to handle forms rather than a procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (compiler:compile-to-assembly-with metadata assembly-file-name . forms)
  `(compiler:compile-procedure-to-assembly-with ,metadata
                                                (macroexpand:procedure-name-using-nonlocals ',forms)
                                                ,assembly-file-name))

(e1:define-macro (compiler:compile-with metadata executable-file-name . forms)
  `(compiler:compile-procedure-with ,metadata
                                    (macroexpand:procedure-name-using-nonlocals ',forms)
                                    ,executable-file-name))

;;; Native compilation.
(e1:define-macro (compiler:compile-to-assembly executable-file-name . forms)
  `(compiler:compile-to-assembly-with compiler:native ,executable-file-name ,@forms))
(e1:define-macro (compiler:compile executable-file-name . forms)
  `(compiler:compile-with compiler:native ,executable-file-name ,@forms))

;;; Convenience aliases:
(e1:define-macro (e1:compile . stuff)
  `(compiler:compile ,@stuff))
(e1:define-macro (e1:compile-to-assembly . stuff)
  `(compiler:compile-to-assembly ,@stuff))


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
  jsr print_newline
  +print_string string_slot1
  +print_stack 1
  jsr print_newline
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
            (e1:cond ((e1:and is-procedure-name (fixnum:= j 9)) ;; native code slot
                      (trivial-compiler:emit-symbol-identifier f pointer))
                     ;; FIXME: do this when building the pointer map (but visit
                     ;; expressions)
                     ((e1:and is-procedure-name
                              (list:has? (list:list 0 ;; name
                                                    1 ;; global boundedness
                                                    3 ;; procedure formals
                                                    4 ;; procedure body
                                                    5 ;; macro
                                                    6 ;; macro procedure name
                                                    7 ;; primitive descriptor
                                                    8 ;; bytecode
                                                    10) ;; user-defined data
                                         j))
                      (fio:write-to f "omitted"))
                     (else
                      (e1:let* ((element (buffer:get pointer j)))
                        (e1:if (e0:primitive whatever:atom? element)
                          (e1:cond ((fixnum:< element -32768)
                                    (fio:write-to f "out_of_bounds_low"))
                                   ((fixnum:> element 65535);32767)
                                    (fio:write-to f "out_of_bounds_high"))
                                   (else
                                    (fio:write-to f (i element))))
                          (e1:begin
                            (fio:write-to f "p")
                            (fio:write-to f (i (unboxed-hash:get hash element)))))))))
          (fio:write-to f "\n")))))
    (fio:write-to f "
global_data_end:
  !16 0\n"))

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
    (fio:write-to f "  ;+print_string ")
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
           ((fixnum:> n 65535);32767)
            65535);32767)
           (else
            n)))

;;; FIXME: factor, using this more often.
(e1:define (compiler:c64-emit-literal f n)
  (e1:cond ((fixnum:< n -32768)
            (fio:write-to f "out_of_bounds_low"))
           ((fixnum:> n 65535);32767)
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
         (fio:write-to f "  ;+print_string ")
         (trivial-compiler:emit-symbol-identifier f procedure-name)
         (fio:write-to f "_out\n"))
       (fio:write-to f "  +jump_to_stack_16bit "
                     (i (compiler:c64-return-stack-index procedure))
                     " ;; return\n"))
      ((trivial-compiler:instruction-tail-call name)
       (fio:write-to f "  ;+print_string ")
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
       (fio:write-to f "  +literal_to_stack_16bit ")
       (compiler:c64-emit-value f procedure value)
       (fio:write-to f
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

(e1:define (compiler:c64-emit-binary-no-result-primitive f name procedure scratch-index)
  (fio:write-to f "  +" (st name) " "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))))
(e1:define (compiler:c64-emit-ternary-no-result-primitive f name procedure scratch-index)
  (fio:write-to f "  +" (st name) " "
                (i (compiler:c64-scratch->stack-index procedure scratch-index))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:1+ scratch-index)))
                ", "
                (i (compiler:c64-scratch->stack-index procedure (fixnum:+ scratch-index 2)))))

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

    ((buffer:get)
     (compiler:c64-emit-binary-primitive f "buffer_get_stack_16bit" p scratch-index))
    ((buffer:set! buffer:initialize!)
     ;; a ternary primitive with no results.  FIXME: generalize?
     (compiler:c64-emit-ternary-no-result-primitive f "buffer_set_stack_16bit" p scratch-index))

    ((io:load-byte)
     (compiler:c64-emit-unary-primitive f "io_load_byte_16bit" p scratch-index))
    ((io:store-byte!)
     (compiler:c64-emit-binary-no-result-primitive f "io_store_byte_8bit" p scratch-index))

    ((io:standard-output)
     (fio:write-to f "  ;; Return an undefined value for io:standard-output\n"))
    ((io:write-character) ;; FIXME: no file support yet: ignore the first parameter
     (fio:write-to f "  +stack_to_a_8bit "
                   (i (compiler:c64-scratch->stack-index p (fixnum:1+ scratch-index)))
                   "\n")
     (fio:write-to f "  jsr io_write_character"))
    (else
     (fio:write "About the primitive " (sy name) ":\n")
     (e1:error "unsupported primitive")))
  (fio:write-to f " ;; primitive " (sy name) "\n"))


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

(e1:define-macro (ca ;; 64:compile-to-assembly
                  executable-file-name . forms)
  `(compiler:compile-to-assembly-with compiler:c64 ,executable-file-name ,@forms))
(e1:define-macro (c ;; 64:compile
                  executable-file-name . forms)
  `(compiler:compile-with compiler:c64 ,executable-file-name ,@forms))

;;; Scratch version not using closures (hence not depending on memory
;;; primitives or indirect calls).
(e1:define-macro (can assembly-file-name . forms)
  `(e1:begin
     (e1:define (main)
       ,@forms)
     (compiler:compile-procedure-to-assembly-with compiler:c64
                                                  (e1:value main)
                                                  ,assembly-file-name)))
