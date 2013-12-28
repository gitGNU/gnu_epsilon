;;;;; This is -*- epsilon -*- (with very little Scheme).
;;;;; Trivial compiler

;;;;; Copyright (C) 2013 Luca Saiu

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
;; (e1:define (trivial-compiler:compile-join future tail height)
;;   (trivial-compiler:prepend-return-if-needed tail height 1
;;      (list:cons (trivial-compiler:instruction-join procedure-name height)
;;                 (trivial-compiler:compile-expression future #f height))))
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


;; ;;;;; Structured data compilation [FIXME: this is obsolete.  Remove]
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (e1:define (trivial-compiler:unknown-buffer? datum hash)
;;   (e1:and (e0:primitive whatever:buffer? datum)
;;           (e1:not (unboxed-hash:has? hash datum))))

;; (e1:define (trivial-compiler:extract! boxed-list)
;;   (e1:let ((content (box:get boxed-list)))
;;     (box:set! boxed-list (list:tail content))
;;     (list:head content)))

;; (e1:define (trivial-compiler:insert! boxed-list element)
;;   (box:set! boxed-list
;;             (list:cons element (box:get boxed-list))))

;; (e1:define (trivial-compiler:remember-datum-if-needed! datum hash worklist pointers)
;;   (e1:when (trivial-compiler:unknown-buffer? datum hash)
;;     (unboxed-hash:set! hash datum (unboxed-hash:element-no hash))
;;     (trivial-compiler:insert! worklist datum)
;;     (trivial-compiler:insert! pointers datum)
;;     (e1:dotimes (i (e0:primitive buffer:length datum))
;;       (e1:let* ((element (buffer:get datum i)))
;;         (e1:when (trivial-compiler:unknown-buffer? element hash)
;;           (trivial-compiler:insert! worklist element))))))

;; (e1:define (trivial-compiler:compile-datum datum)
;;   (e1:let* ((hash (unboxed-hash:make))
;;             (worklist (box:make (list:list datum)))
;;             (pointers (box:make list:nil)))
;;     (e1:while (e1:not (list:null? (box:get worklist)))
;;       (e1:let ((datum (trivial-compiler:extract! worklist)))
;;         (trivial-compiler:remember-datum-if-needed! datum hash worklist pointers)))
;;     (e0:bundle hash
;;                (list:reverse (box:get pointers)))))


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


;;;;; C backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (c-backend:compile main)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file "temporary-c-driver/w.c" io:write-mode)))
    (io:write-string f "#include \"../../../runtime/runtime.h\"\n\n")
    (c-backend:emit-forward-declarations f data-graph)
    (io:write-string f "\n")
    (c-backend:compile-data f data-graph)
    (io:write-string f "\n")
    (c-backend:compile-procedures f data-graph)
    (io:close-file f)))

(e1:define (c-backend:emit-forward-declarations f data-graph)
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

(e1:define (c-backend:compile-data f data-graph)
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


(e1:define (c-backend:compile-procedures f data-graph)
  (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
    (c-backend:compile-procedure f procedure-name data-graph)))

(e1:define (c-backend:compile-procedure f procedure-name data-graph)
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
    (c-backend:compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "// This should be unreachable\n  return;\n}\n\n")))

(e1:define (c-backend:io->stack-index procedure io)
  io)
(e1:define (c-backend:local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            ;;(e1:if (trivial-compiler:procedure-get-leaf procedure) 0 1) ; return slot
            local))
(e1:define (c-backend:scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            ;;(e1:if (trivial-compiler:procedure-get-leaf procedure) 0 1) ; return slot
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))

(e1:define (c-backend:emit-io f procedure io)
  (io:write-string f "stack[")
  (io:write-fixnum f (c-backend:io->stack-index procedure io))
  (io:write-string f "]"))
(e1:define (c-backend:emit-local f procedure local)
  (io:write-string f "stack[")
  (io:write-fixnum f (c-backend:local->stack-index procedure local))
  (io:write-string f "]"))
(e1:define (c-backend:emit-scratch f procedure scratch)
  (io:write-string f "stack[")
  (io:write-fixnum f (c-backend:scratch->stack-index procedure scratch))
  (io:write-string f "]"))

(e1:define (c-backend:emit-value f procedure value)
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

(e1:define (c-backend:compile-instructions f procedure ii)
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
       (c-backend:emit-local f procedure local-index)
       (io:write-string f "))[9]))(stack);\n")
       (io:write-string f "  return;\n")
       (io:write-string f "// tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (io:write-string f "// nontail-call ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "  ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "(stack + ")
       (io:write-fixnum f (c-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (io:write-string f "// nontail-call-indirect: BEGIN\n")
       (io:write-string f "  ((epsilon_compiled_c_function)(((epsilon_value*)(")
       (c-backend:emit-local f procedure local-index)
       (io:write-string f "))[9]))(stack + ")
       (io:write-fixnum f (c-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n")
       (io:write-string f "// nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (io:write-string f "  ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (c-backend:emit-io f procedure io-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (io:write-string f "  ")
       (c-backend:emit-io f procedure io-index)
       (io:write-string f " = ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (io:write-string f "  ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (c-backend:emit-local f procedure local-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (io:write-string f "  ")
       (c-backend:emit-local f procedure local-index)
       (io:write-string f " = ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       (io:write-string f "  ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (c-backend:emit-value f procedure global-name)
       (io:write-string f "[2];\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;; FIXME: implement if ever needed)
      ((trivial-compiler:instruction-get-value value scratch-index)
       (io:write-string f "  ")
       (c-backend:emit-scratch f procedure scratch-index)
       (io:write-string f " = ")
       (c-backend:emit-value f procedure value)
       (io:write-string f ";\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (io:write-string f "// primitive ") (io:write-symbol f name) (io:write-string f "\n")
       (io:write-string f "  //epsilon_call_c_primitive_by_index(")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f ", stack + ")
       (io:write-fixnum f (c-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f ");\n")
       (io:write-string f "  epsilon_c_primitive_functions[")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f "](stack + ")
       (io:write-fixnum f (c-backend:scratch->stack-index procedure scratch-index))
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
           (c-backend:emit-scratch f procedure scratch-index)
           (io:write-string f ") == ")
           (io:write-fixnum f value)
           (io:write-string f ") goto ")
           (io:write-string f then-label)
           (io:write-string f ";\n"))
         (c-backend:compile-instructions f procedure else-instructions)
         (io:write-string f "  goto ")
         (io:write-string f after-label)
         (io:write-string f ";\n")
         (io:write-string f then-label)
         (io:write-string f ":\n")
         (c-backend:compile-instructions f procedure then-instructions)
         (io:write-string f after-label)
         (io:write-string f ":\n")))
      (_
       (e1:error "impossible")))))


;;;;; MIPS backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (mips-backend:compile main)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file "temporary-c-driver/q.s" io:write-mode)))
    (mips-backend:compile-data f data-graph)
    (io:write-string f "# Procedures\n")
    (io:write-string f "  .text\n")
    (io:write-string f "  .set noreorder\n")
    (io:write-string f "  .set macro #  .set nomacro\n")
    (io:write-string f "  .set nomips16\n\n")
    (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
      (mips-backend:compile-procedure f procedure-name data-graph))
    (io:close-file f)))

(e1:define (mips-backend:compile-data f data-graph)
  (trivial-compiler:compile-data f data-graph #f))

(e1:define (mips-backend:compile-procedure f procedure-name data-graph)
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
    (io:write-fixnum f (fixnum:* 4 (mips-backend:return-stack-index procedure)))
    (io:write-string f "($16) # Save return address\n")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "_TAIL:\n")
    (io:write-string f "################ BEGIN\n")
    (mips-backend:compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "################ END
  .end ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "
  .size ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", .-")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\n\n")))

(e1:define (mips-backend:io->stack-index procedure io)
  io)
(e1:define (mips-backend:return-stack-index procedure)
  (trivial-compiler:procedure-get-io-no procedure))
;; (e1:define (mips-backend:frame-pointer-stack-index procedure)
;;   (fixnum:1+ (trivial-compiler:procedure-get-io-no procedure)))
(e1:define (mips-backend:local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            local))
(e1:define (mips-backend:scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))
(e1:define (mips-backend:emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 4))
  (io:write-string f "($16)"))

(e1:define (mips-backend:emit-load-value-to-$2 f procedure value)
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

(e1:define (mips-backend:compile-instructions f procedure ii)
  (e1:dolist (i (list:reverse ii))
    (e1:match i
      ((trivial-compiler:instruction-return)
       (io:write-string f "  # return: BEGIN\n")
       (io:write-string f "  lw $31, ")
       (mips-backend:emit-stack-access f (mips-backend:return-stack-index procedure))
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
       (mips-backend:emit-stack-access f (mips-backend:return-stack-index procedure))
       (io:write-string f " # pass the return address via $31 (delay slot)\n")
       (io:write-string f "  # tail-call: END\n"))
      ((trivial-compiler:instruction-tail-call-indirect local-index)
       (io:write-string f "  # tail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (mips-backend:emit-stack-access f (mips-backend:local->stack-index procedure local-index))
       (io:write-string f " # load code address\n")
       (io:write-string f "  lw $25, 9*4($25) # Load native code address from symbol\n")
       (io:write-string f "  jr $25\n")
       (io:write-string f "  lw $31, ")
       (mips-backend:emit-stack-access f (mips-backend:return-stack-index procedure))
       (io:write-string f " # delay slot: pass the return address via $31\n")
       (io:write-string f "  # tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (io:write-string f "  # nontail-call: BEGIN\n")
       (io:write-string f "  la $2, ")
       (trivial-compiler:emit-symbol-identifier f name)
       (io:write-string f "\n")
       (io:write-string f "  jalr $2\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (mips-backend:scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (mips-backend:scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # restore frame pointer\n")
       (io:write-string f "  # nontail-call: END\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (io:write-string f "  # nontail-call-indirect: BEGIN\n")
       (io:write-string f "  lw $25, ")
       (mips-backend:emit-stack-access f (mips-backend:local->stack-index procedure local-index))
       (io:write-string f " # load symbol address\n")
       (io:write-string f "  lw $25, 9*4($25) # Load native code address from symbol\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $16, $16, ")
       (io:write-fixnum f (fixnum:* (mips-backend:scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # delay slot: pass frame pointer\n")
       (io:write-string f "  addiu $16, $16, -")
       (io:write-fixnum f (fixnum:* (mips-backend:scratch->stack-index procedure scratch-index) 4))
       (io:write-string f " # restore frame pointer\n")
       (io:write-string f "  # nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (io:write-string f "  # get-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-io: END\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (io:write-string f "  # set-io: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:io->stack-index procedure io-index))
       (io:write-string f "\n")
       (io:write-string f "  # set-io: END\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (io:write-string f "  # get-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-local: END\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (io:write-string f "  # set-local: BEGIN\n")
       (io:write-string f "  lw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:local->stack-index procedure local-index))
       (io:write-string f "\n")
       (io:write-string f "  # set-local: END\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       ;;; FIXME: this is correct, but I could generate better code by exploiting global immutability
       (io:write-string f "  # get-global: BEGIN\n")
       (mips-backend:emit-load-value-to-$2 f procedure global-name)
       (io:write-string f "  lw $2, 2*4($2) # Load global binding from symbol\n")
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-global: END\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;;  (io:write-string f "  # set-global [FIXME: IMPLEMENT]\n"))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (io:write-string f "  # get-value: BEGIN\n")
       (mips-backend:emit-load-value-to-$2 f procedure value)
       (io:write-string f "  sw $2, ")
       (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
       (io:write-string f "\n")
       (io:write-string f "  # get-value: END\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (io:write-string f "  # primitive: BEGIN\n")
       (io:write-string f "  lw $25, (4 * ")
       (io:write-fixnum f (state:primitive-get-index name))
       (io:write-string f ")($22) # Load primitive address\n")
       (io:write-string f "  jalr $25\n")
       (io:write-string f "  addiu $4, $16, ")
       (io:write-fixnum f (fixnum:* (mips-backend:scratch->stack-index procedure scratch-index) 4))
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
         (mips-backend:emit-stack-access f (mips-backend:scratch->stack-index procedure scratch-index))
         (io:write-string f " # load the discriminand\n")
         (e1:dolist (value values)
           (mips-backend:emit-load-value-to-$2 f procedure value)
           (io:write-string f "  beq $3, $2, ")
           (io:write-string f then-label)
           (io:write-string f " # branch if equal\n")
           (io:write-string f "  nop\n"))
         ;;(io:write-string f "  # The next instruction should be an lw, harmless as a delay slot; anyway I want to avoid an assembler warning in case the next command is a multi-instruction macro\n")
         (mips-backend:compile-instructions f procedure else-instructions)
         (io:write-string f "  j ")
         (io:write-string f after-label)
         (io:write-string f " # skip the \"else\" branch\n")
         (io:write-string f "  nop # Delay slot\n")
         (io:write-string f then-label)
         (io:write-string f ":\n")
         (mips-backend:compile-instructions f procedure then-instructions)
         (io:write-string f after-label)
         (io:write-string f ":\n"))
       (io:write-string f "  # if-in: END\n"))
      (_
       (e1:error "impossible")))))


;;;;; x86_64 backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (x86_64-backend:compile main)
  (e1:let* ((data-graph (data-graph:graph-from-compiled-only main))
            (f (io:open-file "temporary-c-driver/q.s" io:write-mode)))
    (x86_64-backend:compile-data f data-graph)
    (io:write-string f "# Procedures\n")
    (io:write-string f "  .text\n")
    (e1:dolist (procedure-name (data-graph:graph-get-procedures data-graph))
      (x86_64-backend:compile-procedure f procedure-name data-graph))
    (io:close-file f)))

(e1:define (x86_64-backend:compile-data f data-graph)
  (trivial-compiler:compile-data f data-graph #t))

(e1:define (x86_64-backend:compile-procedure f procedure-name data-graph)
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
    (x86_64-backend:compile-instructions f procedure (trivial-compiler:procedure-get-instructions procedure))
    (io:write-string f "################ END\n")
    ;;(io:write-string f "  .end ")
    ;;(trivial-compiler:emit-symbol-identifier f procedure-name)
    ;;(io:write-string f "\n")
    (io:write-string f "  .size ")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f ", .-")
    (trivial-compiler:emit-symbol-identifier f procedure-name)
    (io:write-string f "\n\n")))

(e1:define (x86_64-backend:io->stack-index procedure io)
  io)
(e1:define (x86_64-backend:return-stack-index procedure)
  (trivial-compiler:procedure-get-io-no procedure))
;; (e1:define (x86_64-backend:frame-pointer-stack-index procedure)
;;   (fixnum:1+ (trivial-compiler:procedure-get-io-no procedure)))
(e1:define (x86_64-backend:local->stack-index procedure local)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            local))
(e1:define (x86_64-backend:scratch->stack-index procedure scratch)
  (fixnum:+ (trivial-compiler:procedure-get-io-no procedure)
            1 ;; saved return address
            ;;1 ;; saved frame pointer
            (trivial-compiler:procedure-get-local-no procedure)
            scratch))
(e1:define (x86_64-backend:emit-stack-access f stack-index)
  (io:write-fixnum f (fixnum:* stack-index 8))
  (io:write-string f "(%rbx)"))
(e1:define (x86_64-backend:emit-load-value-to-%rax f procedure value)
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

(e1:define (x86_64-backend:compile-instructions f procedure ii)
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
       (x86_64-backend:emit-stack-access f (x86_64-backend:local->stack-index procedure local-index))
       (fio:write-to f ", %rax # load symbol address\n")
       (fio:write-to f "  mov 9*8(%rax), %rax # Load native code address from symbol\n")
       (fio:write-to f "  jmp *%rax\n")
       (fio:write-to f "  # tail-call-indirect: END\n"))
      ((trivial-compiler:instruction-nontail-call name scratch-index)
       (fio:write-to f "  # nontail-call: BEGIN\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (x86_64-backend:scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # pass frame pointer\n")
       (fio:write-to f "  callq ")
       (trivial-compiler:emit-symbol-identifier f name)
       (fio:write-to f "\n")
       (fio:write-to f "  addq $-")
       (fio:write-to f (i (fixnum:* (x86_64-backend:scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # restore frame pointer\n")
       (fio:write-to f "  # nontail-call: END\n"))
      ((trivial-compiler:instruction-nontail-call-indirect local-index scratch-index)
       (fio:write-to f "  # nontail-call-indirect: BEGIN\n")
       (fio:write-to f "  movq ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:local->stack-index procedure local-index))
       (fio:write-to f ", %rax # load symbol address\n")
       (fio:write-to f "  movq 9*8(%rax), %rax # Load native code address from symbol\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (x86_64-backend:scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # pass frame pointer\n")
       (fio:write-to f "  callq *%rax\n")
       (fio:write-to f "  addq $-")
       (fio:write-to f (i (fixnum:* (x86_64-backend:scratch->stack-index procedure scratch-index) 8)))
       (fio:write-to f ", %rbx # restore frame pointer\n")
       (fio:write-to f "  # nontail-call-indirect: END\n"))
      ((trivial-compiler:instruction-get-io io-index scratch-index)
       (fio:write-to f "  # get-io: BEGIN\n")
       (fio:write-to f "  movq ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:io->stack-index procedure io-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-io: END\n"))
      ((trivial-compiler:instruction-set-io scratch-index io-index)
       (fio:write-to f "  # set-io: BEGIN\n")
       (fio:write-to f "  movq ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:io->stack-index procedure io-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # set-io: END\n"))
      ((trivial-compiler:instruction-get-local local-index scratch-index)
       (fio:write-to f "  # get-local: BEGIN\n")
       (fio:write-to f "  movq ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:local->stack-index procedure local-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-local: END\n"))
      ((trivial-compiler:instruction-set-local scratch-index local-index)
       (fio:write-to f "  # set-local: BEGIN\n")
       (fio:write-to f "  movq ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f ", %rax\n")
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:local->stack-index procedure local-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # set-local: END\n"))
      ((trivial-compiler:instruction-get-global global-name scratch-index)
       ;;; FIXME: this is correct, but I could generate better code by exploiting global immutability
       (fio:write-to f "  # get-global: BEGIN\n")
       (x86_64-backend:emit-load-value-to-%rax f procedure global-name)
       (fio:write-to f "  movq 2*8(%rax), %rax # Load global binding from symbol\n")
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-global: END\n"))
      ;; ((trivial-compiler:instruction-set-global scratch-index global-name)
      ;;  (fio:write-to f "  # set-global [FIXME: IMPLEMENT]\n"))
      ((trivial-compiler:instruction-get-value value scratch-index)
       (fio:write-to f "  # get-value: BEGIN\n")
       (x86_64-backend:emit-load-value-to-%rax f procedure value)
       (fio:write-to f "  movq %rax, ")
       (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
       (fio:write-to f "\n")
       (fio:write-to f "  # get-value: END\n"))
      ((trivial-compiler:instruction-primitive name scratch-index)
       (fio:write-to f "  # primitive: BEGIN\n")
       (fio:write-to f "  movq 8*")
       (fio:write-to f (i (state:primitive-get-index name)))
       (fio:write-to f "(%r12), %rax # Load primitive address\n")
       (fio:write-to f "  movq %rbx, %rdi\n")
       (fio:write-to f "  addq $")
       (fio:write-to f (i (fixnum:* (x86_64-backend:scratch->stack-index procedure scratch-index) 8)))
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
         (x86_64-backend:emit-stack-access f (x86_64-backend:scratch->stack-index procedure scratch-index))
         (fio:write-to f ", %r11 # load the discriminand\n")
         (e1:dolist (value values)
           (x86_64-backend:emit-load-value-to-%rax f procedure value)
           (fio:write-to f "  cmpq %r11, %rax\n")
           (fio:write-to f "  je ")
           (fio:write-to f (st then-label))
           (fio:write-to f " # branch if equal\n"))
         ;;(fio:write-to f "  # The next instruction should be an lw, harmless as a delay slot; anyway I want to avoid an assembler warning in case the next command is a multi-instruction macro\n")
         (x86_64-backend:compile-instructions f procedure else-instructions)
         (fio:write-to f "  jmp ")
         (fio:write-to f (st after-label))
         (fio:write-to f " # skip the \"else\" branch\n")
         (fio:write-to f (st then-label))
         (fio:write-to f ":\n")
         (x86_64-backend:compile-instructions f procedure then-instructions)
         (fio:write-to f (st after-label))
         (fio:write-to f ":\n"))
       (fio:write-to f "  # if-in: END\n"))
      (_
       (e1:error "impossible")))))


;;;;; Peano naturals; for GC benchmarks [FIXME: move somewhere else]
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


;;
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


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sharealot n)
  (e1:if (fixnum:zero? n)
    0
    (e1:let* ((subtree (sharealot (fixnum:1- n))))
      (tuple:make subtree subtree))))

(e1:define (dontshare n)
  (e1:if (fixnum:zero? n)
    0
    (tuple:make (dontshare (fixnum:1- n))
                (dontshare (fixnum:1- n)))))

(e1:define (complexity q)
  (e1:if (fixnum:zero? q)
    1
    (fixnum:+ (complexity (tuple:get q 0))
              (complexity (tuple:get q 0))
              1)))

(e1:define (complexityp q)
  (e0:if-in q (0)
    1
    (e0:primitive fixnum:1+
                  (e0:primitive fixnum:+
                                (complexityp (e0:primitive buffer:get q 0))
                                (complexityp (e0:primitive buffer:get q 0))))))

(e1:define global-datum
  ;;(list:iota 4)
  symbol:table
  )

(define-macro (testc main)
  `(e1:toplevel (c-backend:compile (e0:value ,main))))
(define-macro (testm main)
  `(e1:toplevel (mips-backend:compile (e0:value ,main))))
(define-macro (testx main)
  `(e1:toplevel (x86_64-backend:compile (e0:value ,main))))
;;(test fact fact-acc fibo zerotozero zerotoone onetozero zerototwo twotozero simplertwotozero average call-indirect-1 call-indirect-2)
;;(define (go) (test f fibo fixnum:+ fixnum:- fixnum:1+ fixnum:1- fixnum:<))
;; (e1:define (f)
;;   (fixnum:write (list:length (list:iota 1000)))
;;   (string:write "\n"))
(e1:define (f)
  (e1:dotimes (i 3000)
    (list:length (list:iota 1000)))
  (fio:write-to (io:standard-output) "Done\n"))

(e1:define (g)
  (e1:dotimes (i 40)
    (fio:write (i i) " -> " (i (fibo i)) "\n")))

(define (go)
  (e1:toplevel (mips-backend:compile symbol:table
                                     (state:procedure-names))))
(define (z)
  (gc)
  (benchmark (e1:toplevel (e0:let (a b c d e) (data-graph:graph-explode (data-graph:graph-from-compiled-only (e0:value q))) (e0:bundle (list:length a) (list:length b) (list:length c))))))
