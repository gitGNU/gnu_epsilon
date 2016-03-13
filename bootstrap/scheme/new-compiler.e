;;;;; This is -*- epsilon -*-.
;;;;; New compiler

;;;;; Copyright (C) 2016 Luca Saiu

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


;;;;; Administrative Normal Form (ANF) for epsilon0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: don't fail for underdimensioned expressions.  The problem is with
;; code such as (list:head variables).

(e1:define-sum anf:expression
  (let-undefined bound body)
  (let-value bound content body)
  (let-global bound global-name body)
  (let-primitive bounds primitive-name actuals body)
  (let-call bounds procedure-name actuals body)
  (let-call-indirect bounds procedure actuals body)
  (let-if-in bounds discriminand values then-expression else-expression body)
  ;; (let bounds variables body) ;; This doesn't actually exist: see anf:expression-let .
  (set-let-if-results target-variables source-variables) ;; this is the leaf of then and else branches in let-if
  (return variables)
  (tail-if-in discriminand values then-expression else-expression)
  (tail-call procedure-name actuals)
  (tail-call-indirect procedure actuals))

(e1:define (anf:write-variables port variables)
  (e1:unless (list:null? variables)
    (fio:write-to port (sy (list:head variables)))
    (e1:let ((rest (list:tail variables)))
      (e1:unless (list:null? rest)
        (fio:write-to port ", ")
        (anf:write-variables port rest)))))
(e1:define (anf:write-values port values)
  (e1:unless (list:null? values)
    (fio:write-to port (i (list:head values)))
    (e1:let ((rest (list:tail values)))
      (e1:unless (list:null? rest)
        (fio:write-to port ", ")
        (anf:write-values port rest)))))
(e1:define (anf:write-anf-expression port e)
  (e1:match e
    ((anf:expression-let-undefined bound body)
     (fio:write-to port "[let-undefined " (sy bound) " be undefined in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-value bound content body)
     (fio:write-to port "[let-value " (sy bound) " be " (i content) " in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-global bound global-name body)
     (fio:write-to port "[let-global " (sy bound) " be " (sy global-name) " in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-primitive bounds primitive-name actuals body)
     (fio:write-to port "[let-primitive ")
     (anf:write-variables port bounds)
     (fio:write-to port " be [primitive " (sy primitive-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "] in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-call bounds procedure-name actuals body)
     (fio:write-to port "[let-call ")
     (anf:write-variables port bounds)
     (fio:write-to port " be [call " (sy procedure-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "] in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-call-indirect bounds procedure actuals body)
     (fio:write-to port "[let-call-indirect ")
     (anf:write-variables port bounds)
     (fio:write-to port " be [call-indirect " (sy procedure) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "] in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-let-if-in bounds discriminand values then-expression else-expression body)
     (fio:write-to port "[let-if-in ")
     (anf:write-variables port bounds)
     (fio:write-to port " be [if " (sy discriminand) " in [")
     (anf:write-values port values)
     (fio:write-to port "] then ")
     (anf:write-anf-expression port then-expression)
     (fio:write-to port " else ")
     (anf:write-anf-expression port else-expression)
     (fio:write-to port "] in ")
     (anf:write-anf-expression port body)
     (fio:write-to port "]"))
    ((anf:expression-return variables)
     (fio:write-to port "[return ")
     (anf:write-variables port variables)
     (fio:write-to port "]"))
    ((anf:expression-tail-if-in discriminand values then-expression else-expression)
     (fio:write-to port "[tail-if-in " (sy discriminand) " in [")
     (anf:write-values port values)
     (fio:write-to port "] then ")
     (anf:write-anf-expression port then-expression)
     (fio:write-to port " else ")
     (anf:write-anf-expression port else-expression)
     (fio:write-to port "]"))
    ((anf:expression-tail-call procedure-name actuals)
     (fio:write-to port "[tail-call " (sy procedure-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((anf:expression-tail-call-indirect procedure actuals)
     (fio:write-to port "[tail-call-indirect " (sy procedure) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((anf:expression-set-let-if-results target-variables source-variables)
     (fio:write-to port "[set-let-if-results [")
     (anf:write-variables port target-variables)
     (fio:write-to port "] [")
     (anf:write-variables port source-variables)
     (fio:write-to port "]]"))
    (else
     (fio:write-to port "<invalid or unimplemented>"))))

;;; It would be convenient to have an ANF form (anf:expression-let bounds
;;; variables body) when generating code, but the renaming it would introduce
;;; would be useless in code generation, and would make debugging more
;;; difficult.  So, instead of adding an ANF syntax case, we add a procedure
;;; which rewrites away the new bindings and returns the form in rewritten form.
(e1:define (anf:expression-let bounds variables body)
  (e1:cond ((e1:and (list:null? bounds)
                    (list:null? variables))
            body)
           ((list:null? variables) ;; more bounds than variables
            (anf:expression-let-undefined
               (list:head bounds)
               (anf:expression-let (list:tail bounds) list:nil body)))
           ((list:null? bounds) ;; more variables than bounds
            body)
           (else
            (anf:replace (list:head bounds)
                         (list:head variables)
                         (anf:expression-let (list:tail bounds)
                                             (list:tail variables)
                                             body)))))
(e1:define (anf:replace-variable to-replace replacement variable)
  (e1:if (whatever:eq? variable to-replace)
    replacement
    variable))
(e1:define (anf:replace-in-list to-replace replacement variables)
  (e1:if (list:null? variables)
    list:nil
    (list:cons (anf:replace-variable to-replace replacement (list:head variables))
               (anf:replace-in-list to-replace replacement (list:tail variables)))))
(e1:define (anf:replace to-replace replacement e)
  (e1:match e
    ((anf:expression-let-undefined bound body)
     (anf:expression-let-undefined bound
                                   (anf:replace to-replace replacement body)))
    ((anf:expression-let-value bound content body)
     (anf:expression-let-value bound
                               content
                               (anf:replace to-replace replacement body)))
    ((anf:expression-let-global bound global-name body)
     (anf:expression-let-global bound
                                global-name
                                (anf:replace to-replace replacement body)))
    ((anf:expression-let-primitive bounds primitive-name actuals body)
     (anf:expression-let-primitive bounds
                                   primitive-name
                                   (anf:replace-in-list to-replace replacement actuals)
                                   (anf:replace to-replace replacement body)))
    ((anf:expression-let-call bounds procedure-name actuals body)
     (anf:expression-let-call bounds
                              procedure-name
                              (anf:replace-in-list to-replace replacement actuals)
                              (anf:replace to-replace replacement body)))
    ((anf:expression-let-call-indirect bounds procedure actuals body)
     (anf:expression-let-call-indirect bounds
                                       procedure
                                       (anf:replace-in-list to-replace replacement actuals)
                                       (anf:replace to-replace replacement body)))
    ((anf:expression-let-if-in bounds discriminand values then-expression else-expression body)
     (anf:expression-let-if-in bounds
                               (anf:replace-variable to-replace replacement discriminand)
                               values
                               (anf:replace to-replace replacement then-expression)
                               (anf:replace to-replace replacement else-expression)
                               (anf:replace to-replace replacement body)))
    ((anf:expression-return variables)
     (anf:expression-return (anf:replace-in-list to-replace replacement variables)))
    ((anf:expression-tail-if-in discriminand values then-expression else-expression)
     (anf:expression-tail-if-in (anf:replace-variable to-replace replacement discriminand)
                                values
                                (anf:replace to-replace replacement then-expression)
                                (anf:replace to-replace replacement else-expression)))
    ((anf:expression-tail-call procedure-name actuals)
     (anf:expression-tail-call procedure-name
                               (anf:replace-in-list to-replace replacement actuals)))
    ((anf:expression-tail-call-indirect procedure actuals)
     (anf:expression-tail-call-indirect (anf:replace-variable to-replace replacement procedure)
                                        (anf:replace-in-list to-replace replacement actuals)))
    ((anf:expression-set-let-if-results target-variables source-variables)
     (anf:expression-set-let-if-results target-variables
                                        (anf:replace-in-list to-replace
                                                             replacement
                                                             source-variables)))
    (else
     (e1:error "anf:replace: invalid case or unimplemented"))))

;;; Flanagan, Sabry, Duba, Felleisen. "The Essence of Compiling with
;;; Continuations", Proceedings ACM SIGPLAN 1993 Conf. on Programming Language
;;; Design and Implementation, 1993.  The interesting transformation is in
;;; Figure 9, page 10.

(e1:define anf:tail-k
  (e1:lambda (variables)
    (anf:expression-return variables)))
(e1:define (anf:convert-tail e bound-variables)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _)
         (e0:expression-join _ _)
         (e0:expression-fork _ _ _))
     (anf:convert-nontail e
                          bound-variables
                          1
                          anf:tail-k))
    ((e0:expression-bundle _ items)
     (anf:convert-nontail e
                          bound-variables
                          (list:length items)
                          anf:tail-k))
    ((e0:expression-primitive _ primitive-name _)
     (anf:convert-nontail e
                          bound-variables
                          (state:primitive-get-out-dimension primitive-name)
                          anf:tail-k))
    ((e0:expression-let _ let-bound-variables bound-expression body)
     (anf:convert-nontail bound-expression
                          bound-variables
                          (list:length let-bound-variables)
                          (e1:lambda (variables)
                            (e1:let ((new-bounds (list:append-reversed let-bound-variables
                                                                       bound-variables)))
                              (anf:expression-let let-bound-variables
                                                  variables
                                                  (anf:convert-tail body new-bounds))))))
    ((e0:expression-call _ procedure-name actuals)
     (anf:convert-nontails actuals
                           bound-variables
                           (e1:lambda (actual-variables)
                             (anf:expression-tail-call procedure-name actual-variables))))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (anf:convert-nontail
         procedure-expression
         bound-variables
         1
         (e1:lambda (procedure-variables)
           (e1:let ((procedure-variable (list:head procedure-variables)))
             (anf:convert-nontails actuals
                                   bound-variables
                                   (e1:lambda (actual-variables)
                                     (anf:expression-tail-call-indirect procedure-variable
                                                                        actual-variables)))))))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (anf:convert-nontail discriminand
                          bound-variables
                          1
                          (e1:lambda (discriminand-variables)
                            (anf:expression-tail-if-in (list:head discriminand-variables)
                                                       values
                                                       (anf:convert-tail then-branch
                                                                         bound-variables)
                                                       (anf:convert-tail else-branch
                                                                         bound-variables)))))
    (else
     (e1:error "ANF convertion, tail: invalid expression"))))

(e1:define (anf:convert-nontail e bound-variables out-dimension k)
  (e1:match e
    ((e0:expression-variable _ name)
     (e1:if (list:has? bound-variables name)
       (e1:call-closure k (list:list name))
       (e1:let ((local-name (symbol:fresh-with-prefix "g")))
         (anf:expression-let-global local-name name (e1:call-closure k (list:list local-name))))))
    ((e0:expression-value _ content)
     (e1:let ((name (symbol:fresh-with-prefix "l")))
       (anf:expression-let-value name content (e1:call-closure k (list:list name)))))
    ((e0:expression-bundle _ items)
     (anf:convert-nontails items bound-variables k))
    ((e0:expression-primitive _ name actuals)
     (e1:let* ((result-no (state:primitive-get-out-dimension name))
               (result-names (symbol:fresh-symbols-with-prefix result-no "pri")))
       (anf:convert-nontails actuals
                             bound-variables
                             (e1:lambda (actual-variables)
                               (anf:expression-let-primitive result-names
                                                             name
                                                             actual-variables
                                                             (e1:call-closure k result-names))))))
    ((e0:expression-let _ let-bound-variables bound-expression body)
     (anf:convert-nontail bound-expression
                          bound-variables
                          (list:length let-bound-variables)
                          (e1:lambda (variables)
                            (e1:let ((new-bounds (list:append-reversed let-bound-variables
                                                                       bound-variables)))
                              (anf:expression-let let-bound-variables
                                                  variables
                                                  (anf:convert-nontail body
                                                                       new-bounds
                                                                       out-dimension
                                                                       k))))))
    ((e0:expression-call _ procedure-name actuals)
     (anf:convert-nontails actuals
                           bound-variables
                           (e1:lambda (parameter-variables)
                             (e1:let ((result-variables
                                       (symbol:fresh-symbols-with-prefix out-dimension
                                                                         "prr")))
                               (anf:expression-let-call result-variables
                                                        procedure-name
                                                        parameter-variables
                                                        (e1:call-closure k result-variables))))))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e1:let ((procedure-variable (symbol:fresh-with-prefix "prn")))
       (anf:convert-nontail procedure-expression
                            bound-variables
                            1
                            (e1:lambda (procedure-variables)
                              (anf:convert-nontails
                                  actuals
                                  bound-variables
                                  (e1:lambda (parameter-variables)
                                    (e1:let ((result-variables
                                              (symbol:fresh-symbols-with-prefix
                                                 out-dimension "prr")))
                                      (anf:expression-let-call-indirect
                                          result-variables
                                          (list:head procedure-variables)
                                          parameter-variables
                                          (e1:call-closure k result-variables)))))))))
    ((e0:expression-if-in _ discriminand values then-expression else-expression)
     (e1:let ((discriminand-variable (symbol:fresh-with-prefix "d"))
              (result-variables (symbol:fresh-symbols-with-prefix out-dimension
                                                                  "c")))
       (anf:convert-nontail
          discriminand
          bound-variables
          1
          (e1:lambda (discriminand-variables)
            (anf:expression-let-if-in
             result-variables
             (list:head discriminand-variables)
             values
             (anf:convert-nontail then-expression
                                  bound-variables
                                  out-dimension
                                  (e1:lambda (variables)
                                    (anf:expression-set-let-if-results result-variables
                                                                       variables)))
             (anf:convert-nontail else-expression
                                  bound-variables
                                  out-dimension
                                  (e1:lambda (variables)
                                    (anf:expression-set-let-if-results result-variables
                                                                       variables)))
             (e1:call-closure k result-variables))))))
    ((e0:expression-fork _ procedure-name actuals)
     (e1:error "fork (ANF conversion, nontail): unimplemented"))
    ((e0:expression-join _ future)
     (e1:error "join (ANF conversion, nontail): unimplemented"))
    (else
     (e1:error "ANF convertion, nontail: invalid expression"))))

(e1:define (anf:convert-nontails es bound-variables k)
  (e1:let ((variables-box (box:make list:nil)))
    (anf:convert-nontails-helper es bound-variables k variables-box)))
(e1:define (anf:convert-nontails-helper es bound-variables k box)
  (e1:if (list:null? es)
    (e1:call-closure k (list:reverse (box:get box)))
    (anf:convert-nontail (list:head es)
                         bound-variables
                         1
                         (e1:lambda (variables)
                           (box:set! box (list:cons (list:head variables)
                                                    (box:get box)))
                           (anf:convert-nontails-helper (list:tail es)
                                                        bound-variables
                                                        k
                                                        box)))))


;;;;; Control flow graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: comment about why this is almost SSA.  Cite
;;; "Fast Liveness Checking for SSA-Form Programs", Benoit Boissinot , Sebastian
;;; Hack , Benoît Dupont De Dinechin , Daniel Grund , Fabrice Rastello, CGO '08
;;; Proceedings of the 6th annual IEEE/ACM international symposium on Code
;;; generation and optimization, 2008

;;; The control flow graph doesn't need to distinguish conditional jumps at the
;;; edge level; conditional destination states are stored in the if instruction,
;;; so they can still be distinguished if needed.
(e1:define-record dataflow:graph
  begin-id
  end-id
  states        ;; unboxed hash: state id -> state
  successors    ;; unboxed hash: state id -> <next state id> list
  predecessors) ;; unboxed hash: state id -> <previous state id> list

(e1:define-record dataflow:state
  in-lives   ;; the set-of-list of live variables before the instruction
  instruction
  out-lives) ;; the set-of-list of live variables after the instruction

(e1:define-sum dataflow:instruction
  (begin)
  (end)
  (return variables)
  (literal bound content)
  (undefined bound)
  (global bound content)
  (if discriminand values then else)
  (pre-phi bounds sources)
  (phi bounds sources-list) ;; FIXME: remove
  (primitive bounds primitive-name actuals)
  (nontail-call bounds procedure-name actuals)
  (nontail-call-indirect bounds procedure actuals)
  (tail-call procedure-name actuals)
  (tail-call-indirect procedure actuals))

(e1:define (dataflow:write-instruction port instruction)
  (e1:match instruction
    ((dataflow:instruction-begin)
     (fio:write-to port "begin"))
    ((dataflow:instruction-end)
     (fio:write-to port "end"))
    ((dataflow:instruction-return variables)
     (fio:write-to port "return [")
     (anf:write-variables port variables)
     (fio:write-to port "]"))
    ((dataflow:instruction-literal bound content)
     (fio:write-to port (sy bound) " := literal " (i content)))
    ((dataflow:instruction-undefined bound)
     (fio:write-to port (sy bound) " := undefined"))
    ((dataflow:instruction-global bound global-name)
     (fio:write-to port (sy bound) " := [global " (sy global-name) "]"))
    ((dataflow:instruction-if discriminand values then else)
     (fio:write-to port "if " (sy discriminand) " &#x220A; {")
     (anf:write-values port values)
     (fio:write-to port "} goto " (i then) " else " (i else)))
    ((dataflow:instruction-pre-phi bounds sources)
     (fio:write-to port "pre-&phi; [")
     (anf:write-variables port bounds)
     (fio:write-to port "] := [")
     (anf:write-variables port sources)
     (fio:write-to port "]"))
    ((dataflow:instruction-phi bounds sources-list)
     (fio:write-to port "[")
     (anf:write-variables port bounds)
     (fio:write-to port "] := [&phi; ...]"))
    ((dataflow:instruction-primitive bounds primitive-name actuals)
     (fio:write-to port "[")
     (anf:write-variables port bounds)
     (fio:write-to port "] := [&pi; " (sy primitive-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((dataflow:instruction-nontail-call bounds procedure-name actuals)
     (fio:write-to port "[")
     (anf:write-variables port bounds)
     (fio:write-to port "] := [call " (sy procedure-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((dataflow:instruction-nontail-call-indirect bounds procedure actuals)
     (fio:write-to port "[")
     (anf:write-variables port bounds)
     (fio:write-to port "] := [call-indirect " (sy procedure) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((dataflow:instruction-tail-call procedure-name actuals)
     (fio:write-to port "[tail-call " (sy procedure-name) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    ((dataflow:instruction-tail-call-indirect procedure actuals)
     (fio:write-to port "[tail-call-indirect " (sy procedure) " ")
     (anf:write-variables port actuals)
     (fio:write-to port "]"))
    (else
     (fio:write-to port "#<instruction>"))))

(e1:define (dataflow:instruction->defineds-useds instruction)
  (e1:match instruction
    ((or (dataflow:instruction-begin)
         (dataflow:instruction-end))
     (e1:bundle () ()))
    ((dataflow:instruction-return variables)
     (e1:bundle () variables))
    ((dataflow:instruction-literal bound content)
     (e1:bundle (list:list bound) ()))
    ((dataflow:instruction-undefined bound)
     (e1:bundle (list:list bound) ()))
    ((dataflow:instruction-global bound global-name)
     (e1:bundle (list:list bound) ()))
    ((dataflow:instruction-if discriminand values then else)
     (e1:bundle () (list:list discriminand)))
    ((dataflow:instruction-pre-phi bounds sources)
     (e1:bundle bounds sources))
    ((dataflow:instruction-phi bounds sources-list)
     (e1:error "this shouldn't be used any longer"))
    ((dataflow:instruction-primitive bounds primitive-name actuals)
     (e1:bundle bounds actuals))
    ((dataflow:instruction-nontail-call bounds procedure-name actuals)
     (e1:bundle bounds actuals))
    ((dataflow:instruction-nontail-call-indirect bounds procedure actuals)
     (e1:bundle bounds (list:cons procedure actuals)))
    ((dataflow:instruction-tail-call procedure-name actuals)
     (e1:bundle () actuals))
    ((dataflow:instruction-tail-call-indirect procedure actuals)
     (e1:bundle () (list:cons procedure actuals)))
    (else
     (e1:error "dataflow:instruction->defineds-useds: invalid or unimplemented case"))))

(e1:define (dataflow:make-graph)
  (e1:let* ((states (unboxed-hash:make))
            (begin-id (dataflow:states-add-state!
                          states
                          (dataflow:state set-as-list:empty
                                          (dataflow:instruction-begin)
                                          set-as-list:empty)))
            (end-id (dataflow:states-add-state!
                       states
                       (dataflow:state set-as-list:empty
                                       (dataflow:instruction-end)
                                       set-as-list:empty))))
    (dataflow:graph begin-id
                    end-id
                    states
                    (unboxed-hash:make)
                    (unboxed-hash:make))))

(e1:define (dataflow:states-next-state states)
  (hash:element-no states))

(e1:define (dataflow:graph-state-no graph)
  (e1:let ((states (dataflow:graph-get-states graph)))
    (hash:element-no states)))

(e1:define (dataflow:graph-next-state graph)
  (dataflow:graph-state-no graph))

(e1:define (dataflow:graph-predecessor-state-ids graph state-id)
  (e1:let ((predecessors (dataflow:graph-get-predecessors graph)))
    (e1:if (unboxed-hash:has? predecessors state-id)
      (unboxed-hash:get predecessors state-id)
      list:nil)))
(e1:define (dataflow:graph-successor-state-ids graph state-id)
  (e1:let ((successors (dataflow:graph-get-successors graph)))
    (e1:if (unboxed-hash:has? successors state-id)
      (unboxed-hash:get successors state-id)
      list:nil)))

(e1:define (dataflow:predecessor-no graph state-id)
  (list:length (dataflow:graph-predecessor-state-ids graph state-id)))
(e1:define (dataflow:successor-no graph state-id)
  (list:length (dataflow:graph-successor-state-ids graph state-id)))

(e1:define (dataflow:graph-get-state graph state-id)
  (e1:let ((states (dataflow:graph-get-states graph)))
    (unboxed-hash:get states state-id)))

(e1:define (dataflow:graph->state-ids graph)
  (e1:let* ((states (dataflow:graph-get-states graph))
            (alist (unboxed-hash:unboxed-hash->alist states)))
    (alist:keys alist)))

(e1:define (dataflow:graph-has-state-id? graph state-id)
  (e1:let ((states (dataflow:graph-get-states graph)))
    (unboxed-hash:has? states state-id)))

(e1:define (dataflow:states-add-state! states state)
  (e1:let ((next-id (dataflow:states-next-state states)))
    (unboxed-hash:set! states next-id state)
    next-id))

(e1:define (dataflow:states-remove-state! states state-id)
  (unboxed-hash:unset! states state-id))

(e1:define (dataflow:graph-add-instruction! graph instruction)
  (e1:let* ((states (dataflow:graph-get-states graph))
            (state (dataflow:state set-as-list:empty
                                   instruction
                                   set-as-list:empty))
            (state-id (dataflow:states-add-state! states state)))
    state-id))

(e1:define (dataflow:graph-add-instruction-after! graph instruction predecessor-id)
  (e1:let ((state-id (dataflow:graph-add-instruction! graph instruction)))
    (dataflow:graph-add-state-edge! graph predecessor-id state-id)
    state-id))

;;; Remove the given state from the given graph, also removing every edge
;;; involving the state.
(e1:define (dataflow:graph-remove-state! graph state-id)
  (e1:let ((states (dataflow:graph-get-states graph))
           (predecessors (dataflow:graph-predecessor-state-ids graph state-id))
           (successors (dataflow:graph-successor-state-ids graph state-id)))
    (e1:dolist (predecessor-id predecessors)
      (dataflow:graph-remove-state-edge! graph predecessor-id state-id))
    (e1:dolist (successor-id successors)
      (dataflow:graph-remove-state-edge! graph state-id successor-id))
    (dataflow:states-remove-state! states state-id)
    state-id))

;;; Remove the given state from the given graph, so that all its predecessors
;;; point to its successor.  Notice that this only makes sense when the
;;; successor is unique: this procedure cannot be applied to an if node having
;;; two distinct successors (instructions such as "if x in {0} then 10 else 10"
;;; can be bypassed, since they only have one successor) -- however the
;;; predecessors and successor are allowed to be an if node, and where needed
;;; the predecessor instructions are updated to contain the new jump
;;; destination.
(e1:define (dataflow:graph-bypass-state! graph state-id)
  (e1:let* ((states (dataflow:graph-get-states graph))
            (predecessors (dataflow:graph-predecessor-state-ids graph state-id))
            (successors (dataflow:graph-successor-state-ids graph state-id)))
    (e1:require (fixnum:= (list:length successors) 1))
    (e1:let ((successor-id (list:head successors)))
      (dataflow:graph-remove-state! graph state-id)
      (e1:dolist (predecessor-id predecessors)
        (dataflow:graph-add-state-edge! graph predecessor-id successor-id)
        (e1:let* ((predecessor-state (unboxed-hash:get states predecessor-id))
                  (predecessor-instruction (dataflow:state-get-instruction predecessor-state)))
          (e1:match predecessor-instruction
            ((dataflow:instruction-if _ _ then else)
             (e1:when (fixnum:= then state-id)
               (dataflow:instruction-if-set-then! predecessor-instruction successor-id))
             (e1:when (fixnum:= else state-id)
               (dataflow:instruction-if-set-else! predecessor-instruction successor-id)))
            (else)))))))

(e1:define (dataflow:graph-add-state-edge! graph from-id to-id)
  (e1:let ((successors (dataflow:graph-get-successors graph))
           (predecessors (dataflow:graph-get-predecessors graph)))
    (e1:let* ((from-successors (dataflow:graph-successor-state-ids graph from-id))
              (to-predecessors (dataflow:graph-predecessor-state-ids graph to-id))
              (new-from-successors (set-as-list:with from-successors to-id))
              (new-to-predecessors (set-as-list:with to-predecessors from-id)))
      (unboxed-hash:set! successors from-id new-from-successors)
      (unboxed-hash:set! predecessors to-id new-to-predecessors))))

(e1:define (dataflow:graph-remove-state-edge! graph from-id to-id)
  (e1:let ((successors (dataflow:graph-get-successors graph))
           (predecessors (dataflow:graph-get-predecessors graph)))
    (e1:let* ((from-successors (dataflow:graph-successor-state-ids graph from-id))
              (to-predecessors (dataflow:graph-predecessor-state-ids graph to-id))
              (new-from-successors (set-as-list:without from-successors to-id))
              (new-to-predecessors (set-as-list:without to-predecessors from-id)))
      (unboxed-hash:set! successors from-id new-from-successors)
      (unboxed-hash:set! predecessors to-id new-to-predecessors))))

(e1:define (dataflow:make-state-initial! graph state)
  (dataflow:graph-add-state-edge! graph
                                  (dataflow:graph-get-begin-id graph)
                                  state))

(e1:define (dataflow:make-state-final! graph state)
  (dataflow:graph-add-state-edge! graph
                                  state
                                  (dataflow:graph-get-end-id graph)))

(e1:define (dataflow:add-e0-to-graph! graph e0-expression bounds)
  (e1:let ((anf (anf:convert-tail e0-expression bounds)))
    (dataflow:add-anf-to-graph! graph anf)))

;;; Return the *last* state at the end of the body.
(e1:define (dataflow:graph-add-nontail-instruction-after! graph predecessor-id instruction body)
  (e1:let ((id (dataflow:graph-add-instruction-after!
                   graph
                   instruction
                   predecessor-id)))
    (dataflow:add-anf-to-graph! graph body id)))

(e1:define (dataflow:graph-add-tail-instruction-after! graph predecessor-id instruction)
  (e1:let ((id (dataflow:graph-add-instruction-after!
                   graph
                   instruction
                   predecessor-id)))
    (dataflow:make-state-final! graph id)
    id))

;;; Return the id of the last added state.
(e1:define (dataflow:add-anf-to-graph! graph e predecessor-id)
  ;;(fio:write "[DEBUG:") (anf:write-anf-expression (io:standard-output) e) (fio:write "]\n")
  (e1:match e
    ((anf:expression-let-undefined bound body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-undefined bound)
        body))
    ((anf:expression-let-value bound content body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-literal bound content)
        body))
    ((anf:expression-let-global bound global-name body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-global bound global-name)
        body))
    ((anf:expression-let-primitive bounds primitive-name actuals body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-primitive bounds primitive-name actuals)
        body))
    ((anf:expression-let-call bounds procedure-name actuals body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-nontail-call bounds procedure-name actuals)
        body))
    ((anf:expression-let-call-indirect bounds procedure actuals body)
     (dataflow:graph-add-nontail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-nontail-call-indirect bounds procedure actuals)
        body))
    ((anf:expression-let-if-in bounds discriminand values then-expression else-expression body)
     (e1:let* ((instruction (dataflow:instruction-if discriminand values -1 -1))
               (if-id (dataflow:graph-add-instruction-after! graph
                                                             instruction
                                                             predecessor-id))
               (then-id (dataflow:graph-next-state graph))
               (then-pre-phi-id (dataflow:add-anf-to-graph! graph then-expression if-id))
               (else-id (dataflow:graph-next-state graph))
               (else-pre-phi-id (dataflow:add-anf-to-graph! graph else-expression if-id))
               (next-id (dataflow:graph-next-state graph)))
       (dataflow:instruction-if-set-then! instruction then-id)
       (dataflow:instruction-if-set-else! instruction else-id)
       (dataflow:graph-add-state-edge! graph else-pre-phi-id next-id)
       (dataflow:add-anf-to-graph! graph body then-pre-phi-id)))
    ((anf:expression-return variables)
     (dataflow:graph-add-tail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-return variables)))
    ((anf:expression-tail-if-in discriminand values then-expression else-expression)
     (e1:let* ((instruction (dataflow:instruction-if discriminand values -1 -1))
               (id (dataflow:graph-add-instruction-after! graph
                                                          instruction
                                                          predecessor-id))
               (then-id (dataflow:graph-next-state graph))
               (() (dataflow:add-anf-to-graph! graph then-expression id))
               (else-id (dataflow:graph-next-state graph))
               (() (dataflow:add-anf-to-graph! graph else-expression id)))
       (dataflow:instruction-if-set-then! instruction then-id)
       (dataflow:instruction-if-set-else! instruction else-id)
       -1)) ;; An intentionally invalid state: the result should never be used.
    ((anf:expression-tail-call procedure-name actuals)
     (dataflow:graph-add-tail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-tail-call procedure-name actuals)))
    ((anf:expression-tail-call-indirect procedure actuals)
     (dataflow:graph-add-tail-instruction-after!
        graph
        predecessor-id
        (dataflow:instruction-tail-call-indirect procedure actuals)))
    ((anf:expression-set-let-if-results target-variables source-variables)
     (dataflow:graph-add-instruction-after!
        graph
        (dataflow:instruction-pre-phi target-variables source-variables)
        predecessor-id))
    (else
     (e1:error "dataflow:add-e0-to-graph!: invalid case or unimplemented"))))


;;;;; Liveness analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Control flow graphs derived from epsilon0 expression are acyclic, but I'm
;;; not currently exploiting this property to simplify the code.  I would be
;;; able to do one pass only to propagate constraints, visiting edges backwards,
;;; breadth first; but right now this is the general algorithm working with any
;;; graph.  If I introduced loops in this representation the thing would work
;;; automatically after simply updating dataflow:instruction->defineds-useds for
;;; each new instruction, if any.

;;; The current implementation, on the current graphs, always converges after
;;; exactly one iteration touching each node once, thanks to the visiting order.

;;; The algorithm is described in "A unified approach to global program
;;; optimization", Gary Kildall, In Conference Record of the ACM Symposium on
;;; Principles of Programming Languages, 1973.  I recommend the article for its
;;; presentation as well.

;;; I haven't taken the time to read about this more recent alternative
;;; approach, which does away with the graph altogether and runs backflow
;;; analysis directly on the program -- which in this case would be in ANF --
;;; via abstract interpretation: "A Graph-Free Approach to Data-Flow Analysis",
;;; Markus Mohnen, in CC '02 Proceedings of the 11th International Conference on
;;; Compiler Construction, 2002.

;;; FIXME: with this same infrastructure I could easily do an Available
;;; Expressions analysis as a (forward) dataflow analysis, then do CSE.

;;; Return the union of the in-lives of the successor of the given state-id.
(e1:define (dataflow:in-lives-of-successors graph state-id)
  (e1:let ((successor-ids (dataflow:graph-successor-state-ids graph state-id)))
    (e1:let loop ((res set-as-list:empty)
                  (rest successor-ids))
      #;(fio:write "- dataflow:in-lives-of-successors: rest has size " (i (list:length rest))
                 " and res has " (i (list:length res)) " elements.\n")
      (e1:if (list:null? rest)
        res
        (e1:let* ((first-id (list:head rest))
                  (first (dataflow:graph-get-state graph first-id))
                  (rest (list:tail rest)))
          (loop (set-as-list:union res (dataflow:state-get-in-lives first))
                rest))))))

;;; FIXME: rename: use "kill-set" and "gen-set" 

;;; Remove all the liveness information from the given graph.
(e1:define (dataflow:clear-liveness! graph)
  (e1:let ((states (dataflow:graph-get-states graph)))
    (e1:dohash (_ state states)
      (dataflow:state-set-in-lives! state list:nil)
      (dataflow:state-set-out-lives! state list:nil))))

(e1:define (dataflow:analyze-liveness! graph)
  (e1:let* ((end-id (dataflow:graph-get-end-id graph))
            (state-no (dataflow:graph-state-no graph))
            (iteration-counter (box:make 0))
            ;; FIXME: topologically sort the initial worklist end-to-beginning, to
            ;; improve convergence speed.  After sorting we can always converge in
            ;; one pass on an acyclic graph.
            (initial-worklist (dataflow:graph->state-ids graph))
            (worklist-elements (unboxed-hash:make))
            #;(terminal-ids (dataflow:graph-predecessor-state-ids graph end-id)))
    (e1:dolist (id initial-worklist)
      (unboxed-hash:set! worklist-elements id #f))
    #;(fio:write "Liveness: end-id is " (i end-id) "\n")
    #;(fio:write "Liveness: terminal-ids has size " (i (list:length terminal-ids)) "\n")
    (e1:let loop ((worklist initial-worklist))
      #;(fio:write "\n* Liveness:\n  worklist has size " (i (list:length worklist)) "\n")
      (e1:unless (list:null? worklist)
        (e1:let* ((first-id (list:head worklist))
                  (predecessor-ids (dataflow:graph-predecessor-state-ids graph first-id))
                  (successor-ids (dataflow:graph-successor-state-ids graph first-id))
                  (state (dataflow:graph-get-state graph first-id))
                  (old-in-lives (dataflow:state-get-in-lives state))
                  (instruction (dataflow:state-get-instruction state))
                  (old-out-lives (dataflow:state-get-out-lives state))
                  ((defineds useds)
                   (dataflow:instruction->defineds-useds instruction))
                  (new-out-lives
                   (set-as-list:union old-out-lives
                                      (dataflow:in-lives-of-successors graph
                                                                       first-id)))
                  (new-in-lives
                   (set-as-list:union old-in-lives
                                      useds
                                      (set-as-list:subtraction new-out-lives
                                                               defineds))))
          (unboxed-hash:unset! worklist-elements first-id)
          #;(fio:write "Liveness: state " (i first-id)
                     " (worklist has size " (i (list:length worklist)) ")"
                     " (there are " (i (dataflow:graph-next-state graph)) " states)\n")
          #;(fio:write "  " (i first-id) ". ")
          #;(dataflow:write-instruction (io:standard-output) instruction)
          #;(fio:write "\n")
          #;(fio:write "  * defineds: {")
          #;(anf:write-variables (io:standard-output) defineds)
          #;(fio:write "}\n")
          #;(fio:write "  * useds: {")
          #;(anf:write-variables (io:standard-output) useds)
          #;(fio:write "}\n")
          #;(e1:unless (fixnum:zero? first-id)  )
          (dataflow:state-set-in-lives! state new-in-lives)
          (dataflow:state-set-out-lives! state new-out-lives)
          (e1:let ((new-worklist
                    (e1:if (fixnum:= (list:length new-in-lives)
                                     (list:length old-in-lives))
                      (list:tail worklist)
                      ;; Our in-lives has changed, so we have to add our
                      ;; predecessors to the worklist -- but for efficiency we
                      ;; don't want to add again any elements already in the
                      ;; worklist.
                      (e1:let ((interesting-predecessor-ids
                                (list:filter-reversed
                                   (e1:lambda (id)
                                     (e1:not (unboxed-hash:has? worklist-elements
                                                                id)))
                                   predecessor-ids)))
                        ;; Mark every element we added to the worklist in the
                        ;; hash, to avoid adding them again later.
                        (e1:dolist (id interesting-predecessor-ids)
                          (unboxed-hash:set! worklist-elements id #f))
                        (list:append-reversed interesting-predecessor-ids
                                              (list:tail worklist))))))
            (box:bump! iteration-counter)
            (loop new-worklist)))))
    (fio:write "Liveness: converged in " (i (box:get iteration-counter))
               " iterations (states are " (i state-no) ").\n")))


;;;;; Control-flow graph: useless if skipping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some optimizations over nested conditionals, particularly as generated by
;;; e1:and and e1:or, are quite easy to perform by transforming the graph after
;;; its construction: for example
;;; (e1:define (f x y z t a b) (e1:if (e1:and x y z t) a b))
;;; first computes a boolean value, then performs a further useless conditional
;;; over the result.  Another obvious case is boolean negation in a condition:
;;; (e1:define-macro (e1:notm x) `(e1:if ,x #f #t))
;;; (e1:define (f x a b) (e1:if (e1:notm x) a b)) .

;;; Cases such as these should be recognized when possible, and jump targets
;;; changed to generate converging edges.  The change can make some if nodes
;;; inaccessible, and some pre-phi nodes useless.  Opportunities to perform this
;;; optimization are found by a form of constant folding thru the graph.
;;;
;;; Similar cases or redundant conditionals also happen with pattern matching:
;;; (e1:define-sum mylist (nil) (cons head tail))
;;; (e1:define (mylength xs)
;;;   (e1:match xs ((mylist-nil) 1)
;;;                ((mylist-cons _ tail) (fixnum:1+ (mylength tail)))))
;;; The last conditional, yielding a no-match error, cannot be eliminated this
;;; way and requires a different e1:match form.  The current e1:match should
;;; also remain available.

(e1:define (dataflow:skip-useless-ifs! graph)
  (e1:let* ((begin-id (dataflow:graph-get-begin-id graph))
            (after-begin-ids (dataflow:graph-successor-state-ids graph begin-id))
            (after-begin-id (e1:assert (fixnum:= (list:length after-begin-ids) 1))
                            (list:head after-begin-ids)))
    (dataflow:skip-useless-ifs-from! graph
                                     begin-id
                                     after-begin-id
                                     alist:nil
                                     list:nil
                                     (string-hash:make))))

;;; Return the given environment updated with the known bindings from a pre-phi
;;; move.  For each bound variable the returned environment contains a binding
;;; if the pre-phi instruction binds the corresponding source variable to a
;;; variable whose value is already known in the environment, and contains no
;;; binding otherwise.
(e1:define (dataflow:bind-pre-phi env bounds sources)
  (e1:cond ((list:null? bounds)
            env)
           ((list:null? sources)
            (alist:unbind-keys-all env bounds))
           (bind (first-bound (list:head bounds))
                 (first-source (list:head sources))
                 (other-bounds (list:tail bounds))
                 (other-sources (list:tail sources)))
           ((alist:has? env first-source)
            (dataflow:bind-pre-phi (alist:bind env
                                               first-bound
                                               (alist:lookup env first-source))
                                   other-bounds
                                   other-sources))
           (else
            (dataflow:bind-pre-phi (alist:unbind-all env first-bound)
                                   other-bounds
                                   other-sources))))

(e1:define (dataflow:skip-useless-ifs-from! graph predecessor-id id env branch-bounds visited)
  (e1:unless (string-hash:has? visited (vector:vector predecessor-id id))
    (string-hash:set! visited (vector:vector predecessor-id id) #f)
    (e1:let* ((state (dataflow:graph-get-state graph id))
              (instruction (dataflow:state-get-instruction state))
              (successor-ids (dataflow:graph-successor-state-ids graph id))
              (successor-id (e1:if (fixnum:zero? (list:length successor-ids))
                              -1 ;; invalid
                              (list:head successor-ids)))
              (id-predecessor-no (dataflow:predecessor-no graph id))
              ;;; If the id node has more than one predecessor then we cannot
              ;;; assume that the variables bound in pre-phi nodes (kept in the
              ;;; branch-bounds list) will be valid from all paths.
              (new-env (e1:if (fixnum:< id-predecessor-no 2)
                         env
                         (alist:unbind-keys-all env branch-bounds))))
      (e1:match instruction
        ((or (dataflow:instruction-return _)
             (dataflow:instruction-tail-call _ _)
             (dataflow:instruction-tail-call-indirect _ _))) ;; do nothing and return
        ((dataflow:instruction-literal bound content)
         (e1:let ((new-env (alist:bind new-env bound content)))
           (dataflow:skip-useless-ifs-from! graph id successor-id new-env branch-bounds visited)))
        ((or (dataflow:instruction-undefined bound)
             (dataflow:instruction-global bound _))
         (e1:let ((new-env (alist:unbind-all new-env bound)))
           (dataflow:skip-useless-ifs-from! graph id successor-id new-env branch-bounds visited)))
        ((or (dataflow:instruction-primitive bounds _ _)
             (dataflow:instruction-nontail-call bounds _ _)
             (dataflow:instruction-nontail-call-indirect bounds _ _))
         (e1:let ((new-env (alist:unbind-keys-all new-env bounds)))
           (dataflow:skip-useless-ifs-from! graph id successor-id new-env branch-bounds visited)))
        ((dataflow:instruction-pre-phi bounds sources)
         (e1:let ((new-env (dataflow:bind-pre-phi new-env bounds sources))
                  (branch-bounds (list:append-reversed bounds branch-bounds)))
           (dataflow:skip-useless-ifs-from! graph id successor-id new-env branch-bounds visited)))
        ((dataflow:instruction-if variable values then-id else-id)
         (e1:if (alist:has? env variable)
           (e1:let ((successor-id
                     (e1:if (list:has? values (alist:lookup env variable))
                       then-id
                       else-id))
                    #;(env (alist:unbind-keys-all env branch-bounds)))
             (fio:write "Optimized away if: " (i predecessor-id) " -> " (i id) "\n")
             (dataflow:graph-remove-state-edge! graph predecessor-id id)
             (dataflow:graph-add-state-edge! graph predecessor-id successor-id)
             ;; The old if successors may still be reachable from other if predecessors,
             ;; so we still have to follow them both.
             (dataflow:skip-useless-ifs-from! graph id then-id new-env branch-bounds visited)
             (dataflow:skip-useless-ifs-from! graph id else-id new-env branch-bounds visited))
           (e1:begin
             (dataflow:skip-useless-ifs-from! graph id then-id new-env branch-bounds visited)
             (dataflow:skip-useless-ifs-from! graph id else-id new-env branch-bounds visited))))
        ((dataflow:instruction-end)
         (e1:error "reached end: this should never happen"))
        ((dataflow:instruction-phi _ _)
         (e1:error "phi should no longer occur"))
        (else
         (fio:write "About the instruction ")
         (dataflow:write-instruction (io:standard-output) instruction)
         (fio:write ":\n")
         (e1:error "dataflow:skip-useless-ifs-from!: unknown instruction"))))))

;;; dataflow:skip-useless-ifs! can make some if nodes inaccessible.  It's
;;; important to remove them before liveness analysis for them not to affect
;;; liveness analysis, where information flows backwards: their use of variables
;;; would keep alive some variables which are made dead by the optimization.
(e1:define (dataflow:remove-inaccessible-states! graph)
  ;;; Keep performing state removal passes until no more states are
  ;;; inaccessible.
  (e1:while (dataflow:remove-inaccessible-states!-one-pass graph)))

;;; Return #t iff at least one state was removed.
(e1:define (dataflow:remove-inaccessible-states!-one-pass graph)
  (e1:let ((states (dataflow:graph-get-states graph))
           (begin-id (dataflow:graph-get-begin-id graph))
           (removed (box:make #f)))
    (e1:dohash (state-id state states)
      #;(fio:write "Considering state " (i state-id) ", with predecessors [ ")
      #;(e1:dolist (predecessor-id (dataflow:graph-predecessor-state-ids graph state-id))
        (fio:write (i predecessor-id) " "))
      #;(fio:write "] and successors [ ")
      #;(e1:dolist (successor-id (dataflow:graph-successor-state-ids graph state-id))
        (fio:write (i successor-id) " "))
      #;(fio:write "]...\n")
      (e1:let ((instruction (dataflow:state-get-instruction state)))
        (e1:when (e1:and (e1:not (dataflow:instruction-begin? instruction))
                         (fixnum:zero? (dataflow:predecessor-no graph state-id)))
          (fio:write "Removed inaccessible state " (i state-id) ":  ")
          (dataflow:write-instruction (io:standard-output) instruction)
          (fio:write "\n")
          (box:set! removed #t)
          (dataflow:graph-remove-state! graph state-id))))
    (box:get removed)))


;;;;; Control-flow graph: useless state removal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Useless if removals will make some pre-phi nodes useless; such nodes are
;;; easy to recognize thru liveness analysis, as pre-phi nodes where all bound
;;; variables are dead.  Bypassing uselss pre-phi nodes may make other nodes
;;; useless, and again they can be identified with liveness analysis.  This
;;; analyze-remove loop may be repeated until convergence.

;;; A useless state implements some operation with no observable effect: either
;;; its results are all dead according to liveness analysis and no side effect
;;; can occur, or the instruction is a conditional jump where the two
;;; destinations are the same.  This procedure removes such useless states from
;;; the given graph, which must contain liveness information on entry.  Return
;;; non-#f iff at least one state was removed.
(e1:define (dataflow:remove-useless-states! graph)
  (e1:let ((states (dataflow:graph-get-states graph))
           (removed (box:make #f)))
    (e1:dohash (state-id state states)
      (e1:let ((instruction (dataflow:state-get-instruction state))
               (out-lives (dataflow:state-get-out-lives state)))
        (e1:match instruction
          ((dataflow:instruction-if _ _ then-id else-id)
           (e1:when (fixnum:= then-id else-id)
             (fio:write "Removed useless if state " (i state-id) ":  ")
             (dataflow:write-instruction (io:standard-output) instruction)
             (fio:write "\n")
             (box:set! removed #t)
             ;; This is safe to perform, since successors are stored as a
             ;; set-as-list: there is only one successor for instruction.
             (dataflow:graph-bypass-state! graph state-id)))
          ((dataflow:instruction-pre-phi bounds _)
           (e1:when (set-as-list:empty? (set-as-list:intersection out-lives bounds))
             (fio:write "Removed useless pre-phi state " (i state-id) ":  ")
             (dataflow:write-instruction (io:standard-output) instruction)
             (fio:write "\n")
             (box:set! removed #t)
             (dataflow:graph-bypass-state! graph state-id)))
          ((dataflow:instruction-primitive bounds primitive-name _)
           (e1:when (e1:and (e1:not (state:primitive-get-side-effecting primitive-name))
                            (set-as-list:empty? (set-as-list:intersection out-lives bounds)))
             (fio:write "Removed useless primitive state " (i state-id) ":  ")
             (dataflow:write-instruction (io:standard-output) instruction)
             (fio:write "\n")
             (box:set! removed #t)
             (dataflow:graph-bypass-state! graph state-id)))
          ((or (dataflow:instruction-literal bound _)
               (dataflow:instruction-global bound _)
               (dataflow:instruction-undefined bound _))
           (e1:unless (set-as-list:has? out-lives bound)
             (fio:write "Removed useless single-binding state " (i state-id) ":  ")
             (dataflow:write-instruction (io:standard-output) instruction)
             (fio:write "\n")
             (box:set! removed #t)
             (dataflow:graph-bypass-state! graph state-id)))
          (else)))) ;; do nothing for the other cases.
    (box:get removed)))

;;; FIXME: pre-phis binding variables which occur only once in the procedure can
;;; be eliminated by renaming the bound variables to the used variables.


;;;;; Control flow graph optimization driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simplify the given control-flow graph, which is not required to have liveness
;;; or register-allocation information at entry.  Such information will be present
;;; [FIXME: actually do that for register allocation] when the procedure returns.
(e1:define (dataflow:optimize-graph! graph)
  ;; First skip useless ifs: this may generate some inaccessible states...
  (dataflow:skip-useless-ifs! graph)
  (fio:write "Skipped useless ifs with success.\n")
  ;; ...that we can easily remove.
  (dataflow:remove-inaccessible-states! graph)
  (fio:write "Removed inaccessible states with success.\n")
  ;; Now there may be useless pre-phi nodes; they are easy to recognize thru a
  ;; liveness analysis.  Removing useless states may generate other useless
  ;; states, recognizable again with a liveness analysis.  Repeat until
  ;; convergence.
  (dataflow:analyze-liveness! graph)
  (e1:while (dataflow:remove-useless-states! graph)
    (dataflow:clear-liveness! graph)
    (dataflow:analyze-liveness! graph))
  (fio:write "Removed useless states with success.\n"))


;;;;; Control flow graph construction driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a control flow graph from the given epsilon0 espression, where the
;;; given symbol list represents formal parameters, if any.  The control flow
;;; graph contains liveness information and [FIXME: implement this] register
;;; allocation for each variable.
(e1:define (dataflow:make-expression-graph expression formals)
  (e1:let* ((optimized-expression (e0:optimize-expression expression))
            (() (fio:write "optimized the epsilon0 expression with success.\n"))
            (alpha-converted-expression (e0:alpha-convert-expression optimized-expression))
            (anf (anf:convert-tail alpha-converted-expression formals))
            (() (fio:write "ANF-converted with success.\n"))
            (graph (dataflow:make-graph)))
    (fio:write "expression:\n" "  " (e expression) "\n")
    (fio:write "optimized-expression:\n" "  " (e optimized-expression) "\n")
    (fio:write "ANF expression:\n  ")
    (anf:write-anf-expression (io:standard-output) anf)
    (fio:write "\n")
    (dataflow:add-anf-to-graph! graph anf (dataflow:graph-get-begin-id graph))
    (fio:write "Made graph with success.\n")
    (dataflow:optimize-graph! graph)
    (fio:write "Analyzed liveness with success.\n")
    graph))

;;; Return a control-flow graph, as per dataflow:make-expression-graph , for the
;;; epsilon0 procedure having the given name.
(e1:define (dataflow:make-procedure-graph procedure-name)
  (e1:let ((body (state:procedure-get-body procedure-name))
           (formals (state:procedure-get-formals procedure-name)))
    (dataflow:make-expression-graph body formals)))


;;;;; Debugging and graph output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (dataflow:print-graph file-name graph graph-comment)
  (e1:let ((file (io:open-file file-name io:write-mode))
           (states (dataflow:graph-get-states graph))
           (successors (dataflow:graph-get-successors graph))
           (begin-id (dataflow:graph-get-begin-id graph))
           (end-id (dataflow:graph-get-end-id graph)))
    (fio:write-to file "digraph {
  rankdir = TB/*LR*/;
  //graph [nodesep=\"1\", fizedsize=true];
  margin = 0; padding = 0;
  pack = false;
  overlap = false;
  fizedsize=true;
  labelloc = top;
  labeljust = left;
  label = \"" (st graph-comment) "\";
  node [shape=\"box\", /*style=\"rounded, filled\",*/ fillcolor=\"lavender\"]
  /*node [shape=\"box\", font=\"Courier\", style=\"rounded, filled\", color=\"white\" fillcolor=\"lavender\", margin=\"0\", pad=\"0\"];*/
  edge [weight=1, ];
  splines=true;
  /*edge [style=\">=stealth',shorten >=1pt\"];*/
\n\n")
    (e1:dohash (id state states)
      (e1:let ((in-lives (dataflow:state-get-in-lives state))
               (instruction (dataflow:state-get-instruction state))
               (out-lives (dataflow:state-get-out-lives state)))
        (fio:write-to file "  " (i id)
                      " [style=\"filled, rounded\", margin=\"0.1\", pad=\"0\", color=\"white\", "
                      (st (e1:cond ((dataflow:instruction-if? instruction)
                                    "/*shape=\"octagon\",*/ /*shape=\"diamond\",*/ /*shape=\"trapezium\",*/ shape=\"diamond\", fillcolor=\"yellow\", margin=\"0\",")
                                   ((dataflow:instruction-pre-phi? instruction)
                                    "shape=\"house\", margin=\"0\", ")
                                   ((dataflow:instruction-phi? instruction)
                                    "shape=\"invhouse\", fillcolor=\"yellow\", margin=\"0\", ")
                                   ((dataflow:instruction-literal? instruction)
                                    "fillcolor=\"aquamarine\", ")
                                   ((dataflow:instruction-primitive? instruction)
                                    "fillcolor=\"greenyellow\", ")
                                   ((e1:or (dataflow:instruction-nontail-call? instruction)
                                           (dataflow:instruction-nontail-call-indirect? instruction))
                                    "fillcolor=\"lightsalmon\", ")
                                   ((e1:or (dataflow:instruction-return? instruction)
                                           (dataflow:instruction-tail-call? instruction)
                                           (dataflow:instruction-tail-call-indirect? instruction))
                                    "fillcolor=\"lightblue\", ")
                                   ((dataflow:instruction-begin? instruction)
                                    "shape=\"ellipse\", fillcolor=\"lightgrey\", color=\"lightgrey\", rank=min, ")
                                   ((dataflow:instruction-end? instruction)
                                    "shape=\"ellipse\", fillcolor=\"lightgrey\", color=\"lightgrey\", rank=max, ")
                                   (else
                                    "")))
                      "label=\"")
        (fio:write-to file "{")
        (anf:write-variables file in-lives)
        (fio:write-to file "}\\n")
        (fio:write-to file (i id) ". ")
        (dataflow:write-instruction file (dataflow:state-get-instruction state))
        (fio:write-to file "\\n{")
        (anf:write-variables file out-lives)
        (fio:write-to file "}")
        (fio:write-to file "\"];\n")
        ;; Force orderining of conditional branches: then on the left and else
        ;; on the right, on the same rank.
        (e1:match instruction
          ((dataflow:instruction-if _ _ then else)
           (fio:write-to file "  {rank=same " (i then) " -> " (i else)
                         " [color=red, style=dashed, weight=1]};\n"))
          (else)))) ;; do nothing in the other cases.
    (fio:write-to file "\n")
    (e1:dohash (from-id to-ids successors)
      (e1:dolist (to-id to-ids)
        (fio:write-to file "  " (i from-id) " -> " (i to-id)
                      (st (e1:cond ((fixnum:= from-id begin-id)
                                    "[/*style=dashed,*/ color=grey]")
                                   ((fixnum:= to-id end-id)
                                    "[/*style=dashed,*/ color=grey]")
                                   (else
                                    "")))
                      ";\n")))
    (fio:write-to file "}\n")
    (io:close-file file)))

(e1:define (dataflow:show-graph graph graph-comment)
  (e1:let ((file-name "/tmp/graph.dot"))
    (dataflow:print-graph file-name graph graph-comment)
    (unix:system "dot /tmp/graph.dot -T pdf -o /tmp/a.pdf && echo xpdf /tmp/a.pdf")))

(e1:define (dataflow:show-procedure-graph procedure-name)
  (e1:let ((graph (dataflow:make-procedure-graph procedure-name))
           (formals (state:procedure-get-formals procedure-name)))
    (e1:let ((label-box
              (box:make (string:append "(" (symbol:symbol->string procedure-name)))))
      (e1:dolist (f formals)
        (box:set! label-box
                  (string:append (box:get label-box)
                                 " "
                                 (symbol:symbol->string f))))
      (box:set! label-box
                (string:append (box:get label-box) ")"))
      (dataflow:show-graph graph (box:get label-box)))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (testg se)
  `(dataflow:show-procedure-graph (e1:value ,se)))

(e1:define-macro (test se)
  `(e1:let* ((e (e1:macroexpand-and-transform ',se))
             (alpha-converted-e (e0:alpha-convert-expression e))
             (a (anf:convert-tail alpha-converted-e (e1:value-list x y z))))
     (fio:write "ANF-converted with success.\n")
     (anf:write-anf-expression (io:standard-output) a)
     (fio:write "\n")))

(e1:define-macro (testp procedure-name)
  `(e1:let* ((body (state:procedure-get-body (e1:value ,procedure-name)))
             ;;(() (fio:write "Got body\n"))
             (() (e1:require (e1:not (fixnum:zero? body))))
             ;;(() (fio:write (e body) "\n"))
             (formals (state:procedure-get-formals (e1:value ,procedure-name)))
             ;;(() (fio:write "Got formals\n"))
             (alpha-converted-body (e0:alpha-convert-expression body))
             ;;(() (fio:write "alpha-converted body\n"))
             (a (anf:convert-tail alpha-converted-body formals)))
     ;;(fio:write "ANF-converted with success.\n")
     (anf:write-anf-expression (io:standard-output) a)
     (fio:write "\n")))
