;;;;; epsilon0 analyses and optimizations in -*- epsilon -*- -- actually in epsilon1.

;;;;; Copyright (C) 2014, 2015 Luca Saiu
;;;;; Written by Luca Saiu

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


;;;;; epsilon0 call graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A call graph is a hash mapping a procedure names (or 0 for the
;;; main expression) to a set-as-list of procedure names (including 0
;;; for any number of indirect calls).  The call graph is closed over
;;; the "calls" relation, disregarding indirect calls [FIXME: dig
;;; global data for symbols, and include the ones bound to procedures
;;; as well; I do that in the trivial compiler].

;;; FIXME: update e1:define to automatically update a global call graph,
;;; to be always kept consistent.

;;; Build and return a new call graph, with the given expression as
;;; the main expression and its callees.
(e1:define (call-graph:build-from-main-expression expression)
  (e1:let ((cg (unboxed-hash:make)))
    (call-graph:build-helper cg
                             (call-graph:add-main-expression!-internal cg expression))))

;;; Build and return a new call graph with no main expression,
;;; containing the given procedure and its callees.
(e1:define (call-graph:build-from-procedure procedure-name)
  (e1:let ((cg (unboxed-hash:make)))
    (call-graph:add-procedure! cg procedure-name)
    cg))

;; In case the given procedure is unknown to the call graph, first add
;; it, along with its callees.
(e1:define (call-graph:procedure-add-when-unknown! call-graph procedure-name)
  (e1:let ((known (unboxed-hash:has? call-graph procedure-name)))
    (e1:unless known
      (call-graph:add-procedure! call-graph procedure-name)
      (unboxed-hash:get call-graph procedure-name))))

;;; Return a set-as-list with the given procedure callee names.  If
;;; the given procedure is not known in the call graph yet then first
;;; add it, along with its callees.
(e1:define (call-graph:procedure-direct-callees call-graph procedure-name)
  (call-graph:procedure-add-when-unknown! call-graph procedure-name)
  (unboxed-hash:get call-graph procedure-name))

;;; Return #t iff the given procedure is directly recursive.  If it's
;;; not known in the call graph yet then first add it, along with its
;;; callees.
(e1:define (call-graph:procedure-directly-recursive? call-graph procedure-name)
  (set-as-list:has? (call-graph:procedure-direct-callees call-graph procedure-name)
                    procedure-name))

;;; Return #t iff the given procedure has no callees.  If it's not
;;; known in the call graph yet then first add it, along with its
;;; callees.
(e1:define (call-graph:procedure-leaf? call-graph procedure-name)
  (set-as-list:empty? (call-graph:procedure-direct-callees call-graph procedure-name)))

;;; Update the given call graph by adding the given procedure, and its
;;; callees.
(e1:define (call-graph:add-procedure! call-graph procedure-name)
  (call-graph:build-helper call-graph (list:list procedure-name)))

(e1:define (call-graph:build-helper call-graph worklist)
  (e1:if (list:null? worklist)
    call-graph
    (e1:let ((first (list:head worklist))
             (rest (list:tail worklist)))
      (e1:if (e1:or (fixnum:zero? first)
                    (unboxed-hash:has? call-graph first))
        (e1:unless (unboxed-hash:has? call-graph first)
          (call-graph:add-procedure! cg first)) ;; Make sure the procedure is known.
        (call-graph:build-helper call-graph rest)
        (e1:let ((worklist (list:append (call-graph:add-procedure!-internal call-graph first)
                                        rest)))
          (unboxed-hash:ensure-non-empty! call-graph first)
          (call-graph:build-helper call-graph worklist))))))

;;; Return the set-as-list of direct callees.
(e1:define (call-graph:add-main-expression!-internal call-graph expression)
  (call-graph:add-expression-direct-callees!-internal call-graph 0 expression))

;;; Return the set-as-list of direct callees.
(e1:define (call-graph:add-procedure!-internal call-graph procedure-name)
  (call-graph:add-expression-direct-callees!-internal call-graph
                                                      procedure-name
                                                      (state:procedure-get-body procedure-name)))

;;; Return the set-as-list of direct callees.
(e1:define (call-graph:add-expression-direct-callees!-internal call-graph procedure-name expression)
  (e1:match expression
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     set-as-list:empty)
    ((e0:expression-bundle _ items)
     (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name items))
    ((e0:expression-primitive _ _ actuals)
     (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name actuals))
    ((e0:expression-let _ bound-variables bound-expression body)
     (set-as-list:union
      (call-graph:add-expression-direct-callees!-internal call-graph procedure-name bound-expression)
         (call-graph:add-expression-direct-callees!-internal call-graph procedure-name body)))
    ((e0:expression-call _ callee-name actuals)
     (unboxed-hash:add-to-set-as-list! call-graph procedure-name callee-name)
     (set-as-list:with
      (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name actuals)
      callee-name))
    ((e0:expression-fork _ _ actuals) ;; FIXME: the fork case may be debatable.
     (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name actuals))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (unboxed-hash:add-to-set-as-list! call-graph procedure-name 0)
     (set-as-list:with
      (set-as-list:union
       (call-graph:add-expression-direct-callees!-internal call-graph procedure-name procedure-expression)
       (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name actuals))
      0))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (set-as-list:union
      (call-graph:add-expression-direct-callees!-internal call-graph procedure-name discriminand)
      (call-graph:add-expression-direct-callees!-internal call-graph procedure-name then-branch)
      (call-graph:add-expression-direct-callees!-internal call-graph procedure-name else-branch)))
    ((e0:expression-join _ future)
     (call-graph:add-expression-direct-callees!-internal call-graph procedure-name future))))

(e1:define (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name expressions)
  (e1:if (list:null? expressions)
    set-as-list:empty
    (set-as-list:union
     (call-graph:add-expression-direct-callees!-internal call-graph procedure-name (list:head expressions))
     (call-graph:add-expressions-direct-callees!-internal call-graph procedure-name (list:tail expressions)))))


;;; Call graph debug.

(e1:define (debug:print-call-graph cg)
  (e1:let ((alist (unboxed-hash:unboxed-hash->alist cg)))
    (e1:dolist (pair alist)
      (fio:write "* ")
      (debug:print-symbol-or-0-internal (cons:car pair) "<main-expression>")
      (fio:write " -> ")
      (e1:dolist (callee (cons:cdr pair))
        (debug:print-symbol-or-0-internal callee "<indirect>")
        (fio:write " "))
      (fio:write "\n"))))

(e1:define-macro (debug:print-call-graph-from sexpression)
  `(e1:let* ((e (repl:macroexpand-and-transform ',sexpression))
             (g (call-graph:build-from-main-expression e)))
     (debug:print-call-graph g)))

(e1:define (debug:print-symbol-or-0-internal s string)
  (e1:if (fixnum:zero? s)
    (fio:write (s string))
    (fio:write (sy s))))


;;;;; epsilon0 call graph analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a set-as-list with (possibly indirect) callees, including 0
;;; for any dynamic one.  If the procedure is not known in the call graph
;;; then add it first, along with its callees.
(e1:define (call-graph:procedure-callees call-graph procedure)
  (call-graph:procedure-callees-helper call-graph
                                       (list:list procedure)
                                       set-as-list:empty
                                       set-as-list:empty))
(e1:define (call-graph:procedure-callees-helper call-graph worklist touched acc)
  (e1:if (list:null? worklist)
    acc
    (e1:let ((first (list:head worklist))
             (rest (list:tail worklist)))
      (e1:if (e1:or (fixnum:zero? first)
                    (set-as-list:has? touched first))
        (call-graph:procedure-callees-helper call-graph rest touched acc)
        (e1:let ((first-direct-callees (call-graph:procedure-direct-callees call-graph
                                                                            first)))
          (call-graph:procedure-callees-helper call-graph
                                               (list:append first-direct-callees rest)
                                               (set-as-list:with touched first)
                                               (set-as-list:union first-direct-callees acc)))))))

;;; Return #f is the procedure is non-recursive, or #t if it's either
;;; definitely (syntactically) recursive or there is any dynamic
;;; callee.  In either case we consider recursion in a possibly
;;; indirect way.  If the procedure is not known yet, first add it
;;; along with its callees.
(e1:define (call-graph:procedure-recursive? call-graph procedure)
  (e1:let ((callees (call-graph:procedure-callees call-graph procedure)))
    (e1:if (set-as-list:has? callees 0)
      #t ;; Conservative approximation.
      (call-graph:procedure-recursive?-helper call-graph callees))))
(e1:define (call-graph:procedure-recursive?-helper call-graph callees)
  (e1:cond ((list:null? callees)
            #f)
           (bind (first (list:head callees))
                 (rest (list:tail callees)))
           ((set-as-list:has? (call-graph:procedure-callees call-graph first)
                              first)
            #t)
           (else
            (call-graph:procedure-recursive?-helper call-graph rest))))

;;; FIXME: reimplement side-effect analysis using the call graph.  It
;;; can be made more efficient, and also more precise then one of my
;;; current two (!) implementations.


;;;;; epsilon0 expression size as syntactic complexity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e0:expression-size e)
  (e1:match e
    ((or (e0:expression-variable _ x)
         (e0:expression-value _ v))
     1)
    ((e0:expression-bundle _ items)
     (fixnum:1+ (e0:expressions-size items)))
    ((or (e0:expression-primitive _ name actuals)
         (e0:expression-call _ procedure-name actuals)
         (e0:expression-fork _ procedure-name actuals))
     (fixnum:1+ (e0:expressions-size actuals)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (fixnum:+ 1
               (list:length bound-variables)
               (e0:expression-size bound-expression)
               (e0:expression-size body)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (fixnum:+ 1
               (e0:expression-size procedure-expression)
               (e0:expressions-size actuals)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (fixnum:+ 1
               (e0:expression-size discriminand)
               (list:length values)
               (e0:expression-size then-branch)
               (e0:expression-size else-branch)))
    ((e0:expression-join _ future)
     (fixnum:1+ (e0:expression-size future)))))
(e1:define (e0:expressions-size es)
  (e1:if (list:null? es)
    0
    (fixnum:+ (e0:expression-size (list:head es))
              (e0:expressions-size (list:tail es)))))

;;; Small expression threshold
(e1:define e0:expression-small-size-threshold
  (fixnum:* 2
            (e0:expression-size (state:procedure-get-body (e1:value cons:make)))))

(e1:define (e0:expression-small? e)
  (fixnum:<= (e0:expression-size e)
             e0:expression-small-size-threshold))

(e1:define (state:procedure-small? name)
  (e0:expression-small? (state:procedure-get-body name)))


;;;;; epsilon0 expression "weight": runtime cost without callees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An approximation of the runtime cost of evaluating an expression,
;;; excluding callees.

(e1:define (e0:expression-weight e)
  (e1:match e
    ((e0:expression-variable _ x)
     1)
    ((e0:expression-value _ v)
     2) ;; loading some big constants can be expensive
    ((e0:expression-bundle _ items)
     (fixnum:+ 0
               (e0:expressions-weight items)))
    ((e0:expression-primitive _ name actuals)
     (fixnum:+ 3
               (e0:expressions-weight actuals)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (fixnum:+ 0
               (fixnum:* 0
                         (list:length bound-variables))
               (e0:expression-weight bound-expression)
               (e0:expression-weight body)))
    ((e0:expression-call _ procedure-name actuals)
     (fixnum:+ 10
               (e0:expressions-weight actuals)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (fixnum:+ 20
               (e0:expression-weight procedure-expression)
               (e0:expressions-weight actuals)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (fixnum:+ 20
               (e0:expression-weight discriminand)
               (fixnum:* 3
                         (list:length values))
               (fixnum:max (e0:expression-weight then-branch)
                           (e0:expression-weight else-branch))))
    ((e0:expression-fork _ procedure-name actuals)
     (fixnum:+ 1000
               (e0:expressions-weight actuals)))
    ((e0:expression-join _ future)
     (fixnum:+ 100
               (e0:expression-weight future)))))
(e1:define (e0:expressions-weight es)
  (e1:if (list:null? es)
    0
    (fixnum:+ (e0:expression-weight (list:head es))
              (e0:expressions-weight (list:tail es)))))

;;; Small expression threshold
(e1:define e0:expression-small-weight-threshold
  (fixnum:* 2
            (e0:expression-weight (state:procedure-get-body (e1:value cons:make)))))

(e1:define (e0:expression-lightweight? e)
  (fixnum:<= (e0:expression-weight e)
             e0:expression-small-weight-threshold))

(e1:define (state:procedure-lightweight? name)
  (e0:expression-lightweight? (state:procedure-get-body name)))


;;;;; epsilon0 expression and procedure alpha-conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e0:alpha-convert-expression e)
  (e0:alpha-convert-expression-with e alist:nil))
(e1:define (e0:alpha-convert-expression-with e a)
  (e1:match e
    ((e0:expression-variable _ x)
     (e1:if (alist:has? a x)
       (e0:variable* (alist:lookup a x))
       e))
    ((e0:expression-value _ v)
     e)
    ((e0:expression-bundle _ items)
     (e0:bundle* (e0:alpha-convert-expressions-with items a)))
    ((e0:expression-primitive _ name actuals)
     (e0:primitive* name (e0:alpha-convert-expressions-with actuals a)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (e1:let* ((variable-no (list:length bound-variables))
               (new-bound-variables (symbol:fresh-symbols variable-no))
               (new-a (alist:bind-lists a bound-variables new-bound-variables)))
       (e0:let* new-bound-variables
                (e0:alpha-convert-expression-with bound-expression a)
                (e0:alpha-convert-expression-with body new-a))))
    ((e0:expression-call _ procedure-name actuals)
     (e0:call* procedure-name (e0:alpha-convert-expressions-with actuals a)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e0:call-indirect* (e0:alpha-convert-expression-with procedure-expression m)
                        (e0:alpha-convert-expressions-with actuals a)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e0:if-in* (e0:alpha-convert-expression-with discriminand a)
                values
                (e0:alpha-convert-expression-with then-branch a)
                (e0:alpha-convert-expression-with else-branch a)))
    ((e0:expression-fork _ procedure-name actuals)
     (e0:fork* procedure-name (e0:alpha-convert-expressions-with actuals a)))
    ((e0:expression-join _ future)
     (e0:join* (e0:alpha-convert-expression-with future a)))))
(e1:define (e0:alpha-convert-expressions-with es a)
  (e1:if (list:null? es)
    list:nil
    (list:cons (e0:alpha-convert-expression-with (list:head es) a)
               (e0:alpha-convert-expressions-with (list:tail es) a))))

(e1:define (e0:alpha-convert-procedure-definition formals body)
  (e1:let* ((new-formals (symbol:fresh-symbols (list:length formals)))
            (a (alist:bind-lists alist:nil formals new-formals)))
    (e1:bundle new-formals
               (e0:alpha-convert-expression-with body a))))

(e1:define (e0:alpha-convert-procedure name)
  (e0:alpha-convert-procedure-definition (state:procedure-get-formals name)
                                         (state:procedure-get-body name)))


;;;;; epsilon0 side-effecting expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An expression judged non-side-effecting by this predicate will
;;; definitely not have side effects when evaluated.  If the predicate
;;; returns #t then the expression may or may not have side effects.
(e1:define (e0:side-effecting-expression? e)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     #f)
    ((e0:expression-bundle _ items)
     (e0:side-effecting-expressions? items))
    ((e0:expression-primitive _ name actuals)
     (e1:or (state:primitive-side-effecting? name)
            (e0:side-effecting-expressions? actuals)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (e1:or (e0:side-effecting-expression? bound-expression)
            (e0:side-effecting-expression? body)))
    ((e0:expression-call _ procedure-name actuals)
     #t) ;; FIXME: this is *extremely* conservative!
    ((e0:expression-call-indirect _ procedure-expression actuals)
     #t) ;; the called procedure is not known
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e1:or (e0:side-effecting-expression? discriminand)
            (e0:side-effecting-expression? then-branch)
            (e0:side-effecting-expression? else-branch)))
    ((e0:expression-fork _ procedure-name actuals)
     #t) ;; making a thread has effects.  Being more subtle would require looking at the procedure.
    ((e0:expression-join _ future)
     (e0:side-effecting-expression? future))))
(e1:define (e0:side-effecting-expressions? es)
  (list:exists? (e1:lambda (e) (e0:side-effecting-expression? e))
                es))


;;;;; epsilon0 expression callees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The result is a set-as-list of procedure names and 0, which stands
;;; for indirect calls.  FIXME: shall I remove this, now that I have
;;; call graph support?  This is faster for one-shot calls, but I'm
;;; not sure that case is useful.

(e1:define (e0:expression-callees e)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     set-as-list:empty)
    ((e0:expression-bundle _ items)
     (e0:expressions-callees items))
    ((e0:expression-primitive _ _ actuals)
     (e0:expressions-callees actuals))
    ((e0:expression-let _ bound-variables bound-expression body)
     (set-as-list:union (e0:expression-callees bound-expression)
                        (e0:expression-callees body)))
    ((or (e0:expression-call _ procedure-name actuals)
         ;; FIXME: should I consider a fork expression to have no callees?
         (e0:expression-fork _ procedure-name actuals))
     (set-as-list:with (e0:expressions-callees actuals)
                       procedure-name))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (set-as-list:with (set-as-list:union (e0:expression-callees procedure-expression)
                                          (e0:expressions-callees actuals))
                       0))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (set-as-list:union (e0:expression-callees discriminand)
                        (e0:expression-callees then-branch)
                        (e0:expression-callees else-branch)))
    ((e0:expression-join _ future)
     (e0:expression-callees future))))
(e1:define (e0:expressions-callees es)
  (e1:if (list:null? es)
    set-as-list:empty
    (set-as-list:union (e0:expression-callees (list:head es))
                       (e0:expressions-callees (list:tail es)))))


;;;;; epsilon0 side-effect analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e0:expression-side-effecting? e)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     #f)
    ((e0:expression-bundle _ items)
     (e0:expressions-side-effecting? items))
    ((e0:expression-primitive _ name actuals)
     (e1:or (state:primitive-side-effecting? name)
            (e0:expressions-side-effecting? actuals)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (e1:or (e0:expression-side-effecting? bound-expression)
            (e0:expression-side-effecting? body)))
    ((e0:expression-call _ procedure-name actuals)
     (e1:or (e0:expressions-side-effecting? actuals) ;; Usually faster: do it first.
            (e0:procedure-side-effecting? procedure-name)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     ;; FIXME: be less conservative, if possible
     #t)
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e1:or (e0:expression-side-effecting? discriminand)
            (e0:expression-side-effecting? then-branch)
            (e0:expression-side-effecting? else-branch)))
    ((e0:expression-fork _ _ _)
     #t)
    ((e0:expression-join _ _)
     #t)))
(e1:define (e0:expressions-side-effecting? es)
  (e1:if (list:null? es)
    #f
    (e1:or (e0:expression-side-effecting? (list:head es))
           (e0:expressions-side-effecting? (list:tail es)))))

(e1:define (e0:procedure-side-effecting? name)
  (e0:expression-side-effecting? (state:procedure-get-body name)))


;;;;; epsilon0 useless-let removal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A useless let is a let whose bound expression is
;;; non-side-effecting (even if not necessarily trivial) and whose
;;; bound variables do not occur free in the body.  Useless-let
;;; removal should be performed *before* trivial-let removal, since
;;; useless-let removal may make some subexpressions trivial.

;; Testcase: (u (e1:let ((a (e1:primitive fixnum:+ 4 2))) 1 2))

;;; FIXME: actually implement useless-let removal.
(e1:define (e0:expression-without-useless-lets e)
  e)

;;;;; epsilon0 let bundle simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (e0:let (x1...xN) (e0:bundle A1...AN) B) can be easily
;;; simplified when some of the bound variables do not occur
;;; in the body and the corresponding items are non-side-effecting.

;; Does this actually occur?  It might, in generated code.
;; FIXME: implement


;;;;; epsilon0 trivial-let removal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A trivial expression is a constant, a variable, or a one-item
;;; bundle containing a trivial expression.

(e1:define (e0:expression-trivial? e)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     #t)
    ((e0:expression-bundle _ items)
     (e1:and (fixnum:= (list:length items) 1)
             (e0:expression-trivial? (list:head items))))
    (else
     #f)))

;;; Return an equivalent trivial expression without redundant bundles.
(e1:define (e0:expression-simplify-trivial e)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     e)
    ((e0:expression-bundle _ items)
     (e1:if (fixnum:= (list:length items) 1)
       (e0:expression-simplify-trivial (list:head items))
       (e1:error "non-trivial expression")))
    (else
     (e1:error "non-trivial expression"))))

;;; A trivial bound expression, with respect to a given number n of
;;; variables, is either:
;;; i)  a trivial non-bundle expression
;;; or
;;; ii) a bundle of non-side-effecting expressions of which at least
;;;     the first n are trivial.
;;; We don't need a iii) case involving let, since we recursively remove
;;; trivial lets from the bound expression before checking for triviality.

;;; This assumes e to have the correct dimension.  We let code fail
;;; otherwise.
(e1:define (e0:trivial-bound-expression? e variable-no)
  (e1:match e
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     #t)
    ((e0:expression-bundle _ items)
     (e1:and (list:for-all?
                 (e1:lambda (i) (e1:not (e0:side-effecting-expression? i)))
                 items)
             (list:for-all?
                 (e1:lambda (i) (e0:expression-trivial? i))
                 (list:take items variable-no))))
    (else
     #f)))

;;; Return a list of trivial expressions.  This assumes the given
;;; bound expression to be trivial.
(e1:define (e0:trivialize-bound-expression e variable-no)
  (list:take (e1:match e
               ((or (e0:expression-variable _ _)
                    (e0:expression-value _ _))
                (list:list e))
               ((e0:expression-bundle _ items)
                (list:map (e1:lambda (i) (e0:expression-simplify-trivial i))
                          (list:take items variable-no)))
               (else
                (e1:error "non-trivial bound expression")))
             variable-no))

;;; The alist used here binds each variable to a trivial expression, which can
;;; itself be a variable.

(e1:define (e0:expression-without-trivial-lets e)
  (e0:expression-without-trivial-lets-with e alist:nil))
(e1:define (e0:expression-without-trivial-lets-with e a)
  (e1:match e
    ((e0:expression-variable _ x)
     (e1:if (alist:has? a x)
       (alist:lookup a x)
       e))
    ((e0:expression-value _ v)
     e)
    ((e0:expression-bundle _ items)
     (e0:bundle* (e0:expressions-without-trivial-lets-with items a)))
    ((e0:expression-primitive _ name actuals)
     (e0:primitive* name
                    (e0:expressions-without-trivial-lets-with actuals a)))
    ((e0:expression-let _ bound-variables bound-expression body)
     ;; This rebinding of bound-expression is needed to preserve
     ;; equivalence: we want variable references in bound-expression
     ;; to be resolved before binding bound-variables.
     (e1:let ((bound-expression
               (e0:expression-without-trivial-lets-with bound-expression a)))
       (e1:if (e0:trivial-bound-expression? bound-expression
                                            (list:length bound-variables))
         (e1:let* ((trivial-expressions (e0:trivialize-bound-expression
                                            bound-expression
                                            (list:length bound-variables)))
                   (new-a (alist:bind-lists a
                                            bound-variables
                                            trivial-expressions)))
           (e0:expression-without-trivial-lets-with body new-a))
         (e1:let ((new-a (alist:unbind-all-list a bound-variables)))
           (e0:let* bound-variables
                    bound-expression ;; we've already rebound this
                    (e0:expression-without-trivial-lets-with body new-a))))))
    ((e0:expression-call _ procedure-name actuals)
     (e0:call* procedure-name
               (e0:expressions-without-trivial-lets-with actuals a)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e0:call-indirect* (e0:expression-without-trivial-lets-with procedure-expression a)
                        (e0:expressions-without-trivial-lets-with actuals a)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e0:if-in* (e0:expression-without-trivial-lets-with discriminand a)
                values
                (e0:expression-without-trivial-lets-with then-branch a)
                (e0:expression-without-trivial-lets-with else-branch a)))
    ((e0:expression-fork _ procedure-name actuals)
     (e0:fork* procedure-name
               (e0:expressions-without-trivial-lets-with actuals a)))
    ((e0:expression-join _ future)
     (e0:join* (e0:expression-without-trivial-lets-with future a)))))
(e1:define (e0:expressions-without-trivial-lets-with es a)
  (e1:if (list:null? es)
    list:nil
    (list:cons (e0:expression-without-trivial-lets-with (list:head es) a)
               (e0:expressions-without-trivial-lets-with (list:tail es) a))))


;;;;; epsilon0 unneeded-let removal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A unneeded let is either a trivial let or a useless let.
(e1:define (e0:expression-without-unneeded-lets e)
  (e0:expression-without-trivial-lets
     (e0:expression-without-useless-lets e)))


;;;;; Inlined epsilon0 procedure calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e0:inlined-call procedure-name actuals)
  (e0:let (formals body) (e0:alpha-convert-procedure procedure-name)
;; (e0:expression-without-unneeded-lets ;; This may be expensive.
     (e0:inlined-call-helper formals actuals body)))
;; )

;;; This assumes formals to be fresh variables, which is true when
;;; called from e0:inlined-call.
;;; FIXME: don't introduce an e0:let at all, when passing a trivial
;;; expression.
(e1:define (e0:inlined-call-helper formals actuals body)
  (e1:cond ((list:null? formals)
            (e1:if (list:null? actuals)
              body
              (e1:error "more actuals than formals")))
           ((list:null? actuals)
            (e1:error "more formals than actuals"))
           (else
            (e0:let* (list:list (list:head formals))
                     (list:head actuals)
                     (e0:inlined-call-helper (list:tail formals)
                                             (list:tail actuals)
                                             body)))))
