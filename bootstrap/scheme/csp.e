;;;;; This is -*- epsilon -*-
;;;;; Constraint-Satisfaction Programming: finite domain solver/optimizer

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


;;; This is a general-purpose Constraint-Satisfaction Programming system over
;;; finite domains.  I wrote it to solve a specific problem in the new epsilon
;;; compiler, but the system seems generally useful.

;;; Constraints may have any arity and contain user-defined boolean-result
;;; procedures (actually closures), and may be also be combined with
;;; propositional connectives.  When possible it is better to write a constraint
;;; as a conjunction or disjunction of simpler constraints, to let the solver
;;; prune more effectively.

;;; The system can find a solution to a given problem and also optimize with
;;; respect to "soft constraint".  A soft constraint is a constraint whose
;;; satisfaction is not mandatory for a solution to be valid, but whose
;;; violation entails a "penalty", or cost.  Penalties are positive fixnums.

;;; An optimal solution to a CSP problem as formulated here satisfies every hard
;;; constraint and minimizes the total penalty for violated soft constraints.
;;; In particualar, the solver can always find a zero-penatly solution, if one
;;; exists.

;;; The solver searches solution by backtracking, using Forward-Checking with
;;; the Minimum Remaining Values and Degree heuristics, and employs a simple
;;; branch-and-bound technique to prune provably suboptimal branches.

;;; The sover can either return the first solution to be found, or search for a
;;; provably optimal solution.


;;;;; CSP problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: add a comment about variables and values.

;;; A domain is represented as a list of values.

(e1:define-record csp:problem
  variables            ;; an alist mapping each variable to its domain
  weighed-constraints) ;; a weighed constraint list

;;; An assignment is an alist mapping each variable to its one value.  The
;;; assignment returned as part of a solution will be complete -- which is to
;;; say, every variable will have a value.

(e1:define-sum csp:solution
  (failure)
  (success penalty assignment))


;;;;; Constraint definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A constraint closure takes a list of values and returns a boolean: #t if the
;;; constraint is satisfied, #f if it's not satisfied.  The closure is never
;;; called unless all the given variables have a value, and doesn't necessarily
;;; need to work with lists of any possible size: it is only called with the
;;; list of values obtained by instantiating every variable, in order.

;;; While searching for a solution variables are instantiated incrementally, and
;;; not necessarily in the same order as given in the closure case.  After one
;;; variable is instantiated constraints are rewritten, replacing the variable
;;; with a constant: therefore an n-ary constraint mentioning the variable X may
;;; become (n-1)-ary.  A constraint can always be evaluated to true or false
;;; after it becomes nullary.
(e1:define-sum csp:constraint
  (false)
  ;; In the closure case closure is the constraint closure as per the comment
  ;; above, name is a string (for printint and debugging), and variables is a
  ;; list.  The last element all-variables is a list of every variable to be
  ;; passed, including the ones which have already been instantiated.  The
  ;; closure case is much more convenient to build using the helper procedures
  ;; in the following section.
  (closure-internal closure name variables all-variables)
  (or constraint-a constraint-b)
  (not constraint))

;;; A constraint is a bare constraint with a specified weight.  The weight
;;; represents the cost of violating the constraint, which however does not
;;; by itself invalidate a solution.
(e1:define-record csp:weighed-constraint
  weight constraint)

(e1:define-sum csp:weight
  (hard)
  (soft penalty)) ;; penalty is a positive fixnum

;;; A list containing just one hard false constraint.  This is convenient to
;;; simplify unsatisfiable constraints into, as it is efficient to evaluate
;;; and to propagate.
(e1:define csp:unsatisfiable-wcs
  (list:list (csp:weighed-constraint (csp:weight-hard)
                                     (csp:constraint-false))))


;;;;; Closure constraints: convenience procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Directly building a constraint in the closure case is slightly inconvenient,
;;; as the caller needs to pass the variable list twice.  This procedure should
;;; be used as a constructor.
(e1:define (csp:constraint-closure closure name variables)
  (csp:constraint-closure-internal closure name variables variables))

;;; Given a closure taking n parameters, a constraint name and n variables,
;;; return a new closure constraint.  This is implemented for common values of
;;; n, and is usually the most convenient way to build a closure constraint;
;;; see the examples below.
(e1:define (csp:constraint-nullary unary-closure name)
  (csp:constraint-closure (csp:nullary-closure->list-closure unary-closure)
                          name
                          list:nil))
(e1:define (csp:constraint-unary unary-closure name variable)
  (csp:constraint-closure (csp:unary-closure->list-closure unary-closure)
                          name
                          (list:list variable)))
(e1:define (csp:constraint-binary binary-closure name variable-1 variable-2)
  (csp:constraint-closure (csp:binary-closure->list-closure binary-closure)
                          name
                          (list:list variable-1 variable-2)))
(e1:define (csp:constraint-ternary ternary-closure name variable-1 variable-2
                                   variable-3)
  (csp:constraint-closure (csp:ternary-closure->list-closure ternary-closure)
                          name
                          (list:list variable-1 variable-2 variable-3)))
(e1:define (csp:constraint-quaternary quaternary-closure name variable-1 variable-2
                                      variable-3 variable-4)
  (csp:constraint-closure (csp:quaternary-closure->list-closure quaternary-closure)
                          name
                          (list:list variable-1 variable-2 variable-3 variable-4)))
;;; In the following examples let X, Y and Z be constraint variables.
;;; Example (unary nonzero constraint):
;;; (csp:constraint-unary (e1:lambda (x) (e1:not (fixnum:zero? x)) "nonzero" X))
;;; Example (binary equality constraint):
;;; (csp:constraint-binary (e1:lambda (x y) (whatever:eq? x y) "eq?" X Y))
;;; Example (ternary constraint forcing X < Y + Z):
;;; (csp:constraint-ternary (e1:lambda (x y z) (fixnum:< x (fixnum:+ y z))) "<+" X Y Z))

;;; Given a closure taking n parameters return a functionally equivalent closure
;;; taking a single list parameter holding the n original parameters in the same
;;; order.  The returned closure doesn't check if the list has the right size.
(e1:define (csp:nullary-closure->list-closure nullary-closure)
  (e1:lambda (useless-list)
    (e1:call-closure nullary-closure)))
(e1:define (csp:unary-closure->list-closure unary-closure)
  (e1:lambda (list)
    (e1:call-closure unary-closure
      (list:head list))))
(e1:define (csp:binary-closure->list-closure binary-closure)
  (e1:lambda (list)
    (e1:call-closure binary-closure
      (list:first list)
      (list:second list))))
(e1:define (csp:ternary-closure->list-closure ternary-closure)
  (e1:lambda (list)
    (e1:call-closure ternary-closure
      (list:first list)
      (list:second list)
      (list:third list))))
(e1:define (csp:quaternary-closure->list-closure quaternary-closure)
  (e1:lambda (list)
    (e1:call-closure quaternary-closure
      (list:first list)
      (list:second list)
      (list:third list)
      (list:fourth list))))


;;;;; Constraint syntactic sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The logical connectives directly expressible in constraints constitute a
;;; minimal set, which simplifies the implementation a little.  However it's
;;; convenient for the user to be able to express constraints in a slightly
;;; richer language.

;;; At the present time we spend no effort in normalizing or even trying to
;;; simplify constraints.

(e1:define (csp:constraint-true)
  (csp:constraint-not (csp:constraint-false)))

(e1:define (csp:constraint-and ca cb)
  (csp:constraint-not (csp:constraint-or (csp:constraint-not ca)
                                         (csp:constraint-not cb))))

(e1:define (csp:constraint-xor ca cb)
  (csp:constraint-or (csp:constraint-and ca
                                         (csp:constraint-not cb))
                     (csp:constraint-and (csp:constraint-not ca)
                                         cb)))

(e1:define (csp:constraint-iff ca cb)
  (csp:constraint-not (csp:constraint-xor ca cb)))

;;; It's also convenient to have variadic syntax, at least for associative
;;; operators with a neutral element, where the meaning is intuitive.
(variadic:define-associative csp:constraint-and
                             csp:constraint-and (csp:constraint-true))
(variadic:define-associative csp:constraint-or
                             csp:constraint-or (csp:constraint-false))


;;;;; Constraint printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (csp:write-constraint-to port c)
  (e1:match c
    ((csp:constraint-false)
     (fio:write-to port "#f"))
    ((csp:constraint-closure-internal _ name variables all-variables)
     (fio:write-to port "[" (st name) " ")
     (e1:dolist (variable all-variables)
       (e1:if (list:has? variables variable)
         (print-variable-to port variable)
         (fio:write-to port "#<value>"))
       (fio:write-to port " "))
     (fio:write-to port "]"))
    ((csp:constraint-or ca cb)
     (fio:write-to port "[or ")
     (csp:write-constraint-to port ca)
     (fio:write-to port " ")
     (csp:write-constraint-to port cb)
     (fio:write-to port "]"))
    ((csp:constraint-not ca)
     (fio:write-to port "[not ")
     (csp:write-constraint-to port ca)
     (fio:write-to port "]"))
    (else
     (fio:write-to port "#<unknown constraint>")
     (e1:assert #f))))

(e1:define (csp:write-weighed-constraint-to port wc)
  (e1:let ((weight (csp:weighed-constraint-get-weight wc))
           (constraint (csp:weighed-constraint-get-constraint wc)))
    (fio:write-to port
                  (st (e1:match weight
                        ((csp:weight-soft penalty)
                         (string:append "soft-" (string:fixnum->string penalty) ": "))
                        ((csp:weight-hard)
                         "hard: ")
                        (else
                         (e1:assert #f)))))
    (csp:write-constraint-to port constraint)))

;;; Print a list of constraints.
(e1:define (csp:print-constraints cs)
  (e1:dolist (c (list:reverse cs))
    (csp:write-constraint-to (io:standard-output) c)
    (fio:write "\n")))

;;; Print a list of weighed constraints.
(e1:define (csp:print-weighed-constraints wcs)
  (e1:dolist (wc (list:reverse wcs))
    (csp:write-weighed-constraint-to (io:standard-output) wc)
    (fio:write "\n")))


;;;;; Operations on constraints: involved variables, arity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Given a constraint return the set-as-list of mentioned variables.
(e1:define (csp:constraint->variables c)
  (e1:match c
    ((csp:constraint-false)
     set-as-list:empty)
    ((csp:constraint-closure-internal _ _ vs _)
     (set-as-list:list->set vs))
    ((csp:constraint-or ca cb)
     (set-as-list:union (csp:constraint->variables ca)
                        (csp:constraint->variables cb)))
    ((csp:constraint-not ca)
     (csp:constraint->variables ca))
    (else
     (fio:write "unknown constraint\n")
     (e1:assert #f))))

(e1:define (csp:weighed-constraint->variables wc)
  (e1:let ((c (csp:weighed-constraint-get-constraint wc)))
    (csp:constraint->variables c)))

;;; Given a constraint return the number of variables it involves.
(e1:define (csp:constraint->arity c)
  (list:length (csp:constraint->variables c)))


;;;;; Constraint instantiation: substitute a variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the given constraint with the given variable replaced by the given
;;; value.
(e1:define (csp:constraint-instantiate c x v)
  (e1:match c
    ((csp:constraint-false)
     c)
    ((csp:constraint-closure-internal closure name variables all-variables)
     (e1:if (list:has? variables x)
       (e1:let* ((index (list:find all-variables x))
                 (new-closure
                  (e1:lambda (actuals)
                    (e1:let ((updated-actuals
                              (list:with-nth-element-replaced-safe actuals
                                                                   index
                                                                   v)))
                      (e1:call-closure closure updated-actuals))))
                 (new-name (string:append name "*"))
                 (new-variables (list:without-in-order variables x)))
         (csp:constraint-closure-internal new-closure
                                          new-name
                                          new-variables
                                          all-variables))
       c))
    ((csp:constraint-or ca cb)
     (csp:constraint-or (csp:constraint-instantiate ca x v)
                        (csp:constraint-instantiate cb x v)))
    ((csp:constraint-not ca)
     (csp:constraint-not (csp:constraint-instantiate ca x v)))
    (else
     (fio:write "unknown constraint\n")
     (e1:assert #f))))

;;; Return a list of all the constraints from the given list, possibly not in
;;; the same order, with the given variable replaced by the given value.
(e1:define (csp:constraints-instantiate wcs x v)
  (csp:constraints-instantiate-acc wcs x v list:nil))
(e1:define (csp:constraints-instantiate-acc wcs x v acc)
  (e1:if (list:null? wcs)
    acc
    (e1:let* ((head (list:head wcs))
              (new-head (csp:constraint-instantiate head x v)))
      (csp:constraints-instantiate-acc (list:tail wcs) x v
                                       (list:cons new-head acc)))))

(e1:define (csp:weighed-constraint-instantiate wc x v)
  (e1:let ((w (csp:weighed-constraint-get-weight wc))
           (c (csp:weighed-constraint-get-constraint wc)))
    (csp:weighed-constraint w
                            (csp:constraint-instantiate c x v))))
(e1:define (csp:weighed-constraints-instantiate wcs x v)
  (csp:weighed-constraints-instantiate-acc wcs x v list:nil))
(e1:define (csp:weighed-constraints-instantiate-acc wcs x v acc)
  (e1:if (list:null? wcs)
    acc
    (e1:let* ((head (list:head wcs))
              (new-head (csp:weighed-constraint-instantiate head x v)))
      (csp:weighed-constraints-instantiate-acc (list:tail wcs) x v
                                               (list:cons new-head acc)))))


;;;;; Constraint evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We evaluate constraints containing variables with unknown values; therefore
;;; in general we are not able to assign a Boolean value to a constraint.

;;; When a constraint is evaluated as either true or false, that truth value is
;;; sure.  When our procedures evaluate it to unknown it is possible that some
;;; other finer analysis might reach a true or false result; however even with
;;; this simple evaluation we always reach a true or false value when evaluating
;;; nullary constraints -- and a constraint becomes nullary after all of its
;;; variables have been instantiated.

;;; With the current sum representation it is possible to use these ternary
;;; truth value as ordinary epsilon generalized boolean, with the caveat that
;;; (csp:constraint-truth-false) is taken as false, and the two remaining values
;;; as true.
(e1:define-sum csp:constraint-truth
  (false)
  (true)
  (unknown))

(e1:define (csp:constraint-truth-not ct)
  (e1:match ct
    ((csp:constraint-truth-false)
     (csp:constraint-truth-true))
    ((csp:constraint-truth-true)
     (csp:constraint-truth-false))
    ((csp:constraint-truth-unknown)
     (csp:constraint-truth-unknown))
    (else
     (e1:assert #f))))

(e1:define (csp:constraint-truth-or cta ctb)
  (e1:match cta
    ((csp:constraint-truth-false)
     ctb)
    ((csp:constraint-truth-true)
     (csp:constraint-truth-true))
    ((csp:constraint-truth-unknown)
     (e1:match ctb
       ((csp:constraint-truth-false)
        (csp:constraint-truth-unknown))
       ((csp:constraint-truth-true)
        (csp:constraint-truth-true))
       ((csp:constraint-truth-unknown)
        (csp:constraint-truth-unknown))
       (else
        (e1:assert #f))))
    (else
     (e1:assert #f))))

(e1:define (csp:eval-constraint c)
  (e1:match c
    ((csp:constraint-false)
     (csp:constraint-truth-false))
    ((csp:constraint-closure-internal closure _ variables _)
     (e1:cond ((e1:not (list:null? variables))
               (csp:constraint-truth-unknown))
              ((e1:call-closure closure list:nil)
               (csp:constraint-truth-true))
              (else
               (csp:constraint-truth-false))))
    ((csp:constraint-or ca cb)
     (csp:constraint-truth-or (csp:eval-constraint ca)
                              (csp:eval-constraint cb)))
    ((csp:constraint-not ca)
     (csp:constraint-truth-not (csp:eval-constraint ca)))
    (else
     (fio:write "unknown constraint\n")
     (e1:assert #f))))

;;; A constraint is viable iff its evaluation is not known to be false.
(e1:define (csp:constraint-viable? c)
  (e1:match (csp:eval-constraint c)
    ((csp:constraint-truth-false)
     #f)
    ((or (csp:constraint-truth-true)
         (csp:constraint-truth-unknown))
     #t)
    (else
     (fio:write "unknown constraint truth value\n")
     (e1:assert #f))))

;;; Are all the constraints in the given list viable?
(e1:define (csp:constraints-viable? cs)
  (e1:cond ((list:null? cs)
            #t)
           ((csp:constraint-viable? (list:head cs))
            (csp:constraints-viable? (list:tail cs)))
           (else
            #f)))


;;;;; Viability of weighed constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (csp:weighed-constraint-viable? wc)
  (e1:match (csp:weighed-constraint-get-weight wc)
    ((csp:weight-soft penalty)
     #t)
    ((csp:weight-hard)
     (e1:let ((constraint (csp:weighed-constraint-get-constraint wc)))
       (csp:constraint-viable? constraint)))
    (else
     (e1:assert #f))))

;;; Are all the weighed constraints in the given list viable?
(e1:define (csp:weighed-constraints-viable? wcs)
  (e1:cond ((list:null? wcs)
            #t)
           ((csp:weighed-constraint-viable? (list:head wcs))
            (csp:weighed-constraints-viable? (list:tail wcs)))
           (else
            #f)))


;;;;; Evaluation of weighed constraints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; When evaluating one weighed constraint there are three possible results:
;;; * a hard constraint was violated:
;;;   The result is (csp:constraint-truth-false);
;;; * the constraint is undecided:
;;;   The result is (csp:constraint-truth-unknown);
;;; * no hard constraint was violated:
;;;   The result is a bundle: (csp:constraint-truth-true), and a penalty.
(e1:define (csp:eval-weighed-constraint wc)
  (e1:let* ((weight (csp:weighed-constraint-get-weight wc))
            (constraint (csp:weighed-constraint-get-constraint wc)))
    (e1:match (csp:eval-constraint constraint)
      ((csp:constraint-truth-true)
       ;; The constraint is satisfied, so there's no penalty in any case.
       (e1:bundle (csp:constraint-truth-true) 0))
      ((csp:constraint-truth-unknown)
       (e1:bundle (csp:constraint-truth-unknown) 0))
      ((csp:constraint-truth-false)
       (e1:match weight
         ((csp:weight-hard)         (e1:bundle (csp:constraint-truth-false) 0))
         ((csp:weight-soft penalty) (e1:bundle (csp:constraint-truth-true) penalty))
         (else                      (e1:assert #f)))))))


;;; When evaluating a list of weighed constraints there are two possible
;;; results:
;;; * no hard constraint is known to be violated, and there is some
;;;   penalty to pay (possibly zero) for violated soft constraints;
;;;   "undecided" constraints mentioning unassigned variables may still
;;;   be open and are to be returned, for later reconsideration.
;;; * at least one hard constraint is violated, and there is no need for
;;;   more information.
;;; In the first case csp:eval-weighed-constraints returns a bundle
;;; containing #t, the remaining weighed constraint list in an unspecified
;;; order, and the penalty.
;;; In the second case it returns #f, a singleton list with a hard false
;;; constraint, and some unspecified penalty.
(e1:define (csp:eval-weighed-constraints wcs)
  (e1:let loop ((wcs wcs)
                (undecideds list:nil)
                (penalty 0))
    (e1:if (list:null? wcs)
      (e1:bundle #t undecideds penalty)
      (e1:let* ((wc (list:head wcs))
                ((wc-result wc-penalty) (csp:eval-weighed-constraint wc)))
        (e1:match wc-result
          ((csp:constraint-truth-false)
           (e1:bundle #f csp:unsatisfiable-wcs 0))
          ((csp:constraint-truth-true)
           (loop (list:tail wcs)
                 undecideds
                 (fixnum:+ penalty wc-penalty)))
          ((csp:constraint-truth-unknown)
           (loop (list:tail wcs)
                 (list:cons wc undecideds)
                 penalty))
          (else
           (e1:assert #f)))))))

(e1:define (csp:print-weighed-constraints-evaluation wcs)
  (e1:let (((result undecideds penalty) (csp:eval-weighed-constraints wcs)))
    "Constraint result: " (fio:write (b result))
    (e1:when result
      (fio:write ",  penalty " (i penalty) ",  "
                 (i (list:length undecideds)) " remaining constraints."))
    (fio:write "\n")))


;;;;; Constraint propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Node Consistency, Arc Consistency and Path consistency are all well
;;; explained in A.K. Mackworth, "Consistency in networks of relations",
;;; Artificial Intelligence, 8:99-118, 1977.  It's a beautifully written and
;;; easily readable paper, back from a less pretentious era.

;;; Given a <variable, domain> alist and a list of weighed constraints,
;;; return a bundle of the new <variable, domain> alist and constraint list
;;; as obtained by constraint propagation.
(e1:define (csp:propagate-weighed-constraints variables wcs)
  #;(e1:bundle variables wcs)
  (e1:let* (((variables-1 wcs-1) (csp:node-consistency variables wcs))
            ((variables-2 wcs-2) (csp:arc-consistency variables-1 wcs-1)))
    (e1:bundle variables-2 wcs-2)))

;;; Given a <variable, domain> alist and a list of weighed constraints,
;;; return a bundle of the new <variable, domain> alist and constraint list
;;; as obtained by imposing node consistency.
(e1:define (csp:node-consistency variables wcs)
  (e1:let ((new-variables (box:make alist:nil))
           (weighed-constraints (box:make list:nil))
           (satisfiable (box:make #t))
           (variable-to-wcs (unboxed-hash:make)))
    ;; Make a table mapping each variable to the constraints it appears in.
    ;; This avoids scanning irrelevant constraints when working on the domain of
    ;; one variable.
    ;; This makes a set-of-list of weighed constraints, which would be a
    ;; questionable idea in general but is okay for node consistency, as
    ;; weighed constraints aren't made, destroyed or changed here: we can just
    ;; compare them by identity.
    (e1:dolist (wc wcs)
      (e1:dolist (variable (csp:weighed-constraint->variables wc))
        (unboxed-hash:add-to-set-as-list! variable-to-wcs variable wc)))
    (e1:doalist (variable domain variables)
      (e1:when (box:get satisfiable)
        (unboxed-hash:ensure-non-empty! variable-to-wcs variable)
        (e1:let ((new-domain (box:make list:nil))
                 (variable-wcs (unboxed-hash:get variable-to-wcs variable)))
          ;; Try and instantiate the constraints on every possible value of the
          ;; variable.  Only add to the new domain the values such that the
          ;; constraints remain viable.
          (e1:dolist (value domain)
            (e1:let ((variable-wcs (csp:weighed-constraints-instantiate variable-wcs
                                                                        variable
                                                                        value)))
              (e1:when (csp:weighed-constraints-viable? variable-wcs)
                (box:set! new-domain (list:cons value (box:get new-domain))))))
          (box:set! new-variables (alist:bind (box:get new-variables)
                                              variable
                                              (box:get new-domain)))
          (e1:when (list:null? (box:get new-domain))
            (box:set! satisfiable #f)))))
    ;; If we emptied a domain there is no point in keeping our constraints.
    ;; Let's replace them all with csp:unsatisfiable-wcs, which will be
    ;; immediately recognized as unsatisfiable; arc consistency will also
    ;; terminate immediately.
    (e1:let ((wcs (e1:if (box:get satisfiable)
                    wcs
                    csp:unsatisfiable-wcs)))
      (e1:bundle (box:get new-variables)
                 wcs))))

;;; Given a <variable, domain> alist and a list of weighed constraints,
;;; return a bundle of the new <variable, domain> alist and constraint list
;;; as obtained by imposing arc consistency.
(e1:define (csp:arc-consistency variables wcs)
  ;; FIXME: really implement arc concistency.
  (e1:bundle variables wcs))


;;;;; Evaluation penalty cutoff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Is the given penalty preferable to the given solution?
(e1:define (csp:penalty-better-than-solution? penalty solution)
  (e1:match solution
    ((csp:solution-failure)
     #t)
    ((csp:solution-success old-penalty _)
     (fixnum:< penalty old-penalty))
    (else
     (e1:assert #f))))


;;;;; Solver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum csp:solution-type
  (first)
  (best))

;;; The imperative state kept during search.  Notice that it's important to keep
;;; bounds information in some destructive state, as this information remains
;;; relevant to callers even after backtracking into far away branches.
(e1:define-record csp:state
  solution-type steps best-known)

(e1:define (csp:first-solution problem)
  (csp:solve problem
             (csp:solution-type-first)))
(e1:define (csp:best-solution problem)
  (csp:solve problem
             (csp:solution-type-best)))

;;; Increment the number of steps and return the new version.
(e1:define (csp:bump-state! state)
  (e1:let* ((old-steps (csp:state-get-steps state))
            (new-steps (fixnum:1+ old-steps)))
    (csp:state-set-steps! state new-steps)
    new-steps))

(e1:define (csp:solve problem type)
  (e1:let* ((variables (csp:problem-get-variables problem))
            (wcs (csp:problem-get-weighed-constraints problem))
            ((variables wcs) (csp:propagate-weighed-constraints variables wcs)))
    ;; The next test is required, as csp:choose-variable-and-solve assumes the
    ;; constraints to be viable.  Re-checking at entry would be inefficient,
    ;; since csp:choose-value-and-solve only calls if constraints are viable.
    (e1:if (csp:weighed-constraints-viable? wcs)
      (e1:let* ((assignment alist:nil)
                (penalty 0)
                (best-known (csp:solution-failure))
                (state (csp:state type 0 best-known)))
        (csp:choose-variable-and-solve variables
                                       wcs
                                       assignment
                                       penalty
                                       state)
        #;(fio:write "Searched for " (i (csp:state-get-steps state)) " steps.\n")
        (csp:state-get-best-known state))
      (csp:solution-failure))))

(e1:define (csp:choose-variable-and-solve variables wcs assignment penalty state)
  #;(csp:show-weighed-constraints wcs)
  ;; This should only be reached when the wcs are viable.  See the comment
  ;; before the call in csp:solve .
  #;(csp:print-weighed-constraints-evaluation wcs)
  #;(print-variables variables)
  (e1:cond ((e1:not (csp:penalty-better-than-solution?
                        penalty (csp:state-get-best-known state)))
            #;(fio:write "  ! penalty cutoff!\n")
            (csp:solution-failure))
           ((list:null? variables)
            ;; Found a solution.  If we were just looking for the first one then
            ;; we're done; otherwise we remember the solution, but fail in order
            ;; to backtrack and search more -- unless the penalty is zero, in
            ;; which case no other solution can be better than what we found.
            (e1:let ((this-solution (csp:solution-success penalty assignment)))
              #;(fio:write "Found a solution with penalty " (i penalty)
                         " after " (i (csp:state-get-steps state)) " steps: ")
              #;(print-assignment assignment)
              ;; Remember that we found a solution better than the best
              ;; previously known.  This will let branch-and-bound prune more.
              (csp:state-set-best-known! state this-solution)
              (e1:match (csp:state-get-solution-type state)
                ((csp:solution-type-first)
                 this-solution)
                ((csp:solution-type-best)
                 (e1:if (fixnum:zero? penalty) this-solution (csp:solution-failure)))
                (else
                 (e1:assert #f)))))
           (else
            (e1:let* ((variable (csp:select-variable variables wcs))
                      (domain (alist:lookup variables variable)))
              (csp:choose-value-and-solve variable
                                          domain
                                          (alist:unbind-one variables variable)
                                          wcs
                                          assignment
                                          penalty
                                          state)))))

(e1:define (csp:choose-value-and-solve variable domain variables wcs assignment penalty state)
  (e1:let loop ((choices (csp:instantiate variable domain variables wcs assignment penalty)))
    (e1:if (list:null? choices)
      (csp:solution-failure)
      (e1:match (list:head choices)
        ((tuple variables assignment penalty wcs)
         (csp:bump-state! state)
         #;(e1:let ((value (alist:lookup assignment variable)))
           (fio:write "[(" (i (csp:state-get-steps state)) ") Try " (sy variable) " = " (sy value) "]\n"))
         (e1:let ((solution (csp:choose-variable-and-solve variables
                                                           wcs
                                                           assignment
                                                           penalty
                                                           state)))
           (e1:match solution
             ((csp:solution-success _ _)
              solution)
             ((csp:solution-failure)
              (loop (list:tail choices)))
             (else
              (e1:assert #f)))))))))


;;;;; Value choice heuristic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Instantiate the variable over the constraints as every possible element of
;;; its domain.  Return a list of <new variables, extended assignment, new
;;; penalty, remaining constraint list> tuples to be scanned in order.
;;; Constraints within the returned list are already propagated.  The advantage
;;; of this procedure compared to just intantiating values one after the other
;;; is that the list is sorted by increasing penalties, which should make the
;;; search process more informed.
(e1:define (csp:instantiate variable domain variables wcs assignment penalty)
  (e1:let ((unsorted-result
            (csp:instantiate-unsorted variable domain variables wcs assignment penalty)))
    (list:sort unsorted-result
               ;; Prefer the tuple with a smaller penalty.
               (e1:lambda (t-a t-b)
                 ;; FIXME: fixnum:< is the right thing, but when debugging I
                 ;; sometimes reverse it, to stress backtracking.
                 (fixnum:< (tuple:get t-a 2) (tuple:get t-b 2))))))

(e1:define (csp:instantiate-unsorted variable domain variables wcs assignment penalty)
  (e1:let loop ((res list:nil)
                (values domain))
    (e1:cond ((list:null? values)
              res)
             (bind* (value (list:head values))
                    (wcs
                     (csp:weighed-constraints-instantiate wcs variable value))
                    ((variables wcs)
                     (csp:propagate-weighed-constraints variables wcs))
                    ((viable wcs penalty-delta)
                     (csp:eval-weighed-constraints wcs))
                    (more-values (list:tail values)))
             (viable
              (loop (list:cons (tuple:make variables
                                           (alist:bind assignment variable value)
                                           (fixnum:+ penalty penalty-delta)
                                           wcs)
                               res)
                    more-values))
             (else
              (loop res more-values)))))


;;;;; Variable choice heuristic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Select and return the next variable to assign from the given non-empty
;;; <variable, domain> alist, knowing that the given weighed constraints apply.
;;; Every already-selected variable should have already been removed or
;;; substituted away in both the alist and the constraints.
(e1:define (csp:select-variable variables wcs)
  (e1:assert (e1:not (alist:null? variables)))
  ;; Use the Minimum Remaining Values heuristic to find the set of variables with
  ;; the smallest domains.
  #;(cons:car (list:head variables))
  (e1:let ((mrv-variables (csp:minimum-remaining-values variables)))
    (e1:assert (e1:not (list:null? mrv-variables)))
    ;; If MRV yielded more than one variable, use the Maximum Degree heuristic
    ;; as a tie-breaker.  The variable involved in the highest number of
    ;; constraints should be selected first.
    (list:head (e1:if (list:null? (list:tail mrv-variables))
                 mrv-variables
                 (csp:maximum-degree mrv-variables wcs)))))

;;; Given a non-empty <variable, domain> alist, return the set-as-list of
;;; variables with minimal-sized domains.  This is the Minimum Remaining
;;; Values heuristic.
(e1:define (csp:minimum-remaining-values variables)
  (e1:assert (e1:not (alist:null? variables)))
  (e1:let ((first (list:head variables)))
    (e1:let loop ((best-variables (list:list (cons:car first)))
                  (best-domain-size (list:length (cons:cdr first)))
                  (alist (list:tail variables)))
      (e1:if (alist:null? alist)
        best-variables
        (e1:let* ((first (list:head alist))
                  (variable (cons:car first))
                  (variable-domain-size (list:length (cons:cdr first)))
                  ((best-variables best-domain-size)
                   (e1:cond ((fixnum:= variable-domain-size best-domain-size)
                             (e1:bundle (list:cons variable best-variables)
                                        best-domain-size))
                            ((fixnum:< variable-domain-size best-domain-size)
                             (e1:bundle (list:list variable) variable-domain-size))
                            (else
                             (e1:bundle best-variables best-domain-size)))))
          (loop best-variables best-domain-size (list:tail alist)))))))

;;; Return the number of the set-as-list structures from the given list
;;; having the given element.
(e1:define (csp:in-how-many element sets-as-lists)
  (csp:in-how-many-acc element sets-as-lists 0))
(e1:define (csp:in-how-many-acc element sets-as-lists acc)
  (e1:cond ((list:null? sets-as-lists)
            acc)
           ((set-as-list:has? (list:head sets-as-lists) element)
            (csp:in-how-many-acc element
                                 (list:tail sets-as-lists)
                                 (fixnum:1+ acc)))
           (else
            (csp:in-how-many-acc element
                                 (list:tail sets-as-lists)
                                 acc))))

;;; Given a list of variables (*not* an alist) and a list of weighed
;;; constraints, return the set-as-list of variables involved in the maximum
;;; number of constraints.  This is the Maximum-Degree heuristic.
(e1:define (csp:maximum-degree variable-names wcs)
  (e1:assert (e1:not (alist:null? variable-names)))
  #;variable-names
  ;; For each weighed constraint compute the set of variables it involves.
  (e1:let* ((variable-sets
             (list:map (e1:lambda (wc) (csp:weighed-constraint->variables wc))
                       wcs))
            (first-variable (list:head variable-names))
            (first-degree (csp:in-how-many first-variable variable-sets)))
    ;; Scan variable names, keeping track of the set of variables having maximum
    ;; degree.
    (e1:let loop ((best-variables (list:list first-variable))
                  (best-degree first-degree)
                  (rest (list:tail variable-names)))
      #;(fio:write "* ")
      #;(e1:dolist (best best-variables)
        (fio:write (sy best) " "))
      #;(fio:write ": degree "(i best-degree) "\n")
      (e1:if (list:null? rest)
        best-variables
        (e1:let* ((next-variable (list:head rest))
                  (next-degree (csp:in-how-many next-variable variable-sets))
                  ((best-variables best-degree)
                   (e1:cond ((fixnum:= next-degree best-degree)
                             (e1:bundle (list:cons next-variable best-variables)
                                        best-degree))
                            ((fixnum:> next-degree best-degree)
                             (e1:bundle (list:list next-variable)
                                        next-degree))
                            (else
                             (e1:bundle best-variables best-degree)))))
          (loop best-variables best-degree (list:tail rest)))))))


;;;;; Ideas for the future
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I should represent constraints as a (hyper-)graph, and check whether it's
;;; connected -- Very often it will not be.  Each connected component represents
;;; a set of constraints that can be solved independently, which dramatically
;;; reduces complexity.  I could even update the variable selection heuristic to
;;; choose (possibly as a tie breaker only -- current heuristics are very
;;; effective and I don't want to lose them) a variable cutting the constraint
;;; graph into largest-sized subgraphs.
;;; Improvements can be very dramatic: from O(b^(m + n)) to O(max{b^m, b^n}).
;;; In case of an optimal cut into two subgraphs, from O(b^2m) to O(b^m).


;;;;; Constraint hypergraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (csp:weighed-constraints->hypergraph wcs)
  (e1:let ((res (string-hash:make)))
    (e1:dolist (wc wcs)
      (e1:let* ((vsl (csp:weighed-constraint->variables wc))
                (vs (vector:list->vector vsl)))
        (vector:sort! vs (e1:lambda (va vb) (variable:<= va vb)))
        (string-hash:add-to-set-as-list! res vs wc)))
    res))

(e1:define (csp:show-weighed-constraints wcs)
  (e1:let* ((file-name "/tmp/wcs.dot")
            (file (io:open-file file-name io:write-mode)))
    (fio:write-to file "graph {
  splines=true;
  pack = true;
  fizedsize=true;
  overlap = false;
")
    (e1:let ((hg (csp:weighed-constraints->hypergraph wcs))
             (constraint-no (string-hash:make)))
      (e1:dohash (vs wcs hg)
        (e1:if (string-hash:has? constraint-no vs)
          (string-hash:set! constraint-no vs (fixnum:1+ (string-hash:get constraint-no vs)))
          (string-hash:set! constraint-no vs 1)))
      (e1:dohash (vs wcs hg)
        (csp:write-variables-as-node-to file vs)
        (fio:write-to file " [label=\""
                      #;(i (string-hash:get constraint-no vs))
                      "\", width=0.15, shape=\"square\"];\n")
        (e1:dovector (v vs)
          (fio:write-to file (sy v) " [shape=\"circle\"];\n")
          (fio:write-to file (sy v) " -- ")
          (csp:write-variables-as-node-to file vs)
          (fio:write-to file ";\n")
          )
        ))
    (fio:write-to file "}\n")
    (io:close-file file)
    (unix:system
     (string:append "neato " file-name " -T pdf -o /tmp/b.pdf && xpdf /tmp/b.pdf"))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (csp:write-variables-as-node-to file vs)
  (fio:write-to file "_c_")
  (e1:dovector-index (v i vs)
    (print-variable-to file v)
    (e1:unless (fixnum:= i (fixnum:1- (vector:length vs)))
      (fio:write-to file "_"))))

(e1:define (print-solution s)
  (e1:match s
    ((csp:solution-failure)
     (fio:write "failure\n"))
    ((csp:solution-success penalty assignment)
     (fio:write "success (penalty " (i penalty) "):\n  ")
     (print-assignment assignment))
    (else
     (e1:assert #f))))

(e1:define (print-assignment assignment)
  (fio:write "{ ")
  (e1:doalist (variable value assignment)
    (print-variable-to (io:standard-output) variable)
    (fio:write ":")
    (print-value-to (io:standard-output) value)
    (fio:write " "))
  (fio:write "}\n"))

(e1:define (print-value-to file value)
  (fio:write-to file (sy value)))
(e1:define (print-variable-to file variable)
  (fio:write-to file (sy variable)))
(e1:define (print-variables variables)
  (e1:doalist (variable domain variables)
    (fio:write "#")
    (print-variable-to (io:standard-output) variable)
    (fio:write "=" (i (list:length domain)) " "))
  (fio:write "\n"))
