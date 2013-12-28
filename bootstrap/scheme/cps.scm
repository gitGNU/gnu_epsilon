;;;;; This is epsilon with some -*- Scheme -*-.
;;;;; Unfinished CPS support.

;;;;; Copyright (C) 2012 Université Paris 13
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


;;;;; Continuation-Passing Style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We add let/cc to the language, providing ways to build let/cc
;;; expressions from code, via a procedure, and from syntax, via a
;;; macro.  All the complexity of this section is ultimately aimed at
;;; transforming away let/cc, so that control is simulated with
;;; closures.  Another transformation running later gets rid of
;;; closures.
(e1:toplevel (sum:extend-open e0:expression
               (let/cc handle continuation-name body)))

(e1:define (e1:let/cc* continuation-name body)
  (e0:expression-let/cc (e0:fresh-handle) continuation-name body))

(e1:define-macro (e1:let/cc captured-continuation-name . body-forms)
  (sexpression:inject-expression
    (e1:let/cc* (sexpression:eject-symbol captured-continuation-name)
                (e1:macroexpand `(e1:begin ,@body-forms)))))

;;;;;;;;;;;;;BEGIN
;; (e1:toplevel (sum:extend-open e0:expression
;;                (non-cps handle body)))
;; (e1:define (e1:non-cps* body)
;;   (e0:expression-non-cps (e0:fresh-handle) body))
;; (e1:define-macro (e1:non-cps . body-forms)
;;   (sexpression:inject-expression
;;     (e1:non-cps* (e1:macroexpand `(e1:begin ,@body-forms)))))

;; (e1:toplevel (sum:extend-open e0:expression
;;                (jump handle continuation-expression actuals)))
;; (e1:define (e1:jump* continuation-expression actuals)
;;   (e0:expression-jump (e0:fresh-handle) continuation-expression actuals))
;; (e1:define-macro (e1:jump continuation-expression . actuals)
;;   (sexpression:inject-expression
;;     (e1:jump* (e1:macroexpand continuation-expression)
;;               (e1:macroexpand-sexpressions actuals))))
(e1:define cps:unused-expression (e0:value* (e0:value 123456789))) ;; Fixme: change after debugging
;;; FIXME: Later (maybe as a separate pass?), try to remove redundant
;;; lets as well.  Or shall I do *both* optimizations in the
;;; general-purpose rewriting optimizer, and get rid of this
;;; code?
(e1:define (e1:jump* continuation-expression actuals)
  (e1:naif-jump* continuation-expression actuals))
  ;;(e1:less-naif-jump* continuation-expression actuals))

(e1:define (e1:naif-jump* continuation-expression actuals)
  (e1:jump-to-nontrivial-continuation* continuation-expression actuals))
(e1:define (e1:jump-to-nontrivial-continuation* continuation-expression actuals)
  (e1:call-closure* continuation-expression
                    (list:cons cps:unused-expression actuals)))
(e1:define (e1:less-naif-jump* continuation-expression actuals)
  (e1:if (e0:expression-lambda? continuation-expression)
    ;; We can beta-reduce at generation time, turning the application
    ;; of a known lambda into a let.  Notice that we can ignore the
    ;; first formal, which continuations dont't use and is only there
    ;; to make all closures take an initial continuation argument for
    ;; compatibility.
    (cps:make-block* (list:tail (e0:expression-lambda-get-formals continuation-expression))
                     actuals
                     (e0:expression-lambda-get-body continuation-expression))
    ;; The continuation expression is not a lambda: I have to generate an application
    (e1:jump-to-nontrivial-continuation* continuation-expression actuals)))
(e1:define (cps:make-block* formals actuals body)
  (e1:cond ((e1:and (list:null? formals)
                    (list:null? actuals))
            body)
           ((list:null? formals) ;; more actuals than formals
            (e1:if (cps:trivial-expression? (list:head actuals))
              (cps:make-block* list:nil (list:tail actuals) body)
              (e0:let* list:nil (list:head actuals)
                (cps:make-block* list:nil (list:tail actuals) body))))
           ((list:null? actuals) ;; more formals than actuals: the generated code will fail if reached
            (e0:let* formals (e0:bundle* list:nil) body))
           (else ;; formals and actuals both nonempty
            (e0:let* (list:list (list:head formals))
                     (list:head actuals)
              (cps:make-block* (list:tail formals) (list:tail actuals) body)))))
;;; A trivial expression in this context is an expression with no
;;; observable side effects, that can be optimized away and never
;;; evaluated.  FIXME: I can add more case, if it's worth the trouble:
;;; for example, bundles containing only trivial expressions are
;;; trivial, non-side-effecting primitive calls with trivial
;;; parameters are trivial.
(e1:define (cps:trivial-expression? e)
  (e1:or (e0:expression-variable? e)
         (e0:expression-value? e)
         (e0:expression-lambda? e)))
(e1:define cps:unused-formal (symbol:fresh))
(e1:define (e1:continuation* formals body)
  (e1:lambda* (list:cons cps:unused-formal formals) body))
;; ;;;;;;;;;;;;;END

;;; We also provide a traditional call/cc operator, which is trivial
;;; to rewrite into let/cc.  The call/cc parameter must evaluate to a
;;; closure.
(e1:define-macro (e1:call/cc closure-expression)
  (e1:let* ((continuation-name (sexpression:fresh-symbol)))
    `(e1:let/cc ,continuation-name
       (e1:call-closure ,closure-expression ,continuation-name))))

;;; Of course this can't work at this point because closure-conversion
;;; doesn't know about call/cc.  FIXME: move down and re-enable this
;;; after installing the CPS transform, which has to run *before*
;;; closure-conversion.
;; ;;; I don't personally use this style, but somebody might like to work
;; ;;; on call/cc as a value; of course it will be seen as a closure.
;; ;;; This also lets the user apply call/cc by an explicit e1:call-closure.
;; (e1:define e1:call/cc
;;   (e1:lambda (k) (e1:call/cc k)))

;; (e1:define cps:trivial-continuation-expression
;;   (e1:lambda* (list:list (e0:value x))
;;               (e0:variable* (e0:value x))))
;; (e1:define cps:k0-expression cps:trivial-continuation-expression) ;; FIXME: remove this debugging alias
;; (e1:define cps:k0 (e1:lambda (x) x))

;;; Of course my continuation can have different in-dimensions:
;;; (e0:let (a b c) (e1:let/cc k (e1:call-closure k 1 2 3)) 75)


;; (e1:toplevel (sum:define cps:dimension
;;                (any)
;;                (exactly number)))

;; (e1:define (cps:dimension-exactly-fixnum? d fixnum)
;;   (e1:and (cps:dimension-exactly? d)
;;           (fixnum:= (cps:dimension-exactly-get-number d) fixnum)))
;; (e1:define (cps:dimension-<fixnum? d fixnum)
;;   (e1:and (cps:dimension-exactly? d)
;;           (fixnum:< (cps:dimension-exactly-get-number d) fixnum)))

;; (e1:define cps:dimension-one
;;   (cps:dimension-exactly 1))

;; (e1:define (cps:dimension-zero? d)
;;   (cps:dimension-exactly-fixnum? d 0))
;; (e1:define (cps:dimension-one? d)
;;   (cps:dimension-exactly-fixnum? d 1))

;;; FIXME: move and generalize
(e1:define (cps:warn string)
  (io:write-string (io:standard-error) "WARNING: ")
  (io:write-string (io:standard-error) string)
  (io:write-string (io:standard-error) ".\n"))

;; ;;; An extension of list:list-elements using a dimension instead of a fixnum.
;; (e1:define (cps:first-elements dimension list)
;;   (e1:if (cps:dimension-any? dimension)
;;     list
;;     (list:first-elements (cps:dimension-exactly-get-number dimension) list)))

;; ;;; An extension of list:list-elements using a dimension instead of a fixnum.
;; (e1:define (cps:first-elements-up-to dimension result-no list)
;;   (e1:cond ((cps:dimension-any? dimension)
;;             list)
;;            ((cps:dimension-<fixnum? dimension result-no)
;;             (list:first-elements (cps:dimension-exactly-get-number dimension) list))
;;            (else ;; this will fail at run time if dimension is larger than result-no
;;             list)))

;;; FIXME: just define one closure per arity when needed, and return
;;; variable references to possibly already-existing objects.  This
;;; should make the generated code more compact and understandable
;;; when debugging; but could it pollute the instruction cache and
;;; screw up branch prediction?  Maybe.  I should investigate these
;;; phenomena after I finish the native compiler.
(e1:define (cps:trivial-continuation* in-and-out-dimension)
  (e1:let* ((formals (symbol:fresh-symbols in-and-out-dimension))
            ;; Don't generate a useless bundle: this makes reading the code easier
            (body (e1:if (fixnum:= (list:length formals) 1)
                    (e0:variable* (list:head formals))
                    (e0:bundle* (cps:variables* formals)))))
    (e1:continuation* formals body)))

(e1:define trivial-continuation0* (e1:continuation* list:nil (e0:bundle* list:nil)))
(e1:define trivial-continuation0 (e0:unbundle (e0:eval-ee (transform:transform-expression trivial-continuation0*))))
(define trivial-continuation0 (e1:toplevel trivial-continuation0))
(e1:define trivial-continuation1* (e1:continuation* (e1:value-list x) (e0:variable* (e0:value x))))
(e1:define trivial-continuation1 (e0:unbundle (e0:eval-ee (transform:transform-expression trivial-continuation1*))))
(define trivial-continuation1 (e1:toplevel trivial-continuation1))


;;; FIXME: use this
(e1:define (cps:transform-nonescaping-tail e k)
  (e1:cond ((e0:expression-variable? e)
            (e1:let* ((variable-name (e0:expression-variable-get-name e)))
              (e1:jump* k (list:list (e0:variable* variable-name)))))
           ((e0:expression-value? e)
            (e1:let* ((value-content (e0:expression-value-get-content e)))
              (e1:jump* k (list:list (e0:value* value-content)))))
           ((e0:expression-bundle? e)
            (e1:let* ((items (e0:expression-bundle-get-items e)))
              (e1:jump* k (cps:transform-nonescaping-nontail-expressions items))))
           ((e0:expression-primitive? e)
            (e1:let* ((primitive-name (e0:expression-primitive-get-name e))
                      (actuals (e0:expression-primitive-get-actuals e))
                      (primitive-out-dimension (state:primitive-get-out-dimension primitive-name))
                      (result-names (symbol:fresh-symbols primitive-out-dimension)))
              (e0:let* result-names
                       (e0:primitive* primitive-name (cps:transform-nonescaping-nontail-expressions actuals))
                       (e1:jump* k (cps:variables* result-names)))))
           ((e0:expression-let? e)
            (e1:let* ((bound-variables (e0:expression-let-get-bound-variables e))
                      (bound-variable-no (list:length bound-variables))
                      (bound-expression (e0:expression-let-get-bound-expression e))
                      (body (e0:expression-let-get-body e)))
              (e0:let* bound-variables
                       (cps:transform-nonescaping-nontail bound-expression bound-variable-no)
                       (cps:transform-nonescaping-tail body k))))
           ((e0:expression-call? e)
            (e1:let* ((procedure-name (e0:expression-call-get-procedure-name e))
                      (actuals (e0:expression-call-get-actuals e)))
              (e0:call* procedure-name
                        (list:cons k (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-call-indirect? e)
            (e1:let* ((procedure-expression (e0:expression-call-indirect-get-procedure-expression e))
                      (actuals (e0:expression-call-indirect-get-actuals e)))
              (e0:call-indirect* (cps:transform-nonescaping-nontail procedure-expression 1)
                                 (list:cons k (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-if-in? e)
            (e1:let* ((discriminand (e0:expression-if-in-get-discriminand e))
                      (values (e0:expression-if-in-get-values e))
                      (then-branch (e0:expression-if-in-get-then-branch e))
                      (else-branch (e0:expression-if-in-get-else-branch e))
                      (shared-continuation-name (symbol:fresh)))
              ;; Avoid duplicating references to k
              (e0:let* (list:list shared-continuation-name)
                       k
                       (e0:if-in* (cps:transform-nonescaping-nontail discriminand 1) values
                         (cps:transform-nonescaping-tail then-branch (e0:variable* shared-continuation-name))
                         (cps:transform-nonescaping-tail else-branch (e0:variable* shared-continuation-name))))))
           ((e0:expression-fork? e)
            (e1:let* ((procedure-name (e0:expression-fork-get-procedure-name e))
                      (actuals (e0:expression-fork-get-actuals e))
                      (actual-no (list:length actuals))
                      (flipper-name (cps:generate-flipper procedure-name (fixnum:+ 2 actual-no))))
              (e1:jump* k
                        (list:list (e0:fork* flipper-name
                                             (list:cons (cps:trivial-continuation* 1)
                                                        (cps:transform-nonescaping-nontail-expressions actuals)))))))
           ((e0:expression-join? e)
            (e1:let* ((future-expression (e0:expression-join-get-future e)))
              (e1:jump* k
                        (list:list (e0:join* (cps:transform-nonescaping-nontail future-expression 1))))))
           ((e0:expression-lambda? e)
            (e1:let* ((formals (e0:expression-lambda-get-formals e))
                      (continuation-formal-name (symbol:fresh))
                      (body (e0:expression-lambda-get-body e)))
              (e1:jump* k
                        (list:list (e1:lambda* (list:cons continuation-formal-name formals)
                                     (cps:transform-nonescaping-tail body (e0:variable* continuation-formal-name)))))))
           ((e0:expression-call-closure? e)
            (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
                      (actuals (e0:expression-call-closure-get-actuals e)))
              (e1:call-closure* (cps:transform-nonescaping-nontail closure-expression 1)
                                (list:cons k
                                           (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-let/cc? e)
            (e1:let* ((continuation-name (e0:expression-let/cc-get-continuation-name e))
                      (body (e0:expression-let/cc-get-body e)))
              ;; If e is nonescaping then the continuation is not used:
              (cps:transform-nonescaping-nontail body k)))
           ;; ((e0:expression-non-cps? e)
           ;;  (e1:let* ((body (e0:expression-non-cps-get-body e)))
           ;;    (e1:error "cps:transform-nonescaping-tail: found a CPS-shielded subexpression")))
           (else
            (e1:error "cps:transform-nonescaping-tail: unknown extended or invalid expression"))))

(e1:define (cps:transform-nonescaping-nontail e out-dimension)
  (e1:cond ((e0:expression-variable? e)
            (e1:let* ((variable-name (e0:expression-variable-get-name e)))
              (e0:variable* variable-name)))
           ((e0:expression-value? e)
            (e1:let* ((value-content (e0:expression-value-get-content e)))
              (e0:value* value-content)))
           ((e0:expression-bundle? e)
            (e1:let* ((items (e0:expression-bundle-get-items e)))
              (e0:bundle* (cps:transform-nonescaping-nontail-expressions items))))
           ((e0:expression-primitive? e)
            (e1:let* ((primitive-name (e0:expression-primitive-get-name e))
                      (actuals (e0:expression-primitive-get-actuals e)))
              (e0:primitive* primitive-name (cps:transform-nonescaping-nontail-expressions actuals))))
           ((e0:expression-let? e)
            (e1:let* ((bound-variables (e0:expression-let-get-bound-variables e))
                      (bound-variable-no (list:length bound-variables))
                      (bound-expression (e0:expression-let-get-bound-expression e))
                      (body (e0:expression-let-get-body e)))
              (e0:let* bound-variables
                       (cps:transform-nonescaping-nontail bound-expression bound-variable-no)
                       (cps:transform-nonescaping-nontail body out-dimension))))
           ((e0:expression-call? e)
            (e1:let* ((procedure-name (e0:expression-call-get-procedure-name e))
                      (actuals (e0:expression-call-get-actuals e)))
              (e0:call* procedure-name
                        (list:cons (cps:trivial-continuation* out-dimension)
                                   (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-call-indirect? e)
            (e1:let* ((procedure-expression (e0:expression-call-indirect-get-procedure-expression e))
                      (actuals (e0:expression-call-indirect-get-actuals e)))
              (e0:call-indirect* (cps:transform-nonescaping-nontail procedure-expression 1)
                                 (list:cons (cps:trivial-continuation* out-dimension)
                                            (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-if-in? e)
            (e1:let* ((discriminand (e0:expression-if-in-get-discriminand e))
                      (values (e0:expression-if-in-get-values e))
                      (then-branch (e0:expression-if-in-get-then-branch e))
                      (else-branch (e0:expression-if-in-get-else-branch e)))
              (e0:if-in* (cps:transform-nonescaping-nontail discriminand 1) values
                         (cps:transform-nonescaping-nontail then-branch out-dimension)
                         (cps:transform-nonescaping-nontail else-branch out-dimension))))
           ((e0:expression-fork? e)
            (e1:let* ((procedure-name (e0:expression-fork-get-procedure-name e))
                      (actuals (e0:expression-fork-get-actuals e))
                      (actual-no (list:length actuals))
                      (flipper-name (cps:generate-flipper procedure-name (fixnum:+ 2 actual-no))))
              (e0:fork* flipper-name
                        (list:cons (cps:trivial-continuation* 1)
                                   (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-join? e)
            (e1:let* ((future-expression (e0:expression-join-get-future e)))
              (e0:join* (cps:transform-nonescaping-nontail future-expression 1))))
           ((e0:expression-lambda? e)
            (e1:let* ((formals (e0:expression-lambda-get-formals e))
                      (continuation-formal-name (symbol:fresh))
                      (body (e0:expression-lambda-get-body e)))
              (e1:lambda* (list:cons continuation-formal-name formals)
                (cps:transform-nonescaping-tail body (e0:variable* continuation-formal-name)))))
           ((e0:expression-call-closure? e)
            (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
                      (actuals (e0:expression-call-closure-get-actuals e)))
              (e1:call-closure* (cps:transform-nonescaping-nontail closure-expression 1)
                                (list:cons (cps:trivial-continuation* out-dimension)
                                           (cps:transform-nonescaping-nontail-expressions actuals)))))
           ((e0:expression-let/cc? e)
            (e1:let* ((continuation-name (e0:expression-let/cc-get-continuation-name e))
                      (body (e0:expression-let/cc-get-body e)))
              ;; If e is nonescaping then the continuation is not used:
              (cps:transform-nonescaping-nontail body out-dimension)))
           ;; ((e0:expression-non-cps? e)
           ;;  (e1:let* ((body (e0:expression-non-cps-get-body e)))
           ;;    (e1:error "cps:transform-nonescaping-nontail: found a CPS-shielded subexpression")))
           (else
            (e1:error "cps:transform-nonescaping-nontail: unknown extended or invalid expression"))))

;;; Simply return a list of the transformed expressions, each of one
;;; is assumed to have dimension 1:
(e1:define (cps:transform-nonescaping-nontail-expressions ee)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (cps:transform-nonescaping-nontail (list:head ee) 1)
               (cps:transform-nonescaping-nontail-expressions (list:tail ee)))))

(e1:define cps:escaping-cache
  (unboxed-hash:make))
(e1:define cps:escaping-cache-maximum-size
  (e0:value 1000))
(e1:define (cps:escaping-cache-clear!)
  (unboxed-hash:clear! cps:escaping-cache))

;;; Given a procedure name and its (untransformed) in-dimension,
;;; generate a procedure tail-calling it with the first two arguments
;;; swapped; return the new procedure name.  This is needed for
;;; fork-procedures, which expect their first parameter to be a thread
;;; identifier, while CPS makes it be a continuation.
(e1:define (cps:generate-flipper procedure-name untransformed-in-dimension)
  (e1:when (fixnum:< untransformed-in-dimension 2)
    (e1:error "cps:generate-flipper: less than two arguments"))
  (e1:let* ((flipper-name (symbol:fresh))
            (formal-1 (symbol:fresh))
            (formal-2 (symbol:fresh))
            (other-formals (symbol:fresh-symbols (fixnum:- untransformed-in-dimension 2))))
    (state:procedure-set! flipper-name
                          (list:cons formal-1 (list:cons formal-2 other-formals))
                          (e0:call* procedure-name
                                    (list:cons (e0:variable* formal-2)
                                               (list:cons (e0:variable* formal-1)
                                                          (cps:variables* other-formals)))))
    flipper-name))

;;; This procedure, and its helpers, consider "escaping" any procedure
;;; which may possibly escape.  Of course this analysis must be partial.
(e1:define (cps:escaping? e)
  (e1:let ((handle (e0:expression-get-handle e)))
    (e1:cond ((fixnum:> (unboxed-hash:element-no cps:escaping-cache)
                        cps:escaping-cache-maximum-size)
              (cps:escaping-cache-clear!)
              (cps:escaping? e)) ; retry
             ((unboxed-hash:has? cps:escaping-cache handle)
              (unboxed-hash:get cps:escaping-cache handle))
             (else
              (e1:let ((result (cps:uncached-escaping? e)))
                (unboxed-hash:set! cps:escaping-cache handle result)
                result)))))

(e1:define (cps:any-escaping? ee)
  (e1:cond ((list:null? ee)
            #f)
           ((cps:escaping? (list:head e))
            #t)
           (else
            (cps:any-escaping? (list:tail ee)))))

(e1:define (cps:uncached-escaping? e)
  (e1:cond ((e0:expression-variable? e)
            #f)
           ((e0:expression-value? e)
            #f)
           ((e0:expression-bundle? e)
            (e1:let* ((items (e0:expression-bundle-get-items e)))
              (cps:any-escaping? items)))
           ((e0:expression-primitive? e)
            (e1:let* ((primitive-name (e0:expression-primitive-get-name e))
                      (actuals (e0:expression-primitive-get-actuals e)))
              (cps:any-escaping? actuals)))
           ((e0:expression-let? e)
            (e1:let* ((bound-variables (e0:expression-let-get-bound-variables e))
                      (bound-expression (e0:expression-let-get-bound-expression e))
                      (body (e0:expression-let-get-body e)))
              (e1:or (cps:escaping? bound-expression)
                     (cps:escaping? body))))
           ((e0:expression-call? e)
            (e1:let* ((procedure-name (e0:expression-call-get-procedure-name e))
                      (actuals (e0:expression-call-get-actuals e)))
              #t)) ;; FIXME: this is WAY too conservative
           ((e0:expression-call-indirect? e)
            (e1:let* ((procedure-expression (e0:expression-call-indirect-get-procedure-expression e))
                      (actuals (e0:expression-call-indirect-get-actuals e)))
              #t)) ;; FIXME: this is too conservative
           ((e0:expression-if-in? e)
            (e1:let* ((discriminand (e0:expression-if-in-get-discriminand e))
                      (values (e0:expression-if-in-get-values e))
                      (then-branch (e0:expression-if-in-get-then-branch e))
                      (else-branch (e0:expression-if-in-get-else-branch e)))
              (e1:or (cps:escaping? discriminand)
                     (cps:escaping? then-branch)
                     (cps:escaping? else-branch))))
           ((e0:expression-fork? e)
            (e1:let* ((procedure-name (e0:expression-fork-get-procedure-name e))
                      (actuals (e0:expression-fork-get-actuals e))
                      (actual-no (list:length actuals)))
              (cps:any-escaping? actuals)))
           ((e0:expression-join? e)
            (e1:let* ((future-expression (e0:expression-join-get-future e)))
              (cps-escaping? future-expression)))
           ((e0:expression-lambda? e)
            (e1:let* ((formals (e0:expression-lambda-get-formals e))
                      (continuation-formal-name (symbol:fresh))
                      (body (e0:expression-lambda-get-body e)))
              ;; Generating a closure is harmless, unless you call it...
              #f))
           ((e0:expression-call-closure? e)
            (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
                      (actuals (e0:expression-call-closure-get-actuals e)))
              #t)) ;; FIXME: this is too conservative
           ((e0:expression-let/cc? e)
            (e1:let* ((continuation-name (e0:expression-let/cc-get-continuation-name e))
                      (body (e0:expression-let/cc-get-body e)))
              ;; FIXME: too conservative: I have to check if the
              ;; continuation is really used, which is to say whether
              ;; it occours free in the body.
              #t))
           ;; ((e0:expression-non-cps? e)
           ;;  (e1:let* ((body (e0:expression-non-cps-get-body e)))
           ;;    #t))
           (else
            (e1:error "cps:uncached-escaping?: unknown extended or invalid expression"))))

;;; CPS-transform the expression e, applied to the continuation k
;;; (also an expression), which takes "dimension" arguments.
(e1:define (cps:transform e k)
  (e1:cond ((e0:expression-variable? e)
            (e1:let* ((variable-name (e0:expression-variable-get-name e)))
              (e1:jump* k (list:list (e0:variable* variable-name)))))
           ((e0:expression-value? e)
            (e1:let* ((value-content (e0:expression-value-get-content e)))
              (e1:jump* k (list:list (e0:value* value-content)))))
           ((e0:expression-bundle? e)
            (e1:let* ((items (e0:expression-bundle-get-items e))
                      (item-no (list:length items))
                      (item-names (symbol:fresh-symbols item-no)))
              (cps:transform-expressions items
                                         (e1:continuation* item-names
                                           (e1:jump* k (cps:variables* item-names))))))
           ((e0:expression-primitive? e)
            (e1:let* ((primitive-name (e0:expression-primitive-get-name e))
                      (actuals (e0:expression-primitive-get-actuals e))
                      (actual-names (symbol:fresh-symbols (list:length actuals)))
                      (primitive-out-dimension (state:primitive-get-out-dimension primitive-name))
                      (result-names (symbol:fresh-symbols primitive-out-dimension)))
              (cps:transform-expressions actuals
                                         (e1:continuation* actual-names
                                           (e0:let* result-names (e0:primitive* primitive-name (cps:variables* actual-names))
                                             (e1:jump* k (cps:variables* result-names)))))))
           ((e0:expression-let? e)
            (e1:let* ((bound-variables (e0:expression-let-get-bound-variables e))
                      (bound-expression (e0:expression-let-get-bound-expression e))
                      (body (e0:expression-let-get-body e)))
              (cps:transform bound-expression
                             (e1:continuation* bound-variables
                               (cps:transform body k)))))
           ((e0:expression-call? e)
            (e1:let* ((procedure-name (e0:expression-call-get-procedure-name e))
                      (actuals (e0:expression-call-get-actuals e))
                      (actual-names (symbol:fresh-symbols (list:length actuals))))
              (cps:transform-expressions actuals
                                         (e1:continuation* actual-names
                                           (e0:call* procedure-name
                                                     (list:cons k (cps:variables* actual-names)))))))
           ((e0:expression-call-indirect? e)
            (e1:let* ((procedure-expression (e0:expression-call-indirect-get-procedure-expression e))
                      (actuals (e0:expression-call-indirect-get-actuals e))
                      (procedure-variable-name (symbol:fresh))
                      (actual-names (symbol:fresh-symbols (list:length actuals))))
              (cps:transform-expressions (list:cons procedure-expression actuals)
                                         (e1:continuation* (list:cons procedure-variable-name actual-names)
                                           (e0:call-indirect* (e0:variable* procedure-variable-name)
                                                              (list:cons k (cps:variables* actual-names)))))))
           ((e0:expression-if-in? e)
            (e1:let* ((discriminand (e0:expression-if-in-get-discriminand e))
                      (values (e0:expression-if-in-get-values e))
                      (then-branch (e0:expression-if-in-get-then-branch e))
                      (else-branch (e0:expression-if-in-get-else-branch e))
                      (discriminand-name (symbol:fresh))
                      (shared-continuation-name (symbol:fresh)))
              (cps:transform discriminand
                             (e1:continuation* (list:list discriminand-name)
                               ;; I introduce the block in order not to generate two references to k
                               (e0:let* (list:list shared-continuation-name)
                                        k
                                 (e0:if-in* (e0:variable* discriminand-name) values
                                   (cps:transform then-branch (e0:variable* shared-continuation-name))
                                   (cps:transform else-branch (e0:variable* shared-continuation-name))))))))
           ((e0:expression-fork? e)
            (e1:let* ((procedure-name (e0:expression-fork-get-procedure-name e))
                      (actuals (e0:expression-fork-get-actuals e))
                      (actual-names (symbol:fresh-symbols (list:length actuals)))
                      (flipper-name (cps:generate-flipper procedure-name (fixnum:+ 2 (list:length actuals)))))
              (cps:transform-expressions actuals
                                         (e1:continuation* actual-names
                                           (e1:jump* k
                                                     (list:list (e0:fork* flipper-name
                                                                          (list:cons (cps:trivial-continuation* 1)
                                                                                     (cps:variables* actual-names)))))))))
           ((e0:expression-join? e)
            (e1:let* ((future-expression (e0:expression-join-get-future e))
                      (future-result-name (symbol:fresh)))
              (cps:transform future-expression
                             (e1:continuation* (list:list future-result-name)
                               (e1:jump* k
                                         (list:list (e0:join* (e0:variable* future-result-name))))))))
           ((e0:expression-lambda? e)
            (e1:let* ((formals (e0:expression-lambda-get-formals e))
                      (body (e0:expression-lambda-get-body e))
                      (continuation-formal-name (symbol:fresh)))
              (e1:jump* k
                        (list:list (e1:lambda* (list:cons continuation-formal-name formals)
                                     (cps:transform body (e0:variable* continuation-formal-name)))))))
           ((e0:expression-call-closure? e)
            (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
                      (actuals (e0:expression-call-closure-get-actuals e))
                      (closure-expression-name (symbol:fresh))
                      (actual-names (symbol:fresh-symbols (list:length actuals))))
              (cps:transform-expressions (list:cons closure-expression actuals)
                                         (e1:continuation* (list:cons closure-expression-name actual-names)
                                           (e1:call-closure* (e0:variable* closure-expression-name)
                                                             (list:cons k (cps:variables* actual-names)))))))
           ((e0:expression-let/cc? e)
            (e1:let* ((continuation-name (e0:expression-let/cc-get-continuation-name e))
                      (body (e0:expression-let/cc-get-body e))
                      ;;(captured-continuation-name (symbol:fresh))
                      (discarded-continuation-name (symbol:fresh))
                      ;;(continuation-argument-names (symbol:fresh-symbols (cps:dimension-exactly-get-number dimension)))
                      )
              (e0:let* (list:list continuation-name)
                       k ;; k is compatible with other closures, and we don't need to know its in-dimension here
                       (cps:transform body k))))
           ;; ((e0:expression-non-cps? e)
           ;;  (e1:let* ((body (e0:expression-non-cps-get-body e)))
           ;;    body))
           (else
            (e1:error "cps:transform: unknown extended or invalid expression"))))

;;; This returns the application of a given continuation to the
;;; results of evaluating the given expressions left-to-right; all
;;; expressions are supposed to have dimension 1.
(e1:define (cps:transform-expressions es k)
  (e1:let* ((names (symbol:fresh-symbols (list:length es))))
    (cps:transform-expressions-aux es names names k)))
(e1:define (cps:transform-expressions-aux remaining-expressions remaining-names all-names k)
  (e1:if (list:null? remaining-expressions)
    (e1:jump* k (cps:variables* all-names))
    (cps:transform (list:head remaining-expressions)
                   (e1:continuation* (list:list (list:head remaining-names))
                     (cps:transform-expressions-aux (list:tail remaining-expressions)
                                                    (list:tail remaining-names)
                                                    all-names
                                                    k)))))

;;; FIXME: rename, moving into another namespace?
(e1:define (cps:variables* variable-names)
  (e1:if (list:null? variable-names)
    list:nil
    (list:cons (e0:variable* (list:head variable-names))
               (cps:variables* (list:tail variable-names)))))


;;   (test (e1:let/cc k (e1:mydotimes (a 10 a) (e0:let (a) (e1:if (e0:primitive whatever:eq? a 70) (e1:call-closure k 1000) 8) (e0:bundle)))))
;  (e1:mydotimes (a 10 a) (e0:let (a) (e1:if #f (e1:call-closure k 1000) 8) (e0:bundle)))

;; (test (e0:primitive fixnum:+ 1 (e1:let/cc k 2)))

;; *********************** 
(e1:define cps:continuation-formal-name (symbol:fresh)) ;; We can use the same formal everywhere
(e1:define (cps:cps-expression-transform expression)
  (cps:transform expression trivial-continuation1*))
;;  (cps:transform-nonescaping-tail expression trivial-continuation1*))

;;; This is suitable to retroactively transform nonescaping code:
(e1:define (cps:nonescaping-cps-procedure-transform name formals body)
  (e0:bundle name
             (list:cons cps:continuation-formal-name formals)
             (cps:transform-nonescaping-tail body (e0:variable* cps:continuation-formal-name))))

(e1:define (cps:cps-procedure-transform name formals body)
  (e0:bundle name
             (list:cons cps:continuation-formal-name formals)
             (cps:transform body (e0:variable* cps:continuation-formal-name))))
             ;; ;;; FIXME: I probably should re-enable the line above
             ;; (cps:transform-nonescaping-tail body (e0:variable* cps:continuation-formal-name))))
;; (e1:define (cps:cps-transform-nonescaping-procedure formals body)
;;   (e0:bundle (list:append (list:list cps:continuation-formal-name) formals)
;;              (cps:transform body (e0:variable* cps:continuation-formal-name))))
(e1:define (cps:cps-transform-value value)
  value)

(e1:define (cps:+ k a b)
  (e1:call-closure k
                   57 ;; the dummy continuation parameter
                   (e0:primitive fixnum:+ a b)))

;;(e1:toplevel (transform:transform-retroactively! (e0:value cps:cps-transform-value) (e0:value cps:cps-procedure-transform)))
;;(test (e0:let (a b) (e0:bundle 1 2 3) (e0:primitive fixnum:+ a b)))
;; *********************** 


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(e1:define-macro (e1:mydotimes (variable iteration-no . result-forms) . body-forms)
  (e1:let* ((limit-variable-name (sexpression:fresh-symbol)))
    `(e1:let* ((,limit-variable-name ,iteration-no))
       (e1:do ((,variable 0 (e0:primitive fixnum:1+ ,variable)))
              ((e0:primitive whatever:eq? ,variable ,limit-variable-name) ,@result-forms)
         ,@body-forms))))

(e1:define-macro (bzz q)
  (e1:dotimes (i 10)
    (fixnum:print i))
  `(e0:value ,q))

;;;;;;;;;;;;
;(e1:define trivial-continuation (e1:lambda (x) x))
;(define trivial-continuation (e1:toplevel trivial-continuation))
;; (e1:define trivial-continuation-expression
;;   (closure:closure-convert (e1:lambda* (e1:value-list x) (e0:variable* (e0:value x)))
;;                                       set-as-list:empty))
;; (define trivial-continuation-expression (e1:toplevel trivial-continuation-expression))
;; (e1:define trivial-continuation (e1:lambda (x) x))
;; (define trivial-continuation (e1:toplevel trivial-continuation))

(e1:define (t!)
;  (state:invalidate-macro-procedure-name-cache!)
  (string:write "Ok-0\n")
  (state:invalidate-macro-procedure-name-cache!)
;  (string:write "Ok-A\n")
  (transform:transform-retroactively! (e1:value-list) ; globals not to transform
                                      (e1:value-list) ; global transform names
                                      (list:list (buffer:get trivial-continuation0 0) ; procedures not to transform
                                                 (buffer:get trivial-continuation1 0)
                                                 ;;(e0:value cps:repl)
                                                 )
                                      (e1:value-list ;cps:cps-procedure-transform ;; !!!!
                                                     cps:nonescaping-cps-procedure-transform
                                                     closure:closure-convert-procedure-transform))
  ;; At this point the system is very fragile: we retroactively
  ;; modified procedures, so we can't call them directly with the
  ;; specified number of parameters; and we haven't added the new
  ;; transformations yet, so we have to manually transform until we
  ;; do.  Notice that the body of this procedure has already been
  ;; macroexpanded and transformed, *before* the retroactive procedure
  ;; transformation.
;  (string:write trivial-continuation0 "Ok-B\n")
;  (string:write trivial-continuation0 "Ok-Bzz!\n")
  (transform:prepend-expression-transform! trivial-continuation0 (e0:value cps:cps-expression-transform))
;  (string:write trivial-continuation0 "Ok-C\n")
  (transform:prepend-procedure-transform! trivial-continuation0 (e0:value cps:cps-procedure-transform))
;  (string:write trivial-continuation0 "Ok-D\n")
  ;;(state:invalidate-macro-procedure-name-cache! trivial-continuation0)
;  (string:write trivial-continuation0 "Ok-E\n")
  (repl:repl trivial-continuation0)
  ;; (string:write (e1:lambda () (e0:bundle)) "Ok-B\n") ;; this lambda becomes a trivial continuation after CPS
  ;; ;;(transform:prepend-procedure-transform! (e1:lambda () (e0:bundle)) (e0:value cps:cps-procedure-transform))
  ;; (string:write (e1:lambda () (e0:bundle)) "Ok-C\n")
  ;; ;; Call this with a continuation parameter: when the call is
  ;; ;; executed, the procedure will be already transformed, so I need to
  ;; ;; pass the additional parameter:
  ;; (state:invalidate-macro-procedure-name-cache!
  ;;    ;; The procedure associated to this lambda is generated at
  ;;    ;; macroexpansion time, so it will be transformed like all the
  ;;    ;; others: that's why we don't manually add a continuation
  ;;    ;; parameter.
  ;;    (e1:lambda () (e0:bundle)))
  ;; (string:write (e1:lambda () (e0:bundle)) "Ok-D\n")
  ;; ;; FIXME: I should maybe invalidate macros *after* the retroactive
  ;; ;; transform, likely as part of the primitive itself: evaluating
  ;; ;; code, such as the primitive call, may always involve a new
  ;; ;; macroexpansion whose result we want to undo.
  )

;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: remove this testing thing in the end
(define-macro (block-failure . forms)
  (let ((whatever-name (gensym)))
    `(catch #t
            (lambda () ,@forms)
            (lambda ,whatever-name (format #f "FAILED: ~s" ,whatever-name)))))
(define (optional-dimension->dimension d)
  (cond ((null? d)
         '(cps:dimension-exactly 1))
        ((not (null? (cdr d)))
         (error "optional-dimension->dimension: too many arguments" d))
        (else
         `(cps:dimension-exactly ,(car d)))))
(define (optional-dimension->continuation d)
  (if (null? d)
      (optional-dimension->continuation '(1))
      `(cps:trivial-continuation* (e0:value ,(car d)))))
(define (list->values x)
  (apply values (list->guile-list x)))
(define-macro (cps-unsafe-with-continuation-argument e1-expression . optional-dimension)
  `(list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension) ,(optional-dimension->dimension optional-dimension)))))))
(define-macro (cps-unsafe-without-continuation-argument e1-expression . optional-dimension)
  `(list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (e1:call-closure* (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->dimension optional-dimension)) (list:list ,(optional-dimension->continuation optional-dimension))))))))
(define-macro (cps-unsafe e1-expression . optional-dimension)
;;  `(cps-unsafe-without-continuation-argument ,e1-expression ,@optional-dimension))
;;  `(list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension)))))))
  `(begin
     (print-expression_ (e1:toplevel (e1:macroexpand ',e1-expression)))
     (print-expression_ (e1:toplevel (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension))))
     ;;(list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform-nonescaping-tail (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension))))))
     (list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension))))))
     ))
(define-macro (cps e1-expression . optional-dimension)
  `(block-failure (cps-unsafe ,e1-expression ,@optional-dimension)))
(define-macro (test-cps e1-expression . optional-dimension)
  `(begin
     (format #t "CPS:     ~s\n" (cps ,e1-expression ,@optional-dimension))
     ;(format #t "CPS:     ~s\n" (block-failure (list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension) ,(optional-dimension->dimension optional-dimension))))))))
     (if #f 42)))
(define-macro (test-cps-unsafe e1-expression . optional-dimension)
  `(begin
     (format #t "CPS:     ~s\n" (cps-unsafe ,e1-expression ,@optional-dimension))
     ;(format #t "CPS:     ~s\n" (list->values (e1:toplevel (e0:eval-ee (transform:transform-expression (cps:transform (e1:macroexpand ',e1-expression) ,(optional-dimension->continuation optional-dimension) ,(optional-dimension->dimension optional-dimension)))))))
     (if #f 42)))
(define-macro (test-direct e1-expression)
  `(begin
     (format #t "Non-CPS: ~s\n" (block-failure (e1:toplevel ,e1-expression)))
     (if #f 42)))
(define-macro (test e1-expression . optional-dimension)
  `(begin
     ;; (test-direct ,e1-expression)
     ;; (test-cps ,e1-expression ,@optional-dimension)
     (gc)
     (benchmark (test-direct ,e1-expression) #t)
     (gc)
     (benchmark (test-cps ,e1-expression ,@optional-dimension) #t)
     ))

;; (define (fibo-guile n) (if (< n 2) n (+ (fibo-guile (- n 2)) (fibo-guile (- n 1)))))
;; (e1:define (fibo n) (e0:if-in n (0 1) n (fixnum:+ (fibo (fixnum:- n (e0:value 2))) (fibo (fixnum:- n (e0:value 1))))))
;; (e1:define (fibo-a t n) (e0:if-in n (0 1) n (fixnum:+ (fibo-a t (fixnum:- n (e0:value 2))) (fibo-a t (fixnum:- n (e0:value 1))))))

;; (e1:define (fact n) (e0:if-in n (0) (e0:value 1) (fixnum:* n (fact (fixnum:1- n)))))
;; (e1:define (fact-acc n a) (e0:if-in n (0) a (fact-acc (fixnum:1- n) (fixnum:* n a))))
;; (e1:define (fact-acc-a t n a) (e0:if-in n (0) a (fact-acc-a t (fixnum:1- n) (fixnum:* n a))))


;; (define-macro (e1:toplevel-form guile-sexpression)
;;   `(let* ((epsilon0-expression-returning-results
;;            (e0:call* (e0:value repl:macroexpand-transform-and-execute)
;;                      (list:list1 (e0:value* (guile-sexpression->sexpression ',guile-sexpression)))))
;;           (results-as-list (e0:unbundle (e0:eval-ee epsilon0-expression-returning-results))))
;;      ;; Return multiple results to Guile:
;;      (apply values (list->guile-list results-as-list))))

;; (define-macro (e1:toplevel . guile-sexpressions)
;;   (cond ((null? guile-sexpressions)
;;          '(values)) ;; return zero results
;;         ((null? (cdr guile-sexpressions))
;;          `(e1:toplevel-form ,(car guile-sexpressions)))
;;         (else
;;          `(begin
;;             (e1:toplevel-form ,(car guile-sexpressions))
;;             (e1:toplevel ,@(cdr guile-sexpressions))))))


;;;;; Fire up the REPL without passing continuations from the outside:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; This is not subject to the retroactive CPS transform:
;; (e1:define (cps:repl)
;;   (repl:repl trivial-continuation0))

;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test macroexpand after the transformation is installed:
(define (testm guile-sexpression)
  (let* ((sexpression (guile-sexpression->sexpression guile-sexpression))
         (expression (e0:call* (e0:value e1:macroexpand) (list:list2 (e0:value* trivial-continuation1) (e0:value* sexpression)))))
    (print-expression_ expression)
    (let* ((results (list->guile-list (e0:eval-ee expression))))
      (format #t "macroexpand results: ~s\n" results)
      (print-expression_ (car results)))))

;;;   (e1:toplevel (t!))

#!
;; Print the definition of a closure procedure when the closure is bound in a global
(meta:print-procedure-definition (symbol->guile-symbol (buffer:get trivial-continuation1 (e0:value 0))))

;;;
;; Ok
(e0:eval-ee (e0:primitive* (e0:value fixnum:+) (list:list2 (e0:value* (e0:value 2)) (e0:value* (e0:value 3)))))

;; Wrong in-dimension, which is true: ok
(e0:eval-ee (e0:call* (e0:value fixnum:+) (list:list2 (e0:value* (e0:value 2)) (e0:value* (e0:value 3)))))

;; This now works (correct):
(e0:eval-ee (e0:call* (e0:value io:standard-input) (list:list1 (e0:value* trivial-continuation1))))

;; Same as above:
(e0:eval-ee (e0:call* (e0:value fixnum:1+) (list:list2 (e0:value* trivial-continuation1) (e0:value* (e0:value 234)))))

;; Same as above:
(e0:eval-ee (e0:call* (e0:value fixnum:+) (list:list3 (e0:value* trivial-continuation1) (e0:value* (e0:value 2)) (e0:value* (e0:value 3)))))
(e0:eval-ee (e0:call* (e0:value fixnum:**) (list:list3 (e0:value* trivial-continuation1) (e0:value* (e0:value 2)) (e0:value* (e0:value 24)))))

(e0:eval-ee (e0:call-indirect* (e0:value* (e0:value fixnum:**)) (list:list3 (e0:value* trivial-continuation1) (e0:value* (e0:value 2)) (e0:value* (e0:value 24)))))

;; Same as above:
(gc) (benchmark (e0:eval-ee (e0:call* (e0:value fibo) (list:list2 (e0:value* trivial-continuation1) (e0:value* (e0:value 25))))))
;; Untransformed:
(gc) (benchmark (e0:eval-ee (e0:call* (e0:value fibo) (list:list1 (e0:value* (e0:value 25))))))

;; REPL:
(e0:eval-ee (e0:call* (e0:value repl:repl) (list:list1 (e0:value* trivial-continuation1))))

;; Invalidate macro cache:
(e0:eval-ee (e0:call* (e0:value state:invalidate-macro-procedure-name-cache!) (list:list1 (e0:value* trivial-continuation0))))

;; Try this with alternative values for s:
(define s (guile-sexpression->sexpression '45))
(e0:eval-ee (e0:call* (e0:value e1:macroexpand) (list:list2 (e0:value* trivial-continuation1) (e0:value* s))))

(e0:eval-ee (e0:call* (e0:value sexpression:type-tag->expression-expander-procedure-name) (list:list2 (e0:value* trivial-continuation1) (e0:value* (e0:value 0)))))

(e1:define (f-tr n)
  (e1:let/cc k
    (f-tr-aux n k)))
(e1:define (f-tr-aux n k)
  (f-tr-aux (e1:if (fixnum:zero? n)
              (e1:call-closure k 0)
              (fixnum:1- n))
            k))

;; f and g do the same thing, but f uses a continuation to directly
;; escape with the result, instead of perfoming a sequence of returns
;; and trivial computations first; f might hence be faster, if I
;; generate good code and I don't rely on the fact that f-aux is
;; non-escaping.  The sum with zero is just to make the recursive call
;; occur in a non-tail context.  In the tail-recursive version, there
;; should be no performance difference (under the conditions above).
(e1:define b (box:make 0))
(e1:define (f n)
  (box:set! b (e1:let/cc k
                (f-aux n 0 k)))
  57)
(e1:define (f-aux n c k)
  (e1:if (fixnum:zero? n)
    (e1:call-closure k c)
    (fixnum:+ 0 (f-aux (fixnum:1- n)
                       (fixnum:1+ c)
                       k))))

(e1:define (g n)
  (box:set! b (g-aux n 0 42))
  57)
(e1:define (g-aux n c k)
  (e1:if (fixnum:zero? n)
    c
    (fixnum:+ 0 (g-aux (fixnum:1- n)
                       (fixnum:1+ c)
                       k))))
(e1:define (f-tr n)
  (box:set! b (e1:let/cc k
                (f-tr-aux n 0 k)))
  57)
(e1:define (f-tr-aux n c k)
  (e1:if (fixnum:zero? n)
    (e1:call-closure k c)
    (f-tr-aux (fixnum:1- n)
              (fixnum:1+ c)
              k)))
(e1:define (g-tr n)
  (box:set! b (g-tr-aux n 0 42))
  57)
(e1:define (g-tr-aux n c k)
  (e1:if (fixnum:zero? n)
    c
    (g-tr-aux (fixnum:1- n)
              (fixnum:1+ c)
              k)))


;;; This works, but (with my pretty inefficient CPS transform) is slightly
;;; slower than the version based on self-applicating closures.
(e1:define-macro (cps:dotimes (variable time-no . result-forms) . body-forms)
  (e1:let* ((time-no-name (sexpression:fresh-symbol))
            (loop-start-box-name (sexpression:fresh-symbol)))
    `(e0:let (,time-no-name) ,time-no
       (e0:let (,loop-start-box-name) (box:make-uninitialized)
         (e0:let (,variable)
                 (e1:let/cc k
                   (box:set! ,loop-start-box-name k)
                   0)
           (e1:if (fixnum:= ,variable ,time-no-name)
             (e1:begin
               ,@result-forms)
             (e1:begin
               ,@body-forms
               (e1:call-closure (box:get ,loop-start-box-name)
                                (fixnum:1+ ,variable)))))))))
(e1:define (test-cps-p n) (cps:dotimes (i n i) (fixnum:print i)))
(e1:define (test-cc-p n) (e1:dotimes (i n i) (fixnum:print i)))
(e1:define (test-cps n) (cps:dotimes (i n i)))
(e1:define (test-cc n) (e1:dotimes (i n i)))
!#

(define (cps:repl)
  (e0:eval-ee (e0:call* (e0:value repl:repl) (list:list1 (e0:value* trivial-continuation1)))))
  ;;(e1:toplevel (cps:repl)))
(define (cps:invalidata-macro-cache!)
  (e0:eval-ee (e0:call* (e0:value state:invalidate-macro-procedure-name-cache!) (list:list1 (e0:value* trivial-continuation0)))))

(define-macro (cps:toplevel-form guile-sexpression)
  `(let* ((epsilon0-expression-returning-results
           (e0:call* (e0:value repl:macroexpand-transform-and-execute)
                     (list:list2 (e0:value* trivial-continuation1)
                                 (e0:value* (guile-sexpression->sexpression ',guile-sexpression)))))
          (results-as-list (e0:unbundle (e0:eval-ee epsilon0-expression-returning-results))))
     ;; Return multiple results to Guile:
     (apply values (list->guile-list results-as-list))))
(define-macro (cps:toplevel . guile-sexpressions)
  (cond ((null? guile-sexpressions)
         '(values)) ;; return zero results
        ((null? (cdr guile-sexpressions))
         `(cps:toplevel-form ,(car guile-sexpressions)))
        (else
         `(begin
            (cps:toplevel-form ,(car guile-sexpressions))
            (cps:toplevel ,@(cdr guile-sexpressions))))))

(e1:define (slow n)
  (slow-acc n 0))
(e1:define (slow-acc n a)
  (e1:if (fixnum:zero? n)
    a
    (slow-acc (fixnum:1- n) (fixnum:1+ a))))

;;; =============================================
;;; FAILING testcase (procedure transformed with nonescaping-tail-cps, expression transformed in any way):
(e1:define (qqqq a) (e1:call-closure a))
;;(cps:toplevel (qqqq (e1:lambda () 3)))
;;; =============================================
