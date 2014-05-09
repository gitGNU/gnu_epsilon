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

(e1:define (** b e)
  (e1:if e
    (fixnum:* b (** b (fixnum:1- e)))
    1))

(e1:define (fact n)
  (e1:if (fixnum:zero? n)
    1
    (fixnum:* n
              (fact (fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))
(e1:define (test n)
  (e1:dotimes (i n)
    (fio:write "fibo(" (i i) ") = " (i (fibo i)) "\n")))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (p s)
  `(e1:let* ((bs (list:list ;; (cons:make (e1:value x) 10)
                            ;; (cons:make (e1:value y) 20)
                            ;; (cons:make (e1:value z) 30)
                            ;; (cons:make (e1:value a) 1)
                            ;; (cons:make (e1:value b) 2)
                            ;; (cons:make (e1:value c) 3)
                            ;;(cons:make (e1:value m) (io:file-content-as-byte-vector "/home/luca/brainfuck/esoteric.sange.fi/brainfuck/bf-source/prog/HELLOUM.BF"))
                            ))
             ;;(_  (fio:write "OK-D 100\n") 42)
             (e1 (repl:macroexpand-and-transform ',s))
             ;;(_  (fio:write "OK-D 200\n") 42)
             (e2
              (peval:value->expression (peval:peval-expression e1 bs)))
             ;;(_  (fio:write "OK-D 300\n") 42)
             (e2-reduced
              (e0:expression-without-unneeded-lets e2)))
     (fio:write (e e1)
                "\n  -peval->\n"
                (e e2)
                "\n  -without-unneeded-lets->\n"
                (e e2-reduced)
                "\n")))

;; These shouldn't fail:
;; (p (e1:let* ((n 0)) (e1:let* ((n m)) (e0:if-in n (0) 1 (e1:primitive fixnum:* n (fact (e1:primitive fixnum:1- n)))))))
;; (p (e1:let* ((n m)) (e0:if-in n (0) 1 (e1:primitive fixnum:* n (fact (e1:primitive fixnum:1- n))))))
;; (p (e1:let* ((n m)) 1))

(e1:define-macro (_ s)
  `(repl:macroexpand-and-transform ',s))

(e1:define-macro (a s)
  `(e1:let* ((e1 (repl:macroexpand-and-transform ',s))
             (e2 (e0:alpha-convert-expression e1)))
     (fio:write (e e1) "\n  ==>\n" (e e2) "\n")))

(e1:define-macro (l s)
  `(e1:let* ((e1 (repl:macroexpand-and-transform ',s))
             (e2 (e0:expression-without-trivial-lets e1)))
     (fio:write (e e1) "\n  ==>\n" (e e2) "\n")))

(e1:define (pa name)
  (e1:let ((p (io:standard-output)))
    (e0:let (new-f new-b)
            (e0:alpha-convert-procedure name)
      (e1:begin
        (io:write-string p "Formals: ")
        (printer:write-symbols p new-f)
        (io:write-string p "\nBody: ")
        (printer:write-expression p new-b)
        (io:write-string p "\n")))))

;;; Destructivrly replace procedure definitions with their optimized versions.
(e1:define (d!)
  (e1:let ((optimization-no (box:make 0)))
    (e1:dolist (name (state:procedure-names))
      (e1:let* ((b1 (state:procedure-get-body name))
                (b1-size (e0:expression-size b1))
                (b2 (e0:expression-without-trivial-lets b1))
                (b2-size (e0:expression-size b2)))
        (e1:when (fixnum:< b2-size b1-size)
          (box:bump! optimization-no))
        (e1:when (fixnum:>= (fixnum:- b1-size b2-size) 1) ;;10)
          (fio:write "Optimizing " (sy name)
                     ": " (i b1-size)
                     " -> " (i b2-size) "\n"))
        (state:procedure-set! name
                              (state:procedure-get-formals name)
                              b2)))
    (fio:write "Optimized " (i (box:get optimization-no))
               " over " (i (list:length (state:procedure-names)))
               " procedures\n")))


;; (l (e0:let (x y z) (e1:bundle 1 2 (e1:primitive fixnum:1+ 3)) y))
(e1:define (f n) (e1:dotimes (i n) (fio:write "i is " (i i) "\n")))
(e1:define (print-symbol-list list)
  (e1:dolist (s list)
     (e1:if (fixnum:zero? s)
       (fio:write "* INDIRECT\n")
       (fio:write "* " (sy s) "\n"))))

(e1:define-macro (c name)
  `(print-symbol-list (e0:procedure-callees (e1:value ,name))))

(e1:define-macro (cc name)
  `(print-symbol-list (e0:procedure-possibly-indirect-callees (e1:value ,name))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum peval:value
  ;; Fully dynamic expression:
  (dynamic expression)
  ;; Fully static value:
  (static epsilon-value)
  ;; A bundle made of peval values (dimension different from 1):
  (bundle peval-values)
  ;; A buffer whose address is dynamic, but whose content may be partially static:
  (partial expression peval-values))

;;; Bindings are from symbols to peval:value's.  In order to properly
;;; shadow globals, all procedure formals and bindings should always
;;; be bound; possibly to a trivial dynamic expression containing the
;;; variable itself.

;;; Bindings are an unboxed-key hash.

(e1:define (peval:empty-bb)
  (unboxed-hash:make))

(e1:define (peval:lookup bs x)
  (e1:cond ((unboxed-hash:has? bs x)
            (unboxed-hash:get bs x))
           ((state:global? x)
            (peval:value-static (state:global-get x)))
           (else
            (peval:value-dynamic (e0:variable* x)))))

(e1:define (peval:value->expression v)
  (e1:match v
    ((peval:value-dynamic e)
     e)
    ((peval:value-static v)
     (e0:value* v))
    ((peval:value-bundle items)
     (e0:bundle* (peval:values->expressions items)))
    ((peval:value-partial e pvs)
     e)))
(e1:define (peval:values->expressions vs)
  (e1:if (list:null? vs)
    list:nil
    (list:cons (peval:value->expression (list:head vs))
               (peval:values->expressions (list:tail vs)))))

(e1:define (peval:value->epsilon-value v)
  (e1:match v
    ((peval:value-dynamic e)
     (e1:error "dynamic"))
    ((peval:value-static v)
     v)
    ((peval:value-bundle items)
     (e1:if (fixnum:zero? (list:length items))
       (e1:error "empty bundle")
       (peval:value->epsilon-value (list:head items))))
    ((peval:value-partial e pvs)
     (e1:error "partial"))))
(e1:define (peval:values->epsilon-values vs)
  (e1:if (list:null? vs)
    list:nil
    (list:cons (peval:value->epsilon-value (list:head vs))
               (peval:values->epsilon-values (list:tail vs)))))

(e1:define (peval:epsilon-values->value vs)
  (e1:if (fixnum:= (list:length vs) 1)
    (peval:value-static (list:head vs))
    (peval:value-bundle (list:map (e1:lambda (v) (peval:value-static v)) vs))))

(e1:define (peval:bind-variable! bs x v)
  (e1:match v
    ((peval:value-dynamic e)
     ;; ;; Bound to a expression containing the variable itself.  This is
     ;; ;; only to shadow globals.
     ;; (peval:bind! bs x (peval:dynamic (e0:variable* x))))
     (unboxed-hash:set! bs x v))
    ((or (peval:value-static _)
         (peval:value-partial _ _))
     (unboxed-hash:set! bs x v))
    ((peval:value-bundle items)
     (e1:if (list:null? items)
       (e1:error "empty bundle")
       (peval:bind-variable! bs x (list:head items))))))
(e1:define (peval:bind-variables! bs variables v)
  (e1:match v
    ((peval:value-dynamic e)
     ;; Bound each variable to a expression containing the variable itself.
     ;; This is only to shadow globals.
     (e1:dolist (variable variables)
       (peval:bind-variable! bs variable (peval:value-dynamic (e0:variable* variable)))))
    ((or (peval:value-static _)
         (peval:value-partial _ _))
     (e1:case (list:length variables)
       ((0))
       ((1)
        (peval:bind-variable! bs (list:head variables) v))
       (else
        (e1:error "too many bound variables"))))
    ((peval:value-bundle items)
     (e1:unless (list:null? variables)
       (peval:bind-variable! bs
                             (list:head variables)
                             (list:head items))
       (peval:bind-variables! bs
                              (list:tail variables)
                              (peval:value-bundle (list:tail items)))))))

(e1:define (peval:values-statics? vs)
  (e1:if (list:null? vs)
    #t
    (e1:match (list:head vs)
      ((peval:value-static _)
       (peval:values-statics? (list:tail vs)))
      ((peval:value-bundle (list:nil))
       ;; Checking for staticity is fine, but binding this will lead
       ;; to a dimension error.
       (peval:values-statics? (list:tail vs)))
      ((peval:value-bundle (list:cons item _))
       (peval:values-statics? (list:cons item (list:tail vs))))
      (_
       #f))))

(e1:define (peval:peval-expression alist e)
  (e1:let ((h (unboxed-hash:make))
           ;;(e (e0:alpha-convert-expression e))
           )
    (e1:dolist (x (e0:free-variables e))
      (unboxed-hash:set! h x (peval:value-dynamic (e0:variable* x))))
    (e1:dolist (b alist)
      (unboxed-hash:set! h (cons:get-car b) (cons:get-cdr b)))
    (e1:let* ((res (peval:peval-expression-helper h e))
              (eres (peval:value->expression res)))
      (fio:write (e e)
                 "\n  -peval->\n"
                 (e eres)
                 "\n  -without-unneeded-lets->\n"
                 (e (e0:expression-without-unneeded-lets eres))
                 "\n")
      res)))
(e1:define (peval:peval-expression-helper bs e)
  (e1:match e
    ((e0:expression-variable _ x)
     (peval:lookup bs x))
    ((e0:expression-value _ v)
     (peval:value-static v))
    ((e0:expression-bundle _ items)
     (e1:if (fixnum:= (list:length items) 1)
       (peval:peval-expression-helper bs (list:head items) bs)
       (peval:value-bundle (peval:peval-expressions-helper items bs))))
    ((e0:expression-primitive _ name actuals)
     (e1:let ((vactuals (peval:peval-expressions-helper bs actuals)))
       ;; FIXME: exploit partially-static values.
       (e1:if (e1:and (peval:values-statics? vactuals)
                      (e1:not (state:primitive-side-effecting? name)))
         (e1:let ((evactuals (peval:values->epsilon-values vactuals)))
           (peval:epsilon-values->value (e0:call-primitive name evactuals)))
         (e1:let ((eactuals (peval:values->expressions vactuals)))
           (peval:value-dynamic (e0:primitive* name eactuals))))))
    ((e0:expression-let _ bound-variables bound-expression body)
     (peval:peval-block bs bound-variables bound-expression body))
    ((e0:expression-call _ procedure-name actuals)
     (e1:unless (state:procedure? procedure-name)
       (fio:write "About the call to " (sy procedure-name) ":\n")
       (e1:error "unbound procedure"))
     (e1:if (e0:procedure-recursive? procedure-name)
       (e1:let ((actualsv (peval:peval-expressions-helper bs actuals)))
         (peval:value-dynamic (e0:call* procedure-name
                                        (peval:values->expressions actualsv))))
       (e1:let ((inlined-call (e0:inlined-call procedure-name actuals)))
         (peval:peval-expression-helper bs inlined-call))))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e1:let ((pv (peval:peval-expression-helper bs procedure-expression))
              (avs (peval:peval-expressions-helper bs actuals)))
       (e1:if (e0:expression-side-effecting? procedure-expression)
         (peval:residualize-call-indirect pv avs)
         (e1:match pv
           ((peval:value-static v)
            (fio:write "GOOD: we were able to turn an indirect call to "
                       (e procedure-expression)
                       " into a direct call to "
                       (sy v)
                       "\n")
            (peval:peval-expression-helper bs (e0:call* v (peval:values->expressions avs))))
           (else
            (peval:residualize-call-indirect pv avs))))))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e1:let ((dv (peval:peval-expression-helper bs discriminand)))
       (e1:match dv
         ((peval:value-static v)
          (e1:if (e0:expression-side-effecting? discriminand)
            (peval:value-dynamic (e0:let* list:nil
                                          (peval:value->expression dv)
                                          (peval:value->expression
                                              (peval:eliminate-if-in bs v values then-branch else-branch))))
            (peval:eliminate-if-in bs v values then-branch else-branch)))
         (else
          (peval:value-dynamic
              (e0:if-in* (peval:value->expression dv)
                         values
                         (peval:value->expression (peval:peval-expression-helper bs then-branch))
                         (peval:value->expression (peval:peval-expression-helper bs else-branch))))))))
    ((e0:expression-fork _ procedure-name actuals)
     (e1:let ((vactuals (peval:peval-expressions-helper bs actuals)))
       (peval:value-dynamic (e0:fork* procedure-name (peval:values->expressions vactuals)))))
    ((e0:expression-join _ future)
     (e1:let ((vfuture (peval:peval-expression-helper bs future)))
       (peval:value-dynamic (e0:join* (peval:value->expression vfuture)))))))
(e1:define (peval:peval-expressions-helper bs es)
  (e1:if (list:null? es)
    list:nil
    (list:cons (peval:peval-expression-helper bs (list:head es))
               (peval:peval-expressions-helper bs (list:tail es)))))

(e1:define (peval:peval-block bs bound-variables bound-expression body)
  (e1:let ((bev (peval:peval-expression-helper bs bound-expression)))
    (peval:bind-variables! bs bound-variables bev)
    (e1:if (e0:expression-side-effecting? bound-expression)
      (peval:residualize-block bs bound-variables bev body)
      (e1:match bev
        ((or (peval:value-static _)
             (peval:value-bundle _))
         (peval:peval-expression-helper bs body))
        (else
         (peval:residualize-block bs bound-variables bev body))))))

(e1:define (peval:residualize-block bs bound-variables bound-expression-as-value body)
  (peval:value-dynamic
      (e0:let* bound-variables
               (peval:value->expression bound-expression-as-value)
               (peval:value->expression (peval:peval-expression-helper bs body)))))

(e1:define (peval:residualize-call-indirect callee-value actual-values)
  (peval:value-dynamic (e0:call-indirect* (peval:value->expression callee-value)
                                          (peval:values->expressions actual-values))))

(e1:define (peval:eliminate-if-in bs discriminand-epsilon-value values then-branch else-branch)
  (e1:if (list:has? values discriminand-epsilon-value)
    (peval:peval-expression-helper bs then-branch)
    (peval:peval-expression-helper bs else-branch)))

;; FIXME: this is unsatisfactory.  A side-effecting expression should still be able to peval into a static
;; value.
;;(peval:peval-expression alist:nil (_ (e1:if-in (e1:begin (box:set! b 42) (fixnum:+ 10 12)) (10) 20 30)))
