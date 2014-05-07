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


;;;;; Partial evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum peval:value
  (unknown expression)
  (known values))

(e1:define (peval:values-knowns? vs)
  (list:for-all? (e1:lambda (v) (peval:value-known? v)) vs))

(e1:define (peval:value-known->whatever v)
  (e1:match v
    ((peval:value-known vs)
     (list:head vs))
    (else
     (e1:error "unknown"))))
(e1:define (peval:values-knowns->whatevers vs)
  (list:map (e1:lambda (v) (peval:value-known->whatever v)) vs))

(e1:define (peval:value->expression v)
  (e1:match v
    ((peval:value-unknown e)
     e)
    ((peval:value-known vs)
     (e1:if (fixnum:= (list:length vs) 1)
       (e0:value* (list:head vs))
       (e0:bundle* (list:map (e1:lambda (v) (e0:value* v))
                             vs))))))

(e1:define (peval:values->expressions vs)
  (list:map (e1:lambda (v) (peval:value->expression v))
            vs))

;;; Similar to alist:bind-lists, but here we ignore excess values.
(e1:define (peval:bind-epsilon-values bs variables vs)
  (e1:cond ((list:null? variables)
            bs)
           ((list:null? vs)
            (e1:error "too many variables"))
           (else
            (peval:bind-epsilon-values (alist:bind bs
                                                   (list:head variables)
                                                   (list:head vs))
                                       (list:tail variables)
                                       (list:tail vs)))))

;;; The implemenation is clunky, but this is more delicate than it looks:
;;; bindings introduced by this should all be activated "in parallel", without
;;; each successive binding being affected by the previous ones.
(e1:define (peval:bind-bundle bs variables is)
  (e1:let* ((unflattened-new-bindings
             (list:map (e1:lambda (pair)
                         (peval:bind-item bs
                                          (cons:get-car pair)
                                          (cons:get-cdr pair)))
                       (list:zip variables
                                 (list:take is (list:length variables)))))
            (new-bindings (list:flatten unflattened-new-bindings)))
  (alist:append bs new-bindings)))
(e1:define (peval:bind-item bs variable i)
  (e1:match i
    ((e0:expression-variable _ x)
     (e1:if (alist:has? bs x)
       (list:list (cons:make variable (alist:lookup bs x)))
       list:nil))
    ((e0:expression-value _ c)
     (list:list (cons:make variable c)))
    ((e0:expression-bundle _ items)
     (peval:bind-item bs variable (list:head items)))
    (else
     list:nil)))

(e1:define (peval:bind-expression bs variables e)
  (e1:match e
    ((e0:expression-value _ v)
     (peval:bind-epsilon-values bs variables (list:list v)))
    ((e0:expression-variable _ x)
     (e1:if (alist:has? bs x)
       (peval:bind-epsilon-values bs variables (list:list (alist:lookup bs x)))
       bs))
    ((e0:expression-bundle _ is)
     (peval:bind-bundle bs variables is))
    (else
     bs)))
(e1:define (peval:bind bs variables value)
  (e1:match value
    ((peval:value-known vs)
     (peval:bind-epsilon-values bs variables vs))
    ((peval:value-unknown e)
     (fio:write "OK-A: the bound expression is " (e e) "\n")
     (peval:bind-expression bs variables e))))

(e1:define (peval:peval-expression e bs)
  ;;(fio:write "peval'ing " (e e) "\n")
  (e1:match e
    ((e0:expression-variable h x)
     (e1:if (alist:has? bs x)
       (peval:value-known (list:list (alist:lookup bs x)))
       (peval:value-unknown e)))
    ((e0:expression-value h v)
     (peval:value-known (list:list v)))
    ((e0:expression-bundle h items)
     (e1:let ((vs (peval:peval-expressions items bs)))
       (e1:if (peval:values-knowns? vs)
         (peval:value-known (peval:values-knowns->whatevers vs))
         (peval:value-unknown (e0:bundle* (peval:values->expressions vs))))))
    ((e0:expression-primitive h name actuals)
     (e1:let ((vs (peval:peval-expressions actuals bs)))
       (e1:if (e1:and (e1:not (state:primitive-side-effecting? name))
                      (e1:not (state:primitive-reflective? name))
                      (peval:values-knowns? vs))
         (e1:let ((ws (peval:values-knowns->whatevers vs)))
           (fio:write "DANGEROUS BRANCH: CALLING PRIMITIVE " (sy name) " WITH ACTUALS:\n")
           (e1:dolist (a actuals)
             (fio:write "* " (e a) "\n"))
           ;; (fio:write ", ACTUALLY COMPILE-TIME-EVALUATED TO \n")
           ;; (e1:dolist (w (peval:values-knowns->whatevers (peval:values->expressions vs)))
           ;;   (fio:write "* " (i w) "\n"))
           (fio:write "\n")
           (peval:value-known (e0:call-primitive name ws)))
         (peval:value-unknown (e0:primitive* name (peval:values->expressions vs))))))
    ((e0:expression-let h bound-variables bound-expression body)
     (e1:let* ((bv (peval:peval-expression bound-expression bs))
               (new-bs (alist:unbind-all-list bs bound-variables))
               (bodyv (peval:peval-expression body
                                              (peval:bind new-bs
                                                          bound-variables
                                                          bv))))
       (e1:if (peval:value-known? bv)
         bodyv
         (peval:value-unknown (e0:let* bound-variables
                                       (peval:value->expression bv)
                                       (peval:value->expression bodyv))))))
    ((e0:expression-call h procedure-name actuals)
     ;; FIXME: also inline recursive procedures, unless within a branch of
     ;; a dynamic conditional.
     (e1:if (e0:procedure-recursive? procedure-name)
       (peval:value-unknown (e0:call* procedure-name (peval:values->expressions (peval:peval-expressions actuals bs))))
       (e1:let ((inlined-call (e0:inlined-call procedure-name actuals)))
         (peval:peval-expression inlined-call bs))))
    ((e0:expression-call-indirect h procedure-expression actuals)
     (fio:write "OK-A 0\n")
     (e1:let* ((pv (peval:peval-expression procedure-expression bs))
               (avs (peval:peval-expressions actuals bs))
               (aes (peval:values->expressions avs)))
       (fio:write "OK-A 100: pv tag is " (i (cons:get-car pv)) "\n")
       (e1:match pv
         ((peval:value-unknown e)
          (fio:write "OK-A 110\n")
          (fio:write "Unfortunately peval didn't resolve the indirect call:\n"
                     " * " (e procedure-expression)
                     "\n => \n"
                     " * " (e e)
                     "\n")
          (peval:value-unknown (e0:call-indirect* e aes)))
         ((peval:value-known vs)
          (fio:write "GOOD! Peval turned an indirect call into a call to " (sy (list:head vs)) "\n")
          (peval:peval-expression (e0:call* (list:head vs) aes) bs)))))
    ((e0:expression-if-in h discriminand values then-branch else-branch)
     (e1:match (peval:peval-expression discriminand bs)
       ((peval:value-unknown e)
        (peval:value-unknown (e0:if-in* e values
                                        (peval:value->expression (peval:peval-expression then-branch bs))
                                        (peval:value->expression (peval:peval-expression else-branch bs)))))
       ((peval:value-known vv)
        (e1:if (list:has? values (list:head vv))
          (peval:peval-expression then-branch bs)
          (peval:peval-expression else-branch bs)))))
    ((e0:expression-fork h procedure-name actuals)
     (peval:value-unknown (e0:fork* procedure-name (peval:values->expressions (peval:peval-expressions actuals bs)))))
    ((e0:expression-join h future)
     (peval:value-unknown (e0:join* (peval:value->expression (peval:peval-expression future bs)))))))
(e1:define (peval:peval-expressions ee bs)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (peval:peval-expression (list:head ee) bs)
               (peval:peval-expressions (list:tail ee) bs))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (p s)
  `(e1:let* ((bs (list:list (cons:make (e1:value x) 10)
                            (cons:make (e1:value y) 20)
                            (cons:make (e1:value z) 30)
                            (cons:make (e1:value a) 1)
                            (cons:make (e1:value b) 2)
                            (cons:make (e1:value c) 3)))
             ;;(_  (fio:write "OK-D 100\n") 42)
             (e1 (repl:macroexpand-and-transform ',s))
             ;;(_  (fio:write "OK-D 200\n") 42)
             (e2 (peval:value->expression (peval:peval-expression e1 bs)))
             ;;(_  (fio:write "OK-D 300\n") 42)
             )
     (fio:write (e e1) "\n  ==>\n" (e e2) "\n")))

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


;;;;; Inlined epsilon0 procedure calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e0:inlined-call procedure-name actuals)
  (e0:let (formals body) (e0:alpha-convert-procedure procedure-name)
    (e0:inlined-call-make-let formals actuals body)))

;;; This assumes formals to be fresh variables, which is true when
;;; called from e0:inline-call.
(e1:define (e0:inlined-call-make-let formals actuals body)
  (e1:cond ((list:null? formals)
            (e1:if (list:null? actuals)
              body
              (e1:error "more actuals than formals")))
           ((list:null? actuals)
            (e1:error "more formals than actuals"))
           (else
            (e0:let* (list:list (list:head formals))
                     (list:head actuals)
                     (e0:inlined-call-make-let (list:tail formals)
                                               (list:tail actuals)
                                               body)))))


;;;;; Brainfuck interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (bf:wrap n size)
  (e1:cond ((fixnum:< n 0)
            (bf:wrap (fixnum:+ n size) size))
           ((fixnum:>= n size)
            (bf:wrap (fixnum:- n size) size))
           (else
            n)))
(e1:define bf:cell-modulo 256)
(e1:define bf:tape-size 100000) ;;30000)

(e1:define (bf:eval program)
  (bf:eval-with program
                 0
                 (string:length program)
                 (vector:make-initialized bf:tape-size 0)
                 0)
  ;; I don't want to be distracted by the return value of bf:eval-with.
  (e1:bundle))

;;; The result is the last value of t-index.
(e1:define (bf:eval-with p p-index p-limit t t-index)
  ;;(fio:write "@" (i p-index) " (limit " (i p-limit) ")\n")
  ;; (e1:when (fixnum:> p-index p-limit)
  ;;   (fio:write "p-index " (i p-index) "; p-limit " (i p-limit) "\n")
  ;;   (e1:error "impossible"))
  (e1:if (fixnum:= p-index p-limit)
    (e1:begin
      ;;(fio:write "Returning\n")
      t-index)
    (e1:let ((c (string:get p p-index)))
      (e1:case c
        ((#\< #\>)
         (e1:let ((delta (e1:case c
                           ((#\<) -1)
                           (else +1))))
           (bf:eval-with p (fixnum:1+ p-index) p-limit t (fixnum:+ t-index delta))))
        ((#\- #\+)
         (e1:let ((delta (e1:case c
                           ((#\-) -1)
                           (else +1))))
           (vector:set! t t-index (bf:wrap (fixnum:+ (vector:get t t-index) delta)
                                           bf:cell-modulo)))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((#\.)
         ;;(fio:write "Printing " (C (bf:wrap (vector:get t t-index) 256)) "\n")
         (fio:write (c (bf:wrap (vector:get t t-index) 256)))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((44) ;; #\,
         (e1:let ((c (character:read)))
           (e1:if (io:eof-object? c)
             (vector:set! t t-index 0))
             (vector:set! t t-index c))
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))
        ((#\[)
         (e1:let* ((next-p-index (fixnum:1+ p-index))
                   (loop-end-index (bf:loop-end p next-p-index))
                   (new-t-index
           ;;(fio:write "+ Looping in [" (i next-p-index) ", " (i loop-end-index) ")...\n")
                    (bf:eval-loop p next-p-index loop-end-index t t-index)))
           ;;(fio:write "- Continuing from " (i (fixnum:1+ loop-end-index)) "(" (c (string:get p (fixnum:1+ loop-end-index))) ").\n")
           (bf:eval-with p (fixnum:1+ loop-end-index) p-limit t new-t-index)))
        ((#\])
         ;;(fio:write "Failing at " (i next-p-index) "(" (c (string:get p next-p-index)) ").\n")
         (e1:error "this shouldn't happen"))
        (else
         (bf:eval-with p (fixnum:1+ p-index) p-limit t t-index))))))

(e1:define (bf:loop-end p i)
  ;;(fio:write "Matching the [ at " (i (fixnum:1- i)) "...\n")
  (bf:find-matching-closed-bracked p i 0))
(e1:define (bf:find-matching-closed-bracked p i open-bracket-no)
  (e1:if (fixnum:>= i (string:length p))
    (e1:error "no matching ] for [")
    (e1:case (string:get p i)
      ((#\[)
       (bf:find-matching-closed-bracked p (fixnum:1+ i) (fixnum:1+ open-bracket-no)))
      ((#\])
       (e1:if (fixnum:zero? open-bracket-no)
         (e1:begin
           ;;(fio:write "The ] at " (i i) ".\n")
           i)
         (bf:find-matching-closed-bracked p (fixnum:1+ i) (fixnum:1- open-bracket-no))))
      (else
       (bf:find-matching-closed-bracked p (fixnum:1+ i) open-bracket-no)))))
(e1:define (bf:eval-loop p p-index p-limit t t-index)
  (e1:if (vector:get t t-index)
    (e1:let ((new-t-index (bf:eval-with p p-index p-limit t t-index)))
      ;;(fio:write "AGAIN (" (i (string:get t new-t-index)) ")\n")
      (bf:eval-loop p p-index p-limit t new-t-index))
    (e1:begin
      ;;(fio:write "ENOUGH: out of the loop at "(i (fixnum:1+ p-limit)) "\n")
      t-index)))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

