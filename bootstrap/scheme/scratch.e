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


;;;;; More alist procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (alist:unbind-one-list alist keys)
  (e1:if (list:null? keys)
    alist
    (alist:unbind-one-list (alist:unbind-one alist (list:head keys))
                           (list:tail keys))))


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

(e1:define (peval:peval e bs)
  (fio:write "peval'ing " (e e) "\n")
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
         (e1:let ((actuals (peval:values->expressions vs)))
           (peval:value-known (e0:eval-primitive name actuals alist:nil)))
         (peval:value-unknown (e0:primitive* name (peval:values->expressions vs))))))
    ((e0:expression-let h bound-variables bound-expression body)
     (e1:let ((bv (peval:peval bound-expression bs))
              (new-bs (alist:unbind-one-list bs bound-variables)))
       (e0:let* bound-variables
                (peval:value->expression bv)
                ;; FIXME: really use bv in the body
                (peval body new-bs))))
    ((e0:expression-call h procedure-name actuals)
     (fio:write "OK-B 100\n")
     ;; FIXME: do it for real.
     (peval:value-unknown (e0:call* procedure-name (peval:values->expressions (peval:peval-expressions actuals bs)))))
    ((e0:expression-call-indirect h procedure-expression actuals)
     (e1:let* ((pv (peval:peval procedure-expression bs))
               (avs (peval:peval-expressions actuals bs))
               (aes (peval:values->expressions avs)))
       (e1:match pv
         ((peval-value-unknown e)
          (peval-value-unknown (e0:call-indirect* e aes)))
         ((peval-value-known vs)
          (peval:peval (e0:call* (list:head vs) aes))))))
    ((e0:expression-if-in h discriminand values then-branch else-branch)
     (e1:match (peval:peval discriminand bs)
       ((peval:value-unknown e)
        (peval:value-unknown (e0:if-in* e values
                                        (peval:value->expression (peval:peval then-branch bs))
                                        (peval:value->expression (peval:peval else-branch bs)))))
       ((peval:value-known vv)
        (e1:if (list:has? values (list:head vv))
          (peval:peval then-branch bs)
          (peval:peval else-branch bs)))))
    ((e0:expression-fork h procedure-name actuals)
     (peval:value-unknown (e0:fork* procedure-name (peval:values->expressions (peval:peval-expressions actuals bs)))))
    ((e0:expression-join h future)
     (peval:value-unknown (e0:join* (peval:value->expression (peval:peval future bs)))))))


(e1:define (peval:peval-expressions ee bs)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (peval:peval (list:head ee) bs)
               (peval:peval-expressions (list:tail ee) bs))))


(e1:define-macro (p s)
  `(e1:let* ((bs (list:list (cons:make (e1:value x) 10)
                            (cons:make (e1:value y) 20)
                            (cons:make (e1:value z) 30)
                            (cons:make (e1:value a) 1)
                            (cons:make (e1:value b) 2)
                            (cons:make (e1:value c) 3)))
             (_  (fio:write "OK-D 100\n") 42)
             (e1 (repl:macroexpand-and-transform ',s))
             (_  (fio:write "OK-D 200\n") 42)
             (e2 (peval:value->expression (peval:peval e1 bs)))
             (_  (fio:write "OK-D 300\n") 42))
     (fio:write (e e1) "\n  ==>\n" (e e2) "\n")))
