;;;;; This is -*- epsilon -*- (with some Scheme).
;;;;; Bootstrap driver

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2013 Luca Saiu
;;;;; Written by Luca Saiu

;;;;; Copyright (C) 2013 Jérémie Koenig

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


;;;;; Macros for epsilon0 syntactic forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These first crude versions do not perform error-checking, silently
;;; ignoring additional subforms at the end.

;;; FIXME: define a safe version later; even better, make this safe.
;;; It shouldn't be so hard.
(e1:trivial-define-macro e0:variable
  (sexpression:inject-expression
    (e0:variable* (sexpression:eject-symbol (sexpression:car arguments)))))
(e1:trivial-define-macro e0:value
  (sexpression:inject-expression
    (e0:value* (sexpression:eject (sexpression:car arguments)))))
(e1:trivial-define-macro e0:bundle
  (sexpression:inject-expression
    (e0:bundle* (e1:macroexpand-sexpressions arguments))))
(e1:trivial-define-macro e0:primitive
  (sexpression:inject-expression
    (e0:primitive* (sexpression:eject-symbol (sexpression:car arguments))
                   (e1:macroexpand-sexpressions (sexpression:cdr arguments)))))
(e1:trivial-define-macro e0:let
  (sexpression:inject-expression
    (e0:let* (sexpression:eject-symbols (sexpression:car arguments))
             (e1:macroexpand (sexpression:cadr arguments))
             (e1:macroexpand (sexpression:caddr arguments)))))
(e1:trivial-define-macro e0:call
  (sexpression:inject-expression
    (e0:call* (sexpression:eject-symbol (sexpression:car arguments))
              (e1:macroexpand-sexpressions (sexpression:cdr arguments)))))
(e1:trivial-define-macro e0:call-indirect
  (sexpression:inject-expression
    (e0:call-indirect* (e1:macroexpand (sexpression:car arguments))
                       (e1:macroexpand-sexpressions (sexpression:cdr arguments)))))
(e1:trivial-define-macro e0:if-in
  (sexpression:inject-expression
    (e0:if-in* (e1:macroexpand (sexpression:car arguments))
               (sexpression:eject-whatevers (sexpression:cadr arguments))
               (e1:macroexpand (sexpression:caddr arguments))
               (e1:macroexpand (sexpression:cadddr arguments)))))
(e1:trivial-define-macro e0:fork
  (sexpression:inject-expression
    (e0:fork* (sexpression:eject-symbol (sexpression:car arguments))
              (e1:macroexpand-sexpressions (sexpression:cdr arguments)))))
(e1:trivial-define-macro e0:join
  (sexpression:inject-expression
    (e0:join* (e1:macroexpand (sexpression:car arguments)))))

;; ;;; FIXME: move these examples away
;; ;;; Simple demo macro: ignore parameters and return the s-integer 0
;; (e1:trivial-define-macro zero
;;   ;; '0
;;   (sexpression:inject-fixnum (e0:value 0)))
;; ;;; Simple demo macro: return the first parameter and ignore the others:
;; (e1:trivial-define-macro stupid
;;   (sexpression:car arguments))
;; ;;; Simple demo macro: return an expression multiplying by itself the
;; ;;; first parameter, which is evaluated twice:
;; (e1:trivial-define-macro stupid-square
;;   ;; `(fixnum:* ,(sexpression:car arguments) ,(sexpression:car arguments))
;;   (sexpression:list3 (sexpression:inject-symbol (e0:value fixnum:*))
;;                      (sexpression:car arguments)
;;                      (sexpression:car arguments)))


;;;;; Macro utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I define once and for all some sexpression things I will reuse.

;;; The "uninteresting" result:
(e1:define e1:uninteresting-expression
  (e0:bundle* list:nil))
(e1:define e1:injected-uninteresting-expression
  (sexpression:inject-expression e1:uninteresting-expression))

;;; The sexpression (#f):
(e1:define e1:false-list-sexpression
  (sexpression:cons (sexpression:inject-boolean (e0:value #f))
                    sexpression:nil))


;;;;; List variadic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We define a variadic list macro very early, because it is convenient to use
;;; instead of cons before having quasiquoting

(e1:define (sexpression:list* xs)
  (e0:if-in (sexpression:null? xs) (#f)
    ;;`(sexpression:cons ,(sexpression:car xs) ,(sexpression:list* (sexpression:cdr xs)))
    (sexpression:cons (sexpression:inject-symbol (e0:value sexpression:cons))
                      (sexpression:cons (sexpression:car xs)
                                        (sexpression:cons (sexpression:list* (sexpression:cdr xs))
                                                          sexpression:nil)))
    (sexpression:inject-symbol (e0:value sexpression:nil))))
(e1:trivial-define-macro sexpression:list
  (sexpression:list* arguments))

(e1:define (sexpression:list-tr* xs)
  (sexpression:list-tr*-acc (sexpression:reverse xs)
                            (sexpression:inject-symbol (e0:value sexpression:nil))))
(e1:define (sexpression:list-tr*-acc xs acc)
  (e0:if-in (sexpression:null? xs) (#f)
    (sexpression:list-tr*-acc (sexpression:cdr xs)
                              (sexpression:cons (sexpression:inject-symbol (e0:value sexpression:cons))
                                                (sexpression:cons (sexpression:car xs)
                                                                  (sexpression:cons acc sexpression:nil))))
    acc))
(e1:trivial-define-macro sexpression:list-tr
  (sexpression:list-tr* arguments))

(e1:define (list:list* xs)
  (e0:if-in (sexpression:null? xs) (#f)
    (sexpression:cons (sexpression:inject-symbol (e0:value list:cons))
                      (sexpression:cons (sexpression:car xs)
                                        (sexpression:cons (list:list* (sexpression:cdr xs))
                                                          sexpression:nil)))
    (sexpression:inject-symbol (e0:value list:nil))))
(e1:trivial-define-macro list:list
  (list:list* arguments))
(e1:trivial-define-macro list:make ;; alias, with a less traditional but better name
  (list:list* arguments))

;;; A variadic s-expression append will be convenient for implementing quasiquoting:
(e1:define (sexpression:append* xs)
  (e0:if-in (sexpression:null? xs) (#f)
    (e0:if-in (sexpression:null? (sexpression:cdr xs)) (#f)
      ;; xs has two or more elements:
      ;; `(sexpression:append2 ,(sexpression:car xs) ,(sexpression:append* (sexpression:cdr xs)))
      (sexpression:cons (sexpression:inject-symbol (e0:value sexpression:append2))
                        (sexpression:cons (sexpression:car xs)
                                          (sexpression:cons (sexpression:append* (sexpression:cdr xs))
                                                            sexpression:nil)))
      ;; xs has one element
      (sexpression:car xs))
    ;; xs is nil
    (sexpression:inject-symbol (e0:value sexpression:nil))))
(e1:trivial-define-macro sexpression:append
  (sexpression:append* arguments))


;;;;; Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This definition is not a very good example of a macro: all the
;;; important work is done by a helper procedure, at the level of
;;; expressions.  This is less readable than the more usual style, but
;;; is fast; and the helper procedure will be useful later as well.

;;; The s-expression is an s-list of forms, each of which must be
;;; macroexpanded, yielding a nested block expression.
(e1:define (e1:macroexpand-sequence-into-expression sexpression)
  (e0:if-in (sexpression:null? sexpression) (#f)
    ;; there is at least one form
    (e0:if-in (sexpression:null? (sexpression:cdr sexpression)) (#f)
      ;; there are at least two forms
      (e0:let* list:nil
               (e1:macroexpand (sexpression:car sexpression))
               (e1:macroexpand-sequence-into-expression (sexpression:cdr sexpression)))
      ;; there is exactly one form
      (e1:macroexpand (sexpression:car sexpression)))
    ;; there are zero forms
    e1:uninteresting-expression))

(e1:define (e1:macroexpand-sequence-into-sexpression sexpression)
  (sexpression:inject-expression (e1:macroexpand-sequence-into-expression sexpression)))

(e1:trivial-define-macro e1:begin
  (e1:macroexpand-sequence-into-sexpression arguments))


;;;;; One- or two-way conditional: if
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ELisp-compatible if, accepting zero or more forms in the else branch

(e1:define (e1:if* condition then-branch-form else-branch-forms)
  ;;`(e0:if-in ,condition (#f) (e1:begin ,@else-branch-forms) ,then-branch-form)
  (sexpression:list (sexpression:inject-symbol (e0:value e0:if-in))
                    condition
                    e1:false-list-sexpression
                    (e1:macroexpand-sequence-into-sexpression else-branch-forms)
                    then-branch-form))

(e1:trivial-define-macro e1:if
  (e1:if* (sexpression:car arguments)
          (sexpression:cadr arguments)
          (sexpression:cddr arguments)))


;;;;; One-way conditionals: when, unless
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e1:when* condition body-forms)
  ;;`(e0:if-in condition (#f) 57 ,(e1:macroexpand-sequence-into-sexpression body-forms)
  (sexpression:list (sexpression:inject-symbol (e0:value e0:if-in))
                    condition
                    e1:false-list-sexpression
                    e1:injected-uninteresting-expression
                    (e1:macroexpand-sequence-into-sexpression body-forms)))
(e1:define (e1:unless* condition body-forms)
  (sexpression:list (sexpression:inject-symbol (e0:value e0:if-in))
                    condition
                    e1:false-list-sexpression
                    (e1:macroexpand-sequence-into-sexpression body-forms)
                    e1:injected-uninteresting-expression))

(e1:trivial-define-macro e1:when
  (e1:when* (sexpression:car arguments)
            (sexpression:cdr arguments)))
(e1:trivial-define-macro e1:unless
  (e1:unless* (sexpression:car arguments)
              (sexpression:cdr arguments)))


;;;;; Short-circuit boolean connectives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e1:and* conditions)
  (e1:if (sexpression:null? conditions)
    sexpression:true
    (e1:if (sexpression:null? (sexpression:cdr conditions))
      (sexpression:car conditions)
      ;;`(e0:if-in ,(car conditions) (#f) #f ,(e1:and* (cdr conditions)))
      (sexpression:list (sexpression:inject-symbol (e0:value e0:if-in))
                        (sexpression:car conditions)
                        (sexpression:list (sexpression:inject-boolean (e0:value #f)))
                        (sexpression:inject-boolean (e0:value #f))
                        (e1:and* (sexpression:cdr conditions))))))
(e1:trivial-define-macro e1:and
  (e1:and* arguments))

(e1:define (e1:or* conditions)
  (e1:if (sexpression:null? conditions)
    sexpression:false
    (e1:if (sexpression:null? (sexpression:cdr conditions))
      (sexpression:car conditions)
      ;;`(e0:if-in ,(car conditions) (#f) ,(e1:or* (cdr conditions)) #t)
      (sexpression:list (sexpression:inject-symbol (e0:value e0:if-in))
                        (sexpression:car conditions)
                        (sexpression:list (sexpression:inject-boolean (e0:value #f)))
                        (e1:or* (sexpression:cdr conditions))
                        (sexpression:inject-boolean (e0:value #t))))))
(e1:trivial-define-macro e1:or
  (e1:or* arguments))

;;; This isn't variadic, nor a syntactic form; but I like to have t just
;;; for symmetry:
(e1:define (e1:not condition)
  (e1:if condition
         (e0:value #f)
         (e0:value #t)))

;;; The xor function is associative, which will make a variadic
;;; version particularly intuitive.  Anyway, no short-circuiting is
;;; possible.
;;; FIXME: add variadic support with general-purpose variadic
;;; definitions, below.
(e1:define (e1:xor condition1 condition2)
  (e1:if condition1
         (e1:not condition2)
         condition2))


;;;;; Many-way conditional: cond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sexpression:else-symbol? sexpression)
  (e1:and (sexpression:symbol? sexpression)
          (whatever:eq? (sexpression:eject-symbol sexpression)
                        (e0:value else))))

(e1:define (e1:cond* cases)
  (e1:if (sexpression:null? cases)
    e1:injected-uninteresting-expression
    (e0:let (condition) (sexpression:caar cases)
      (e0:let (case-forms) (sexpression:cdar cases)
        (e1:if (sexpression:else-symbol? condition)
          ;; The condition is the else symbol
          (e1:if (sexpression:null? (sexpression:cdr cases))
            (e1:macroexpand-sequence-into-sexpression case-forms)
            (e1:error (e0:value "cond: else condition is not terminal")))
          ;; The condition is not the else symbol:
          (e1:if* condition
                  (e1:macroexpand-sequence-into-sexpression case-forms)
                  (sexpression:list1 (e1:cond* (sexpression:cdr cases)))))))))

(e1:trivial-define-macro e1:cond
  (e1:cond* arguments))


;;;;; Quoting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Quoting takes an s-expression s and returns an expression which,
;;; when evaluated, builds a structurally-equal *copy* of s.  This
;;; deep-cloning at run time would not be strictly necessary for
;;; quoting, but we share some logic with quasiquoting.

(e1:trivial-define-macro sexpression:quote
  (e1:cond ((sexpression:null? arguments)
            (e1:error (e0:value "sexpression:quote: zero arguments")))
           ((sexpression:null? (sexpression:cdr arguments))
            (sexpression:quote-into-sexpression (sexpression:car arguments)))
           (else
            (e1:error (e0:value "sexpression:quote: more tha one argument")))))

(e1:define (sexpression:quote-into-sexpression x)
  (sexpression:inject-expression (sexpression:quoter x)))

(e1:define (sexpression:quoter any-sexpression)
  (e0:let (tag) (sexpression:get-tag any-sexpression)
          (e0:call-indirect (sexpression:type-tag->quoter-procedure-name tag)
                            any-sexpression)))

(e1:define (sexpression:leaf-quoter sexpression)
  (e0:call* (e0:value sexpression:make)
            (list:list2 (e0:value* (sexpression:get-tag sexpression))
                        (e0:value* (sexpression:eject sexpression)))))

(e1:define (sexpression:cons-quoter cons-sexpression)
  (e0:call* (e0:value sexpression:cons)
            (list:list2 (sexpression:quoter (sexpression:car cons-sexpression))
                        (sexpression:quoter (sexpression:cdr cons-sexpression)))))


;;;;; Quasiquoting (R6RS-style)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:trivial-define-macro sexpression:quasiquote
  (e1:if (e1:and (sexpression:cons? arguments)
                 (e1:not (sexpression:null? (sexpression:cdr arguments))))
         (e1:error (e0:value "quasiquote has more than one argument"))
         (sexpression:quasiquoter (sexpression:car arguments) (e0:value 0))))

;;; Dispatch to the specific case, looking at the quasiquoter
;;; procedure in the type table:
(e1:define (sexpression:quasiquoter any-sexpression depth)
  (e0:let (tag) (sexpression:get-tag any-sexpression)
          (e0:call-indirect (sexpression:type-tag->quasiquoter-procedure-name tag)
                            any-sexpression
                            depth)))

;;; This follows Bawden's updated proposal (different from the older
;;; one in [Alan Bawden, "Quasiquotation in Lisp", 1999], Appendix B),
;;; as quoted by Kent Dybvig at
;;; http::/www.r6rs.org:r6rs-editors:2006-June:001376.html.  The new
;;; version was eventually adopted in R6RS.

;;; Compute once and for all some symbolic constants we're gonna use:
(e1:define sexpression:quote-sexpression (sexpression:quote sexpression:quote))
(e1:define sexpression:car-sexpression (sexpression:quote sexpression:car))
(e1:define sexpression:cdr-sexpression (sexpression:quote sexpression:cdr))
(e1:define sexpression:cons-sexpression (sexpression:quote sexpression:cons))
(e1:define sexpression:list1-sexpression (sexpression:quote sexpression:list1))
(e1:define sexpression:list-sexpression (sexpression:quote sexpression:list))
(e1:define sexpression:flatten-sexpression (sexpression:quote sexpression:flatten))
(e1:define sexpression:append2-sexpression (sexpression:quote sexpression:append2))
(e1:define sexpression:append-sexpression (sexpression:quote sexpression:append))
(e1:define sexpression:arguments-sexpression (sexpression:quote arguments))
(e1:define (sexpression:quasiquote-symbol? sexpression)
  (e1:and (sexpression:symbol? sexpression)
          (e1:or (whatever:eq? (sexpression:eject sexpression) (e0:value sexpression:quasiquote))
                 (whatever:eq? (sexpression:eject sexpression) (e0:value quasiquote)))))
(e1:define (sexpression:unquote-symbol? sexpression)
  (e1:and (sexpression:symbol? sexpression)
          (e1:or (whatever:eq? (sexpression:eject sexpression) (e0:value unquote))
                 (whatever:eq? (sexpression:eject sexpression) (e0:value sexpression:unquote)))))
(e1:define (sexpression:unquote-splicing-symbol? sexpression)
  (e1:and (sexpression:symbol? sexpression)
          (e1:or (whatever:eq? (sexpression:eject sexpression) (e0:value unquote-splicing))
                 (whatever:eq? (sexpression:eject sexpression) (e0:value sexpression:unquote-splicing)))))

(e1:define (sexpression:leaf-quasiquoter x depth)
  ;;`',x
  ;;(sexpression:list2 sexpression:quote-sexpression x))
  (sexpression:quote-into-sexpression x))

(e1:define (sexpression:cons-quasiquoter cons depth)
  (sexpression:quasiquote-cons (sexpression:car cons) (sexpression:cdr cons) depth))
(e1:define (sexpression:quasiquote-cons car cdr depth)
  (e1:cond ((sexpression:quasiquote-symbol? car)
            ;; `(sexpression:cons ',car ,(sexpression:quasiquoter cdr (fixnum:1+ depth)))
            (sexpression:list3 sexpression:cons-sexpression
                               (sexpression:quote-into-sexpression car) ;; (sexpression:list2 sexpression:quote-sexpression car)
                               (sexpression:quasiquoter cdr (fixnum:1+ depth))))
           ((e1:or (sexpression:unquote-symbol? car)
                   (sexpression:unquote-splicing-symbol? car))
            (e1:cond ((fixnum:> depth (e0:value 0))
                      ;; `(sexpression:cons ',car ,(sexpression:quasiquoter cdr (fixnum:1- depth)))
                      (sexpression:list3 sexpression:cons-sexpression
                                         (sexpression:quote-into-sexpression car) ;; (sexpression:list2 sexpression:quote-sexpression car)
                                         (sexpression:quasiquoter cdr (fixnum:1- depth))))
                     ((e1:and (sexpression:unquote-symbol? car)
                              (e1:not (sexpression:null? cdr))
                              (sexpression:null? (sexpression:cdr cdr)))
                      (sexpression:car cdr))
                     ((sexpression:null? cdr)
                      (e1:error (e0:value "unquoting form with zero parameters")))
                     (else
                      (e1:error (e0:value "splicing unquote form in a non-splicing context")))))
           (else
            ;;`(sexpression:append2 ,(sexpression:list-quasiquoter car depth) ,(sexpression:quasiquoter cdr depth)))
            (sexpression:list3 sexpression:append2-sexpression
                               (sexpression:list-quasiquoter car depth)
                               (sexpression:quasiquoter cdr depth)))))

(e1:define (sexpression:list-quasiquoter x depth)
  (e1:if (sexpression:cons? x)
    (sexpression:cons-list-quasiquoter (sexpression:car x) (sexpression:cdr x) depth)
    (sexpression:leaf-list-quasiquoter x depth)))

(e1:define (sexpression:leaf-list-quasiquoter x depth)
  ;;`'(,x)
  ;;(sexpression:list2 sexpression:quote-sexpression (sexpression:list1 x)))
  (sexpression:quote-into-sexpression (sexpression:list1 x)))

(e1:define (sexpression:cons-list-quasiquoter car cdr depth)
  (e1:cond ((sexpression:quasiquote-symbol? car)
            ;; `(list (sexpression:cons ',car ,(sexpression:quasiquoter cdr (fixnum:1+ depth))))
            (sexpression:list2 sexpression:list1-sexpression
                               (sexpression:list3 sexpression:cons-sexpression
                                                  (sexpression:quote-into-sexpression car);;(sexpression:list2 sexpression:quote-sexpression car)
                                                  (sexpression:quasiquoter cdr (fixnum:1+ depth)))))
           ((e1:or (sexpression:unquote-symbol? car)
                   (sexpression:unquote-splicing-symbol? car))
            (e1:cond ((fixnum:> depth (e0:value 0))
                      ;;`(sexpression:list1 (sexpression:cons ',car ,(sexpression:quasiquoter cdr (fixnum:1- depth))))
                      (sexpression:list2 sexpression:list1-sexpression
                                         (sexpression:list3 sexpression:cons-sexpression
                                                            (sexpression:quote-into-sexpression car);;(sexpression:list2 sexpression:quote-sexpression car)
                                                            (sexpression:quasiquoter cdr (fixnum:1- depth)))))
                     ((sexpression:unquote-symbol? car)
                      ;;`(sexpression:list . ,cdr)
                      (sexpression:cons sexpression:list-sexpression cdr))
                     (else
                      ;;`(sexpression:append . ,cdr)
                      (sexpression:cons sexpression:append-sexpression cdr))))
           (else
            ;;`(sexpression:list1 (sexpression:append2 ,(sexpression:list-quasiquoter car depth) ,(sexpression:list-quasiquoter cdr depth)))
            (sexpression:list2 sexpression:list1-sexpression
                               (sexpression:list3 sexpression:append2-sexpression
                                                  (sexpression:list-quasiquoter car depth)
                                                  (sexpression:quasiquoter cdr depth))))))

;;; For Scheme compatibility only, using reader abbreviations.  FIXME:
;;; remove this as soon as we have namespaces or our custom reader.
(e1:trivial-define-macro quote
  ;; `(sexpression:quote . ,arguments)
  (sexpression:cons (sexpression:quote sexpression:quote)
                    arguments))
(e1:trivial-define-macro quasiquote
  ;; `(sexpression:quasiquote . ,arguments)
  (sexpression:cons (sexpression:quote sexpression:quasiquote)
                    arguments))


;;;;; destructuring-bind à-la Common Lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will be useful soon below, to define a friendlier macroexpand.

(e1:define (e1:destructuring-bind* pattern structure body-forms)
  (e0:let (variable) (symbol:fresh)
    (sexpression:inject-expression
      (e0:let* (list:list1 variable) (e1:macroexpand structure)
        (e1:macroexpand (e1:destructuring-bind-variable*s pattern
                                                          (sexpression:inject-symbol variable)
                                                          (e1:macroexpand-sequence-into-sexpression body-forms)))))))

;;; All three parameters are s-expressions
(e1:define (e1:destructuring-bind-variable*s pattern variable body-form)
  (e1:cond ((sexpression:null? pattern)
            (sexpression:quasiquote (e1:if (sexpression:null? ,variable)
                                      ,body-form
                                      (e1:error (e0:value "e1:destructuring-bind: no match for ()")))))
           ((sexpression:symbol? pattern)
            (sexpression:quasiquote (e0:let (,pattern) ,variable
                                      ,body-form)))
           ((sexpression:cons? pattern)
            (e0:let (car-name) (sexpression:fresh-symbol)
              (e0:let (cdr-name) (sexpression:fresh-symbol)
                (sexpression:quasiquote (e1:if (sexpression:cons? ,variable)
                                          (e0:let (,car-name) (sexpression:car ,variable)
                                            ,(e1:destructuring-bind-variable*s
                                                   (sexpression:car pattern)
                                                   car-name
                                                   (sexpression:quasiquote (e0:let (,cdr-name) (sexpression:cdr ,variable)
                                                                             ,(e1:destructuring-bind-variable*s (sexpression:cdr pattern)
                                                                                                                cdr-name
                                                                                                                body-form)))))
                                          (e1:error (e0:value "e1:destructuring-bind: no match for cons")))))))
           (else
             (e1:error (e0:value "e1:destructuring-bind: ill-formed pattern")))))

(e1:define (e1:destructuring-bind-of-arguments* arguments)
  (e1:destructuring-bind* (sexpression:car arguments)
                          (sexpression:cadr arguments)
                          (sexpression:cddr arguments)))

(e1:trivial-define-macro e1:destructuring-bind
  (e1:destructuring-bind-of-arguments* arguments))


;;;;; A better define-macro, supporting named parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Our destructuring-bind lets us build a friendlier kind of macro
;;; definition using named parameters.  Notice that this new kind of
;;; macro definitions is itself an epsilon1 macro, expanding to an
;;; ordinary epsilon0 expression which can be executed from anywhere
;;; in a program.

;; (e1:trivial-define-macro e1:define-macro
;;   (e1:destructuring-bind ((macro-name . formals) . body-forms) arguments
;;     (sexpression:quasiquote (state:macro-set! (e0:value ,macro-name)
;;                                               (sexpression:quote (e1:destructuring-bind ,formals arguments ,@body-forms))))))

(e1:trivial-define-macro e1:define-macro
  (e1:destructuring-bind ((macro-name . formals) . body-forms) arguments
(e0:let () (string:write "Defining the macro ")
(e0:let () (string:write (symbol:symbol->string (sexpression:eject-symbol (sexpression:caar arguments))))
(e0:let () (string:write "...\n")
    (sexpression:quasiquote
      (e0:let () (state:macro-set! (e0:value ,macro-name)
                                   (sexpression:quote (e1:destructuring-bind ,formals arguments ,@body-forms)))
        ;;(e0:value ,macro-name)
        (e0:bundle) ;; don't return anything
        ))))
)))


;;;;; Definitions as epsilon1 macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Up to this point we used Scheme macros for defining globals and
;;; procedures.  But now our macros are sophisticated enough for us to
;;; easily re-implement global definitions in epsilon1 itself.  This
;;; also enables self-modifying programs, which can add or change
;;; globals and procedures from anywhere.

;;; A procedure version may be convenient to use as well.  Among the
;;; rest, this version lets up finally use a sequence of forms within
;;; a definition.  We are still defining this with an e1:define
;;; written in Guile.
(e1:define (e1:define-non-procedure-procedure name-as-symbol form-sequence-as-sexpression)
  (e0:let () (string:write "Defining the non-procedure ")
    (e0:let () (string:write (symbol:symbol->string name-as-symbol))
      (e0:let () (string:write "...\n")
        (e0:let (sexpression) (e1:macroexpand-sequence-into-sexpression form-sequence-as-sexpression)
          (e0:let (results) (repl:macroexpand-transform-and-execute sexpression)
            (e0:let (result) (list:head results)
              (state:global-set! name-as-symbol result))))))))
;;              (e0:let () (state:global-set! name-as-symbol result)
;;                name-as-symbol))))))))

;;; A subtle point: macroexpansion and transformation occur at
;;; execution time, not at the time of the macroexpansion of
;;; e1:define, e1:define-procedure and e1:define-non-procedure.

;;; Macro version of the above.
(e1:toplevel (e1:define-macro (e1:define-non-procedure name . forms)
               `(e1:define-non-procedure-procedure (e0:value ,name)
                                                   ',forms)))

(e1:define (e1:define-procedure-procedure name-symbol formal-symbols body-forms-sexpression)
  (e0:let () (string:write "Defining the procedure ")
    (e0:let () (string:write (symbol:symbol->string name-symbol))
      (e0:let () (string:write "...\n")
        (e0:let (untransformed-body)
                (e1:macroexpand-sequence-into-expression body-forms-sexpression)
          (e0:let (transformed-name transformed-formals transformed-body)
                  (transform:transform-procedure name-symbol formal-symbols untransformed-body)
            (state:procedure-set! transformed-name
                                  transformed-formals
                                  transformed-body)))))))
            ;; (e0:let () (state:procedure-set! transformed-name
            ;;                                  transformed-formals
            ;;                                  transformed-body)
            ;;   name-symbol)))))))

;;; Macro version of the above:
(e1:toplevel (e1:define-macro (e1:define-procedure (name . formals) . body-forms)
              `(e1:define-procedure-procedure (e0:value ,name)
                                              (sexpression:eject-symbols ',formals)
                                              ',body-forms)))

;; (e1:define-macro (e1:define-procedure (name . formals) . body-forms)
;;   `(e0:let () (string:write "Defining the procedure ")
;;      (e0:let () (string:write (symbol:symbol->string (e0:value ,name)))
;;        (e0:let () (string:write "...\n")
;;          (e0:let (formals) (sexpression:eject-symbols ',formals)
;;            (e0:let (untransformed-body) (e1:macroexpand '(e1:begin ,@body-forms))
;;              (e0:let (transformed-formals transformed-body)
;;                      (transform:transform-procedure formals untransformed-body)
;;                (state:procedure-set! (e0:value ,name)
;;                                      transformed-formals
;;                                      transformed-body))))))))

;;; A comfortable Scheme-style e1:define which works for both
;;; procedures and non-procedures, according to the shape of the first
;;; parameter:
(e1:toplevel (e1:define-macro (e1:define name-possibly-with-formals . body-forms)
               (e1:if (sexpression:cons? name-possibly-with-formals)
                 `(e1:define-procedure ,name-possibly-with-formals ,@body-forms)
                 `(e1:define-non-procedure ,name-possibly-with-formals ,@body-forms))))

;;; Make e1:define and e1:define-macro as defined above also available
;;; from the Guile toplevel, so that we can avoid e1:toplevel most of
;;; the time from now on:
(load "export-toplevel-forms-to-guile.scm")


;;;;; Multi-way discriminand conditional: case
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Of course case only compares "by identity", so it is not reliable
;;; on unboxed data.

(e1:define-macro (e1:case discriminand . cases)
  (e0:let (discriminand-name) (sexpression:fresh-symbol)
    `(e0:let (,discriminand-name) ,discriminand
       (case:dispatch ,discriminand-name ,@cases))))

(e1:define-macro (case:dispatch discriminand-name . cases)
  (e1:cond ((sexpression:null? cases)
            e1:injected-uninteresting-expression)
           ((sexpression:else-symbol? (sexpression:caar cases))
            (e1:if (sexpression:null? (sexpression:cdr cases))
                   `(e1:begin ,@(sexpression:cdar cases))
                   (e1:error "e1:case: the else case is not the last one")))
           (else
            `(e0:if-in ,discriminand-name ,(sexpression:caar cases)
               (e1:begin ,@(sexpression:cdar cases))
               (case:dispatch ,discriminand-name ,@(sexpression:cdr cases))))))


;;;;; Simple block: let*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;; Simple and readable -- but slow -- implementation:
;; (e1:define-macro (e1:let* bindings . body-forms)
;;   (e1:if (sexpression:null? bindings)
;;     (sexpression:quasiquote (e1:begin ,@body-forms))
;;     (sexpression:quasiquote (e0:let (,(sexpression:caar bindings)) (e1:begin ,@(sexpression:cdar bindings))
;;                               (e1:let* ,(sexpression:cdr bindings) ,@body-forms)))))

;;; Optimized implementation:
(e1:define (e1:macroexpand-expand-let*-into-expression bindings-as-sexpression body-as-expression)
  (e1:if (sexpression:null? bindings-as-sexpression)
    body-as-expression
    (e0:let* (list:list1 (sexpression:eject-symbol (sexpression:caar bindings-as-sexpression)))
             (e1:macroexpand-sequence-into-expression (sexpression:cdar bindings-as-sexpression))
             (e1:macroexpand-expand-let*-into-expression (sexpression:cdr bindings-as-sexpression)
                                                         body-as-expression))))
(e1:define-macro (e1:let* bindings . body-forms)
  (sexpression:inject-expression (e1:macroexpand-expand-let*-into-expression
                                    bindings
                                    (e1:macroexpand-sequence-into-expression body-forms))))


;;;;; Generalization of binary procedures into variadic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (variadic:call-left-deep* neutral-element-sexpression
                                     binary-procedure-name-sexpression
                                     arguments-sexpression)
  (e1:cond ((sexpression:null? arguments-sexpression)
            neutral-element-sexpression)
           ((sexpression:null? (sexpression:cdr arguments-sexpression))
            (sexpression:car arguments-sexpression))
           (else ;; arguments-sexpression has at least two elements
            (variadic:call-left-deep* neutral-element-sexpression
                                      binary-procedure-name-sexpression
                                      ;; We generate explicit e0:call sexpressions, which enables us to overload
                                      ;; the procedure name and use it as a macro as well.
                                      (sexpression:cons (sexpression:list4 (sexpression:quote e0:call)
                                                                           binary-procedure-name-sexpression
                                                                           (sexpression:car arguments-sexpression)
                                                                           (sexpression:cadr arguments-sexpression))
                                                        (sexpression:cddr arguments-sexpression))))))
(e1:define (variadic:call-right-deep* neutral-element-sexpression
                                      binary-procedure-name-sexpression
                                      arguments-sexpression)
  (e1:cond ((sexpression:null? arguments-sexpression)
            neutral-element-sexpression)
           ((sexpression:null? (sexpression:cdr arguments-sexpression))
            (sexpression:car arguments-sexpression))
           (else ;; arguments-sexpression has at least two elements
            ;; Just as above, we generate explicit e0:call sexpressions
            (sexpression:list4 (sexpression:quote e0:call)
                               binary-procedure-name-sexpression
                               (sexpression:car arguments-sexpression)
                               (variadic:call-right-deep* neutral-element-sexpression
                                                          binary-procedure-name-sexpression
                                                          (sexpression:cdr arguments-sexpression))))))

;;; If the procedure is associative then we're free to choose the more
;;; efficient version:
(e1:define (variadic:call-associative* neutral-element-sexpression
                                       binary-procedure-name-sexpression
                                       arguments-sexpression)
  (variadic:call-left-deep* neutral-element-sexpression
                            binary-procedure-name-sexpression
                            arguments-sexpression))

(e1:define-macro (variadic:call-left-deep neutral-element binary-procedure-name . arguments)
  (variadic:call-left-deep* neutral-element binary-procedure-name arguments))

(e1:define-macro (variadic:call-right-deep neutral-element binary-procedure-name . arguments)
  (variadic:call-right-deep* neutral-element binary-procedure-name arguments))

(e1:define-macro (variadic:call-associative neutral-element binary-procedure-name . arguments)
  (variadic:call-associative* neutral-element binary-procedure-name arguments))

;;; At this point we can call procecures in a variadic way quite
;;; easily, for example by writing (variadic:call-associative 0
;;; fixnum:+ 1 2 3 4 5 6).  But we would like to just use the
;;; procedure name as a variadic macro, without having to specify
;;; every time the associativity direction and the neutral element...
;;; Here's the support letting us provide this information only once
;;; per procedure.  Notice that we need macros which expand to other
;;; macro definitions.

(e1:define-macro (variadic:define-left-deep name-as-sexpression procedure-name-as-sexpression neutral-element-as-sexpression)
  `(e1:define-macro (,name-as-sexpression . many-parameters)
    `(variadic:call-left-deep ,',neutral-element-as-sexpression ,',procedure-name-as-sexpression ,@many-parameters)))

(e1:define-macro (variadic:define-right-deep name-as-sexpression procedure-name-as-sexpression neutral-element-as-sexpression)
  `(e1:define-macro (,name-as-sexpression . many-parameters)
    `(variadic:call-right-deep ,',neutral-element-as-sexpression ,',procedure-name-as-sexpression ,@many-parameters)))

(e1:define-macro (variadic:define-associative name-as-sexpression procedure-name-as-sexpression neutral-element-as-sexpression)
  `(e1:define-macro (,name-as-sexpression . many-parameters)
    `(variadic:call-associative ,',neutral-element-as-sexpression ,',procedure-name-as-sexpression ,@many-parameters)))

;;; Put variadic definition operators into a more visible namespace:
(e1:define-macro (e1:define-variadic-left-deep . stuff)
  `(variadic:define-left-deep ,@stuff))
(e1:define-macro (e1:define-variadic-right-deep . stuff)
  `(variadic:define-right-deep ,@stuff))
(e1:define-macro (e1:define-variadic-associative . stuff)
  `(variadic:define-associative ,@stuff))
;;; FIXME: use these in the following.

;;; Now it's easy to let some procedures have variadic syntax:
(e1:toplevel
  (variadic:define-associative fixnum:+ fixnum:+ 0)
  (variadic:define-associative fixnum:* fixnum:* 1)
  (variadic:define-associative e1:xor e1:xor #f)
  (variadic:define-right-deep fixnum:** fixnum:** 1)
  (variadic:define-associative fixnum:bitwise-and fixnum:bitwise-and -1)
  (variadic:define-associative fixnum:bitwise-or fixnum:bitwise-or 0)
  (variadic:define-associative fixnum:bitwise-xor fixnum:bitwise-xor 0)
  (variadic:define-left-deep fixnum:left-shift fixnum:left-shift 0)
  (variadic:define-left-deep fixnum:right-shift fixnum:right-shift 0)

  (variadic:define-right-deep list:append list:append2 list:nil) ;; right-deep for performance

  ;;; Stack overflow if I do this.  I think the reason is the mutual
  ;;; dependency between quasiquoting and s-expression append.
  ;; ;; overwrite the old definition of sexpression:append: this generates faster code with >0 parameters
  ;; (variadic:define-right-deep sexpression:append sexpression:append2 sexpression:nil) ;; right-deep for performance

  (variadic:define-right-deep vector:append vector:append2 vector:empty)
  (variadic:define-right-deep string:append string:append2 string:empty))

;;; Some procedures have a very different behavior according to the
;;; number of their paramters; such behavior is useful, and customary
;;; in many Lisp dialects:
(e1:define-macro (fixnum:- . arguments)
  (e1:cond ((sexpression:null? arguments)
            ;; (-) ==> 0
            (sexpression:inject-fixnum 0))
           ((sexpression:null? (sexpression:cdr arguments))
            ;; (- a) ==> (negate a)
            `(fixnum:negate ,(sexpression:car arguments)))
           (else
            ;; (- a b c ...) ==> (- ... (- (- a b) c) ... )
            `(variadic:call-left-deep 0 fixnum:- ,@arguments)))) ; the neutral element is unused
            ;; (- a b1 ... bn) ==> (- a (+ b1 ... bn))
            ;; `(e0:call fixnum:-
            ;;           ,(sexpression:car arguments)
            ;;           (fixnum:+ ,@(sexpression:cdr arguments))))))
(e1:define-macro (fixnum:/ . arguments)
  (e1:cond ((sexpression:null? arguments)
            ;; (:) ==> 1
            (sexpression:inject-fixnum 1))
           ((sexpression:null? (sexpression:cdr arguments))
            ;; (/ a) ==> (/ 1 a)
            `(e0:call fixnum:/ 1 ,(sexpression:car arguments)))
           (else
            ;; (/ a b c ...) ==> (/ ... (/ (/ a b) c) ... )
            `(variadic:call-left-deep 0 fixnum:/ ,@arguments)))) ; the neutral element is unused

;;; Others just make sense with a certain minimum number of parameters:
(e1:define-macro (fixnum:min first-argument . more-arguments)
  `(variadic:call-associative 42 ;; unused: there is always at least one argument
                              fixnum:min
                              ,@(sexpression:cons first-argument more-arguments)))
(e1:define-macro (fixnum:max first-argument . more-arguments)
  `(variadic:call-associative 42 ;; unused: there is always at least one argument
                              fixnum:max
                              ,@(sexpression:cons first-argument more-arguments)))

;;; FIXME: the idea behind the implementation of fixnum:min and
;;; fixnum:max is to *always* involve the supposedly "neutral" element
;;; in the computation.  This should be generalized as a boolean
;;; option to variadic:define-* and variadic:call-*; even better,
;;; there could be different macros for that.

;;; Generalization of a three-argument procedure into a variadic
;;; syntax accepting 1 + 2n parameters, for any n.  This does not
;;; really fit the pattern above, so I define it from scratch.
;;; FIXME: generalize this if I find the same syntactic structure elsewhere
(e1:define-macro (alist:bind alist . rest)
  (e1:if (sexpression:null? rest)
    alist
    `(alist:bind (e0:call alist:bind ,alist ,(sexpression:car rest) ,(sexpression:cadr rest))
                 ,@(sexpression:cddr rest))))


;;;;; Less-essential sequencing forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(e1:define-macro (e1:begin1 first-form . more-forms)
  (e1:if (sexpression:null? more-forms)
    first-form
    (e1:let* ((first-form-result-name (sexpression:fresh-symbol)))
      `(e1:let* ((,first-form-result-name ,first-form))
         ,@more-forms
         ,first-form-result-name))))

(e1:define-macro (e1:begin-index index . forms)
  (e1:cond ((fixnum:< (sexpression:eject-fixnum index) 1)
            (e1:error "e1:begin-index: index less than 1"))
           ((fixnum:= (sexpression:eject-fixnum index) 1)
            `(e1:begin1 ,@forms))
           (else
            `(e1:begin
               ,(sexpression:car forms)
               (e1:begin-index
                 ,(sexpression:inject-fixnum (fixnum:1- (sexpression:eject-fixnum index)))
                 ,@(sexpression:cdr forms))))))

;;; Evaluate in order then return only the second, third, ..., result:
(e1:define-macro (e1:begin2 . forms) `(e1:begin-index 2 ,@forms))
(e1:define-macro (e1:begin3 . forms) `(e1:begin-index 3 ,@forms))
(e1:define-macro (e1:begin4 . forms) `(e1:begin-index 4 ,@forms))
(e1:define-macro (e1:begin5 . forms) `(e1:begin-index 5 ,@forms))

(e1:define-macro (e1:begin-index-from-the-end index . forms)
  (e1:cond ((fixnum:< (sexpression:length forms) (sexpression:eject-fixnum index))
            (e1:error "e1:begin-index-from-the-end: too few forms"))
           ((fixnum:= (sexpression:length forms) (sexpression:eject-fixnum index))
            `(e1:begin1 ,@forms))
           (else
            `(e1:begin
               ,(sexpression:car forms)
               (e1:begin-index-from-the-end
                 ,index
                 ,@(sexpression:cdr forms))))))

;;; Evaluate in order then return the next-to-last, next-to-next-to-last, ..., result:
(e1:define-macro (e1:begin-2 . forms) `(e1:begin-index-from-the-end 2 ,@forms))
(e1:define-macro (e1:begin-3 . forms) `(e1:begin-index-from-the-end 3 ,@forms))
(e1:define-macro (e1:begin-4 . forms) `(e1:begin-index-from-the-end 4 ,@forms))
(e1:define-macro (e1:begin-5 . forms) `(e1:begin-index-from-the-end 5 ,@forms))

;;; Just an alias, for consistency (potentially useful for making
;;; rapid changes while debugging)::
(e1:define-macro (e1:begin-1 . forms) `(e1:begin ,@forms))


;;;;; Sets as lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An implementation of sets using unsorted lists containing a single
;;; copy of each element.  We assume elements to be comparable by
;;; identity.

;;; We didn't need these before, so we waited till now to be able to
;;; define them in a more comfortable way.

(e1:define set-as-list:empty
  list:nil)

(e1:define (set-as-list:empty? set)
  (boolean:not set))

(e1:define (set-as-list:singleton element)
  (list:singleton element))

(e1:define (set-as-list:with set element)
  (e1:if (set-as-list:has? set element)
    set
    (list:cons element set)))

(e1:define (set-as-list:has? set element)
  (e1:cond ((set-as-list:empty? set)
            #f)
           ((whatever:eq? (list:head set) element)
            #t)
           (else
            (set-as-list:has? (list:tail set) element))))

(e1:define (set-as-list:without set element)
  (set-as-list:without-acc set element set-as-list:empty))
(e1:define (set-as-list:without-acc set element acc)
  (e1:cond ((set-as-list:empty? set)
            acc)
           ((whatever:eq? element (list:head set))
            (list:append-reversed (list:tail set) acc))
           (else
            (set-as-list:without-acc (list:tail set) element (list:cons (list:head set) acc)))))

(e1:define (set-as-list:list->set list)
  (set-as-list:list->set-acc list set-as-list:empty))

(e1:define (set-as-list:list->set-acc list acc)
  (e0:if-in list (0)
    acc
    (set-as-list:list->set-acc (list:tail list) (set-as-list:with acc (list:head list)))))

(e1:define (set-as-list:union set1 set2)
  (e1:if (set-as-list:empty? set1)
    set2
    (set-as-list:union (list:tail set1) (set-as-list:with set2 (list:head set1)))))

(e1:define (set-as-list:intersection set1 set2)
  (set-as-list:intersection-acc set1 set2 set-as-list:empty))
(e1:define (set-as-list:intersection-acc set1 set2 acc)
  (e1:cond ((set-as-list:empty? set1)
            acc)
           ((set-as-list:has? set2 (list:head set1))
            (set-as-list:intersection-acc (list:tail set1) set2 (set-as-list:with acc (list:head set1))))
           (else
            (set-as-list:intersection-acc (list:tail set1) set2 acc))))

(e1:define (set-as-list:subtraction set1 set2)
  (e1:if (set-as-list:empty? set2)
    set1
    (set-as-list:subtraction (set-as-list:without set1 (list:head set2))
                             (list:tail set2))))

;;; Set operators can have a very reasonable variadic syntax
(e1:toplevel
  (variadic:define-associative set-as-list:union set-as-list:union set-as-list:empty)
  (variadic:define-associative set-as-list:intersection set-as-list:intersection set-as-list:empty)
  (variadic:define-left-deep set-as-list:subtraction set-as-list:subtraction set-as-list:empty)
  (variadic:define-left-deep set-as-list:with set-as-list:with set-as-list:empty)
  (variadic:define-left-deep set-as-list:without set-as-list:without set-as-list:empty))
(e1:define-macro (set-as-list:make . elements)
  `(set-as-list:with set-as-list:empty ,@elements))


;;;;; Value-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A convenient way of building a list of literal constants:
(e1:define-macro (e1:value-list . values)
  (e1:if (sexpression:null? values)
         'list:nil
         `(list:cons (e0:value ,(sexpression:car values))
                     (e1:value-list ,@(sexpression:cdr values)))))


;;;;; Tuples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tuples are simply buffers with a constant number of elements
;;; (size not stored), with convenient syntax allowing to create,
;;; select, destructively or non-destructively update tuples.

(e1:define-macro (tuple:make . elements)
  (e1:let* ((result-name (sexpression:fresh-symbol))
            (element-no (sexpression:inject-fixnum (sexpression:length elements))))
    `(e1:let* ((,result-name (buffer:make ,element-no)))
       ,@(tuple:initialize-from!*s result-name 0 elements)
       ,result-name)))

;;; Return an s-list containing the initialization statements
(e1:define (tuple:initialize-from!*s tuple-sexpression-name index elements)
  (e1:if (sexpression:null? elements)
    '()
    `((buffer:initialize! ,tuple-sexpression-name
                          ,(sexpression:inject-fixnum index)
                          ,(sexpression:car elements))
      ,@(tuple:initialize-from!*s tuple-sexpression-name
                                  (fixnum:1+ index)
                                  (sexpression:cdr elements)))))

(e1:define-macro (tuple:explode-from tuple first-index element-no)
  (e1:let* ((tuple-name (sexpression:fresh-symbol)))
    `(e1:let* ((,tuple-name ,tuple))
       (e0:bundle ,@(tuple:explode-elements*s tuple-name
                                              (sexpression:eject-fixnum first-index)
                                              (sexpression:eject-fixnum element-no))))))

(e1:define-macro (tuple:explode tuple element-no)
  `(tuple:explode-from ,tuple 0 ,element-no))

;;; This will be useful for sum-of-products
(e1:define-macro (tuple:explode-from-second-element tuple element-no)
  `(tuple:explode-from ,tuple 1 ,element-no))

;;; Return an s-list containing the initialization statements
(e1:define (tuple:explode-elements*s tuple-name-sexpression minimum-element-index element-no)
  (e1:if (fixnum:<= element-no minimum-element-index)
    '()
    (e1:let* ((element-no-1 (fixnum:1- element-no)))
      `(,@(tuple:explode-elements*s tuple-name-sexpression minimum-element-index element-no-1)
        (e0:primitive buffer:get ,tuple-name-sexpression ,(sexpression:inject-fixnum element-no-1))))))

;;; Non-destructive substitution
(e1:define-macro (tuple:with tuple element-no index new-element)
  (e1:let* ((tuple-name (sexpression:fresh-symbol)))
    `(e1:let* ((,tuple-name ,tuple))
       (tuple:make ,@(tuple:substitution-elements*s tuple-name
                                                    (sexpression:eject-fixnum element-no)
                                                    (sexpression:eject-fixnum index)
                                                    new-element)))))

;;; Return an s-list containing the initialization statements
(e1:define (tuple:substitution-elements*s tuple-name-sexpression element-no index new-element-sexpression)
  (e1:if (fixnum:zero? element-no)
    '()
    (e1:let* ((element-no-1 (fixnum:1- element-no)))
      `(,@(tuple:substitution-elements*s tuple-name-sexpression element-no-1 index new-element-sexpression)
        ,(e1:if (fixnum:= element-no-1 index)
           new-element-sexpression
           `(e0:primitive buffer:get ,tuple-name-sexpression ,(sexpression:inject-fixnum element-no-1)))))))

;;; Lookup
(e1:define (tuple:get tuple index)
  (buffer:get tuple index))

;;; Destructive update
(e1:define (tuple:set! tuple index element)
  (e0:primitive buffer:set! tuple index element))
(e1:define (tuple:initialize! tuple index element)
  (e0:primitive buffer:initialize! tuple index element))

;;; Handy alias for building tuples:
(e1:define-macro (e1:tuple . elements)
  `(tuple:make ,@elements))


;;;;; Symbol utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These will come in handy for macros

(e1:define (symbol:append s1 s2)
  (symbol:string->symbol (string:append (symbol:symbol->string s1)
                                        (symbol:symbol->string s2))))

(e1:define symbol:empty
  (symbol:intern ""))

(e1:define sexpression:empty-symbol
  (sexpression:inject-symbol symbol:empty))

(e1:define (sexpression:append-symbols ssymbol1 ssymbol2)
  (sexpression:inject-symbol (symbol:append (sexpression:eject-symbol ssymbol1)
                                            (sexpression:eject-symbol ssymbol2))))

(e1:toplevel
  (variadic:define-right-deep symbol:append symbol:append symbol:empty)
  (variadic:define-right-deep sexpression:append-symbols sexpression:append-symbols sexpression:empty-symbol))


;;;;; Keyword arguments for procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A procedure defined with e1:define-with-keywords can be called
;;; either the usual way, or by supplying keyword arguments.
;;;
;;; If using keyword arguments, arguments may be supplied in any
;;; order, and parameters with a default may be omitted.
;;;
;;; When defining a procedure with keywords, parameters with a default
;;; are specified as (NAME DEFAULT-VALUE) instead of NAME in the
;;; parameter list.  The DEFAULT-VALUE is any s-expression, which will
;;; be supplied as it is (no renaming) in the call site.
;;;
;;; This keyword argument syntax is a *syntactic* astraction: no trace
;;; of keywords remains after macroexpansion as the call is rewritten
;;; into an ordinary non-keyword calls, including parameters (both
;;; implicit and explicit) in the correct order.  This reordering is
;;; important for parameters with side effects. [FIXME: shall I
;;; introduce a block just to avoid this?]

;;; The residual call code is as efficient as an ordinary non-keyword
;;; call.
;;;
;;; Examples:
;;; (e1:define-with-keywords (point x y (z 0)) ...)
;;; (point 1 2 3)                                 ;; traditional syntax
;;; (point #:y 10 #:z 2 #:x 4) ==> (point 4 10 2) ;; keyword parameters
;;; (point #:y 10 #:x 4)       ==> (point 4 10 0) ;; implicit parameter

(e1:define (sexpression:keyword? q)
  (e1:and (sexpression:symbol? q)
          (e1:let* ((symbol (sexpression:eject-symbol q))
                    (string (symbol:symbol->string symbol)))
            (e1:and (fixnum:>= (string:length string) 2)
                    (whatever:eq? (string:get string 0) #\#)
                    (whatever:eq? (string:get string 1) #\:)))))

(e1:define (keyword:is-in-table? symbol table)
  (e1:cond ((sexpression:null? table)
            #f)
           ((sexpression:symbol? (sexpression:car table))
            (e1:if (whatever:eq? (sexpression:eject-symbol (sexpression:car table))
                                 symbol)
              #t
              (keyword:is-in-table? symbol (sexpression:cdr table))))
           (else
            (e1:if (whatever:eq? (sexpression:eject-symbol (sexpression:caar table))
                                 symbol)
              #t
              (keyword:is-in-table? symbol (sexpression:cdr table))))))

;;; Keywords are symbols in epsilon1
(e1:define (keyword:keyword->symbol s)
  ;; FIXME: make this more efficient.  I need substring and subvector operators
  (symbol:string->symbol (vector:list->vector (list:tail (list:tail (vector:vector->list (symbol:symbol->string s)))))))

(e1:define (keyword:make-argument-alist-acc table actuals acc)
  (e1:cond ((sexpression:null? actuals)
            acc)
           ((sexpression:keyword? (sexpression:car actuals))
            (e1:let* ((symbol (keyword:keyword->symbol (sexpression:eject-symbol (sexpression:car actuals)))))
              (e1:cond ((alist:has? acc symbol)
                        (e1:error "keyword argument ~s supplied twice" symbol))
                       ((keyword:is-in-table? symbol table)
                        (keyword:make-argument-alist-acc table
                                                         (sexpression:cddr actuals)
                                                         (alist:bind acc symbol (sexpression:cadr actuals))))
                       (else
                        (e1:error "unknown keyword argument ~s" symbol)))))
           (else
            (e1:error "invalid keyword syntax ~s" actuals))))
(e1:define (keyword:make-argument-alist table actuals)
  (keyword:make-argument-alist-acc table actuals alist:nil))

;;; Using the alist containing the keyword arguments supplied by the user
;;; and the table containing formals possibly with defaults, build an s-list
;;; of actuals not using keywords.  The order is the same as in table.
(e1:define (keyword:adapt-args-recursive table alist)
  (e1:cond ((sexpression:null? table)
            '())
           ((sexpression:symbol? (sexpression:car table))
            (e1:if (alist:has? alist (sexpression:eject (sexpression:car table)))
              `(,(alist:lookup alist (sexpression:eject (sexpression:car table)))
                ,@(keyword:adapt-args-recursive (sexpression:cdr table) alist))
              (e1:error "missing non-optional parameter ~s" (sexpression:car table))))
           (else
            `(,(e1:if (alist:has? alist (sexpression:eject (sexpression:caar table)))
                 (alist:lookup alist (sexpression:eject (sexpression:caar table)))
                 (sexpression:cadar table))
              ,@(keyword:adapt-args-recursive (sexpression:cdr table) alist)))))

(e1:define (keyword:adapt-args table actuals)
  (e1:let* ((alist (keyword:make-argument-alist table actuals)))
    (keyword:adapt-args-recursive table alist)))

(e1:define-macro (keyword:call-possibly-with-keywords procedure table actuals)
  (e1:if (e1:or (sexpression:null? actuals)
                (sexpression:keyword? (sexpression:car actuals)))
    `(e0:call ,procedure ,@(keyword:adapt-args table actuals))
    `(e0:call ,procedure ,@actuals)))

(e1:define (keyword:table->formals table)
  (e1:cond ((sexpression:null? table)
            '())
           ((sexpression:symbol? (sexpression:car table))
            (sexpression:cons (sexpression:car table)
                              (keyword:table->formals (sexpression:cdr table))))
           ;; We perform this check here; it's as good a place as any other
           ((e1:or (e1:not (sexpression:cons? (sexpression:car table)))
                   (sexpression:null? (sexpression:cdar table))
                   (e1:not (sexpression:null? (sexpression:cddar table))))
            (e1:error "ill-formed default parameter ~s" (sexpression:car table)))
           (else
            (sexpression:cons (sexpression:caar table)
                              (keyword:table->formals (sexpression:cdr table))))))

(e1:define-macro (e1:define-with-keywords procedure-and-table . body)
  (e1:let* ((procedure-name (sexpression:car procedure-and-table))
            (table (sexpression:cdr procedure-and-table))
            ;;(all-args-name (sexpression:fresh-symbol "all-args")))
            (all-args-name (sexpression:fresh-symbol)))
    `(e1:begin
      ;; Define the keyword syntax *first*: it could be used
      ;; recursively in the body, or we may have to undo a previous
      ;; incompatbile definition, which again would screw up the
      ;; body exapansion if we defined this too late.
      (e1:define-macro (,procedure-name . ,all-args-name)
        `(keyword:call-possibly-with-keywords ,',procedure-name
                                              ,',table
                                              ,,all-args-name))
      (e1:define ,(sexpression:cons procedure-name (keyword:table->formals table))
        ,@body))))


;;;;; Records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define e1:word-separator-string (e0:value "-"))
(e1:define e1:namespace-separator-string (e0:value ":"))

(e1:define e1:word-separator-symbol (symbol:string->symbol e1:word-separator-string))
(e1:define e1:word-separator e1:word-separator-symbol)
(e1:define e1:namespace-separator-symbol (symbol:string->symbol e1:namespace-separator-string))
(e1:define e1:namespace-separator e1:namespace-separator-symbol)

;;; The record table maps each record name string into a record
;;; descriptor (conceptually another record), containing in order:
;;; - an ordered list of fields, as symbols.
(e1:define record:table
  (string-hash:make))

(e1:define (record:name->descriptor name-as-symbol)
  (string-hash:get record:table (symbol:symbol->string name-as-symbol)))
(e1:define (record:name->field-names name-as-symbol)
  (tuple:get (record:name->descriptor name-as-symbol) 0))

(e1:define-macro (record:define record-name-symbol . field-names)
  (e1:let* ((common-first-part-string (string:append (symbol:symbol->string (sexpression:eject-symbol record-name-symbol))
                                                     e1:word-separator-string))
            (constructor-name-string (symbol:symbol->string (sexpression:eject-symbol record-name-symbol)))
            (constructor-uninitialized-name-string (string:append common-first-part-string "make-uninitialized"))
            (exploder-name-string (string:append common-first-part-string "explode"))
            (exploder-from-second-element-name-string (string:append common-first-part-string "explode-from-second-element"))
            (constructor-name-symbol (symbol:string->symbol constructor-name-string))
            (constructor-uninitialized-name-symbol (symbol:string->symbol constructor-uninitialized-name-string))
            (exploder-name-symbol (symbol:string->symbol exploder-name-string))
            (exploder-from-second-element-name-symbol (symbol:string->symbol exploder-from-second-element-name-string))
            (field-no (sexpression:length field-names))
            (field-no-sexpression (sexpression:inject-fixnum field-no)))
    `(e1:begin
       (record:define-metadata (e0:value ,record-name-symbol)
                               (e1:value-list ,@field-names))
       (e1:define-with-keywords (,(sexpression:inject-symbol constructor-name-symbol) ,@field-names)
         (tuple:make ,@field-names))
       (e1:define (,(sexpression:inject-symbol constructor-uninitialized-name-symbol))
         (buffer:make ,(sexpression:inject-fixnum (sexpression:length field-names))))
       (e1:define (,(sexpression:inject-symbol exploder-name-symbol) ,record-name-symbol)
         (tuple:explode ,record-name-symbol ,field-no-sexpression))
       (e1:define (,(sexpression:inject-symbol exploder-from-second-element-name-symbol) ,record-name-symbol)
         (tuple:explode-from-second-element ,record-name-symbol ,field-no-sexpression))
       (record:define-accessors ,record-name-symbol
                                ,field-names
                                0
                                ,field-no-sexpression))))

(e1:define-macro (record:define-accessors record-name-symbol-sexpression
                                          field-names-slist
                                          index-sexpression
                                          field-no-sexpression)
  (e1:if (sexpression:null? field-names-slist)
    '(e0:bundle)
    (e1:let* ((record-name-symbol (sexpression:eject-symbol record-name-symbol-sexpression))
              (record-name-string (symbol:symbol->string record-name-symbol))
              (common-first-part-string (string:append record-name-string e1:word-separator-string))
              (first-field-name-symbol-sexpression (sexpression:car field-names-slist))
              (first-field-name-symbol (sexpression:eject-symbol first-field-name-symbol-sexpression))
              (first-field-name-string (symbol:symbol->string first-field-name-symbol)))
      `(e1:begin
         (e1:define (,(sexpression:inject-symbol (symbol:string->symbol (string:append common-first-part-string
                                                                                       "get"
                                                                                       e1:word-separator-string
                                                                                       first-field-name-string)))
                     ,record-name-symbol-sexpression)
           (tuple:get ,record-name-symbol-sexpression ,index-sexpression))
         (e1:define (,(sexpression:inject-symbol (symbol:string->symbol (string:append common-first-part-string
                                                                                       "with"
                                                                                       e1:word-separator-string
                                                                                       first-field-name-string)))
                     ,record-name-symbol-sexpression
                     ,first-field-name-symbol-sexpression)
           (tuple:with ,record-name-symbol-sexpression ,field-no-sexpression ,index-sexpression ,first-field-name-symbol-sexpression))
         (e1:define (,(sexpression:inject-symbol (symbol:string->symbol (string:append common-first-part-string
                                                                                       "set"
                                                                                       e1:word-separator-string
                                                                                       first-field-name-string
                                                                                       "!")))
                     ,record-name-symbol-sexpression
                     ,first-field-name-symbol-sexpression)
           (tuple:set! ,record-name-symbol-sexpression ,index-sexpression ,first-field-name-symbol-sexpression))
         (record:define-accessors ,record-name-symbol-sexpression
                                  ,(sexpression:cdr field-names-slist)
                                  ,(sexpression:1+ index-sexpression)
                                  ,field-no-sexpression)))))

(e1:define (record:define-metadata record-name-symbol field-name-symbols)
  (e1:let* ((record-name-string (symbol:symbol->string record-name-symbol))
            (descriptor (tuple:make field-name-symbols)))
    (string-hash:set! record:table record-name-string descriptor)))

;;; Record definition is useful in practice: give it a visible namespace:
(e1:define-macro (e1:define-record . stuff)
  `(record:define ,@stuff))
;;; FIXME: use this in the following


;;;;; Sum-of-product "types", ML-style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Each case may be implemented as:
;;; * trival unboxed (single fixnum)
;;; * trivial boxed (no header word)
;;; * nontrivial (boxed, header word)
;;;
;;; Only if all cases except at most one have no elements, we can
;;; implement them as trivial; if there are two or more cases with
;;; elements, *all* cases must be implemented as nontrivial.
;;;
;;; In "open" sum types new cases may be added at any time without
;;; changing representation for old cases, so we have to
;;; conservatively assume that a trivial implementation is not
;;; possible.

(e1:define-macro (sum:define sum-name . cases)
  (e1:let* ((trivial (sum:trivial-cases? cases))
            (sum-name-as-string (symbol:symbol->string (sexpression:eject-symbol sum-name))))
    `(e1:begin
       ,(e1:if trivial
         `(sum:define-trivial-cases ,sum-name 0 ,@cases)
         `(sum:define-nontrivial-cases ,sum-name 0 ,@cases))
      (string-hash:set! sum:table
                        ,(sexpression:inject-string sum-name-as-string)
                        (sum:descriptor ,(sexpression:inject-boolean trivial) ',cases)))))

(e1:define-macro (sum:define-open sum-name . cases)
  `(e1:begin ;; an open sum is just like a nontrivial sum:
     (sum:define-nontrivial-cases ,sum-name 0 ,@cases)
     (string-hash:set! sum:table
                      ,(sexpression:inject-string (symbol:symbol->string (sexpression:eject-symbol sum-name)))
                      (sum:descriptor #f ',cases))))
(e1:define-macro (sum:extend-open sum-name . new-cases)
  (e1:let* ((old-cases-name (sexpression:fresh-symbol))
            (descriptor-name (sexpression:fresh-symbol)))
    `(e1:let* ((,old-cases-name (sum:name->cases (e0:value ,sum-name)))
               (,descriptor-name (sum:name->descriptor (e0:value ,sum-name))))
      (sum:descriptor-set-cases-sexpression! ,descriptor-name (sexpression:append ,old-cases-name ',new-cases))
      (sum:define-nontrivial-cases ,sum-name
                                   ,(sexpression:inject-fixnum (sum:name->case-no (sexpression:eject-symbol sum-name)))
                                   ,@new-cases))))

(e1:define (sum:trivial-cases? cases-sexpression)
  (sum:trivial-cases-acc? cases-sexpression 0))
(e1:define (sum:trivial-cases-acc? cases-sexpression case-with-element-no)
  (e1:cond ((sexpression:null? cases-sexpression)
            #t)
           ((sexpression:null? (sexpression:cdar cases-sexpression))
            (sum:trivial-cases-acc? (sexpression:cdr cases-sexpression) case-with-element-no))
           ;; If we arrived here we found a case with elements.
           ((fixnum:zero? case-with-element-no)
            (sum:trivial-cases-acc? (sexpression:cdr cases-sexpression) 1))
           (else ;; it's not the first one...
            #f)))

;;; The sum table maps each sum name string into a record
;;; descriptor, containing in order:
;;; - A boolean: is it the sum trivial?
;;; - the cases s-expression as given at definition time
(e1:toplevel (record:define sum:descriptor trivial cases-sexpression))
(e1:define sum:table
  (string-hash:make))

(e1:define (sum:name->descriptor name-as-symbol)
  (string-hash:get sum:table (symbol:symbol->string name-as-symbol)))
(e1:define (sum:trivial? name-as-symbol)
  (sum:descriptor-get-trivial (sum:name->descriptor name-as-symbol)))
(e1:define (sum:name->cases name-as-symbol)
  (sum:descriptor-get-cases-sexpression (sum:name->descriptor name-as-symbol)))
(e1:define (sum:name->case-no name-as-symbol)
  (sexpression:length (sum:name->cases name-as-symbol)))

(e1:define-macro (sum:define-trivial-cases sum-name case-index . cases)
  (e1:if (sexpression:null? cases)
    '(e0:bundle)
    `(e1:begin
      ,(e1:if (sexpression:null? (sexpression:cdar cases))
        `(sum:define-trivial-unboxed-case ,sum-name ,case-index ,@(sexpression:car cases))
        `(sum:define-trivial-boxed-case ,sum-name ,@(sexpression:car cases)))
      (sum:define-trivial-cases ,sum-name
                                ;; We only increment the tag index if we actually represent it
                                ,(e1:if (e1:not (sexpression:null? (sexpression:cdar cases)))
                                   case-index
                                   (sexpression:1+ case-index))
                                ,@(sexpression:cdr cases)))))

(e1:define-macro (sum:define-nontrivial-cases sum-name case-index . cases)
  (e1:if (sexpression:null? cases)
    '(e0:bundle)
    `(e1:begin
      (sum:define-nontrivial-case ,sum-name ,case-index ,@(sexpression:car cases))
      (sum:define-nontrivial-cases ,sum-name ,(sexpression:1+ case-index) ,@(sexpression:cdr cases)))))

(e1:define-macro (sum:define-trivial-unboxed-case sum-name case-index case-name)
  (e1:let* ((maker-name (symbol:append (sexpression:eject-symbol sum-name)
                                       e1:word-separator
                                       (sexpression:eject case-name)))
            (checker-name (symbol:append (sexpression:eject-symbol sum-name)
                                         e1:word-separator
                                         (sexpression:eject case-name)
                                         (e0:value ?)))
            (exploder-name (symbol:append (sexpression:eject-symbol sum-name)
                                          e1:word-separator
                                          (sexpression:eject case-name)
                                          e1:word-separator
                                          (e0:value explode))))
    `(e1:begin
      (e1:define (,(sexpression:inject-symbol maker-name))
        ,case-index)
      (e1:define (,(sexpression:inject-symbol checker-name) ,sum-name)
        (whatever:eq? ,sum-name ,case-index))
      (e1:define (,(sexpression:inject-symbol exploder-name) ,sum-name)
        (e0:bundle)))))

(e1:define-macro (sum:define-trivial-boxed-case sum-name case-name . case-elements)
  (e1:let* ((record-name (symbol:append (sexpression:eject-symbol sum-name)
                                        e1:word-separator
                                        (sexpression:eject-symbol case-name)))
            (checker-name (symbol:append (sexpression:eject-symbol sum-name)
                                         e1:word-separator
                                         (sexpression:eject-symbol case-name)
                                         (e0:value ?))))
  `(e1:begin
     (record:define ,(sexpression:inject-symbol record-name)
                    ,@case-elements)
     (e1:define (,(sexpression:inject-symbol checker-name)
                 ,case-name)
       (boxedness:potentially-boxed? ,case-name)))))

(e1:define-macro (sum:define-nontrivial-case sum-name case-index case-name . case-elements)
  (e1:let* ((instance-name (symbol:append (sexpression:eject-symbol sum-name)
                                          e1:word-separator
                                          (sexpression:eject-symbol case-name)
                                          e1:word-separator
                                          (e0:value instance)))
            (record-name (symbol:append (sexpression:eject-symbol sum-name)
                                        e1:word-separator
                                        (sexpression:eject case-name)))
            (maker-name (symbol:append (sexpression:eject-symbol sum-name)
                                       e1:word-separator
                                       (sexpression:eject case-name)))
            (checker-name (symbol:append (sexpression:eject-symbol sum-name)
                                         e1:word-separator
                                         (sexpression:eject case-name)
                                         (e0:value ?)))
            (exploder-name (symbol:append (sexpression:eject-symbol sum-name)
                                          e1:word-separator
                                          (sexpression:eject case-name)
                                          e1:word-separator
                                          (e0:value explode)))
            (element-no (sexpression:length case-elements))
            (slot-no (fixnum:1+ element-no)))
    `(e1:begin
      ,(e1:if (sexpression:null? case-elements)
         `(e1:begin ;; if there are no elements we can always reuse the same instance
            (e1:define ,(sexpression:inject-symbol instance-name)
              (tuple:make ,case-index))
            (e1:define (,(sexpression:inject-symbol maker-name))
              ,(sexpression:inject-symbol instance-name)))
         `(e1:define-with-keywords (,(sexpression:inject-symbol maker-name) ,@case-elements)
            (tuple:make ,case-index ,@case-elements)))
      (e1:define (,(sexpression:inject-symbol checker-name) ,sum-name)
        (whatever:eq? (buffer:get ,sum-name 0) ,case-index))
      (e1:define (,(sexpression:inject-symbol exploder-name) ,sum-name)
        (tuple:explode-from-second-element ,sum-name ,(sexpression:inject-fixnum slot-no)))
      ;; Notice that we didn't define a record corresponding to the sum type: we're only
      ;; recycling our accessor-defining logic.
      (record:define-accessors ,(sexpression:inject-symbol record-name)
                               ,case-elements
                               ,(sexpression:inject-fixnum 1)
                               ,(sexpression:inject-fixnum slot-no)))))

;;; Sum definition is useful in practice: give it a visible namespace:
(e1:define-macro (e1:define-sum . stuff)
  `(sum:define ,@stuff))
(e1:define-macro (e1:define-sum-open . stuff)
  `(sum:define-open ,@stuff))
(e1:define-macro (e1:extend-sum . stuff)
  `(sum:extend-open ,@stuff))
;;; FIXME: use these in the following


;;;; Lists as sum types:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Re-define lists as a sum type.  Of course the representation will
;;; be the same, so we can still use the procedures we have defined up
;;; to this point; but thanks to this, we will have pattern matching
;;; as well.

(e1:toplevel (sum:define list:list
               (nil)
               (cons head tail)))


;;;; Expressions as an open sum type:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We can now finally re-define epsilonzero expressions as an open
;;; sum type: this has de advantage of also setting up metadata
;;; letting us define extensions; as per expression data structure
;;; themselves, this definition is compatibile with the old one.  In
;;; particular, the memory representation of each case is exactly the
;;; same -- by design.

(e1:toplevel (sum:define-open e0:expression
               (variable handle name)
               (value handle content)
               (bundle handle items)
               (primitive handle name actuals)
               (let handle bound-variables bound-expression body)
               (call handle procedure-name actuals)
               (call-indirect handle procedure-expression actuals)
               (if-in handle discriminand values then-branch else-branch)
               (fork handle procedure-name actuals)
               (join handle future)))

;;; Given an expression, which can have any case, return its handle:
(e1:define (e0:expression-get-handle expression)
  ;; This relies on the sum representation, but it will always work as
  ;; long as extended cases keep a handle as their first element.
  (buffer:get expression 1)) ;; right after the tag


;;;; Closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Add lambda as a new expression syntactic case (we don't need to do
;;; that for calling closures: closure application is a simple macro, but
;;; lambda must be transformed away, since it's context-dependent)
(e1:toplevel (sum:extend-open e0:expression
               (lambda handle formals body)))

(e1:toplevel (sum:extend-open e0:expression
               (call-closure handle closure-expression actuals)))

;;; Friendly constructor, similar to the epsilon0 syntactic cases:
(e1:define (e1:lambda* formals body)
  (e0:expression-lambda (e0:fresh-handle) formals body))
(e1:define (e1:call-closure* closure-expression actuals)
  (e0:expression-call-closure (e0:fresh-handle) closure-expression actuals))

;;; User syntax: the macro generates an extended form which the
;;; interpreter does not recognize, and must therefore be transformed
;;; away before execution:
(e1:define-macro (e1:lambda formals . body-forms)
  (sexpression:inject-expression
    (e1:lambda* (sexpression:eject-symbols formals)
                (e1:macroexpand `(e1:begin ,@body-forms)))))
(e1:define-macro (e1:call-closure closure-expression . actuals)
  (sexpression:inject-expression
    (e1:call-closure* (e1:macroexpand closure-expression)
                      (e1:macroexpand-sexpressions actuals))))

;;; Free variables in an epsilon0 expression.  This will be useful to
;;; compute the set of variables to close over, anyway we *don't* need
;;; to support lambda here, since we can call this procedure on
;;; expressions where lambda has already been transformed away.
(e1:define (e0:free-variables e)
  (e1:cond ((e0:expression-variable? e)
            (set-as-list:singleton (e0:expression-variable-get-name e)))
           ((e0:expression-value? e)
            set-as-list:empty)
           ((e0:expression-bundle? e)
            (e0:free-variables-of-expressions (e0:expression-bundle-get-items e)))
           ((e0:expression-primitive? e)
            (e0:free-variables-of-expressions (e0:expression-primitive-get-actuals e)))
           ((e0:expression-let? e)
            (set-as-list:union (e0:free-variables (e0:expression-let-get-bound-expression e))
                               (set-as-list:subtraction (e0:free-variables (e0:expression-let-get-body e))
                                                        (set-as-list:list->set (e0:expression-let-get-bound-variables e)))))
           ((e0:expression-call? e)
            (e0:free-variables-of-expressions (e0:expression-call-get-actuals e)))
           ((e0:expression-call-indirect? e)
            (set-as-list:union (e0:free-variables (e0:expression-call-indirect-get-procedure-expression e))
                               (e0:free-variables-of-expressions (e0:expression-call-indirect-get-actuals e))))
           ((e0:expression-if-in? e)
            (set-as-list:union (e0:free-variables (e0:expression-if-in-get-discriminand e))
                               (e0:free-variables (e0:expression-if-in-get-then-branch e))
                               (e0:free-variables (e0:expression-if-in-get-else-branch e))))
           ((e0:expression-fork? e)
            (e0:free-variables-of-expressions (e0:expression-fork-get-actuals e)))
           ((e0:expression-join? e)
            (e0:free-variables (e0:expression-join-get-future e)))
           (else
            (e1:error "e0:free-variables: unknown extended or invalid expression"))))
(e1:define (e0:free-variables-of-expressions es)
  (e0:free-variables-of-expressions-acc es set-as-list:empty))
(e1:define (e0:free-variables-of-expressions-acc es acc)
  (e0:if-in es (0)
    acc
    (e0:free-variables-of-expressions-acc (list:tail es)
                                          (set-as-list:union (e0:free-variables (list:head es)) acc))))

;;; Replace all lambdas in an epsilon0+lambda expression.  Bound
;;; variables are encoded as a set-as-list
(e1:define (closure:closure-convert e bounds)
  (e1:cond ((e0:expression-variable? e)
            (e0:variable* (e0:expression-variable-get-name e)))
           ((e0:expression-value? e)
            (e0:value* (e0:expression-value-get-content e)))
           ((e0:expression-bundle? e)
            (e0:bundle* (closure:closure-convert-expressions (e0:expression-bundle-get-items e) bounds)))
           ((e0:expression-primitive? e)
            (e0:primitive* (e0:expression-primitive-get-name e)
                           (closure:closure-convert-expressions (e0:expression-primitive-get-actuals e) bounds)))
           ((e0:expression-let? e) ;; Interesting case
            (e1:let* ((new-bounds (e0:expression-let-get-bound-variables e)))
              (e0:let* new-bounds
                       (closure:closure-convert (e0:expression-let-get-bound-expression e) bounds)
                       (closure:closure-convert (e0:expression-let-get-body e)
                                                           (set-as-list:union bounds new-bounds)))))
           ((e0:expression-call? e)
            (e0:call* (e0:expression-call-get-procedure-name e)
                      (closure:closure-convert-expressions (e0:expression-call-get-actuals e) bounds)))
           ((e0:expression-call-indirect? e)
            (e0:call-indirect* (closure:closure-convert (e0:expression-call-indirect-get-procedure-expression e) bounds)
                               (closure:closure-convert-expressions (e0:expression-call-indirect-get-actuals e) bounds)))
           ((e0:expression-if-in? e)
            (e0:if-in* (closure:closure-convert (e0:expression-if-in-get-discriminand e) bounds)
                       (e0:expression-if-in-get-values e)
                       (closure:closure-convert (e0:expression-if-in-get-then-branch e) bounds)
                       (closure:closure-convert (e0:expression-if-in-get-else-branch e) bounds)))
           ((e0:expression-fork? e)
            (e0:fork* (e0:expression-fork-get-procedure-name e)
                      (closure:closure-convert-expressions (e0:expression-fork-get-actuals e) bounds)))
           ((e0:expression-join? e)
            (e0:join* (closure:closure-convert (e0:expression-join-get-future e) bounds)))
           ((e0:expression-lambda? e) ;; Interesting case
            (e1:let* ((formals (e0:expression-lambda-get-formals e))
                      (nonlocals (set-as-list:subtraction bounds formals))
                      (old-body (e0:expression-lambda-get-body e))
                      (new-body (closure:closure-convert old-body (set-as-list:union bounds formals)))
                      ;; FIXME: use caching, or find some other way of making this efficient.  Anyway
                      ;; generating minimal closures seems to be globally faster than the alternative.
                      (used-nonlocals (set-as-list:intersection nonlocals (e0:free-variables new-body)))
                      ;;(used-nonlocals nonlocals) ;; FIXME: the commented-out version above generates smaller closures (but transforms more slowly)
                      )
               (closure:make* used-nonlocals
                              (closure:variables* used-nonlocals)
                              formals
                              new-body)))
           ;; ((e0:expression-call-closure? e) ;; Notice that this would have worked as a macro as well
           ;;  (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
           ;;            (actuals (e0:expression-call-closure-get-actuals e))
           ;;            ;;; This is an optimization to avoid generating a trivial block every time. It's
           ;;            ;;; particularly useful to understand the generated code at debug time.
           ;;            ;;; FIXME: I should simplify this later, after I introduce the generic expression
           ;;            ;;; simplifier, which will be trivially able to remove useless lets.
           ;;            (trivial-closure (e0:expression-variable? closure-expression))
           ;;            (transformed-closure-name (e1:if trivial-closure
           ;;                                        (e0:expression-variable-get-name closure-expression)
           ;;                                        (symbol:fresh)))
           ;;            (transformed-body
           ;;             (e0:call-indirect* (e0:primitive* (e0:value buffer:get)
           ;;                                               (list:list (e0:variable* transformed-closure-name) (e0:value* 0)))
           ;;                                (list:cons (e0:variable* transformed-closure-name)
           ;;                                           (closure:closure-convert-expressions actuals bounds)))))
           ;;    (e1:if trivial-closure
           ;;      transformed-body
           ;;      (e0:let* (list:singleton transformed-closure-name)
           ;;               (closure:closure-convert closure-expression bounds)
           ;;               transformed-body))))
           ;; Eeasier, unoptimized version, to show in my thesis.
           ;; The commented-out version above works; it's ugly, but
           ;; slightly more efficient.
           ((e0:expression-call-closure? e) ;; Notice that this would have worked as a macro as well
            (e1:let* ((closure-expression (e0:expression-call-closure-get-closure-expression e))
                      (actuals (e0:expression-call-closure-get-actuals e))
                      (transformed-closure-name (symbol:fresh)))
              (e0:let* (list:singleton transformed-closure-name)
                       (closure:closure-convert closure-expression bounds)
                       (e0:call-indirect* (e0:primitive* (e0:value buffer:get)
                                                         (list:list (e0:variable* transformed-closure-name) (e0:value* 0)))
                                          (list:cons (e0:variable* transformed-closure-name)
                                                     (closure:closure-convert-expressions actuals bounds))))))
           (else
            (string:write "About the tag ")
            (fixnum:write (buffer:get e 0))
            (string:write "\n")
            (e1:error "closure:closure-convert-expression: unknown extended or invalid expression"))))
(e1:define (closure:closure-convert-expressions es bounds)
  (e1:if (list:null? es)
    list:nil
    (list:cons (closure:closure-convert (list:head es) bounds)
               (closure:closure-convert-expressions (list:tail es) bounds))))

;;; Add lambda-replacement as a transformation, for both non-procedures and
;;; procedures:
(e1:define (closure:closure-convert-expression-transform expression)
  (closure:closure-convert expression set-as-list:empty))
(e1:define (closure:closure-convert-procedure-transform name formals body)
  (e0:bundle name formals (closure:closure-convert body formals)))

(e1:toplevel (transform:prepend-expression-transform!
                (e0:value closure:closure-convert-expression-transform)))
(e1:toplevel (transform:prepend-procedure-transform!
                (e0:value closure:closure-convert-procedure-transform)))

;; ;;; In closure calls, the evaluation strategy is:
;; ;;; a) first the closure;
;; ;;; b) then its actuals, left-to-right.
;; (e1:define-macro (closure:call closure . actuals)
;;   (e1:let* ((closure-name (sexpression:fresh-symbol)))
;;     `(e0:let (,closure-name) ,closure
;;        (e0:call-indirect (e0:primitive buffer:get ,closure-name 0) ,closure-name ,@actuals))))

;; ;;; More efficient version allowing to call a closure whose procedure
;; ;;; component is known in advance:
;; (e1:define-macro (closure:call-known procedure-name closure . actuals)
;;   (e1:let* ((closure-name (sexpression:fresh-symbol)))
;;     `(e0:let (,closure-name) ,closure
;;        (e0:call ,procedure-name ,closure-name ,@actuals))))

;; (e1:define (closure:with-nonlocals*s closure-name nonlocal-names next-index body-forms)
;;   (e1:if (sexpression:null? nonlocal-names)
;;     `(e1:begin ,@body-forms)
;;     `(e0:let (,(sexpression:car nonlocal-names))
;;              (e0:primitive buffer:get ,closure-name ,next-index)
;;        ,(closure:with-nonlocals*s closure-name
;;                                   (sexpression:cdr nonlocal-names)
;;                                   (sexpression:inject-fixnum (fixnum:1+ (sexpression:eject-fixnum next-index)))
;;                                   body-forms))))

;; (e1:define-macro (closure:make-uninitialized nonlocal-names formals . body-forms)
;;   (e1:let* ((closure-name (sexpression:fresh-symbol))
;;             (closure-formal-name (sexpression:fresh-symbol))
;;             (procedure-name (sexpression:fresh-symbol))
;;             (nonlocal-no (sexpression:length nonlocal-names))
;;             (closure-length (sexpression:inject-fixnum (fixnum:+ 1 nonlocal-no))))
;;     ;; Define the procedure associated to the closure, only once at
;;     ;; macroexpansion time:
;;     (state:procedure-set! (sexpression:eject-symbol procedure-name)
;;                           (sexpression:eject-symbols (sexpression:cons closure-formal-name formals))
;;                           (e1:macroexpand (closure:with-nonlocals*s closure-formal-name
;;                                                                     nonlocal-names
;;                                                                     (sexpression:inject-fixnum 1)
;;                                                                     body-forms)))
;;     ;; The code returned by the macro is much simpler: it just builds
;;     ;; the closure data structure.  Notice that the code *returned* by
;;     ;; this macro may be executed many times, but the procedure
;;     ;; definition above is performed only once.
;;     `(e0:let (,closure-name) (e0:primitive buffer:make ,closure-length)
;;        (e1:begin
;;          (e0:primitive buffer:initialize! ,closure-name 0 (e0:value ,procedure-name))
;;          ,closure-name))))

;; ;;; Initialize the nonlocals in the given closure with the result of
;; ;;; the evaluation of the given nonlocals, starting from the first
;; ;;; nonlocal:
;; (e1:define-macro (closure:initialize-nonlocals! closure-variable-name . nonlocals)
;;   `(e1:begin
;;      ,@(closure:initialize-nonlocals-from!*s `,closure-variable-name `,nonlocals 1)))
;; ;;; closure:initialize-nonlocals-from!*s returns a *list* of forms as
;; ;;; s-expressions, to be spliced.
;; (e1:define (closure:initialize-nonlocals-from!*s closure-variable-name nonlocals index)
;;   (e1:if (sexpression:null? nonlocals)
;;     '()
;;     `((e0:primitive buffer:initialize! ,closure-variable-name ,(sexpression:inject-fixnum index) ,(sexpression:car nonlocals))
;;       ,@(closure:initialize-nonlocals-from!*s closure-variable-name (sexpression:cdr nonlocals) (fixnum:1+ index)))))

;;; Make an expression producing a closure when executed.  This would
;;; have been more conveient to directly define as a macro, but since
;;; I need this functionality from within a transformation, I have to
;;; do this in order not to introduce a circular dependency.  Of
;;; course it's important that the returned code be efficient.
(e1:define (closure:make* nonlocal-names nonlocal-expressions formals body-expression)
  (e1:let* ((procedure-name (symbol:fresh))
            (closure-name (symbol:fresh))) ; we also use this as the closure hidden parameter name
    (e1:unless (fixnum:= (list:length nonlocal-names) (list:length nonlocal-expressions))
      (e1:error "closure:make*: nonlocal-names and nonlocal-expressions have different sizes"))
    (e1:begin
      ;; Define the procedure once, at generation time:
      (state:procedure-set! procedure-name
                            (list:cons closure-name formals)
                            (closure:body* closure-name nonlocal-names body-expression 1))
      ;; Important optimization: if the closure is trivial, which is to say has zero nonlocals,
      ;; we can make the closure datastructure once and for all at generation time:
      (e1:if (list:null? nonlocal-names)
        ;; Define the closure at generation time, and simply return
        ;; its name in a global variable as the result expression:
        (e1:let* ((result (e0:primitive buffer:make 1)))
          (buffer:set! result 0 procedure-name)
          (state:global-set! closure-name result)
          (e0:variable* closure-name))
        ;; Return the expression making the closure:
        (e0:let* (list:list closure-name)
                 (e0:primitive* (e0:value buffer:make)
                                (list:list (e0:value* (fixnum:1+ (list:length nonlocal-names)))))
          ;; Initialize the procedure field:
          (e0:let* list:nil (e0:primitive* (e0:value buffer:set!)
                                           (list:list (e0:variable* closure-name)
                                                      (e0:value* 0)
                                                      (e0:value* procedure-name)))
            ;; Initialize nonlocal fields, and return:
            (closure:initialize* closure-name nonlocal-expressions 1)))))))
(e1:define (closure:body* closure-name nonlocal-names body-expression next-index)
  (e1:if (list:null? nonlocal-names)
    body-expression
    (e0:let* (list:list (list:head nonlocal-names))
             (e0:primitive* (e0:value buffer:get)
                            (list:list (e0:variable* closure-name)
                                       (e0:value* next-index)))
      (closure:body* closure-name (list:tail nonlocal-names) body-expression (fixnum:1+ next-index)))))
(e1:define (closure:initialize* closure-name nonlocal-expressions next-index)
  (e1:if (list:null? nonlocal-expressions)
    (e0:variable* closure-name)
    (e0:let* list:nil (e0:primitive* (e0:value buffer:set!)
                                     (list:list (e0:variable* closure-name)
                                                (e0:value* next-index)
                                                (list:head nonlocal-expressions)))
      (closure:initialize* closure-name (list:tail nonlocal-expressions) (fixnum:1+ next-index)))))


;;; FIXME: Using this directly, without passing thru lambda, is broken
;;; by CPS.  Is there an easy way to fix that, without extending
;;; epsilon0 expressions?  Should I have this *instead* of lambda, as
;;; a syntactic case?

;;; Make a closure, given the explicit list of nonlocals to keep
(e1:define-macro (closure:make nonlocal-names nonlocal-contents formals . body-forms)
  (sexpression:inject-expression
    (closure:make* (sexpression:eject-symbols nonlocal-names)
                   (e1:macroexpand-sexpressions nonlocal-contents)
                   (sexpression:eject-symbols formals)
                   (e1:macroexpand `(e1:begin ,@body-forms)))))
(e1:define (closure:variables* variable-names)
  (e1:if (list:null? variable-names)
    list:nil
    (list:cons (e0:variable* (list:head variable-names))
               (closure:variables* (list:tail variable-names)))))

;;; FIXME: the macro above is fine and covers the common case, but we
;;; could do something more general, associating an arbitrary
;;; user-defined expression to each nonlocal...


;;; This is an ML-style function, which closes over all the free
;;; variables including globals.  Notice that this can be implemented
;;; without transformations, because the result does not depend on the
;;; context -- which is to say, does not depend on the bound variables
;;; *out* of the lambda.
(e1:define-macro (closure:ml-lambda formals . body-forms)
  (e1:let* ((body-as-expression (e1:macroexpand `(e1:begin ,@body-forms)))
            (nonlocals (set-as-list:subtraction (e0:free-variables body-as-expression)
                                                (set-as-list:list->set (sexpression:eject-symbols formals))))
            (body-as-sexpression (sexpression:inject-expression body-as-expression)))
    (sexpression:inject-expression
      (closure:make* nonlocals
                     (closure:variables* nonlocals)
                     (sexpression:eject-symbols formals)
                     (e1:macroexpand `(e1:begin ,@body-forms))))))

;; ;;; Make the feature easily accessible in the e1 namespace:
;; (e1:define-macro (e1:call-closure . stuff)
;;   `(closure:call ,@stuff))
(e1:define-macro (e1:closure . stuff)
  `(closure:make ,@stuff))


;;;;; Non-closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sometimes we want to use the "anonymous procedure" idea, without
;;; actually needing nonlocals.  The provided interface resembles
;;; closures, but we simply implement a non-closure as the symbol
;;; naming its associated procedure.

(e1:define-macro (e1:nonclosure formals . body-forms)
  (e1:let* ((procedure-name (sexpression:fresh-symbol)))
    ;; Define once and for all, at macroexpansion time, the procedure:
    (state:procedure-set! (sexpression:eject-symbol procedure-name)
                          (sexpression:eject-symbols formals)
                          (e1:macroexpand `(e1:begin ,@body-forms)))
    ;; Simply return its name, as a symbol literal:
    `(e0:value ,procedure-name)))

;;; Since we implement a non-closure as a procedure name, calling it
;;; is trivial:
(e1:define-macro (e1:call-nonclosure non-closure . actuals)
  `(e0:call-indirect ,non-closure ,@actuals))


;;;;; Local macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Our local macros share a serious flaw with Common Lips's: see the
;;; macrolet example in the Common Lisp HyperSpec:
;;; http://www.lispworks.com/documentation/HyperSpec/Body/s_flet_.htm#macrolet
;;; There are no closures for macros, and nonlocals are not visible at
;;; macroexpansion time.  Example in Common Lisp:
;;; (let ((n 1)) (macrolet ((m (a) `'(,a ,n))) (m 5))) fails because n isn't
;;; visible at macroexpansion time.
;;;
;;; Nonlocals in local macros would be useful, but they seem difficult
;;; to add to this system without completely changing it by threading
;;; local environments thru macroexpansion.  Interestingly, the
;;; problem is *not* relevant for local macros *within macro-generated
;;; code*, since in practice we manage visibility with nested
;;; quasiquoting anyway -- not beautiful, but it works.  I did that in
;;; e1:named-let.

;;; This macro is atypical as it does all the complex work at
;;; macroexpansion time, and then directly returns an injected
;;; expression.  Notice that the explicit call to e1:macroexpand must
;;; be performed when the temporary macro is visible.
(e1:define-macro (e1:let-macro-1 name-and-formals macro-body-sexpression . body-forms)
  (e1:let* ((name (sexpression:eject-symbol (sexpression:car name-and-formals)))
            (old-macro-body-or-zero (state:macro-get-body (e0:value name)))
            ;; Define the macro, and macroexpand the body form sequence into the result:
            (result-expression
             (e0:let () ;; this hack is more readable than destructuring-bind + transforms...
                     (repl:macroexpand-transform-and-execute `(e1:define-macro ,name-and-formals
                                                                               ,macro-body-sexpression))
                     (e1:macroexpand `(e1:begin ,@body-forms)))))
    ;; Restore the old definition, and return the result we were keeping aside:
    (state:macro-set! name old-macro-body-or-zero)
    (sexpression:inject-expression result-expression)))

;;; Generalization to any number of local macros.  Macros are not
;;; visible to one another.
(e1:define-macro (e1:let-macro bindings . body-forms)
  (e1:if (sexpression:null? bindings)
    `(e1:begin ,@body-forms)
    `(e1:let-macro-1 ,(sexpression:caar bindings)
                     (e1:begin ,@(sexpression:cdar bindings))
                     (e1:let-macro ,(sexpression:cdr bindings) ,@body-forms))))
;;; Example: (e1:let-macro (((s x) `(fixnum:* ,x ,x))) (s (s (s 2))))


;;;;; S-expression macro utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This comes in handy for many macros who have to work on s-lists,
;;; such as the ones encoding let bindings.
(e1:define (sexpression:map-nonclosure procedure-name s-list)
  (sexpression:reverse (sexpression:reversed-map-nonclosure-into procedure-name
                                                                 s-list
                                                                 sexpression:nil)))
(e1:define (sexpression:reversed-map-nonclosure-into procedure-name s-list acc)
  (e1:if (sexpression:null? s-list)
    acc
    (sexpression:reversed-map-nonclosure-into procedure-name
                                              (sexpression:cdr s-list)
                                              (sexpression:cons (e0:call-indirect procedure-name
                                                                                  (sexpression:car s-list))
                                                                acc))))
;;; Handy for macros needing to turn an s-list of encoded expressions
;;; into a single sequence:
(e1:define (sexpression:prepend-begin s-list)
  (sexpression:cons 'e1:begin
                     s-list))

;;; Handy for *building* binding s-lists and such:
(e1:define (sexpression:zip list-a list-b)
  (sexpression:reverse (sexpression:reversed-zip-into list-a list-b sexpression:nil)))
(e1:define (sexpression:reversed-zip-into list-a list-b acc)
  (e1:cond ((sexpression:null? list-a)
            (e1:if (sexpression:null? list-b)
                   acc
                   (e1:error "sexpression:zip-and-reverse: first argument os shorter")))
           ((sexpression:null? list-b)
            (e1:error "sexpression:zip-and-reverse: second argument os shorter"))
           (else
            (sexpression:reversed-zip-into (sexpression:cdr list-a)
                                           (sexpression:cdr list-b)
                                           (sexpression:cons (sexpression:list (sexpression:car list-a)
                                                                               (sexpression:car list-b))
                                                             acc)))))

;;;;; Loops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differently from Scheme and Common Lisp, our variables are
;;; non-mutable (even if they may be bound to mutable objects, such as
;;; boxes); hence our loop variables are no different from ordinary
;;; variables, and our loops are a little less flexible than Lisp's.

;;; This is currently the "most fundamental" kind of loop, since all
;;; the others rewrite into this.  The implementation is not terribly
;;; efficient, based as it is on closure self-application.  It should
;;; be much faster than a generic fix-point combinator, but the thing
;;; can probably be improved.  The loop name can't be directly
;;; accessed as a procedural object, but it's easy to do an
;;; eta-conversion and build an explicit lambda; for example:
;;; (e1:named-let loop ((n 1)) (display n) (e1:lambda (n) (loop n)))
(e1:define-macro (e1:named-let loop-name bindings . body-forms)
  (e1:let* ((next-name (sexpression:fresh-symbol))
            (auto-applicator-name (sexpression:fresh-symbol))
            (variable-names (sexpression:map-nonclosure (e0:value sexpression:car) bindings))
            (variable-initial-values (sexpression:map-nonclosure (e0:value sexpression:prepend-begin)
                                                                 (sexpression:map-nonclosure (e0:value sexpression:cdr)
                                                                                             bindings))))
    `(e0:let (,auto-applicator-name)
             (e1:lambda (,next-name ,@variable-names)
               ;; The local macro hides the fact that ,loop-name is a closure name,
               ;; and permits to call it as if it were a procedure:
               (e1:let-macro (((,loop-name ,@variable-names)
                               `(e1:call-closure ,',next-name ,',next-name ,,@variable-names)))
                 ,@body-forms))
       (e1:call-closure ,auto-applicator-name ,auto-applicator-name ,@variable-initial-values))))

;; ;;; Alternative version: this *does* require to call the "loop-name" as a closure:
;; (e1:define-macro (e1:named-let loop-name bindings . body-forms)
;;   (e1:let* ((next-name (sexpression:fresh-symbol))
;;             (auto-applicator-name (sexpression:fresh-symbol))
;;             (variable-names (sexpression:map-nonclosure (e0:value sexpression:car) bindings))
;;             (variable-initial-values (sexpression:map-nonclosure (e0:value sexpression:prepend-begin)
;;                                                                  (sexpression:map-nonclosure (e0:value sexpression:cdr)
;;                                                                                              bindings))))
;;     `(e0:let (,auto-applicator-name)
;;              (e1:lambda (,next-name ,@variable-names)
;;                (e1:let* ((,loop-name (e1:lambda ,variable-names
;;                                        (e1:call-closure ,next-name ,next-name ,@variable-names))))
;;                  ,@body-forms))
;;        (e1:call-closure ,auto-applicator-name ,auto-applicator-name ,@variable-initial-values))))

;;; In our do loop next-value expressions are mandatory
(e1:define-macro (e1:do bindings termination . body-forms)
  (e1:let* ((loop-name (sexpression:fresh-symbol))
            (auto-applicator-name (sexpression:fresh-symbol))
            (variables (sexpression:map-nonclosure (e0:value sexpression:car) bindings))
            (initial-values (sexpression:map-nonclosure (e0:value sexpression:cadr)
                                                        bindings))
            (new-values (sexpression:map-nonclosure (e0:value sexpression:prepend-begin)
                                                    (sexpression:map-nonclosure (e0:value sexpression:cddr)
                                                                                bindings)))
            (exit-condition (e1:if (sexpression:null? termination)
                                   (sexpression:inject-boolean #f)
                                   (sexpression:car termination)))
            (result (e1:if (e1:or (sexpression:null? termination)
                                  (sexpression:null? (sexpression:cdr termination)))
                           '(e0:bundle)
                           `(e1:begin ,@(sexpression:cdr termination)))))
    `(e1:named-let ,loop-name ,(sexpression:zip variables initial-values)
       (e1:if ,exit-condition
         ,result
         (e1:begin
           ,@body-forms
           (,loop-name ,@new-values))))))
;;; Example:
;; (e1:define (fact n)
;;   (e1:do ((result 1 (fixnum:* result i))
;;           (i n (fixnum:1- i)))
;;          ((fixnum:zero? i) result)))

;;; Trivial while loop
(e1:define-macro (e1:while condition . body-forms)
  `(e1:do ()
          ((e1:not ,condition))
     ,@body-forms))

;; ;;;;;;;;;;;;;;
;; ;;; FIXME: this is just for benchmarking; move away
;; (e1:define (test-r n)
;;   (e0:if-in n (0)
;;     0
;;     (test-r (fixnum:1- n))))
;; (e1:define (test-i initial)
;;   (e1:named-let loop ((n initial))
;;     (e0:if-in n (0)
;;       0
;;       (loop (fixnum:1- n)))))
;; ;; (gc) (benchmark (e1:toplevel (test-r 1000000)))
;; ;; (gc) (benchmark (e1:toplevel (test-i 1000000)))
;; ;;;;;;;;;;;;;;

;; ;;; Common Lisp-style dolist.  Differently from Common Lisp, we can
;; ;;; have more than one result forms.
;; (e1:define-macro (e1:dolist (variable list-expression . result-forms) . body-forms)
;;   (e1:let* ((rest-variable-name (sexpression:fresh-symbol)))
;;     `(e1:do ((,rest-variable-name ,list-expression (list:tail ,rest-variable-name)))
;;             ((list:null? ,rest-variable-name) ,@result-forms)
;;        (e1:let* ((,variable (list:head ,rest-variable-name)))
;;          ,@body-forms))))

;; ;;; Generalization of Common Lisp-style dolist.  Differently from
;; ;;; Common Lisp, we can have more than one result forms.
;; (e1:define-macro (e1:dostructure head-name tail-name null?-name (variable list-expression . result-forms) . body-forms)
;;   (e1:let* ((rest-variable-name (sexpression:fresh-symbol)))
;;     `(e1:do ((,rest-variable-name ,list-expression (,tail-name ,rest-variable-name)))
;;             ((,null?-name ,rest-variable-name) ,@result-forms)
;;        (e1:let* ((,variable (,head-name ,rest-variable-name)))
;;          ,@body-forms))))

;; ;;; Common Lisp-style dolist.
;; (e1:define-macro (e1:dolist (variable list-expression . result-forms) . body-forms)
;;   `(e1:dostructure list:head list:tail list:null?
;;                    (,variable ,list-expression ,@result-forms)
;;      ,@body-forms))

;;; Generalization of Common Lisp-style dolist.  Differently from
;;; Common Lisp, we can have more than one result forms.
(e1:define-macro (e1:doiterator make-iterator-name get-name next-name end?-name (variable structure-expression . result-forms) . body-forms)
  (e1:let* ((structure-variable-name (sexpression:fresh-symbol))
            (iterator-variable-name (sexpression:fresh-symbol)))
    `(e1:let* ((,structure-variable-name ,structure-expression))
       (e1:do ((,iterator-variable-name (,make-iterator-name ,structure-variable-name)
                                        (,next-name ,structure-variable-name ,iterator-variable-name)))
           ((,end?-name ,structure-variable-name ,iterator-variable-name) ,@result-forms)
         (e1:let* ((,variable (,get-name ,structure-variable-name ,iterator-variable-name)))
           ,@body-forms)))))

;;; Common Lisp-style dolist.
(e1:define-macro (list:iterator-get structure iterator)
  `(list:head ,iterator))
(e1:define-macro (list:iterator-next structure iterator)
  `(list:tail ,iterator))
(e1:define-macro (list:iterator-end? structure iterator)
  `(list:null? ,iterator))
(e1:define-macro (e1:dolist (variable list-expression . result-forms) . body-forms)
  `(e1:doiterator whatever:identity list:iterator-get list:iterator-next list:iterator-end?
                  (,variable ,list-expression ,@result-forms)
     ,@body-forms))

;;; dovector loop, in the style of Common Lisp's dolist:
(e1:define-macro (vector:iterator-make structure)
  '0)
(e1:define-macro (vector:iterator-next structure iterator)
  `(fixnum:1+ ,iterator))
(e1:define (vector:iterator-end? structure iterator)
  (fixnum:= (vector:length structure) iterator))
(e1:define-macro (e1:dovector (variable vector-expression . result-forms) . body-forms)
  `(e1:doiterator vector:iterator-make vector:get vector:iterator-next vector:iterator-end?
                  (,variable ,vector-expression ,@result-forms)
     ,@body-forms))

;;; Common Lisp-style dotimes.  Differently from Common Lisp, we can
;;; have more than one result forms.
(e1:define-macro (e1:dotimes (variable iteration-no . result-forms) . body-forms)
  (e1:let* ((limit-variable-name (sexpression:fresh-symbol)))
    `(e1:let* ((,limit-variable-name ,iteration-no))
       (e1:when (fixnum:> ,limit-variable-name 0)
         (e1:do ((,variable 0 (fixnum:1+ ,variable)))
                ((fixnum:= ,variable ,limit-variable-name) ,@result-forms)
           ,@body-forms)))))

;;; Pascal-style for loop, with optional step.  The syntax is my idea,
;;; and is not standard in Lisp.  Not terribly efficient: the thing
;;; works with both ascending and descending loops, but checks *two*
;;; bounds per iteration.
(e1:define-macro (e1:for1 (variable initial-value final-value step . result-forms) . body-forms)
  (e1:let* ((loop-name (sexpression:fresh-symbol))
            (initial-value-name (sexpression:fresh-symbol))
            (final-value-name (sexpression:fresh-symbol))
            (step-name (sexpression:fresh-symbol)))
    `(e1:let* ((,initial-value-name ,initial-value)
               (,final-value-name ,final-value)
               (,step-name ,step))
      (e1:named-let ,loop-name ((,variable ,initial-value-name))
        (e1:if (e1:or (fixnum:< ,variable ,initial-value-name)
                      (fixnum:> ,variable ,final-value-name))
               (e1:begin
                 ,@result-forms)
               (e1:begin
                 ,@body-forms
                 (,loop-name (fixnum:+ ,variable ,step-name))))))))

;;; Nested for loops: each loop is described by clause with the shape
;;; (variable initial-value final-value [step]), where step is 1 by
;;; default.  The last clause is the innermost.  With zero clauses,
;;; the body is exectuted once.
;;; No results when there is at least one loop.
;;; The syntax is my idea, and is not standard in Lisp.
(e1:define-macro (e1:for clauses . body-forms)
  (e1:if (sexpression:null? clauses)
    `(e1:begin ,@body-forms)
    `(e1:for1 (,@(for:clause->clause-with-step (sexpression:car clauses)))
       (e1:for ,(sexpression:cdr clauses) ,@body-forms))))
(e1:define (for:clause->clause-with-step clause)
  (e1:case (sexpression:length clause)
           ((0 1 2)
            (e1:error "e1:for: clause too short"))
           ((3) ;; step omitted: the default is one
            (sexpression:append clause '(1)))
           ((4) ;; the step is alredy there
            clause)
           (else
            (e1:error "e1:for: clause too long"))))

;;; Example: (e1:for ((i 1 3) (j 1 3)) (fixnum:print (fixnum:* i j)))
;;; (e1:toplevel (e1:for ((i 1 3) (j 1 3) (k 1 3)) (fixnum:print i) (fixnum:print j) (string:write "\n")))


;;;;; Scheme-style let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Armed with let* and named-let, we can finally define a generic
;;; Scheme-style let.

;;; According to the shape of the first subform, we can decide whether
;;; the let is a loop or a block:
(e1:define-macro (e1:let first-thing . more-things)
  (e1:if (sexpression:symbol? first-thing)
    `(e1:named-let ,first-thing ,@more-things)
    `(e1:non-named-let ,first-thing ,@more-things)))

;;; Easy: first bind fresh names to user definitions sequentially in
;;; a let*; then, within the let* body, bind user names to the
;;; variables we introduced before:
(e1:define-macro (e1:non-named-let bindings . body-forms)
  (e1:let* ((variables (sexpression:map-nonclosure (e0:value sexpression:car) bindings))
            (variable-no (sexpression:length variables))
            (definitions (sexpression:map-nonclosure (e0:value sexpression:prepend-begin)
                                                     (sexpression:map-nonclosure (e0:value sexpression:cdr)
                                                                                 bindings)))
            (fresh-variables (sexpression:fresh-symbols variable-no)))
    `(e1:let* (,@(sexpression:zip fresh-variables definitions))
       (e1:let* (,@(sexpression:zip variables fresh-variables))
         ,@body-forms))))


;;;;; A multi-way conditional including local bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sometimes I feel the need for a let or let* block spanning thru
;;; multiple cond condition and bodies; having a block out of the cond
;;; may not be an option because of efficiency, or preconditions to be
;;; tested.
;;; This new definition of e1:cond is a backwards-compatible extension.
;;;
;;; Example:
;;; (e1:cond ((list:null? list)
;;;           'something)
;;;          (bind (head (list:head list))
;;;                (tail (list:tail list)))
;;;          ((fixnum:even? head)
;;;           (something-even head tail))
;;;          (else
;;;           (something-odd head tail)))
;;;
;;; A bind* clause with the same syntax as bind is also available,
;;; generating sequential bindings in the style of let*.
(e1:define-macro (e1:cond . cond-items)
  (e1:if (sexpression:null? cond-items)
    '(e1:bundle)
    (e1:if (e1:not (sexpression:cons? (sexpression:car cond-items)))
      (e1:error "cond item is not a cons")
      (e1:let ((caar (sexpression:caar cond-items))
               (cdar (sexpression:cdar cond-items))
               (cdr (sexpression:cdr cond-items)))
        (e1:if (sexpression:eq? caar 'else)
          (e1:if (sexpression:null? cdr)
            `(e1:begin ,@cdar)
            (e1:error "else cond item not at the end"))
          (e1:if (sexpression:eq? caar 'bind)
            `(e1:let (,@cdar)
               (e1:cond ,@cdr))
            (e1:if (sexpression:eq? caar 'bind*)
              `(e1:let* (,@cdar)
                 (e1:cond ,@cdr))
              `(e1:if ,caar
                 (e1:begin ,@cdar)
                 (e1:cond ,@cdr)))))))))


;;;;; Futures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; By using closures we can implement friendly future expressions,
;;; which may containing any variable visible at future-creation time,
;;; including nonlocals.

;;; The only fork-procedure used for futures; the action performed in
;;; the background thread consists in calling the received closure
;;; with zero parameterss, and returning its result.
(e1:define (future:fork-procedure thread-name future-closure)
  (e1:call-closure future-closure))

;;; Build a future which will asynchronously call the given closure:
(e1:define (future:asynchronously-call-closure closure)
  (e0:fork future:fork-procedure closure))

;;; Friendly syntax: build a future which will asynchronously evaluate
;;; the given expression.
(e1:define-macro (e1:future . forms)
  `(future:asynchronously-call-closure (e1:lambda () ,@forms)))

;; (e1:define-macro (e1:future . forms)
;;   `(e0:fork future:fork-procedure (e1:lambda () ,@forms)))

;;; Essentially an alias of e0:join, except for the implicit begin:
(e1:define-macro (e1:join . forms)
  `(e0:join (e1:begin ,@forms)))


;;;;; Friendly syntax for unexec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This definition is interesting: it defines a global closure
;;; containing the given forms as body, so that the program main
;;; expression can simply apply it and have visibility over the
;;; variables visible by the unexec:unexec caller.
(e1:define-macro (unexec:unexec file-name . forms)
  (e1:let* ((closure-name (sexpression:fresh-symbol)))
    `(e1:begin
       (state:global-set! (e0:value ,closure-name)
                          (e1:lambda () ,@forms))
       (unexec:unexec-procedure ,file-name
                                (repl:macroexpand-and-transform
                                   '(e1:call-closure ,closure-name))))))
(e1:define-macro (unexec:quick-unexec . forms)
  `(unexec:unexec ,(sexpression:inject-string unexec:default-file)
     ,@forms))

;;; I consider unexec and exec as part of the language; they deserve
;;; handy aliases:
(e1:define-macro (e1:exec . stuff)
  `(unexec:exec ,@stuff))
(e1:define-macro (e1:unexec . stuff)
  `(unexec:unexec ,@stuff))


;;;;; Patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A pattern is an s-expression to be formed the following way:
;;; _                           ;; irrefutable
;;; X                           ;; capture variable: X is a symbol
;;; 'L                          ;; literal
;;; (SUM-CONSTRUCTOR P1 ... Pn) ;; sum type
;;; (tuple P1 ... Pn)           ;; tuple
;;; (and P1 ... Pn)             ;; conjunctive pattern: all must match
;;; (or P1 ... Pn)              ;; disjunctive pattern: at least one must match
;;; (when P E1 ... En)          ;; guarded pattern: all expressions must be true

;;; FIXME: add support for record patterns

(e1:define (pattern:irrefutable? pattern)
  (e1:and (sexpression:symbol? pattern)
          (whatever:eq? (e0:value _) ;; ?_)
                        (sexpression:eject-symbol pattern))))

(e1:define (pattern:variable? pattern)
  (e1:and (sexpression:symbol? pattern)
          (e1:not (pattern:irrefutable? pattern))))
(e1:define (pattern:variable-get-name pattern)
  (sexpression:eject-symbol pattern))

(e1:define (pattern:constant? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:= (sexpression:length pattern) 2)
          (sexpression:symbol? (sexpression:car pattern))
          (whatever:eq? (e0:value quote)
                        (sexpression:eject-symbol (sexpression:car pattern)))))
(e1:define (pattern:constant-get-value-as-sexpression pattern)
  (sexpression:cadr pattern))

(e1:define (pattern:sum? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:>= (sexpression:length pattern) 1)
          (sexpression:symbol? (sexpression:car pattern))
          (e1:not (e1:or (whatever:eq? (sexpression:eject (sexpression:car pattern)) (e0:value tuple))
                         (whatever:eq? (sexpression:eject (sexpression:car pattern)) (e0:value and))
                         (whatever:eq? (sexpression:eject (sexpression:car pattern)) (e0:value or))
                         (whatever:eq? (sexpression:eject (sexpression:car pattern)) (e0:value when))))))
(e1:define (pattern:sum-get-constructor pattern)
  (sexpression:eject-symbol (sexpression:car pattern)))
(e1:define (pattern:sum-get-subpatterns pattern)
  (sexpression:eject-list (sexpression:cdr pattern)))

(e1:define (pattern:tuple? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:>= (sexpression:length pattern) 1)
          (sexpression:symbol? (sexpression:car pattern))
          (whatever:eq? (e0:value tuple)
                        (sexpression:eject-symbol (sexpression:car pattern)))))
(e1:define (pattern:tuple-get-subpatterns pattern)
  (sexpression:eject-list (sexpression:cdr pattern)))

(e1:define (pattern:and? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:>= (sexpression:length pattern) 1)
          (sexpression:symbol? (sexpression:car pattern))
          (whatever:eq? (e0:value and)
                        (sexpression:eject-symbol (sexpression:car pattern)))))
(e1:define (pattern:and-get-subpatterns pattern)
  (sexpression:eject-list (sexpression:cdr pattern)))

(e1:define (pattern:or? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:>= (sexpression:length pattern) 1)
          (sexpression:symbol? (sexpression:car pattern))
          (whatever:eq? (e0:value or)
                        (sexpression:eject-symbol (sexpression:car pattern)))))
(e1:define (pattern:or-get-subpatterns pattern)
  (sexpression:eject-list (sexpression:cdr pattern)))

(e1:define (pattern:when? pattern)
  (e1:and (sexpression:list? pattern)
          (fixnum:>= (sexpression:length pattern) 2) ;; (when PATTERN . CONDITIONS)
          (sexpression:symbol? (sexpression:car pattern))
          (whatever:eq? (e0:value when)
                        (sexpression:eject-symbol (sexpression:car pattern)))))
(e1:define (pattern:when-get-subpattern pattern)
  (sexpression:cadr pattern))
(e1:define (pattern:when-get-conditions pattern)
  (sexpression:eject-list (sexpression:cddr pattern)))


;;;;; Pattern-matching checker and binder expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; In this section all procedure arguments and results are s-expressions.

;;; Return a boolean expression, encoded as an s-expression, which,
;;; when evaluated, returns #t iff the given discriminand variable
;;; matches the given pattern.
(e1:define (pattern-matching:check-expression*s discriminand-variable pattern)
  (e1:cond ((e1:or (pattern:irrefutable? pattern)
                   (pattern:variable? pattern))
            '#t)
           ((pattern:constant? pattern)
            `(whatever:eq? ,(pattern:constant-get-value-as-sexpression pattern)
                           ,discriminand-variable))
           ((pattern:tuple? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:tuple-get-subpatterns pattern)))
                      (element-no (sexpression:length subpatterns))
                      (subexpression-variables (sexpression:fresh-symbols element-no)))
              `(e0:let ,subexpression-variables
                       (tuple:explode ,discriminand-variable ,(sexpression:inject-fixnum element-no))
                       ,(pattern-matching:check-expressions*s subexpression-variables subpatterns))))
           ((pattern:sum? pattern)
            (e1:let* ((constructor (sexpression:inject-symbol (pattern:sum-get-constructor pattern)))
                      (subpatterns (sexpression:inject-sexpressions (pattern:sum-get-subpatterns pattern)))
                      (subexpression-variables (sexpression:fresh-symbols (sexpression:length subpatterns))))
              `(e1:and  (,(sexpression:append-symbols constructor (sexpression:inject-symbol (e0:value ?)))
                         ,discriminand-variable)
                        (e0:let ,subexpression-variables
                                (,(sexpression:append-symbols constructor
                                                              (sexpression:inject-symbol e1:word-separator)
                                                              (sexpression:inject-symbol (e0:value explode)))
                                 ,discriminand-variable)
                          ,(pattern-matching:check-expressions*s subexpression-variables subpatterns)))))
           ((pattern:and? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:and-get-subpatterns pattern)))
                      (repeated-discriminand (sexpression:n-times (sexpression:length subpatterns) discriminand-variable)))
              (pattern-matching:check-expressions*s repeated-discriminand subpatterns)))
           ((pattern:or? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:or-get-subpatterns pattern))))
             (e1:if (sexpression:null? subpatterns)
               '#f
               `(e1:or ,(pattern-matching:check-expression*s discriminand-variable (sexpression:car subpatterns))
                       ,(pattern-matching:check-expression*s discriminand-variable `(or ,@(sexpression:cdr subpatterns)))))))
           ((pattern:when? pattern)
            (e1:let* ((subpattern (pattern:when-get-subpattern pattern))
                      (conditions (sexpression:inject-sexpressions (pattern:when-get-conditions pattern))))
              ;; In order to evaluate the guards we have to bind capture variables:
              (pattern-matching:bind-expression*s discriminand-variable
                                                  subpattern
                                                  `(e1:and ,@conditions))))
           (else
             (e1:error "pattern-matching:check-expression*s: ill-formed pattern"))))
(e1:define (pattern-matching:check-expressions*s discriminand-variables patterns)
  (e1:cond ((e1:and (sexpression:null? discriminand-variables)
                    (sexpression:null? patterns))
            '#t)
           ((e1:or (sexpression:null? discriminand-variables)
                   (sexpression:null? patterns))
            (e1:error "pattern-matching:check-expression*s: different lengths"))
           (else
            `(e1:and ,(pattern-matching:check-expression*s (sexpression:car discriminand-variables)
                                                           (sexpression:car patterns))
                     ,(pattern-matching:check-expressions*s (sexpression:cdr discriminand-variables)
                                                            (sexpression:cdr patterns))))))

;;; Return an expression, encoded as an s-expression, which, when
;;; evaluated, evaluates the given body after binding the given
;;; pattern, provided that the pattern matches.
(e1:define (pattern-matching:bind-expression*s discriminand-variable pattern body-form)
  (e1:cond ((pattern:irrefutable? pattern)
            body-form)
           ((pattern:variable? pattern)
            `(e0:let (,(sexpression:inject-symbol (pattern:variable-get-name pattern)))
                     ,discriminand-variable
               ,body-form))
           ((pattern:constant? pattern)
            body-form)
           ((pattern:tuple? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:tuple-get-subpatterns pattern)))
                      (element-no (sexpression:length subpatterns))
                      (subexpression-variables (sexpression:fresh-symbols element-no)))
              `(e0:let ,subexpression-variables
                       (tuple:explode ,discriminand-variable ,(sexpression:inject-fixnum element-no))
                 ,(pattern-matching:bind-expressions*s subexpression-variables subpatterns body-form))))
           ((pattern:sum? pattern)
            (e1:let* ((constructor (sexpression:inject-symbol (pattern:sum-get-constructor pattern)))
                      (subpatterns (sexpression:inject-sexpressions (pattern:sum-get-subpatterns pattern)))
                      (subexpression-variables (sexpression:fresh-symbols (sexpression:length subpatterns))))
              `(e0:let ,subexpression-variables
                       (,(sexpression:append-symbols constructor
                                                     (sexpression:inject-symbol e1:word-separator)
                                                     (sexpression:inject-symbol (e0:value explode)))
                        ,discriminand-variable)
                 ,(pattern-matching:bind-expressions*s subexpression-variables subpatterns body-form))))
           ((pattern:and? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:and-get-subpatterns pattern)))
                      (repeated-discriminand (sexpression:n-times (sexpression:length subpatterns) discriminand-variable)))
              (pattern-matching:bind-expressions*s repeated-discriminand subpatterns body-form)))
           ((pattern:or? pattern)
            (e1:let* ((subpatterns (sexpression:inject-sexpressions (pattern:or-get-subpatterns pattern))))
              (e1:if (sexpression:null? subpatterns)
                '(e1:error "pattern-matching:bind-expression*s: the or pattern doesn't really match")
                `(e1:if ,(pattern-matching:check-expression*s discriminand-variable (sexpression:car subpatterns))
                   ,(pattern-matching:bind-expression*s discriminand-variable (sexpression:car subpatterns) body-form)
                   ,(pattern-matching:bind-expression*s discriminand-variable
                                                        `(or ,@(sexpression:cdr subpatterns))
                                                        body-form)))))
           ((pattern:when? pattern)
            (e1:let* ((subpattern (pattern:when-get-subpattern pattern)))
              ;; The conditions are true if we arrived here, and we can ignore them
              (pattern-matching:bind-expression*s discriminand-variable subpattern body-form)))
           (else
             (e1:error "pattern-matching:bind-expression*s: ill-formed pattern"))))
(e1:define (pattern-matching:bind-expressions*s discriminand-variables patterns body-form)
  (e1:cond ((e1:and (sexpression:null? discriminand-variables)
                    (sexpression:null? patterns))
            body-form)
           ((e1:or (sexpression:null? discriminand-variables)
                   (sexpression:null? patterns))
            (e1:error "pattern-matching:bind-expression*s: different lengths"))
           (else
            (pattern-matching:bind-expression*s (sexpression:car discriminand-variables)
                                                (sexpression:car patterns)
                                                (pattern-matching:bind-expressions*s (sexpression:cdr discriminand-variables)
                                                                                     (sexpression:cdr patterns)
                                                                                     body-form)))))

;;;;; Pattern-matching user syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ML-style multi-way match syntax.
(e1:define-macro (pattern-matching:match discriminand . cases)
  (e1:let* ((discriminand-variable (sexpression:fresh-symbol)))
    `(e0:let (,discriminand-variable)
             ,discriminand
       (pattern-matching:match-discriminand-variable ,discriminand-variable ,@cases))))

(e1:define-macro (pattern-matching:match-discriminand-variable discriminand-variable . cases)
  (e1:cond ((sexpression:null? cases)
            `(e1:error "pattern-matching:match: no match")) ;; FIXME: I should probably just return an empty bundle instead
           ((e1:and (sexpression:symbol? (sexpression:caar cases))
                    (whatever:eq? (e0:value else) (sexpression:eject (sexpression:caar cases))))
            (e1:if (sexpression:null? (sexpression:cdr cases))
              `(e1:begin ,@(sexpression:cdar cases))
              (e1:error "pattern-matching:match: else isn't the last case")))
           (else
            `(e1:if ,(pattern-matching:check-expression*s discriminand-variable
                                                          (sexpression:caar cases))
                    ,(pattern-matching:bind-expression*s discriminand-variable
                                                         (sexpression:caar cases)
                                                         `(e1:begin ,@(sexpression:cdar cases)))
                    (pattern-matching:match-discriminand-variable ,discriminand-variable
                                                                  ,@(sexpression:cdr cases))))))

;;; Handy alias:
(e1:define-macro (e1:match . stuff)
  `(pattern-matching:match ,@stuff))


;;;;; epsilon0 forms in epsilon1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We provide epsilon0 forms in the e1 namespace as well.

;;; These are mostly useful for beginners who don't yet distinguish
;;; epsilon0 from epsilon1, and also for catching some more mistakes:
;;; these macros don't silently ignore extra arguments, as the
;;; bootstrapping versions in e0 do.
;;; As a bonus, an implicit e1:begin is added whenever practical.

;;; A utility procedure which might come in handy elsewhere as well.
;;; I don't want to define this too early, using only epsilon0.
(e1:define (sexpression:symbol-list? sexpression)
  (e1:cond ((sexpression:null? sexpression)
            #t)
           ((e1:not (sexpression:cons? sexpression))
            #f)
           ((sexpression:symbol? (sexpression:car sexpression))
            (sexpression:symbol-list? (sexpression:cdr sexpression)))
           (else
            #f)))

(e1:define-macro (e1:variable symbol)
  (e1:unless (sexpression:symbol? symbol)
    (e1:error "e1:variable: not an s-symbol"))
  `(e0:variable ,symbol))

(e1:define-macro (e1:value value)
  `(e0:value ,value))

(e1:define-macro (e1:bundle . forms)
  `(e0:bundle ,@forms))

(e1:define-macro (e1:primitive primitive-name . actuals)
  (e1:unless (sexpression:symbol? primitive-name)
    (e1:error "e1:primitive: the primitive name isn't an s-symbol"))
  `(e0:primitive ,primitive-name ,@actuals))

;;; We can't name this e1:let.  FIXME: name e1:let and e1:let* support
;;; unbundling.
(e1:define-macro (e1:unbundle variables bound-form . body-forms)
  (e1:unless (sexpression:symbol-list? variables)
    (e1:error "e1:unbundle: variables should be a symbol s-list"))
  (e1:let ,variables ,bound-form (e1:begin ,@body-forms)))

(e1:define-macro (e1:call procedure-name . actuals)
  (e1:unless (sexpression:symbol? procedure-name)
    (e1:error "e1:call: the procedure name isn't an s-symbol"))
  `(e0:call ,procedure-name ,@actuals))

(e1:define-macro (e1:call-indirect procedure . actuals)
  `(e0:call-indirect ,procedure ,@actuals))

(e1:define-macro (e1:if-in discriminand values then-branch else-branch)
  (e1:unless (sexpression:list? values)
    (e1:error "e1:if-in: values should be an s-list"))
  `(e0:if-in ,discriminand ,values ,then-branch ,else-branch))

(e1:define-macro (e1:fork procedure-name . actuals)
  `(e0:fork ,procedure-name ,@actuals))

;;; We already defined e1:join, along with futures.


;;;;; Formatted output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; [FIXME: generalize into an extensible functionality]

(e1:define-macro (fio:write1-to-evaluating-file file s)
  (e1:cond ((sexpression:string? s)
            `(io:write-string ,file ,s))
           ((sexpression:symbol? s)
            `(io:write-symbol ,file (e0:value ,s)))
           ((sexpression:fixnum? s)
            `(io:write-fixnum ,file ,s))
           ((sexpression:boolean? s)
            `(io:write-boolean ,file ,s))
           ((sexpression:character? s)
            `(io:write-character ,file ,s))
           ((e1:and (sexpression:cons? s)
                    (sexpression:symbol? (sexpression:car s)))
            (e1:let* ((name (sexpression:eject-symbol (sexpression:car s)))
                      (args (sexpression:cdr s)))
              (e1:case name
                ((c)
                 `(io:write-character ,file ,@args))
                ((s st)
                 `(io:write-string ,file ,@args))
                ((sy)
                 `(io:write-symbol ,file ,@args))
                ((i)
                 `(io:write-fixnum ,file ,@args))
                ((b)
                 `(io:write-boolean ,file ,@args))
                (else
                 (e1:error "unknown format")))))
           (else
            (e1:error "unknown case"))))

(e1:define-macro (fio:write-to-evaluating-file file . stuff)
  (e1:if (sexpression:null? stuff)
    '(e1:begin)
    `(e1:begin
       (fio:write1-to-evaluating-file ,file ,(sexpression:car stuff))
       (fio:write-to-evaluating-file ,file ,@(sexpression:cdr stuff)))))

(e1:define-macro (fio:write-to file . stuff)
  (e1:let ((file-name (sexpression:fresh-symbol)))
    `(e1:let* ((,file-name ,file))
       (fio:write-to-evaluating-file ,file-name ,@stuff))))

(e1:define-macro (fio:write . stuff)
  `(fio:write-to (io:standard-output) ,@stuff))


;;;;; Ad-hoc polymorphic operations using boxedness tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: support cyclic structures as well, and keep this implementation
;;; as an optimized less-general version, available with another name.

;;; These operations inspect object shapes at runtime, hence can
;;; only work on runtimes representing boxedness tags.

(e1:define (whatever:equal? a b)
  (e1:let ((fixnum-a (boxedness:fixnum? a))
           (fixnum-b (boxedness:fixnum? b)))
    (e1:cond ((e1:and fixnum-a fixnum-b)
              (whatever:eq? a b))
             ((e1:and (e1:not fixnum-a) (e1:not fixnum-b))
              (whatever:buffer-equal? a b))
             (else
              #f))))

(e1:define (whatever:buffer-equal? pointer-1 pointer-2)
  (e1:let ((length-1 (boxedness:buffer-length pointer-1))
           (length-2 (boxedness:buffer-length pointer-2)))
    (e1:if (fixnum:= length-1 length-2)
      (whatever:buffer-equal-from-length? pointer-1 pointer-2 0 length-1)
      #f)))

;;; Always used on pointers to buffers of the same length
(e1:define (whatever:buffer-equal-from-length? pointer-1 pointer-2 from length)
  (e1:cond ((fixnum:= from length)
            #t)
           ((whatever:equal? (buffer:get pointer-1 from)
                             (buffer:get pointer-2 from))
            (whatever:buffer-equal-from-length? pointer-1 pointer-2 (fixnum:1+ from) length))
           (else
            #f)))


;;;;; List utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These aren't used in the bootstrap code, so I can afford to define
;;; them now in epsilon1, rather than near the beginning in epsilon0.

(e1:define (list:take list n)
  (list:reverse (list:take-reversed list n)))

(e1:define (list:take-reversed list n)
  (list:take-reversed-acc list n list:nil))
(e1:define (list:take-reversed-acc list n acc)
  (e1:if (fixnum:zero? n)
    acc
    (list:take-reversed-acc (list:tail list)
                            (fixnum:1- n)
                            (list:cons (list:head list) acc))))

(e1:define (list:drop list n)
  (e1:if (fixnum:zero? n)
    list
    (list:drop (list:tail list)
               (fixnum:1- n))))

;;; Fail horribly if the list is empty
(e1:define (list:last list)
  (list:head (list:last-cons list)))

;;; Fail horribly if the list is empty
(e1:define (list:last-cons list)
  (e1:let* ((tail (list:tail list)))
    (e1:if (list:null? tail)
      list
      (list:last-cons tail))))

;;; Return a copy of the list, which may share structure with the
;;; given list, having the element at the given index replaced with
;;; the given new element:
(e1:define (list:with list index element)
  (list:with-acc list index element list:nil))
(e1:define (list:with-acc list index element acc)
  (e1:if (fixnum:zero? index)
    (list:append-reversed acc (list:cons element (list:tail list)))
    (list:with-acc (list:tail list)
                   (fixnum:1- index)
                   element
                   (list:cons (list:head list) acc))))

;;; Return a copy of the given list with a new spine, but sharing all
;;; elements:
(e1:define (list:shallow-clone list)
  (list:reverse (list:reverse list)))


;;;;; Promises
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A promise is a two-element buffer.
;;; The first element contains:
;;; * 0 when the promise is not ready
;;; * 1 when it's ready;
;;; The second element contains:
;;; * a closure for the non-ready case
;;; * the result for the ready case.
;;;
;;; [FIXME: I'm not completely sure about this issue.  Maybe the
;;; Scheme workaround doesn't impact performance too much and I should
;;; adopt it as well] Differently from Scheme, we don't specify
;;; behavior of forcing a promise when this force causes another
;;; forcing of the same promise.

(e1:define (promise:ready? promise)
  (buffer:get promise 0))

(e1:define (promise:force promise)
  (e1:if (promise:ready? promise)
    (buffer:get promise 1)
    (e1:let ((result (e1:call-closure (buffer:get promise 1))))
      (buffer:set! promise 0 1)
      (buffer:set! promise 1 result)
      result)))

(e1:define-macro (promise:delay . forms)
  `(tuple:make 0
               (e1:lambda () ,@forms)))

;;; Promises are part of epsilon1: let's give operations convenient names:
(e1:define-macro (e1:delay . stuff)
  `(promise:delay ,@stuff))
(e1:define (e1:force promise)
  (promise:force promise))


;;;;; ML-style options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum option:option
  (none)
  (some content))


;;;;; Closure-based list operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NB: The order of arguments for proc follows that of stream-fold
;;; in SRFI-41, which is different from that of fold in SRFI-1.
;;; The SRFI1 order makes sense for fold-right (where it's more
;;; consistent with the catamorphism view of fold-right), but for
;;; fold-left the "accumulator, item" order is preferable IMHO. --jk
(e1:define (list:fold proc base list)
  (e1:if (list:null? list)
    base
    (list:fold proc
               (e1:call-closure proc base (list:head list))
               (list:tail list))))

(e1:define (list:map f xs)
  (e1:if (list:null? xs)
     list:nil
     (list:cons (e1:call-closure f (list:head xs))
                (list:map f (list:tail xs)))))


;;;;; Simple generic input ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-record input-port:port
  eof?-closure
  read-character-closure)

(e1:define (input-port:eof? p)
  (e1:call-closure (input-port:port-get-eof?-closure p)))

;;; This only works if the port is not on EOF
(e1:define (input-port:read-character p)
  (e1:call-closure (input-port:port-get-read-character-closure p)))

(e1:define (input-port:file->input-port f)
  (input-port:port (e1:lambda () (io:eof? f))
                   (e1:lambda () (io:read-character f))))

(e1:define (input-port:string->input-port s)
  (e1:let ((next-character-index (box:make 0))
           (length (string:length s)))
    (input-port:port (e1:lambda () (fixnum:= (box:get next-character-index)
                                             length))
                     (e1:lambda () (e1:let ((used-index (box:get next-character-index)))
                                     (box:set! next-character-index (fixnum:1+ used-index))
                                     (string:get s used-index))))))

;;;;; List element selectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(e1:define (list:first x)
  (list:head x))
(e1:define (list:second x)
  (list:head (list:tail x)))
(e1:define (list:third x)
  (list:head (list:tail (list:tail x))))
(e1:define (list:fourth x)
  (list:head (list:tail (list:tail (list:tail x)))))
(e1:define (list:fifth x)
  (list:head (list:tail (list:tail (list:tail (list:tail x))))))


;;;;; S-expression printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is different from (and less powerful than) the pretty-printer
;;; by Jérémie Koenig.  This should be a little faster, but is mostly
;;; interesting for its simplicity.

(e1:define (io:write-sexpression sexpression port)
  (printer:write-sexpression sexpression port))

(e1:define (sexpression:write sexpression)
  (printer:write-sexpression (io:standard-output) sexpression))

(e1:define (printer:write-sexpression port sexpression)
  (e1:let* ((tag (sexpression:get-tag sexpression))
            (printer-procedure-name (sexpression:type-tag->printer-procedure-name tag))
            (value (sexpression:eject sexpression)))
    (e1:if (whatever:zero? printer-procedure-name)
      (e1:begin
        (io:write-character port #\#)
        (io:write-character port #\<)
        (io:write-string port (sexpression:type-tag->name-as-string tag))
        (io:write-character port #\>))
      (e1:call-indirect printer-procedure-name
                        port
                        value))))

(e1:define (printer:write-empty-list port value)
  (io:write-character port #\()
  (io:write-character port #\)))

(e1:define (printer:write-boolean port value)
  (io:write-character port #\#)
  (io:write-character port (e1:if value
                             #\t
                             #\f)))

(e1:define (printer:write-fixnum port value)
  (io:write-fixnum port value))

(e1:define (printer:write-rest port value)
  (e1:cond ((sexpression:null? value)
            (io:write-character port #\)))
           ((sexpression:cons? value)
            (io:write-character port #\space)
            (printer:write-sexpression port (sexpression:car value))
            (printer:write-rest port (sexpression:cdr value)))
           (else
            (io:write-character port #\space)
            (io:write-character port #\.)
            (io:write-character port #\space)
            (printer:write-sexpression port value)
            (io:write-character port #\)))))

(e1:define (printer:write-cons port value)
  (e1:let ((left (cons:get-car value))
           (right (cons:get-cdr value)))
    (io:write-character port #\()
    (printer:write-sexpression port left)
    (printer:write-rest port right)))

(e1:define sexpression:character-escape-table   ;; character-to-string
  (unboxed-hash:make))
(e1:define sexpression:character-unescape-table ;; string-to-character
  (string-hash:make))

(e1:define sexpression:string-escape-table   ;; character->character
  (unboxed-hash:make))
(e1:define sexpression:string-unescape-table ;; character->character
  (unboxed-hash:make))

(e1:define (reader:unescape-string-literal quoted-escaped-string)
  (e1:let* ((length (fixnum:- (string:length quoted-escaped-string) 2))
            (limit (fixnum:1+ length))
            (result-to-cut (vector:make length))
            (used-character-no (reader:unescape-string-literal-helper result-to-cut quoted-escaped-string 0 1 limit)))
    (e1:if (fixnum:= used-character-no length)
      result-to-cut
      (e1:let ((result (vector:make used-character-no)))
        (vector:blit result 0 result-to-cut 0 used-character-no)
        (buffer:destroy result-to-cut)
        result))))
(e1:define (reader:unescape-string-literal-helper target source target-i source-i source-limit)
  (e1:cond ((fixnum:= source-i source-limit)
            target-i)
           ((whatever:eq? (string:get source source-i) #\\)
            (e1:cond ((fixnum:= (fixnum:1+ source-i) source-limit)
                      (e1:error "trailing #\\\\ in string literal")) ;; impossible if recognized by the regexp
                     ((unboxed-hash:has? sexpression:string-unescape-table
                                         (string:get source (fixnum:1+ source-i)))
                      (string:set! target
                                   target-i
                                   (unboxed-hash:get sexpression:string-unescape-table
                                                     (string:get source (fixnum:1+ source-i))))
                      (reader:unescape-string-literal-helper target
                                                             source
                                                             (fixnum:1+ target-i)
                                                             (fixnum:+ source-i 2)
                                                             source-limit))
                     (else
                      (e1:error "unknown string escape"))))
           (else
            (string:set! target target-i (string:get source source-i))
            (reader:unescape-string-literal-helper target source (fixnum:1+ target-i) (fixnum:1+ source-i) source-limit))))

(e1:define (sexpression:set-character-escape! character string)
  (unboxed-hash:set! sexpression:character-escape-table
                     character string)
  (string-hash:set! sexpression:character-unescape-table
                    string character)
  (item-list:add-before! ;; better than add-after!: misbehavior will be more evident
     reader:atom-item-list-box
     (e1:value unescaped-character)
     (symbol:string->symbol string)
     (reader:atom-case (regexp:sregexp->regexp (sexpression:inject-string
                                                   (string:append "#\\" string)))
                       (e1:lambda (_ locus)
                         (sexpression:make-with-locus sexpression:character-tag
                                                      character
                                                      locus)))))

(e1:define (sexpression:set-string-escape! character escape)
  (unboxed-hash:set! sexpression:string-escape-table
                     character escape)
  (unboxed-hash:set! sexpression:string-unescape-table
                     escape character))

(e1:define (printer:write-character port value)
  (io:write-character port #\#)
  (io:write-character port #\\)
  (e1:if (unboxed-hash:has? sexpression:character-escape-table
                            value)
    (io:write-string port (unboxed-hash:get sexpression:character-escape-table
                                            value))
    (io:write-character port value)))

(e1:define (printer:escaping-write-string port string from-index)
  (e1:if (fixnum:= from-index (string:length string))
    (e1:bundle)
    (e1:let ((c (string:get string from-index)))
      ;; FIXME: handle non-printable non-escaped characters in a different way.
      (e1:if (unboxed-hash:has? sexpression:string-escape-table c)
        (e1:let ((e (unboxed-hash:get sexpression:string-escape-table c)))
          (io:write-character port #\\)
          (io:write-character port e))
        (io:write-character port c)))
    (printer:escaping-write-string port string (fixnum:1+ from-index))))

;; This is useful to test wide characters: 中, 日本語.
(e1:define (printer:write-string port value)
  (io:write-character port #\")
  (printer:escaping-write-string port value 0)
  (io:write-character port #\"))

(e1:define (printer:write-symbol port value)
  ;; FIXME: shall I *also* unescape some other (Scheme-compatible) way?
  (printer:escaping-write-string port (symbol:symbol->string value) 0))

(e1:define (printer:write-fixed-point port value)
  (io:write-string port "#<fixed-point>")) ;; FIXME: implement

(e1:define (printer:write-expression port value)
  (io:write-string port "#<expression>")) ;; FIXME: implement
