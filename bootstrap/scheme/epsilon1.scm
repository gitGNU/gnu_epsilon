;;;;; This is -*- epsilon -*- (with some Scheme).
;;;;; Bootstrap driver

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2013, 2014, 2015 Luca Saiu
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
  (variadic:define-left-deep fixnum:arithmetic-right-shift fixnum:arithmetic-right-shift 0)
  (variadic:define-left-deep fixnum:logic-right-shift fixnum:logic-right-shift 0)

  (variadic:define-right-deep list:append list:append2 list:nil) ;; right-deep for performance

  ;;; Stack overflow if I do this.  I think the reason is the mutual
  ;;; dependency between quasiquoting and s-expression append.
  ;; ;; overwrite the old definition of sexpression:append: this generates faster code with >0 parameters
  ;; (variadic:define-right-deep sexpression:append sexpression:append2 sexpression:nil) ;; right-deep for performance

  ;; FIXME: with O(n^2) space consumption, this is *really* inefficient, and
  ;; can use up all memory on runtimes with no garbage collector.
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

;;; It's useful to convert between set-as-list's and (unboxed) hashes
;;; with unused data.

(e1:define (set-as-list:set-as-list->unboxed-hash sal)
  (e1:let ((res (unboxed-hash:make-given-bucket-no (list:length sal))))
    (set-as-list:set-as-list->unboxed-hash-helper sal res)
    res))
(e1:define (set-as-list:set-as-list->unboxed-hash-helper sal hash)
  (e1:unless (list:null? sal)
    (unboxed-hash:set! hash (list:head sal) #f)
    (set-as-list:set-as-list->unboxed-hash-helper (list:tail sal) hash)))

(e1:define (set-as-list:unboxed-hash->set-as-list h)
  (e1:let ((alist (unboxed-hash:unboxed-hash->alist h)))
    (set-as-list:unboxed-hash->set-as-list-helper alist list:nil)))
(e1:define (set-as-list:unboxed-hash->set-as-list-helper al acc)
  (e1:if (list:null? al)
    acc
    (e1:let ((new-acc (list:cons (cons:get-car (list:head al)) acc)))
      (set-as-list:unboxed-hash->set-as-list-helper (list:tail al) new-acc))))


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
            (e1:or (e1:and (fixnum:>= (string:length string) 1)
                           (whatever:eq? (string:get string 0) #\:))
                   (e1:and (fixnum:>= (string:length string) 2)
                           (whatever:eq? (string:get string 0) #\#)
                           (whatever:eq? (string:get string 1) #\:))))))

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
;; FIXME: make this more efficient.  I need substring and subvector operators
(e1:define (keyword:more-clumsy-keyword->symbol s)
  (symbol:string->symbol (vector:list->vector (list:tail (list:tail (vector:vector->list (symbol:symbol->string s)))))))
(e1:define (keyword:less-clumsy-keyword->symbol s)
  (symbol:string->symbol (vector:list->vector (list:tail (vector:vector->list (symbol:symbol->string s))))))
(e1:define (keyword:keyword->symbol s)
  (e1:case (string:get (symbol:symbol->string s) 0)
    ((#\#) (keyword:more-clumsy-keyword->symbol s))
    ((#\:) (keyword:less-clumsy-keyword->symbol s))
    (else (e1:error "not a keyword"))))

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

;;; An easy but inefficient solution: first bind fresh names to user
;;; definitions sequentially in a let*; then, within the let* body,
;;; bind user names to the variables we introduced before.
;;; FIXME: remove trivial lets.
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
(e1:define (macroexpand:expression-using-nonlocals forms-as-sexpression-list)
  (repl:macroexpand-and-transform `(e1:call-closure (e1:lambda () ,@forms-as-sexpression-list))))
(e1:define-macro (unexec:unexec-table file-name table . forms)
  `(unexec:unexec-table-procedure ,file-name
                                  ,table
                                  (macroexpand:expression-using-nonlocals ',forms)))
(e1:define-macro (unexec:quick-unexec-table table . forms)
  `(unexec:unexec-table ,table
                        ,(sexpression:inject-string unexec:default-file)
     ,@forms))

(e1:define-macro (unexec:quick-unexec . forms)
  `(unexec:quick-unexec-table symbol:table ,@forms))
(e1:define-macro (unexec:unexec file-name . forms)
  `(unexec:unexec-table ,file-name symbol:table ,@forms))

;;; I consider unexec and exec to be part of the language; they deserve
;;; handy aliases:
(e1:define-macro (e1:exec . stuff)
  `(unexec:exec ,@stuff))
(e1:define-macro (e1:unexec-table . stuff)
  `(unexec:unexec-table ,@stuff))
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

(e1:define-macro (fio:write1-to-evaluating-port port s)
  (e1:cond ((sexpression:string? s)
            `(io:write-string ,port ,s))
           ((sexpression:symbol? s)
            `(io:write-symbol ,port (e0:value ,s)))
           ((sexpression:fixnum? s)
            `(io:write-fixnum ,port ,s))
           ((sexpression:boolean? s)
            `(io:write-boolean ,port ,s))
           ((sexpression:character? s)
            `(io:write-character ,port ,s))
           ((e1:and (sexpression:cons? s)
                    (sexpression:symbol? (sexpression:car s)))
            (e1:let* ((name (sexpression:eject-symbol (sexpression:car s)))
                      (args (sexpression:cdr s)))
              (e1:case name
                ((c)
                 `(io:write-character ,port ,@args))
                ((C)
                 `(printer:write-character ,port ,@args))
                ((e)
                 `(printer:write-expression ,port ,@args))
                ((se)
                 `(printer:write-sexpression ,port ,@args))
                ((s st)
                 `(io:write-string ,port ,@args))
                ((S st)
                 `(printer:write-string ,port ,@args))
                ((sy)
                 `(io:write-symbol ,port ,@args))
                ((i)
                 `(io:write-fixnum ,port ,@args))
                ((f)
                 `(printer:write-fixed-point ,port ,@args))
                ((b)
                 `(io:write-boolean ,port ,@args))
                (else
                 (e1:error "unknown format")))))
           (else
            (e1:error "unknown case"))))

(e1:define-macro (fio:write-to-evaluating-port port . stuff)
  (e1:if (sexpression:null? stuff)
    '(e1:begin)
    `(e1:begin
       (fio:write1-to-evaluating-port ,port ,(sexpression:car stuff))
       (fio:write-to-evaluating-port ,port ,@(sexpression:cdr stuff)))))

(e1:define-macro (fio:write-to port . stuff)
  (e1:let ((port-name (sexpression:fresh-symbol)))
    `(e1:let* ((,port-name ,port))
       (fio:write-to-evaluating-port ,port-name ,@stuff))))

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


;;;;; Alist utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (alist:unbind-all-list alist keys)
  (e1:if (list:null? keys)
    alist
    (alist:unbind-all-list (alist:unbind-all alist (list:head keys))
                           (list:tail keys))))

(e1:define (alist:append low-priority high-priority)
  (list:append high-priority low-priority))


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

(e1:define (list:zip as bs)
  (e1:cond ((list:null? as)
            (e1:if (list:null? bs)
              list:nil
              (e1:error "second list too long")))
           ((list:null? bs)
            (e1:error "first list too long"))
           (else
            (list:cons (cons:make (list:head as)
                                  (list:head bs))
                       (list:zip (list:tail as)
                                 (list:tail bs))))))


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


;;;;; Higher-order closure-based list operations
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

(e1:define (list:exists? p xs)
  (e1:cond ((list:null? xs)
            #f)
           ((e1:call-closure p (list:head xs))
            #t)
           (else
            (list:exists? p (list:tail xs)))))

(e1:define (list:for-all? p xs)
  (e1:cond ((list:null? xs)
            #t)
           ((e1:call-closure p (list:head xs))
            (list:for-all? p (list:tail xs)))
           (else
            #f)))

(e1:define (list:filter p xs)
  (list:reverse (list:filter-reversed p xs)))
(e1:define (list:filter-reversed p xs)
  (list:filter-reversed-acc p xs list:nil))
(e1:define (list:filter-reversed-acc p xs acc)
  (e1:cond ((list:null? xs)
            acc)
           (bind (head (list:head xs))
                 (tail (list:tail xs)))
           ((e1:call-closure p head)
            (list:filter-reversed-acc p tail (list:cons head acc)))
           (else
            (list:filter-reversed-acc p tail acc))))

;;;;; The same, for s-expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (sexpression:reversed-map-closure-into closure s-list acc)
  (e1:if (sexpression:null? s-list)
    acc
    (sexpression:reversed-map-closure-into closure
                                           (sexpression:cdr s-list)
                                           (sexpression:cons (e1:call-closure
                                                                 closure
                                                                 (sexpression:car s-list))
                                                             acc))))

(e1:define (sexpression:map closure s-list)
  (sexpression:reverse (sexpression:reversed-map-closure-into closure
                                                              s-list
                                                              sexpression:nil)))


;;;;; Hash multiple bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These procedures allow to use a hash as a binding to a set-as-list, updating
;;; set-as-list elements.

(e1:define (unboxed-hash:ensure-non-empty! uh key)
  (e1:let ((old-set-as-list (e1:if (unboxed-hash:has? uh key)
                              (unboxed-hash:get uh key)
                              set-as-list:empty)))
    (unboxed-hash:set! uh key old-set-as-list)))

(e1:define (unboxed-hash:add-to-set-as-list! uh key value)
  (e1:let ((old-set-as-list (e1:if (unboxed-hash:has? uh key)
                              (unboxed-hash:get uh key)
                              set-as-list:empty)))
    (unboxed-hash:set! uh key (set-as-list:with old-set-as-list value))))

;;; FIXME: add other procedure when needed.


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


;;;;; Simple generic input ports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-record input-port:port
  eof?-closure
  read-character-closure)

(e1:define (input-port:eof? p)
  (e1:call-closure (input-port:port-get-eof?-closure p)))

;;; Return either a valid character or io:eof.
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
                                     (e1:if (fixnum:= used-index length)
                                       io:eof
                                       (e1:begin
                                         (box:set! next-character-index (fixnum:1+ used-index))
                                         (string:get s used-index))))))))

;;;;; Readline input port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make an input port reading from stdin with, readline line editing.
;;; FIXME: change the primitive to explicitly receive a prompt string;
;;; the readline C API supports this.
(e1:define (input-port:readline-input-port)
  (e1:let ((buffer-option (box:make (option:option-none)))
           (eof (box:make #f))
           (next-character-index (box:make 0)))
    (input-port:port (e1:lambda ()
                       (box:get eof))
                     (e1:lambda ()
                       (readline-input-port:get-character buffer-option eof next-character-index)))))
(e1:define (readline-input-port:get-character buffer-option-box eof-box next-character-index-box)
  (e1:if (box:get eof-box)
    io:eof
    (e1:match (box:get buffer-option-box)
      ((option:option-none)
       (box:set! buffer-option-box
                 (readline-input-port:get-chunk! eof-box))
       (readline-input-port:get-character buffer-option-box eof-box next-character-index-box))
      ((option:option-some string)
       (e1:if (fixnum:= (box:get next-character-index-box)
                        (string:length string))
         (e1:let ((bo (readline-input-port:get-chunk! eof-box)))
           (box:set! next-character-index-box 0)
           (box:set! buffer-option-box bo)
           (readline-input-port:get-character buffer-option-box eof-box next-character-index-box))
         (string:get string (box:get-and-bump! next-character-index-box)))))))

;; A chunk is the next buffer-option
(e1:define (readline-input-port:get-chunk! eof-box)
  (e1:let ((readline-result (io:readline)))
    (e1:if (fixnum:zero? readline-result)
      (e1:begin
        (box:set! eof-box #t)
        (option:option-none))
      (option:option-some readline-result))))


;;;;; Read the whole content of a file into a byte vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (io:file-content-as-byte-vector file-name)
  (e1:let* ((f (io:open-file file-name io:read-mode))
            (p (input-port:file->input-port f))
            (list (io:file-content-as-byte-vector-helper p list:nil)))
    (io:close-file f)
    (vector:list->vector (list:reverse list))))
(e1:define (io:file-content-as-byte-vector-helper p acc)
  (e1:let ((c (input-port:read-character p)))
    (e1:if (input-port:eof? p)
      acc
      (io:file-content-as-byte-vector-helper p (list:cons c acc)))))

(e1:define-macro (io:file-content-as-byte-vector-literal file-name)
  (sexpression:inject-string (io:file-content-as-byte-vector (sexpression:eject-string file-name))))


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


;;;;; S-expression printing (the printer subsystem)
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

;;; Take a string, return a string
(e1:define (reader:unescape-symbol-literal escaped-string)
  (reader:unescape-possibly-quoted-string-literal escaped-string #f))

(e1:define (st x) (reader:unescape-string-literal x))
(e1:define (sy x) (reader:unescape-symbol-literal x))

;;; Take a string with surrounding #\" characters, return a string
;;; without them.
(e1:define (reader:unescape-string-literal escaped-string)
  (reader:unescape-possibly-quoted-string-literal escaped-string #t))

(e1:define (reader:unescape-possibly-quoted-string-literal escaped-string
                                                           quoted) ;; surrounded by #\"?
  (e1:let* ((length (fixnum:- (string:length escaped-string)
                              (e1:if quoted 2 0)))
            (limit (fixnum:+ length (e1:if quoted 1 0)))
            (result-to-cut (vector:make length))
            (used-character-no (reader:unescape-string-literal-helper result-to-cut escaped-string 0 (e1:if quoted 1 0) limit)))
    (e1:if (fixnum:= used-character-no length)
      result-to-cut
      (e1:let ((result (vector:make used-character-no)))
        (vector:blit result 0 result-to-cut 0 used-character-no)
        (buffer:destroy result-to-cut)
        result))))
(e1:define (reader:unescape-string-literal-helper target source target-i source-i source-limit)
  (e1:cond ((fixnum:= source-i source-limit)
            target-i)
           (bind (source-c (string:get source source-i))
                 (source-next-i (fixnum:1+ source-i)))
           ((whatever:eq? source-c #\\)
            (e1:cond (bind (source-next-c (string:get source source-next-i)))
                     ((unboxed-hash:has? sexpression:string-unescape-table
                                         source-next-c)
                      (string:set! target
                                   target-i
                                   (unboxed-hash:get sexpression:string-unescape-table
                                                     source-next-c))
                      (reader:unescape-string-literal-helper target
                                                             source
                                                             (fixnum:1+ target-i)
                                                             (fixnum:+ source-i 2)
                                                             source-limit))
                     (else
                      (fio:write "About the escape character " (C source-next-c) "\n")
                      (e1:error "unknown string escape"))))
           (else
            (string:set! target target-i source-c)
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

;; This is useful to test wide characters: 中国、日本語。
(e1:define (printer:write-string port value)
  (io:write-character port #\")
  (printer:escaping-write-string port value 0)
  (io:write-character port #\"))

(e1:define (printer:write-symbol port value)
  ;; FIXME: shall I *also* unescape some other (Scheme-compatible) way?
  (printer:escaping-write-string port (symbol:symbol->string value) 0))


;;;;; Simple debugging support for procedures and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (debug:print-expression expression)
  (e1:let ((p (io:standard-output)))
    (printer:write-expression p expression)
    (io:write-string p "\n")))

(e1:define (debug:print-procedure-definition name)
  (e1:let ((p (io:standard-output)))
    (io:write-string p "Formals: ")
    (printer:write-symbols p (state:procedure-get-formals name))
    (io:write-string p "\nBody: ")
    (printer:write-expression p (state:procedure-get-body name))
    (io:write-string p "\n")))

(e1:define (debug:print-macro-definition name)
  (e1:let ((p (io:standard-output)))
    (printer:write-sexpression p  (state:macro-get-body name))
    (io:write-string p "\n")))

(e1:define (debug:print-macro-procedure-name macro-name)
  (e1:let ((p (io:standard-output)))
    (printer:write-symbol p (state:macro-get-macro-procedure-name macro-name))
    (io:write-string p "\n")))

(e1:define (debug:macroexpand sexpression)
  (e1:let ((p (io:standard-output)))
    (printer:write-expression p (e1:macroexpand sexpression))
    (io:write-string p "\n")))


;;;;; The EOF object as an s-expression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define sexpression:eof-tag
  (sexpression:define-base-type "eof"
                                0
                                (e0:value pp:eof)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))

(e1:define sexpression:eof
  (sexpression:make sexpression:eof-tag io:eof))
(e1:define (sexpression:eof)
  sexpression:eof)
(e1:define (sexpression:eof? s)
  (sexpression:has-tag? s sexpression:eof-tag))

;; Harmless aliases:
(e1:define sexpression:eof-object
  sexpression:eof)
(e1:define (sexpression:eof-object)
  (sexpression:eof))
(e1:define (sexpression:eof-object? s) ;; A harmless alias.
  (sexpression:eof? s))


;;;;; epsilon0 expression printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; epsilon0 expressions have not input syntax but they can be
;;; printed, which is very useful for debugging.
(e1:define (printer:write-expression port e)
  (e1:match e
    ((e0:expression-variable handle name)
     (io:write-symbol port name)
     (printer:write-handle port handle))
    ((e0:expression-value handle content)
     (e1:primitive io:write-value port content)
     (printer:write-handle port handle))
    ((e0:expression-bundle handle items)
     (io:write-string port "[bundle ")
     (printer:write-expressions port items)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-primitive handle name actuals)
     (io:write-string port "[primitive ")
     (printer:write-symbol port name)
     (io:write-string port " ")
     (printer:write-expressions port actuals)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-let handle bound-variables bound-expression body)
     (io:write-string port "[let [")
     (printer:write-symbols port bound-variables)
     (io:write-string port "] be ")
     (printer:write-expression port bound-expression)
     (io:write-string port " in ")
     (printer:write-expression port body)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-call handle procedure-name actuals)
     (io:write-string port "[call ")
     (printer:write-symbol port procedure-name)
     (io:write-string port " ")
     (printer:write-expressions port actuals)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-call-indirect handle procedure-expression actuals)
     (io:write-string port "[call-indirect ")
     (printer:write-expression port procedure-expression)
     (io:write-string port " ")
     (printer:write-expressions port actuals)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-if-in handle discriminand values then-branch else-branch)
     (io:write-string port "[if ")
     (printer:write-expression port discriminand)
     (io:write-string port " in {")
     (printer:write-values port values)
     (io:write-string port "} then ")
     (printer:write-expression port then-branch)
     (io:write-string port " else ")
     (printer:write-expression port else-branch)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-fork handle procedure-name actuals)
     (io:write-string port "[fork ")
     (printer:write-symbol port procedure-name)
     (io:write-string port " ")
     (printer:write-expressions port actuals)
     (io:write-string port "]")
     (printer:write-handle port handle))
    ((e0:expression-join handle future)
     (io:write-string port "[join ")
     (printer:write-expression port future)
     (io:write-string port "]")
     (printer:write-handle port handle))
    (else
     (io:write-string "#<non-epsilon0-expression>"))))

(e1:define (printer:write-handle port hh)
  (e1:unless (fixnum:zero? hh)
    (printer:write-handle port (fixnum:/ hh 10))
    (printer:write-handle-digit port (fixnum:% hh 10))))
(e1:define (printer:write-handle-digit port d)
  (io:write-character port (fixnum:+ d printer:subscript-0)))
(e1:define printer:subscript-0
  8320) ;; The character #\₀.  "#\₀" can't be parsed in Guile 1.8...

(e1:define (printer:write-expressions port ee)
  (e1:unless (list:null? ee)
    (printer:write-expression port (list:head ee))
    (e1:unless (list:null? (list:tail ee))
      (io:write-string port " "))
    (printer:write-expressions port (list:tail ee))))

(e1:define (printer:write-symbols port ss)
  (e1:unless (list:null? ss)
    (printer:write-symbol port (list:head ss))
    (e1:unless (list:null? (list:tail ss))
      (io:write-string port " "))
    (printer:write-symbols port (list:tail ss))))

(e1:define (printer:write-values port ss)
  (e1:unless (list:null? ss)
    (e1:primitive io:write-value port (list:head ss))
    (e1:unless (list:null? (list:tail ss))
      (io:write-string port " "))
    (printer:write-values port (list:tail ss))))


;;;;; Item-lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An item-list is a list of <key, value> pairs with unique keys
;;; where elements are kept in a user-controlled order.  The user may
;;; insert new elements at the beginning, at the end, and before or
;;; after existing elements.  Elements can also be deleted, by key.

;;; Update performance is particularly not critical: this structure
;;; must only be efficient to traverse.  A simple list is a fine
;;; implementation.

(e1:define item-list:nil
  list:nil)

(e1:define (item-list:add-first il key value)
  (list:cons (cons:make key value)
             (item-list:remove il key)))
(e1:define (item-list:add-last il key value)
  (list:append (item-list:remove il key)
               (list:list (cons:make key value))))
(e1:define (item-list:add-before il reference-key new-key new-value)
  (e1:cond ((list:null? il)
            (e1:error "not found"))
           ((whatever:eq? reference-key (cons:get-car (list:head il)))
            (list:cons (cons:make new-key new-value)
                       il))
           (else
            (list:cons (list:head il)
                       (item-list:add-before (list:tail il)
                                             reference-key
                                             new-key
                                             new-value)))))
(e1:define (item-list:add-after il reference-key new-key new-value)
  (e1:cond ((list:null? il)
            (e1:error "not found"))
           ((whatever:eq? reference-key (cons:get-car (list:head il)))
            (list:cons (list:head il)
                       (list:cons (cons:make new-key new-value)
                                  (list:tail il))))
           (else
            (list:cons (list:head il)
                       (item-list:add-after (list:tail il)
                                            reference-key
                                            new-key
                                            new-value)))))
(e1:define (item-list:remove il key)
  (e1:cond ((list:null? il)
            list:nil)
           ((whatever:eq? key (cons:get-car (list:head il)))
            (list:tail il))
           (else
            (list:cons (list:head il)
                       (item-list:remove (list:tail il)
                                         key)))))

;;; Destructive versions, working on an item list box:
(e1:define (item-list:add-first! bil key value)
  (box:set! bil (item-list:add-first (box:get bil) key value)))
(e1:define (item-list:add-last! bil key value)
  (box:set! bil (item-list:add-last (box:get bil) key value)))
(e1:define (item-list:add-before! bil reference-key new-key new-value)
  (box:set! bil (item-list:add-before (box:get bil) reference-key new-key new-value)))
(e1:define (item-list:add-after! bil reference-key new-key new-value)
  (box:set! bil (item-list:add-after (box:get bil) reference-key new-key new-value)))
(e1:define (item-list:remove! bil key)
  (box:set! bil (item-list:remove (box:get bil) key)))


;;;;; Locus support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum locus:locus
  (unknown)
  (known file-name-option
         start-row
         start-column
         end-row
         end-column
         description))

(e1:define (locus:locus->file-name-option locus)
  (e1:match locus
    ((locus:locus-unknown)
     (option:option-none))
    ((locus:locus-known file-name-option _ _ _ _ _)
     file-name-option)))

(e1:define (position:<= row-1 column-1 row-2 column-2)
  (e1:cond ((fixnum:< row-1 row-2)
            #t)
           ((fixnum:> row-1 row-2)
            #f)
           (else
            (fixnum:<= column-1 column-2))))

(e1:define (locus:join locus-1 locus-2)
  (e1:match locus-1
    ((locus:locus-unknown)
     (locus:locus-unknown))
    ((locus:locus-known file-name-option-1
                        start-row-1 start-column-1
                        end-row-1 end-column-1
                        description-1)
     (e1:match locus-2
       ((locus:locus-unknown)
        (locus:locus-unknown))
       ((locus:locus-known file-name-option-2
                           start-row-2 start-column-2
                           end-row-2 end-column-2
                           description-2)
        ;; FIXME: correctly merge file-name-option's
        ;; FIXME: shall we do something with descriptions?
        (e1:let ((start-1-prevails (position:<= start-row-1 start-column-1
                                                start-row-2 start-column-2))
                 (end-1-prevails (position:<= end-row-2 end-column-2
                                              end-row-1 end-column-1)))
          (locus:locus-known file-name-option-1
                             (e1:if start-1-prevails start-row-1 start-row-2)
                             (e1:if start-1-prevails start-column-1 start-column-2)
                             (e1:if end-1-prevails end-row-1 end-row-2)
                             (e1:if end-1-prevails end-column-1 end-column-2)
                             string:empty)))))))
;;; FIXME: write a locus-merging variadic macro.  The operation is
;;; associative.

;;; FIXME: write a locus->string procedure.


;;;;; Buffered backtrackable input port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A backtrackable input port provides the same operations as an
;;; input port (read-character and eof?); in addition to them, it also
;;; makes it possible to obtain the current "port state" and then
;;; later to backtrack to it, so that the same characters and eof
;;; state can be re-obtained, in the same order as the first time.
;;; The commit operation makes it impossible to backtrack to a state
;;; earlier than the current one, thus freeing resources.
;;; Backtracking to an earlier state destroys information about later
;;; states: it's not possible to "backtrack into the future".

;;; A backtrackable port provides locus information as well.

;;; FIXME: this extension over ports is hard to reuse and generalize.
;;; I'm simulating a poor man's inheritance system, but not well
;;; enough.  I should probably redo this after thinking a little more
;;; about the general problem.

(e1:define-record backtrackable-port:backtrackable-port
  input-port
  uncommitted-characters
  buffered-characters
  file-name-option
  row
  column
  backtrackable-states)

(e1:define (backtrackable-port:input-port->backtrackable-port p file-name-option)
  (backtrackable-port:backtrackable-port p
                                         list:nil
                                         list:nil
                                         file-name-option
                                         1 0
                                         list:nil))

(e1:define-record backtrackable-port:state
  uncommitted-characters
  buffered-characters
  row
  column)

(e1:define (backtrackable-port:backtrackable-port->state bp)
  (e1:let* ((uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp))
            (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp))
            (states (backtrackable-port:backtrackable-port-get-backtrackable-states bp))
            (row (backtrackable-port:backtrackable-port-get-row bp))
            (column (backtrackable-port:backtrackable-port-get-column bp))
            (new-state (backtrackable-port:state uncommitteds
                                                 buffereds
                                                 row
                                                 column)))
    (backtrackable-port:backtrackable-port-set-backtrackable-states!
       bp
       (list:cons new-state states))
    new-state))

;;; Commit the current state, releasing resources; it won't be
;;; possible to backtrack to any previous state.
(e1:define (backtrackable-port:commit! bp)
  (backtrackable-port:backtrackable-port-set-uncommitted-characters! bp list:nil)
  (backtrackable-port:backtrackable-port-set-backtrackable-states! bp list:nil))

;;; Backtrack to a previous state.  Notice that the state is compared by identity.
;;; FIXME: it should be possible to relax this assumption, to allow backtracks into
;;; the future and even to make the thing slightly more efficient by simply not
;;; storing a state stack in the port.  I'll try, but this needs careful testing.
(e1:define (backtrackable-port:backtrack! bp state)
  (e1:let* ((states (backtrackable-port:backtrackable-port-get-backtrackable-states bp))
            (first-state (list:head states)))
    (e1:if (whatever:eq? first-state state)
      ;; Restore the state:
      (e1:let ((uncommitteds (backtrackable-port:state-get-uncommitted-characters state))
               (buffereds (backtrackable-port:state-get-buffered-characters state))
               (row (backtrackable-port:state-get-row state))
               (column (backtrackable-port:state-get-column state))
               (current-uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp))
               (current-buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp)))
        (backtrackable-port:backtrackable-port-set-row! bp row)
        (backtrackable-port:backtrackable-port-set-column! bp column)
        (backtrackable-port:backtrackable-port-set-uncommitted-characters! bp uncommitteds)
        (backtrackable-port:backtrackable-port-set-buffered-characters!
            bp
            (list:append (list:drop (list:reverse current-uncommitteds)
                                    (list:length uncommitteds))
                         current-buffereds)))
      (e1:let ((other-states (list:tail states)))
        ;; Pop the most recent state and try again:
        (backtrackable-port:backtrackable-port-set-backtrackable-states!
            bp
            other-states)
        (backtrackable-port:backtrack! bp state)))))

(e1:define (backtrackable-port:eof? bp)
  (e1:let ((p (backtrackable-port:backtrackable-port-get-input-port bp))
           (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp)))
    (e1:and (list:null? buffereds)
            (input-port:eof? p))))

(e1:define-sum backtrackable-port:character-class
  (ordinary)
  (newline)
  (nothing))

(e1:define (backtrackable-port:classify-characater character)
  (e1:case character
    ((#\newline)
     (backtrackable-port:character-class-newline))
    ((#\cr)
     (backtrackable-port:character-class-nothing))
    (else
     (backtrackable-port:character-class-ordinary))))

(e1:define (backtrackable-port:backtrackable-update-locus! bp c)
  (e1:let ((row (backtrackable-port:backtrackable-port-get-row bp))
           (column (backtrackable-port:backtrackable-port-get-column bp))
           (newline (fixnum:= c #\newline)))
    (e1:match (backtrackable-port:classify-characater c)
      ((backtrackable-port:character-class-nothing)) ;; do nothing
      ((backtrackable-port:character-class-newline)
        (backtrackable-port:backtrackable-port-set-row! bp (fixnum:1+ row))
        (backtrackable-port:backtrackable-port-set-column! bp 0))
      ((backtrackable-port:character-class-ordinary)
       (backtrackable-port:backtrackable-port-set-column! bp (fixnum:1+ column))))))

(e1:define (backtrackable-port:read-character bp)
  (e1:if (backtrackable-port:eof? bp)
    io:eof
    (backtrackable-port:read-non-eof-character bp)))

(e1:define (backtrackable-port:read-non-eof-character bp)
  (e1:let ((p (backtrackable-port:backtrackable-port-get-input-port bp))
           (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp))
           (uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp)))
    (e1:if (list:null? buffereds)
      (e1:let ((result (input-port:read-character p)))
        (backtrackable-port:backtrackable-update-locus! bp result)
        (backtrackable-port:backtrackable-port-set-uncommitted-characters!
           bp
           (list:cons result uncommitteds))
        result)
      (e1:let ((result (list:head buffereds)))
        (backtrackable-port:backtrackable-update-locus! bp result)
        (backtrackable-port:backtrackable-port-set-buffered-characters!
           bp
           (list:tail buffereds))
        (backtrackable-port:backtrackable-port-set-uncommitted-characters!
           bp
           (list:cons result uncommitteds))
        result))))


;;;;; Range sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A "range set" is just a list of disjoint character ranges, both
;;; ends included, sorted.  Here two intervals being disjoint means
;;; that they do not even have one end in common: there must be a
;;; distance of at least one character.
;;; A range set is represented as a sorted list of pairs, each pair
;;; containing the first and last character of a range.

(e1:define range-set:minimum-character
  0) ;; 0 is actually a valid code point
(e1:define range-set:maximum-character
  1114111) ;; or #x10FFFF: the last valid Unicode code point

(e1:define range-set:empty
  list:nil)
(e1:define range-set:universe
  (list:list (cons:make range-set:minimum-character
                        range-set:maximum-character)))

(e1:define (range-set:singleton character)
  (list:list (cons:make character character)))

;;; A user-friendly way of describing a range set, to be automatically
;;; rewritten into the efficient version
(e1:define-sum range-set:sugared
  (empty)
  (universe)
  (character character)
  (range from to)
  (union first second)
  (intersection first second)
  (subtraction first second)
  (complement sugared))

(e1:define (range-set:sugared->range-set sugared)
  (e1:match sugared
    ((range-set:sugared-empty)
     range-set:empty)
    ((range-set:sugared-universe)
     range-set:universe)
    ((range-set:sugared-character c)
     (list:list (cons:make c c)))
    ((range-set:sugared-range from to)
     (list:list (cons:make from to)))
    ((range-set:sugared-union first second)
     (range-set:union (range-set:sugared->range-set first)
                      (range-set:sugared->range-set second)))
    ((range-set:sugared-intersection first second)
     (range-set:intersection (range-set:sugared->range-set first)
                             (range-set:sugared->range-set second)))
    ((range-set:sugared-subtraction first second)
     (range-set:subtraction (range-set:sugared->range-set first)
                            (range-set:sugared->range-set second)))
    ((range-set:sugared-complement sugared)
     (range-set:complement (range-set:sugared->range-set sugared)))))

;;; Return #t iff the first range completely prececes the second, and it's not
;;; joinable to it
(e1:define (range-set:range-< first-range second-range)
  (e1:let ((first-end (cons:cdr first-range))
           (second-beginning (cons:car second-range)))
    (fixnum:< (fixnum:1+ first-end) second-beginning)))

(e1:define (range-set:joinable-ranges? first-range second-range)
  (e1:not (e1:or (range-set:range-< first-range second-range)
                 (range-set:range-< second-range first-range))))

;;; This assumes the ranges are joinable
(e1:define (range-set:join-ranges first-range second-range)
  (e1:let ((first-beginning (cons:car first-range))
           (first-end (cons:cdr first-range))
           (second-beginning (cons:car second-range))
           (second-end (cons:cdr second-range)))
    (cons:make (fixnum:min first-beginning second-beginning)
               (fixnum:max first-end second-end))))

(e1:define (range-set:complement range-set)
  (range-set:complement-helper range-set:minimum-character range-set))
(e1:define (range-set:complement-helper first-to-consider range-set)
  (e1:match range-set
    ((list:list-nil)
     (e1:if (fixnum:<= first-to-consider range-set:maximum-character)
       (list:list (cons:make first-to-consider range-set:maximum-character))
       list:nil))
    ((list:list-cons (tuple first-beginning first-end) more)
     (e1:if (fixnum:< first-to-consider first-beginning)
       (list:cons (cons:make first-to-consider (fixnum:1- first-beginning))
                  (range-set:complement-helper (fixnum:1+ first-end) more))
       (range-set:complement-helper (fixnum:1+ first-end) more)))))
(e1:define (range-set:union first second)
  (e1:match (tuple:make first second)
    ((tuple (list:list-nil) _)
     second)
    ((tuple _ (list:list-nil))
     first)
    ((tuple (list:list-cons first-range first-rest)
            (list:list-cons second-range second-rest))
     (e1:cond ((range-set:joinable-ranges? first-range second-range)
               (range-set:union (list:cons (range-set:join-ranges first-range
                                                                  second-range)
                                           first-rest)
                                second-rest))
              ((range-set:range-< first-range second-range)
               (list:cons first-range
                          (range-set:union first-rest second)))
              (else
               (list:cons second-range
                          (range-set:union first second-rest)))))))

(e1:define (range-set:intersection first second)
  (range-set:complement (range-set:union (range-set:complement first)
                                         (range-set:complement second))))
(e1:define (range-set:subtraction first second)
  (range-set:complement (range-set:union (range-set:complement first)
                                         second)))

;;; FIXME: given a range set, generate a more efficient testing
;;; procedure.  I can do even better than "partial evaluation by
;;; hand", exploiting the ordering invariants to make a balanced
;;; comparison tree.
(e1:define (range-set:has? range-set character)
  (e1:match range-set
    ((list:list-nil)
     #f)
    ((list:list-cons (tuple first-beginning first-end) rest)
     (e1:or (e1:and (fixnum:<= first-beginning character)
                    (fixnum:<= character first-end))
            (range-set:has? rest character)))))


;;;;; S-range-sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A comfortable way of expressing rangesets is by an s-expression
;;; encoding.  Here we provide a facility to translate the
;;; s-expression representation (similar to sugared range sets with
;;; trivial variadic extensions).

;;; Let C, C1, C2... be characters and R1, R2... be s-range-sets.
;;; Then a s-range-set may be any of the following s-expressions:
;;; * empty
;;; * universe
;;; * C
;;; * (range C1 C2)
;;; * (union R1 ... Rn) or (\| R1 ... Rn)      [n >= 0]
;;; * (intersection R1 ... Rn)                 [n >= 0]
;;; * (subtraction R1 ... Rn) or (- R1 ... Rn) [n >= 1; R1 minus the others]
;;; * (complement R1 ... Rn)                   [n >= 0; complement of union]

(e1:define (range-set:srange-set->range-set s)
  (e1:let ((sugared (range-set:srange-set->sugared-range-set s)))
    (range-set:sugared->range-set sugared)))

(e1:define (range-set:srange-set->sugared-range-set s)
  (e1:cond ((sexpression:symbol? s)
            (e1:let ((name (sexpression:eject s)))
              (e1:cond ((whatever:eq? name (e1:value empty))
                        (range-set:sugared-empty))
                       ((whatever:eq? name (e1:value universe))
                        (range-set:sugared-universe))
                       (else
                        (e1:error "unknown range-set symbol")))))
           ((sexpression:character? s)
            (range-set:sugared-character (sexpression:eject s)))
           ((e1:and (sexpression:list? s)
                    (sexpression:symbol? (sexpression:car s)))
            (range-set:complex-srange-set->sugared-range-set (sexpression:eject (sexpression:car s))
                                                             (sexpression:eject-list (sexpression:cdr s))))
           (else
            (e1:error "unknown range-set case"))))

(e1:define (range-set:complex-srange-set->sugared-range-set symbol args)
  (e1:case symbol
    ((range)
     (e1:if (fixnum:= (list:length args) 2)
       (range-set:sugared-range (sexpression:eject-character (list:first args))
                                (sexpression:eject-character (list:second args)))
       (e1:error "non-binary range-set range")))
    ((union \|) ;; FIXME: remove this useless escaping after switching to my parser (possibly)
     (e1:if (list:null? args)
       (range-set:sugared-empty)
       (range-set:sugared-union (range-set:srange-set->sugared-range-set (list:head args))
                                (range-set:complex-srange-set->sugared-range-set symbol
                                                                                 (list:tail args)))))
    ((intersection)
     (e1:if (list:null? args)
       (range-set:sugared-universe)
       (range-set:sugared-intersection (range-set:srange-set->sugared-range-set (list:head args))
                                       (range-set:complex-srange-set->sugared-range-set symbol
                                                                                        (list:tail args)))))
    ((subtraction -)
     (e1:if (list:null? args)
       (e1:error "nullary subtraction")
       (range-set:sugared-subtraction (range-set:srange-set->sugared-range-set (list:head args))
                                      (range-set:complex-srange-set->sugared-range-set (e1:value union)
                                                                                       (list:tail args)))))
    ((complement)
     (range-set:sugared-complement (range-set:complex-srange-set->sugared-range-set (e1:value union)
                                                                                    args)))
    (else
     (e1:error "unknown symbol"))))


;;;;; Regular expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum regexp:regexp
  (empty) ;; very different from an empty rangeset! [FIXME: shall I eliminate the empty rangeset to remove ambiguity?  Do I need to add a "fail" case? [probably not the empty rangeset, should have the same effect]]
  (range-set range-set) ;; of course a *non-sugared* range set
  (sequence first second)
  (or first second)
  (plus regexp)) ;; I take + instead of * as primitive

;;; A sugared version, to be presented to the user:
(e1:define-sum regexp:sugared
  (empty)
  (range-set range-set) ;; a *non-sugared* range set
  (string string)
  (sequence regexps)
  (or regexps)
  (star regexp)
  (optional regexp)
  (plus regexp)
  (variable name)) ;; names map into *non-sugared* regexps

(e1:define (regexp:desugar s environment)
  (e1:match s
    ((regexp:sugared-empty)
     (regexp:regexp-empty))
    ((regexp:sugared-range-set range-set)
     (regexp:regexp-range-set range-set)) ; already desugared
    ((regexp:sugared-string string)
     (regexp:desugar-string string))
    ((regexp:sugared-sequence regexps)
     (regexp:desugar-sequence regexps environment))
    ((regexp:sugared-or regexps)
     (regexp:desugar-or regexps environment))
    ((regexp:sugared-star regexp)
     (regexp:regexp-or (regexp:regexp-plus (regexp:desugar regexp environment))
                       (regexp:regexp-empty)))
    ((regexp:sugared-optional regexp)
     (regexp:regexp-or (regexp:desugar regexp environment)
                       (regexp:regexp-empty)))
    ((regexp:sugared-plus regexp)
     (regexp:regexp-plus (regexp:desugar regexp environment)))
    ((regexp:sugared-variable name)
     (alist:lookup environment name))))

(e1:define (regexp:desugar-sequence ss environment)
  (e1:cond ((list:null? ss)
            (regexp:regexp-empty))
           ((list:null? (list:tail ss))
            (regexp:desugar (list:head ss) environment))
           (else
            (regexp:regexp-sequence (regexp:desugar (list:head ss) environment)
                                    (regexp:desugar-sequence (list:tail ss) environment)))))

;;; FIXME: it is possible to optimize a union of two range-set regexps
;;; into a single range-set regexp.  I don't know if it's worth the
;;; trouble.
(e1:define (regexp:desugar-or ss environment)
  (e1:cond ((list:null? ss)
            (e1:error "regexp:desugar-or: empty list"))
           ((list:null? (list:tail ss))
            (regexp:desugar (list:head ss) environment))
           (else
            (regexp:regexp-or (regexp:desugar (list:head ss) environment)
                              (regexp:desugar-or (list:tail ss) environment)))))

(e1:define (regexp:desugar-string string)
  (regexp:desugar-string-index string 0 (fixnum:1- (string:length string))))
(e1:define (regexp:desugar-string-index string index last-index)
  (e1:if (fixnum:> index last-index)
    (regexp:regexp-empty)
    (e1:let* ((c (string:get string index))
              (first-regexp (regexp:regexp-range-set (list:list (cons:make c c)))))
      (e1:if (fixnum:= index last-index)
        first-regexp
        (regexp:regexp-sequence first-regexp
                                (regexp:desugar-string-index string (fixnum:1+ index) last-index))))))

;;; The most convenient way for a user to enter a regular expression
;;; is by an s-expression syntax encoding the sugared version.  We call
;;; "s-regexp" such an s-expression.
;;; An s-regexp may be:
;;; * an s-range-set, encoding a range-set
;;; * an s-string, encoding a sequence of single-character range sets;
;;; * an s-list of s-regexps, encoding a (possibly empty) sequence;
;;; * an s-list of the s-symbol | and one or more s-regexps, encoding an or;
;;; * an s-list of the s-symbol * and an s-regexp, encoding a star;
;;; * an s-list of the s-symbol ? and an s-regexp, encoding an option;
;;; * an s-list of the s-symbol + and an s-regexp, encoding a plus;
;;; * an s-symbol different from empty and universe, encoding a variable.
(e1:define (regexp:sregexp->sugared s)
  (e1:cond ((sexpression:null? s)
            (regexp:sugared-empty))
           ((sexpression:string? s)
            (regexp:sugared-string (sexpression:eject s)))
           ((sexpression:symbol? s)
            (e1:let ((name (sexpression:eject s)))
              (e1:case name
                ((universe) ;; not empty!  That has a different implementation (empty regexp: success -- empty rangeset: failure)
                 (regexp:sugared-range-set (range-set:srange-set->range-set s)))
                ((empty)
                 (regexp:sugared-empty))
                (else
                 (regexp:sugared-variable (sexpression:eject s))))))
           ((sexpression:cons? s)
            (e1:let* ((car (sexpression:car s))
                      (cdr (sexpression:cdr s))
                      (car-value (sexpression:eject car)))
              (e1:if (sexpression:symbol? car)
                (e1:case car-value
                  ((\|)
                   (regexp:sugared-or (regexp:sregexps->sugareds cdr)))
                  ((*)
                   (regexp:sugared-star (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((?)
                   (regexp:sugared-optional (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((+)
                   (regexp:sugared-plus (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((range union intersection subtraction - complement)
                   (regexp:sugared-range-set (range-set:srange-set->range-set s)))
                  (else
                   (regexp:sugared-sequence (regexp:sregexps->sugareds s))))
                ;; An s-cons not starting with an s-symbol:
                (regexp:sugared-sequence (regexp:sregexps->sugareds s)))))
           (else ;; assume it's an s-range-set
            (regexp:sugared-range-set (range-set:srange-set->range-set s)))))

(e1:define (regexp:sregexps->sugareds slist)
  (e1:if (sexpression:null? slist)
    list:nil
    (list:cons (regexp:sregexp->sugared (sexpression:car slist))
               (regexp:sregexps->sugareds (sexpression:cdr slist)))))

(e1:define (regexp:sregexp->regexp-in s environment)
  (e1:let ((sugared (regexp:sregexp->sugared s)))
    (regexp:desugar sugared environment)))

;;; A regexp maching anything: [FIXME: remove if unused]
(e1:define regexp:anything
  (regexp:sregexp->regexp-in '(* universe)
                             alist:nil))


;;;;; A convenient way of updating a regexp global table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define regexp:global-environment
  (box:make alist:nil))

(e1:define (regexp:define-sregexp! name sregexp)
  (e1:let* ((old-alist (box:get regexp:global-environment))
            (regexp (regexp:sregexp->regexp-in sregexp old-alist)))
    (box:set! regexp:global-environment
              (alist:bind-unique old-alist name regexp))))

;;; Handy syntax.
;;; Example: (e1:define-regexp digit (range #\0 #\9))
(e1:define-macro (e1:define-regexp sname sregexp)
  (e1:unless (sexpression:symbol? sname)
    (e1:error "non-symbol name"))
  `(regexp:define-sregexp! (e1:value ,sname)
                           ',sregexp))

;;; Turn an s-regexp into a regexp using the current global
;;; environment:
(e1:define (regexp:sregexp->regexp s)
  (regexp:sregexp->regexp-in s (box:get regexp:global-environment)))


;;;;; Sample regexps [mostly for testing [FIXME: really?]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-regexp sexpression:letter
  (\| (range #\a #\z)
      (range #\A #\Z)))

(e1:define-regexp sexpression:decimal-digit
  (range #\0 #\9))

(e1:define-regexp sexpression:sign
  (\| #\+
      #\-))

(e1:define-regexp sexpression:radix-prefix
  (#\# (\| (union #\b
                  #\o
                  #\d
                  #\x)
           ;; #Dr, with D in [2, 9]
           ((range #\2 #\9)
            #\r)
           ;; #DDr, with DD in [10, 29]
           ((range #\1 #\2)
            (range #\0 #\9)
            #\r)
           ;; #DDr, with DD in [30, 36]
           (#\3
            (range #\0 #\6)
            #\r))))

(e1:define-regexp sexpression:fixnum
  (\| ((? sexpression:sign)
       (+ sexpression:decimal-digit))
      (sexpression:radix-prefix
       (? sexpression:sign)
       (+ (\| (range #\0 #\9)
              (range #\a #\z))))))

(e1:define-regexp sexpression:fixed-point-in-simple-dot-notation
  (\| ((? sexpression:sign)
       (* sexpression:decimal-digit)
       #\.
       (* sexpression:decimal-digit))))
;; FIXME: add scientific notation as well

(e1:define-regexp sexpression:unescaped-character
  (#\# #\\ universe)) ;; escaped characters are recognized *before* this.

(e1:define-regexp sexpression:comment
  (+ (#\;
      (* (complement #\newline))
      #\newline)))

(e1:define-regexp sexpression:whitespace
  (+ (union #\space
            #\tab
            #\cr
            #\newline)))

(e1:define-regexp sexpression:ignorable
  (+ (\| sexpression:whitespace
         sexpression:comment)))

(e1:define-regexp sexpression:open
  #\()
(e1:define-regexp sexpression:close
  #\))
(e1:define-regexp sexpression:dot
  #\.)

(e1:define-regexp sexpression:string
  (#\"
   (* (\| (complement #\" #\\)
          (#\\ universe)))
   #\"))

(e1:define-regexp sexpression:atom
  (+ (\| (complement #\space
                     #\tab
                     #\cr
                     #\newline
                     #\(
                     #\)
                     #\;
                     #\'
                     #\"
                     #\,
                     ;;#\# ;; this is used in some atoms
                     )
         (#\\ (\| #\space
                  #\tab
                  #\cr
                  #\newline
                  #\| ;; for Scheme compatibility
                  #\(
                  #\)
                  #\;
                  #\'
                  #\"
                  #\,
                  ;;#\#
                  ))
         )))


;;;;; Regexp recognizer [FIXME: this is tentative and must be made
;;;;; reusable]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum regexp:result
  (failure)
  ;; the "final" position is the location of the last character.  FIXME: verify in every case.
  (success initial-row initial-column final-row final-column string)) ;; we use a reversed list of characters instead of a string within regexp:read-regexp-helper

(e1:define (regexp:characters->string cc)
  (vector:list->vector (list:reverse cc)))

(e1:define (regexp:read-regexp bp regexp)
  (e1:let* ((beginning-state (backtrackable-port:backtrackable-port->state bp))
            (row (backtrackable-port:backtrackable-port-get-row bp))
            (column (backtrackable-port:backtrackable-port-get-column bp))
            (result (regexp:read-regexp-helper bp
                                               regexp
                                               row column
                                               row column
                                               list:nil)))
    (e1:match result
      ((regexp:result-success initial-row initial-column final-row final-column characters)
       (regexp:result-success initial-row initial-column final-row final-column
                              (regexp:characters->string characters)))
      ((regexp:result-failure)
       (backtrackable-port:backtrack! bp beginning-state)
       result))))
(e1:define (regexp:read-regexp-helper bp regexp initial-row initial-column final-row final-column characters)
  ;; FIXME: only compute beginning-state, row and column where needed, in each case.  This might be an important optimization.
  (e1:let ((beginning-state (backtrackable-port:backtrackable-port->state bp))
           (eof (backtrackable-port:eof? bp)))
    (e1:match regexp
      ((regexp:regexp-empty)
       (regexp:result-success initial-row
                              initial-column
                              final-row
                              final-column
                              characters))
      ((regexp:regexp-range-set rs)
       (e1:if eof
         (regexp:result-failure)
         (e1:let* ((c-row (backtrackable-port:backtrackable-port-get-row bp))
                   (c-column (backtrackable-port:backtrackable-port-get-column bp))
                   (c (backtrackable-port:read-character bp)))
           (e1:if (range-set:has? rs c)
             (regexp:result-success initial-row
                                    initial-column
                                    c-row
                                    c-column
                                    (list:cons c characters))
             (e1:begin
               (backtrackable-port:backtrack! bp beginning-state)
               (regexp:result-failure))))))
      ((regexp:regexp-sequence first second)
       (e1:match (regexp:read-regexp-helper bp first initial-row initial-column final-row final-column characters)
         ((regexp:result-failure)
          (backtrackable-port:backtrack! bp beginning-state)
          (regexp:result-failure))
         ((regexp:result-success _ _ new-final-row new-final-column new-characters)
          (regexp:read-regexp-helper bp second initial-row initial-column new-final-row new-final-column new-characters))))
      ((regexp:regexp-or first second)
       (e1:match (regexp:read-regexp-helper bp first initial-row initial-column final-row final-column characters)
         ((regexp:result-failure)
          (backtrackable-port:backtrack! bp beginning-state) ;; FIXME: unneeded?  The recursive call has alraeady backtracked
          (regexp:read-regexp-helper bp second initial-row initial-column final-row final-column characters))
         (success
          success)))
      ((regexp:regexp-plus plussed)
       (regexp:read-regexp-helper
           bp
           (regexp:regexp-sequence plussed
                                   (regexp:regexp-or regexp
                                                     (regexp:regexp-empty)))
           initial-row initial-column
           final-row final-column
           characters)))))


;;;;; String-to-fixnum parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This assumes that the radix prefix, if any, is well-formed; we're
;;; supposed to call this only *after* recognizing a fixnum as
;;; well-formed, thru a regexp.  Anyway this code checks for
;;; out-of-range character digits, so the regexp can be kept simple.
(e1:define (reader:string->fixnum s)
  (e1:if (whatever:eq? (string:get s 0) #\#)
    (e1:case (string:get s 1)
      ((#\b) ; #b[s]MMM binary
       (reader:string->fixnum-sign-and-magnitude-helper s 2 2))
      ((#\o) ; #o[s]MMM octal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 8))
      ((#\d) ; #o[s]MMM decimal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 10))
      ((#\x) ; #o[s]MMM hexadecimal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 16))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (e1:if (whatever:eq? (string:get s 2) #\r)
         ;; #Rr[s]MMM R in radix 10
         (e1:let ((radix (reader:character-value (string:get s 1) 10)))
           (reader:string->fixnum-sign-and-magnitude-helper s 3 radix))
         ;; #RRr[s]MMM RR in radix 10
         (e1:let ((radix (fixnum:+ (fixnum:* 10 (reader:character-value (string:get s 1) 10))
                                   (reader:character-value (string:get s 2) 10))))
           (reader:string->fixnum-sign-and-magnitude-helper s 4 radix)))))
    (reader:string->fixnum-sign-and-magnitude-helper s 0 10)))

(e1:define (reader:string->fixnum-sign-and-magnitude-helper s i radix)
  (e1:case (string:get s i)
    ((#\-)
     (fixnum:negate (reader:string->fixnum-magnitude-helper s (fixnum:1+ i) 0 radix)))
    ((#\+)
     (reader:string->fixnum-magnitude-helper s (fixnum:1+ i) 0 radix))
    (else
     (reader:string->fixnum-magnitude-helper s i 0 radix))))

(e1:define (reader:string->fixnum-magnitude-helper s i acc radix)
  (e1:if (fixnum:= i (string:length s))
    acc
    (e1:let ((c (string:get s i)))
      (e1:if (reader:valid-for-radix? c radix)
        (reader:string->fixnum-magnitude-helper s
                                                (fixnum:1+ i)
                                                (fixnum:+ (fixnum:* radix acc)
                                                          (reader:character-value c radix))
                                                radix)
        (e1:error "bad character")))))

(e1:define (reader:valid-for-radix? character radix)
  (e1:if (fixnum:<= radix 10)
    (e1:and (fixnum:<= #\0 character)
            (fixnum:< character (fixnum:+ #\0 radix)))
    (e1:or (e1:and (fixnum:<= #\0 character)
                   (fixnum:<= character #\9))
           (e1:and (fixnum:<= #\a character)
                   (fixnum:<  character (fixnum:+ #\a radix -10))))))

;;; This assumes that the given character be valid for the given radix.
(e1:define (reader:character-value character radix)
  (e1:if (e1:or (fixnum:<= radix 10)
                (e1:and (fixnum:<= #\0 character)
                        (fixnum:<= character #\9)))
    (fixnum:- character #\0)
    (fixnum:+ (fixnum:- character #\a)
              10)))


;;;;; Fixed-point parsing and printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (reader:string-in-simple-dot-notation->fixed-point s)
  (e1:let ((c (string:get s 0)))
    (e1:case c
      ((#\+)
       (reader:string-in-simple-dot-notation->fixed-point-helper s 1 0))
      ((#\-)
       (fixedpoint:negate
        (reader:string-in-simple-dot-notation->fixed-point-helper s 1 0))
       )
      (else
       (reader:string-in-simple-dot-notation->fixed-point-helper s 0 0)))))
(e1:define (reader:string-in-simple-dot-notation->fixed-point-helper s i acc)
  (e1:let ((c (string:get s i)))
    (e1:if (whatever:eq? c #\.)
      (e1:let ((integer-part (fixedpoint:fixnum->fixedpoint acc)))
        (reader:string-in-simple-dot-notation->fixed-point-fractional-helper
         s
         (fixnum:1+ i)
         fixedpoint:1/10
         integer-part))
      (e1:let ((c-digit (reader:character-value c 10)))
        (reader:string-in-simple-dot-notation->fixed-point-helper
         s
         (fixnum:1+ i)
         (fixnum:+ c-digit (fixnum:* acc 10)))))))
(e1:define (reader:string-in-simple-dot-notation->fixed-point-fractional-helper s i weight acc)
  (e1:if (fixnum:= i (string:length s))
    acc
    (e1:let* ((digit-as-fixnum (reader:character-value (string:get s i) 10))
              (digit (fixedpoint:fixnum->fixedpoint digit-as-fixnum))
              (weighted-digit (fixedpoint:* weight digit)))
      (reader:string-in-simple-dot-notation->fixed-point-fractional-helper
       s
       (fixnum:1+ i)
       (fixedpoint:10/ weight)
       (fixedpoint:+ acc weighted-digit)))))
(e1:define fixedpoint:1
  (fixedpoint:fixnum->fixedpoint 1))
(e1:define fixedpoint:10
  (fixedpoint:fixnum->fixedpoint 10))
(e1:define fixedpoint:1/10
  (fixedpoint:/ fixedpoint:1
                fixedpoint:10))
(e1:define (fixedpoint:10/ x)
  (fixedpoint:* x fixedpoint:1/10))

(e1:define (printer:write-fixed-point port fp)
  (e1:if (fixnum:= (fixedpoint:sign fp) -1)
    (e1:begin
      (io:write-character port #\-)
      (printer:write-fixed-point port (fixedpoint:negate fp)))
    (e1:begin
      (printer:write-fixnum port (fixedpoint:get-integer-part fp))
      (io:write-character port #\.)
      (printer:write-fixed-point-fractional-part port (fixnum:bitwise-and fixedpoint:fractional-bitmask fp)))))
(e1:define (printer:write-fixed-point-fractional-part port fractional-part-only)
  (e1:if (fixnum:zero? fractional-part-only)
    (e1:bundle)
    (e1:let* ((decimal-digit
               (fixnum:bitwise-and (fixnum:bitwise-not fixedpoint:fractional-bitmask)
                                   (fixnum:* fractional-part-only 10)))
              (digit-as-fixnum
               (fixnum:logic-right-shift decimal-digit fixedpoint:fractional-bit-no)))
      (io:write-character port (fixnum:+ #\0 digit-as-fixnum))
      (printer:write-fixed-point-fractional-part port (fixnum:bitwise-and fixedpoint:fractional-bitmask (fixnum:* fractional-part-only 10))))))


;;;;; Reader programmable interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum reader:result
  (success sexpression)
  (ignore)
  (failure))

;;; A reader case is a closure, taking a backtrackable input port and
;;; returning an s-expression option.  If the option is none, then the
;;; case failed recognition; otherwise its content is the recognized
;;; s-expression.
(e1:define-record reader:case
  closure) ;; backtrackable-input-port -> reader:result

;;; An item list of reader cases
(e1:define reader:item-list-box
  (box:make item-list:nil))

;;; An item list of reader cases to be ignored, or fail: none of these
;;; cases can successfully recognize anything.  Each case is a
;;; reader:case.
(e1:define reader:ignore-item-list-box
  (box:make item-list:nil))

(e1:define-record reader:atom-case
  regexp
  closure) ;; (string locus) -> s-expression

;;; An item list of atom cases.
(e1:define reader:atom-item-list-box
  (box:make item-list:nil))

(e1:define-record reader:prefix-case
  prefix-regexp
  closure) ;; (prefix-string s-expression) -> s-expression reader:result

;;; An item list of prefix cases.
(e1:define reader:prefix-item-list-box
  (box:make item-list:nil))

;;; The simplest and most common prefix case, recognizing PREFIX e
;;; as the two-element s-list (SYMBOL e).
(e1:define (reader:simple-prefix-case prefix-regexp symbol-name)
  (reader:prefix-case
   prefix-regexp
   (e1:lambda (prefix-string prefix-locus sexpression)
     ;;; If the prefix regexp matched, we always succeed:
     (e1:let* ((ssymbol (sexpression:make-with-locus sexpression:symbol-tag
                                                     symbol-name
                                                     prefix-locus))
               (sexpression-locus (sexpression:get-locus sexpression))
               (empty-slist (sexpression:make-with-locus sexpression:empty-list-tag
                                                         0
                                                         sexpression-locus))
               (joined-locus (locus:join prefix-locus sexpression-locus)))
       (reader:result-success
        (sexpression:cons-with-locus ssymbol
                                     (sexpression:cons-with-locus sexpression
                                                                  empty-slist
                                                                  sexpression-locus)
                                     joined-locus))))))

;;; Add PREFIX-NAME-AS-STRING as a prefix expanding to (symbol-name E) where
;;; E is the following s-expression.  The added prefix will be the first one.
(e1:define-macro (reader:define-simple-prefix prefix-name-as-string symbol-name)
  `(item-list:add-first!
       reader:prefix-item-list-box
       (symbol:string->symbol (string:append (symbol:symbol->string (e1:value ,symbol-name))
                                             "-prefix"))
       (reader:simple-prefix-case (regexp:desugar-string ,prefix-name-as-string)
                                  (e1:value ,symbol-name))))

(e1:define (reader:read input-port)
  (e1:let ((bp (backtrackable-port:input-port->backtrackable-port input-port
                                                                  (option:option-none))))
    (reader:read-bp bp)))

(e1:define (reader:read-bp bp)
  (reader:read-bp-helper (box:get reader:item-list-box) bp))
(e1:define (reader:read-bp-helper item-list bp)
  (e1:cond ((list:null? item-list)
            (e1:error "all rules failed"))
           (else
            (backtrackable-port:commit! bp)
            (e1:match (e1:call-closure (cons:get-cdr (list:head item-list)) bp)
              ((reader:result-ignore)
               (reader:read-bp bp))
              ((reader:result-failure)
               (reader:read-bp-helper (list:tail item-list) bp))
              ((reader:result-success sexpression)
               sexpression)))))

(e1:define (reader:recognize-atom string locus)
  (e1:let* ((p (input-port:string->input-port string))
            (file-name-option (locus:locus->file-name-option locus))
            (bp (backtrackable-port:input-port->backtrackable-port p file-name-option))
            (state (backtrackable-port:backtrackable-port->state bp)))
    (reader:recognize-atom-helper (box:get reader:atom-item-list-box) bp state locus)))

(e1:define (reader:recognize-atom-helper item-list bp state locus)
  (e1:if (list:null? item-list)
    (e1:error "all atom rules failed")
    (e1:let* ((atom-case (cons:get-cdr (list:head item-list)))
              (regexp (reader:atom-case-get-regexp atom-case))
              (closure (reader:atom-case-get-closure atom-case)))
      (e1:match (regexp:read-regexp bp regexp)
        ((regexp:result-failure)
         (reader:recognize-atom-helper (list:tail item-list) bp state locus))
        ((regexp:result-success initial-row initial-column final-row final-column string)
         (e1:if (backtrackable-port:eof? bp)
           (e1:call-closure closure string locus)
           (e1:begin ;; we didn't match the whole input
             (backtrackable-port:backtrack! bp state)
             (reader:recognize-atom-helper (list:tail item-list) bp state locus))))))))


(e1:define regexp:ignorable
  (regexp:sregexp->regexp 'sexpression:ignorable))
(e1:define regexp:open
  (regexp:sregexp->regexp '#\())
(e1:define regexp:close
  (regexp:sregexp->regexp '#\)))
(e1:define regexp:dot
  (regexp:sregexp->regexp '#\.))
(e1:define regexp:string
  (regexp:sregexp->regexp 'sexpression:string))
(e1:define regexp:boolean
  (regexp:sregexp->regexp '(#\# (\| #\t
                                    #\f))))
(e1:define regexp:comment-prefix
  (regexp:sregexp->regexp '"#;"))

;; rest ::=
;;   )                     { () }
;; | . <s-expression> )    { s-expression }
;; | <s-expression> <rest> { s-cons(s-expression, rest) }
;;;; s-expressions occurring in rest must *not* be #<eof>'s.
(e1:define (reader:read-rest bp)
  (reader:eat-ignorables bp)
  (e1:match (regexp:read-regexp bp regexp:close)
    ((regexp:result-success initial-row initial-column final-row final-column _)
     (sexpression:make-with-locus
         sexpression:empty-list-tag
         (e0:value 0)
         (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                            initial-row initial-column
                            final-row final-column
                            string:empty)))
    ((regexp:result-failure)
     (e1:match (regexp:read-regexp bp regexp:dot)
       ((regexp:result-success _ _ _ _ _)
        (reader:eat-ignorables bp)
        (e1:let ((sexpression (reader:identity-unless-eof (reader:read-bp bp))))
          (reader:eat-ignorables bp)
          (e1:match (regexp:read-regexp bp regexp:close)
            ((regexp:result-failure)
             (e1:error "expected closed parens"))
            ((regexp:result-success close-initial-row close-initial-column close-final-row close-final-column _)
             (e1:let* ((sexpression-locus (sexpression:get-locus sexpression))
                       (close-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                       close-initial-row close-initial-column
                                                       close-final-row close-final-column
                                                       string:empty)))
               (sexpression:with-locus sexpression
                                       (locus:join sexpression-locus close-locus)))))))
       ((regexp:result-failure) ;; we didn't recognize "."
        (e1:let* ((sexpression (reader:identity-unless-eof (reader:read-bp bp)))
                  (sexpression-locus (sexpression:get-locus sexpression))
                  (rest (reader:read-rest bp))
                  (rest-locus (sexpression:get-locus rest)))
          (sexpression:cons-with-locus sexpression
                                       rest
                                       (locus:join sexpression-locus
                                                   rest-locus))))))))

(e1:define (reader:identity-unless-eof sexpression)
  (e1:if (sexpression:eof-object? sexpression)
    (e1:error "eof within parenthesized sexpression")
    sexpression))

;; s-expression ::=
;; | prefix <s-expression> { lookup-procedure(prefix)(s-expression, scanner-state) }
(e1:define (reader:recognize-prefixed bp)
  (reader:recognize-prefixed-helper bp (box:get reader:prefix-item-list-box)))
(e1:define (reader:recognize-prefixed-helper bp item-list)
  (e1:if (list:null? item-list)
    (reader:result-failure)
    (e1:let* ((prefix-case (cons:get-cdr (list:head item-list)))
              (regexp (reader:prefix-case-get-prefix-regexp prefix-case))
              (closure (reader:prefix-case-get-closure prefix-case)))
      (e1:match (regexp:read-regexp bp regexp)
        ((regexp:result-failure)
         (reader:recognize-prefixed-helper bp (list:tail item-list)))
        ((regexp:result-success initial-row initial-column final-row final-column prefix-string)
         (e1:let ((read-sexpression (reader:read-bp bp))
                  (prefix-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                   initial-row initial-column
                                                   final-row final-column
                                                   string:empty)))
           (e1:match (e1:call-closure closure prefix-string prefix-locus read-sexpression)
             ((reader:result-ignore)
              (reader:result-ignore))
             ((reader:result-failure)
              (reader:result-failure))
             ((reader:result-success result-sexpression)
              (e1:let* ((read-sexpression-locus (sexpression:get-locus read-sexpression))
                        (result-locus (locus:join prefix-locus read-sexpression-locus)))
                (reader:result-success (sexpression:with-locus result-sexpression
                                                               result-locus)))))))))))

(e1:define (reader:eat-ignorables bp)
  ;; Keep eating until we fail:
  (e1:match (reader:eat-ignorables-helper bp (box:get reader:ignore-item-list-box))
    ((reader:result-ignore)
     (backtrackable-port:commit! bp)
     (reader:eat-ignorables bp))
    ((reader:result-failure)
     (reader:result-failure))))
(e1:define (reader:eat-ignorables-helper bp item-list)
  (e1:if (list:null? item-list)
    (reader:result-failure)
    (e1:match (e1:call-closure (cons:get-cdr (list:head item-list)) bp)
      ((reader:result-ignore)
       (reader:result-ignore))
      ((reader:result-failure)
       (reader:eat-ignorables-helper bp (list:tail item-list)))
      ((reader:result-success _)
       (e1:error "eat-ignorables: not supposed to succeed")))))

(e1:toplevel
  (item-list:add-first!
     reader:item-list-box
     (e1:value ignorable)
     (e1:lambda (bp)
       (reader:eat-ignorables bp)))

  (item-list:add-first!
     reader:ignore-item-list-box
     (e1:value ignorable-regexp)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:ignorable)
         ((regexp:result-success _ _ _ _ _)
          (reader:result-ignore))
         ((regexp:result-failure)
          (reader:result-failure)))))

  (item-list:add-last!
     reader:ignore-item-list-box
     (e1:value comment-prefix)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:comment-prefix)
         ((regexp:result-success _ _ _ _ _)
          (reader:read-bp bp) ;; eat and ignore this
          (reader:result-ignore))
         ((regexp:result-failure)
          (reader:result-failure)))))

  (item-list:add-after!
     reader:item-list-box
     (e1:value ignorable)
     (e1:value parenthesized)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:open)
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column _)
          (e1:let* ((open-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                   initial-row initial-column
                                                   final-row final-column
                                                   string:empty))
                    (rest (reader:read-rest bp))
                    (rest-locus (sexpression:get-locus rest)))
            (reader:result-success (sexpression:with-locus rest
                                                           (locus:join open-locus
                                                                       rest-locus))))))))

  (item-list:add-after!
     reader:item-list-box (e1:value parenthesized)
     (e1:value prefix)
     (e1:lambda (bp)
       (reader:recognize-prefixed bp)))

  (item-list:add-after!
     reader:item-list-box (e1:value parenthesized)
     (e1:value string)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:string)
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column string)
          (e1:let* ((result-string (reader:unescape-string-literal string))
                    (file-name-option (backtrackable-port:backtrackable-port-get-file-name-option bp))
                    (locus (locus:locus-known file-name-option
                                              initial-row initial-column
                                              final-row final-column
                                              string:empty)))
            (reader:result-success (sexpression:make-with-locus sexpression:string-tag
                                                                result-string
                                                                locus)))))))

  (item-list:add-last!
     reader:item-list-box
     (e1:value atom)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp (regexp:sregexp->regexp 'sexpression:atom))
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column string)
          (e1:let* ((file-name-option (backtrackable-port:backtrackable-port-get-file-name-option bp))
                    (locus (locus:locus-known file-name-option
                                              initial-row initial-column
                                              final-row final-column
                                              string:empty)))
            (reader:result-success (reader:recognize-atom string locus)))))))

  (item-list:add-last!
     reader:item-list-box
     (e1:value eof)
     (e1:lambda (bp)
       (e1:let ((s (backtrackable-port:backtrackable-port->state bp))
                (c (backtrackable-port:read-character bp)))
         (backtrackable-port:backtrack! bp s)
         (e1:if (whatever:eq? c io:eof)
           ;; FIXME: add locus, if needed.  However I'd say that '#<eof>
           ;; is already informative enough.
           (reader:result-success sexpression:eof)
           (reader:result-failure)))))

  ;; The "," prefix has to be defined before the other prefixes
  ;; starting with a comma, so that the others take priority.
  (reader:define-simple-prefix "," unquote)
  (reader:define-simple-prefix ",@" unquote-splicing)
  (reader:define-simple-prefix "`" quasiquote)
  (reader:define-simple-prefix "'" quote)

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value boolean)
     (reader:atom-case regexp:boolean
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:boolean-tag
                                                      (whatever:eq? (string:get string 1) #\t)
                                                      locus))))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value fixnum)
     (reader:atom-case (regexp:sregexp->regexp 'sexpression:fixnum)
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:fixnum-tag
                                                      (reader:string->fixnum string)
                                                      locus))))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value fixed-point)
     (reader:atom-case (regexp:sregexp->regexp 'sexpression:fixed-point-in-simple-dot-notation)
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:fixed-point-tag
                                                      (reader:string-in-simple-dot-notation->fixed-point string)
                                                      locus))))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value unescaped-character)
     (reader:atom-case (regexp:sregexp->regexp 'sexpression:unescaped-character)
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:character-tag
                                                      (string:get string 2) ;; #\a
                                                      locus))))

  (item-list:add-last!
     reader:atom-item-list-box
     (e1:value symbol)
     (reader:atom-case regexp:anything ;; this always matches
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:symbol-tag
                                                      (symbol:string->symbol
                                                       (reader:unescape-symbol-literal string))
                                                      locus)))))

;;;;; Character and string escaping
;;;;; FIXME: move the rest of the implementation here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
  (sexpression:set-character-escape! #\nul "nul")
  (sexpression:set-character-escape! #\space "space")
  (sexpression:set-character-escape! #\tab "tab")
  (sexpression:set-character-escape! #\newline "newline")
  (sexpression:set-character-escape! #\cr "cr")
  (sexpression:set-character-escape! io:eof "eof")

  (sexpression:set-string-escape! #\" #\")
  (sexpression:set-string-escape! #\\ #\\)
  (sexpression:set-string-escape! #\tab #\t)
  (sexpression:set-string-escape! #\newline #\n)
  (sexpression:set-string-escape! #\cr #\c)
  (sexpression:set-string-escape! #\| #\|) ;; for Guile compatibility only

  ;;; No, this is unsatisfactory.  It would be nice to be able to
  ;;; support *symbol* names including spaces, but I don't want to pay
  ;;; the price of having escaped spaces in printed *strings*.
  ;; (sexpression:set-string-escape! #\space #\space)
  )

;;;;; String utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (string:trim s)
  (e1:let* ((first-index (string:trim-index-left s))
            (last-index (string:trim-index-right s))
            (result-length (fixnum:- last-index first-index -1))
            (result-length (e1:if (fixnum:>= result-length 0)
                                   result-length
                                   0))
            (result (vector:make result-length)))
    (vector:blit result 0 s first-index result-length)
    result))
;;; Return the first non-whitespace character index, or 0.
(e1:define (string:trim-index-left s)
  (string:trim-index-left-from s 0))
(e1:define (string:trim-index-left-from s i)
  (e1:cond ((fixnum:= i (string:length s))
            0)
           (bind (c (string:get s i)))
           ((e1:or (whatever:eq? c #\space)
                   (whatever:eq? c #\newline)
                   (whatever:eq? c #\tab))
            (string:trim-index-left-from s (fixnum:1+ i)))
           (else
            i)))

;;; Return the last non-whitespace character index, or the length predecessor.
(e1:define (string:trim-index-right s)
  (string:trim-index-right-from s (fixnum:1- (string:length s))))
(e1:define (string:trim-index-right-from s i)
  (e1:cond ((fixnum:= i -1)
            (fixnum:1- (string:length s)))
           (bind (c (string:get s i)))
           ((e1:or (whatever:eq? c #\space)
                   (whatever:eq? c #\newline)
                   (whatever:eq? c #\tab))
            (string:trim-index-right-from s (fixnum:1- i)))
           (else
            i)))


;;;;; Object properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A facility to associate user-defined data to arbitrary objects.
;;; Notice that this essentially assumes a non-moving garbage
;;; collector (if any), as pointers can be used as keys.

(e1:define state:property-table
  (unboxed-hash:make))

(e1:define (state:get-property-alist object)
  (e1:if (unboxed-hash:has? state:property-table object)
    (unboxed-hash:get state:property-table object)
    alist:nil))
(e1:define (state:set-property-alist! object alist)
  (unboxed-hash:set! state:property-table object alist))
(e1:define (state:unset-property-alist! object)
  (unboxed-hash:unset! state:property-table object))

(e1:define (state:get-property object key)
  (alist:lookup (state:get-property-alist object) key))

(e1:define (state:has-property? object key)
  (alist:has? (state:get-property-alist object) key))

(e1:define (state:set-property! object key value)
  (e1:let* ((old-alist (state:get-property-alist object))
            (new-alist (alist:bind-unique old-alist key value)))
    (state:set-property-alist! object new-alist)))

(e1:define (state:unset-property! object key)
  (e1:let* ((old-alist (state:get-property-alist object))
            (new-alist (alist:unbind-one old-alist key)))
    (e1:if (alist:null? new-alist)
      (state:unset-property-alist! object)
      (state:set-property-alist! object new-alist))))

;;; Attach the given property to the object which is being built, and
;;; return it.
(e1:define-macro (state:with-property key value . forms)
  (e1:let ((result-name (sexpression:fresh-symbol)))
    `(e1:let ((,result-name ,@forms))
       (state:set-property! ,result-name ,key ,value)
       ,result-name)))

;;; FIXME: update unexec to keep properties into account.


;;;;; Source file loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e1:load file-name)
  (e1:let* ((f (io:open-file file-name io:read-mode))
            (p (input-port:file->input-port f))
            (bp (backtrackable-port:input-port->backtrackable-port
                    p
                    (option:option-some file-name))))
    (e1:load-helper file-name bp)))
(e1:define (e1:load-helper file-name bp)
  (e1:let ((s (reader:read-bp bp)))
    (e1:unless (sexpression:eof-object? s)
      (repl:macroexpand-transform-and-execute s)
      (e1:load-helper file-name bp))))


;;;;; Guile-compatibility macro to help bootstrap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e1:toplevel was needed in Guile; in epsilon1 it's essentially
;;; useless, but we want to temporarily keep it just to be able to
;;; read the same files with Guile's load and with epsilon1's e1:load.
(e1:define-macro (e1:toplevel . forms)
  `(e1:begin ,@forms))
