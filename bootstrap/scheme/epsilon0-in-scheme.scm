;;;;; epsilon0 with s-expression syntax defined in -*- Scheme -*-

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


;;;; This file contains Scheme compatibility stuff only useful to
;;;; simulate epsilon0 on top of Scheme.  The internal implementation
;;;; may be kludgish, but that doesn't matter: the purpose is just to
;;;; simulate epsilon0 in Guile.

;;(use-modules (srfi srfi-11)) ;; for let-values


;;; We need to make the stack bigger than the default for Guile 1,
;;; since we may use some deep indirect non-tail recursion:
(if (equal? (major-version) "1")
  (debug-set! stack 100000000))


;;; Utility definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Utility procedure returning a symbol with a name equal to the
;;;; given prefix prepended to the name of the given symbol:
(define (prefix-symbol prefix-as-string symbol)
  (let ((result-name (string-append prefix-as-string
                                    (symbol->string symbol))))
    (string->symbol result-name)))

;;; Utility procedure to check that e0 forms don't bind the same
;;; variable more than once in the same form:
(define (assert-distinct variables form-name)
  (let loop ((remaining-variables variables))
    (cond ((null? remaining-variables)
           #t)
          ((memq (car remaining-variables) (cdr remaining-variables))
           (error (string-append form-name ": the same variable is bound twice:")
                  (car remaining-variables)))
          (else
           (loop (cdr remaining-variables))))))

;;;; epsilon0 constants:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define e0:the-empty-list '())
(define e0:unspecified (if #f #f)) ;; 0)


;;;; epsilon0 syntactic forms:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; e0:variable would need whatever symbol literals, which can't be
;;; parsed with my trivial Guile extension.  That's why there is no
;;; e0:variable macro: the bootstrap code will have to do without it
;;; (but it can use symbols as variables).

;; (define-macro (e0:variable variable-name)
;;   (if (symbol? variable-name)
;;       `,variable-name
;;       (error "e0:variable: not a symbol:" variable-name)))

;;; The following commented-out definition of e0:value works but is
;;; very inefficient, as it creates a new whatever object for
;;; each use of the literal, after macroexpansion.  Whatever objects
;;; can not be generated once and for all as part of the
;;; macroexpansion, since this is not supported in Guile 2.x (as of
;;; 2.0.5); hence the next definition.

;; (define-macro (e0:value literal)
;;   (cond ((whatever? literal)
;;          literal)
;;         ((null? literal)
;;          'e0:the-empty-list)
;;         ((symbol? literal)
;;          `(guile-symbol->symbol ',literal))
;;         ((boolean? literal)
;;          `(guile-sexpression->whatever ,(if literal #t #f)))
;;         ((integer? literal)
;;          `(guile-sexpression->whatever ,literal))
;;         ((string? literal)
;;          `(guile-string->string ,literal))
;;         (else
;;          (error "e0:value: unimplemented case:" literal))))


;;; The following alternative definition of e0:value is more efficent
;;; and works with all versions of Guile.  It associates a fresh
;;; global variable to each literal, replacing a literal call with a
;;; reference to the global, created once and for all at
;;; macroexpansion time.

;;; Helper procedure: associate the given (whatever) value to the
;;; given literal using a fresh global variable; return the global
;;; name.
(define (associate-value-to literal value)
  (let* ((associated-global-or-false (object-property literal 'associated-global))
         (associated-global (or associated-global-or-false
                                (gensym "associated-global-"))))
    (unless associated-global-or-false
      (define-object-from-anywhere associated-global value))
    associated-global))
(define-macro (e0:value literal)
  (associate-value-to literal
                      (cond ((whatever? literal)
                             literal)
                            ((symbol? literal)
                             (guile-symbol->symbol literal))
                            ((or (null? literal)
                                 (boolean? literal)
                                 (integer? literal)
                                 (char? literal))
                             (guile-sexpression->whatever literal))
                            ((string? literal)
                             (guile-string->string literal))
                            (else
                             (error "e0:value: unimplemented case:" literal)))))

(define-macro (e0:if-in discriminand constants then-branch else-branch)
  (if (= (length constants) 1)
    `(if (eq? (whatever->guile-fixnum ,discriminand)
              ,(whatever->guile-fixnum (guile-sexpression->whatever (car constants)))) ;; translate the car into a fixnum
         ,then-branch
         ,else-branch)
    ;; Don't create the list at runtime:
    (let ((constants-name (associate-value-to constants constants)))
      `(if (memq (whatever->guile-fixnum ,discriminand) ,constants-name)
           ,then-branch
           ,else-branch))))

;;; Using the associate-value-to hack is particularly important
;;; here, in order to avoid creating a list at runtime.
(define-macro (e0:if-in_ discriminand constants then-branch else-branch)
  ;; In Guile I have to use member instead of memq, becuase our
  ;; whatever objects are boxed
  (let ((constants-as-whatevers-name (associate-value-to constants
                                                         (map guile-sexpression->whatever constants))))
    `(if (member ,discriminand ,constants-as-whatevers-name)
         ,then-branch
         ,else-branch)))

;;; I need this complicated definition to force left-to-right evaluation:
(define-macro (e0:bundle . expressions)
  (let ((names (map (lambda whatever (gensym "bundle-item-"))
                    expressions)))
    `(let* ,(map (lambda (name expression) `(,name ,expression))
                 names
                 expressions)
       (values ,@names))))
(define-macro (e0:let bound-variables bound-expression body)
  (assert-distinct bound-variables "e0:let")
  (let ((unused-rest-name (gensym "let-bound-")))
    `(call-with-values (lambda () ,bound-expression)
       (lambda ,(append bound-variables unused-rest-name) ,body))))

;;; When bootstrapping from Scheme, primitives are just ordinary procedures
;;; with a particular prefix in their name:
;; (define-macro (e0:primitive primitive-name . arguments)
;;   `(,(prefix-symbol "PRIMITIVE-" primitive-name) ,@arguments))
(define-macro (e0:primitive primitive-name . arguments)
  `(whatever-call-with-guile-parameters ',primitive-name (list ,@arguments)))

;;; When bootstrapping from Scheme, procedures are just ordinary Scheme
;;; procedures with a particular prefix in their name.  The prefix is useful
;;; so that we don't forget to use the e0:call macro:
(define-macro (e0:call procedure-name . arguments)
  `(,procedure-name
    ,@arguments))

;; (define (e0:trivial-list* arguments)
;;   (if (null? arguments)
;;       'list:nil
;;       `(list:cons ,(car arguments)
;;                   ,(e0:trivial-list* (cdr arguments)))))
(define (e0:call-indirect procedure-expression . arguments)
  (apply (primitive-eval (symbol->guile-symbol procedure-expression))
         arguments))

;;; Global definitions (from anywhere: we can't just use Scheme's
;;; define form):
(define (define-object-from-anywhere variable-name value)
  (let* ((obarray (module-obarray (current-module)))
         (variable-or-#f (module-obarray-ref obarray variable-name)))
    ;; This different behavior in case of an already existing binding is needed
    (if variable-or-#f
        (variable-set! variable-or-#f value)
        (module-obarray-set! obarray
                             variable-name
                             (make-variable value)))
    ;;(format #t "Defined ~s as ~s\n" variable-name value)
    (values) ;; don't return anything
    ))
(define (undefine-object-from-anywhere variable-name)
  (module-obarray-remove! (module-obarray (current-module))
                          variable-name)
  (format #t "Undefined ~s\n" variable-name))
;; (define (PRIMITIVE-e0:define-non-procedure variable-name something)
;;   (if (procedure? something)
;;       (error "e0:define-non-procedure: the argument is a procedure")
;;       (define-object-from-anywhere variable-name something)))
;; (define (PRIMITIVE-e0:define-procedure procedure-name formals #!only one!#expression)
;;   (assert-distinct formals "PRIMITIVE-e0:define-procedure")
;;   (define-object-from-anywhere
;;     procedure-name
;;     (primitive-eval `(lambda (,@formals) ,expression))))
;; (define (PRIMITIVE-e0:undefine-non-procedure variable-name)
;;   (undefine-object-from-anywhere variable-name))
;; (define (PRIMITIVE-e0:undefine-procedure procedure-name)
;;   (undefine-object-from-anywhere procedure-name
;; ))


;;;; Threads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-modules (ice-9 threads))
(define (call-with-our-thread-as-the-first-argument procedure . actuals)
  (apply procedure (cons (guile-sexpression->whatever (current-thread))
                         actuals)))
(define-macro (e0:fork procedure-name . actuals)
  `(guile-sexpression->whatever (call-with-new-thread (lambda ()
                                              (call-with-our-thread-as-the-first-argument ,procedure-name
                                                                                          ,@actuals)))))
(define (e0:join thread)
  (join-thread (whatever->guile-thread thread)))


;; ;;;; Conditional execution flag:
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define e0:using-guile #t)


;; ;;;; Values (with tagging):
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define value
;;   (make-record-type "value" '(tag content)))

;; (define (make-value tag content)
;;   ((record-constructor value '(tag content))
;;    tag
;;    content))

;; ;;; Accessors to members of the record "value" (for reading).
;; (define value->tag (record-accessor value 'tag))
;; (define value->content (record-accessor value 'content))


(define (PRIMITIVE-debug-dump stuff)
  (dump stuff)
  (e0:value 42))


;;;;; Global definition facility -- first implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Global Guile data structures keeping track of what we have defined
;;; up to this point:
(define globals-to-define '())
(define procedures-to-define '())

(define-macro (e1:define name-or-list body-form)
  (let* ((procedure (pair? name-or-list))
         (name (if procedure (car name-or-list) name-or-list))
         (lambda-name (gensym "lambda-name-")))
    (if procedure
        `(begin
           (define ,name-or-list ,body-form)
           (format #t "Defining the procedure ~s\n" ',name)
           (set! procedures-to-define
                 (cons (list ',name ',(cdr name-or-list) ',body-form)
                       procedures-to-define)))
        `(let ((result ,body-form))
           (format #t "Defining the global ~s\n" ',name)
           (set! globals-to-define
                 (cons ',name
                       globals-to-define))
           (define-object-from-anywhere
             ',name
             result)))))


;;;;; I want to be able to evaluate e1:load from Guile as well
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (e1:load file-name)
  `(begin
     (format #t "Loading ~s from Guile...\n" ,file-name)
     (load ,file-name)
     (format #t "... successfully loaded ~s from Guile.\n" ,file-name)))


;;;; We're done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Also make available our utility procedures for converting between
;;; our representation and Guile's:
(load "conversion.scm")

(display "scheme-to-epsilon0.scm: STILL ALIVE AT THE END\n")
;(define (boo t a b) (dump t a b) (sleep 5) (cons a b))

(define-macro (when-guile . forms)
  `(begin
     ,@forms))

(define-macro (unless-guile . forms)
  `(begin))
