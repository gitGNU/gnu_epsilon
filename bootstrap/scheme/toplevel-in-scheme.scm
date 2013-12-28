;;;;; -*- Scheme -*- or something close enough for Emacs.
;;;;; Macros for using epsilonone toplevel expressions and definitions from Scheme.

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


;;; FIXME: move
;;;;; Debugging facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (meta:macroexpand guile-sexpression)
  (print-expression_ (e1:macroexpand (_ guile-sexpression)))
  e0:unspecified)

(define (meta:print-macro-definition macro-name-as-guile-symbol)
  (sexpression->guile-sexpression (state:macro-get-body (guile-symbol->symbol macro-name-as-guile-symbol))))
(define (meta:print-macro-procedure-name macro-name-as-guile-symbol)
  (symbol->guile-symbol (state:macro-get-macro-procedure-name (guile-symbol->symbol macro-name-as-guile-symbol))))

(define (meta:print-procedure-definition procedure-name-as-guile-symbol)
  (let* ((procedure-name (guile-symbol->symbol procedure-name-as-guile-symbol))
         (formals (state:procedure-get-formals procedure-name))
         (body (state:procedure-get-body procedure-name)))
    (format #t "Formals: ~s\n" (map symbol->guile-symbol (list->guile-list formals)))
    (print-expression_ body)))

;; (define (meta:macroexpand-1 guile-sexpression)
;;   (let* ((sexpression (guile-sexpression->sexpression guile-sexpression)))
;;     (e0:let (result did-we-expand) (e1:macroexpand-1 sexpression)
;;       (e0:bundle (sexpression->guile-sexpression result) did-we-expand))))

(define (meta:eval expression-as-guile-sexpr local-environment)
  (let ((results-as-list
         (e0:eval (e1:macroexpand (guile-sexpression->sexpression expression-as-guile-sexpr))
                  local-environment)))
    (apply values
           (list->guile-list results-as-list))))

(define (meta:eval-ee expression-as-guile-sexpr)
  (meta:eval expression-as-guile-sexpr alist:nil))

(define (sort-symbols symbol-list)
  (sort symbol-list
        (lambda (a b)
          (string< (symbol->string a)
                   (symbol->string b)))))

(define (meta:global-names)
  (sort-symbols (map symbol->guile-symbol
                     (list->guile-list (state:global-names)))))

(define (meta:procedure-names)
  (sort-symbols (map symbol->guile-symbol
                     (list->guile-list (state:procedure-names)))))

(define (meta:primitive-names)
  (sort-symbols (map symbol->guile-symbol
                     (list->guile-list (state:primitive-names)))))

(define (meta:macro-names)
  (sort-symbols (map symbol->guile-symbol
                     (list->guile-list (state:macro-names)))))

;; (define (e1:repl)
;;   (e1:repl-loop))
;; (define (e1:repl-loop)
;;   (format #t "\ne1>\n")
;;   (let ((input (read)))
;;     (unless (eof-object? input)
;;       (format #t "You wrote: ~s\n  -->\n" input)
;;       (let ((expression (e1:macroexpand (guile-sexpression->sexpression input))))
;;         (print-expression_ expression)
;;         (let ((results (e0:eval-ee expression)))
;;           (format #t "Results:\n")
;;           (do ((remaining-results results (list:tail remaining-results)))
;;               ((whatever->guile-boolean (list:null? remaining-results)))
;;             (format #t "~s\n" (list:head remaining-results)))
;;           (e1:repl-loop))))))


;;;;; epsilon1 interpreter interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Just for convenience, define a version of e1:macroexpand which is
;;; safe to call from Guile, without using Guile's state environment
;;; by mistake:
(define (e1:macroexpand epsilon1-sexpression)
  (e0:unbundle (e0:eval-ee (e0:call* (e0:value e1:macroexpand)
                                     (list:singleton (e0:value* epsilon1-sexpression))))))
;; (define-macro (e1:toplevel-form form-as-guile-sexpression)
;;   `(let* ((form-as-sexpression (guile-sexpression->sexpression ',form-as-guile-sexpression))
;;           (untransformed-expression (e1:macroexpand form-as-sexpression))
;;           (transformed-expression-builder (e0:call* (e0:value transform:transform-non-procedure)
;;                                                     (list:singleton (e0:value* untransformed-expression))))
;;           (transformed-expression (e0:unbundle (e0:eval-ee transformed-expression-builder)))
;;           (results (e0:eval-ee transformed-expression)))
;;      (apply values (list->guile-list results))))
;; (define-macro (e1:toplevel . forms-as-guile-sexpression)
;;   `(e1:toplevel-form (e1:begin ,@forms-as-guile-sexpression)))

(define-macro (e1:toplevel-form guile-sexpression)
  `(let* ((epsilon0-expression-returning-results
           (e0:call* (e0:value repl:macroexpand-transform-and-execute)
                     (list:list1 (e0:value* (guile-sexpression->sexpression ',guile-sexpression)))))
          (results-as-list (e0:unbundle (e0:eval-ee epsilon0-expression-returning-results))))
     ;; Return multiple results to Guile:
     (apply values (list->guile-list results-as-list))))

(define-macro (e1:toplevel . guile-sexpressions)
  (cond ((null? guile-sexpressions)
         '(values)) ;; return zero results
        ((null? (cdr guile-sexpressions))
         `(e1:toplevel-form ,(car guile-sexpressions)))
        (else
         `(begin
            (e1:toplevel-form ,(car guile-sexpressions))
            (e1:toplevel ,@(cdr guile-sexpressions))))))


;;;;; epsilon1 procedure and global definitions from Guile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Ok, at this point we have reflective information.  Now re-define
;;; e1:define so that new definitions update reflective information.
;;; We won't touch Guile's state environment any longer.

(define-macro (e1:define name-or-list body-form)
  (let* ((procedure (pair? name-or-list))
         (name (if procedure (car name-or-list) name-or-list)))
    (if procedure
        `(begin
           (format #t "Defining the procedure ~s and setting metadata...\n" ',name)
           (let ((body (e1:macroexpand (guile-sexpression->sexpression ',body-form))))
             (print-expression_ body)
             (state:procedure-set! (guile-symbol->symbol ',name)
                                   (guile-list->list ',(cdr name-or-list) guile-symbol->symbol)
                                   body))
           (format #t "Ok.\n"))
        `(begin
           (format #t "Defining the global ~s and setting metadata...\n" ',name)
           (let ((body (e1:macroexpand (guile-sexpression->sexpression ',body-form))))
             (print-expression_ body)
             (state:global-set! (guile-symbol->symbol ',name)
                                (e0:unbundle (e0:eval-ee body)))
             (format #t "Ok.\n"))))))


;;;;; epsilon1 macro definitions from Guile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (e1:trivial-define-macro name-as-guile-symbol body-as-guile-sexpression)
  (format #t "Defining the trivial macro ~s as ~s\n" name-as-guile-symbol body-as-guile-sexpression)
  `(begin
     ;;(define-macro (,name-as-guile-symbol . arguments) ,body-as-guile-sexpression)
     (state:macro-set! (guile-symbol->symbol ',name-as-guile-symbol)
                       (guile-sexpression->sexpression ',body-as-guile-sexpression))))

;; ;;; [FIXME: explain this in a good comment]
;; (define-macro (e1:define-macro . stuff)
;;   (format #t "Defining the macro ~s:...\n" (caar stuff))
;;   `(e1:toplevel (e1:define-macro ,@stuff)))


;;;;; Making available to Guile any epsilon1 top level form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This simple facility lets us save some ugly e1:toplevel forms at
;;; the top level in Guile, for selected epsilon1 toplevel forms or
;;; procedures.
(define-macro (export-to-guile-toplevel macro-name)
  `(define-macro (,macro-name . stuff)
     `(e1:toplevel (,',macro-name ,@stuff))))
