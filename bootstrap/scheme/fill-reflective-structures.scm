;;;;; This is  -*- Scheme -*-.
;;;;; Facility for filling our reflective data structures as defiend in core.e
;;;;; from the Guile's data structures we have updated at each definition.

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


;;;;; Non-macro expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define once and for all the epsilon1 version of some
;;; commonly-needed symbols.  This optimization is important in
;;; practice, as we can avoid interning the same symbols over and over
;;; again for each test in each iteration:
(define-macro (define-epsilon1-symbol guile-symbol-name)
  `(define ,(symbol-append 'epsilon1-symbol-named- guile-symbol-name)
     (e0:value ,guile-symbol-name)))
(define-epsilon1-symbol e0:variable)
(define-epsilon1-symbol e0:value)
(define-epsilon1-symbol e0:bundle)
(define-epsilon1-symbol e0:primitive)
(define-epsilon1-symbol e0:let)
(define-epsilon1-symbol e0:call)
(define-epsilon1-symbol e0:call-indirect)
(define-epsilon1-symbol e0:if-in)
(define-epsilon1-symbol e0:fork)
(define-epsilon1-symbol e0:join)

;;; Temporary definition with no actual macro support, written in
;;; Scheme.  We use this version only until proper macroexpansion
;;; support is added later --- and of course we will use s-expression
;;; syntax for defining it, hence the need for this bootstrapping
;;; version.  The argument is an *epsilonone* (not Guile)
;;; s-expression, and of course the result is an epsilonone expression.
(define (e0:non-macro-expand s)
  ;;(dump s (^ s))
  (cond ((whatever->guile-boolean (sexpression:expression? s))
         (sexpression:eject-expression s))
        ((whatever->guile-boolean (sexpression:symbol? s))
         (e0:variable* (sexpression:eject-symbol s)))
        ((or (whatever->guile-boolean (sexpression:null? s))
             (whatever->guile-boolean (sexpression:fixnum? s))
             (whatever->guile-boolean (sexpression:boolean? s))
             (whatever->guile-boolean (sexpression:string? s)))
         (e0:value* (sexpression:eject s)))
        ((and (whatever->guile-boolean (sexpression:cons? s))
              (whatever->guile-boolean (sexpression:symbol? (sexpression:car s))))
         (let ((symbol (sexpression:eject-symbol (sexpression:car s)))
               (arguments (sexpression:cdr s)))
           ;; Here we don't perform arity checks
           (cond ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:variable))
                  (e0:variable* (sexpression:eject-symbol (sexpression:car arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:value))
                  (e0:value* (sexpression:eject (sexpression:car arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:bundle))
                  (e0:bundle* (e0:non-macro-expand-sexpressions arguments)))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:primitive))
                  (e0:primitive* (sexpression:eject-symbol (sexpression:car arguments))
                                 (e0:non-macro-expand-sexpressions (sexpression:cdr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:let))
                  (e0:let* (e0:non-macro-expand-symbols (sexpression:car arguments))
                           (e0:non-macro-expand (sexpression:cadr arguments))
                           (e0:non-macro-expand (sexpression:caddr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:call))
                  (e0:call* (sexpression:eject-symbol (sexpression:car arguments))
                            (e0:non-macro-expand-sexpressions (sexpression:cdr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:call-indirect))
                  (e0:call-indirect* (e0:non-macro-expand (sexpression:car arguments))
                                     (e0:non-macro-expand-sexpressions (sexpression:cdr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:if-in))
                  (e0:if-in* (e0:non-macro-expand (sexpression:car arguments))
                             (e0:non-macro-expand-values (sexpression:cadr arguments))
                             (e0:non-macro-expand (sexpression:caddr arguments))
                             (e0:non-macro-expand (sexpression:cadddr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:fork))
                  (e0:fork* (sexpression:eject-symbol (sexpression:car arguments))
                            (e0:non-macro-expand-sexpressions (sexpression:cdr arguments))))
                 ((whatever->guile-boolean (whatever:eq? symbol epsilon1-symbol-named-e0:join))
                  (e0:join* (e0:non-macro-expand (sexpression:car arguments))))
                 ;; I've removed the "extension" case
                 (else
                  ;;(format #t "The car is not a form name: using ~s as a procedure name" (symbol->guile-symbol symbol))
                  (e0:call* symbol
                            (e0:non-macro-expand-sexpressions arguments))))))
        (else
         (format #t "About to fail: unimplemented case in e0:non-macro-expand (Scheme version): ~s\n" s)
         ;(dump s)
         ;(dump (sexpression->guile-sexpression s))
         (e1:error (e0:value "e0:non-macro-expand (Scheme version): unimplemented")))))
(define (e0:non-macro-expand-sexpressions ss)
  (if (whatever->guile-boolean (sexpression:null? ss))
      list:nil
      (list:cons (e0:non-macro-expand (sexpression:car ss))
                 (e0:non-macro-expand-sexpressions (sexpression:cdr ss)))))
(define (e0:non-macro-expand-symbols ss)
  (if (whatever->guile-boolean (sexpression:null? ss))
      list:nil
      (list:cons (sexpression:eject-symbol (sexpression:car ss))
                 (e0:non-macro-expand-symbols (sexpression:cdr ss)))))
(define (e0:non-macro-expand-values ss)
  (if (whatever->guile-boolean (sexpression:null? ss))
      list:nil
      (list:cons (sexpression:eject (sexpression:car ss))
                 (e0:non-macro-expand-values (sexpression:cdr ss)))))

;;;;; Fill structures:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set metadata for the globals and procedures we have already
;;; defined with e1:guile:
(define (set-metadata!)
  ;; Add global metadata:
  (format #t "Adding global metadata...\n")
  (let loop ((remaining-globals globals-to-define))
    (unless (null? remaining-globals)
      (let ((first-global (car remaining-globals)))
        (format #t "Adding metadata for the global ~s...\n" first-global)
        ;;(dump first-global)
        (state:global-set! (guile-symbol->symbol first-global)
                           (primitive-eval first-global)))
        (loop (cdr remaining-globals))))
  ;; Add procedure metadata.  Notice that we have to reverse the list,
  ;; so as to give priority to the most recent definition of each
  ;; symbol:
  (format #t "Adding procedure metadata...\n")
  (let loop ((remaining-procedures (reverse procedures-to-define)))
    (unless (null? remaining-procedures)
      (let* ((first-procedure (car remaining-procedures))
             (name (car first-procedure))
             (formals (cadr first-procedure))
             (body (caddr first-procedure)))
        ;;(dump name formals body)
        (format #t "Adding metadata for the procedure ~s (~s to go)...\n" name (length remaining-procedures))
        ;;(dump name formals body)
;;(benchmark
        (state:procedure-set! (guile-symbol->symbol name)
                              (guile-list->list formals guile-symbol->symbol)
                              (e0:non-macro-expand (guile-sexpression->sexpression body)))
;;)
        (loop (cdr remaining-procedures)))))
  (format #t "Done\n"))

;;; Do it:
(set-metadata!)
