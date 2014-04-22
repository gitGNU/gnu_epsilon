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

(e1:define (fact n) (e1:if-in n (0) 1 (e1:primitive fixnum:* n (fact (e1:primitive fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))
(e1:define (test n)
  (e1:dotimes (i n)
    (fio:write "fibo(" (i i) ") = " (i (fibo i)) "\n")))


;;;;; Secondary symbol table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A second implementation of symbols, compatible with the one in
;;; core.e, but of course written at a higher level.
(e1:define-record symbol:symbol
  name          ;; 0
  global-valid  ;; 1
  global        ;; 2
  formals       ;; 3
  procedure-body;; 4
  macro-body    ;; 5
  macro-procedure-name ;; 6
  primitive-descriptor ;; 7
  bytecode      ;; 8
  native        ;; 9
  extensions)   ;; 10

(e1:define (symbol:empty name)
  (symbol:symbol #:name                 (vector:shallow-clone name)
                 #:global-valid         #f
                 #:global               127 ;; just my silly convention for debugging
                 #:formals              list:nil
                 #:procedure-body       0
                 #:macro-body           0
                 #:macro-procedure-name 0
                 #:primitive-descriptor 0
                 #:bytecode             0
                 #:native               0
                 #:extensions           list:nil))

(e1:define symbol:secondary-table
  (e1:let ((secondary-table (string-hash:make)))
    ;; The secondary table "sees" itself as the primary table:
    (string-hash:set! secondary-table "symbol:table" secondary-table)
    ;;; This just takes up space for no reason when unexecing:
    ;; (string-hash:set! secondary-table "symbol:secondary-table" symbol:table)
    secondary-table))

(e1:define (symbol:drop-secondary!)
  (e1:define symbol:secondary-table 0))


;;; FIXME: this is even *conceptually* wrong
(e1:define (symbol:swap-tables!)
  (e1:let ((primary symbol:table)
           (secondary symbol:secondary-table))
    ;; FIXME: this is probably silly
    (fio:write "OK-A 100\n")
    (e1:define symbol:secondary-table primary)
    (fio:write "OK-A 200\n")
    (e1:define symbol:table secondary)
    (fio:write "OK-A 300\n")))
(e1:define (s)
  (symbol:swap-tables!))

(e1:define (symbol:in-table? symbol table)
  (e1:let ((name (symbol:symbol->string symbol)))
    (e1:and (string-hash:has? table name)
            (whatever:eq? (string-hash:get table name)
                          symbol))))

;;; The parameter has to be a symbol.
(e1:define (symbol:primary? symbol)
  (symbol:in-table? symbol symbol:table))
(e1:define (symbol:secondary? symbol)
  (symbol:in-table? symbol symbol:secondary-table))

(e1:define (symbol:intern-in! table string)
  (e1:if (string-hash:has? table string)
    (string-hash:get table string)
    (e1:let ((symbol (symbol:empty string)))
      (string-hash:set! table string symbol)
      symbol)))

;;; The parameter has to be a symbol.
(e1:define (symbol:primary->secondary symbol)
  (e1:if (symbol:primary? symbol)
    (symbol:intern-in! symbol:secondary-table
                       (symbol:symbol-get-name symbol))
    (e1:error "not a primary symbol")))
(e1:define (symbol:secondary->primary symbol)
  (e1:if (symbol:secondary? symbol)
    (symbol:intern-in! symbol:table
                       (symbol:symbol-get-name symbol))
    (e1:error "not a secondary symbol")))
(e1:define (symbol:primaries->secondaries ss)
  (e1:if (list:null? ss)
    list:nil
    (list:cons (symbol:primary->secondary (list:head ss))
               (symbol:primaries->secondaries (list:tail ss)))))

(e1:define (symbol:primary-expression->secondary-expression e)
  (e1:match e
    ((e0:expression-variable _ x)
     (e0:variable* (symbol:primary->secondary x)))
    ((e0:expression-value v)
     e)
    ((e0:expression-bundle _ items)
     (e0:bundle* (symbol:primary-expressions->secondary-expressions items)))
    ((e0:expression-primitive _ name actuals)
     (e0:primitive* name (symbol:primary-expressions->secondary-expressions actuals)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (e0:let* (symbol:primaries->secondaries bound-variables)
              (symbol:primary-expression->secondary-expression bound-expression)
              (symbol:primary-expression->secondary-expression body)))
    ((e0:expression-call _ procedure-name actuals)
     (e0:call* (symbol:primary->secondary procedure-name)
               (symbol:primary-expressions->secondary-expressions actuals)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e0:call-indirect* (symbol:primary-expression->secondary-expression procedure-expression)
                        (symbol:primary-expressions->secondary-expressions actuals)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e0:if-in* (symbol:primary-expression->secondary-expression discriminand)
                values ;; FIXME: should symbols, if any, be converted?
                (symbol:primary-expression->secondary-expression then-branch)
                (symbol:primary-expression->secondary-expression else-branch)))
    ((e0:expression-fork _ procedure-name actuals)
     (e0:fork* (symbol:primary->secondary procedure-name)
               (symbol:primary-expressions->secondary-expressions actuals)))
    ((e0:expression-join _ future)
     (e0:join* (symbol:primary-expression->secondary-expression future)))))
(e1:define (symbol:primary-expressions->secondary-expressions ee)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (symbol:primary-expression->secondary-expression (list:head ee))
               (symbol:primary-expressions->secondary-expressions (list:tail ee)))))

(e1:define-macro (e1:define-secondary-non-procedure s-name . s-forms)
  (e1:let ((secondary-name (sexpression:fresh-symbol)))
    (fio:write "Defining the SECONDARY non-procedure " (se s-name) "...\n")
    `(e1:let ((,secondary-name (symbol:primary->secondary (e1:value ,s-name))))
       (symbol:symbol-set-global-valid! ,secondary-name #t)
       (symbol:symbol-set-global! ,secondary-name
                                  (e0:unbundle (repl:macroexpand-transform-and-execute
                                                '(e1:begin ,@s-forms)))))))

(e1:define-macro (e1:define-secondary-procedure s-header . s-body-forms)
  (e1:let ((secondary-name (sexpression:fresh-symbol))
           (parameters-name (sexpression:fresh-symbol)))
    (fio:write "Defining the SECONDARY procedure " (se (sexpression:car s-header)) "...\n")
    `(e1:let ((,secondary-name (symbol:primary->secondary (e1:value ,(sexpression:car s-header))))
              (,parameters-name (symbol:primaries->secondaries (sexpression:eject-symbols ',(sexpression:cdr s-header)))))
       (symbol:symbol-set-formals! ,secondary-name ,parameters-name)
       (symbol:symbol-set-procedure-body!
           ,secondary-name
           (symbol:primary-expression->secondary-expression (repl:macroexpand-and-transform '(e1:begin ,@s-body-forms)))))))

(e1:define-macro (e1:define-secondary s-header . forms)
  `(,(e1:if (sexpression:cons? s-header)
       'e1:define-secondary-procedure
       'e1:define-secondary-non-procedure)
    ,s-header
    ,@forms))

(e1:define (symbol:fresh-secondary)
  (symbol:primary->secondary (symbol:fresh)))

;; ;;; NO: this cannot work, because unexec:unexec-table makes a closure
;; ;;; and names it in the primary table; then it passes the closure name
;; ;;; to unexec:unexec-table-procedure; in practice the primary symbol
;; ;;; table will be dumped as well.
;; (e1:define-macro (e1:unexec-secondary filename . forms)
;;   `(e1:unexec-table ,filename symbol:secondary-table ,@forms))

;;; This is horrible.  It doesn't support nonlocals and doesn't use
;;; gensym (I'd need a secondary gensym).  FIXME: fix it.
(e1:define-macro (e1:unexec-secondary file-name . forms)
  `(e1:begin
     ;; (e1:define-secondary (__blah__) ,@forms)
     ;; (e1:unexec-table ,file-name symbol:secondary-table (__blah__))
     (unexec:unexec-table-procedure ,file-name
                                    symbol:secondary-table
                                    (symbol:primary-expression->secondary-expression
                                     (repl:macroexpand-and-transform
                                      '(e1:begin ,@forms))))))

(e1:define-macro (e1:when-guile . stuff)
  `(e1:bundle))
(e1:define-macro (e1:unless-guile . stuff)
  `(e1:toplevel ,@stuff))

(e1:define-macro (e1:toplevel-secondary . stuff)
  (e1:let* ((stuff-as-primary-expression
             (repl:macroexpand-and-transform `(e1:begin ,@stuff)))
            (stuff-as-secondary-expression
             (symbol:primary-expression->secondary-expression stuff-as-primary-expression)))
    (sexpression:inject-expression stuff-as-secondary-expression)))

(e1:define-macro (e1:secondary . stuff)
  `(e1:toplevel-secondary ,@stuff))

(e1:define-macro (e1:trivial-define-macro-secondary name body-form) ;; only one body form
  `(symbol:symbol-set-macro-body! (symbol:primary->secondary (e1:value ,name))
                                  (symbol:primary-sexpression->secondary-sexpression ',body-form)))
(e1:define (symbol:primary-sexpression->secondary-sexpression x) x) ;; FIXME: do it for real
;; FIXME: use e1:trivial-define-macro-secondary instead of e1:trivial-define-macro in the beginning of epsilon1.scm (to be moved to core.e)

(e1:define-macro (d . stuff)
  `(e1:define-secondary ,@stuff))
(e1:define-macro (dd . stuff)
  `(debug:macroexpand '(e1:define-secondary ,@stuff)))

(e1:define x 42)
(e1:define (x a) 43)
(e1:define (g)
  (d x 42)
  (d (x a) 43))
