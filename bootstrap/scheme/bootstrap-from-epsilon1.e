;;;;; This is -*- epsilon -*-.
;;;;; Bootstrapping code: epsilon0 implemented in epsilon1.

;;;;; Copyright (C) 2015 Luca Saiu

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


;;; FIXME: this file can probably be simplified a lot.


;; ;;;;; Scratch
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (e1:define (t)
;;   (e1:let* ((input-port (input-port:readline-input-port))
;;             (backtrackable-input-port
;;              (backtrackable-port:input-port->backtrackable-port input-port
;;                                                                 (option:option-none))))
;;     (fio:write "sexp")
;;     (e1:let loop ((sexp (reader:read-bp backtrackable-input-port)))
;;       (e1:unless (sexpression:eof? sexp)
;;         (e1:let ((sexp (secondary:primary-sexp->secondary-sexp sexp)))
;;         (fio:write "You wrote: >" (se sexp) "<\n")
;;         (fio:write "sexp")
;;         (loop (reader:read-bp backtrackable-input-port)))))))

;; (e1:define (t2)
;;   (secondary:load "/home/luca/repos/epsilon/bootstrap/scheme/test.e"))


;;;;; Symbols as epsilon1 records.  FIXME: move this to epsilon1.[scm,e]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-record symbol:symbol
  name
  is-global
  global-value
  formals
  body
  macro
  macro-procedure-name
  primitive-descriptor
  compiled-procedure
  compiled-procedure-data
  user-defined)

(e1:define (symbol:intern-in-table table name)
  (e1:if (string-hash:has? table name)
    (string-hash:get table name)
    (e1:let* ((name (vector:shallow-clone name))
              (s (symbol:symbol #:name                    name
                                #:is-global               #f
                                #:global-value            127
                                #:formals                 0
                                #:body                    0
                                #:macro                   0
                                #:macro-procedure-name    0
                                #:primitive-descriptor    0
                                #:compiled-procedure      0
                                #:compiled-procedure-data 0
                                #:user-defined            0)))
      (string-hash:set! table name s)
      s)))

;;; FIXME: use symbol:intern-in-table for primary symbols as well.


;;;;; Secondary symbol table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define secondary:symbol-table
  (string-hash:make))

(e1:define (secondary:intern name)
  (symbol:intern-in-table secondary:symbol-table name))


;;;;; Symbol flipping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following assumes that the object bound to parameters named "symbol"
;;; actually be well-formed symbols, in a given symbol table or not.

(e1:define (secondary:symbol-table-has? symbol-table symbol)
  (e1:let* ((name (symbol:symbol-get-name symbol)))
    (e1:and (string-hash:has? symbol-table name)
            (whatever:eq? symbol
                          (string-hash:get symbol-table name)))))

(e1:define (secondary:primary-symbol? symbol)
  (secondary:symbol-table-has? symbol:table symbol))
(e1:define (secondary:secondary-symbol? symbol)
  (secondary:symbol-table-has? secondary:symbol-table symbol))

(e1:define (secondary:primary-symbol->secondary-symbol symbol)
  (e1:require (secondary:primary-symbol? symbol))
  (secondary:intern (symbol:symbol-get-name symbol)))

(e1:define (secondary:secondary-symbol->primary-symbol symbol)
  (e1:require (secondary:secondary-symbol? symbol))
  (symbol:intern (symbol:symbol-get-name symbol)))


;;;;; Secondary state environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (secondary:global-define! name value)
  (e1:require (secondary:secondary-symbol? name))
  (fio:write "Defining global " (sy name) " as " (i value) "\n")
  (symbol:symbol-set-is-global! name #t)
  (symbol:symbol-set-global-value! name value))

(e1:define (secondary:procedure-define! name formals body)
  (e1:require (secondary:secondary-symbol? name))
  (fio:write "Defining procedure " (sy name) " of [")
  (e1:dolist (f formals)
    (fio:write (sy f) " "))
  (fio:write "] as " (e body) "\n")
  (symbol:symbol-set-formals! name formals)
  (symbol:symbol-set-body! name body))

(e1:define (secondary:macro-define! name definition)
  ;;(fio:write "Checking before defining macro " (sy name) " as " (se definition) "...\n")
  (e1:require (secondary:secondary-symbol? name))
  (fio:write "Defining macro " (sy name) " as " (se definition) "\n")
  (symbol:symbol-set-macro-procedure-name! name 0)
  (symbol:symbol-set-macro! name definition))

;;; FIXME: Do I really want this?  The idea is perverse and possibly confusing,
;;; but I think it simplifies bootstrap.
;;; From the point of view of the secondary symbol table the primary symbol table
;;; is the secondary one, and the secondary table is not visible -- therefore I
;;; intentionally make it invalid as a table.
(secondary:global-define! (secondary:primary-symbol->secondary-symbol (e1:value symbol:table))
                          secondary:symbol-table)
(secondary:global-define! (secondary:primary-symbol->secondary-symbol (e1:value secondary:symbol-table))
                          424242)

;;; FIXME: this is perverse, but it will probably help.
;;; The primary symbol arguments is bound to each macro argument actual s-list; it's
;;; convenient for the primary and secondary symbols to be the same, so that I can
;;; use the same macro definitions for either stage.  Therefore the arguments symbol is
;;; both primary and secondary.
(string-hash:set! secondary:symbol-table
                  "arguments"
                  (e1:value arguments))


;;;;; Symbol writing taking into account the secondary table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This functionality is mostly useful for debugging the bootstrap process, but
;;; I think I should keep some version of it indefinitely.  It's highly desirable
;;; to be warned about secondary symbols creeping in by mistake.

(e1:define (printer:write-primary-symbol port symbol)
  (io:write-string port (symbol:symbol->string symbol)))

(e1:define (printer:write-secondary-symbol port symbol)
  (fio:write-to port (c 27) "[0m" (c 27) "[36m")
  (printer:write-primary-symbol port symbol)
  (fio:write-to port (c 27) "[0m"))

;;; This is a redefinition.
(e1:define (printer:write-symbol port symbol)
  (e1:cond ((secondary:primary-symbol? symbol)
            (printer:write-primary-symbol port symbol))
           ((secondary:secondary-symbol? symbol)
            (printer:write-secondary-symbol port symbol))
           (else
            (fio:write "About the value ")
            (e1:primitive io:write-value (io:standard-output) symbol)
            (fio:write ":\n")
            (e1:error "symbol isn't primary or secondary"))))


;;;;; S-expression flipping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This only covers the s-expressions which are directly read from
;;; the input, flipping every contained symbol.  epsilon0 expression
;;; macroexpanded from secondary s-expression will already only
;;; contain secondary symbols, by construction.

(e1:define (secondary:primary-sexp->secondary-sexp sexp)
  (e1:cond ((e1:or (sexpression:null? sexp)
                   (sexpression:boolean? sexp)
                   (sexpression:character? sexp)
                   (sexpression:fixnum? sexp)
                   (sexpression:fixed-point? sexp)
                   (sexpression:string? sexp)
                   (sexpression:eof? sexp))
            sexp)
           ((sexpression:symbol? sexp)
            (e1:let* ((primary-name (sexpression:eject-symbol sexp))
                      (secondary-name (secondary:primary-symbol->secondary-symbol primary-name)))
              (sexpression:inject-symbol secondary-name)))
           ((sexpression:cons? sexp)
            (sexpression:cons (secondary:primary-sexp->secondary-sexp (sexpression:car sexp))
                              (secondary:primary-sexp->secondary-sexp (sexpression:cdr sexp))))
           (else
            (fio:write "secondary:primary-sexp->secondary-sexp: unsupported case " (se sexp) "\n")
            (e1:error "secondary:primary-sexp->secondary-sexp"))))
(e1:define (secondary:primary-sexps->secondary-sexps sexps)
  (e1:require (e1:or (sexpression:null? sexps)
                     (sexpression:cons? sexps)))
  (secondary:primary-sexp->secondary-sexp sexps))


;;;;; Secondary load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: this and e1:load should probably be factored.

(e1:define (secondary:load file-name)
  (e1:let* ((f (io:open-file file-name io:read-mode))
            (p (input-port:file->input-port f))
            (bp (backtrackable-port:input-port->backtrackable-port
                    p
                    (option:option-some file-name))))
    (secondary:load-helper file-name bp)))
(e1:define (secondary:load-helper file-name bp)
  (e1:let* ((unflipped-s (reader:read-bp bp))
            (s (secondary:primary-sexp->secondary-sexp unflipped-s)))
    (e1:unless (sexpression:eof-object? s)
      (fio:write "  secondary:load: just read " (se s) ".  Evaluating it...\n")
      #;(e1:let ((e (repl:macroexpand-and-transform s)))
        (fio:write "  Evaluating " (e e) "\n")
        (e0:eval-ee e))
      ;;(repl:macroexpand-transform-and-execute s)
      (bootstrap:eval-sexpr s)
      (fio:write "  secondary:load: evaluated with success.\n\n")
      (secondary:load-helper file-name bp))))


;;;;; epsilon1 form interpreter, working on secondary s-expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (bootstrap:eval-sexpr sexp)
  (e1:cond ((e1:not (sexpression:cons? sexp))
            (fio:write "Evaluating the toplevel non-cons " (se sexp) "\n")
            (bootstrap:eval-non-definition-sexpr sexp))
           ((e1:not (sexpression:symbol? (sexpression:car sexp)))
            (fio:write "The toplevel cons " (se sexp) " has a non-symbol car\n")
            ;;(bootstrap:eval-non-definition-sexpr sexp)
            (e1:error "ill-formed epsilon0 toplevel"))
           (else
            (fio:write "Evaluating the toplevel form " (se sexp) "\n")
            (bootstrap:eval-scons-with-symbol-scar sexp))))

(e1:define (bootstrap:eval-scons-with-symbol-scar sexp)
  (e1:let ((car-symbol (sexpression:eject-symbol (sexpression:car sexp)))
           (s-cdr (sexpression:cdr sexp)))
    (e1:require (secondary:secondary-symbol? car-symbol))
    ;; When car-symbol is a macro name its definition overrides the default
    ;; in the second e1:if branch here.
    (e1:if (symbol:symbol-get-macro car-symbol)
      (e1:begin
        (fio:write "Evaluating toplevel macro call\n")
        (bootstrap:eval-non-definition-sexpr sexp))
      (e1:case (secondary:secondary-symbol->primary-symbol car-symbol)
        ((e1:define)
         (fio:write "Evaluating procedure definition\n")
         (bootstrap:eval-definition-sexpr s-cdr))
        ((e1:trivial-define-macro)
         (fio:write "Evaluating macro definition\n")
         (bootstrap:eval-macro-definition-sexpr s-cdr))
        ((e1:toplevel)
         (fio:write "Evaluating non-definition\n")
         (bootstrap:eval-non-definition-sexprs s-cdr))
        (else
         (fio:write "Evaluating non-definition without an explicit e1:toplevel\n")
         (bootstrap:eval-non-definition-sexpr sexp))))))

(e1:define (bootstrap:eval-non-definition-sexprs sexps)
  (e1:cond ((sexpression:null? sexps)
            (e1:bundle))
           ((sexpression:null? (sexpression:cdr sexps))
            (e1:require (fixnum:= 1 (sexpression:length sexprs)))
            (bootstrap:eval-non-definition-sexpr (sexpression:car sexps)))
           (else
            ;;(bootstrap:eval-non-definition-sexpr (sexpression:car sexps))
            ;;(bootstrap:eval-non-definition-sexprs (sexpression:cdr sexps))
            (e1:error "ill-formed non-definition sexprs"))))

(e1:define (bootstrap:eval-non-definition-sexpr sexp)
  (fio:write "Evaluating non-definition " (se sexp) "\n")
  (e1:let ((e (repl:macroexpand-and-transform sexp)))
    (fio:write "Evaluating " (e e) "\n")
    (e1:let* ((e-results (e0:eval-ee e))
              (res (e1:if (fixnum:> (list:length e-results) 0)
                     (list:head e-results)
                     (secondary:primary-symbol->secondary-symbol (e1:value no-results)))))
      (e1:primitive io:write-value (io:standard-output) res)
      (fio:write "\n")
      res)))


(e1:define (bootstrap:eval-definition-sexpr sexp)
  (e1:cond ((e1:not (sexpression:list? sexp))
            (e1:error "definition is not an s-list"))
           ((sexpression:null? sexp)
            (e1:error "definition is '()"))
           ((e1:and (e1:not (sexpression:symbol? (sexpression:car sexp)))
                    (e1:not (e1:and (sexpression:cons? (sexpression:car sexp))
                                    (sexpression:symbol? (sexpression:caar sexp)))))
            (e1:error "ill-formed definition"))
           ((sexpression:symbol? (sexpression:car sexp))
            (bootstrap:eval-global-definition-sexpr (sexpression:eject-symbol (sexpression:car sexp))
                                                    (sexpression:cdr sexp)))
           (else
            (bootstrap:eval-procedure-definition-sexpr (sexpression:eject-symbol (sexpression:caar sexp))
                                                       (sexpression:cdar sexp)
                                                       (sexpression:cdr sexp)))))

(e1:define (bootstrap:eval-global-definition-sexpr name sexps)
  (e1:when (e1:or (e1:not (sexpression:list? sexps))
                  (fixnum:<> (sexpression:length sexps) 1))
    (e1:error "ill-formed non-procedure definition"))
  (e1:let* ((result (bootstrap:eval-non-definition-sexpr (sexpression:car sexps))))
    (secondary:global-define! name result)))

(e1:define (bootstrap:eval-procedure-definition-sexpr name formals-sexps body-sexps)
  (e1:when (e1:or (e1:not (sexpression:list? formals-sexps))
                  (e1:not (sexpression:symbol-list? formals-sexps)))
    (e1:error "ill-formed procedure definition formals"))
  (e1:when (e1:or (e1:not (sexpression:list? body-sexps))
                  (fixnum:<> (sexpression:length body-sexps) 1))
    (e1:error "ill-formed procedure definition body"))
  (e1:let* ((formals (sexpression:eject-symbols formals-sexps))
            ;;; FIXME: this doesn't support transforms!!!
            (body (e1:macroexpand-sequence-into-expression body-sexps)))
    (fio:write "defining " (sy name) " of [omitted] as " (e body) "\n")
    (secondary:procedure-define! name formals body)))

(e1:define (bootstrap:eval-macro-definition-sexpr s-cdr)
  (e1:unless (sexpression:list? s-cdr)
    (e1:error "trivial-macro form not a list"))
  (e1:unless (sexpression:symbol? (sexpression:car s-cdr))
    (e1:error "trivial-macro first subform not a symbol"))
  (e1:unless (fixnum:= (sexpression:length s-cdr) 2)
    (e1:error "trivial-macro form not of the right length"))
  (e1:let* ((name (sexpression:eject-symbol (sexpression:car s-cdr)))
            (body (sexpression:cadr s-cdr)))
    (secondary:macro-define! name body)))


;;;;; Trivial macros defined with powerful macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is useful for loading epsilon1.scm from epsilon1.
(e1:define-macro (e1:trivial-define-macro variable-name body)
  `(e1:begin
     (symbol:symbol-set-macro-procedure-name! (e1:value ,variable-name) 0)
     (symbol:symbol-set-macro! (e1:value ,variable-name) ',body)))

;; ;;; FIXME: is this ever useful?
;; (e1:trivial-define-macro e1:define-macro
;;   (e1:destructuring-bind ((macro-name . formals) . body-forms) arguments
;;     (e0:let () (string:write "QQQ Defining the macro ")
;;       (e0:let () (string:write (symbol:symbol->string (sexpression:eject-symbol (sexpression:caar arguments))))
;;         (e0:let () (string:write "...\n")
;;           (sexpression:quasiquote
;;            (e0:let () (state:macro-set! (e0:value ,macro-name)
;;                                         (sexpression:quote (e1:destructuring-bind ,formals arguments ,@body-forms)))
;;              ;;(e0:value ,macro-name)
;;              (e0:bundle)))))))) ;; don't return anything

(e1:define-macro (when-guile . forms)
  `(e1:bundle))

(e1:define-macro (unless-guile . forms)
  `(e1:bundle ,@forms))


;;;;; Secondary primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For every primitive with a primary name define a secondary name as well.
(e1:dolist (pair (string-hash:string-hash->salist symbol:table))
  (e1:let ((name (cons:get-cdr pair)))
    (e1:when (state:primitive? name)
      (fio:write "Adding a secondary primitive descriptor for " (sy name) "...\n")
      (e1:let ((descriptor (symbol:symbol-get-primitive-descriptor name))
               (secondary-name (secondary:primary-symbol->secondary-symbol name)))
        (symbol:symbol-set-primitive-descriptor! secondary-name descriptor)))))


;;;;; Secondary definitions with a convenient syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (secondary:define header . body)
  (e1:let ((header (secondary:primary-sexp->secondary-sexp header))
           (body (secondary:primary-sexps->secondary-sexps body)))
    `(e1:define ,header ,@body)))

;; (e1:define-macro (secondary:trivial-define-macro name body)
;;   (e1:let ((name (secondary:primary-sexp->secondary-sexp name))
;;            (body (secondary:primary-sexp->secondary-sexp body)))
;;     `(e1:trivial-define-macro ,name ,body)))

(e1:define-macro (secondary:define-macro header . body)
  (e1:let ((header (secondary:primary-sexps->secondary-sexps header))
           (body (secondary:primary-sexps->secondary-sexps body)))
    `(e1:define-macro ,header ,@body)))

;;; Make a primary macro visible as a secondary macro.  The exported macro
;;; will refer primary globals and procedures.
(e1:define-macro (secondary:export-macro name)
  (e1:let* ((secondary-name (secondary:primary-sexp->secondary-sexp name)))
    (fio:write "Exporting the macro " (se name) " as " (se secondary-name) "...\n")
    `(secondary:macro-define! (e1:value , secondary-name)
                              (symbol:symbol-get-macro (e1:value ,name)))))

;; (secondary:define-macro (mmm x) x)
;; (secondary:define (a1 x y) x)
;; (secondary:define (a2 x y) y)
;; (secondary:define a3 345)
;; (e1:define-macro (secondary:define header . body)
;;   (e1:if (sexpression:symbol? header)
;;     `(secondary:define-non-procedure ,header ,@body)
;;     `(secondary:define-procedure ,header ,@body)))

;;;(e1:define-non-procedure-procedure name-as-symbol form-sequence-as-sexpression)

;; (e1:define-macro (secondary:define-procedure name-and-formals . body)
;;   (e1:let ((name-and-formals (secondary:primary-sexps->secondary-sexps name-and-formals))
;;            (body (secondary:primary-sexps->secondary-sexps body)))
;;     (body (e1:macroexpand-sequence-into-expression body-sexps)))
;; )


;;;;; Secondary essential macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(e1:define (e0:primitive* name actuals)
;  (e0:expression-primitive (e0:fresh-handle) name actuals))

;; (symbol:symbol-set-macro!
;;    (secondary:primary-symbol->secondary-symbol (e1:value e0:primitive))
;;    (secondary:primary-sexp->secondary-sexp
;;     '
;;(secondary:define-macro (e0:value v) v)
(secondary:export-macro e0:value)
(secondary:export-macro e0:variable)
(secondary:export-macro e0:bundle)
(secondary:export-macro e0:if-in)
(secondary:export-macro e0:let)
(secondary:export-macro e0:call)
(secondary:export-macro e0:call-indirect)
(secondary:export-macro e0:primitive)
(secondary:export-macro e0:fork)
(secondary:export-macro e0:join)

(secondary:export-macro e1:toplevel)
(secondary:export-macro when-guile)
(secondary:export-macro unless-guile)


;;;;; Bootstrap and unexec into unexec-repl-bootstrapped.u
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(secondary:load (string:append configuration:abs_top_srcdir "/bootstrap/scheme/core.e"))
(secondary:load (string:append configuration:abs_top_srcdir "/bootstrap/scheme/epsilon1.scm"))
(secondary:load (string:append configuration:abs_top_srcdir "/bootstrap/scheme/unexec.e"))
(secondary:load (string:append configuration:abs_top_builddir "/bootstrap/scheme/configuration.e"))
(secondary:load (string:append configuration:abs_top_srcdir "/bootstrap/scheme/bootstrap-from-epsilon1-secondary.e"))
