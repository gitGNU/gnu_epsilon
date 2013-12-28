;;;;; -*- Scheme -*-, plus something close enough for Emacs
;;;;; Conversion utilities between Guile s-expressions and whatevers

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Written by Luca Saiu
;;;;; Updated in 2013 by Luca Saiu

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


;;; Guile 1.8 does not come with when and unless predefined:
(define-macro (when condition . body-forms)
  `(if ,condition (begin ,@body-forms)))
(define-macro (unless condition . body-forms)
  `(when (not ,condition) ,@body-forms))


;;;;;; String, symbol, list and s-expression conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-unspecified-value (if #f #t))

(define (guile-symbol->symbol s)
  (symbol:intern (guile-string->string (symbol->string s))))
(define (guile-symbol->string s)
  (guile-string->string (symbol->string s)))
(define (guile-string->string s)
  (let* ((length (string-length s))
         (buffer (e0:primitive buffer:make (guile-sexpression->whatever (1+ length)))))
    (e0:primitive buffer:set! buffer (e0:value 0) (guile-sexpression->whatever length))
    (do ((i 1 (1+ i)))
        ((> i length))
      (e0:primitive buffer:set! buffer (guile-sexpression->whatever i) (guile-sexpression->whatever (string-ref s (1- i)))))
    buffer))
(define (string->guile-string string)
  (let* ((length (whatever->guile-fixnum (e0:primitive buffer:get string (e0:value 0))))
         (result (make-string length)))
    (do ((i 0 (1+ i)))
        ((= i length) result)
      (string-set! result
                   i
                   (integer->char (whatever->guile-fixnum (e0:primitive buffer:get string (guile-sexpression->whatever (1+ i)))))))))
(define (symbol->guile-symbol symbol)
  ;; We have to print the symbol, so it should better have a name.
  ;; Intern it now, in case it wasn't already interned:
  (unless (whatever->guile-boolean (symbol:interned? symbol))
    (symbol:intern-uninterned! symbol))
  (let ((symbol-name (symbol:symbol->string symbol)))
    (if (whatever->guile-boolean (whatever:zero? symbol-name))
        (error "symbol->guile-symbol: uninterned symbol")
        (string->symbol (string->guile-string symbol-name)))))
(define (list->reversed-guile-list-acc xs acc)
  (e0:if-in (list:null? xs) (1)
    acc
    (list->reversed-guile-list-acc (list:tail xs)
                                   (cons (list:head xs) acc))))
(define (list->reversed-guile-list xs)
  (list->reversed-guile-list-acc xs '()))
(define (list->guile-list xs)
  (reverse (list->reversed-guile-list xs)))
(define (symbols->guile-symbols xs)
  (map symbol->guile-symbol (list->guile-list xs)))

(define (guile-list->list xs element-transformer)
  (list:reverse (guile-list->reversed-list xs element-transformer)))
(define (guile-list->reversed-list xs element-transformer)
  (guile-list->reversed-list-acc xs element-transformer list:nil))
(define (guile-list->reversed-list-acc xs element-transformer acc)
  (if (null? xs)
      acc
      (guile-list->reversed-list-acc (cdr xs)
                                     element-transformer
                                     (list:cons (element-transformer (car xs))
                                                acc))))

(define (guile-sexpression->sexpression x)
  (cond ((null? x)
         sexpression:nil)
        ((boolean? x)
         (sexpression:make sexpression:boolean-tag (guile-sexpression->whatever x)))
        ((integer? x)
         (sexpression:make sexpression:fixnum-tag (guile-sexpression->whatever x)))
        ((char? x)
         (sexpression:make sexpression:character-tag (guile-sexpression->whatever x)))
        ((symbol? x)
         (sexpression:make sexpression:symbol-tag (guile-symbol->symbol x)))
        ((keyword? x)
         (sexpression:make sexpression:symbol-tag
                           (guile-symbol->symbol
                            (string->symbol (string-append "#:"
                                                           (symbol->string (keyword->symbol x)))))))
        ((string? x)
         (sexpression:make sexpression:string-tag (guile-string->string x)))
        ((pair? x)
         (sexpression:make sexpression:cons-tag (cons:make (guile-sexpression->sexpression (car x))
                                                           (guile-sexpression->sexpression (cdr x)))))
        (else
         (error "guile-sexpression->sexpression: unimplemented" x))))
(define (_ guile-sexpression) ;; just a convenient alias
  (guile-sexpression->sexpression guile-sexpression))
(define (sexpression->guile-sexpression x)
  (cond ((whatever->guile-boolean (sexpression:null? x))
         '())
        ((whatever->guile-boolean (sexpression:boolean? x))
         (whatever->guile-boolean (sexpression:eject-boolean x)))
        ((whatever->guile-boolean (sexpression:fixnum? x))
         (whatever->guile-fixnum (sexpression:eject-fixnum x)))
        ((whatever->guile-boolean (sexpression:character? x))
         (integer->char (whatever->guile-fixnum (sexpression:eject-character x))))
        ((whatever->guile-boolean (sexpression:symbol? x))
         (symbol->guile-symbol (sexpression:eject-symbol x)))
        ((whatever->guile-boolean (sexpression:string? x))
         (string->guile-string (sexpression:eject-string x)))
        ((whatever->guile-boolean (sexpression:cons? x))
         (cons (sexpression->guile-sexpression (sexpression:car x))
               (sexpression->guile-sexpression (sexpression:cdr x))))
        ((whatever->guile-boolean (sexpression:expression? x))
         (error "sexpression->guile-sexpression: unimplemented (expression)" x))
        (else
         (error "sexpression->guile-sexpression: unimplemented" x))))
(define (^ sexpression) ;; just a convenient alias
  (sexpression->guile-sexpression sexpression))

(use-modules (ice-9 format))

(define (hash-dump-sizes h)
  (let* ((buckets-as-list (list:tail (vector:vector->list (box:get h))))
         (buckets (list->guile-list buckets-as-list))
         (bucket-lengths (map-reversed list:length buckets))
         (bucket-no (whatever->guile-fixnum (hash:bucket-no h)))
         (sizes (map-reversed whatever->guile-fixnum bucket-lengths))
         (nonzero-sizes (filter-reversed (lambda (s) (> s 0)) sizes))
         (maximum-size (apply max (cons 0 nonzero-sizes)))
         (element-no (whatever->guile-fixnum (hash:element-no h))))
    (format #t "Element no:  ~5d\n" element-no)
    (format #t "Buckets:     ~5d\n" bucket-no)
    (format #t "Fill factor:  ~,2f\n" (exact->inexact (/ element-no bucket-no)))
    (let* ((nonempty-bucket-no (length nonzero-sizes))
           (empty-bucket-no (- bucket-no nonempty-bucket-no)))
      (format #t " - empty:    ~5d  \n" empty-bucket-no)
      (format #t " - nonempty: ~5d  \n" nonempty-bucket-no)
      (let loop ((size 1))
        (when (<= size maximum-size)
          (let ((bucket-no-of-given-size
                 (length (filter-reversed (lambda (q) (= q size)) nonzero-sizes))))
            (unless (zero? bucket-no-of-given-size)
              (format #t "    - size ~2d: ~5d (~,2f%)\n"
                      size
                      bucket-no-of-given-size
                      (* (/ bucket-no-of-given-size nonempty-bucket-no) 100.0)))
            (loop (1+ size))))))))

(define (print-digit base digit)
  (character:write (fixnum:+ base digit)))
(define (print-fixnum-rest base i)
  (unless (whatever->guile-boolean (fixnum:zero? i))
    (print-fixnum-rest base (fixnum:/ i (e0:value 10)))
    (print-digit base
                 (fixnum:% i (e0:value 10)))))
(define (print-fixnum base i)
  (cond ((whatever->guile-boolean (fixnum:< i (e0:value 0)))
         (string:write (guile-string->string "-"))
         (print-fixnum base (fixnum:- (e0:value 0) i)))
        ((whatever->guile-boolean (fixnum:zero? i))
         (print-digit base (e0:value 0)))
        (else
         (print-fixnum-rest base i))))
(define (print-subscript-handle h)
  (print-fixnum (guile-sexpression->whatever 8320) ;; The character '₀'
                h))
(define (print-expression e)
  (cond ((whatever->guile-boolean (e0:expression-variable? e))
         (e0:let (handle name) (e0:expression-variable-explode e)
           (begin
             (print-symbol name)
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-value? e))
         (e0:let (handle content) (e0:expression-value-explode e)
           (begin
             (print-value content)
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-bundle? e))
         (e0:let (handle items) (e0:expression-bundle-explode e)
           (begin
             (string:write (guile-string->string "[bundle "))
             (print-expressions items)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-primitive? e))
         (e0:let (handle name actuals) (e0:expression-primitive-explode e)
           (begin
             (string:write (guile-string->string "[primitive "))
             (string:write (symbol:symbol->string name))
             (string:write (guile-string->string " "))
             (print-expressions actuals)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-let? e))
         (e0:let (handle bound-variables bound-expression body) (e0:expression-let-explode e)
           (begin
             (string:write (guile-string->string "[let ["))
             (print-symbols bound-variables)
             (string:write (guile-string->string "] be "))
             (print-expression bound-expression)
             (string:write (guile-string->string " in "))
             (print-expression body)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-call? e))
         (e0:let (handle name actuals) (e0:expression-call-explode e)
           (begin
             (string:write (guile-string->string "[call "))
             (string:write (symbol:symbol->string name))
             (string:write (guile-string->string " "))
             (print-expressions actuals)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-call-indirect? e))
         (e0:let (handle procedure-expression actuals) (e0:expression-call-indirect-explode e)
           (begin
             (string:write (guile-string->string "[call-indirect "))
             (print-expressions (list:cons procedure-expression actuals))
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-if-in? e))
         (e0:let (handle discriminand values then-branch else-branch) (e0:expression-if-in-explode e)
           (begin
             (string:write (guile-string->string "[if "))
             (print-expression discriminand)
             (string:write (guile-string->string " "))
             (character:write (e0:value 8712)) ;; 8714:∊ 8712:∈
             (string:write (guile-string->string " {"))
             ;;(string:write (guile-string->string " ∈ {"))
             (print-values values)
             (string:write (guile-string->string "} then "))
             (print-expression then-branch)
             (string:write (guile-string->string " else "))
             (print-expression else-branch)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-fork? e))
         (e0:let (handle name actuals) (e0:expression-fork-explode e)
           (begin
             (string:write (guile-string->string "[fork "))
             (string:write (symbol:symbol->string name))
             (string:write (guile-string->string " "))
             (print-expressions actuals)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (e0:expression-join? e))
         (e0:let (handle future) (e0:expression-join-explode e)
           (begin
             (string:write (guile-string->string "[join "))
             (print-expression future)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ;;; We also support a couple of extensions, just to make debugging easier:
        ((whatever->guile-boolean (whatever:eq? (buffer:get e (e0:value 0)) (e0:value 10)))
         (e0:let (handle formals body) (e0:bundle (buffer:get e (e0:value 1)) (buffer:get e (e0:value 2)) (buffer:get e (e0:value 3)))
           (begin
             (string:write (guile-string->string "[lambda ["))
             (print-symbols formals)
             (string:write (guile-string->string "] "))
             (print-expression body)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (whatever:eq? (buffer:get e (e0:value 0)) (e0:value 11)))
         (e0:let (handle closure-expression actuals) (e0:bundle (buffer:get e (e0:value 1)) (buffer:get e (e0:value 2)) (buffer:get e (e0:value 3)))
           (begin
             (string:write (guile-string->string "[call-closure "))
             (print-expression closure-expression)
             (string:write (guile-string->string " "))
             (print-expressions actuals)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (whatever:eq? (buffer:get e (e0:value 0)) (e0:value 12)))
         (e0:let (handle continuation-name body) (e0:bundle (buffer:get e (e0:value 1)) (buffer:get e (e0:value 2)) (buffer:get e (e0:value 3)))
           (begin
             (string:write (guile-string->string "[let/cc "))
             (print-symbol continuation-name)
             (string:write (guile-string->string " "))
             (print-expression body)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ((whatever->guile-boolean (whatever:eq? (buffer:get e (e0:value 0)) (e0:value 12)))
         (e0:let (handle body) (e0:bundle (buffer:get e (e0:value 1)) (buffer:get e (e0:value 2)))
           (begin
             (string:write (guile-string->string "[non-cps "))
             (print-expression body)
             (string:write (guile-string->string "]"))
             (print-subscript-handle handle))))
        ;;; If we arrived here, we really don't know how to print this case
        (else
         (e0:let (tag) (buffer:get e (e0:value 0))
           (e0:let (handle) (buffer:get e (e0:value 1))
             (begin
               (string:write (guile-string->string "[extension "))
               (print-value tag)
               (string:write (guile-string->string "]"))
               (print-subscript-handle handle)
               ))))))
(define (print-expressions es)
  (cond ((whatever->guile-boolean (list:null? es))
         the-unspecified-value)
        ((whatever->guile-boolean (list:null? (list:tail es)))
         (print-expression (list:head es)))
        (else
         (print-expression (list:head es))
         (string:write (guile-string->string " "))
         (print-expressions (list:tail es)))))
(define (print-guile-string guile-string)
  (string:write (guile-string->string guile-string)))
(define (print-symbol v)
  (symbol:intern-when-uninterned! v)
  (string:write (symbol:symbol->string v)))
(define (print-symbols vs)
  (cond ((whatever->guile-boolean (list:null? vs))
         the-unspecified-value)
        ((whatever->guile-boolean (list:null? (list:tail vs)))
         (print-symbol (list:head vs)))
        (else
         (print-symbol (list:head vs))
         (string:write (guile-string->string " "))
         (print-symbols (list:tail vs)))))
(define (print-value v)
;;  (print-fixnum (e0:value 48) ;; The character '0'
;;                v))
  (e0:primitive io:write-value (io:standard-output) v))
(define (print-values vs)
  (cond ((whatever->guile-boolean (list:null? vs))
         the-unspecified-value)
        ((whatever->guile-boolean (list:null? (list:tail vs)))
         (print-value (list:head vs)))
        (else
         (print-value (list:head vs))
         (string:write (guile-string->string ", "))
         (print-values (list:tail vs)))))
(define (print-expression_ e)
  (print-expression e)
  (character:write character:newline)
  the-unspecified-value)

(define (print-environment alist)
  (print-guile-string "{")
  (print-environment-bindings alist)
  (print-guile-string "}"))
(define (print-environment_ alist)
  (print-environment alist)
  (print-guile-string "\n"))
(define (print-environment-bindings alist)
  (cond ((whatever->guile-boolean (list:null? alist))
         the-unspecified-value)
        ((whatever->guile-boolean (list:null? (list:tail alist)))
         (print-symbol (cons:car (list:head alist)))
         (print-guile-string " ")
         (character:write (e0:value 8614)) ;; ↦
         (print-guile-string " ")
         (print-value (cons:cdr (list:head alist))))
        (else
         (print-symbol (cons:car (list:head alist)))
         (print-guile-string " ")
         (character:write (e0:value 8614)) ;; ↦
         (print-guile-string " ")
         (print-value (cons:cdr (list:head alist)))
         (print-guile-string ", ")
         (print-environment-bindings (list:tail alist)))))
