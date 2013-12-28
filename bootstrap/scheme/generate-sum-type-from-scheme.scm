#!/home/luca/usr/bin/guile --debug
This is actually -*- Scheme -*-, not epsilon.
!#
;;;;; Process this file with automake to produce Makefile.in -*-Makefile-*-

;;;;; This file is part of GNU epsilon

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
;;(load "/home/luca/.guile")

;;; We use this to generate sum type definitions for epsilon0 from
;;; Scheme, without relying on advanced features of epsilon1.  Since
;;; the thing is only used at an early stage when bootstrapping, we
;;; consider acceptable to just insert the output of this code in the
;;; bootstrapping sequence.

(define initial-tag 0);;1000)

(define (range a b)
  (define (range-acc a b acc)
    (if (> a b)
        acc
        (range-acc a (1- b) (cons b acc))))
  (range-acc a b '()))

(define (iterate procedure list)
  (if (null? list)
      #t
      (begin
        (procedure (car list))
        (iterate procedure (cdr list)))))

(define (whatever-literal integer)
  (define (make-is how-many)
    (let loop ((result '())
               (how-many how-many))
      (if (zero? how-many)
          result
          (loop (cons 'i result)
                (1- how-many)))))
  (define fixnum:biggest-named-literal-value 128)
  (cond ((not (integer? integer))
         (error "whatever-literal: not an integer"))
        (else
         `(e0:value ,integer))))
        ;; ((< integer 0)
        ;;  (error "whatever-literal: negative paramter"))
        ;; ((<= integer fixnum:biggest-named-literal-value)
        ;;  (string->symbol (format #f "fixnum:~s" integer)))
        ;; (else
        ;;  (let ((quotient (quotient integer 10))
        ;;        (remainder (remainder integer 10)))
        ;;    (if (zero? remainder)
        ;;        `(fixnum:* ,(whatever-literal quotient)
        ;;                   ,(whatever-literal 10))
        ;;        `(fixnum:+ (fixnum:* ,(whatever-literal quotient)
        ;;                             ,(whatever-literal 10))
        ;;                   ,(whatever-literal remainder)))))))

(define (define-sum-type-case-dependent-part* namespace sum-name cases case-tags)
  (if (null? cases)
      '()
      `(,@(define-sum-type-case* namespace sum-name (caar cases) (cdar cases) (car case-tags))
        ,@(define-sum-type-case-dependent-part* namespace sum-name (cdr cases) (cdr case-tags)))))

(define (define-sum-type* namespace sum-name cases)
  `(,@(define-sum-type-case-independent-part* namespace sum-name)
    ,@(define-sum-type-case-dependent-part* namespace sum-name cases (range initial-tag (+ initial-tag (length cases))))));;(iota (length cases)))))

(define (define-sum-type-case-independent-part* namespace sum-name)
  `(;;(FIXME-INDEPENDENT ,sum-name)
    ))

;;(define (make-sum* sum-name constructor constructor-parameters)

(define (forms->nested-lets forms)
  (cond ((null? forms)
         (error "forms->nested-lets: empty list"))
        ((null? (cdr forms))
         (car forms))
        (else
         `(e0:let ()
                  ,(car forms)
            ,(forms->nested-lets (cdr forms))))))

(define (define-sum-type-case* namespace sum-name constructor constructor-parameters case-tag)
  (let* ((prefix namespace);;(symbol-append namespace ':))
         (constructor-tag-name (symbol-append prefix sum-name '- constructor '-tag))
         (checker-name (symbol-append prefix sum-name '- constructor '?))
         (maker-name (symbol-append prefix sum-name '- constructor));; '-make))
         (exploder-name (symbol-append prefix sum-name '- constructor '-explode))
         )
    `(;;'-------------------------------------------------------------BEGIN
      (e1:define ,constructor-tag-name ,(whatever-literal case-tag))
      ,@(if (null? constructor-parameters)
            ;;; Trivial case: we represent the object as an unboxed tag
            `((e1:define (,checker-name ,sum-name)
                (whatever:eq? ,sum-name ,(whatever-literal case-tag)))
              (e1:define (,maker-name ,@constructor-parameters)
                ,(whatever-literal case-tag)))
            ;;; Nontrivial case: boxed case
            `((e1:define (,checker-name ,sum-name)
                (e0:if-in (boxedness:definitely-unboxed? ,sum-name) (#f)
                  (whatever:eq? (e0:primitive buffer:get ,sum-name ,(whatever-literal 0))
                                ,(whatever-literal case-tag))
                  (e0:value #f)))
              (e1:define (,maker-name ,@constructor-parameters)
                ;;,(make-sum* sum-name constructor constructor-parameters))))
                (e0:let (,constructor)
                        (e0:primitive buffer:make ,(whatever-literal (1+ (length constructor-parameters))))
                  ,(let loop ((initialization-forms `((e0:primitive buffer:set! ,constructor ,(whatever-literal 0) ,(whatever-literal case-tag))))
                              (next-index 1)
                              (fields constructor-parameters))
                     (if (null? fields)
                         (forms->nested-lets `(,@initialization-forms ,constructor))
                         (loop `(,@initialization-forms
                                 (e0:primitive buffer:set! ,constructor ,(whatever-literal next-index) ,(car fields)))
                               (1+ next-index)
                               (cdr fields))))))
              ,@(let loop ((next-index 1)
                           (fields constructor-parameters)
                           (definitions '()))
                  (if (null? fields)
                      definitions
                      (loop (1+ next-index)
                            (cdr fields)
                            `(,@definitions
                              (e1:define (,(symbol-append prefix sum-name '- constructor '-get- (car fields)) ,constructor)
                                (e0:primitive buffer:get ,constructor ,(whatever-literal next-index)))
                              (e1:define (,(symbol-append prefix sum-name '- constructor '-set- (car fields) '!) ,constructor ,(car fields))
                                (e0:primitive buffer:set! ,constructor ,(whatever-literal next-index) ,(car fields)))))))))
      (e1:define (,exploder-name ,constructor)
        (e0:bundle ,@(map (lambda (field-name)
                            `(,(symbol-append prefix sum-name '- constructor '-get- field-name) ,constructor))
                          constructor-parameters)))
      ;;'-------------------------------------------------------------END
      )))

;;; Write generated forms, as separate elements instead of a single list, without pretty-printing:
(define (write-forms forms)
  (iterate (lambda (form)
             (write form)
             ;;(newline)
             )
           forms)
  (newline))

(define-macro (define-sum-type prefix-with-no-separator sum-name . cases)
  `(write-forms (define-sum-type* ',(symbol-append prefix-with-no-separator ':)
                                  ',sum-name
                                  ',cases)))

;; ------------------------

;(define-sum-type boo list (nil) (cons car cdr))
(define-sum-type e0 expression
  (variable handle name)
  (value handle content)
  (bundle handle items)
  (primitive handle name actuals)
  (let handle bound-variables bound-expression body)
  (call handle procedure-name actuals)
  (call-indirect handle procedure-expression actuals)
  (if-in handle discriminand values then-branch else-branch)
  (fork handle procedure-name actuals)
  (join handle future))
