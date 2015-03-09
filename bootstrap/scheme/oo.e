;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2014, 2015 Luca Saiu

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

(e1:define (fact n)
  (e1:if (fixnum:zero? n)
    1
    (fixnum:* n (fact (fixnum:1- n)))))

(e1:define (gauss n)
  (e1:if (fixnum:zero? n)
    0
    (fixnum:+ n (gauss (fixnum:1- n)))))

(e1:define (fibo n)
  (e1:if (fixnum:< n 2);;(e1:if-in n (0 1)
    n
    (fixnum:+ (fibo (fixnum:- n 2))
              (fibo (fixnum:1- n)))))

(e1:define (wait-till limit)
  (e1:let ((current (c64:get-jiffies)))
    ;;(fio:write "current: " (i current) ", limit: " (i limit) "\n")
    (e1:if (fixnum:< current limit)
      (wait-till limit)
      current)))

(e1:define (every limit)
  (e1:let ((now (wait-till limit)))
    (fio:write ".")
    (every (fixnum:+ now 50))))


;;;;; Assertions and requirements (FIXME: move)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (e1:assert-or-require name e . messages)
  `(e1:unless ,e
     (fio:write "Error: " (st ,name) " " (se ',e) " violated. " ,@messages "\n")
     (e1:error ,name)))

(e1:define-macro (e1:assert e . messages)
  `(e1:assert-or-require "assertion" ,e ,@messages))

(e1:define-macro (e1:require e . messages)
  `(e1:assert-or-require "requirement" ,e ,@messages))


;;;;; Object system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
 (e1:define-record oo:class
   name         ;; a symbol
   superclasses ;; a list of oo:class records
   slot-names)  ;; a list of symbols
 (e1:define-record oo:instance class parents slots))

(e1:define (oo:subclass-of? candidate-subclass candidate-superclass)
  (e1:or (whatever:eq? candidate-subclass candidate-superclass)
         (oo:any-subclass-of? (oo:class-get-superclasses candidate-subclass)
                              candidate-superclass)))
(e1:define (oo:any-subclass-of? candidate-subclasses candidate-superclass)
  (e1:cond ((list:null? candidate-subclasses)
            #f)
           ((oo:subclass-of? (list:head candidate-subclasses) candidate-superclass)
            #t)
           (else
            (oo:any-subclass-of? (list:tail candidate-subclasses) candidate-superclass))))

(e1:define (oo:classes->all-slots cs)
  (e1:if (list:null? cs)
    list:nil
    (set-as-list:union (oo:class->all-slots (list:head cs))
                       (oo:classes->all-slots (list:tail cs)))))
(e1:define (oo:class->all-slots c)
  (set-as-list:union (oo:class-get-slot-names c)
                     (oo:classes->all-slots (oo:class-get-superclasses c))))

(e1:define (ssexpression:append-ssymbol-string ssymbol string)
  (e1:let* ((ssymbol-as-string (symbol:symbol->string (sexpression:eject-symbol ssymbol)))
            (appended-string (string:append ssymbol-as-string string))
            (appended-symbol (symbol:string->symbol appended-string)))
    (sexpression:inject-symbol appended-symbol)))

(e1:define-macro (oo:export-accessors superclass subclass)
  (e1:let* ((superclass-object-name (symbol:append (sexpression:eject-symbol superclass)
                                                   (e1:value -class)))
            (slots (e1:if (state:global? superclass-object-name)
                     (oo:class->all-slots (state:global-get superclass-object-name))
                     list:nil)))
    `(e1:begin
       ,@(sexpression:map
          (e1:lambda (slot)
            `(e1:begin
               (e1:define (,(sexpression:append-symbols subclass '-get- slot) x)
                 (,(sexpression:append-symbols superclass '-get- slot)
                  (,(sexpression:append-symbols subclass '-> superclass) x)))
               (e1:define (,(sexpression:append-symbols subclass '-set- slot '!) x v)
                 (,(sexpression:append-symbols superclass '-set- slot '!)
                  (,(sexpression:append-symbols subclass '-> superclass) x)
                  v))))
          (sexpression:inject-symbols slots)))))

(e1:define-macro (e1:define-class class superclasses . slots)
  (e1:let ((c-class (sexpression:append-symbols class '-class))
           (c? (sexpression:append-symbols class '?))
           (exactly-c? (sexpression:append-symbols class '-exactly?))
           (c-parents (sexpression:append-symbols class '-parents))
           (c-slots (sexpression:append-symbols class '-slots))
           (make-c class))
    `(e1:begin
       (e1:define-record ,c-slots
         ,@slots)
       (e1:define ,c-class
         (oo:class (e1:value ,c-class)
                   (list:list ,@(sexpression:map (e1:lambda (c)
                                                   (sexpression:append-symbols c '-class))
                                                 superclasses))
                   (e1:value-list ,@slots)))
       ;; Define upcast operators.  FIXME: do it for real.  FIXME: do it for other ancestors.
       ,@(sexpression:map
          (e1:lambda (superclass)
            `(e1:define (,(sexpression:append-symbols superclass '-> class) x)
               "..."))
          superclasses)
       ;; Define superclass slot accessors.  Right overrides left,
       ;; subclass overrides superclass.  Notice how macro calls to
       ;; oo:export-accessors have to be macroexpanded late, after the
       ;; class objects already exist.
       (repl:macroexpand-transform-and-execute
        '(e1:begin ,@(sexpression:map (e1:lambda (superclass)
                                        `(oo:export-accessors ,superclass ,class))
                                      superclasses)))
       ;; Define local slot accessors.  Of course this can override the previous definitions.
       ,@(sexpression:map (e1:lambda (slot)
                            `(e1:begin
                               (e1:define (,(sexpression:append-symbols class '-get- slot) x)
                                 (,(sexpression:append-symbols c-slots '-get- slot)
                                  (oo:instance-get-slots x)))
                               (e1:define (,(sexpression:append-symbols class '-set- slot '!) x v)
                                 (,(sexpression:append-symbols c-slots '-set- slot '!)
                                  (oo:instance-get-slots x)
                                  v))))
                          slots)
       (e1:define (,make-c ,@superclasses ,@slots)
         ,@(sexpression:map (e1:lambda (c)
                              `(e1:require (,(sexpression:append-symbols c '?) ,c)))
                            superclasses)
         (oo:instance ,c-class
                      (e1:value-list ,@(sexpression:map (e1:lambda (o) `(oo:instance-get-slots ,o))
                                                        superclasses))
                      (,c-slots ,@slots)))
       (e1:define (,exactly-c? o)
         (whatever:eq? (oo:instance-get-class o) ,c-class))
       (e1:define (,c? o)
         (oo:subclass-of? (oo:instance-get-class o) ,c-class))
)))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
  (e1:define-class living () species)
  (e1:define-class animal (living))

  (e1:define-class person (animal) name surname)
  (e1:define-class hacker (person) email)
  (e1:define-class stupid (person) iq)
  (e1:define-class stupid-hacker (stupid hacker))


  (e1:define-class plant (living) name)
  (e1:define-class tree (plant))
  (e1:define-class oak (tree))
  (e1:define the-oak (oak (tree (plant (living "oak") "the oak in the garden"))))

  (e1:define p (person (animal (living "human")) "John" "Doe"))
  (e1:define h (hacker p "john@doe.net"))
  (e1:define sh (stupid-hacker (stupid p 50) h))

  )
