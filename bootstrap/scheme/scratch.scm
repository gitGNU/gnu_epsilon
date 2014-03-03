;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2012, 2013 Luca Saiu
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


;;(load "cps.scm")
;;(load "compiler.scm")
;;(load "pattern-matching.e")

(e1:define-macro (n-times-in-parallel n . forms)
  (e1:let* ((formal-name (sexpression:fresh-symbol))
            (list-name (sexpression:fresh-symbol)))
    `(e1:let* ((,list-name (list:map (e1:lambda (,formal-name)
                                       (e1:future ,@forms))
                                     (list:iota ,n))))
       (vector:list->vector (list:map (e1:lambda (,formal-name)
                                        (e1:join ,formal-name))
                                      ,list-name)))))

(define (fibo-guile n) (if (< n 2) n (+ (fibo-guile (- n 2)) (fibo-guile (- n 1)))))
(e1:define (fibo n) (e0:if-in n (0 1) n (fixnum:+ (fibo (fixnum:- n (e0:value 2))) (fibo (fixnum:1- n)))))
(e1:define (fibo-a t n) (e0:if-in n (0 1) n (fixnum:+ (fibo-a t (fixnum:- n (e0:value 2))) (fibo-a t (fixnum:1- n)))))
(e1:define (fibop n) (e0:if-in n (0 1) n (e0:primitive fixnum:+ (fibop (e0:primitive fixnum:- n (e0:value 2))) (fibop (e0:primitive fixnum:1- n)))))

(e1:define (fact n) (e0:if-in n (0) (e0:value 1) (fixnum:* n (fact (fixnum:1- n)))))
(e1:define (fact-acc n a) (e0:if-in n (0) a (fact-acc (fixnum:1- n) (fixnum:* n a))))
(e1:define (fact-acc-a t n a) (e0:if-in n (0) a (fact-acc-a t (fixnum:1- n) (fixnum:* n a))))
(e1:define (factp n) (e0:if-in n (0) (e0:value 1) (e0:primitive fixnum:* n (fact (e0:primitive fixnum:1- n)))))
(e1:define (return-constant) 42)
(e1:define (return-local) (e0:let (a) 25 a))
(e1:define a-global 47)
(e1:define (return-global) a-global)
;;(e1:define (f n) (fibo n))
(e1:define (f n) (fixnum:+ n 500 500))

(e1:define (zerotozero) (e0:bundle))
(e1:define (onetozero x) (e1:let* ((q x)) (e0:bundle)))
(e1:define (zerotoone) 42)
(e1:define (zerototwo) (e0:bundle 10 20))
(e1:define (twotozero x y) (e1:let* ((q1 x) (q2 y)) (e0:bundle)))
(e1:define (simplertwotozero x y) (e0:let (q1) x (e0:let (q2) y (e0:bundle))))
(e1:define (average a b) (fixnum:/ (fixnum:+ a b) 2))
(e1:define (call-indirect-1 f x) (e0:primitive fixnum:1+ (e0:call-indirect f x)))
(e1:define (call-indirect-2 f x) (e0:call-indirect f x))

;;(load "parallel-test.e")

;;(load "compiler.e")
;;(load "formatted-output.e")


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


;;;;; Readline input port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Make an input port reading from stdin with, readline line editing.
;;; FIXME: change the primitive to explicitly receive a prompt string;
;;; the readline C API supports itx.
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


;;;;; REPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (repl:repl)
  (e1:let* ((input-port (input-port:readline-input-port))
            (backtrackable-input-port
             (backtrackable-port:input-port->backtrackable-port input-port
                                                                (option:option-none))))
    (repl:repl-helper backtrackable-input-port)))
(e1:define (repl:repl-helper bp)
  (e1:if (backtrackable-port:eof? bp)
    (e1:bundle)
    (e1:let ((sexpression (reader:read bp)))
      (fio:write "[You wrote: ")
      (sexpression:write sexpression)
      (fio:write "]\n")
      (e1:if (sexpression:eof-object? sexpression)
        (fio:write "Goodbye.\n")
        (e1:begin
          ;;(fio:write "Macroexpanding, transforming and interpreting... ")
          (e1:let ((expression (repl:macroexpand-and-transform sexpression)))
            ;;(fio:write "The macroexpand and tranformation part is done.\n")
            (e1:let ((results (e0:eval-ee expression)))
              ;;(fio:write "There are " (i (list:length results)) " results\n")
              (e1:dolist (result results)
                (e1:primitive io:write-value (io:standard-output) result)
                (fio:write "\n"))
              (repl:repl-helper bp))))))))


;;;;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

