;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Lazy, potentially infinite streams

;;;;; Copyright (C) 2016 Luca Saiu

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


;;; FIXME: add a general comment.  This enables functional programming over
;;; infinite streams, in the sytle of Haskell.


;;;;; Core stream definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A stream is a lazy-head lazy-spine list.  A stream cons is evaluated
;;; whenever it's checked for nullity; this however does not imply an evaluation
;;; of the stream head, nor of course of the stream tail which is another
;;; stream.

(e1:define stream:nil
  (promise:trivial list:nil))

;;; Make a stream whose head is already a promise, and which therefore does not
;;; need to be delayed again.  Calling stream:head on the result will force the
;;; promise given here.
(e1:define-macro (stream:cons-promise head-promise tail)
  `(promise:delay (list:cons ,head-promise
                             ,tail)))

(e1:define (stream:cons-promise-procedure head-promise tail)
  (promise:trivial (list:cons head-promise
                              tail)))

(e1:define-macro (stream:cons head tail)
  `(stream:cons-promise (promise:delay ,head)
                        ,tail))

(e1:define (stream:null? s)
  (list:null? (promise:force s)))

(e1:define (stream:nnull? s)
  (e1:not (stream:null? s)))

;;; Return the promise computing the value of the stream head, without forcing
;;; it.  Forcing the result of this later at some point at some point will
;;; automatically force the stream head.
(e1:define (stream:head-promise s)
  (list:head (promise:force s)))

;;; Return the value of the stream head, forcing its evaluation.
(e1:define (stream:head s)
  (promise:force (stream:head-promise s)))

;;; Return the stream at the tail of the given stream, assumed to be non-null.
(e1:define (stream:tail s)
  (list:tail (promise:force s)))


;;;;; Stream readiness checks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (stream:ready? s)
  (promise:ready? s))

(e1:define (stream:nready? s)
  (e1:not (stream:ready? s)))

;;; Return the number of ready conses from the beginning in the given stream,
;;; without forcing any spine cons or any element.  This loops if the stream's
;;; ready portion has infinite length.
(e1:define (stream:ready-length s)
  (stream:ready-length-acc s 0))
(e1:define (stream:ready-length-acc s acc)
  (e1:cond ((stream:nready? s)
            acc)
           ((stream:null? s)
            acc)
           (else
            (stream:ready-length-acc (stream:tail s)
                                     (fixnum:1+ acc)))))

;;; Return non-#f iff the given stream has a completely ready spine.  Loop if
;;; the ready part has infinite length.
(e1:define (stream:ready-spine? s)
  (e1:cond ((stream:nready? s)
            #f)
           ((stream:null? s)
            #t)
           (else
            (stream:ready-spine? (stream:tail s)))))

;;; Return non-#f iff the given stream has a completely ready spine and ready
;;; elements.  Loop if the ready part has infinite length.
(e1:define (stream:fully-ready? s)
  (e1:cond ((stream:nready? s)
            #f)
           ((stream:null? s)
            #t)
           ((promise:ready? (stream:head-promise s))
            (stream:fully-ready? (stream:tail s)))
           (else
            #f)))


;;;;; Generic stream procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the n-th element of the given stream (0-based), forcing the spine up
;;; to its cons but not forcing any other element.
(e1:define (stream:nth stream n)
  (e0:if-in n (0)
    (stream:head stream)
    (stream:nth (stream:tail stream) (fixnum:1- n))))

;;; Return a promise which, when forced, will yield the n-the element of the
;;; given stream (0-based) forcing the spine up to the relevant cons but without
;;; forcing any element.  The returned promise doesn't force anything
;;; until itself is forced.
(e1:define (stream:nth-promise stream n)
  (promise:delay (stream:nth stream n)))

;;; Return a promise which will yield the n-the element of the given stream
;;; (0-based).  This procedure, when called, immediately forces the spine up to
;;; the relevant cons but without forcing any element.
(e1:define (stream:nth-promise-eager stream n)
  (e0:if-in n (0)
    (stream:head-promise stream)
    (stream:nth-promise-eager (stream:tail stream) (fixnum:1- n))))

;;; Return the length of the given stream, forcing its spine but not its
;;; elements.  This loops if the stream is infinite.
(e1:define (stream:length s)
  (stream:length-acc s 0))
(e1:define (stream:length-acc s acc)
  (e1:if (stream:null? s)
    acc
    (stream:length-acc (stream:tail s) (fixnum:1+ acc))))

;;; This (when forced) only forces the spine of s1.
(e1:define-lazy (stream:append s1 s2)
  (e1:if (stream:null? s1)
    s2
    (stream:cons-promise-procedure (stream:head-promise s1)
                                   (stream:append (stream:tail s1) s2))))

;;; Convenient variadic syntax for stream:append.
(variadic:define-right-deep stream:append stream:append stream:nil)

(e1:define (stream:reverse s)
  (stream:reverse-into s stream:nil))
(e1:define-lazy (stream:reverse-into s acc)
  (stream:reverse-into-eager s acc))
(e1:define (stream:reverse-into-eager s acc)
  (e1:if (stream:null? s)
    acc
    ;; Here it's useless to make anything but the elements lazy: when reversing
    ;; s we are already forcing its spine, so also the stream we are buidling is
    ;; already known.
    (stream:reverse-into-eager (stream:tail s)
                               (stream:cons-promise-procedure (stream:head-promise s)
                                                              acc))))

;;; Given a stram s and a natural fixnum n, return a new stream holding its
;;; first n elements.  This (when forced) forces (part of) the spine of s, but
;;; not its elements.
(e1:define-lazy (stream:take s n)
  (e1:cond ((fixnum:= n 0)
            stream:nil)
           ((stream:null? s)
            (e1:error "stream:take: stream too short"))
           (else
            (stream:cons-promise (stream:head-promise s)
                                 (stream:take (stream:tail s)
                                              (fixnum:1- n))))))

;;; Given a stram s and a natural fixnum n return a stream which, when forced,
;;; will return the rest of the given stream after n conses.  No element is
;;; forced, but forcing the results causes the forcing of the first n conses of
;;; s.
(e1:define-lazy (stream:drop s n)
  (e1:cond ((fixnum:= n 0)
            s)
           ((stream:null? s)
            (e1:error "stream:drop: stream too short"))
           (else
            (stream:drop (stream:tail s) (fixnum:1- n)))))

;;; Return the last element of the given stream.  This forces the whole
;;; spine of the stream, but not its elements except for the last one.
(e1:define (stream:last s)
  (e1:if (stream:null? s)
    (e1:error "stream:last: null stream")
    (stream:last-nonnull s)))
(e1:define (stream:last-nonnull s)
  (e1:let ((tail (stream:tail s)))
    (e1:if (stream:null? tail)
      (stream:head s)
      (stream:last-nonnull tail))))


;;;;; Conversion between streams and lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (stream:->list s)
  (e1:if (stream:null? s)
    list:nil
    (list:cons (stream:head s)
               (stream:->list (stream:tail s)))))
(e1:define (stream:stream->list s) (stream:->list s)) ;; An alias.

(e1:define (stream:list-> xs)
  (e1:if (list:null? xs)
    stream:nil
    (stream:cons-promise-procedure (promise:trivial (list:head xs))
                                   (stream:list-> (list:tail xs)))))
(e1:define (stream:list->stream xs) (stream:list-> xs)) ;; An alias.

;;; Given a stream return a list of its first n elements.
(e1:define (stream:take-list s n)
  (e1:cond ((fixnum:= n 0)
            list:nil)
           ((stream:null? s)
            (e1:error "stream:take-list: stream too short"))
           (else
            (list:cons (stream:head s)
                       (stream:take-list (stream:tail s)
                                         (fixnum:1- n))))))


;;;;; Literal, so to speak, syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a stream holding the given elements, in order.  No element is
;;; evaluated, and no part of the stream is built, until the result is forced.
(e1:define-macro (stream:stream . elements)
  (e1:if (sexpression:null? elements)
    'stream:nil
    `(stream:cons ,(sexpression:car elements)
                  (stream:stream ,@(sexpression:cdr elements)))))


;;;;; Fixnum stream constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return an infinite fixnum streams holding n, n+1, n+2, ...
(e1:define (stream:from n)
  (stream:cons n (stream:from (fixnum:1+ n))))

;;; Return an infinite fixnum streams holding n, n+step, n+2step, ...
(e1:define (stream:from-step n step)
  (stream:cons n (stream:from-step (fixnum:+ n step) step)))

;;; Return a fixnum stream holding [0, n-1].
(e1:define-lazy (stream:iota n)
  (stream:range 0 (fixnum:1- n)))

;;; Return a fixnum stream holding [a, b].
(e1:define-lazy (stream:range a b)
  (e1:if (fixnum:> a b)
    stream:nil
    (stream:cons a (stream:range (fixnum:1+ a) b))))


;;;;; Stream higher-order procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-lazy (stream:map f s)
  (e1:if (stream:null? s)
    stream:nil
    (stream:cons (e1:call-closure f (stream:head s))
                 (stream:map f (stream:tail s)))))

(e1:define-lazy (stream:map2 f s1 s2)
  (e1:cond ((e1:and (stream:null? s1)
                    (stream:null? s2))
            stream:nil)
           ((e1:or (stream:null? s1)
                   (stream:null? s2))
            (e1:error "stream:map2: streams of different lengths"))
           (else
            ;;(fio:write "Calling f on " (i (stream:head s1)) " and " (i (stream:head s2)) "\n")
            (stream:cons-promise-procedure
               (promise:delay (e1:call-closure f (stream:head s1)
                                                 (stream:head s2)))
               (stream:map2 f (stream:tail s1) (stream:tail s2))))))

(e1:define-lazy (stream:filter p? s)
  (e1:cond ((stream:null? s)
            stream:nil)
           (bind (head (stream:head s)))
           ((e1:call-closure p? head)
            (stream:cons-promise (promise:trivial head)
                                 (stream:filter p? (stream:tail s))))
           (else
            (stream:filter p? (stream:tail s)))))

(e1:define (stream:for-all? p? s)
  (e1:cond ((stream:null? s)
            #t)
           ((e1:call-closure p? (stream:head s))
            (stream:for-all? p? (stream:tail s)))
           (else
            #f)))

(e1:define-lazy (stream:up-to p? s)
  (e1:cond ((stream:null? s)
            stream:nil)
           (bind (head (stream:head s)))
           ((e1:call-closure p? head)
            stream:nil)
           (else
            (stream:cons-promise (promise:trivial head)
                                 (stream:up-to p? (stream:tail s))))))


;;;;; Stream iteration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-macro (e1:dostream (variable stream . result-forms) . body-forms)
  (e1:let ((loop-name (sexpression:fresh-symbol-with-prefix "loop"))
           (stream-name (sexpression:fresh-symbol-with-prefix "stream")))
    `(e1:let ,loop-name ((,stream-name ,stream))
       (e1:if (stream:null? ,stream-name)
         (e1:begin
           ,@result-forms)
         (e1:let ((,variable (stream:head ,stream-name)))
           ,@body-forms
           (,loop-name (stream:tail ,stream-name)))))))


;;;;; Predefined fixnum streams
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define stream:zeros
  (stream:cons 0 stream:zeros))

(e1:define stream:ones
  (stream:cons 1 stream:ones))

(e1:define stream:naturals
  (stream:from 0))

(e1:define stream:fibonaccis
  (stream:cons 0
               (stream:cons 1
                            (stream:map2 (e1:lambda (x y) (fixnum:+ x y))
                                         stream:fibonaccis
                                         (stream:tail stream:fibonaccis)))))

(e1:define stream:primes
  (stream:cons
     2
     (stream:filter
        (e1:lambda (n)
          (stream:for-all? (e1:lambda (smaller)
                             (fixnum:% n smaller)) ;; non-zero remainder
                           (stream:up-to (e1:lambda (smaller)
                                           (fixnum:> (fixnum:* smaller smaller)
                                                     n))
                                         stream:primes)))
        (stream:from-step 3 2))))

(e1:define-lazy (stream:gaps s)
  (e1:let* ((first (stream:head s))
            (tail (stream:tail s))
            (second (stream:head tail)))
    (stream:cons (fixnum:- second first)
                 (stream:gaps tail))))

(e1:define stream:prime-gaps
  (stream:gaps stream:primes))


;;;;; Utility procedures, mostly for debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (stream:print-maxes s)
  (e1:let loop ((max (fixnum:1- (stream:head s)))
                (s s))
    (e1:let ((head (stream:head s)))
      (e1:if (fixnum:> head max)
        (e1:begin
          (fio:write (i head) "\n")
          (loop head (stream:tail s)))
        (e1:begin
          #;(fio:write ".")
          (loop max (stream:tail s)))))))

;;; FIXME: this is mostly for debugging
(e1:define (stream:print-fixnums s)
  (e1:dostream (x s)
    (fio:write (i x) " "))
  (fio:write "\n"))
(e1:define (stream:print-newline-fixnums s)
  (e1:dostream (x s)
    (fio:write (i x) "\n")))


;;;;; Utility procedures, for debugging only  [FIXME: remove]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (ps s)
  (stream:print-fixnums s))
(e1:define (psn s)
  (stream:print-newline-fixnums s))
(e1:define (pl xs)
  (e1:dolist (x xs)
    (fio:write (i x) " "))
  (fio:write "\n"))
(e1:define (pln xs)
  (e1:dolist (x xs)
    (fio:write (i x) "\n")))
