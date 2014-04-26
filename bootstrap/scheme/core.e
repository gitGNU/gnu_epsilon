;;;;; This is -*- epsilon -*- (with very little Scheme).
;;;;; e0 global state, interpreter and macros defined in epsilon0 plus e1:define

;;;;; Copyright (C) 2012 Université Paris 13
;;;;; Copyright (C) 2013, 2014 Luca Saiu
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


;;;;; Utility procedures working on any data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (whatever:zero? a)
  (e0:primitive whatever:zero? a))

;;; The naming convention ("eq?" instead of "equal?") is a little
;;; misleading if we consider the Guile implementation where whatever
;;; objects are boxed, but that doesn't really matter:
(e1:define (whatever:eq? a b)
  (e0:primitive whatever:eq? a b))
(e1:define (whatever:neq? a b) ;; the opposite of equality
  (e0:primitive whatever:zero? (e0:primitive whatever:eq? a b)))

;;; Return the parameter, whatever it is:
(e1:define (whatever:identity x)
  x)


;;;;; The empty list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define empty-list:empty-list (e0:value 0))


;;;;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (boolean:not boolean)
  (whatever:zero? boolean))

(e1:define (boolean:canonicalize generalized-boolean)
  (e0:if-in generalized-boolean (#f)
    (e0:value #f)
    (e0:value #t)))

;;; These are procedures, not syntactic forms:
(e1:define (boolean:and2 a b)
  (e0:if-in a (#f)
    (e0:value #f)
    b))
(e1:define (boolean:or2 a b)
  (e0:if-in a (#f)
    b
    (e0:value #t)))


;;;;; Fixnums
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wrap primitives into reasonably-named procedures:
(e1:define (fixnum:zero? a)
  (e0:primitive whatever:zero? a))
(e1:define (fixnum:1+ a)
  (e0:primitive fixnum:1+ a))
(e1:define (fixnum:1- a)
  (e0:primitive fixnum:1- a))
(e1:define (fixnum:+ a b)
  (e0:primitive fixnum:+ a b))
(e1:define (fixnum:negate a)
  (e0:primitive fixnum:negate a))
(e1:define (fixnum:- a b)
  (e0:primitive fixnum:- a b))
(e1:define (fixnum:* a b)
  (e0:primitive fixnum:* a b))
(e1:define (fixnum:/ a b)
  (e0:primitive fixnum:/ a b))
(e1:define (fixnum:% a b)
  (e0:primitive fixnum:% a b))
(e1:define (fixnum:= a b)
  (e0:primitive whatever:eq? a b))
(e1:define (fixnum:<> a b)
  (e0:primitive whatever:zero? (e0:primitive whatever:eq? a b)))
(e1:define (fixnum:< a b)
  (e0:primitive fixnum:< a b))
(e1:define (fixnum:<= a b)
  (e0:primitive fixnum:<= a b))
(e1:define (fixnum:> a b)
  (e0:primitive fixnum:< b a))
(e1:define (fixnum:>= a b)
  (e0:primitive fixnum:<= b a))
(e1:define (fixnum:bitwise-not a)
  (e0:primitive fixnum:bitwise-not a))
(e1:define (fixnum:bitwise-and a b)
  (e0:primitive fixnum:bitwise-and a b))
(e1:define (fixnum:bitwise-or a b)
  (e0:primitive fixnum:bitwise-or a b))
(e1:define (fixnum:bitwise-xor a b)
  (e0:primitive fixnum:bitwise-xor a b))
(e1:define (fixnum:left-shift a b)
  (e0:primitive fixnum:left-shift a b))
(e1:define (fixnum:arithmetic-right-shift a b)
  (e0:primitive fixnum:arithmetic-right-shift a b))
(e1:define (fixnum:logic-right-shift a b)
  (e0:primitive fixnum:logic-right-shift a b))

(e1:define (fixnum:min a b)
  (e0:if-in (fixnum:< a b) (#f)
    b
    a))
(e1:define (fixnum:max a b)
  (e0:if-in (fixnum:< a b) (#f)
    a
    b))

(e1:define (fixnum:square a)
  (fixnum:* a a))
(e1:define (fixnum:half a) ;; with positive arguments, return the floor
  (fixnum:arithmetic-right-shift a (e0:value 1)))
(e1:define (fixnum:double a)
  (fixnum:left-shift a (e0:value 1)))

(e1:define (fixnum:odd? a)
  (fixnum:bitwise-and a (e0:value 1)))
(e1:define (fixnum:even? a)
  (boolean:not (fixnum:odd? a)))

(e1:define (fixnum:sign a)
  (e0:if-in a (0)
    (e0:value 0)
    (e0:if-in (fixnum:< a 0) (#f)
      (e0:value 1)
      (e0:value -1))))

;;; Useful for backends with no hardware multiplication. [FIXME: use it]
(e1:define (fixnum:non-primitive-* a b)
  (e0:if-in (fixnum:< b (e0:value 0)) (#f)
    (fixnum:non-primitive-*-non-negative-b-acc a b (e0:value 0))
    (fixnum:negate (fixnum:non-primitive-*-non-negative-b-acc a (fixnum:negate b) (e0:value 0)))))
(e1:define (fixnum:non-primitive-*-non-negative-b-acc a b acc)
  (e0:if-in b (0)
    acc
    (e0:if-in (fixnum:odd? b) (#f)
      (fixnum:non-primitive-*-non-negative-b-acc (fixnum:double a) (fixnum:half b) acc) ; a*2c = 2a*c
      (fixnum:non-primitive-*-non-negative-b-acc (fixnum:double a) (fixnum:half b) (fixnum:+ acc a))))) ; a*(2c+1) = (2a*c)+a

(e1:define (fixnum:** base exponent)
  (e0:if-in exponent (0)
    (e0:value 1)
    (e0:if-in (fixnum:odd? exponent) (#f)
      (fixnum:square (fixnum:** base (fixnum:half exponent)))
      (fixnum:* base (fixnum:** base (fixnum:1- exponent))))))

(e1:define (fixnum:mod n)
  (e0:if-in (fixnum:< n (e0:value 0)) (#f)
    n
    (fixnum:negate n)))

;;; This returns the *floor* of the decimal logarithm of the given
;;; fixnum.  Handy for obtaining the number of digits of a number,
;;; which is the successor of the result of this procedure.
(e1:define (fixnum:log10 n)
  (e0:if-in n (0)
    (e1:error (e0:value "range"))
    (fixnum:log10-helper n (e0:value 0))))
(e1:define (fixnum:log10-helper n acc)
  (e0:if-in (fixnum:< n (e0:value 10)) (#f)
    (fixnum:log10-helper (fixnum:/ n (e0:value 10))
                         (fixnum:1+ acc))
    acc))


;;;;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Define wrapper procedures for buffers, to make them a little more
;;;; convenient to use (but of course still unsafe)

(e1:define (buffer:make element-no)
  (e0:primitive buffer:make element-no))

(e1:define (buffer:destroy buffer)
  (e0:primitive buffer:destroy buffer))

(e1:define (buffer:get buffer index)
  (e0:primitive buffer:get buffer index))

(e1:define (buffer:set! buffer index new-element)
  (e0:primitive buffer:set! buffer index new-element))

(e1:define (buffer:initialize! buffer index new-element)
  (e0:primitive buffer:initialize! buffer index new-element))


;;;;; Boxedness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The minimum numeric value a word may have to be considered a
;;; potential pointer.  This is used in boxedness tests.  This should
;;; not be directly used; the following two helper procedures make
;;; code much easier to read.
(e1:define boxedness:minimum-pointer-value
  (e0:value 1024))

;;; Return non-false iff the given word may represent a pointer; if
;;; the result is false, then the parameter is definitely a
;;; non-pointer.
;;;
;;; [FIXME: here I could also check the least significatn bits, but
;;; that is word-length- and allocator-dependant]
(e1:define (boxedness:potentially-boxed? word)
  (fixnum:>= word boxedness:minimum-pointer-value))

;;; Return non-false iff the given word is definitely a non-pointer.
;;; If the result is non-false, then the parameter is definitely a
;;; non-pointer.
;;;
;;; [FIXME: here I could also check the least significatn bits, but
;;; that is word-length- and allocator-dependant]
(e1:define (boxedness:definitely-unboxed? word)
  (fixnum:< word boxedness:minimum-pointer-value))

;;; Wrap primitive looking up boxedness tags.  These *fail* on
;;; backends not representing boxedness tags, of course:
(e1:define (boxedness:fixnum? word)
  (e0:primitive whatever:atom? word))
(e1:define (boxedness:buffer? word)
  (e0:primitive whatever:buffer? word))
(e1:define (boxedness:thread? word) ;; FIXME: shall I remove this?
  (e0:primitive whatever:therad? word))
(e1:define (boxedness:buffer-length pointer)
  (e0:primitive buffer:length pointer))


;;;;; Conses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (cons:make car cdr)
  (e0:let (result) (buffer:make (e0:value 2))
    (e0:let () (buffer:set! result (e0:value 0) car)
      (e0:let () (buffer:set! result (e0:value 1) cdr)
        result))))

(e1:define (cons:get-car cons)
  (buffer:get cons (e0:value 0)))
(e1:define (cons:get-cdr cons)
  (buffer:get cons (e0:value 1)))

(e1:define (cons:set-car! cons new-car)
  (buffer:set! cons (e0:value 0) new-car))
(e1:define (cons:set-cdr! cons new-cdr)
  (buffer:set! cons (e0:value 1) new-cdr))

;;; Useful "traditional" aliases:
(e1:define (cons:cons car cdr)
  (cons:make car cdr))
(e1:define (cons:car cons)
  (cons:get-car cons))
(e1:define (cons:cdr cons)
  (cons:get-cdr cons))


;;;;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A list is either zero, or a cons whose cdr is a list.

;;; Useful "traditional" aliases:
(e1:define list:nil
  empty-list:empty-list)
(e1:define (list:cons element list)
  (cons:cons element list))
(e1:define (list:head list)
  (cons:car list))
(e1:define (list:tail list)
  (cons:cdr list))
(e1:define (list:null? list)
  (whatever:zero? list))

;;; Updaters:
(e1:define (list:set-head! list new-value)
  (cons:set-car! list new-value))
(e1:define (list:set-tail! list new-value)
  (cons:set-cdr! list new-value))

;;; Utility procedures:
(e1:define (list:length x)
  (list:length-acc x (e0:value 0)))
(e1:define (list:length-acc x acc)
  (e0:if-in x (0)
    acc
    (list:length-acc (list:tail x) (fixnum:1+ acc))))
(e1:define (list:list1 x)
  (list:cons x list:nil))
(e1:define (list:list2 x y)
  (list:cons x (list:cons y list:nil)))
(e1:define (list:list3 x y z)
  (list:cons x (list:cons y (list:cons z list:nil))))
(e1:define (list:singleton x) (list:list1 x)) ;; just an alias
(e1:define (list:append-reversed xs acc)
  (e0:if-in xs (0)
    acc
    (list:append-reversed (list:tail xs) (list:cons (list:head xs) acc))))
(e1:define (list:reverse xs)
  (list:append-reversed xs list:nil))
(e1:define (list:append2 xs ys)
  (list:append-reversed (list:reverse xs) ys))
(e1:define (list:flatten xs)
  (list:reverse (list:flatten-acc xs list:nil)))
(e1:define (list:flatten-acc xs acc)
  (e0:if-in xs (0)
    acc
    (list:flatten-acc (list:tail xs)
                      (list:append-reversed (list:head xs) acc))))
(e1:define (list:memq x xs)
  (e0:if-in xs (0)
    (e0:value #f)
    (e0:if-in (whatever:eq? x (list:head xs)) (#f)
      (list:memq x (list:tail xs))
      (e0:value #t))))
(e1:define (list:nth list n)
  (e0:if-in n (0)
    (list:head list)
    (list:nth (list:tail list) (fixnum:1- n))))
(e1:define (list:set-nth-head! list index value)
  (e0:if-in index (0)
    (list:set-head! list value)
    (list:set-nth-head! (list:tail list) (fixnum:1- index) value)))
(e1:define (list:set-nth-tail! list index value)
  (e0:if-in index (0)
    (list:set-tail! list value)
    (list:set-nth-tail! (list:tail list) (fixnum:1- index) value)))
(e1:define (list:first-elements n list)
  (list:first-elements-acc n list list:nil))
(e1:define (list:first-elements-acc n list acc)
  (e0:if-in n (0)
    (list:reverse acc)
    (list:first-elements-acc (fixnum:1- n) (list:tail list) (list:cons (list:head list) acc))))

;;; Return a copy of the first list without the given element or
;;; elements; only the first instance is reamoved, and the result may
;;; share structure with the first parameter.  The resulting list has
;;; unspecified order.  Elements are compared by identity.
(e1:define (list:without list element)
  (list:without-acc list element list:nil))
(e1:define (list:without-acc list element acc)
  (e0:if-in list (0)
    acc
    (e0:if-in (whatever:eq? (list:head list) element) (#f)
      (list:without-acc (list:tail list) element (list:cons (list:head list) acc))
      ;; We found the element:
      (list:append-reversed acc (list:tail list)))))
(e1:define (list:without-list list elements)
  (e0:if-in elements (0)
    list
    (list:without-list (list:without list (list:head elements))
                       (list:tail elements))))

;;; Return the index, 0-based, of the first element equal in the
;;; sense of whatever:eq?; fail (horribly) if there is no match.
(e1:define (list:index-of list element)
  (list:index-of-starting-from list element 0))
(e1:define (list:index-of-starting-from list element index)
  (e0:if-in (whatever:eq? (list:head list) element) (#f)
    (list:index-of-starting-from (list:tail list) element (fixnum:1+ index))
    index))

;;; Same as list:index-of, but return the index of the *last* occurrence:
(e1:define (list:last-index-of list element)
  (fixnum:- (fixnum:- (list:length list) (e0:value 1))
            (list:index-of (list:reverse list) element)))

;;; An alternative version of memq with a name more consistent with our conventions:
(e1:define (list:has? xs x)
  (list:memq x xs))

(e1:define (list:range from to)
  (list:range-acc from to list:nil))
(e1:define (list:range-acc from to acc)
  (e0:if-in (fixnum:> from to) (#f)
    (list:range-acc from (fixnum:1- to) (list:cons to acc))
    acc))
(e1:define (list:iota n)
  (list:range 0 (fixnum:1- n)))

(e1:define (list:n-times n object)
  (list:n-times-acc n object list:nil))
(e1:define (list:n-times-acc n object acc)
  (e0:if-in n (0)
    acc
    (list:n-times-acc (fixnum:1- n) object (list:cons object acc))))


;;;;; Alists with unboxed keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An empty alist:
(e1:define alist:nil
  list:nil)

;;; Return the given alist with a new binding prepended.  No bindings
;;; are removed in the result.
(e1:define (alist:bind alist key value)
  (list:cons (cons:make key value)
             alist))

;;; Given an alists, a list of keys, and a list of values of its same
;;; size, return an alist with the given keys bound to the
;;; corresponding given values.  The relative order of the new
;;; bindings is undefined.  The result may share structure with the
;;; given alist.
(e1:define (alist:bind-lists alist keys values)
  (e0:if-in keys (0)
    (e0:if-in values (0)
      alist
      (e1:error (e0:value "alist:bind-lists: more values then keys")))
    (e0:if-in values (0)
      (e1:error (e0:value "alist:bind-lists: more keys than values"))
      (alist:bind-lists (alist:bind alist
                                    (list:head keys)
                                    (list:head values))
                        (list:tail keys)
                        (list:tail values)))))
(e1:define (alist:bind-lists-unsafe alist keys values)
  (e0:if-in keys (0)
    (e0:if-in values (0)
      alist
      (e1:error (e0:value "alist:bind-lists: more values then keys")))
    (alist:bind-lists-unsafe (alist:bind alist
                                         (list:head keys)
                                         (list:head values))
                             (list:tail keys)
                             (list:tail values))))

;;; Return a copy of the the given alist with a new binding prepended,
;;; and the first previous binding (if any) for the same key removed.
;;; The result may share structure with the given alist.
(e1:define (alist:bind-unique alist key value)
  (list:cons (cons:make key value)
             (alist:unbind-one alist key)))

;;; Return (e0:value #t) if the given key is bound in the given alist,
;;; (e0:value #f) otherwise.
(e1:define (alist:has? alist key)
  (e0:if-in alist (0)
    (e0:value #f)
    (e0:let (first-pair) (list:head alist)
      (e0:if-in (whatever:eq? (cons:car first-pair) key) (#f)
        (alist:has? (list:tail alist) key)
        (e0:value #t)))))

;;; Return the given alist without the first binding for the given
;;; key, or a copy of the list in case of no match.  The result may
;;; share structure with the alist.
(e1:define (alist:unbind-one alist key)
  (e0:if-in alist (0)
    list:nil
    (e0:let (first-pair) (list:head alist)
      (e0:if-in (whatever:eq? (cons:car first-pair) key) (#f)
        (list:cons first-pair (alist:unbind-one (list:tail alist) key))
        (list:tail alist)))))

;;; Return the given alist without the first binding for the given
;;; key, or a copy of the list in case of no match.  The result
;;; may share structure with the alist.
(e1:define (alist:unbind-all alist key)
  (e0:if-in alist (0)
    list:nil
    (e0:let (first-pair) (list:head alist)
      (e0:if-in (whatever:eq? (cons:car first-pair) key) (#f)
        (list:cons first-pair (alist:unbind-one (list:tail alist) key))
        (alist:unbind-all (list:tail alist) key)))))

;;; Lookup the given key in the given alist, and fail in case of no
;;; match
(e1:define (alist:lookup alist key)
  (e0:if-in alist (0)
    (e1:errors (e0:value "alist:lookup: no match") key)
    (e0:let (first-pair) (list:head alist)
      (e0:if-in (whatever:eq? (cons:car first-pair) key) (0)
        (alist:lookup (list:tail alist) key)
        (cons:cdr first-pair)))))

;;; Given an alist return the list of its keys, in some unspecified order:
(e1:define (alist:keys alist)
  (alist:keys-acc alist list:nil))
(e1:define (alist:keys-acc alist acc)
  (e0:if-in alist (0)
    acc
    (alist:keys-acc (list:tail alist)
                    (list:cons (cons:car (list:head alist)) acc))))


;;;;; Vectors (with stored size, but no bound checks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A vector is a buffer whose first position is reserved for holding
;;; the number of its payload elements.
(e1:define (vector:make element-no)
  (e0:let (result) (buffer:make (fixnum:1+ element-no))
    (e0:let () (buffer:set! result (e0:value 0) element-no)
      result)))

(e1:define (vector:make-initialized element-no element-value)
  (e0:let (result) (vector:make element-no)
    (e0:let () (vector:fill-from-header! result (e0:value 1) element-no element-value)
      result)))

(e1:define (vector:fill-from-header! vector header-based-index element-no element-value)
  (e0:if-in (fixnum:> header-based-index element-no) (#f)
    (e0:let () (vector:set-from-header! vector header-based-index element-value)
      (vector:fill-from-header! vector (fixnum:1+ header-based-index) element-no element-value))
    (e0:bundle)))

(e1:define (vector:destroy vector)
  (buffer:destroy vector))

(e1:define (vector:length vector)
  (buffer:get vector (e0:value 0)))

(e1:define (vector:get-from-header vector header-based-index)
  (buffer:get vector header-based-index))
(e1:define (vector:set-from-header! vector header-based-index new-element)
  (buffer:set! vector header-based-index new-element))

(e1:define (vector:get vector index)
  (vector:get-from-header vector (fixnum:1+ index)))
(e1:define (vector:set! vector index new-element)
  (vector:set-from-header! vector (fixnum:1+ index) new-element))

;;; Are two vectors element-by-element eq? (we compare elements with eq?)
(e1:define (vector:equal-unboxed-elements? vector1 vector2)
  (e0:let (length1) (vector:length vector1)
    (e0:let (length2) (vector:length vector2)
      (e0:if-in (fixnum:= length1 length2) (#f)
        ;; vectors have different lengths
        (e0:value #f)
        (vector:equal-unboxed-elements-same-size? vector1 vector2 (e0:value 1) (fixnum:1+ length1))))))
(e1:define (vector:equal-unboxed-elements-same-size? vector1 vector2 from-header-based-index element-no-including-header)
  (e0:if-in (fixnum:< from-header-based-index element-no-including-header)
            (0)
    ;; We've checked all corresponding elements, and all pairs were eq?
    (e0:value #t)
    ;; We haven't checked all elements yet
    (e0:if-in (whatever:eq? (vector:get-from-header vector1 from-header-based-index)
                            (vector:get-from-header vector2 from-header-based-index))
              (0)
      ;; corresponding elements are not eq?
      (e0:value #f)
      ;; corresponging elements are eq: check the rest
      (vector:equal-unboxed-elements-same-size? vector1 vector2 (fixnum:1+ from-header-based-index) element-no-including-header))))

;;; Return a copy of the given vector, containing the same elements of
;;; the given one.  Of course elements are not deep-copied, so if they
;;; are boxed they will be shared by the copied vector.
(e1:define (vector:shallow-clone source)
  (e0:let (length) (vector:length source)
    (e0:let (target) (vector:make length)
      (e0:let () (vector:shallow-clone-from! target source (e0:value 1) length)
        target))))
(e1:define (vector:shallow-clone-from! target source header-based-index length)
  (e0:if-in (fixnum:> header-based-index length) (0)
    (e0:let () (vector:set-from-header! target header-based-index (vector:get-from-header source header-based-index))
      (vector:shallow-clone-from! target source (fixnum:1+ header-based-index) length))
    (e0:bundle)))


(e1:define (vector:append2 v1 v2)
  (e0:let (length1) (vector:length v1)
    (e0:let (length2) (vector:length v2)
      (e0:let (result) (vector:make (fixnum:+ length1 length2))
        (e0:let () (vector:blit-from-header result (e0:value 1) v1 (e0:value 1) length1)
          (e0:let () (vector:blit-from-header result (fixnum:1+ length1) v2 (e0:value 1) length2)
            result))))))

(e1:define (vector:blit-from-header target target-index-from-header source source-index-from-header word-no)
  (e0:if-in word-no (0)
    (e0:bundle)
    (e0:let (word) (buffer:get source source-index-from-header)
      (e0:let () (buffer:set! target target-index-from-header word)
        (vector:blit-from-header target (fixnum:1+ target-index-from-header) source (fixnum:1+ source-index-from-header) (fixnum:1- word-no))))))

(e1:define (vector:blit target target-index source source-index word-no)
  (vector:blit-from-header target (fixnum:1+ target-index) source (fixnum:1+ source-index) word-no))

(e1:define (vector:vector->list v)
  (e0:let (length) (vector:length v)
    (vector:vector->list-acc v length list:nil)))
(e1:define (vector:vector->list-acc vector index-from-header acc)
  (e0:if-in index-from-header (0) ;; don't copy the header element
    acc
    (vector:vector->list-acc vector
                             (fixnum:1- index-from-header)
                             (list:cons (vector:get-from-header vector index-from-header)
                                        acc))))

(e1:define (vector:list->vector list)
  (e0:let (vector) (vector:make (list:length list))
    (vector:list->vector-acc list (e0:value 1) vector)))
(e1:define (vector:list->vector-acc list index-from-header vector)
  (e0:if-in list (0)
    vector
    (e0:let () (vector:set-from-header! vector index-from-header (list:head list))
      (vector:list->vector-acc (list:tail list) (fixnum:1+ index-from-header) vector))))

;;; An empty vector may be convenient to use as a neural element for
;;; concatenation, or stuff like that:
(e1:define vector:empty (vector:make (e0:value 0)))


;;;;; Characters and Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; At a low level characters are just fixnums (Unicode code points),
;;; and strings are just character vectors.

(e1:define character:newline
  (e0:value 10))

;;; The eof value, returned as an invalid character by some read
;;; operations.
(e1:define io:eof
  (e0:value -1))

;; The name io:eof? would be too error-prone.
(e1:define (io:eof-object? character-or-eof)
  (whatever:eq? character-or-eof io:eof))

(e1:define (character:write character)
  (e0:primitive io:write-character (io:standard-output) character))
(e1:define (character:read character)
  (e0:primitive io:read-character (io:standard-output)))

(e1:define character:0-as-character
  (e0:value 48)) ;; #\0 in ASCII and all its supersets including Unicode
(e1:define (character:fixnum->character fixnum)
  (fixnum:+ fixnum character:0-as-character))
(e1:define (character:character->fixnum character)
  (fixnum:- character character:0-as-character))

(e1:define (string:write string)
  (io:write-string (io:standard-output) string))

(e1:define (string:length string)
  (vector:length string))

(e1:define (string:equal? s1 s2)
  (vector:equal-unboxed-elements? s1 s2))

(e1:define (string:append2 s1 s2)
  (vector:append2 s1 s2))

(e1:define (string:append3 s1 s2 s3)
  (vector:append2 s1 (vector:append2 s2 s3)))

(e1:define (string:get s i)
  (vector:get s i))
(e1:define (string:set! s i c)
  (vector:set! s i c))

;;; Of course an empty string looks like an empty vector:
(e1:define string:empty vector:empty)

(e1:define (string:character->string character)
  (vector:make-initialized (e0:value 1) character))

(e1:define (string:fixnum->string fixnum)
  (e0:if-in (fixnum:zero? fixnum) (#f)
    (e0:if-in (fixnum:< fixnum (e0:value 0)) (#f)
      (string:positive-fixnum->string fixnum)
      (string:append2 (e0:value "-")
                      (string:positive-fixnum->string (fixnum:negate fixnum))))
    (e0:value "0")))
(e1:define (fixnum:digit-no fixnum)
  (e0:if-in fixnum (0)
    (e0:value 1)
    (fixnum:1+ (fixnum:log10 fixnum))))
(e1:define (string:positive-fixnum->string fixnum)
  (e0:let (digit-no) (fixnum:digit-no fixnum)
    (e0:let (result) (vector:make digit-no)
      (e0:let () (string:fill-with-positive-fixnum-digits! result fixnum digit-no)
        result))))
(e1:define (string:fill-with-positive-fixnum-digits! string fixnum remaining-digit-no)
  (e0:if-in remaining-digit-no (0)
    (e0:bundle)
    (e0:let () (string:set! string
                            (fixnum:1- remaining-digit-no)
                            (character:fixnum->character (fixnum:% fixnum (e0:value 10))))
      (string:fill-with-positive-fixnum-digits! string (fixnum:/ fixnum (e0:value 10)) (fixnum:1- remaining-digit-no)))))

(e1:define (fixnum:write n)
  (string:write (string:fixnum->string n)))

;;; FIXME: move away or write similar procedures for the other base types
(e1:define (fixnum:print n)
  (e0:let () (fixnum:write n)
    (string:write (e0:value "\n"))))

(e1:define (boolean:write b)
  (e0:if-in b (#f)
    (string:write (e0:value "#f"))
    (string:write (e0:value "#t"))))


;;;;; Simple file support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: should I move this?

(e1:define (io:standard-input)
  (e0:primitive io:standard-input))
(e1:define (io:standard-output)
  (e0:primitive io:standard-output))
(e1:define (io:standard-error)
  (e0:primitive io:standard-error))

(e1:define io:read-mode (e0:value 0))
(e1:define io:write-mode (e0:value 1))

(e1:define (io:open-file file-name mode)
  (e0:primitive io:open-file file-name mode))
(e1:define (io:close-file file)
  (e0:primitive io:close-file file))

(e1:define (io:eof? file)
  (e0:primitive io:eof? file))

;;; Return -1 on failure
(e1:define (io:read-character file)
  (e0:primitive io:read-character file))

;;; Return 0 on failure
(e1:define (io:readline)
  (e0:primitive io:readline))

(e1:define (io:write-character file character)
  (e0:primitive io:write-character file character))

(e1:define (io:write-32-bit-big-endian file x)
  (e0:primitive io:write-32-bit-big-endian file x))
(e1:define (io:read-32-bit-big-endian file) ;; no error reporting.  FIXME: add second result
  (e0:primitive io:read-32-bit-big-endian file))

(e1:define (io:write-string file s)
  (io:write-string-from file s (e0:value 0) (string:length s)))
(e1:define (io:write-string-from file s from-index length)
  (e0:if-in (fixnum:= from-index length) (#f)
    (e0:let () (io:write-character file (string:get s from-index))
      (io:write-string-from file s (fixnum:1+ from-index) length))
    (e0:bundle)))
(e1:define (io:read-line file)
  (io:read-line-acc file list:nil))
(e1:define (io:read-line-acc file acc)
  (e0:let (c) (io:read-character file)
    (e0:if-in (boolean:or2 (whatever:eq? c character:newline)
                           (whatever:eq? c -1))
              (#f)
      (io:read-line-acc file (list:cons c acc))
      (vector:list->vector (list:reverse acc)))))

(e1:define (io:write-symbol file symbol)
  (io:write-string file (symbol:symbol->string symbol)))
(e1:define (io:write-fixnum file fixnum)
  (io:write-string file (string:fixnum->string fixnum)))
(e1:define (io:write-boolean file boolean)
  (io:write-string file (e0:if-in boolean (#f)
                          (e0:value "#f")
                          (e0:value "#t"))))


;;;;; System interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (unix:system command-string)
  (e0:primitive unix:system command-string))
(e1:define (unix:unlink file-name)
  (e0:primitive unix:unlink file-name))


;;;;; GC control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: move

(e1:define (gc:disable!)
  (e0:primitive gc:disable!))

(e1:define (gc:reenable!)
  (e0:primitive gc:reenable!))


;;;;; SAlists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An salist is an alist with string as keys --- or, more generally,
;;; with vectors of unboxed objects as keys.

(e1:define (salist:bind salist key value)
  (alist:bind salist key value)) ;; identical to the alist version

;;; Return (e0:value #t) if the given key is bound in the given salist,
;;; (e0:value #f) otherwise.
(e1:define (salist:has? salist key)
  (e0:if-in salist (0)
    (e0:value #f)
    (e0:let (first-pair) (list:head salist)
      (e0:if-in (string:equal? (cons:car first-pair) key) (#f)
        (salist:has? (list:tail salist) key)
        (e0:value #t)))))

(e1:define (salist:lookup salist key)
  (e0:if-in salist (0)
    (e1:errors (e0:value "salist:lookup: unbound key") key)
    (e0:let (first-pair) (list:head salist)
      (e0:if-in (string:equal? (cons:car first-pair) key) (#f)
        (salist:lookup (list:tail salist) key)
        (cons:cdr first-pair)))))

;;; Return the given salist without the first binding for the given
;;; key, or a copy of the list in case of no match.  The result may
;;; share structure with the salist.
(e1:define (salist:unbind-one salist key)
  (e0:if-in salist (0)
    list:nil
    (e0:let (first-pair) (list:head salist)
      (e0:if-in (string:equal? (cons:car first-pair) key) (#f)
        (list:cons first-pair (salist:unbind-one (list:tail salist) key))
        (list:tail salist)))))


;;;;; Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (box:make-uninitialized)
  (buffer:make (e0:value 1)))

(e1:define (box:make-initialized content)
  (e0:let (result)
    (box:make-uninitialized)
    (e0:let ()
      (buffer:initialize! result (e0:value 0) content)
      result)))

(e1:define (box:make content) ;; an alias
  (box:make-initialized content))

(e1:define (box:make-0-initialized)
  (box:make-initialized (e0:value 0)))

(e1:define (box:set! box new-value)
  (buffer:set! box (e0:value 0) new-value))

(e1:define (box:get box)
  (buffer:get box (e0:value 0)))

;;; Of course, this is only useful for mutable fixnum counters
(e1:define (box:get-and-bump! box)
  (e0:let (old-value) (box:get box)
    (e0:let ()
      (box:set! box (fixnum:1+ old-value))
      old-value)))
(e1:define (box:bump-and-get! box)
  (e0:let (new-value) (fixnum:1+ (box:get box))
    (e0:let ()
      (box:set! box new-value)
      new-value)))
(e1:define (box:bump! box)
  (box:bump-and-get! box))


;;;;; Hashes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support two kinds of hashes: unboxed-key hashes, and string-key
;;; hashes.  String-key hashes actually support any vector key whose
;;; elements are unboxed.  Since we want our hashes to represent maps
;;; rather than multimaps we can affort *not* to enforce any ordering
;;; in buckers.

;;; A hash is implemented as a box referring a vector, containing the
;;; element number as a fixnum in its first position, and then the
;;; buckets.  So starting from the header we find two fields: the
;;; element number, as in all vectors, equal to the number of buckets
;;; plus one; and, as the second element, the number of elements.
;;; Then there are buckets.

;;  We need the box indirection for maintaining the hash identity
;;  after resizing.
(e1:define hash:default-bucket-no (e0:value 31))

(e1:define (string-hash:make-given-bucket-no bucket-no)
  (hash:make-given-bucket-no bucket-no))
(e1:define (unboxed-hash:make-given-bucket-no bucket-no)
  (hash:make-given-bucket-no bucket-no))

(e1:define (string-hash:make)
  (hash:make))
(e1:define (unboxed-hash:make)
  (hash:make))

(e1:define (hash:make-bucket-vector bucket-no)
  (vector:make-initialized (fixnum:1+ bucket-no) alist:nil))
(e1:define (hash:make-given-bucket-no bucket-no)
  ;; Notice that alist:nil, which is to say 0, is also the correct
  ;; content of the second element counting from the header, since the
  ;; hash is empty.
  (box:make-initialized (hash:make-bucket-vector bucket-no)))

(e1:define (hash:make)
  (hash:make-given-bucket-no hash:default-bucket-no))

(e1:define (hash:bucket-no hash)
  (fixnum:1- (vector:length (box:get hash))))

(e1:define (hash:element-no hash)
  (vector:get-from-header (box:get hash) (e0:value 1)))

(e1:define (unboxed-hash:element-no hash)
  (hash:element-no hash))
(e1:define (string-hash:element-no hash)
  (hash:element-no hash))

;;; Given any integer, return a header-based bucket index:
(e1:define (hash:fix-bucket-index bucket-no bucket-index-no-modulo)
  ;; Our % implementation is currently based on the C version, so we
  ;; can't know whether the modulo of a negative number is negative...
  ;; This version at least works anywhere.
  (fixnum:% (fixnum:+ (fixnum:% bucket-index-no-modulo bucket-no)
                      bucket-no)
            bucket-no))

(e1:define (hash:get-bucket hash-vector bucket-index-no-modulo)
  (e0:let (bucket-no) (fixnum:1- (vector:length hash-vector))
    (vector:get-from-header hash-vector
                            (fixnum:+ (hash:fix-bucket-index bucket-no bucket-index-no-modulo)
                                      (e0:value 2)))))
(e1:define (hash:set-bucket! hash-vector bucket-index-no-modulo bucket)
  (e0:let (bucket-no) (fixnum:1- (vector:length hash-vector))
    (vector:set-from-header! hash-vector
                             (fixnum:+ (hash:fix-bucket-index bucket-no bucket-index-no-modulo)
                                       (e0:value 2))
                             bucket)))

;;; Our hash functions do not compute the modulo; it's computed externally.
(e1:define (unboxed-hash:hash whatever)
  whatever)
(e1:define (string-hash:hash string)
  (string-hash:hash-acc string (string:length string) (e0:value 0)))

;;; Only use the given number of bits in the string hash domain.  This
;;; is needed to make the result identical on both 64- and 32-bit
;;; machines; thanks to this our unexec dumps are architecture-neutral.
(e1:define hash:bit-no
  (e0:value 30))

;;; A bitmask with only the hash:bit-no least-significant-bits
;;; set at 1:
(e1:define string-hash:bitmask
  (fixnum:1- (fixnum:** (e0:value 2) hash:bit-no)))
(e1:define (string-hash:cut n)
  (fixnum:bitwise-and n string-hash:bitmask))

(e1:define (string-hash:hash-acc string i acc) ; i is header-based
  (e0:if-in i (0)
    acc
    (string-hash:hash-acc string
                          (fixnum:1- i)
                          (string-hash:cut
                            (fixnum:bitwise-xor acc
                                                (string-hash:cut
                                                   (fixnum:left-shift (vector:get-from-header string i)
                                                                      (fixnum:% i hash:bit-no))))))))

;; (e1:define (string-hash:hash-from-header string header-based-index element-no acc)
;;   (e0:if-in (fixnum:> header-based-index element-no) (0)
;;     (string-hash:hash-from-header string
;;                                   (fixnum:1+ header-based-index)
;;                                   element-no
;;                                   (fixnum:bitwise-xor acc
;;                                                       (fixnum:left-shift (vector:get-from-header string
;;                                                                                                  header-based-index)
;;                                                                          (fixnum:1- header-based-index))))
;;     acc))

(e1:define (unboxed-hash:has? hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (bucket) (hash:get-bucket hash-vector (unboxed-hash:hash key))
      (alist:has? bucket key))))
(e1:define (unboxed-hash:get hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (unboxed-hash:hash key)
      (e0:let (bucket) (hash:get-bucket hash-vector hash-result)
        (alist:lookup bucket key)))))
(e1:define (unboxed-hash:set-without-resizing! hash key value)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (unboxed-hash:hash key)
      (e0:let (old-bucket) (hash:get-bucket hash-vector hash-result)
        (e0:if-in (alist:has? old-bucket key) (0)
          ;; Adding a new binding
          (e0:let (new-bucket) (alist:bind old-bucket key value)
            (e0:let () (hash:set-bucket! hash-vector hash-result new-bucket)
              (e0:let (element-no) (vector:get-from-header hash-vector (e0:value 1))
                (vector:set-from-header! hash-vector (e0:value 1) (fixnum:1+ element-no)))))
          ;; Replacing a binding
          (e0:let (new-bucket) (alist:bind (alist:unbind-one old-bucket key) key value)
            (hash:set-bucket! hash-vector hash-result new-bucket)))))))
(e1:define (unboxed-hash:unset! hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (unboxed-hash:hash key)
      (e0:let (old-bucket) (hash:get-bucket hash-vector hash-result)
        (e0:if-in (alist:has? old-bucket key) (0)
          ;; Removing a non-existing binding: do nothing
          (e0:bundle)
          ;; Removing an existing binding
          (e0:let (new-bucket) (alist:unbind-one old-bucket key)
            (e0:let () (hash:set-bucket! hash-vector hash-result new-bucket)
              (e0:let (element-no) (vector:get-from-header hash-vector (e0:value 1))
                (vector:set-from-header! hash-vector (e0:value 1) (fixnum:1- element-no))))))))))

(e1:define (string-hash:has? hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (bucket) (hash:get-bucket hash-vector (string-hash:hash key))
      (salist:has? bucket key))))
(e1:define (string-hash:get hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (string-hash:hash key)
      (e0:let (bucket) (hash:get-bucket hash-vector hash-result)
        (salist:lookup bucket key)))))
(e1:define (string-hash:set-without-resizing! hash key value) ;; the key is *not* copied
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (string-hash:hash key)
      (e0:let (old-bucket) (hash:get-bucket hash-vector hash-result)
        (e0:if-in (salist:has? old-bucket key) (#f)
          ;; Adding a new binding
          (e0:let (new-bucket) (salist:bind old-bucket key value)
            (e0:let () (hash:set-bucket! hash-vector hash-result new-bucket)
              (e0:let (element-no) (vector:get-from-header hash-vector (e0:value 1))
                (vector:set-from-header! hash-vector (e0:value 1) (fixnum:1+ element-no)))))
          ;; Replacing a binding
          (e0:let (new-bucket) (salist:bind (salist:unbind-one old-bucket key) key value)
            (hash:set-bucket! hash-vector hash-result new-bucket)))))))
(e1:define (string-hash:unset! hash key)
  (e0:let (hash-vector) (box:get hash)
    (e0:let (hash-result) (string-hash:hash key)
      (e0:let (old-bucket) (hash:get-bucket hash-vector hash-result)
        (e0:if-in (salist:has? old-bucket key) (#f)
          ;; Removing a non-existing binding: do nothing
          (e0:bundle)
          ;; Removing an existing binding
          (e0:let (new-bucket) (salist:unbind-one old-bucket key)
            (e0:let () (hash:set-bucket! hash-vector hash-result new-bucket)
              (e0:let (element-no) (vector:get-from-header hash-vector (e0:value 1))
                (vector:set-from-header! hash-vector (e0:value 1) (fixnum:1- element-no))))))))))

;;; Return a list of all the bindings in the given hash, in some
;;; unspecified order.  The result will share structure with the hash
;;; table.
(e1:define (unboxed-hash:unboxed-hash->alist hash)
  (hash:hash->list hash))
(e1:define (string-hash:string-hash->salist hash)
  (hash:hash->list hash))

(e1:define (hash:hash->list hash)
  (hash:add-from-index (box:get hash) list:nil (e0:value 2) (fixnum:1+ (hash:bucket-no hash))))
(e1:define (hash:add-from-index hash-vector acc header-based-index last-useful-header-based-index)
  (e0:if-in (fixnum:> header-based-index last-useful-header-based-index) (#f)
    (e0:let (new-acc) (hash:add-bucket-keys (vector:get-from-header hash-vector header-based-index) acc)
      (hash:add-from-index hash-vector new-acc (fixnum:1+ header-based-index) last-useful-header-based-index))
    acc))
(e1:define (hash:add-bucket-keys bucket acc)
  (e0:if-in bucket (0)
    acc
    (hash:add-bucket-keys (list:tail bucket)
                          (list:cons (list:head bucket) acc))))

;;; We currently don't shrink hashes.  We enlarge them by doubling the
;;; number of buckets when the fill factor exceeds the following
;;; maximum *before* a -set! operation is attempted:

;;; The maximum fill factor (past which we resize the hash), expressed
;;; as a fraction:
(e1:define hash:max-fill-factor-numerator   (e0:value 7))
(e1:define hash:max-fill-factor-denominator (e0:value 10))

;;; Let #e be the number of elements, and #b the number of buckets.
;;; Now, we can see the fraction hash:max-fill-factor-numerator /
;;; hash:max-fill-factor-denominator as #E/#B.  Naively, we would need
;;; to check whether #e/#b > #E/#B.  But we can do that without any
;;; division and using only integers: noticing that both #b and #B are
;;; positive, we can equivalently check whether #e*#B > #E*#b.  We can
;;; even save a multiplication if one of #E and #B is a power of two.
(e1:define (hash:overfull? hash)
  (e0:let (bucket-no) (hash:bucket-no hash)
    (e0:let (element-no) (hash:element-no hash)
      (fixnum:> (fixnum:* element-no hash:max-fill-factor-denominator)
                (fixnum:* hash:max-fill-factor-numerator bucket-no)))))

(e1:define (unboxed-hash:set! hash key value) ;; the key is *not* copied
  (e0:let ()
    (e0:if-in (hash:overfull? hash) (#f)
      (e0:bundle)
      (unboxed-hash:enlarge! hash))
    (unboxed-hash:set-without-resizing! hash key value)))
(e1:define (string-hash:set! hash key value) ;; the key is *not* copied
  (e0:let ()
    (e0:if-in (hash:overfull? hash) (#f)
      (e0:bundle)
      (string-hash:enlarge! hash))
    (string-hash:set-without-resizing! hash key value)))

;;; FIXME: hash resizing can be re-implemented more efficiently
;;; without building temporary lists.
(e1:define (unboxed-hash:add-alist! hash alist)
  (e0:if-in alist (0)
    (e0:bundle)
    (e0:let ()
      (e0:let (first-pair) (list:head alist)
        ;; FIXME: use a new variant of set-without-resizing! which doesn't check for presence
        (unboxed-hash:set-without-resizing! hash (cons:car first-pair) (cons:cdr first-pair)))
      (unboxed-hash:add-alist! hash (list:tail alist)))))
(e1:define (string-hash:add-salist! hash salist)
  (e0:if-in salist (0)
    (e0:bundle)
    (e0:let ()
      (e0:let (first-pair) (list:head salist)
        ;; FIXME: use a new variant of set-without-resizing! which doesn't check for presence
        (string-hash:set-without-resizing! hash (cons:car first-pair) (cons:cdr first-pair)))
      (string-hash:add-salist! hash (list:tail salist)))))

(e1:define (hash:bucket-no-after-enlargement old-bucket-no)
;;  (fixnum:* old-bucket-no (e0:value 2)))
  (fixnum:1+ (fixnum:* old-bucket-no (e0:value 2))))

(e1:define (unboxed-hash:enlarge! hash)
  (e0:let (old-bucket-no) (hash:bucket-no hash)
    (unboxed-hash:resize! hash (hash:bucket-no-after-enlargement old-bucket-no))))
(e1:define (string-hash:enlarge! hash)
  (e0:let (old-bucket-no) (hash:bucket-no hash)
    (string-hash:resize! hash (hash:bucket-no-after-enlargement old-bucket-no))))

(e1:define (unboxed-hash:resize! hash new-bucket-no)
  (e0:let (alist) (hash:hash->list hash)
    (e0:let (new-hash) (hash:make-given-bucket-no new-bucket-no)
      (e0:let ()
        (unboxed-hash:add-alist! new-hash alist)
        (box:set! hash (box:get new-hash))))))
        ;; (box:set! hash (box:get new-hash)) ;; make the old bucket vector become garbage immediately
        ;; (unboxed-hash:add-alist! hash alist)))))
(e1:define (string-hash:resize! hash new-bucket-no)
  (e0:let (salist) (hash:hash->list hash)
    (e0:let (new-hash) (hash:make-given-bucket-no new-bucket-no)
      (e0:let ()
        (string-hash:add-salist! new-hash salist)
        (box:set! hash (box:get new-hash))))))
        ;; (box:set! hash (box:get new-hash)) ;; make the old bucket vector become garbage immediately
        ;; (string-hash:add-salist! hash salist)))))

(e1:define (hash:clear! hash)
  (e0:let (new-hash) (hash:make)
    (e0:let (new-buckets) (box:get new-hash)
      (box:set! hash new-buckets))))

(e1:define (string-hash:clear! hash)
  (hash:clear! hash))
(e1:define (unboxed-hash:clear! hash)
  (hash:clear! hash))

;;; Return new hashes with the same content of the given ones,
;;; exchanging each datum with the corresponding key.  Where the given
;;; hash is not surjective (i.e. where the given hash has more than
;;; one key mapped to the same datum), the result will contain one
;;; binding; it is not defined which one.
(e1:define (unboxed-hash:invert-into-unboxed-hash hash)
  (hash:invert-into-unboxed-hash hash))
(e1:define (unboxed-hash:invert-into-string-hash hash)
  (hash:invert-into-string-hash hash))
(e1:define (string-hash:invert-into-unboxed-hash hash)
  (hash:invert-into-unboxed-hash hash))
(e1:define (string-hash:invert-into-string-hash hash)
  (hash:invert-into-string-hash hash))

(e1:define (hash:invert-into-unboxed-hash h)
  (e0:let (bucket-no) (hash:bucket-no h)
    (e0:let (new-hash) (hash:make-given-bucket-no bucket-no)
      (e0:let () (hash:invert-into-unboxed-hash-index! new-hash
                                                       (box:get h)
                                                       bucket-no)
        new-hash))))
(e1:define (hash:invert-into-unboxed-hash-index! new-hash old-buckets index)
  (e0:if-in index (1)
    (e0:bundle)
    (e0:let () (hash:invert-bucket-into-unboxed-hash! new-hash
                                                      (buffer:get old-buckets index))
      (hash:invert-into-unboxed-hash-index! new-hash old-buckets (fixnum:1- index)))))
(e1:define (hash:invert-bucket-into-unboxed-hash! new-hash old-bucket)
  (e0:if-in old-bucket (0)
    (e0:bundle)
    (e0:let (first-pair) (list:head old-bucket)
      (e0:let () (unboxed-hash:set-without-resizing! new-hash
                                                     (cons:cdr first-pair)
                                                     (cons:car first-pair))
        (hash:invert-bucket-into-unboxed-hash! new-hash (list:tail old-bucket))))))

(e1:define (hash:invert-into-string-hash h)
  (e0:let (bucket-no) (hash:bucket-no h)
    (e0:let (new-hash) (hash:make-given-bucket-no bucket-no)
      (e0:let () (hash:invert-into-string-hash-index! new-hash
                                                       (box:get h)
                                                       bucket-no)
        new-hash))))
(e1:define (hash:invert-into-string-hash-index! new-hash old-buckets index)
  (e0:if-in index (1)
    (e0:bundle)
    (e0:let () (hash:invert-bucket-into-string-hash! new-hash
                                                      (buffer:get old-buckets index))
      (hash:invert-into-string-hash-index! new-hash old-buckets (fixnum:1- index)))))
(e1:define (hash:invert-bucket-into-string-hash! new-hash old-bucket)
  (e0:if-in old-bucket (0)
    (e0:bundle)
    (e0:let (first-pair) (list:head old-bucket)
      (e0:let () (string-hash:set-without-resizing! new-hash
                                                     (cons:cdr first-pair)
                                                     (cons:car first-pair))
        (hash:invert-bucket-into-string-hash! new-hash (list:tail old-bucket))))))


;;;;; Symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Symbols, once created, are easy to deal with: they can be compared
;;; by identity, and their names can be extracted.  Despite their
;;; uniqueness property enabling comparisons by identity, symbols are boxed
;;; objects.  They are in fact buffers containing, in order:
;;;
;;; 0. the the symbol name pointer for interned symbol, or nil for
;;;    uninterned symbols;
;;; 1. zero if there is no global associated with the symbol, another
;;;    value otherwise;
;;; 2. the symbol value in the global environment, or an unspecified
;;;    value if there is no associated global;
;;; 3. a pointer to the formal list of the procedure named by the symbol;
;;; 4. a pointer to the procedure body of the procedure named by the
;;;    symbol, or zero if there's no associated procedure;
;;; 5. the macro associated with the symbol (an s-expression), or zero
;;;    if there's no associated macro;
;;; 6. the name of the macro procedure (already transformed) associated,
;;;    or zero if the expansion is not cached or there is no associated
;;;    macro;
;;; 7. the primitive descriptor associated to the name, or zero;
;;; 8. bytecode-compiled procedure, or zero;
;;; 9. native-compiled procedure, or zero;
;;; 10. alist with user-defined data.
;;;
;;; The same general idea was used in some historical Lisp
;;; implementations such as PDP-10 MacLisp, as described in the MIT
;;; AIM 420

(e1:define (symbol:make-uninterned)
  (e0:let (result) (buffer:make (e0:value 11))
    ;; Not actually needed if buffers are zero-filled at creation
    ;; time, but here I want to emphasize:
    (e0:let () (buffer:initialize! result (e0:value 0) (e0:value 0)) ; name
      (e0:let () (buffer:initialize! result (e0:value 1) (e0:value 0)) ; unbound in the global environment
        (e0:let () (buffer:initialize! result (e0:value 2) (e0:value 127)) ; global ("unbound marker")
          (e0:let () (buffer:initialize! result (e0:value 3) (e0:value 0)) ; formal list (empty)
            (e0:let () (buffer:initialize! result (e0:value 4) (e0:value 0)) ; body (no body: unbound as procedure)
              (e0:let () (buffer:initialize! result (e0:value 5) (e0:value 0)) ; macro definition or 0
                (e0:let () (buffer:initialize! result (e0:value 6) (e0:value 0)) ; macro procedure or 0
                  (e0:let () (buffer:initialize! result (e0:value 7) (e0:value 0)) ; primitive descriptor or 0
                    (e0:let () (buffer:initialize! result (e0:value 8) (e0:value 0)) ; bytecode procedure or 0
                      (e0:let () (buffer:initialize! result (e0:value 9) (e0:value 0)) ; native procedure or 0
                        (e0:let () (buffer:initialize! result (e0:value 10) alist:nil) ; extensions
                          result)))))))))))))

(e1:define (symbol:fresh)
  (e0:let (result) (symbol:make-uninterned)
    (e0:let () (symbol:intern-uninterned! result string:empty)
      result)))

(e1:define symbol:gensym-box
  (box:make-0-initialized))

(e1:define symbol:fresh-prefix-as-string
  (e0:value "_"))

(e1:define (symbol:intern-uninterned! symbol prefix)
  (e0:let (suffix-fixnum) (box:get-and-bump! symbol:gensym-box)
    (e0:let (candidate-name) (string:append3 symbol:fresh-prefix-as-string
                                             prefix
                                             (string:fixnum->string suffix-fixnum))
      (e0:if-in (string-hash:has? symbol:table candidate-name) (#f)
        ;; Good, the name is free:
        (e0:let () (buffer:set! symbol (e0:value 0) candidate-name)
          (string-hash:set! symbol:table candidate-name symbol))
        ;; Name collision (this should be rare); try again:
        (symbol:intern-uninterned! symbol)))))

(e1:define (symbol:intern-when-uninterned! symbol)
  (e0:if-in (symbol:interned? symbol) (#f)
            (symbol:intern-uninterned! symbol)
            (e0:bundle)))

(e1:define (symbol:symbol->string symbol)
  (buffer:get symbol (e0:value 0)))

(e1:define (symbol:interned? symbol)
  ;; The symbol is interned iff its name isn't nil
  (symbol:symbol->string symbol))

;;; Return non-#f iff the given object, which must be a symbol, is a
;;; non-procedure name.
(e1:define (symbol:global-name? symbol)
  (buffer:get symbol (e0:value 2)))

;;; Return non-#f iff the given object, which must be a symbol, is a
;;; procedure name.
(e1:define (symbol:procedure-name? symbol)
  (buffer:get symbol (e0:value 4)))

;;; The symbol table is a global box containing a list of <name,
;;; symbol> conses, one element per interned symbol.
(e1:define symbol:table
  (string-hash:make))

(e1:define (symbol:interned-symbol-name? name-as-string)
  (string-hash:has? symbol:table name-as-string))
(e1:define (symbol:intern-without-checking! name-as-string)
  (e0:let (new-symbol) (symbol:make-uninterned)
    (e0:let () (buffer:set! new-symbol (e0:value 0) name-as-string)
      (e0:let () (string-hash:set! symbol:table name-as-string new-symbol)
        new-symbol)))) ;; FIXME: test

;;; Return the symbol with the given name; if no such symbol currently
;;; exists, make one (*copying* the given string, so that it can be
;;; safely modified later) and return it.
(e1:define (symbol:intern name-as-string)
  (e0:if-in (symbol:interned-symbol-name? name-as-string) (#f)
    (symbol:intern-without-checking! (vector:shallow-clone name-as-string))
    (string-hash:get symbol:table name-as-string)))
(e1:define (symbol:string->symbol name-as-string) ;; an alias
  (symbol:intern name-as-string))

(e1:define (symbol:fresh-symbols how-many)
  (symbol:fresh-symbols-acc how-many list:nil))
(e1:define (symbol:fresh-symbols-acc how-many acc)
  (e0:if-in how-many (0)
    acc
    (e0:let (new-symbol) (symbol:fresh)
        (symbol:fresh-symbols-acc (fixnum:1- how-many)
                                       (list:cons new-symbol acc)))))

(e1:define (symbol:write s)
  (string:write (symbol:symbol->string s)))


;;; Fixed-point numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We reserve this many bits for the fractional part of each
;;; fixed-point number, in the the least significant part of the word.
;;; All the other bits will be used for the integer part and the sign
;;; bit; we assume two's complement representation for integers.
;;; Warning: using more than 30 bits causes the bitmask to be
;;; undumptable in the current system based on 32-bit words for all
;;; architactures.
(e1:define fixedpoint:fractional-bit-no
  (e0:value 16))

;;; This is used for some conversions:
(e1:define fixedpoint:fractional-bitmask
  (fixnum:1- (fixnum:left-shift (e0:value 1) fixedpoint:fractional-bit-no)))

;;; FIXME: remove this crap
;;; Useful for building small constants.  All fixnums must be zero or
;;; positive; the others must be in [0, 10). For example, supplying
;;; the fixnums 1, 192, 0, 3 and 2 we get the fixed-point result
;;; -192.032 .
(e1:define (fixedpoint:make negative
                            integer-part
                            fractional-decimal-digit-1
                            fractional-decimal-digit-2
                            fractional-decimal-digit-3)
  (e0:let (fractional-part)
          (fixnum:+ (fixnum:* fractional-decimal-digit-1 (e0:value 100))
                    (fixnum:+ (fixnum:* fractional-decimal-digit-2 (e0:value 10))
                              fractional-decimal-digit-3))
    (e0:let (absolute-value)
            (fixnum:bitwise-or (fixnum:left-shift integer-part fixedpoint:fractional-bit-no)
                               (fixnum:/ (fixnum:left-shift fractional-part fixedpoint:fractional-bit-no)
                                         (e0:value 1000)))
      (e0:if-in negative (0)
        absolute-value
        (fixnum:negate absolute-value)))))

(e1:define (fixedpoint:fixnum->fixedpoint fixnum)
  (fixnum:left-shift fixnum fixedpoint:fractional-bit-no))

;;; Basic arithmetic:
(e1:define (fixedpoint:+ a b)
  (fixnum:+ a b))
(e1:define (fixedpoint:negate a)
  (fixnum:negate a))
(e1:define (fixedpoint:- a b)
  (fixnum:- a b))
(e1:define (fixedpoint:* a b)
  (fixnum:arithmetic-right-shift (fixnum:* a b) fixedpoint:fractional-bit-no))
(e1:define (fixedpoint:/ a b)
  (fixnum:/ (fixnum:left-shift a fixedpoint:fractional-bit-no) b))
(e1:define (fixedpoint:sign a)
  (fixnum:sign a))

;;; Comparisons:
(e1:define (fixedpoint:< a b)
  (fixnum:< a b))
(e1:define (fixedpoint:> a b)
  (fixnum:> a b))
(e1:define (fixedpoint:<= a b)
  (fixnum:<= a b))
(e1:define (fixedpoint:>= a b)
  (fixnum:>= a b))
(e1:define (fixedpoint:= a b)
  (fixnum:= a b))
(e1:define (fixedpoint:<> a b)
  (fixnum:<> a b))

;;; Conversion:
(e1:define (fixedpoint:fixedpoint->fixnum fixed)
  (fixnum:arithmetic-right-shift fixed fixedpoint:fractional-bit-no))

;;; More computation:
(e1:define (fixedpoint:mod a)
  (fixnum:mod a))

;;; Particularly useful for printing: [FIXME: get rid of this crap]
(e1:define (fixedpoint:get-integer-part fixed)
  (fixnum:arithmetic-right-shift fixed fixedpoint:fractional-bit-no))
(e1:define (fixedpoint:get-fractional-part fixed digit-no)
  (e0:let (possibly-negative-result)
          (fixnum:arithmetic-right-shift (fixnum:* (fixnum:bitwise-and fixed fixedpoint:fractional-bitmask)
                                                   (fixnum:** (e0:value 10) digit-no))
                                         fixedpoint:fractional-bit-no)
    (fixnum:mod possibly-negative-result)))

;;; Some useful constants:
(e1:define fixedpoint:0
           (e0:value 0))
(e1:define fixedpoint:1   ;; + 1. 0 0 0
           (fixedpoint:make (e0:value #f) (e0:value 1) (e0:value 0) (e0:value 0) (e0:value 0)))
(e1:define fixedpoint:-1  ;; - 1. 0 0 0
           (fixedpoint:make (e0:value #t) (e0:value 1) (e0:value 0) (e0:value 0) (e0:value 0)))
(e1:define fixedpoint:2   ;; + 2. 0 0 0
           (fixedpoint:make (e0:value #f) (e0:value 2) (e0:value 0) (e0:value 0) (e0:value 0)))
(e1:define fixedpoint:3   ;; + 3. 0 0 0
           (fixedpoint:make (e0:value #f) (e0:value 2) (e0:value 0) (e0:value 0) (e0:value 0)))
(e1:define fixedpoint:1:2 ;; + 0. 5 0 0
           (fixedpoint:make (e0:value #f) (e0:value 0) (e0:value 5) (e0:value 0) (e0:value 0)))
(e1:define fixedpoint:1:4 ;; + 0. 2 5 0
           (fixedpoint:make (e0:value #f) (e0:value 0) (e0:value 2) (e0:value 5) (e0:value 0)))

(e1:define fixedpoint:10  ;; + 10. 0 0 0
           (fixedpoint:make (e0:value #f) (e0:value 10) (e0:value 0) (e0:value 0) (e0:value 0)))


;;;;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here we define data structures representing all the cases of
;;; epsilon0 expression ASTs.  We code ASTs as a sum type.

;;; The code is complex, so we automatically generated it from the
;;; definition below; but since the generator can't run on epsilon0 at
;;; this stage, we used Scheme to bootstrap.  Of course this generation
;;; will also be doable in epsilon1, after some language extension:

#!
;;; Definition source:
(sum:define e0:expression
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
!#

;;; The definition above, automatically translated by
;;; generate-sum-type-from-scheme.scm piped to ../COMPRESS-WHITESPACE:
(e1:define e0:expression-variable-tag(e0:value 0))(e1:define(e0:expression-variable? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 0))(e0:value #f)))(e1:define(e0:expression-variable handle name)(e0:let(variable)(e0:primitive buffer:make(e0:value 3))(e0:let()(e0:primitive buffer:set! variable(e0:value 0)(e0:value 0))(e0:let()(e0:primitive buffer:set! variable(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! variable(e0:value 2)name)variable)))))(e1:define(e0:expression-variable-get-handle variable)(e0:primitive buffer:get variable(e0:value 1)))(e1:define(e0:expression-variable-set-handle! variable handle)(e0:primitive buffer:set! variable(e0:value 1)handle))(e1:define(e0:expression-variable-get-name variable)(e0:primitive buffer:get variable(e0:value 2)))(e1:define(e0:expression-variable-set-name! variable name)(e0:primitive buffer:set! variable(e0:value 2)name))(e1:define(e0:expression-variable-explode variable)(e0:bundle(e0:expression-variable-get-handle variable)(e0:expression-variable-get-name variable)))(e1:define e0:expression-value-tag(e0:value 1))(e1:define(e0:expression-value? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 1))(e0:value #f)))(e1:define(e0:expression-value handle content)(e0:let(value)(e0:primitive buffer:make(e0:value 3))(e0:let()(e0:primitive buffer:set! value(e0:value 0)(e0:value 1))(e0:let()(e0:primitive buffer:set! value(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! value(e0:value 2)content)value)))))(e1:define(e0:expression-value-get-handle value)(e0:primitive buffer:get value(e0:value 1)))(e1:define(e0:expression-value-set-handle! value handle)(e0:primitive buffer:set! value(e0:value 1)handle))(e1:define(e0:expression-value-get-content value)(e0:primitive buffer:get value(e0:value 2)))(e1:define(e0:expression-value-set-content! value content)(e0:primitive buffer:set! value(e0:value 2)content))(e1:define(e0:expression-value-explode value)(e0:bundle(e0:expression-value-get-handle value)(e0:expression-value-get-content value)))(e1:define e0:expression-bundle-tag(e0:value 2))(e1:define(e0:expression-bundle? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 2))(e0:value #f)))(e1:define(e0:expression-bundle handle items)(e0:let(bundle)(e0:primitive buffer:make(e0:value 3))(e0:let()(e0:primitive buffer:set! bundle(e0:value 0)(e0:value 2))(e0:let()(e0:primitive buffer:set! bundle(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! bundle(e0:value 2)items)bundle)))))(e1:define(e0:expression-bundle-get-handle bundle)(e0:primitive buffer:get bundle(e0:value 1)))(e1:define(e0:expression-bundle-set-handle! bundle handle)(e0:primitive buffer:set! bundle(e0:value 1)handle))(e1:define(e0:expression-bundle-get-items bundle)(e0:primitive buffer:get bundle(e0:value 2)))(e1:define(e0:expression-bundle-set-items! bundle items)(e0:primitive buffer:set! bundle(e0:value 2)items))(e1:define(e0:expression-bundle-explode bundle)(e0:bundle(e0:expression-bundle-get-handle bundle)(e0:expression-bundle-get-items bundle)))(e1:define e0:expression-primitive-tag(e0:value 3))(e1:define(e0:expression-primitive? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 3))(e0:value #f)))(e1:define(e0:expression-primitive handle name actuals)(e0:let(primitive)(e0:primitive buffer:make(e0:value 4))(e0:let()(e0:primitive buffer:set! primitive(e0:value 0)(e0:value 3))(e0:let()(e0:primitive buffer:set! primitive(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! primitive(e0:value 2)name)(e0:let()(e0:primitive buffer:set! primitive(e0:value 3)actuals)primitive))))))(e1:define(e0:expression-primitive-get-handle primitive)(e0:primitive buffer:get primitive(e0:value 1)))(e1:define(e0:expression-primitive-set-handle! primitive handle)(e0:primitive buffer:set! primitive(e0:value 1)handle))(e1:define(e0:expression-primitive-get-name primitive)(e0:primitive buffer:get primitive(e0:value 2)))(e1:define(e0:expression-primitive-set-name! primitive name)(e0:primitive buffer:set! primitive(e0:value 2)name))(e1:define(e0:expression-primitive-get-actuals primitive)(e0:primitive buffer:get primitive(e0:value 3)))(e1:define(e0:expression-primitive-set-actuals! primitive actuals)(e0:primitive buffer:set! primitive(e0:value 3)actuals))(e1:define(e0:expression-primitive-explode primitive)(e0:bundle(e0:expression-primitive-get-handle primitive)(e0:expression-primitive-get-name primitive)(e0:expression-primitive-get-actuals primitive)))(e1:define e0:expression-let-tag(e0:value 4))(e1:define(e0:expression-let? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 4))(e0:value #f)))(e1:define(e0:expression-let handle bound-variables bound-expression body)(e0:let(let)(e0:primitive buffer:make(e0:value 5))(e0:let()(e0:primitive buffer:set! let(e0:value 0)(e0:value 4))(e0:let()(e0:primitive buffer:set! let(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! let(e0:value 2)bound-variables)(e0:let()(e0:primitive buffer:set! let(e0:value 3)bound-expression)(e0:let()(e0:primitive buffer:set! let(e0:value 4)body)let)))))))(e1:define(e0:expression-let-get-handle let)(e0:primitive buffer:get let(e0:value 1)))(e1:define(e0:expression-let-set-handle! let handle)(e0:primitive buffer:set! let(e0:value 1)handle))(e1:define(e0:expression-let-get-bound-variables let)(e0:primitive buffer:get let(e0:value 2)))(e1:define(e0:expression-let-set-bound-variables! let bound-variables)(e0:primitive buffer:set! let(e0:value 2)bound-variables))(e1:define(e0:expression-let-get-bound-expression let)(e0:primitive buffer:get let(e0:value 3)))(e1:define(e0:expression-let-set-bound-expression! let bound-expression)(e0:primitive buffer:set! let(e0:value 3)bound-expression))(e1:define(e0:expression-let-get-body let)(e0:primitive buffer:get let(e0:value 4)))(e1:define(e0:expression-let-set-body! let body)(e0:primitive buffer:set! let(e0:value 4)body))(e1:define(e0:expression-let-explode let)(e0:bundle(e0:expression-let-get-handle let)(e0:expression-let-get-bound-variables let)(e0:expression-let-get-bound-expression let)(e0:expression-let-get-body let)))(e1:define e0:expression-call-tag(e0:value 5))(e1:define(e0:expression-call? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 5))(e0:value #f)))(e1:define(e0:expression-call handle procedure-name actuals)(e0:let(call)(e0:primitive buffer:make(e0:value 4))(e0:let()(e0:primitive buffer:set! call(e0:value 0)(e0:value 5))(e0:let()(e0:primitive buffer:set! call(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! call(e0:value 2)procedure-name)(e0:let()(e0:primitive buffer:set! call(e0:value 3)actuals)call))))))(e1:define(e0:expression-call-get-handle call)(e0:primitive buffer:get call(e0:value 1)))(e1:define(e0:expression-call-set-handle! call handle)(e0:primitive buffer:set! call(e0:value 1)handle))(e1:define(e0:expression-call-get-procedure-name call)(e0:primitive buffer:get call(e0:value 2)))(e1:define(e0:expression-call-set-procedure-name! call procedure-name)(e0:primitive buffer:set! call(e0:value 2)procedure-name))(e1:define(e0:expression-call-get-actuals call)(e0:primitive buffer:get call(e0:value 3)))(e1:define(e0:expression-call-set-actuals! call actuals)(e0:primitive buffer:set! call(e0:value 3)actuals))(e1:define(e0:expression-call-explode call)(e0:bundle(e0:expression-call-get-handle call)(e0:expression-call-get-procedure-name call)(e0:expression-call-get-actuals call)))(e1:define e0:expression-call-indirect-tag(e0:value 6))(e1:define(e0:expression-call-indirect? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 6))(e0:value #f)))(e1:define(e0:expression-call-indirect handle procedure-expression actuals)(e0:let(call-indirect)(e0:primitive buffer:make(e0:value 4))(e0:let()(e0:primitive buffer:set! call-indirect(e0:value 0)(e0:value 6))(e0:let()(e0:primitive buffer:set! call-indirect(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! call-indirect(e0:value 2)procedure-expression)(e0:let()(e0:primitive buffer:set! call-indirect(e0:value 3)actuals)call-indirect))))))(e1:define(e0:expression-call-indirect-get-handle call-indirect)(e0:primitive buffer:get call-indirect(e0:value 1)))(e1:define(e0:expression-call-indirect-set-handle! call-indirect handle)(e0:primitive buffer:set! call-indirect(e0:value 1)handle))(e1:define(e0:expression-call-indirect-get-procedure-expression call-indirect)(e0:primitive buffer:get call-indirect(e0:value 2)))(e1:define(e0:expression-call-indirect-set-procedure-expression! call-indirect procedure-expression)(e0:primitive buffer:set! call-indirect(e0:value 2)procedure-expression))(e1:define(e0:expression-call-indirect-get-actuals call-indirect)(e0:primitive buffer:get call-indirect(e0:value 3)))(e1:define(e0:expression-call-indirect-set-actuals! call-indirect actuals)(e0:primitive buffer:set! call-indirect(e0:value 3)actuals))(e1:define(e0:expression-call-indirect-explode call-indirect)(e0:bundle(e0:expression-call-indirect-get-handle call-indirect)(e0:expression-call-indirect-get-procedure-expression call-indirect)(e0:expression-call-indirect-get-actuals call-indirect)))(e1:define e0:expression-if-in-tag(e0:value 7))(e1:define(e0:expression-if-in? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 7))(e0:value #f)))(e1:define(e0:expression-if-in handle discriminand values then-branch else-branch)(e0:let(if-in)(e0:primitive buffer:make(e0:value 6))(e0:let()(e0:primitive buffer:set! if-in(e0:value 0)(e0:value 7))(e0:let()(e0:primitive buffer:set! if-in(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! if-in(e0:value 2)discriminand)(e0:let()(e0:primitive buffer:set! if-in(e0:value 3)values)(e0:let()(e0:primitive buffer:set! if-in(e0:value 4)then-branch)(e0:let()(e0:primitive buffer:set! if-in(e0:value 5)else-branch)if-in))))))))(e1:define(e0:expression-if-in-get-handle if-in)(e0:primitive buffer:get if-in(e0:value 1)))(e1:define(e0:expression-if-in-set-handle! if-in handle)(e0:primitive buffer:set! if-in(e0:value 1)handle))(e1:define(e0:expression-if-in-get-discriminand if-in)(e0:primitive buffer:get if-in(e0:value 2)))(e1:define(e0:expression-if-in-set-discriminand! if-in discriminand)(e0:primitive buffer:set! if-in(e0:value 2)discriminand))(e1:define(e0:expression-if-in-get-values if-in)(e0:primitive buffer:get if-in(e0:value 3)))(e1:define(e0:expression-if-in-set-values! if-in values)(e0:primitive buffer:set! if-in(e0:value 3)values))(e1:define(e0:expression-if-in-get-then-branch if-in)(e0:primitive buffer:get if-in(e0:value 4)))(e1:define(e0:expression-if-in-set-then-branch! if-in then-branch)(e0:primitive buffer:set! if-in(e0:value 4)then-branch))(e1:define(e0:expression-if-in-get-else-branch if-in)(e0:primitive buffer:get if-in(e0:value 5)))(e1:define(e0:expression-if-in-set-else-branch! if-in else-branch)(e0:primitive buffer:set! if-in(e0:value 5)else-branch))(e1:define(e0:expression-if-in-explode if-in)(e0:bundle(e0:expression-if-in-get-handle if-in)(e0:expression-if-in-get-discriminand if-in)(e0:expression-if-in-get-values if-in)(e0:expression-if-in-get-then-branch if-in)(e0:expression-if-in-get-else-branch if-in)))(e1:define e0:expression-fork-tag(e0:value 8))(e1:define(e0:expression-fork? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 8))(e0:value #f)))(e1:define(e0:expression-fork handle procedure-name actuals)(e0:let(fork)(e0:primitive buffer:make(e0:value 4))(e0:let()(e0:primitive buffer:set! fork(e0:value 0)(e0:value 8))(e0:let()(e0:primitive buffer:set! fork(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! fork(e0:value 2)procedure-name)(e0:let()(e0:primitive buffer:set! fork(e0:value 3)actuals)fork))))))(e1:define(e0:expression-fork-get-handle fork)(e0:primitive buffer:get fork(e0:value 1)))(e1:define(e0:expression-fork-set-handle! fork handle)(e0:primitive buffer:set! fork(e0:value 1)handle))(e1:define(e0:expression-fork-get-procedure-name fork)(e0:primitive buffer:get fork(e0:value 2)))(e1:define(e0:expression-fork-set-procedure-name! fork procedure-name)(e0:primitive buffer:set! fork(e0:value 2)procedure-name))(e1:define(e0:expression-fork-get-actuals fork)(e0:primitive buffer:get fork(e0:value 3)))(e1:define(e0:expression-fork-set-actuals! fork actuals)(e0:primitive buffer:set! fork(e0:value 3)actuals))(e1:define(e0:expression-fork-explode fork)(e0:bundle(e0:expression-fork-get-handle fork)(e0:expression-fork-get-procedure-name fork)(e0:expression-fork-get-actuals fork)))(e1:define e0:expression-join-tag(e0:value 9))(e1:define(e0:expression-join? expression)(e0:if-in(boxedness:definitely-unboxed? expression)(#f)(whatever:eq?(e0:primitive buffer:get expression(e0:value 0))(e0:value 9))(e0:value #f)))(e1:define(e0:expression-join handle future)(e0:let(join)(e0:primitive buffer:make(e0:value 3))(e0:let()(e0:primitive buffer:set! join(e0:value 0)(e0:value 9))(e0:let()(e0:primitive buffer:set! join(e0:value 1)handle)(e0:let()(e0:primitive buffer:set! join(e0:value 2)future)join)))))(e1:define(e0:expression-join-get-handle join)(e0:primitive buffer:get join(e0:value 1)))(e1:define(e0:expression-join-set-handle! join handle)(e0:primitive buffer:set! join(e0:value 1)handle))(e1:define(e0:expression-join-get-future join)(e0:primitive buffer:get join(e0:value 2)))(e1:define(e0:expression-join-set-future! join future)(e0:primitive buffer:set! join(e0:value 2)future))(e1:define(e0:expression-join-explode join)(e0:bundle(e0:expression-join-get-handle join)(e0:expression-join-get-future join)))


;;; I want to be able to easily generate fresh handles:
(e1:define e0:handle-generator-box
  (box:make-initialized (e0:value 0)));;(e0:value 1000000)))
(e1:define (e0:fresh-handle)
  (box:bump-and-get! e0:handle-generator-box))

;;; User-friendly procedural constructors:
(e1:define (e0:variable* name)
  (e0:expression-variable (e0:fresh-handle) name))
(e1:define (e0:value* content)
  (e0:expression-value (e0:fresh-handle) content))
(e1:define (e0:bundle* items)
  (e0:expression-bundle (e0:fresh-handle) items))
(e1:define (e0:primitive* name actuals)
  (e0:expression-primitive (e0:fresh-handle) name actuals))
(e1:define (e0:let* bound-variables bound-expression body)
  (e0:expression-let (e0:fresh-handle) bound-variables bound-expression body))
(e1:define (e0:call* name actuals)
  (e0:expression-call (e0:fresh-handle) name actuals))
(e1:define (e0:call-indirect* procedure-expression actuals)
  (e0:expression-call-indirect (e0:fresh-handle) procedure-expression actuals))
(e1:define (e0:if-in* discriminand values then-branch else-branch)
  (e0:expression-if-in (e0:fresh-handle) discriminand values then-branch else-branch))
(e1:define (e0:fork* name actuals)
  (e0:expression-fork (e0:fresh-handle) name actuals))
(e1:define (e0:join* future)
  (e0:expression-join (e0:fresh-handle) future))


;;;;; State: global dynamic state, with reflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; The epsilon0 interpreter has some global state

;;;; This is quite simple and only consists of helper procedures: all
;;;; the relevant data is held in symbols.

(e1:define (state:global? symbol)
  (buffer:get symbol (e0:value 1)))
(e1:define (state:global-names)
  (state:add-globally-bound-symbols (string-hash:string-hash->salist symbol:table) list:nil))
(e1:define (state:add-globally-bound-symbols salist acc)
  (e0:if-in salist (0)
    acc
    (e0:let (first-symbol) (cons:cdr (list:head salist))
      (e0:if-in (state:global? first-symbol) (#f)
        (state:add-globally-bound-symbols (list:tail salist) acc)
        (state:add-globally-bound-symbols (list:tail salist) (list:cons first-symbol acc))))))
(e1:define (state:global-get-unsafe symbol)
  (buffer:get symbol (e0:value 2)))
(e1:define (state:global-get symbol)
  (e0:if-in (state:global? symbol) (0)
    (e1:errors (e0:value "state:global-get: unbound global") symbol)
    (buffer:get symbol (e0:value 2))))
(e1:define (state:global-set! name value)
  (e0:let () (buffer:set! name (e0:value 1) (e0:value 1)) ;; bound
    (buffer:set! name (e0:value 2) value))) ;; value
(e1:define (state:global-unset! name)
  (e0:let () (buffer:set! name (e0:value 1) (e0:value 0)) ;; unbound
    ;; Let the old value become garbage, and prevent it from being accessed by mistake:
    (buffer:set! name (e0:value 2) (e0:value 127)))) ;; "invalid" marker

(e1:define (state:procedure? name)
  ;; return (e0:value 0) iff the procedure is unbound, which is to say return its body
  (state:procedure-get-body name))
(e1:define (state:procedure-names)
  (state:add-procedure-symbols (string-hash:string-hash->salist symbol:table) list:nil))
(e1:define (state:add-procedure-symbols salist acc)
  (e0:if-in salist (0)
    acc
    (e0:let (first-symbol) (cons:cdr (list:head salist))
      (e0:if-in (state:procedure? first-symbol) (#f)
        (state:add-procedure-symbols (list:tail salist) acc)
        (state:add-procedure-symbols (list:tail salist) (list:cons first-symbol acc))))))

(e1:define (state:procedure-get-formals name)
  (buffer:get name (e0:value 3)))
(e1:define (state:procedure-get-body name)
  (buffer:get name (e0:value 4)))
(e1:define (state:procedure-get-in-dimension name)
  (list:length (state:procedure-get-formals name)))
(e1:define (state:procedure-get name)
  (e0:bundle (state:procedure-get-formals name)
             (state:procedure-get-body name)))
(e1:define (state:procedure-set! name formals body)
  (e0:let () (buffer:set! name (e0:value 3) formals)
    (buffer:set! name (e0:value 4) body)))
(e1:define (state:procedure-unset! name)
  (e0:let () (buffer:set! name (e0:value 3) list:nil)
    ;; invalid body
    (buffer:set! name (e0:value 4) (e0:value 0))))

(e1:define (state:bytecode-procedure-get name)
  (buffer:get name (e0:value 8)))
(e1:define (state:bytecode-procedure-set! name bytecode)
  (buffer:set! name (e0:value 8) bytecode))
(e1:define (state:bytecode-procedure-unset! name)
  (buffer:set! name (e0:value 8) 0))

(e1:define (state:native-procedure-get name)
  (buffer:get name (e0:value 9)))
(e1:define (state:native-procedure-set! name native)
  (buffer:set! name (e0:value 9) native))
(e1:define (state:native-procedure-unset! name)
  (buffer:set! name (e0:value 9) 0))



;;;; A primitive descriptor is a buffer containing the following fields, in order:
;;;;
;;;; 0. in-dimension;
;;;; 1. out-dimension;
;;;; 2. a boolean, true iff the primitive is side-effecting;
;;;; 3. a boolean, true iff the primitive is reflective;
;;;; 4. a global primitive index, used to avoid a name lookup in the C interpreter
(e1:define (state:primitive-set! name-as-symbol in-dimension out-dimension side-effecting reflective)
  (e0:let (descriptor) (buffer:make (e0:value 5))
    (e0:let () (buffer:set! descriptor (e0:value 0) in-dimension)
      (e0:let () (buffer:set! descriptor (e0:value 1) out-dimension)
        (e0:let () (buffer:set! descriptor (e0:value 2) side-effecting)
          (e0:let () (buffer:set! descriptor (e0:value 3) reflective)
            (e0:let () (buffer:set! descriptor (e0:value 4) (e0:primitive primitive:get-index (symbol:symbol->string name-as-symbol)))
              (buffer:set! name-as-symbol (e0:value 7) descriptor))))))))
(e1:define (state:primitive-get name-as-symbol)
  (buffer:get name-as-symbol (e0:value 7)))
(e1:define (state:primitive-get-in-dimension name-as-symbol)
  (e0:let (descriptor) (state:primitive-get name-as-symbol)
    (buffer:get descriptor (e0:value 0))))
(e1:define (state:primitive-get-out-dimension name-as-symbol)
  (e0:let (descriptor) (state:primitive-get name-as-symbol)
    (buffer:get descriptor (e0:value 1))))
(e1:define (state:primitive-get-side-effecting name-as-symbol)
  (e0:let (descriptor) (state:primitive-get name-as-symbol)
    (buffer:get descriptor (e0:value 2))))
(e1:define (state:primitive-get-reflective name-as-symbol)
  (e0:let (descriptor) (state:primitive-get name-as-symbol)
    (buffer:get descriptor (e0:value 3))))
(e1:define (state:primitive-get-index name-as-symbol)
  (e0:let (descriptor) (state:primitive-get name-as-symbol)
    (buffer:get descriptor (e0:value 4))))
(e1:define (state:primitive-side-effecting? name-as-symbol) ; an alias
  (state:primitive-get-side-effecting name-as-symbol))
(e1:define (state:primitive-reflective? name-as-symbol) ; an alias
  (state:primitive-get-reflective name-as-symbol))

(e1:define (state:primitive-names)
  (state:add-primitive-symbols (string-hash:string-hash->salist symbol:table) list:nil))
(e1:define (state:add-primitive-symbols salist acc)
  (e0:if-in salist (0)
    acc
    (e0:let (first-symbol) (cons:cdr (list:head salist))
      (e0:if-in (state:primitive? first-symbol) (#f)
        (state:add-primitive-symbols (list:tail salist) acc)
        (state:add-primitive-symbols (list:tail salist) (list:cons first-symbol acc))))))

(e1:define (state:primitive? primitive-name-as-symbol)
  ;; return (e0:value 0) iff the name is unbound, which is to say return its descriptor
  (state:primitive-get primitive-name-as-symbol))

;;; By using the same technique based on reflective information, we
;;; can automatically generate the procedures named "state:apply" and
;;; "state:apply-primitive".  We call such procedures "appliers", and
;;; we use a boolean parameter named "apply" to distinguish them in
;;; applier-generators.

(e1:define (state:generate-apply!)
  (state:generate-applier! (e0:value #t)))
(e1:define (state:generate-apply-primitive!)
  (state:generate-applier! (e0:value #f)))

(e1:define (state:generate-applier! apply)
  (e0:let (callee-names) (e0:if-in apply (0)
                           (state:primitive-names)
                           (state:procedure-names))
    (e0:let (default-body) (e0:call* (e0:value state:error) list:nil)
      (state:procedure-set! (e0:if-in apply (0)
                              (e0:value state:apply-primitive)
                              (e0:value state:apply))
                            (list:list2 (e0:value f) (e0:value arguments))
                            (state:applier-body* callee-names default-body apply)))))
(e1:define (state:applier-body* remaining-callee-names default-body apply)
  (e0:if-in remaining-callee-names (0)
    default-body
    (e0:let (first-callee-name) (list:head remaining-callee-names)
      ;;; We would like to return the following code, but we have no
      ;;; quasiquoting support yet:
      ;; `(e0:if-in (e0:primitive whatever-eq? (e0:variable f) (e0:value ,first-callee-name)) (0)
      ;;    ,(state:applier-body* (list:tail remaining-callee-names) default-body apply)
      ;;    ,(e0:if-in apply (0)
      ;;       (e0:primitive ,first-callee-name
      ;;                     ,@(state:extract-arguments* arguments (state:primitive-get-in-dimension first-callee-name)))
      ;;       (e0:call ,first-callee-name
      ;;                ,@(state:extract-arguments* arguments (state:procedure-get-in-dimension first-callee-name)))))
      (e0:if-in* (e0:primitive* (e0:value whatever:eq?)
                                (list:list2 (e0:variable* (e0:value f))
                                            (e0:value* first-callee-name)))
                 (list:singleton (e0:value 0))
                 (state:applier-body* (list:tail remaining-callee-names) default-body apply)
                 (e0:if-in apply (0)
                           (e0:primitive* first-callee-name
                                          (state:extract-arguments* (e0:variable* (e0:value arguments))
                                                                    (state:primitive-get-in-dimension first-callee-name)))
                           (e0:call* first-callee-name
                                     (state:extract-arguments* (e0:variable* (e0:value arguments))
                                                               (state:procedure-get-in-dimension first-callee-name))))))))

(e1:define (state:extract-arguments* argument-list-expression how-many)
  (state:extract-arguments-acc* argument-list-expression how-many list:nil))
(e1:define (state:extract-arguments-acc* argument-list-expression how-many acc)
  (e0:if-in how-many (0)
    acc
    (e0:let (nth-argument-code)
            ;; Equivalent to `(list:nth argument-list-expression ,(fixnum:1- how-many))
            (list:nth* argument-list-expression
                       (fixnum:1- how-many))
      (state:extract-arguments-acc* argument-list-expression (fixnum:1- how-many) (list:cons nth-argument-code acc)))))

;;; Return the expression for extracting the index-th element from the
;;; result of the given expression, assuming it evaluates to a single
;;; list.  The index must be a fixnum.
(e1:define (list:nth* expression element-index)
  (e0:primitive* (e0:value buffer:get)
                 (list:list2 (list:nth*-acc expression element-index)
                             (e0:value* (e0:value 0)))))
(e1:define (list:nth*-acc expression element-index)
  (e0:if-in element-index (0)
    expression
    (list:nth*-acc (e0:primitive* (e0:value buffer:get)
                                  (list:list2 expression
                                              (e0:value* (e0:value 1))))
                   (fixnum:1- element-index))))


;;;; The interpreter uses some automatically-generated code for
;;;; primitives which depends on primitive in- and out-dimension.
(e1:define (state:state-list-from-variables* variables-as-symbols)
  (e0:if-in variables-as-symbols (0)
    ;; 'list:nil
    (e0:variable* (e0:value list:nil))
    ;; `(list:cons (e0:value* ,(car variables-as-symbols))
    ;;             ,(state:state-list-from-variables* (cdr variables-as-symbols)))
    (e0:call* (e0:value list:cons)
                     (list:list2 (e0:variable* (list:head variables-as-symbols))
                                 (state:state-list-from-variables* (list:tail variables-as-symbols))))))
(e1:define (state:eval-given-primitive-into-list* primitive-name actual-values-list-expression)
  (e0:let (in-dimension) (state:primitive-get-in-dimension primitive-name)
    (e0:let (out-dimension) (state:primitive-get-out-dimension primitive-name)
      (e0:let (result-names) (symbol:fresh-symbols out-dimension)
        (e0:let* result-names
                        (e0:primitive* primitive-name
                                              (state:extract-arguments* actual-values-list-expression in-dimension))
                        (state:state-list-from-variables* result-names))))))
(e1:define (state:eval-any-primitive-into-list* primitive-name-expression actual-values-list-expression primitive-names)
  (e0:if-in primitive-names (0)
    (e0:call* (e0:value error) list:nil)
    (e0:if-in* (e0:primitive* (e0:value whatever:eq?)
                              (list:list2 primitive-name-expression
                                          (e0:value* (list:head primitive-names))))
                      (list:list1 (e0:value 0))
       (state:eval-any-primitive-into-list* primitive-name-expression actual-values-list-expression (list:tail primitive-names))
       (state:eval-given-primitive-into-list* (list:head primitive-names) actual-values-list-expression))))
(e1:define (state:generate-eval-primitive!)
  (e0:let ()
    (state:procedure-set! (e0:value state:eval-primitive-into-list)
                          (list:list2 (e0:value primitive-name)
                                      (e0:value parameters))
                          (state:eval-any-primitive-into-list* (e0:variable* (e0:value primitive-name))
                                                               (e0:variable* (e0:value parameters))
                                                               (state:primitive-names)))
    (state:procedure-set! (e0:value state:eval-primitive)
                          (list:list3 (e0:value name) (e0:value actuals) (e0:value local))
                          (e0:let* (list:list1 (e0:value actual-values))
                                   (e0:call* (e0:value e0:eval-expressions)
                                             (list:list2 (e0:variable* (e0:value actuals))
                                                         (e0:variable* (e0:value local))))
                                   (e0:call* (e0:value state:eval-primitive-into-list)
                                             (list:list2 (e0:variable* (e0:value name))
                                                         (e0:variable* (e0:value actual-values))))))))

;;;; Add primitive reflective information:

;;                                                  in-dim.      out-dim.     side effects  reflective
(state:primitive-set! (e0:value whatever:eq?)       (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value whatever:zero?)     (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value whatever:make-zero) (e0:value 0) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value buffer:get)         (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value buffer:set!)        (e0:value 3) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value buffer:initialize!) (e0:value 3) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value buffer:make-uninitialized) (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value buffer:make)        (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value buffer:destroy)     (e0:value 1) (e0:value 0) (e0:value #f) (e0:value #f)) ; no visible effects
(state:primitive-set! (e0:value fixnum:1+)          (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:1-)          (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:<)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:<=)          (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:negate)      (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:+)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:-)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:*)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:/)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:%)           (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:bitwise-not) (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:bitwise-and) (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:bitwise-or)  (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:bitwise-xor) (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:left-shift)  (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:arithmetic-right-shift) (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value fixnum:logic-right-shift) (e0:value 2) (e0:value 1) (e0:value #f) (e0:value #f))

(state:primitive-set! (e0:value whatever:duplicate) (e0:value 1) (e0:value 2) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value whatever:swap)      (e0:value 2) (e0:value 2) (e0:value #f) (e0:value #f))

(state:primitive-set! (e0:value io:standard-input)  (e0:value 0) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value io:standard-output) (e0:value 0) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value io:standard-error)  (e0:value 0) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value io:open-file)       (e0:value 2) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:close-file)      (e0:value 1) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:eof?)            (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:read-character)  (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:write-character) (e0:value 2) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:read-32-bit-big-endian) (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:write-32-bit-big-endian)(e0:value 2) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value io:readline)        (e0:value 0) (e0:value 1) (e0:value #t) (e0:value #f))

;;; FIXME: this relies on Guile, and of course I must re-implement it
(state:primitive-set! (e0:value io:read-sexpression)(e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))

(state:primitive-set! (e0:value gc:disable!)        (e0:value 0) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value gc:reenable!)       (e0:value 0) (e0:value 0) (e0:value #t) (e0:value #f))

;;; Unexecing primitives, not always available.  Those are *not* reflective: they just read boxedness tags
(state:primitive-set! (e0:value whatever:atom?)     (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value whatever:buffer?)   (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value whatever:thread?)   (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))
(state:primitive-set! (e0:value buffer:length)      (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #f))

;;; Debugging primitives.  FIXME: remove.
(state:primitive-set! (e0:value debug:dump)         (e0:value 1) (e0:value 0) (e0:value #t) (e0:value #f)) ;; FIXME: remove 
;(state:primitive-set! (e0:value debug:dump-sexpression) (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f)) ;; FIXME: remove 
(state:primitive-set! (e0:value debug:fail)         (e0:value 0) (e0:value 0) (e0:value #t) (e0:value #f)) ;; FIXME: remove 

(state:primitive-set! (e0:value primitive:call-in-c)(e0:value 2) (e0:value 1) (e0:value #t) (e0:value #t))
(state:primitive-set! (e0:value primitive:get-index) (e0:value 1) (e0:value 1) (e0:value #f) (e0:value #t))

(state:primitive-set! (e0:value marshal:marshal-to-open-file) (e0:value 2) (e0:value 0) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value marshal:unmarshal-from-open-file) (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value state:update-globals-and-procedures!) (e0:value 2) (e0:value 0) (e0:value #t) (e0:value #t))
(state:primitive-set! (e0:value e0:eval-in-c)       (e0:value 2) (e0:value 1) (e0:value #t) (e0:value #t))
(state:primitive-set! (e0:value unix:system)     (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))
(state:primitive-set! (e0:value unix:unlink)     (e0:value 1) (e0:value 1) (e0:value #t) (e0:value #f))

(state:primitive-set! (e0:value io:write-value)     (e0:value 2) (e0:value 0) (e0:value #t) (e0:value #f)) ;; FIXME: remove after bootstrapping from Guile

;;; Generate the (proof-of-concept: a more efficent version,
;;; e0:primitive is availabel, itself mostly implemented as a
;;; primitive) procedure state:eval-primitive for evaluating
;;; primitives in the interpreter:
;; (state:generate-eval-primitive!) ;; FIXME: I've disabled this: it's slow and now useless, since I have the C version


;;;;; epsilon0 self-interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Now that we defined expressions we can just implement
;;;; environments as alists and define a first epsilon0 eval
;;;; procedure.  This interpreter, written in epsilon0, will only
;;;; serve for bootstrapping, hence it will be ugly and inefficient.
;;;; We will replace it later after some linguistic extensions.

;; (define (show e local)
;;   (print-guile-string "e0:eval: evaluating ")
;;   (print-expression e)
;;   (print-guile-string " in ")
;;   (print-environment local)
;;   (print-guile-string "\n"))

;; (define (PRIMITIVE-debug e local)
;;   ;;(when (whatever->guile-boolean (e0:expression-call? e))
;;    (show e local)
;; ;;    42)
;;   (e0:value 42))

(e1:define (e0:eval e local)
;; (e0:let () (e0:primitive debug e local)
  (e0:if-in (e0:expression-variable? e) (#t)
    (e0:let (h name) (e0:expression-variable-explode e)
      (e0:eval-variable name local))
    (e0:if-in (e0:expression-value? e) (#t)
      (e0:let (h content) (e0:expression-value-explode e)
        (e0:eval-value content))
      (e0:if-in (e0:expression-bundle? e) (#t)
        (e0:let (h items) (e0:expression-bundle-explode e)
          (e0:eval-bundle items local))
        (e0:if-in (e0:expression-primitive? e) (#t)
          (e0:let (h name actuals) (e0:expression-primitive-explode e)
            (e0:eval-primitive name actuals local))
          (e0:if-in (e0:expression-let? e) (#t)
            (e0:let (h bound-variables bound-expression body) (e0:expression-let-explode e)
              (e0:eval-let bound-variables bound-expression body local))
            (e0:if-in (e0:expression-call? e) (#t)
              (e0:let (h name actuals) (e0:expression-call-explode e)
                (e0:eval-call name actuals local))
              (e0:if-in (e0:expression-call-indirect? e) (#t)
                (e0:let (h procedure-expression actuals) (e0:expression-call-indirect-explode e)
                  (e0:eval-call-indirect procedure-expression actuals local))
                (e0:if-in (e0:expression-if-in? e) (#t)
                  (e0:let (h discriminand values then-branch else-branch) (e0:expression-if-in-explode e)
                    (e0:eval-if-in discriminand values then-branch else-branch local))
                  (e0:if-in (e0:expression-fork? e) (#t)
                    (e0:let (h name actuals) (e0:expression-fork-explode e)
                      (e0:eval-fork name actuals local))
                    (e0:if-in (e0:expression-join? e) (#t)
                      (e0:let (h future) (e0:expression-join-explode e)
                        (e0:eval-join future local))
                      (e0:if-in (e0:expression-extension? e) (#t)
                        (e0:let (h name subexpressions) (e0:expression-extension-explode e)
                          (e0:eval-extension name subexpressions local))
                        (e1:error (e0:value "impossible"))))))))))))))
;; )
(e1:define (e0:unbundle bundle)
  (e0:if-in (list:null? bundle) (#f)
    (e0:if-in (list:null? (list:tail bundle)) (#f)
      (e1:error (e0:value "e0:unbundle: the bundle has at least two elements"))
      (list:head bundle))
    (e1:error (e0:value "e0:unbundle: empty bundle"))))

(e1:define (e0:eval-ee e)
  (e0:eval e alist:nil))

;;; Eval the given expresisons, each of which must return a
;;; 1-dimension bundle, and return the n-dimensioned concatenation of
;;; such result bundles:
(e1:define (e0:eval-expressions expressions local)
  (e0:if-in expressions (0)
    list:nil
    (list:cons (e0:unbundle (e0:eval (list:head expressions) local))
               (e0:eval-expressions (list:tail expressions) local))))

(e1:define (e0:eval-variable name local)
  (list:singleton (e0:if-in (alist:has? local name) (#f)
                    (state:global-get name)
                    (alist:lookup local name))))

(e1:define (e0:eval-value content)
  (list:singleton content))

(e1:define (e0:eval-bundle items local)
  (e0:eval-expressions items local))

(e1:define (e0:eval-primitive name-as-symbol actuals local)
  (e0:let (actual-values) (e0:eval-expressions actuals local)
    (e0:primitive primitive:call-in-c
                  (symbol:symbol->string name-as-symbol)
                  actual-values)))

(e1:define (e0:eval-let bound-variables bound-expression body local)
  (e0:let (bound-expression-results) (e0:eval bound-expression local)
    (e0:eval-with-let-bindings bound-variables bound-expression-results body local)))

;;; Eval the given body in the given environment extended by binding
;;; the given local variables to the first givel local values; if
;;; values are more than variables, ignore the remaining ones.  If
;;; variables are more than values, it's an error.  Return the body
;;; results.
(e1:define (e0:eval-with-let-bindings local-variables local-values body local)
  (e0:if-in local-variables (0)
    (e0:eval body local)
    (e0:if-in local-values (0)
      (e1:error (e0:value "e0:eval-with-let-bindings: variables are more than results"))
      (e0:eval-with-let-bindings (list:tail local-variables)
                                 (list:tail local-values)
                                 body
                                 (alist:bind local
                                             (list:head local-variables)
                                             (list:head local-values))))))

(e1:define (e0:eval-call name actuals local)
  (e0:if-in (whatever:eq? name (e0:value e0:eval)) (#f)
    (e0:eval-non-eval-call name actuals local)
    (e0:eval-eval-call actuals local)))

(e1:define (e0:eval-non-eval-call name actuals local)
  (e0:let (actual-values) (e0:eval-expressions actuals local)
    (e0:if-in (state:procedure? name) (#f)
      (e1:errors (e0:value "e0:eval-non-eval-call: not a procedure name") name)
      (e0:let (formals) (state:procedure-get-formals name)
        (e0:let (body) (state:procedure-get-body name)
          (e0:if-in (whatever:eq? (list:length formals) (list:length actual-values)) (0)
            (e1:errors (e0:value "e0:eval-non-eval-call: in-dimension mismatch") name) ;;(list:length formals) (list:length actual-values))
            (e0:eval body
                     (alist:bind-lists-unsafe alist:nil ;; nonlocals must not be visible
                                              formals
                                              actual-values))))))))

;; (define (PRIMITIVE-debug-eval-call expression local)
;;   (format #t "Evaluating an eval call:\n")
;;   (print-expression_ expression)
;;   (print-environment_ local)
;;   ;;(dump (list:length local))
;;   ;;(dump (^ (cons:cdr (list:head local))))
;;   (e0:value 42))

;; (define (PRIMITIVE-debug-eval-results results)
;;   (format #t "eval results:\n")
;;   (dump results)
;;   (e0:value 42))

;; (define (PRIMITIVE-debug:dump-sexpression stuff)
;;   (dump (^ stuff))
;;   (e0:value 42))

(e1:define (e0:eval-eval-call actuals local)
  (e0:let (actual-values) (e0:eval-expressions actuals local)
    (e0:if-in (whatever:eq? (list:length actual-values) (e0:value 2)) (#f)
      (e1:error (e0:value "e0:eval-eval-call: in-dimension mismatch") (list:length actual-values) actual-values)
      (e0:let (expression) (list:head actual-values)
        (e0:let (local) (list:head (list:tail actual-values))
          ;;; We need to wrap the result into a list, to simulate what
          ;;; would happen with a meta-eval returning a meta-bundle of
          ;;; values, represented as a single list:
          (list:singleton (e0:eval expression local)))))))
(e1:define (e0:eval-call-indirect procedure-expression actuals local)
  ;; In this interpreted version, a procedure "pointer" is just a procedure name
  (e0:let (procedure-name) (e0:unbundle (e0:eval procedure-expression local))
    (e0:eval-call procedure-name actuals local)))

(e1:define (e0:eval-if-in discriminand values then-branch else-branch local)
  (e0:let (discriminand-value) (e0:unbundle (e0:eval discriminand local))
    (e0:if-in (list:memq discriminand-value values) (#f)
      (e0:eval else-branch local) 
      (e0:eval then-branch local))))

(e1:define (e0:eval-fork name actuals local)
  (e0:let (actual-values) (e0:eval-expressions actuals local)
    (e0:if-in (state:procedure? name) (0)
      (e1:errors (e0:value "e0:eval-fork: not a procedure name") name)
      (e0:let (formals) (state:procedure-get-formals name)
        (e0:if-in (whatever:eq? (list:length formals) ;; the first formal is for the thread id
                                (fixnum:1+ (list:length actual-values))) (0)
          (e1:errors (e0:value "e0:eval-fork: in-dimension mismatch") name)
          (e0:let (body) (state:procedure-get-body name)
            (list:singleton (e0:fork e0:eval-from-new-thread
                                     body
                                     formals
                                     actual-values))))))))

(e1:define (e0:eval-from-new-thread this-thread body formals actual-values)
  (e0:unbundle (e0:eval body
                        (alist:bind-lists-unsafe alist:nil
                                                 formals
                                                 (list:cons this-thread actual-values)))))

(e1:define (e0:eval-join future local)
  (e0:let (future-value) (e0:unbundle (e0:eval future local))
    (list:singleton (e0:join future-value))))

;(e1:define (e0:eval-extension name subexpressions local)
;  (e1:errors (e0:value "e0:eval-extension") name))


;;;;; Type table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; "Base" types are actually user-defined, and the user has to
;;;; specify some behaviour before being able to use a base type in a
;;;; dynamically-typed context -- which is to say, as an s-expression
;;;; case.

(e1:define sexpression:tag-generator-box
  (box:make-initialized (e0:value 0)))
(e1:define (sexpression:make-tag)
  (box:get-and-bump! sexpression:tag-generator-box))

;;; A type descriptor is a buffer containing the following elements, in order:
;;; - tag
;;; - type name, as a string
;;; - printer procedure name
;;; - pretty-printer procedure name
;;; - quoter procedure name
;;; - quasiquoter procedure name
;;; - expression expander procedure name
;;; - alist, for user-defined fields

;;; A table mapping tags into descriptors:
(e1:define sexpression:type-table
  (unboxed-hash:make))

;;; Another table mapping names (as strings) into descriptors.
;;; Descriptors are shared with the table above.:
(e1:define sexpression:name-to-type-table
  (unboxed-hash:make))

;;; Add the given information to tables, and return the automatically-generated type tag:
(e1:define (sexpression:define-base-type name-as-string
                                         printer-procedure-name
                                         pretty-printer-procedure-name
                                         quoter-procedure-name
                                         quasiquoter-procedure-name
                                         expression-expander-procedure-name
                                         alist)
  (e0:let (tag) (sexpression:make-tag)
    ;; Make and fill the descriptor:
    (e0:let (descriptor) (buffer:make (e0:value 8))
      (e0:let () (buffer:set! descriptor (e0:value 0) tag)
        (e0:let () (buffer:set! descriptor (e0:value 1) name-as-string)
          (e0:let () (buffer:set! descriptor (e0:value 2) printer-procedure-name)
            (e0:let () (buffer:set! descriptor (e0:value 3) pretty-printer-procedure-name)
              (e0:let () (buffer:set! descriptor (e0:value 4) quoter-procedure-name)
                (e0:let () (buffer:set! descriptor (e0:value 5) quasiquoter-procedure-name)
                  (e0:let () (buffer:set! descriptor (e0:value 6) expression-expander-procedure-name)
                    (e0:let () (buffer:set! descriptor (e0:value 7) alist)
                      ;; Add entries in our two tables:
                      (e0:let () (unboxed-hash:set! sexpression:type-table tag descriptor)
                        (e0:let () (string-hash:set! sexpression:name-to-type-table name-as-string descriptor)
                          tag)))))))))))))

;;; Selectors:
(e1:define (sexpression:type-tag->type-descriptor tag)
  (unboxed-hash:get sexpression:type-table tag))
(e1:define (sexpression:type-tag->name-as-string tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 1)))
(e1:define (sexpression:type-tag->printer-procedure-name tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 2)))
(e1:define (sexpression:type-tag->pretty-printer-procedure-name tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 3)))
(e1:define (sexpression:type-tag->quoter-procedure-name tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 4)))
(e1:define (sexpression:type-tag->quasiquoter-procedure-name tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 5)))
(e1:define (sexpression:type-tag->expression-expander-procedure-name tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 6)))
(e1:define (sexpression:type-tag->alist tag)
  (buffer:get (sexpression:type-tag->type-descriptor tag) (e0:value 7)))
(e1:define (sexpression:type-tag->user-defined-field tag field-name)
  (e0:let (alist) (sexpression:type-tag->alist tag)
    (alist:lookup alist field-name)))

;;; Add type informations for the types we already defined:
(e1:define sexpression:empty-list-tag
  (sexpression:define-base-type (e0:value "empty-list")
                                (e0:value printer:write-empty-list)
                                (e0:value pp:empty-list)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:boolean-tag
  (sexpression:define-base-type (e0:value "boolean")
                                (e0:value printer:write-boolean)
                                (e0:value pp:boolean)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:fixnum-tag
  (sexpression:define-base-type (e0:value "fixnum")
                                (e0:value printer:write-fixnum)
                                (e0:value pp:fixnum)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:cons-tag
  (sexpression:define-base-type (e0:value "cons")
                                (e0:value printer:write-cons)
                                (e0:value pp:scons)
                                (e0:value sexpression:cons-quoter)
                                (e0:value sexpression:cons-quasiquoter)
                                (e0:value sexpression:cons-expression-expander)
                                alist:nil))
(e1:define sexpression:character-tag
  (sexpression:define-base-type (e0:value "character")
                                (e0:value printer:write-character)
                                (e0:value pp:character)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:string-tag
  (sexpression:define-base-type (e0:value "string")
                                (e0:value printer:write-string)
                                (e0:value pp:string)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:symbol-tag
  (sexpression:define-base-type (e0:value "symbol")
                                (e0:value printer:write-symbol)
                                (e0:value pp:symbol)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:variable-expression-expander)
                                alist:nil))
(e1:define sexpression:fixed-point-tag
  (sexpression:define-base-type (e0:value "fixed-point")
                                (e0:value printer:write-fixed-point)
                                (e0:value pp:fixed-point)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:literal-expression-expander)
                                alist:nil))
(e1:define sexpression:expression-tag
  (sexpression:define-base-type (e0:value "expression")
                                (e0:value printer:write-expression)
                                (e0:value pp:expression)
                                (e0:value sexpression:leaf-quoter)
                                (e0:value sexpression:leaf-quasiquoter)
                                (e0:value sexpression:expression-expression-expander)
                                alist:nil))


;;;;; Expression expanders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return the given object as a literal constant expression:
(e1:define (sexpression:literal-expression-expander whatever)
  (e0:value* whatever))

;;; Return the given symbol as a variable expression:
(e1:define (sexpression:variable-expression-expander symbol)
  (e0:variable* symbol))

;;; How to expand a cons of two s-expressions into an expression
;;; (first version, with no higher-order support; this can be replaced
;;; later, of course).
(e1:define (sexpression:cons-expression-expander cons)
  (e0:let (car-sexpression) (cons:car cons)
    (e0:if-in (sexpression:symbol? car-sexpression) (#f)
      ;;(error (e0:value "cons:expression-expander: the car is not a symbol") car-sexpression (^ car-sexpression))
      (e1:error (e0:value "cons:expression-expander: the car is not a symbol"))
      (e0:let (car-symbol) (sexpression:eject-symbol car-sexpression)
        (e0:if-in (state:macro? car-symbol) (#f)
          ;; If the car is a symbol which is not a macro name, expand
          ;; to a procedure call:
          (e0:call* car-symbol (e1:macroexpand-sexpressions (cons:cdr cons)))
          (e1:macroexpand-macro-call car-symbol (cons:cdr cons)))))))

;;; How to "expand" an expression (just in case it was ever needed):
;;; we simply return the expression itself:
(e1:define (sexpression:expression-expression-expander expression)
  expression)


;;;;; S-expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; S-expressions are tagged values represented by a three-element
;;;; buffer, whose first element contains a tag, whose second element
;;;; contains a case-dependent representation, and the third one
;;;; either a locus or 0.  This implementation is not very efficient:
;;;; *all* cases are boxed.  S-expressions should be safe to use:
;;;; operations should always check that the operands have the right
;;;; cases.

;;;; By convention, values containing sub-values, when wrapped as
;;;; s-expression, only contain s-expression sub-values, so that we
;;;; can have tagging at all levels for what constitutes in fact a
;;;; dynamically-typed object.

(e1:define (sexpression:make-with-locus tag value locus)
  (e0:let (result) (buffer:make (e0:value 3))
    (e0:let () (buffer:set! result (e0:value 0) tag)
      (e0:let () (buffer:set! result (e0:value 1) value)
        (e0:let () (buffer:set! result (e0:value 2) locus)
          result)))))
(e1:define (sexpression:make-without-locus tag value)
  (sexpression:make-with-locus tag value (e0:value 0)))

;;; Return a new s-expression with the same tag and value as the given
;;; sexpression, but with the given locus instead of the original one:
(e1:define (sexpression:with-locus sexpression locus)
  (sexpression:make-with-locus (sexpression:get-tag sexpression)
                               (sexpression:eject sexpression)
                               locus))

(e1:define (sexpression:make tag value)
  (sexpression:make-without-locus tag value))
(e1:define (sexpression:get-tag sexpression)
  (buffer:get sexpression (e0:value 0)))
(e1:define (sexpression:eject sexpression)
  (buffer:get sexpression (e0:value 1)))
(e1:define (sexpression:get-locus sexpression)
  (buffer:get sexpression (e0:value 2)))
(e1:define (sexpression:set-locus! sexpression locus)
  (buffer:set! sexpression (e0:value 2) locus))
(e1:define (sexpression:has-tag? x tag)
  (whatever:eq? (sexpression:get-tag x) tag))

(e1:define (sexpression:null? x)
  (sexpression:has-tag? x sexpression:empty-list-tag))
(e1:define (sexpression:boolean? x)
  (sexpression:has-tag? x sexpression:boolean-tag))
(e1:define (sexpression:character? x)
  (sexpression:has-tag? x sexpression:character-tag))
(e1:define (sexpression:fixnum? x)
  (sexpression:has-tag? x sexpression:fixnum-tag))
(e1:define (sexpression:string? x)
  (sexpression:has-tag? x sexpression:string-tag))
(e1:define (sexpression:symbol? x)
  (sexpression:has-tag? x sexpression:symbol-tag))
(e1:define (sexpression:cons? x)
  (sexpression:has-tag? x sexpression:cons-tag))
(e1:define (sexpression:expression? x)
  (sexpression:has-tag? x sexpression:expression-tag))

(e1:define (sexpression:car-of-singleton x)
  (e0:if-in (sexpression:cons? x) (#f)
    (e1:error (e0:value "sexpression:car-of-singleton: not a cons"))
    (e0:if-in (sexpression:null? (sexpression:cdr x)) (#f)
      (e1:error (e0:value "sexpression:car-of-singleton: not a singleton"))
      (cons:get-car (sexpression:eject x)))))

(e1:define sexpression:nil
  (sexpression:make sexpression:empty-list-tag empty-list:empty-list))
(e1:define (sexpression:car x)
  (e0:if-in (sexpression:cons? x) (#f)
    (e1:error (e0:value "sexpression:car: not a cons"))
    (cons:get-car (sexpression:eject x))))
(e1:define (sexpression:cdr x)
  (e0:if-in (sexpression:cons? x) (#f)
    (e1:error (e0:value "sexpression:cdr: not a cons"))
    (cons:get-cdr (sexpression:eject x))))

(e1:define (sexpression:set-car! c new-car)
  (e0:if-in (sexpression:cons? c) (#f)
    (e1:error (e0:value "sexpression:set-car!: not a cons"))
    (cons:set-car! (sexpression:eject c) new-car)))
(e1:define (sexpression:set-cdr! c new-cdr)
  (e0:if-in (sexpression:cons? c) (#f)
    (e1:error (e0:value "sexpression:set-cdr!: not a cons"))
    (cons:set-cdr! (sexpression:eject c) new-cdr)))

(e1:define (sexpression:cons x y)
  (sexpression:make sexpression:cons-tag
                    (cons:make x y)))
(e1:define (sexpression:cons-with-locus x y locus)
  (sexpression:make-with-locus sexpression:cons-tag
                               (cons:make x y)
                               locus))
(e1:define (sexpression:singleton x)
  (sexpression:make sexpression:cons-tag
                    (cons:make x sexpression:nil)))
(e1:define (sexpression:eject-empty-list x)
  (e0:if-in (sexpression:null? x) (#f)
    (e1:error (e0:value "sexpression:eject-empty-list: not a empty-list"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-boolean x)
  (e0:if-in (sexpression:boolean? x) (#f)
    (e1:error (e0:value "sexpression:eject-boolean: not a boolean"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-fixnum x)
  (e0:if-in (sexpression:fixnum? x) (#f)
    (e1:error (e0:value "sexpression:eject-fixnum: not a fixnum"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-character x)
  (e0:if-in (sexpression:character? x) (#f)
    (e1:error (e0:value "sexpression:eject-character: not a character"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-symbol x)
  (e0:if-in (sexpression:symbol? x) (#f)
(e0:let () (e0:primitive debug:dump x)
    (e1:error (e0:value "sexpression:eject-symbol: not a symbol"))
)
    (sexpression:eject x)))
(e1:define (sexpression:eject-string x)
  (e0:if-in (sexpression:string? x) (#f)
    (e1:error (e0:value "sexpression:eject-string: not a string"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-cons x)
  (e0:if-in (sexpression:cons? x) (#f)
    (e1:error (e0:value "sexpression:eject-cons: not a cons"))
    (sexpression:eject x)))
(e1:define (sexpression:eject-expression x)
  (e0:if-in (sexpression:expression? x) (#f)
;;(e0:let () (e0:primitive debug:dump (^ x))
    (e1:error (e0:value "sexpression:eject-expression: not an expression"));; x)
;;)
    (sexpression:eject x)))

(e1:define (sexpression:inject-empty-list x)
  (sexpression:make sexpression:empty-list-tag x))
(e1:define (sexpression:inject-boolean x)
  (sexpression:make sexpression:boolean-tag x))
(e1:define (sexpression:inject-fixnum x)
  (sexpression:make sexpression:fixnum-tag x))
(e1:define (sexpression:inject-character x)
  (sexpression:make sexpression:character-tag x))
(e1:define (sexpression:inject-symbol x)
  (sexpression:make sexpression:symbol-tag x))
(e1:define (sexpression:inject-string x)
  (sexpression:make sexpression:string-tag x))
(e1:define (sexpression:inject-cons x)
  (sexpression:make sexpression:cons-tag x))
(e1:define (sexpression:inject-expression x)
  (sexpression:make sexpression:expression-tag x))

;;; Make an s-list of s-symbols from a list of symbols:
(e1:define (sexpression:inject-symbols symbols)
  (e0:if-in symbols (0)
    sexpression:nil
    (sexpression:cons (sexpression:inject-symbol (list:head symbols))
                      (sexpression:inject-symbols (list:tail symbols)))))
(e1:define (sexpression:fresh-symbol)
  (sexpression:inject-symbol (symbol:fresh)))

;;; Return an s-list of fresh s-symbols:
(e1:define (sexpression:fresh-symbols how-many)
  (sexpression:fresh-symbols-acc how-many sexpression:nil))
(e1:define (sexpression:fresh-symbols-acc how-many acc)
  (e0:if-in how-many (0)
    acc
    (e0:let (sexpression) (sexpression:fresh-symbol)
      (sexpression:fresh-symbols-acc (fixnum:1- how-many)
                                     (sexpression:cons sexpression acc)))))

;;; Make an s-list of expression sexpressions from a list of expressions:
(e1:define (sexpression:inject-expressions expressions)
  (e0:if-in expressions (0)
    sexpression:nil
    (sexpression:cons (sexpression:inject-expression (list:head expressions))
                      (sexpression:inject-expressions (list:tail expressions)))))

(e1:define (sexpression:caar x) (sexpression:car (sexpression:car x)))
(e1:define (sexpression:cadr x) (sexpression:car (sexpression:cdr x)))
(e1:define (sexpression:cdar x) (sexpression:cdr (sexpression:car x)))
(e1:define (sexpression:cddr x) (sexpression:cdr (sexpression:cdr x)))
(e1:define (sexpression:caaar x) (sexpression:car (sexpression:car (sexpression:car x))))
(e1:define (sexpression:caadr x) (sexpression:car (sexpression:car (sexpression:cdr x))))
(e1:define (sexpression:cadar x) (sexpression:car (sexpression:cdr (sexpression:car x))))
(e1:define (sexpression:caddr x) (sexpression:car (sexpression:cdr (sexpression:cdr x))))
(e1:define (sexpression:cdaar x) (sexpression:cdr (sexpression:car (sexpression:car x))))
(e1:define (sexpression:cdadr x) (sexpression:cdr (sexpression:car (sexpression:cdr x))))
(e1:define (sexpression:cddar x) (sexpression:cdr (sexpression:cdr (sexpression:car x))))
(e1:define (sexpression:cdddr x) (sexpression:cdr (sexpression:cdr (sexpression:cdr x))))
(e1:define (sexpression:caaaar x) (sexpression:car (sexpression:car (sexpression:car (sexpression:car x)))))
(e1:define (sexpression:caaadr x) (sexpression:car (sexpression:car (sexpression:car (sexpression:cdr x)))))
(e1:define (sexpression:caadar x) (sexpression:car (sexpression:car (sexpression:cdr (sexpression:car x)))))
(e1:define (sexpression:caaddr x) (sexpression:car (sexpression:car (sexpression:cdr (sexpression:cdr x)))))
(e1:define (sexpression:cadaar x) (sexpression:car (sexpression:cdr (sexpression:car (sexpression:car x)))))
(e1:define (sexpression:cadadr x) (sexpression:car (sexpression:cdr (sexpression:car (sexpression:cdr x)))))
(e1:define (sexpression:caddar x) (sexpression:car (sexpression:cdr (sexpression:cdr (sexpression:car x)))))
(e1:define (sexpression:cadddr x) (sexpression:car (sexpression:cdr (sexpression:cdr (sexpression:cdr x)))))
(e1:define (sexpression:cdaaar x) (sexpression:cdr (sexpression:car (sexpression:car (sexpression:car x)))))
(e1:define (sexpression:cdaadr x) (sexpression:cdr (sexpression:car (sexpression:car (sexpression:cdr x)))))
(e1:define (sexpression:cdadar x) (sexpression:cdr (sexpression:car (sexpression:cdr (sexpression:car x)))))
(e1:define (sexpression:cdaddr x) (sexpression:cdr (sexpression:car (sexpression:cdr (sexpression:cdr x)))))
(e1:define (sexpression:cddaar x) (sexpression:cdr (sexpression:cdr (sexpression:car (sexpression:car x)))))
(e1:define (sexpression:cddadr x) (sexpression:cdr (sexpression:cdr (sexpression:car (sexpression:cdr x)))))
(e1:define (sexpression:cdddar x) (sexpression:cdr (sexpression:cdr (sexpression:cdr (sexpression:car x)))))
(e1:define (sexpression:cddddr x) (sexpression:cdr (sexpression:cdr (sexpression:cdr (sexpression:cdr x)))))

;;; These simple operations over lists come in handy for macros:
(e1:define (sexpression:list1 x)
  (sexpression:cons x sexpression:nil))
(e1:define (sexpression:list2 x y)
  (sexpression:cons x (sexpression:cons y sexpression:nil)))
(e1:define (sexpression:list3 x y z)
  (sexpression:cons x (sexpression:cons y (sexpression:cons z sexpression:nil))))
(e1:define (sexpression:list4 x y z t)
  (sexpression:cons x (sexpression:cons y (sexpression:cons z (sexpression:cons t sexpression:nil)))))
(e1:define (sexpression:singleton x)
  (sexpression:list1 x))
(e1:define (sexpression:reverse x)
  (sexpression:append-reversed x sexpression:nil))
(e1:define (sexpression:append-reversed x y)
  (e0:if-in (sexpression:null? x) (#f)
    (sexpression:append-reversed (sexpression:cdr x)
                                 (sexpression:cons (sexpression:car x) y))
    y))
(e1:define (sexpression:append2 x y)
  (sexpression:append-reversed (sexpression:reverse x) y))
(e1:define (sexpression:append3 x y z)
  (sexpression:append2 x (sexpression:append2 y z)))
(e1:define (sexpression:append4 x y z t)
  (sexpression:append2 x (sexpression:append3 y z t)))
(e1:define (sexpression:length x)
  (sexpression:length-acc x (e0:value 0)))
(e1:define (sexpression:length-acc sexpression acc)
  (e0:if-in (sexpression:null? sexpression) (#f)
    (sexpression:length-acc (sexpression:cdr sexpression) (fixnum:1+ acc))
    acc))

(e1:define (sexpression:flatten xs)
  (sexpression:reverse (sexpression:flatten-acc xs sexpression:nil)))
(e1:define (sexpression:flatten-acc xs acc)
  (e0:if-in (sexpression:null? xs) (#f)
    (sexpression:flatten-acc (sexpression:cdr xs)
                             (sexpression:append-reversed (sexpression:car xs) acc))
    acc))
(e1:define (sexpression:list? xs)
  (e0:if-in (sexpression:null? xs) (#f)
    ;; Not (); is it a cons?
    (e0:if-in (sexpression:cons? xs) (#f)
      (e0:value #f)
      ;; Yes, xs is a cons
      (sexpression:list? (sexpression:cdr xs)))
    ;; xs is ()
    (e0:value #t)))

(e1:define (sexpression:n-times n sexpression)
  (sexpression:n-times-acc n sexpression sexpression:nil))
(e1:define (sexpression:n-times-acc n sexpression acc)
  (e0:if-in n (0)
    acc
    (sexpression:n-times-acc (fixnum:1- n) sexpression (sexpression:cons sexpression acc))))

;;; Some simple arithmetics is useful on injected fixnum:
(e1:define (sexpression:zero? x) ;; the result is unboxed
  (whatever:zero? (sexpression:eject-fixnum x)))
(e1:define (sexpression:1+ x)
  (sexpression:inject-fixnum (fixnum:1+ (sexpression:eject-fixnum x))))
(e1:define (sexpression:1- x)
  (sexpression:inject-fixnum (fixnum:1- (sexpression:eject-fixnum x))))
(e1:define (sexpression:+ x y)
  (sexpression:inject-fixnum (fixnum:+ (sexpression:eject-fixnum x)
                                       (sexpression:eject-fixnum y))))
(e1:define (sexpression:- x y)
  (sexpression:inject-fixnum (fixnum:- (sexpression:eject-fixnum x)
                                       (sexpression:eject-fixnum y))))

;;; Convert from s-list to list of sexpressions
(e1:define (sexpression:eject-list x)
  (e0:if-in (sexpression:null? x) (#f)
    ;; x is not ()
    (e0:if-in (sexpression:cons? x) (#f)
      ;; x is not a cons
      (e1:error (e0:value "sexpression:eject-list: not an s-list"));; x)
      ;; x is a cons
      (list:cons (sexpression:car x)
                 (sexpression:eject-list (sexpression:cdr x))))
    ;; x is ()
    list:nil))

;;; Convert from s-list of sexpression symbols to list of symbols
(e1:define (sexpression:eject-symbols x)
  (e0:if-in (sexpression:null? x) (#f)
    ;; x is not ()
    (e0:if-in (sexpression:cons? x) (#f)
      ;; x is not a cons
      (e1:error (e0:value "sexpression:eject-symbols: not an s-list"));; x)
      ;; x is a cons
      (list:cons (sexpression:eject-symbol (sexpression:car x))
                 (sexpression:eject-symbols (sexpression:cdr x))))
    ;; x is ()
    list:nil))

;;; Convert from s-list to list of whatever
(e1:define (sexpression:eject-whatevers x)
  (e0:if-in (sexpression:null? x) (#f)
    ;; x is not ()
    (e0:if-in (sexpression:cons? x) (#f)
      ;; x is not a cons
      (e1:error (e0:value "sexpression:eject-whatevers: not an s-list"));; x)
      ;; x is a cons
      (list:cons (sexpression:eject (sexpression:car x))
                 (sexpression:eject-whatevers (sexpression:cdr x))))
    ;; x is ()
    list:nil))

;;; Convert an sexpression list into an sexpression s-list:
(e1:define (sexpression:inject-sexpressions xs)
  (e0:if-in xs (0)
    sexpression:nil
    (sexpression:cons (list:head xs)
                      (sexpression:inject-sexpressions (list:tail xs)))))

(e1:define sexpression:true
  (sexpression:inject-boolean (e0:value #t)))
(e1:define sexpression:false
  (sexpression:inject-boolean (e0:value #f)))

(e1:define (sexpression:eq? s1 s2)
  ;; We don't have a good e1:and macro yet, and we don't want to use
  ;; boolean:and2 for efficiency's sake.
  (e0:if-in (whatever:eq? (sexpression:get-tag s1)
                          (sexpression:get-tag s2))
            (#f)
    (e0:value #f)
    (whatever:eq? (sexpression:eject s1)
                  (sexpression:eject s2))))


;;;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (state:macro-set! macro-name macro-body-sexpression) ;; the cdr-formal name is always "arguments"
  (e0:let ()
    ;; If we're re-defining an existing macro, invalidate its previous
    ;; macro-procedure-name, if any:
    (e0:if-in (buffer:get macro-name (e0:value 5)) (0)
      (e0:bundle)
      (state:invalidate-macro-procedure-name-cache-of! macro-name))
    ;; Store the new version:
    (buffer:set! macro-name (e0:value 5) macro-body-sexpression)))
(e1:define (state:macro-get-body macro-name)
  (buffer:get macro-name (e0:value 5)))

;;; A singleton list containing the symbol named "arguments".  Useful
;;; for macro formal parameters.
(e1:define state:arguments-list
  (list:singleton (e0:value arguments)))

;;; Build the macro procedure for the given macro name, and return
;;; the procedure name:
(e1:define (state:macro-get-macro-procedure-name-ignoring-cache macro-name)
  (e0:let (body-as-sexpression) (state:macro-get-body macro-name)
    (e0:let (untransformed-name) (symbol:fresh)
      (e0:let (untransformed-formals) state:arguments-list
  ;; (e0:let () (string:write "+ Caching ")
  ;; (e0:let () (string:write (symbol:symbol->string macro-name))
  ;; (e0:let () (string:write " macroexpansion...\n")
        (e0:let (untransformed-body) (e1:macroexpand body-as-sexpression)
  ;; (e0:let () (string:write "  + Transforming the macro procedure of ")
  ;; (e0:let () (string:write (symbol:symbol->string macro-name))
  ;; (e0:let () (string:write " macroexpansion: begin\n")
          (e0:let () (state:procedure-set! untransformed-name
                                           untransformed-formals
                                           untransformed-body)
            (e0:let (transformed-name transformed-formals transformed-body)
                    (transform:transform-procedure untransformed-name
                                                   untransformed-formals
                                                   untransformed-body)
    ;; (e0:let () (string:write "  - Transforming the macro procedure of ")
    ;; (e0:let () (string:write (symbol:symbol->string macro-name))
    ;; (e0:let () (string:write " macroexpansion: end\n")
              (e0:let () (state:procedure-set! transformed-name
                                               transformed-formals
                                               transformed-body)
                (e0:let () (buffer:set! macro-name (e0:value 6) transformed-name)
    ;; (e0:let () (string:write "- Caching ")
    ;; (e0:let () (string:write (symbol:symbol->string macro-name))
    ;; (e0:let () (string:write " macroexpansion: done.\n")
                  transformed-name)))))))))
;; ))))))))))))
;;; Return the macro procedure name for the given macro name, re-using
;;; the existing version if any.
(e1:define (state:macro-get-macro-procedure-name macro-name)
  (e0:let (cached-macro-procedure-name-or-zero) (buffer:get macro-name (e0:value 6))
    (e0:if-in cached-macro-procedure-name-or-zero (0)
      (state:macro-get-macro-procedure-name-ignoring-cache macro-name)
      cached-macro-procedure-name-or-zero)))

(e1:define (state:macro? name)
  ;; return (e0:value 0) iff the macro is unbound, which is to say return its body
  (state:macro-get-body name))
(e1:define (state:macro-names)
  (state:add-macro-symbols (string-hash:string-hash->salist symbol:table) list:nil))
(e1:define (state:add-macro-symbols salist acc)
  (e0:if-in salist (0)
    acc
    (e0:let (first-symbol) (cons:cdr (list:head salist))
      (e0:if-in (state:macro? first-symbol) (#f)
        (state:add-macro-symbols (list:tail salist) acc)
        (state:add-macro-symbols (list:tail salist) (list:cons first-symbol acc))))))
(e1:define (state:invalidate-macro-procedure-name-cache!)
  (state:invalidate-macro-procedure-name-cache-of-macro-names! (state:macro-names)))
(e1:define (state:invalidate-macro-procedure-name-cache-of! macro-name)
  (e0:let (old-procedure-name) (buffer:get macro-name (e0:value 6))
    (e0:if-in old-procedure-name (0)
      ;; There is no procedure associated: nothing to do
      (e0:bundle)
      ;; Invalidate the *procedure* definition corresponding to the old name
      (e0:let () (buffer:set! old-procedure-name (e0:value 3) list:nil)
        (e0:let () (buffer:set! old-procedure-name (e0:value 4) (e0:value 0))
          (buffer:set! macro-name (e0:value 6) (e0:value 0)))))))
(e1:define (state:invalidate-macro-procedure-name-cache-of-macro-names! remaining-macro-names)
  (e0:if-in remaining-macro-names (0)
    (e0:bundle)
    (e0:let () (state:invalidate-macro-procedure-name-cache-of! (list:head remaining-macro-names))
      (state:invalidate-macro-procedure-name-cache-of-macro-names! (list:tail remaining-macro-names)))))

;;; Macroexpand one call to the given macro just once, yielding an
;;; s-expression:
(e1:define (e1:macroexpand-1-macro-call symbol arguments)
  (e0:let (macro-procedure-name) (state:macro-get-macro-procedure-name symbol)
    (e0:call-indirect macro-procedure-name
                      arguments)))
;;; Start macroexpanding one call to the given macro, obtaining an
;;; s-expression; macroexpand it until it's reduced to an expression,
;;; and return the result untagged:
(e1:define (e1:macroexpand-macro-call symbol arguments)
  (e0:let (sexpression-after-one-expansion)
          (e1:macroexpand-1-macro-call symbol arguments)
    (e1:macroexpand sexpression-after-one-expansion)))

;;; Simply expand the given s-expression by tail-calling the
;;; appropriate expression-expander, which will return an untagged
;;; expression:
(e1:define (e1:macroexpand s)
  (e0:let (tag) (sexpression:get-tag s)
    (e0:let (content) (sexpression:eject s)
      (e0:call-indirect (sexpression:type-tag->expression-expander-procedure-name tag)
                        content))))

;;; Take an s-list, and macroexpand each element.  Return the list of
;;; results.
(e1:define (e1:macroexpand-sexpressions sexpressions)
  (list:reverse (e1:macroexpand-sexpressions-acc sexpressions list:nil)))
(e1:define (e1:macroexpand-sexpressions-acc sexpressions acc)
  (e0:if-in (sexpression:null? sexpressions) (#f)
    ;; sexpressions is not ()
    (e1:macroexpand-sexpressions-acc (sexpression:cdr sexpressions)
                                     (list:cons (e1:macroexpand (sexpression:car sexpressions))
                                                acc))
    ;; sexpressions is ()
    acc))


;;;;; Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Transform sequences are just a boxed lists of symbols; of course
;;; they start empty.  Notice that there is no transform list for
;;; globals, which can only be transformed retroactively.
(e1:define transform:expression-transforms
  (box:make-initialized list:nil))
(e1:define transform:procedure-transforms
  (box:make-initialized list:nil))
(e1:define transform:global-transforms
  (box:make-initialized list:nil))

;;; User entry point.
(e1:define (transform:transform-retroactively! globals-not-to-transform
                                               value-transform-names
                                               procedures-not-to-transform
                                               procedure-transform-names)
  (e0:let (global-names) (list:without-list (state:global-names) globals-not-to-transform)
    (e0:let (procedure-names) (list:without-list (state:procedure-names) procedures-not-to-transform)
      ;; Compute the transformed global and procedure list:
      (e0:let (transformed-name-global-list) (transform:compute-transformed-globals global-names value-transform-names)
        (e0:let (transformed-name-formal-body-list) (transform:compute-transformed-procedures procedure-names procedure-transform-names)
          ;; Change global environments with a single primitive call, so that the effect is atomic:
          (e0:primitive state:update-globals-and-procedures! transformed-name-global-list
                                                             transformed-name-formal-body-list))))))

;;; Prepend or append a given transforms:
(e1:define (transform:prepend-expression-transform! new-transform-name)
  (box:set! transform:expression-transforms
            (list:cons new-transform-name (box:get transform:expression-transforms))))
(e1:define (transform:append-expression-transform! new-transform-name)
  (box:set! transform:expression-transforms
            (list:append2 (box:get transform:expression-transforms)
                          (list:singleton new-transform-name))))
(e1:define (transform:prepend-procedure-transform! new-transform-name)
  (e0:let () (box:set! transform:procedure-transforms
                       (list:cons new-transform-name (box:get transform:procedure-transforms)))
    (state:invalidate-macro-procedure-name-cache!))) ;; All macros have to be re-transformed
(e1:define (transform:append-procedure-transform! new-transform-name)
  (e0:let () (box:set! transform:procedure-transforms
                       (list:append2 (box:get transform:procedure-transforms)
                                     (list:singleton new-transform-name)))
    (state:invalidate-macro-procedure-name-cache!))) ;; All macros have to be re-transformed

;;; Transforms are applied left-to-right, which is to say the first
;;; element in a transform list is the first to fire, and its result
;;; goes to the next transform in the list.

(e1:define (transform:transform-expression expression)
  (transform:apply-expression-transforms (box:get transform:expression-transforms) expression))
(e1:define (transform:apply-expression-transforms remaining-transforms object)
  (e0:if-in remaining-transforms (0)
    object
;;(e0:let () (string:write "Ok-B: Applying the transform ")
;;(e0:let () (string:write (symbol:symbol->string (list:head remaining-transforms)))
;;(e0:let () (string:write "\n")
    (e0:let (transformed-object)
            (e0:call-indirect (list:head remaining-transforms) object)
      (transform:apply-expression-transforms (list:tail remaining-transforms)
                                             transformed-object))))
;)))

;;; Global transforms have both in-dimension and out-dimension 2,
;;; because they also transform each name.  By convention the name
;;; comes first in arguments and results.
(e1:define (transform:transform-global name value)
  (transform:apply-global-transforms (box:get transform:global-transforms) name value))
(e1:define (transform:apply-global-transforms remaining-transforms name value)
  (e0:if-in remaining-transforms (0)
    (e0:bundle name value)
;;(e0:let () (string:write "Ok-B: Applying the global transform ")
;;(e0:let () (string:write (symbol:symbol->string (list:head remaining-transforms)))
;;(e0:let () (string:write " to ")
;;(e0:let () (string:write (symbol:symbol->string name))
;;(e0:let () (string:write "\n")
    (e0:let (transformed-name transformed-value)
            (e0:call-indirect (list:head remaining-transforms) name value)
      (transform:apply-global-transforms (list:tail remaining-transforms)
                                         transformed-name
                                         transformed-value))))
;;)))))

;;; Procedure transforms have both in-dimension and out-dimension 3,
;;; as they transform procedure name, formals and body.  Arguments and
;;; results are always in this order.
(e1:define (transform:transform-procedure name formals body)
  (e0:let (transform-names) (box:get transform:procedure-transforms)
    (transform:apply-procedure-transforms transform-names name formals body)))
(e1:define (transform:apply-procedure-transforms remaining-transforms name formals body)
  (e0:if-in remaining-transforms (0)
    (e0:bundle name formals body)
;;(e0:let () (string:write "Ok-B: Applying the procedure transform ")
;;(e0:let () (string:write (symbol:symbol->string (list:head remaining-transforms)))
;;(e0:let () (string:write " to ")
;;(e0:let () (string:write (symbol:symbol->string name))
;;(e0:let () (string:write "\n")
    (e0:let (transformed-name transformed-formals transformed-body)
            (e0:call-indirect (list:head remaining-transforms) name formals body)
      (transform:apply-procedure-transforms (list:tail remaining-transforms)
                                            transformed-name
                                            transformed-formals
                                            transformed-body))))
;;)))))

;;; We won't have generic tuples till much later...
(e1:define (transform:triple a b c)
  (e0:let (result) (buffer:make (e0:value 3))
    (e0:let () (buffer:initialize! result (e0:value 0) a)
      (e0:let () (buffer:initialize! result (e0:value 1) b)
        (e0:let () (buffer:initialize! result (e0:value 2) c)
          result)))))

;;; Apply the current global/procedure transformations to all already
;;; defined procedures/globals.  This is conceptually simple, but
;;; tricky to implement if we want to support some transformations
;;; which make transformed procedures incompatible with untransformed
;;; ones (for example, CPS).  The problem is that, at some point, the
;;; transform itself (or a procedure called by it) would be
;;; transformed in an incompatible way, and it would break.  The
;;; solution is to make the procedure-update step not depend on
;;; epsilonzero; that is the only reason why
;;; state:update-globals-and-procedures!  must be a primitive.
(e1:define (transform:compute-transformed-procedures procedure-names procedure-transform-names)
  (transform:compute-transformed-procedures-acc procedure-names procedure-transform-names list:nil))
(e1:define (transform:compute-transformed-procedures-acc procedure-names procedure-transform-names acc)
  (e0:if-in procedure-names (0)
    (list:reverse acc)
    (e0:let (untransformed-name) (list:head procedure-names)
(e0:let () (string:write "Transforming ")
(e0:let () (string:write (symbol:symbol->string untransformed-name))
(e0:let () (string:write " (")
(e0:let () (fixnum:write (list:length procedure-names))
(e0:let () (string:write " remaining)\n")
      (e0:let (untransformed-formals) (state:procedure-get-formals untransformed-name)
        (e0:let (untransformed-body) (state:procedure-get-body untransformed-name)
          (e0:let (transformed-name transformed-formals transformed-body)
                  (transform:apply-procedure-transforms procedure-transform-names
                                                        untransformed-name
                                                        untransformed-formals
                                                        untransformed-body)
            (transform:compute-transformed-procedures-acc (list:tail procedure-names)
                                                          procedure-transform-names
                                                          (list:cons (transform:triple transformed-name
                                                                                       transformed-formals
                                                                                       transformed-body)
                                                                     acc))))))))
)))))

;;; Same idea for non-procedures.  Notice that globals are not
;;; (usually) expressions, and we can't transform them with usual
;;; expression transforms.
(e1:define (transform:compute-transformed-globals global-names value-transform-names)
  (transform:compute-transformed-globals-acc global-names value-transform-names list:nil))
(e1:define (transform:compute-transformed-globals-acc global-names value-transform-names acc)
  (e0:if-in global-names (0)
    (list:reverse acc)
    (e0:let (untransformed-name) (list:head global-names)
      (e0:let (untransformed-global) (state:global-get untransformed-name)
        (e0:let (transformed-name transformed-global)
                (transform:apply-global-transforms value-transform-names untransformed-name untransformed-global)
          (transform:compute-transformed-globals-acc (list:tail global-names)
                                                     value-transform-names
                                                     (list:cons (cons:make transformed-name transformed-global)
                                                                acc)))))))


;;;;; REPL: the "E" part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We can already implement the core of the REPL: or at least the "E"
;;; and part of the "R" in Read-Eval-Print Loop.

;;; Macroexpand the given s-expression and transform the resulting
;;; extended expression into an executable epsilon0 expression; then
;;; evaluate it, and return the results as a list.
(e1:define (repl:macroexpand-and-transform sexpression)
  (e0:let (untransformed-expression) (e1:macroexpand sexpression)
    (e0:let (transformed-expression) (transform:transform-expression untransformed-expression)
      transformed-expression)))
(e1:define (repl:macroexpand-transform-and-execute sexpression)
  (e0:eval-ee (repl:macroexpand-and-transform sexpression)))


;;;;; A very crude error-reporting facility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (e1:fail)
  (e0:primitive debug:fail))
;;  (e0:primitive fixnum:/ (e0:value 1) (e0:value 0)))
;;  (e1:fail))
;;  (error "boo"))

;; FIXME: rename
(e1:define (e1:error string)
  ;;(e0:primitive fixnum:/ (e0:value 1) (e0:value 0)))
  (e0:let ()
    (string:write (e0:value "ERROR: "))
    (e0:let ()
      (string:write string)
      (e0:let ()
        (character:write character:newline)
        (e1:fail)))))

;; FIXME: rename
(e1:define (e1:errors string symbol)
  (e0:let ()
    (string:write (e0:value "ERROR: "))
    (e0:let ()
      (string:write string)
      (e0:let ()
        (string:write (e0:value ": "))
        (e0:let ()
          (string:write (symbol:symbol->string symbol))
          (e0:let ()
            (character:write character:newline)
            (e1:fail)))))))

;; (define-macro (e1:error string)
;;   `(error (string->guile-string ,string)))
;; (define-macro (e1:errors string symbol)
;;   `(error (string->guile-string ,string)
;;           (symbol->guile-symbol ,symbol)))

(e1:define (e0:fast-eval expression local-environment)
  (e0:primitive e0:eval-in-c expression local-environment))

(e1:define (e0:eval expression local-environment) (e0:fast-eval expression local-environment))
