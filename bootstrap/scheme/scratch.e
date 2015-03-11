;;;;; This is -*- epsilon -*- with some Scheme
;;;;; Tentative code

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


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define fixnum:random-seed
  (box:make 1234))
(e1:define (fixnum:absolute-value x)
  (e1:if (fixnum:< x 0)
    (fixnum:negate x)
    x))
(e1:define (fixnum:random)
  (e1:let* ((old-seed (box:get fixnum:random-seed))
            (old-seed old-seed)
            (new-seed (e0:if-in old-seed (0)
                        1
                        (fixnum:non-primitive-% (fixnum:+ old-seed
                                                          (fixnum:non-primitive-* old-seed
                                                                                  213))
                                                32323)))
            (new-seed (fixnum:absolute-value new-seed)))
    (box:set! fixnum:random-seed new-seed)
    new-seed))

(e1:define (list:map-reversed-acc f xs acc)
  (e1:if (list:null? xs)
    acc
    (list:map-reversed-acc f
                           (list:tail xs)
                           (list:cons (e1:call-closure f (list:head xs))
                                      acc))))
(e1:define (list:map-reversed f xs)
  (list:map-reversed-acc f xs list:nil))

(e1:define (random-list size)
  (list:map-reversed (e1:lambda (x) (fixnum:random))
                     (list:iota size)))

(e1:define (insert-tr x xs reverse-sorted-acc)
  (e1:cond ((list:null? xs)
            (list:append-reversed reverse-sorted-acc (list:list x)))
           ((fixnum:<= x (list:head xs))
            (list:append-reversed reverse-sorted-acc (list:cons x xs)))
           (else
            (insert-tr x (list:tail xs) (list:cons (list:head xs) reverse-sorted-acc)))))
(e1:define (insert x xs)
  (insert-tr x xs list:nil))

(e1:define (insertion-sort-tr xs acc)
  (e1:if (list:null? xs)
    acc
    (insertion-sort-tr (list:tail xs) (insert (list:head xs) acc))))
(e1:define (insertion-sort xs)
  (insertion-sort-tr xs list:nil))

(e1:define (print-list list)
  (e1:dolist (x list)
    (fio:write (i x) " "))
  (fio:write "\n"))

(e1:define list
  (random-list 10))
(e1:define (go)
  (fio:write "Begin...\n")
  (e1:let ((sorted-list (insertion-sort list)))
    ;;(print-list sorted-list)
    (fio:write "...end\n")))


;;gcc -g -lunistring -lreadline -lpthread -lgc ~/repos/epsilon/_build/49s/lib/{libepsilondriver-native-tagged-egc.a,libepsilonruntime-tagged-egc.a,libepsilonutility.a} boo.s
;;gcc -g -lunistring -lreadline -lpthread ~/repos/epsilon/_build/49s/lib/{libepsilondriver-native-tagged-egc.a,libepsilonruntime-tagged-egc.a,libepsilonutility.a} boo.s
