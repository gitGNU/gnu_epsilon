;;;;; This is -*- epsilon -*-
;;;;; Memory allocation and tagging in epsilon1

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


;;;;; Alternative implementation of memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (go)
  (e1:define a:word-size
    configuration:sizeof_void_p)
  (e1:define a:size-in-words
    (fixnum:* 1024 100))
  (e1:define a:emergency-buffer-size-in-words
    (fixnum:* 4096))
  (e1:define a:space
    (vector:make-initialized a:size-in-words
                             -1))
  (e1:define a:limit
    (fixnum:+ a:space
              (fixnum:* a:size-in-words a:word-size)
              (fixnum:- (fixnum:* a:emergency-buffer-size-in-words a:word-size))))
  (e1:define a:allocation-pointer
    (box:make a:space))
  (e1:define (a:available-bytes)
    (fixnum:- a:limit (box:get a:allocation-pointer)))
  (e1:define (a:available-words)
    (fixnum:/ (a:available-bytes) a:word-size))

  (e1:define (a:get b o)
    (e1:primitive buffer:get
                  (e1:primitive fixnum:+ b (e1:primitive fixnum:* a:word-size o))
                  0))

  (e1:define (a:set! b o v)
    (e0:primitive buffer:set!
                  (e1:primitive fixnum:+ b (e1:primitive fixnum:* a:word-size o))
                  0
                  v))

  (e1:define (a:make size)
    (e1:let* ((old (box:get a:allocation-pointer))
              (new (fixnum:+ old
                             (fixnum:* a:word-size size))))
      (e1:when (fixnum:> new a:limit)
        (e1:error "space overflow"))
      ;;(fio:write ": " (i (fixnum:- a:limit new)) " bytes still available\n")
      (box:set! a:allocation-pointer new)
      old))

  ;;(e1:define (buffer:get b o) (a:get b o))
  ;;(e1:define (buffer:set! b o v) (a:set! b o v))
  (e1:define (buffer:make s) (a:make s))
  )

