;;;;; This is -*- epsilon -*- , which looks quite like Scheme.
;;;;; Tentative code

;;;;; Copyright (C) 2012 Luca Saiu
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

(e1:toplevel (sum:define tree (empty) (nonempty left right)))
(e1:define (t n)
  (e1:if (fixnum:zero? n)
    (tree-empty)
    (e1:let* ((subtree (t (fixnum:1- n))))
      (tree-nonempty subtree subtree))))
(e1:define (size t)
  (e1:if (tree-empty? t)
    1
    (fixnum:+ (size (tree-nonempty-get-left t))
              (size (tree-nonempty-get-right t)))))
(e1:define (sizep t)
  (e1:match t
    ((tree-empty)
     1)
    ((tree-nonempty left right)
     (fixnum:+ (sizep left) (sizep right)))))

(e1:define (fibopa n)
  (e1:match n
    ('0 0)
    ('1 1)
    (_ (fixnum:+ (fibopa (fixnum:- n (e0:value 2))) (fibopa (fixnum:1- n))))))
(e1:define (fibopa2 n)
  (e1:match n
    ((when _ (fixnum:< n 2)) n)
    (_ (fixnum:+ (fibopa2 (fixnum:- n (e0:value 2))) (fibopa2 (fixnum:1- n))))))

(e1:define (count n)
  (count-acc n 0))
(e1:define (count-acc n a)
  (e0:if-in n (0)
    a
    (count-acc (e0:primitive fixnum:1- n)
               (e0:primitive fixnum:1+ a))))

;;(e1:define n 10000000)

(e1:define (c n)
  (string:write "n is ")
  (fixnum:write n)
  (string:write "\n")
  (unexec:unexec "/tmp/c"
    (string:write "Counting up to ")
    (fixnum:write n)
    (string:write "\n")
    (count n)
    (string:write "Done\n")))

;;; Unexec into /tmp/e .  The main expression computes and prints Fibonacci's function.
(e1:define (e)
  (unexec:unexec "/tmp/e"))

(e1:define (m)
  (unexec:unexec "/tmp/m"
    (string:write "I'm starting the computation right now\n")
    (fixnum:write (fibo 33))
    (string:write "\n")
    (unexec:unexec "/tmp/another"
      (string:write "Hello from /tmp/another\n"))))

(e1:define-macro (wrap . forms)
  `(e1:begin-2
     (string:write "+\n")
     ,@forms
     (string:write "-\n")))

(e1:define (work n) (list:length (list:iota (fixnum:* n 1))))

(e1:define (ps n)
  (string:write "Starting six threads\n")
  (e1:let* ((f1 (e1:future (size (t n))))
            (f2 (e1:future (size (t n))))
            (f3 (e1:future (size (t n))))
            (f4 (e1:future (size (t n))))
            (f5 (e1:future (size (t n))))
            (f6 (e1:future (size (t n)))))
    (string:write "Started.  Joining...\n")
    (e1:let* ((v1 (e1:join f1))
              (v2 (e1:join f2))
              (v3 (e1:join f3))
              (v4 (e1:join f4))
              (v5 (e1:join f5))
              (v6 (e1:join f6)))
      (string:write "Joined all. Results are: \n")
      (string:write "* ") (fixnum:write v1) (string:write "\n")
      (string:write "* ") (fixnum:write v2) (string:write "\n")
      (string:write "* ") (fixnum:write v3) (string:write "\n")
      (string:write "* ") (fixnum:write v4) (string:write "\n")
      (string:write "* ") (fixnum:write v5) (string:write "\n")
      (string:write "* ") (fixnum:write v6) (string:write "\n")
      (string:write "\n"))))
(e1:define (m-ps n)
  (unexec:unexec "/tmp/ps"
    (ps n)))

(e1:define (m-s n)
  (unexec:unexec "/tmp/s"
    (string:write "I'm starting the computation right now\n")
    (fixnum:write (size (t n)))
    (string:write "\n")))

(e1:define (pf n)
  (string:write "Starting six threads\n")
  (e1:let* ((f1 (e1:future (fibo n)))
            (f2 (e1:future (fibo n)))
            (f3 (e1:future (fibo n)))
            (f4 (e1:future (fibo n)))
            (f5 (e1:future (fibo n)))
            (f6 (e1:future (fibo n))))
    (string:write "Started.  Joining...\n")
    (e1:let* ((v1 (e1:join f1))
              (v2 (e1:join f2))
              (v3 (e1:join f3))
              (v4 (e1:join f4))
              (v5 (e1:join f5))
              (v6 (e1:join f6)))
      (string:write "Joined all. Results are: \n")
      (string:write "* ") (fixnum:write v1) (string:write "\n")
      (string:write "* ") (fixnum:write v2) (string:write "\n")
      (string:write "* ") (fixnum:write v3) (string:write "\n")
      (string:write "* ") (fixnum:write v4) (string:write "\n")
      (string:write "* ") (fixnum:write v5) (string:write "\n")
      (string:write "* ") (fixnum:write v6) (string:write "\n")
      (string:write "\n"))))
(e1:define (m-pf n)
  (unexec:unexec "/tmp/pf"
    (pf n)))

(e1:define (m-f n)
  (unexec:unexec "/tmp/f"
    (string:write "I'm starting the computation right now\n")
    (fixnum:write (fibo n))
    (string:write "\n")))

(e1:define (pw n)
  (string:write "Starting six threads\n")
  (e1:let* ((f1 (e1:future (work n)))
            (f2 (e1:future (work n)))
            (f3 (e1:future (work n)))
            (f4 (e1:future (work n)))
            (f5 (e1:future (work n)))
            (f6 (e1:future (work n))))
    (string:write "Started.  Joining...\n")
    (e1:let* ((v1 (e1:join f1))
              (v2 (e1:join f2))
              (v3 (e1:join f3))
              (v4 (e1:join f4))
              (v5 (e1:join f5))
              (v6 (e1:join f6)))
      (string:write "Joined all. Results are: \n")
      (string:write "* ") (fixnum:write v1) (string:write "\n")
      (string:write "* ") (fixnum:write v2) (string:write "\n")
      (string:write "* ") (fixnum:write v3) (string:write "\n")
      (string:write "* ") (fixnum:write v4) (string:write "\n")
      (string:write "* ") (fixnum:write v5) (string:write "\n")
      (string:write "* ") (fixnum:write v6) (string:write "\n")
      (string:write "\n"))))
(e1:define (m-pw n)
  (unexec:unexec "/tmp/pw"
    (pw n)))
(e1:define (m-w n)
  (unexec:unexec "/tmp/w"
    (string:write "The result is ")
    (fixnum:write (work n))
    (string:write "\n")))

(e1:define (work-forever n)
  (string:write "The result is ")
  (fixnum:write (work n))
  (string:write "\n")
  (work-forever n))

(e1:define (print-forever s)
  (string:write s)
  (print-forever s))
(e1:define (print-two-things)
  (e1:let ((q (e1:future (print-forever "a"))))
    (print-forever "b")))

(e1:define (compute-and-print-forever s)
  (string:write s)
  (work 100000)
  (compute-and-print-forever s))
(e1:define (compute-and-print-two-things)
  (e1:let ((q (e1:future (compute-and-print-forever "a\n"))))
    (compute-and-print-forever "b\n")))
(define (go)
  (e1:toplevel (e1:unexec "/tmp/compute-and-print-two-things" (compute-and-print-two-things))))
(e1:define (meta)
  (e1:unexec "/tmp/meta"
    (string:write "Working...\n")
    (work 100000)
    (string:write "Still alive.  Reproducing myself...\n")
    (meta)))
(define (meta)
  (e1:toplevel (meta)))
