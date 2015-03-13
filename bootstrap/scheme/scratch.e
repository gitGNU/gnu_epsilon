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


;;;;; epsilon0 statically reachable globals, buffers and procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; It's useful for compilers and analyzers to know the globals,
;;; buffers and called procedures (including via fork) within an
;;; expression and the other expressions it may reach thru calls and
;;; forks.

;;; The updated sets are represented as unboxed hashes with generic
;;; objects (pointers (including symbols) or unboxed objects) as keys,
;;; and unused associated data.

;;; Given an expression and a list of its local variables (hence not
;;; to be interpreted as globals) or a procedure name, update the
;;; given sets of used globals, buffers and procedures with the
;;; globals, buffers and procedures used by the given entity and all
;;; its reachable procedures.

;;; Unfortunately e0:call-indirect makes the set of reachable callees
;;; (even in a "syntactic" sense) undecidable, because the procedure
;;; expression might compute procedure names at run time in a complex
;;; way.  Here we make the assumption that e0:call-indirect can only
;;; obtain a procedure name via:
;;;   i)  a literal symbol;
;;;   ii) the value of a global;
;;;   iii) a reachable pointer in memory, excluding every other symbol
;;;        field.
;;; This is adequate for "static" epsilon0 programs; for dynamic
;;; epsilon0 programs we will have to be conservative and consider
;;; *every* procedure to be reachable.

;;; The rationale for the choice above is being able to compile static
;;; programs without including unnecessary procedures and global data,
;;; and without translating symbol data structures and epsilon0
;;; expressions as data.  This is crucial for memory-constrained
;;; targets, and still desirable elsewhere.

;;; FIXME: add buffers to the worklist

;;; Given an expression an a set-as-list of its local variables update
;;; the given hashes of globals, pointer literals and procedures it
;;; refers, itself of by its statically reachable callees.
(e1:define (e0:scan-expression-for-globals-literals-procedures! e vars gs bs ps)
  (e1:let ((wl (box:make list:nil)))
    (e0:scan-expression-for-globals-literals-procedures!-wl e vars gs bs ps wl)
    (e0:scan-worklist-for-globals-literals-procedures! gs bs ps wl)))

;;; Given the name of a procedure update the globals, pointer literals
;;; and procedures it refers, itself or via its statically reachable callees.
(e1:define (e0:scan-procedure-for-globals-literals-procedures! name gs bs ps)
  (e0:scan-worklist-for-globals-literals-procedures! gs bs ps (box:make (list:list name))))

(e1:define (e0:scan-expression-for-globals-literals-procedures!-wl e vars gs bs ps wl)
  ;; (fio:write "wl size: " (i (list:length (box:get wl))) "; expression: " (e e) "\n")
  (e1:match e
    ((e0:expression-variable _ name)
     (e1:unless (set-as-list:has? vars name)
       (unboxed-hash:set! gs name #f)))
    ((e0:expression-value _ content)
     (e1:when (boxedness:buffer? content)
       (unboxed-hash:set! bs content #f)))
    ((e0:expression-bundle _ items)
     (e1:dolist (i items)
       (e0:scan-expression-for-globals-literals-procedures!-wl i vars gs bs ps wl)))
    ((e0:expression-primitive _ _ actuals)
     (e1:dolist (a actuals)
       (e0:scan-expression-for-globals-literals-procedures!-wl a vars gs bs ps wl)))
    ((e0:expression-let _ bound-variables bound-expression body)
     (e0:scan-expression-for-globals-literals-procedures!-wl bound-expression vars gs bs ps wl)
     (e1:let ((new-vars (set-as-list:union bound-variables vars)))
       (e0:scan-expression-for-globals-literals-procedures!-wl body new-vars gs bs ps wl)))
    ((or (e0:expression-call _ procedure-name actuals)
         (e0:expression-fork _ procedure-name actuals))
     (e0:scan-expression-for-globals-literals-procedures!-add-procedure! procedure-name gs bs ps wl)
     (e1:dolist (a actuals)
       (e0:scan-expression-for-globals-literals-procedures!-wl a vars gs bs ps wl)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e1:dolist (a (list:cons procedure-expression actuals))
       (e0:scan-expression-for-globals-literals-procedures!-wl a vars gs bs ps wl)))
    ((e0:expression-if-in _ discriminand _ then-branch else-branch)
     (e0:scan-expression-for-globals-literals-procedures!-wl discriminand vars gs bs ps wl)
     (e0:scan-expression-for-globals-literals-procedures!-wl then-branch vars gs bs ps wl)
     (e0:scan-expression-for-globals-literals-procedures!-wl else-branch vars gs bs ps wl))
    ((e0:expression-join _ future)
     (e0:scan-expression-for-globals-literals-procedures!-wl future vars gs bs ps wl))
    (else
     (e1:error "not an epsilon0 expression"))))

(e1:define (e0:scan-expression-for-globals-literals-procedures!-add-procedure! name gs bs ps wl)
  (e1:unless (unboxed-hash:has? ps name)
    (box:set! wl (cons:make name (box:get wl)))
    (unboxed-hash:set! ps name #f)))

(e1:define (e0:scan-worklist-for-globals-literals-procedures! gs bs ps wl)
  (e1:unless (list:null? (box:get wl))
    (e1:let* ((name (list:head (box:get wl)))
              (formals (state:procedure-get-formals name))
              (body (state:procedure-get-body name)))
      (box:set! wl (list:tail (box:get wl)))
      (unboxed-hash:set! ps name #f)
      (e0:scan-expression-for-globals-literals-procedures!-wl body formals gs bs ps wl))
    (e0:scan-worklist-for-globals-literals-procedures! gs bs ps wl)))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define (go name)
  (e1:let ((gs (unboxed-hash:make))
           (bs (unboxed-hash:make))
           (ps (unboxed-hash:make)))
    (e0:scan-procedure-for-globals-literals-procedures! name gs bs ps)
    (e1:dolist (g (set-as-list:unboxed-hash->set-as-list gs))
      (fio:write "Global " (sy g) "\n"))
    (e1:dolist (p (set-as-list:unboxed-hash->set-as-list ps))
      (fio:write "Procedure " (sy p) "\n"))
    (e1:dolist (b (set-as-list:unboxed-hash->set-as-list bs))
      (fio:write "Buffer " (i b) "\n"))))
