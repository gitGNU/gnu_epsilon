;;;;; This is -*- epsilon -*-.
;;;;; Ugly code to ensure bootstrapping works without cross-stage spilling, which will
;;;;; be cleaned up or go away.

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


;;; FIXME: this functionality is useful and I think I should keep it to help
;;; ensure bootstrapping doesn't break.  However I should use more rational names.


(e1:define (explore x action-closure continue?-closure)
  (explore-helper (unboxed-hash:make) x action-closure continue?-closure))
(e1:define (explore-helper h x action-closure continue?-closure)
  (e1:cond ((e1:or (boxedness:fixnum? x)
                   #;(whatever:eq? x symbol:table)
                   #;(unboxed-hash:has? all-symbols x))
            (e1:call-closure action-closure x))
           ((unboxed-hash:has? h x))
           (else
            (unboxed-hash:set! h x #f)
            (e1:call-closure action-closure x)
            (e1:when (e1:call-closure continue?-closure x)
              (e1:dobuffer (e x)
                (explore-helper h e action-closure continue?-closure))))))

(e1:define (good-symbol? x)
  (e1:and (looks-like-a-symbol? x)
          (symbol:interned-in? x symbol:table)))
(e1:define (suspect-symbol? x)
  (e1:and (looks-like-a-symbol? x)
          (e1:not (symbol:interned-in? x symbol:table))))

(e1:define (suspect-symbols-from root)
  (e1:let ((h (unboxed-hash:make)))
    (explore root
             (e1:lambda (x)
               (e1:when (suspect-symbol? x)
                 (unboxed-hash:set! h x #f)))
             (e1:lambda (x) (e1:and (e1:not (good-symbol? x))
                                    (e1:not (suspect-symbol? x)))))
    (hash:keys h)))

(e1:define-macro (print-list-when-nonnull . variables)
  `(e1:begin
     (print-list-when-nonnullq ,@variables)
     #;(print-list-when-nonnullz ,@variables))) ;; FIXME: this works!
(e1:define-macro (print-list-when-nonnullz . variables)
  (e1:if (sexpression:null? variables)
    '(e1:begin)
    `(e1:begin
       (print-list-when-nonnullz1 ,(sexpression:car variables))
       (print-list-when-nonnullz ,@(sexpression:cdr variables)))))
(e1:define-macro (print-list-when-nonnullz1 variable)
  (e1:let ((v (sexpression:fresh-symbol)))
    `(e1:when ,variable
       (e1:dolist (,v ,variable)
         (fio:write "# Now analyzing" (sy ,v) " ...\n")
         (debug:print* (e1:variable ,v))
         (fio:write "... done with " (sy ,v) "\n\n")))))

(e1:define-macro (print-list-when-nonnullq . variables)
  (e1:if (sexpression:null? variables)
    '(e1:begin
       (fio:write "\n"))
    `(e1:begin
       (print-list-when-nonnull1 ,(sexpression:car variables))
       (print-list-when-nonnullq ,@(sexpression:cdr variables)))))
(e1:define-macro (print-list-when-nonnull1 variable)
  (e1:let ((v (sexpression:fresh-symbol)))
    `(e1:when ,variable
       (fio:write "[" (sy (e1:value ,variable)) ": ")
       (e1:dolist (,v ,variable)
         (fio:write (sy ,v) " "))
       (fio:write "] "))))

(e1:define (looks-like-a-symbol? x)
  (e1:and (boxedness:buffer? x)
          (fixnum:= (boxedness:buffer-length x) 11)
          (looks-like-a-string? (buffer:get x 0))))
(e1:define (looks-like-a-vector? x)
  (e1:and (boxedness:buffer? x)
          (fixnum:= (buffer:get x 0)
                    (fixnum:1- (boxedness:buffer-length x)))))
(e1:define (looks-like-a-string? x)
  (e1:and (looks-like-a-vector? x)
          (looks-like-a-string-from? x 1)))
(e1:define (looks-like-a-string-from? x from-index)
  (e1:cond ((fixnum:= from-index (boxedness:buffer-length x))
            #t)
           ((fixnum:<= (buffer:get x from-index) 0)
            #f)
           ;; FIXME: this assumes ASCII-only text, which is what I commonly use.
           ;; It should be generalized if I recycle this code, which I should.
           ((fixnum:> (buffer:get x from-index) 127)
            #f)
           (else
            (looks-like-a-string-from? x (fixnum:1+ from-index)))))

(e1:define (check-for-spills)
  (e1:let ((failed (box:make #f)))
    (e1:dohash (_ s  symbol:table)
      (e1:let* ((from-nonprocedure (suspect-symbols-from (symbol:symbol-get-global-value s)))
                (from-formals (suspect-symbols-from (symbol:symbol-get-formals s)))
                (from-body (suspect-symbols-from (symbol:symbol-get-body s)))
                (from-macro (suspect-symbols-from (symbol:symbol-get-macro s)))
                (from-macro-procedure-name (suspect-symbols-from (symbol:symbol-get-macro-procedure-name s)))
                (everything (list:append from-nonprocedure
                                         from-body
                                         from-macro
                                         from-macro-procedure-name)))
        (e1:when everything
          (fio:write "Found spills when bootstrapping:\n")
          (fio:write (sy s) ": ")
          (print-list-when-nonnull from-nonprocedure
                                   from-body
                                   from-macro
                                   from-macro-procedure-name)
          ;;(fio:write " | ")
          ;; (e1:dolist (suspect everything)
          ;;   (fio:write (sy suspect) " "))
          (fio:write "\n")
          (box:set! failed #t))))
    (e1:if (box:get failed)
      (e1:error "I have to fix this.")
      (fio:write "No symbol spilled from the old primary table.\n"))))
  ;; (e1:dolist (s (suspect-symbols-from (e1:value fixnum:+))) (fio:write (sy s) "\n"))
