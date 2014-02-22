;;;;; This is -*- epsilon -*- with just a little Scheme, if any
;;;;; Tentative code

;;;;; Copyright (C) 2013, 2014  Luca Saiu

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


;;;;; Item-lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An item-list is a list of <key, value> pairs with unique keys
;;; where elements are kept in a user-controlled order.  The user may
;;; insert new elements at the beginning, at the end, and before or
;;; after existing elements.  Elements can also be deleted, by key.

;;; Update performance is particularly not critical: this structure
;;; must only be efficient to traverse.  A simple list is a fine
;;; implementation.

(e1:define item-list:nil
  list:nil)

(e1:define (item-list:add-first il key value)
  (list:cons (cons:make key value)
             (item-list:remove il key)))
(e1:define (item-list:add-last il key value)
  (list:append (item-list:remove il key)
               (list:list (cons:make key value))))
(e1:define (item-list:add-before il reference-key new-key new-value)
  (e1:cond ((list:null? il)
            (e1:error "not found"))
           ((whatever:eq? reference-key (cons:get-car (list:head il)))
            (list:cons (cons:make new-key new-value)
                       il))
           (else
            (list:cons (list:head il)
                       (item-list:add-before (list:tail il)
                                             reference-key
                                             new-key
                                             new-value)))))
(e1:define (item-list:add-after il reference-key new-key new-value)
  (e1:cond ((list:null? il)
            (e1:error "not found"))
           ((whatever:eq? reference-key (cons:get-car (list:head il)))
            (list:cons (list:head il)
                       (list:cons (cons:make new-key new-value)
                                  (list:tail il))))
           (else
            (list:cons (list:head il)
                       (item-list:add-after (list:tail il)
                                            reference-key
                                            new-key
                                            new-value)))))
(e1:define (item-list:remove il key)
  (e1:cond ((list:null? il)
            list:nil)
           ((whatever:eq? key (cons:get-car (list:head il)))
            (list:tail il))
           (else
            (list:cons (list:head il)
                       (item-list:remove (list:tail il)
                                         key)))))

;;; Destructive versions, working on an item list box:
(e1:define (item-list:add-first! bil key value)
  (box:set! bil (item-list:add-first (box:get bil) key value)))
(e1:define (item-list:add-last! bil key value)
  (box:set! bil (item-list:add-last (box:get bil) key value)))
(e1:define (item-list:add-before! bil reference-key new-key new-value)
  (box:set! bil (item-list:add-before (box:get bil) reference-key new-key new-value)))
(e1:define (item-list:add-after! bil reference-key new-key new-value)
  (box:set! bil (item-list:add-after (box:get bil) reference-key new-key new-value)))
(e1:define (item-list:remove! bil key)
  (box:set! bil (item-list:remove (box:get bil) key)))


;;;;; Locus support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum locus:locus
  (unknown)
  (known file-name-option
         start-row
         start-column
         end-row
         end-column
         description))

(e1:define (locus:locus->file-name-option locus)
  (e1:match locus
    ((locus:locus-unknown)
     (option:option-none))
    ((locus:locus-known file-name-option _ _ _ _ _)
     file-name-option)))

(e1:define (position:<= row-1 column-1 row-2 column-2)
  (e1:cond ((fixnum:< row-1 row-2)
            #t)
           ((fixnum:> row-1 row-2)
            #f)
           (else
            (fixnum:<= column-1 column-2))))

(e1:define (locus:join locus-1 locus-2)
  (e1:match locus-1
    ((locus:locus-unknown)
     (locus:locus-unknown))
    ((locus:locus-known file-name-option-1
                        start-row-1 start-column-1
                        end-row-1 end-column-1
                        description-1)
     (e1:match locus-2
       ((locus:locus-unknown)
        (locus:locus-unknown))
       ((locus:locus-known file-name-option-2
                           start-row-2 start-column-2
                           end-row-2 end-column-2
                           description-2)
        ;; FIXME: correctly merge file-name-option's
        ;; FIXME: shall we do something with descriptions?
        (e1:let ((start-1-prevails (position:<= start-row-1 start-column-1
                                                start-row-2 start-column-2))
                 (end-1-prevails (position:<= end-row-2 end-column-2
                                              end-row-1 end-column-1)))
          (locus:locus-known file-name-option-1
                             (e1:if start-1-prevails start-row-1 start-row-2)
                             (e1:if start-1-prevails start-column-1 start-column-2)
                             (e1:if end-1-prevails end-row-1 end-row-2)
                             (e1:if end-1-prevails end-column-1 end-column-2)
                             string:empty)))))))
;;; FIXME: write a locus-merging variadic macro.  The operation is
;;; associative.

;;; FIXME: write a locus->string procedure.


;;;;; Buffered backtrackable input port
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A backtrackable input port provides the same operations as an
;;; input port (read-character and eof?); in addition to them, it also
;;; makes it possible to obtain the current "port state" and then
;;; later to backtrack to it, so that the same characters and eof
;;; state can be re-obtained, in the same order as the first time.
;;; The commit operation makes it impossible to backtrack to a state
;;; earlier than the current one, thus freeing resources.
;;; Backtracking to an earlier state destroys information about later
;;; states: it's not possible to "backtrack into the future".

;;; A backtrackable port provides locus information as well.

;;; FIXME: this extension over ports is hard to reuse and generalize.
;;; I'm simulating a poor man's inheritance system, but not well
;;; enough.  I should probably redo this after thinking a little more
;;; about the general problem.

(e1:define-record backtrackable-port:backtrackable-port
  input-port
  uncommitted-characters
  buffered-characters
  file-name-option
  row
  column
  backtrackable-states)

(e1:define (backtrackable-port:input-port->backtrackable-port p file-name-option)
  (backtrackable-port:backtrackable-port p
                                         list:nil
                                         list:nil
                                         file-name-option
                                         1 0
                                         list:nil))

(e1:define-record backtrackable-port:state
  uncommitted-characters
  buffered-characters
  row
  column)

(e1:define (backtrackable-port:backtrackable-port->state bp)
  (e1:let* ((uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp))
            (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp))
            (states (backtrackable-port:backtrackable-port-get-backtrackable-states bp))
            (row (backtrackable-port:backtrackable-port-get-row bp))
            (column (backtrackable-port:backtrackable-port-get-column bp))
            (new-state (backtrackable-port:state uncommitteds
                                                 buffereds
                                                 row
                                                 column)))
    (backtrackable-port:backtrackable-port-set-backtrackable-states!
       bp
       (list:cons new-state states))
    new-state))

;;; Commit the current state, releasing resources; it won't be
;;; possible to backtrack to any previous state.
(e1:define (backtrackable-port:commit! bp)
  (backtrackable-port:backtrackable-port-set-uncommitted-characters! bp list:nil)
  (backtrackable-port:backtrackable-port-set-backtrackable-states! bp list:nil))

;;; Backtrack to a previous state.  Notice that the state is compared by identity.
;;; FIXME: it should be possible to relax this assumption, to allow backtracks into
;;; the future and even to make the thing slightly more efficient by simply not
;;; storing a state stack in the port.  I'll try, but this needs careful testing.
(e1:define (backtrackable-port:backtrack! bp state)
  (e1:let* ((states (backtrackable-port:backtrackable-port-get-backtrackable-states bp))
            (first-state (list:head states)))
    (e1:if (whatever:eq? first-state state)
      ;; Restore the state:
      (e1:let ((uncommitteds (backtrackable-port:state-get-uncommitted-characters state))
               (buffereds (backtrackable-port:state-get-buffered-characters state))
               (row (backtrackable-port:state-get-row state))
               (column (backtrackable-port:state-get-column state))
               (current-uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp))
               (current-buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp)))
        (backtrackable-port:backtrackable-port-set-row! bp row)
        (backtrackable-port:backtrackable-port-set-column! bp column)
        (backtrackable-port:backtrackable-port-set-uncommitted-characters! bp uncommitteds)
        (backtrackable-port:backtrackable-port-set-buffered-characters!
            bp
            (list:append (list:drop (list:reverse current-uncommitteds)
                                    (list:length uncommitteds))
                         current-buffereds)))
      (e1:let ((other-states (list:tail states)))
        ;; Pop the most recent state and try again:
        (backtrackable-port:backtrackable-port-set-backtrackable-states!
            bp
            other-states)
        (backtrackable-port:backtrack! bp state)))))

(e1:define (backtrackable-port:eof? bp)
  (e1:let ((p (backtrackable-port:backtrackable-port-get-input-port bp))
           (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp)))
    (e1:and (list:null? buffereds)
            (input-port:eof? p))))

(e1:define-sum backtrackable-port:character-class
  (ordinary)
  (newline)
  (nothing))

(e1:define (backtrackable-port:classify-characater character)
  (e1:case character
    ((#\newline)
     (backtrackable-port:character-class-newline))
    ((#\cr)
     (backtrackable-port:character-class-nothing))
    (else
     (backtrackable-port:character-class-ordinary))))

(e1:define (backtrackable-port:backtrackable-update-locus! bp c)
  (e1:let ((row (backtrackable-port:backtrackable-port-get-row bp))
           (column (backtrackable-port:backtrackable-port-get-column bp))
           (newline (fixnum:= c #\newline)))
    (e1:match (backtrackable-port:classify-characater c)
      ((backtrackable-port:character-class-nothing)) ;; do nothing
      ((backtrackable-port:character-class-newline)
        (backtrackable-port:backtrackable-port-set-row! bp (fixnum:1+ row))
        (backtrackable-port:backtrackable-port-set-column! bp 0))
      ((backtrackable-port:character-class-ordinary)
       (backtrackable-port:backtrackable-port-set-column! bp (fixnum:1+ column))))))

(e1:define (backtrackable-port:read-character bp)
  (e1:let ((p (backtrackable-port:backtrackable-port-get-input-port bp))
           (buffereds (backtrackable-port:backtrackable-port-get-buffered-characters bp))
           (uncommitteds (backtrackable-port:backtrackable-port-get-uncommitted-characters bp)))
    (e1:if (list:null? buffereds)
      (e1:let ((result (input-port:read-character p)))
        (backtrackable-port:backtrackable-update-locus! bp result)
        (backtrackable-port:backtrackable-port-set-uncommitted-characters!
           bp
           (list:cons result uncommitteds))
        result)
      (e1:let ((result (list:head buffereds)))
        (backtrackable-port:backtrackable-update-locus! bp result)
        (backtrackable-port:backtrackable-port-set-buffered-characters!
           bp
           (list:tail buffereds))
        (backtrackable-port:backtrackable-port-set-uncommitted-characters!
           bp
           (list:cons result uncommitteds))
        result))))


;;;;; Range sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A "range set" is just a list of disjoint character ranges, both
;;; ends included, sorted.  Here two intervals being disjoint means
;;; that they do not even have one end in common: there must be a
;;; distance of at least one character.
;;; A range set is represented as a sorted list of pairs, each pair
;;; containing the first and last character of a range.

(e1:define range-set:minimum-character
  0) ;; 0 is actually a valid code point
(e1:define range-set:maximum-character
  1114111) ;; or #x10FFFF: the last valid Unicode code point

(e1:define range-set:empty
  list:nil)
(e1:define range-set:universe
  (list:list (cons:make range-set:minimum-character
                        range-set:maximum-character)))

;;; A user-friendly way of describing a range set, to be automatically
;;; rewritten into the efficient version
(e1:define-sum range-set:sugared
  (empty)
  (universe)
  (character character)
  (range from to)
  (union first second)
  (intersection first second)
  (subtraction first second)
  (complement sugared))

(e1:define (range-set:sugared->range-set sugared)
  (e1:match sugared
    ((range-set:sugared-empty)
     range-set:empty)
    ((range-set:sugared-universe)
     range-set:universe)
    ((range-set:sugared-character c)
     (list:list (cons:make c c)))
    ((range-set:sugared-range from to)
     (list:list (cons:make from to)))
    ((range-set:sugared-union first second)
     (range-set:union (range-set:sugared->range-set first)
                      (range-set:sugared->range-set second)))
    ((range-set:sugared-intersection first second)
     (range-set:intersection (range-set:sugared->range-set first)
                             (range-set:sugared->range-set second)))
    ((range-set:sugared-subtraction first second)
     (range-set:subtraction (range-set:sugared->range-set first)
                            (range-set:sugared->range-set second)))
    ((range-set:sugared-complement sugared)
     (range-set:complement (range-set:sugared->range-set sugared)))))

;;; Return #t iff the first range completely prececes the second, and it's not
;;; joinable to it
(e1:define (range-set:range-< first-range second-range)
  (e1:let ((first-end (cons:cdr first-range))
           (second-beginning (cons:car second-range)))
    (fixnum:< (fixnum:1+ first-end) second-beginning)))

(e1:define (range-set:joinable-ranges? first-range second-range)
  (e1:not (e1:or (range-set:range-< first-range second-range)
                 (range-set:range-< second-range first-range))))

;;; This assumes the ranges are joinable
(e1:define (range-set:join-ranges first-range second-range)
  (e1:let ((first-beginning (cons:car first-range))
           (first-end (cons:cdr first-range))
           (second-beginning (cons:car second-range))
           (second-end (cons:cdr second-range)))
    (cons:make (fixnum:min first-beginning second-beginning)
               (fixnum:max first-end second-end))))

(e1:define (range-set:complement range-set)
  (range-set:complement-helper range-set:minimum-character range-set))
(e1:define (range-set:complement-helper first-to-consider range-set)
  (e1:match range-set
    ((list:list-nil)
     (e1:if (fixnum:<= first-to-consider range-set:maximum-character)
       (list:list (cons:make first-to-consider range-set:maximum-character))
       list:nil))
    ((list:list-cons (tuple first-beginning first-end) more)
     (e1:if (fixnum:< first-to-consider first-beginning)
       (list:cons (cons:make first-to-consider (fixnum:1- first-beginning))
                  (range-set:complement-helper (fixnum:1+ first-end) more))
       (range-set:complement-helper (fixnum:1+ first-end) more)))))
(e1:define (range-set:union first second)
  (e1:match (tuple:make first second)
    ((tuple (list:list-nil) _)
     second)
    ((tuple _ (list:list-nil))
     first)
    ((tuple (list:list-cons first-range first-rest)
            (list:list-cons second-range second-rest))
     (e1:cond ((range-set:joinable-ranges? first-range second-range)
               (range-set:union (list:cons (range-set:join-ranges first-range
                                                                  second-range)
                                           first-rest)
                                second-rest))
              ((range-set:range-< first-range second-range)
               (list:cons first-range
                          (range-set:union first-rest second)))
              (else
               (list:cons second-range
                          (range-set:union first second-rest)))))))

(e1:define (range-set:intersection first second)
  (range-set:complement (range-set:union (range-set:complement first)
                                         (range-set:complement second))))
(e1:define (range-set:subtraction first second)
  (range-set:complement (range-set:union (range-set:complement first)
                                         second)))

;;; FIXME: given a range set, generate a more efficient testing
;;; procedure.  I can do even better than "partial evaluation by
;;; hand", exploiting the ordering invariants to make a balanced
;;; comparison tree.
(e1:define (range-set:has? range-set character)
  (e1:match range-set
    ((list:list-nil)
     #f)
    ((list:list-cons (tuple first-beginning first-end) rest)
     (e1:or (e1:and (fixnum:<= first-beginning character)
                    (fixnum:<= character first-end))
            (range-set:has? rest character)))))


;;;;; S-range-sets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A comfortable way of expressing rangesets is by an s-expression
;;; encoding.  Here we provide a facility to translate the
;;; s-expression representation (similar to sugared range sets with
;;; trivial variadic extensions).

;;; Let C, C1, C2... be characters and R1, R2... be s-range-sets.
;;; Then a s-range-set may be any of the following s-expressions:
;;; * empty
;;; * universe
;;; * C
;;; * (range C1 C2)
;;; * (union R1 ... Rn) or (\| R1 ... Rn)      [n >= 0]
;;; * (intersection R1 ... Rn)                 [n >= 0]
;;; * (subtraction R1 ... Rn) or (- R1 ... Rn) [n >= 1; R1 minus the others]
;;; * (complement R1 ... Rn)                   [n >= 0; complement of union]

(e1:define (range-set:srange-set->range-set s)
  (e1:let ((sugared (range-set:srange-set->sugared-range-set s)))
    (range-set:sugared->range-set sugared)))

(e1:define (range-set:srange-set->sugared-range-set s)
  (e1:cond ((sexpression:symbol? s)
            (e1:let ((name (sexpression:eject s)))
              (e1:cond ((whatever:eq? name (e1:value empty))
                        (range-set:sugared-empty))
                       ((whatever:eq? name (e1:value universe))
                        (range-set:sugared-universe))
                       (else
                        (e1:error "unknown range-set symbol")))))
           ((sexpression:character? s)
            (range-set:sugared-character (sexpression:eject s)))
           ((e1:and (sexpression:list? s)
                    (sexpression:symbol? (sexpression:car s)))
            (range-set:complex-srange-set->sugared-range-set (sexpression:eject (sexpression:car s))
                                                             (sexpression:eject-list (sexpression:cdr s))))
           (else
            (e1:error "unknown range-set case"))))

(e1:define (range-set:complex-srange-set->sugared-range-set symbol args)
  (e1:case symbol
    ((range)
     (e1:if (fixnum:= (list:length args) 2)
       (range-set:sugared-range (sexpression:eject-character (list:first args))
                                (sexpression:eject-character (list:second args)))
       (e1:error "non-binary range-set range")))
    ((union \|) ;; FIXME: remove this useless escaping after switching to my parser (possibly)
     (e1:if (list:null? args)
       (range-set:sugared-empty)
       (range-set:sugared-union (range-set:srange-set->sugared-range-set (list:head args))
                                (range-set:complex-srange-set->sugared-range-set symbol
                                                                                 (list:tail args)))))
    ((intersection)
     (e1:if (list:null? args)
       (range-set:sugared-universe)
       (range-set:sugared-intersection (range-set:srange-set->sugared-range-set (list:head args))
                                       (range-set:complex-srange-set->sugared-range-set symbol
                                                                                        (list:tail args)))))
    ((subtraction -)
     (e1:if (list:null? args)
       (e1:error "nullary subtraction")
       (range-set:sugared-subtraction (range-set:srange-set->sugared-range-set (list:head args))
                                      (range-set:complex-srange-set->sugared-range-set (e1:value union)
                                                                                       (list:tail args)))))
    ((complement)
     (range-set:sugared-complement (range-set:complex-srange-set->sugared-range-set (e1:value union)
                                                                                    args)))
    (else
     (e1:error "unknown symbol"))))


;;;;; Regular expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum regexp:regexp
  (empty) ;; very different from an empty rangeset! [FIXME: shall I eliminate the empty rangeset to remove ambiguity?  Do I need to add a "fail" case? [probably not the empty rangeset, should have the same effect]]
  (range-set range-set) ;; of course a *non-sugared* range set
  (sequence first second)
  (or first second)
  (plus regexp)) ;; I take + instead of * as primitive

;;; A sugared version, to be presented to the user:
(e1:define-sum regexp:sugared
  (empty)
  (range-set range-set) ;; a *non-sugared* range set
  (string string)
  (sequence regexps)
  (or regexps)
  (star regexp)
  (optional regexp)
  (plus regexp)
  (variable name)) ;; names map into *non-sugared* regexps

(e1:define (regexp:desugar s environment)
  (e1:match s
    ((regexp:sugared-empty)
     (regexp:regexp-empty))
    ((regexp:sugared-range-set range-set)
     (regexp:regexp-range-set range-set)) ; already desugared
    ((regexp:sugared-string string)
     (regexp:desugar-string string))
    ((regexp:sugared-sequence regexps)
     (regexp:desugar-sequence regexps environment))
    ((regexp:sugared-or regexps)
     (regexp:desugar-or regexps environment))
    ((regexp:sugared-star regexp)
     (regexp:regexp-or (regexp:regexp-plus (regexp:desugar regexp environment))
                       (regexp:regexp-empty)))
    ((regexp:sugared-optional regexp)
     (regexp:regexp-or (regexp:desugar regexp environment)
                       (regexp:regexp-empty)))
    ((regexp:sugared-plus regexp)
     (regexp:regexp-plus (regexp:desugar regexp environment)))
    ((regexp:sugared-variable name)
     (alist:lookup environment name))))

(e1:define (regexp:desugar-sequence ss environment)
  (e1:cond ((list:null? ss)
            (regexp:regexp-empty))
           ((list:null? (list:tail ss))
            (regexp:desugar (list:head ss) environment))
           (else
            (regexp:regexp-sequence (regexp:desugar (list:head ss) environment)
                                    (regexp:desugar-sequence (list:tail ss) environment)))))

;;; FIXME: it is possible to optimize a union of two range-set regexps
;;; into a single range-set regexp.  I don't know if it's worth the
;;; trouble.
(e1:define (regexp:desugar-or ss environment)
  (e1:cond ((list:null? ss)
            (e1:error "regexp:desugar-or: empty list"))
           ((list:null? (list:tail ss))
            (regexp:desugar (list:head ss) environment))
           (else
            (regexp:regexp-or (regexp:desugar (list:head ss) environment)
                              (regexp:desugar-or (list:tail ss) environment)))))

(e1:define (regexp:desugar-string string)
  (regexp:desugar-string-index string 0 (fixnum:1- (string:length string))))
(e1:define (regexp:desugar-string-index string index last-index)
  (e1:if (fixnum:> index last-index)
    (regexp:regexp-empty)
    (e1:let* ((c (string:get string index))
              (first-regexp (regexp:regexp-range-set (list:list (cons:make c c)))))
      (e1:if (fixnum:= index last-index)
        first-regexp
        (regexp:regexp-sequence first-regexp
                                (regexp:desugar-string-index string (fixnum:1+ index) last-index))))))

;;; The most convenient way for a user to enter a regular expression
;;; is by an s-expression syntax encoding the sugared version.  We call
;;; "s-regexp" such an s-expression.
;;; An s-regexp may be:
;;; * an s-range-set, encoding a range-set
;;; * an s-string, encoding a sequence of single-character range sets;
;;; * an s-list of s-regexps, encoding a (possibly empty) sequence;
;;; * an s-list of the s-symbol | and one or more s-regexps, encoding an or;
;;; * an s-list of the s-symbol * and an s-regexp, encoding a star;
;;; * an s-list of the s-symbol ? and an s-regexp, encoding an option;
;;; * an s-list of the s-symbol + and an s-regexp, encoding a plus;
;;; * an s-symbol different from empty and universe, encoding a variable.
(e1:define (regexp:sregexp->sugared s)
  (e1:cond ((sexpression:null? s)
            (regexp:sugared-empty))
           ((sexpression:string? s)
            (regexp:sugared-string (sexpression:eject s)))
           ((sexpression:symbol? s)
            (e1:let ((name (sexpression:eject s)))
              (e1:case name
                ((universe) ;; not empty!  That has a different implementation (empty regexp: success -- empty rangeset: failure)
                 (regexp:sugared-range-set (range-set:srange-set->range-set s)))
                ((empty)
                 (regexp:sugared-empty))
                (else
                 (regexp:sugared-variable (sexpression:eject s))))))
           ((sexpression:cons? s)
            (e1:let* ((car (sexpression:car s))
                      (cdr (sexpression:cdr s))
                      (car-value (sexpression:eject car)))
              (e1:if (sexpression:symbol? car)
                (e1:case car-value
                  ((\|)
                   (regexp:sugared-or (regexp:sregexps->sugareds cdr)))
                  ((*)
                   (regexp:sugared-star (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((?)
                   (regexp:sugared-optional (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((+)
                   (regexp:sugared-plus (regexp:sregexp->sugared (sexpression:car-of-singleton cdr))))
                  ((range union intersection subtraction - complement)
                   (regexp:sugared-range-set (range-set:srange-set->range-set s)))
                  (else
                   (regexp:sugared-sequence (regexp:sregexps->sugareds s))))
                ;; An s-cons not starting with an s-symbol:
                (regexp:sugared-sequence (regexp:sregexps->sugareds s)))))
           (else ;; assume it's an s-range-set
            (regexp:sugared-range-set (range-set:srange-set->range-set s)))))

(e1:define (regexp:sregexps->sugareds slist)
  (e1:if (sexpression:null? slist)
    list:nil
    (list:cons (regexp:sregexp->sugared (sexpression:car slist))
               (regexp:sregexps->sugareds (sexpression:cdr slist)))))

(e1:define (regexp:sregexp->regexp-in s environment)
  (e1:let ((sugared (regexp:sregexp->sugared s)))
    (regexp:desugar sugared environment)))

;;; A regexp maching anything: [FIXME: remove if unused]
(e1:define regexp:anything
  (regexp:sregexp->regexp-in '(* universe)
                             alist:nil))


;;;;; A convenient way of updating a regexp global table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define regexp:global-environment
  (box:make alist:nil))

(e1:define (regexp:define-sregexp! name sregexp)
  (e1:let* ((old-alist (box:get regexp:global-environment))
            (regexp (regexp:sregexp->regexp-in sregexp old-alist)))
    (box:set! regexp:global-environment
              (alist:bind-unique old-alist name regexp))))

;;; Handy syntax.
;;; Example: (e1:define-regexp digit (range #\0 #\9))
(e1:define-macro (e1:define-regexp sname sregexp)
  (e1:unless (sexpression:symbol? sname)
    (e1:error "non-symbol name"))
  `(regexp:define-sregexp! (e1:value ,sname)
                           ',sregexp))

;;; Turn an s-regexp into a regexp using the current global
;;; environment:
(e1:define (regexp:sregexp->regexp s)
  (regexp:sregexp->regexp-in s (box:get regexp:global-environment)))


;;;;; Sample regexps [mostly for testing [FIXME: really?]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-regexp sexpression:letter
  (\| (range #\a #\z)
      (range #\A #\Z)))

(e1:define-regexp sexpression:decimal-digit
  (range #\0 #\9))

(e1:define-regexp sexpression:sign
  (\| #\+
      #\-))

(e1:define-regexp sexpression:radix-prefix
  (#\# (\| (union #\b
                  #\o
                  #\d
                  #\x)
           ;; #Dr, with D in [2, 9]
           ((range #\2 #\9)
            #\r)
           ;; #DDr, with DD in [10, 29]
           ((range #\1 #\2)
            (range #\0 #\9)
            #\r)
           ;; #DDr, with DD in [30, 36]
           (#\3
            (range #\0 #\6)
            #\r))))

(e1:define-regexp sexpression:fixnum
  (\| ((? sexpression:sign)
       (+ sexpression:decimal-digit))
      (sexpression:radix-prefix
       (? sexpression:sign)
       (+ (\| (range #\0 #\9)
              (range #\a #\z))))))

(e1:define-regexp sexpression:unescaped-character
  (#\# #\\ universe)) ;; escaped characters are recognized *before* this.

(e1:define-regexp sexpression:comment
  (+ (#\;
      (* (complement #\newline))
      #\newline)))

(e1:define-regexp sexpression:whitespace
  (+ (union #\space
            #\tab
            #\cr
            #\newline)))

(e1:define-regexp sexpression:ignorable
  (+ (\| sexpression:whitespace
         sexpression:comment)))

(e1:define-regexp sexpression:open
  #\()
(e1:define-regexp sexpression:close
  #\))
(e1:define-regexp sexpression:dot
  #\.)

(e1:define-regexp sexpression:string
  (#\"
   (* (\| (complement #\" #\\)
          (#\\ universe)))
   #\"))

(e1:define-regexp sexpression:atom
  (+ (\| (complement #\space
                     #\tab
                     #\cr
                     #\newline
                     #\| ;; for Scheme compatibility
                     #\(
                     #\)
                     #\;
                     #\'
                     #\"
                     #\,
                     ;;#\# ;; this is used in some atoms
                     )
         (#\\ (\| #\space
                  #\tab
                  #\cr
                  #\newline
                  #\| ;; for Scheme compatibility
                  #\(
                  #\)
                  #\;
                  #\'
                  #\"
                  #\,
                  ;;#\#
                  ))
         )))


;;;;; Regexp recognizer [FIXME: this is tentative and must be made
;;;;; reusable]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum regexp:result
  (failure)
  ;; the "final" position is the location of the last character.  FIXME: verify in every case.
  (success initial-row initial-column final-row final-column string)) ;; we use a reversed list of characters instead of a string within regexp:read-regexp-helper

(e1:define (regexp:characters->string cc)
  (vector:list->vector (list:reverse cc)))

(e1:define (regexp:read-regexp bp regexp)
  (e1:let* ((beginning-state (backtrackable-port:backtrackable-port->state bp))
            (row (backtrackable-port:backtrackable-port-get-row bp))
            (column (backtrackable-port:backtrackable-port-get-column bp))
            (result (regexp:read-regexp-helper bp
                                               regexp
                                               row column
                                               row column
                                               list:nil)))
    (e1:match result
      ((regexp:result-success initial-row initial-column final-row final-column characters)
       (regexp:result-success initial-row initial-column final-row final-column
                              (regexp:characters->string characters)))
      ((regexp:result-failure)
       (backtrackable-port:backtrack! bp beginning-state)
       result))))
(e1:define (regexp:read-regexp-helper bp regexp initial-row initial-column final-row final-column characters)
  ;; FIXME: only compute beginning-state, row and column where needed, in each case.  This might be an important optimization.
  (e1:let ((beginning-state (backtrackable-port:backtrackable-port->state bp))
           (eof (backtrackable-port:eof? bp)))
    (e1:match regexp
      ((regexp:regexp-empty)
       (regexp:result-success initial-row
                              initial-column
                              final-row
                              final-column
                              characters))
      ((regexp:regexp-range-set rs)
       (e1:if eof
         (regexp:result-failure)
         (e1:let* ((c-row (backtrackable-port:backtrackable-port-get-row bp))
                   (c-column (backtrackable-port:backtrackable-port-get-column bp))
                   (c (backtrackable-port:read-character bp)))
           (e1:if (range-set:has? rs c)
             (regexp:result-success initial-row
                                    initial-column
                                    c-row
                                    c-column
                                    (list:cons c characters))
             (e1:begin
               (backtrackable-port:backtrack! bp beginning-state)
               (regexp:result-failure))))))
      ((regexp:regexp-sequence first second)
       (e1:match (regexp:read-regexp-helper bp first initial-row initial-column final-row final-column characters)
         ((regexp:result-failure)
          (backtrackable-port:backtrack! bp beginning-state)
          (regexp:result-failure))
         ((regexp:result-success _ _ new-final-row new-final-column new-characters)
          (regexp:read-regexp-helper bp second initial-row initial-column new-final-row new-final-column new-characters))))
      ((regexp:regexp-or first second)
       (e1:match (regexp:read-regexp-helper bp first initial-row initial-column final-row final-column characters)
         ((regexp:result-failure)
          (backtrackable-port:backtrack! bp beginning-state) ;; FIXME: unneeded?  The recursive call has alraeady backtracked
          (regexp:read-regexp-helper bp second initial-row initial-column final-row final-column characters))
         (success
          success)))
      ((regexp:regexp-plus plussed)
       (regexp:read-regexp-helper
           bp
           (regexp:regexp-sequence plussed
                                   (regexp:regexp-or regexp
                                                     (regexp:regexp-empty)))
           initial-row initial-column
           final-row final-column
           characters)))))


;;;;; String-to-fixnum parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This assumes that the radix prefix, if any, is well-formed; we're
;;; supposed to call this only *after* recognizing a fixnum as
;;; well-formed, thru a regexp.  Anyway this code checks for
;;; out-of-range character digits, so the regexp can be kept simple.
(e1:define (reader:string->fixnum s)
  (e1:if (whatever:eq? (string:get s 0) #\#)
    (e1:case (string:get s 1)
      ((#\b) ; #b[s]MMM binary
       (reader:string->fixnum-sign-and-magnitude-helper s 2 2))
      ((#\o) ; #o[s]MMM octal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 8))
      ((#\d) ; #o[s]MMM decimal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 10))
      ((#\x) ; #o[s]MMM hexadecimal
       (reader:string->fixnum-sign-and-magnitude-helper s 2 16))
      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (e1:if (whatever:eq? (string:get s 2) #\r)
         ;; #Rr[s]MMM R in radix 10
         (e1:let ((radix (reader:character-value (string:get s 1) 10)))
           (reader:string->fixnum-sign-and-magnitude-helper s 3 radix))
         ;; #RRr[s]MMM RR in radix 10
         (e1:let ((radix (fixnum:+ (fixnum:* 10 (reader:character-value (string:get s 1) 10))
                                   (reader:character-value (string:get s 2) 10))))
           (reader:string->fixnum-sign-and-magnitude-helper s 4 radix)))))
    (reader:string->fixnum-sign-and-magnitude-helper s 0 10)))

(e1:define (reader:string->fixnum-sign-and-magnitude-helper s i radix)
  (e1:case (string:get s i)
    ((#\-)
     (fixnum:negate (reader:string->fixnum-magnitude-helper s (fixnum:1+ i) 0 radix)))
    ((#\+)
     (reader:string->fixnum-magnitude-helper s (fixnum:1+ i) 0 radix))
    (else
     (reader:string->fixnum-magnitude-helper s i 0 radix))))

(e1:define (reader:string->fixnum-magnitude-helper s i acc radix)
  (e1:if (fixnum:= i (string:length s))
    acc
    (e1:let ((c (string:get s i)))
      (e1:if (reader:valid-for-radix? c radix)
        (reader:string->fixnum-magnitude-helper s
                                                (fixnum:1+ i)
                                                (fixnum:+ (fixnum:* radix acc)
                                                          (reader:character-value c radix))
                                                radix)
        (e1:error "bad character")))))

(e1:define (reader:valid-for-radix? character radix)
  (e1:if (fixnum:<= radix 10)
    (e1:and (fixnum:<= #\0 character)
            (fixnum:< character (fixnum:+ #\0 radix)))
    (e1:or (e1:and (fixnum:<= #\0 character)
                   (fixnum:<= character #\9))
           (e1:and (fixnum:<= #\a character)
                   (fixnum:<  character (fixnum:+ #\a radix -10))))))

;;; This assumes that the given character be valid for the given radix.
(e1:define (reader:character-value character radix)
  (e1:if (e1:or (fixnum:<= radix 10)
                (e1:and (fixnum:<= #\0 character)
                        (fixnum:<= character #\9)))
    (fixnum:- character #\0)
    (fixnum:+ (fixnum:- character #\a)
              10)))


;;;;; Reader programmable interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum reader:result
  (success sexpression)
  (ignore)
  (failure))

;;; A reader case is a closure, taking a backtrackable input port and
;;; returning an s-expression option.  If the option is none, then the
;;; case failed recognition; otherwise its content is the recognized
;;; s-expression.
(e1:define-record reader:case
  closure) ;; backtrackable-input-port -> reader:result

;;; An item list of reader cases
(e1:define reader:item-list-box
  (box:make item-list:nil))

;;; An item list of reader cases to be ignored, or fail: none of these
;;; cases can successfully recognize anything.  Each case is a
;;; reader:case.
(e1:define reader:ignore-item-list-box
  (box:make item-list:nil))

(e1:define-record reader:atom-case
  regexp
  closure) ;; (string locus) -> s-expression

;;; An item list of atom cases.
(e1:define reader:atom-item-list-box
  (box:make item-list:nil))

(e1:define-record reader:prefix-case
  prefix-regexp
  closure) ;; (prefix-string s-expression) -> s-expression reader:result

;;; An item list of prefix cases.
(e1:define reader:prefix-item-list-box
  (box:make item-list:nil))

;;; The simplest and most common prefix case, recognizing PREFIX e
;;; as the two-element s-list (SYMBOL e).
(e1:define (reader:simple-prefix-case prefix-regexp symbol-name)
  (reader:prefix-case
   prefix-regexp
   (e1:lambda (prefix-string prefix-locus sexpression)
     ;;; If the prefix regexp matched, we always succeed:
     (e1:let* ((ssymbol (sexpression:make-with-locus sexpression:symbol-tag
                                                     symbol-name
                                                     prefix-locus))
               (sexpression-locus (sexpression:get-locus sexpression))
               (empty-slist (sexpression:make-with-locus sexpression:empty-list-tag
                                                         0
                                                         sexpression-locus))
               (joined-locus (locus:join prefix-locus sexpression-locus)))
       (reader:result-success
        (sexpression:cons-with-locus ssymbol
                                     (sexpression:cons-with-locus sexpression
                                                                  empty-slist
                                                                  sexpression-locus)
                                     joined-locus))))))

;; FIXME: hide the backtrackable port from the user: she should only
;; see a generic input port, and be able to change it; ideally, in a
;; stack fashion.
(e1:define (reader:eof? bp)
  (e1:or (backtrackable-port:eof? bp)
         (e1:begin
           (reader:eat-ignorables bp)
           ;; FIXME: is this somehow different from eat-ignorables?  I think not
           ;;(regexp:read-regexp bp (regexp:sregexp->regexp 'sexpression:ignorable))
           (backtrackable-port:eof? bp))))
(e1:define (reader:read bp)
  (reader:read-helper (box:get reader:item-list-box) bp))

(e1:define (reader:read-helper item-list bp)
  (e1:cond ((backtrackable-port:eof? bp)
            (e1:error "end of input"))
           ((list:null? item-list)
            (e1:error "all rules failed"))
           (else
            (backtrackable-port:commit! bp)
            (e1:match (e1:call-closure (cons:get-cdr (list:head item-list)) bp)
              ((reader:result-ignore)
               (reader:read bp))
              ((reader:result-failure)
               (reader:read-helper (list:tail item-list) bp))
              ((reader:result-success sexpression)
               sexpression)))))

(e1:define (reader:recognize-atom string locus)
  (e1:let* ((p (input-port:string->input-port string))
            (file-name-option (locus:locus->file-name-option locus))
            (bp (backtrackable-port:input-port->backtrackable-port p file-name-option))
            (state (backtrackable-port:backtrackable-port->state bp)))
    (reader:recognize-atom-helper (box:get reader:atom-item-list-box) bp state locus)))

(e1:define (reader:recognize-atom-helper item-list bp state locus)
  (e1:if (list:null? item-list)
    (e1:error "all atom rules failed")
    (e1:let* ((atom-case (cons:get-cdr (list:head item-list)))
              (regexp (reader:atom-case-get-regexp atom-case))
              (closure (reader:atom-case-get-closure atom-case)))
      (e1:match (regexp:read-regexp bp regexp)
        ((regexp:result-failure)
         (reader:recognize-atom-helper (list:tail item-list) bp state locus))
        ((regexp:result-success initial-row initial-column final-row final-column string)
         (e1:if (backtrackable-port:eof? bp)
           (e1:call-closure closure string locus)
           (e1:begin ;; we didn't match the whole input
             (backtrackable-port:backtrack! bp state)
             (reader:recognize-atom-helper (list:tail item-list) bp state locus))))))))


(e1:define regexp:ignorable
  (regexp:sregexp->regexp 'sexpression:ignorable))
(e1:define regexp:open
  (regexp:sregexp->regexp '#\())
(e1:define regexp:close
  (regexp:sregexp->regexp '#\)))
(e1:define regexp:dot
  (regexp:sregexp->regexp '#\.))
(e1:define regexp:string
  (regexp:sregexp->regexp 'sexpression:string))
(e1:define regexp:boolean
  (regexp:sregexp->regexp '(#\# (\| #\t
                                    #\f))))

(e1:define regexp:quote-prefix
  (regexp:sregexp->regexp '"'"))
(e1:define regexp:quasiquote-prefix
  (regexp:sregexp->regexp '"`"))
(e1:define regexp:unquote-prefix
  (regexp:sregexp->regexp '","))
(e1:define regexp:unquote-splicing-prefix
  (regexp:sregexp->regexp '",@"))
(e1:define regexp:comment-prefix
  (regexp:sregexp->regexp '"#;"))

;; rest ::=
;;   )                     { () }
;; | . <s-expression> )    { s-expression }
;; | <s-expression> <rest> { s-cons(s-expression, rest) }
(e1:define (reader:read-rest bp)
  (reader:eat-ignorables bp)
  (e1:match (regexp:read-regexp bp regexp:close)
    ((regexp:result-success initial-row initial-column final-row final-column _)
     (sexpression:make-with-locus
         sexpression:empty-list-tag
         (e0:value 0)
         (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                            initial-row initial-column
                            final-row final-column
                            string:empty)))
    ((regexp:result-failure)
     (e1:match (regexp:read-regexp bp regexp:dot)
       ((regexp:result-success _ _ _ _ _)
        (reader:eat-ignorables bp)
        (e1:let ((sexpression (reader:read bp)))
          (reader:eat-ignorables bp)
          (e1:match (regexp:read-regexp bp regexp:close)
            ((regexp:result-failure)
             (e1:error "expected closed parens"))
            ((regexp:result-success close-initial-row close-initial-column close-final-row close-final-column _)
             (e1:let* ((sexpression-locus (sexpression:get-locus sexpression))
                       (close-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                       close-initial-row close-initial-column
                                                       close-final-row close-final-column
                                                       string:empty)))
               (sexpression:with-locus sexpression
                                       (locus:join sexpression-locus close-locus)))))))
       ((regexp:result-failure) ;; we didn't recognize "."
        (e1:let* ((sexpression (reader:read bp))
                  (sexpression-locus (sexpression:get-locus sexpression))
                  (rest (reader:read-rest bp))
                  (rest-locus (sexpression:get-locus rest)))
          (sexpression:cons-with-locus sexpression
                                       rest
                                       (locus:join sexpression-locus
                                                   rest-locus))))))))


;; s-expression ::=
;; | prefix <s-expression> { lookup-procedure(prefix)(s-expression, scanner-state) }
(e1:define (reader:recognize-prefixed bp)
  (reader:recognize-prefixed-helper bp (box:get reader:prefix-item-list-box)))
(e1:define (reader:recognize-prefixed-helper bp item-list)
  (e1:if (list:null? item-list)
    (reader:result-failure)
    (e1:let* ((prefix-case (cons:get-cdr (list:head item-list)))
              (regexp (reader:prefix-case-get-prefix-regexp prefix-case))
              (closure (reader:prefix-case-get-closure prefix-case)))
      (e1:match (regexp:read-regexp bp regexp)
        ((regexp:result-failure)
         (reader:recognize-prefixed-helper bp (list:tail item-list)))
        ((regexp:result-success initial-row initial-column final-row final-column prefix-string)
         (e1:let ((read-sexpression (reader:read bp))
                  (prefix-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                   initial-row initial-column
                                                   final-row final-column
                                                   string:empty)))
           (e1:match (e1:call-closure closure prefix-string prefix-locus read-sexpression)
             ((reader:result-ignore)
              (reader:result-ignore))
             ((reader:result-failure)
              (reader:result-failure))
             ((reader:result-success result-sexpression)
              (e1:let* ((read-sexpression-locus (sexpression:get-locus read-sexpression))
                        (result-locus (locus:join prefix-locus read-sexpression-locus)))
                (reader:result-success (sexpression:with-locus result-sexpression
                                                               result-locus)))))))))))

(e1:define (reader:eat-ignorables bp)
  ;; Keep eating until we fail:
  (e1:match (reader:eat-ignorables-helper bp (box:get reader:ignore-item-list-box))
    ((reader:result-ignore)
     (backtrackable-port:commit! bp)
     (reader:eat-ignorables bp))
    ((reader:result-failure)
     (reader:result-failure))))
(e1:define (reader:eat-ignorables-helper bp item-list)
  (e1:if (list:null? item-list)
    (reader:result-failure)
    (e1:match (e1:call-closure (cons:get-cdr (list:head item-list)) bp)
      ((reader:result-ignore)
       (reader:result-ignore))
      ((reader:result-failure)
       (reader:eat-ignorables-helper bp (list:tail item-list)))
      ((reader:result-success _)
       (e1:error "eat-ignorables: not supposed to succeed")))))

(e1:toplevel
  (item-list:add-first!
     reader:item-list-box
     (e1:value ignorable)
     (e1:lambda (bp)
       (reader:eat-ignorables bp)))

  (item-list:add-first!
     reader:ignore-item-list-box
     (e1:value ignorable-regexp)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:ignorable)
         ((regexp:result-success _ _ _ _ _)
          (reader:result-ignore))
         ((regexp:result-failure)
          (reader:result-failure)))))

  (item-list:add-last!
     reader:ignore-item-list-box
     (e1:value comment-prefix)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:comment-prefix)
         ((regexp:result-success _ _ _ _ _)
          (reader:read bp) ;; eat and ignore this
          (reader:result-ignore))
         ((regexp:result-failure)
          (reader:result-failure)))))

  (item-list:add-after!
     reader:item-list-box (e1:value ignorable)
     (e1:value parenthesized)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:open)
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column _)
          (e1:let* ((open-locus (locus:locus-known (backtrackable-port:backtrackable-port-get-file-name-option bp)
                                                   initial-row initial-column
                                                   final-row final-column
                                                   string:empty))
                    (rest (reader:read-rest bp))
                    (rest-locus (sexpression:get-locus rest)))
            (reader:result-success (sexpression:with-locus rest
                                                           (locus:join open-locus
                                                                       rest-locus))))))))

  (item-list:add-after!
     reader:item-list-box (e1:value parenthesized)
     (e1:value prefix)
     (e1:lambda (bp)
       (reader:recognize-prefixed bp)))

  (item-list:add-after!
     reader:item-list-box (e1:value parenthesized)
     (e1:value string)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp regexp:string)
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column string)
          (e1:let* ((result-string (reader:unescape-string-literal string))
                    (file-name-option (backtrackable-port:backtrackable-port-get-file-name-option bp))
                    (locus (locus:locus-known file-name-option
                                              initial-row initial-column
                                              final-row final-column
                                              string:empty)))
            (reader:result-success (sexpression:make-with-locus sexpression:string-tag
                                                                result-string
                                                                locus)))))))

  (item-list:add-last!
     reader:item-list-box
     (e1:value atom)
     (e1:lambda (bp)
       (e1:match (regexp:read-regexp bp (regexp:sregexp->regexp 'sexpression:atom))
         ((regexp:result-failure)
          (reader:result-failure))
         ((regexp:result-success initial-row initial-column final-row final-column string)
          (e1:let* ((file-name-option (backtrackable-port:backtrackable-port-get-file-name-option bp))
                    (locus (locus:locus-known file-name-option
                                              initial-row initial-column
                                              final-row final-column
                                              string:empty)))
            (reader:result-success (reader:recognize-atom string locus)))))))

  (item-list:add-last!
     reader:prefix-item-list-box
     (e1:value quasiquote)
     (reader:simple-prefix-case regexp:quasiquote-prefix (e1:value quasiquote)))
  (item-list:add-last!
     reader:prefix-item-list-box
     (e1:value unquote)
     (reader:simple-prefix-case regexp:unquote-prefix (e1:value unquote)))
  (item-list:add-first! ;; ",@" needs to come before ","
     reader:prefix-item-list-box
     (e1:value unquote-splicing)
     (reader:simple-prefix-case regexp:unquote-splicing-prefix (e1:value unquote-splicing)))
  (item-list:add-first! ;; "'" should be the most common case.  Make it the first.
     reader:prefix-item-list-box
     (e1:value quote)
     (reader:simple-prefix-case regexp:quote-prefix (e1:value quote)))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value boolean)
     (reader:atom-case regexp:boolean
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:boolean-tag
                                                      (whatever:eq? (string:get string 1) #\t)
                                                      locus))))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value fixnum)
     (reader:atom-case (regexp:sregexp->regexp 'sexpression:fixnum)
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:fixnum-tag
                                                      (reader:string->fixnum string)
                                                      locus))))

  (item-list:add-first!
     reader:atom-item-list-box
     (e1:value unescaped-character)
     (reader:atom-case (regexp:sregexp->regexp 'sexpression:unescaped-character)
                       (e1:lambda (string locus)
                         (sexpression:make-with-locus sexpression:character-tag
                                                      (string:get string 2) ;; #\a
                                                      locus))))

  (item-list:add-last!
     reader:atom-item-list-box
     (e1:value symbol)
     (reader:atom-case regexp:anything ;; this always matches
                       (e1:lambda (string locus)
                         ;; FIXME: unescape
                         (sexpression:make-with-locus sexpression:symbol-tag
                                                      (symbol:string->symbol string)
                                                      locus)))))


;;;;; Character and string escaping
;;;;; FIXME: move the rest of the implementation here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:toplevel
  (sexpression:set-character-escape! #\nul "nul")
  (sexpression:set-character-escape! #\space "space")
  (sexpression:set-character-escape! #\tab "tab")
  (sexpression:set-character-escape! #\newline "newline")
  (sexpression:set-character-escape! #\cr "cr")

  (sexpression:set-string-escape! #\" #\")
  (sexpression:set-string-escape! #\\ #\\)
  (sexpression:set-string-escape! #\tab #\t)
  (sexpression:set-string-escape! #\newline #\n)
  (sexpression:set-string-escape! #\cr #\c)
  )


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define s1_
;0123456789
"abc def g ")
(e1:define s1 "'a     ;; line 1
(a)     ;; line 2
(a #;(all of this is ignored (cool ,is not it?))) ;; line 3
(a . b) ;; line 4
#b1110  ;; line 5
(a b)   ;; line 6
\"abcd\"
`((a . b) ,c ((a (d e . f)) h) ,@g)
#t;; a comment right after the atom
-
a-b
'quoted
`quasiquoted
,unquoted
,@unquoted-splicing
#;(a (very) complex . comment) 452
( )
()
#f
(a b)
(a . b)
(. b)
(define (fact n)
  (if (zero? n)
     1
     (* n (fact (1- n)))))
\"this is a string\"
(define (fact n)
  (if (zero? n)
     1
     (* n (fact (1- n)))))
(define (fact n)
  (if (zero? n)
     1
     (* n (fact (1- n)))))
(THIS IS THE LAST S-EXPRESSION)
;; a little whitespace
;; and
;; some
;; comments
")
;;(e1:define s1 "aaaaab")
;(e1:define s1 "one (#t ()) two (a b . c) #f three four five six seven")
(e1:define p1 (input-port:string->input-port s1))
(e1:define b1 (backtrackable-port:input-port->backtrackable-port p1 (option:option-none)))

;;;;;;;;;;
(e1:define (backtrackable-port:print-port-point bp)
  (backtrackable-port:print-point (backtrackable-port:backtrackable-port-get-row bp)
                                  (backtrackable-port:backtrackable-port-get-column bp)))
(e1:define (backtrackable-port:print-point row column)
  (fio:write ":" (i row)
             ":" (i column)
             "\n"))
;;;;;;;;;

(define-macro (test-sregexp sregexp)
  `(e1:toplevel
       (fio:write   "                     0123456789012345678901234567890")
       (fio:write "\nThe whole string is >" (s s1) "<\n")
       (string:write "First: ")
       (backtrackable-port:print-port-point b1)
       (e1:match (regexp:read-regexp b1 (regexp:sregexp->regexp ',sregexp))
         ((regexp:result-success initial-row initial-column final-row final-column string)
          (fio:write "* Recognized " (i (string:length string))
                     " characters: >" (st string) "<\n")
          (string:write "Last:  ")
          (backtrackable-port:print-point final-row final-column))
         ((regexp:result-failure)
          (string:write "* FAILURE.\n")))
       (e1:bundle)))

(define (test)
  (let* ((q (begin
              (e1:define last-read-sexpression (reader:read b1))
              (e1:toplevel last-read-sexpression)))
         (locus (buffer:get q (e0:value 2))))
    ;;(display q) (newline)
    (format #t "S-expression, converted to Guile:\n  ~s\n" (sexpression->guile-sexpression q))
    (format #t "Locus:\n  ~s\n" locus)
    (format #t "~s\n" q)
    (values)))

;;;;;;;;;;;;
(e1:define s "a")
(e1:define (test new-character)
  (string:set! s 0 new-character)
  (printer:write-string (io:standard-output) s)
  (io:write-character (io:standard-output) 10)
  (io:write-string (io:standard-output) "The first character of the string is ")
  (printer:write-character (io:standard-output) (string:get s 0))
  (io:write-character (io:standard-output) 10)
  )
