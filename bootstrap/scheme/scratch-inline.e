;;;;; Inlining
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An inline table is an unboxed hash mapping each procedure name, as
;;; symbol, into a boolean: #t if already inlined.

;; (e1:define (inline-leaves! #;inline-table call-graph procedure-names)
;;   (e1:unless (list:null? procedure-names)
;;     (inline-leaf! #;inline-table call-graph (list:head procedure-name))
;;     (inline-leaves! #;inline-table call-graph (list:tail procedure-name))))

(e1:define (inliner:inline-calls-in-procedure! procedure-name callees-to-inline)
  (e1:let* ((old-body (state:procedure-get-body procedure-name))
            (new-body (inliner:inline-calls-in-expression old-body callees-to-inline)))
    (state:procedure-set-body! procedure-name new-body)))

(e1:define (inliner:inline-calls-in-expression expression callees-to-inline)
  (e1:match expression
    ((or (e0:expression-variable _ _)
         (e0:expression-value _ _))
     expression)
    ((e0:expression-bundle _ items)
     (e0:bundle* (inliner:inline-calls-in-expressions items callees-to-inline)))
    ((e0:expression-primitive _ primitive-name actuals)
     (e0:primitive* primitive-name (inliner:inline-calls-in-expressions actuals callees-to-inline)))
    ((e0:expression-let _ bound-variables bound-expression body)
(e0:expression-without-unneeded-lets
     (e0:let* bound-variables
              (inliner:inline-calls-in-expression bound-expression callees-to-inline)
              (inliner:inline-calls-in-expression body callees-to-inline)))
)
    ((e0:expression-call _ procedure-name actuals)
     (e1:if (list:has? callees-to-inline procedure-name)
(e0:expression-without-unneeded-lets
       (e0:inlined-call procedure-name
                        (inliner:inline-calls-in-expressions actuals callees-to-inline))
)
       (e0:call* procedure-name (inliner:inline-calls-in-expressions actuals callees-to-inline))))
    ((e0:expression-fork _ procedure-name actuals)
     (e0:fork* procedure-name (inliner:inline-calls-in-expressions actuals callees-to-inline)))
    ((e0:expression-call-indirect _ procedure-expression actuals)
     (e0:call-indirect* (inliner:inline-calls-in-expression procedure-expression callees-to-inline)
                        (inliner:inline-calls-in-expressions actuals callees-to-inline)))
    ((e0:expression-if-in _ discriminand values then-branch else-branch)
     (e0:if-in* (inliner:inline-calls-in-expression discriminand callees-to-inline)
                values
                (inliner:inline-calls-in-expression then-branch callees-to-inline)
                (inliner:inline-calls-in-expression else-branch callees-to-inline)))
    ((e0:expression-join _ future)
     (e0:join* (inliner:inline-calls-in-expression future callees-to-inline)))))

(e1:define (inliner:inline-calls-in-expressions expressions callees-to-inline)
  (e1:if (list:null? expressions)
    list:nil
    (list:cons (inliner:inline-calls-in-expression (list:head expressions) callees-to-inline)
               (inliner:inline-calls-in-expressions (list:tail expressions) callees-to-inline))))

(e1:define (inliner:inline-calls-in-procedures! call-graph inlinable?-closure)
  (e1:let* ((procedures (call-graph:call-graph->procedures call-graph))
            (procedures (call-graph:topological-sort procedures-call-graph))
            (callees-to-inline (list:filter inlinable?-closure procedures)))
    (e1:dolist (procedure procedures)
      (inliner:inline-calls-in-procedure! procedure callees-to-inline))))

(e1:define (inliner:inline-leaf-calls-in-procedures! call-graph)
  (inliner:inline-calls-in-procedures! call-graph
                                       (e1:lambda (procedure)
                                         (call-graph:procedure-leaf? call-graph
                                                                     procedure))))

(e1:define (call-graph:call-graph->procedures call-graph)
  (list:map (e1:lambda (pair) (cons:car pair))
            (unboxed-hash:unboxed-hash->alist call-graph)))

(e1:define (call-graph:topological-sort call-graph)
  (call-graph:topological-sort-helper call-graph
                                      (call-graph:call-graph->procedures call-graph)
                                      (unboxed-hash:make)
                                      list:nil))
(e1:define (call-graph:topological-sort-helper graph stack seen acc)
  (e1:cond ((list:null? stack)
            acc)
           (bind (head (list:head stack))
                 (tail (list:tail stack)))
           ((unboxed-hash:has? seen head)
            (call-graph:topological-sort-helper graph tail seen acc))
           (else
            (e1:if (fixnum:zero? head)
              (fio:write "POPPING <indirect>\n")
              (fio:write "POPPING " (sy head) "\n"))
            (unboxed-hash:set! seen head #t)
            (e1:let ((children (call-graph:procedure-direct-callees graph head)))
              (call-graph:topological-sort-helper graph
                                                  (list:append-reversed children tail)
                                                  seen
                                                  (list:cons head acc))))))


;;;;; Scratch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(e1:define cg (unboxed-hash:make))

(e1:define-macro (test procedure-name)
  `(e1:let* ((cg (call-graph:build-from-procedure (e1:value ,procedure-name)))
             (sorted (call-graph:topological-sort cg)))
     (e1:dolist (p (call-graph:procedure-callees cg (e1:value ,procedure-name)))
       (e1:if (fixnum:zero? p)
         (fio:write "* <indirect>\n")
         (fio:write "* " (sy p) "\n")))
     (fio:write "Is " (sy (e1:value ,procedure-name)) " leaf? "
                (b (call-graph:procedure-leaf? cg (e1:value ,procedure-name)))
                "\n")
     (fio:write "Is " (sy (e1:value ,procedure-name)) " directly recusrive? "
                (b (call-graph:procedure-directly-recursive? cg (e1:value ,procedure-name)))
                "\n")
     (fio:write "Is " (sy (e1:value ,procedure-name)) " recusrive? "
                (b (call-graph:procedure-recursive? cg (e1:value ,procedure-name)))
                "\n")
     (fio:write "Here are the procedures, sorted in topological order:\n")
     (e1:dolist (p sorted)
       (e1:if (fixnum:zero? p)
         (fio:write "* <indirect>\n")
         (fio:write "* " (sy p) "\n")))))


;;;;; Scratch C64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define fixnum:/%-initial-mask
  (fixnum:long-division-initial-mask (e0:value 1)))

(e1:define (fixnum:non-primitive-/%-unsigned n d)
  (fixnum:non-primitive-/%-unsigned-helper n d
                                           fixnum:/%-initial-mask
                                           (e0:value 0) (e0:value 0)))
