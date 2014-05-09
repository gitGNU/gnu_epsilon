;;; THIS IS THE "STABLE", WORKING VERSION -- WITH ITS LIMITATIONS, OF COURSE

;;;;; Partial evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(e1:define-sum peval:value
  (unknown expression)
  (known values))

(e1:define (peval:values-knowns? vs)
  (list:for-all? (e1:lambda (v) (peval:value-known? v)) vs))

(e1:define (peval:value-known->whatever v)
  (e1:match v
    ((peval:value-known vs)
     (list:head vs))
    (else
     (e1:error "unknown"))))
(e1:define (peval:values-knowns->whatevers vs)
  (list:map (e1:lambda (v) (peval:value-known->whatever v)) vs))

(e1:define (peval:value->expression v)
  (e1:match v
    ((peval:value-unknown e)
     e)
    ((peval:value-known vs)
     (e1:if (fixnum:= (list:length vs) 1)
       (e0:value* (list:head vs))
       (e0:bundle* (list:map (e1:lambda (v) (e0:value* v))
                             vs))))))

(e1:define (peval:values->expressions vs)
  (list:map (e1:lambda (v) (peval:value->expression v))
            vs))

;;; Similar to alist:bind-lists, but here we ignore excess values.
(e1:define (peval:bind-epsilon-values bs variables vs)
  (e1:cond ((list:null? variables)
            bs)
           ((list:null? vs)
            (e1:error "too many variables"))
           (else
            (peval:bind-epsilon-values (alist:bind bs
                                                   (list:head variables)
                                                   (list:head vs))
                                       (list:tail variables)
                                       (list:tail vs)))))

;;; A variable may be bound either in the given peval bindings (which
;;; of course has higher priority), or globally.  The associated
;;; object is a whatever value (not a peval value).
(e1:if #t
  (e1:begin
    (e1:define (peval:bound-variable? bs x)
      (e1:or (alist:has? bs x)
             (state:global? x)))
    (e1:define (peval:lookup-bound-variable bs x)
      (e1:if (alist:has? bs x)
        (alist:lookup bs x)
        (state:global-get x))))
  (e1:begin
    ;; FIXME: I currently disabled global support here.  This would work
    ;; fine if I explicitly shadowed globals before starting partial evaluation.
    (e1:define (peval:bound-variable? bs x)
      (alist:has? bs x))
    (e1:define (peval:lookup-bound-variable bs x)
      (alist:lookup bs x))))

;;; The implemenation is clunky, but this is more delicate than it looks:
;;; bindings introduced by this should all be activated "in parallel", without
;;; each successive binding being affected by the previous ones.
(e1:define (peval:bind-bundle bs variables is)
  (e1:let* ((unflattened-new-bindings
             (list:map (e1:lambda (pair)
                         (peval:bind-item bs
                                          (cons:get-car pair)
                                          (cons:get-cdr pair)))
                       (list:zip variables
                                 (list:take is (list:length variables)))))
            (new-bindings (list:flatten unflattened-new-bindings)))
  (alist:append bs new-bindings)))
(e1:define (peval:bind-item bs variable i)
  (e1:match i
    ((e0:expression-variable _ x)
     (e1:if (peval:bound-variable? bs x)
       (list:list (cons:make variable (peval:lookup-bound-variable bs x)))
       list:nil))
    ((e0:expression-value _ c)
     (list:list (cons:make variable c)))
    ((e0:expression-bundle _ items)
     (peval:bind-item bs variable (list:head items)))
    (else
     list:nil)))

(e1:define (peval:bind-expression bs variables e)
  (e1:match e
    ((e0:expression-value _ v)
     (peval:bind-epsilon-values bs variables (list:list v)))
    ((e0:expression-variable _ x)
     (e1:if (peval:bound-variable? bs x)
       (peval:bind-epsilon-values bs variables (list:list (peval:lookup-bound-variable bs x)))
       bs))
    ((e0:expression-bundle _ is)
     (peval:bind-bundle bs variables is))
    (else
     bs)))
(e1:define (peval:bind bs variables value)
  (e1:match value
    ((peval:value-known vs)
     (peval:bind-epsilon-values bs variables vs))
    ((peval:value-unknown e)
     ;;(fio:write "OK-A: the bound expression is " (e e) "\n")
     (peval:bind-expression bs variables e))))

(e1:define (peval:peval-expression e bs)
  (peval:peval-expression-helper e bs #t))

;;; The r parameter stands for "recurse".  It is kept true unless
;;; partial evaluation is taking place within a conditional with an
;;; unknown discriminand.  This way we guarantee termination and
;;; generation of finite residual code, unless the original code
;;; loops.
(e1:define (peval:peval-expression-helper e bs r)
  ;;(fio:write "peval'ing " (e e) ", r=" (st (e1:if r "#t" "#f")) "\n")
  (e1:match e
    ((e0:expression-variable h x)
     (e1:if (peval:bound-variable? bs x)
       (peval:value-known (list:list (peval:lookup-bound-variable bs x)))
       (peval:value-unknown e)))
    ((e0:expression-value h v)
     (peval:value-known (list:list v)))
    ((e0:expression-bundle h items)
     (e1:let ((vs (peval:peval-expressions-helper items bs r)))
       (e1:if (peval:values-knowns? vs)
         (peval:value-known (peval:values-knowns->whatevers vs))
         (peval:value-unknown (e0:bundle* (peval:values->expressions vs))))))
    ((e0:expression-primitive h name actuals)
     (e1:let ((vs (peval:peval-expressions-helper actuals bs r)))
       (e1:if (e1:and (e1:not (state:primitive-side-effecting? name))
                      (e1:not (state:primitive-reflective? name))
                      (peval:values-knowns? vs))
         (e1:let ((ws (peval:values-knowns->whatevers vs)))
           ;;(fio:write "DANGEROUS BRANCH: CALLING PRIMITIVE " (sy name) " WITH ACTUALS:\n")
           ;; (e1:dolist (a actuals)
           ;;   (fio:write "* " (e a) "\n"))
           ;; (fio:write ", ACTUALLY COMPILE-TIME-EVALUATED TO \n")
           ;; (e1:dolist (w (peval:values-knowns->whatevers (peval:values->expressions vs)))
           ;;   (fio:write "* " (i w) "\n"))
           ;;(fio:write "\n")
           (peval:value-known (e0:call-primitive name ws)))
         (peval:value-unknown (e0:primitive* name (peval:values->expressions vs))))))
    ((e0:expression-let h bound-variables bound-expression body)
     (e1:let* ((bv (peval:peval-expression-helper bound-expression bs r))
               (new-bs (alist:unbind-all-list bs bound-variables))
               (bodyv (peval:peval-expression-helper body
                                              (peval:bind new-bs
                                                          bound-variables
                                                          bv)
                                              r)))
       (e1:if (peval:value-known? bv)
         bodyv
         (peval:value-unknown (e0:let* bound-variables
                                       (peval:value->expression bv)
                                       (peval:value->expression bodyv))))))
    ((e0:expression-call h procedure-name actuals)
     ;; FIXME: also inline recursive procedures, unless within a branch of
     ;; a dynamic conditional.
     (e1:if (e1:or r (e1:not (e0:procedure-recursive? procedure-name)))
       (e1:let ((inlined-call (e0:inlined-call procedure-name actuals)))
         (peval:peval-expression-helper inlined-call bs r))
       (peval:value-unknown (e0:call* procedure-name (peval:values->expressions (peval:peval-expressions-helper actuals bs r))))))
    ((e0:expression-call-indirect h procedure-expression actuals)
     ;;(fio:write "OK-A 0\n")
     (e1:let* ((pv (peval:peval-expression-helper procedure-expression bs r))
               (avs (peval:peval-expressions-helper actuals bs r))
               (aes (peval:values->expressions avs)))
       ;;(fio:write "OK-A 100: pv tag is " (i (cons:get-car pv)) "\n")
       (e1:match pv
         ((peval:value-unknown e)
          ;;(fio:write "OK-A 110\n")
          ;; (fio:write "Unfortunately peval didn't resolve the indirect call:\n"
          ;;            " * " (e procedure-expression)
          ;;            "\n => \n"
          ;;            " * " (e e)
          ;;            "\n")
          (peval:value-unknown (e0:call-indirect* e aes)))
         ((peval:value-known vs)
          (fio:write "GOOD! Peval turned an indirect call into a call to " (sy (list:head vs)) "\n")
          (peval:peval-expression-helper (e0:call* (list:head vs) aes) bs r)))))
    ((e0:expression-if-in h discriminand values then-branch else-branch)
     (e1:match (peval:peval-expression-helper discriminand bs r)
       ((peval:value-unknown e)
        (peval:value-unknown (e0:if-in* e values
                                        (peval:value->expression (peval:peval-expression-helper then-branch bs #f))
                                        (peval:value->expression (peval:peval-expression-helper else-branch bs #f)))))
       ((peval:value-known vv)
        (e1:if (list:has? values (list:head vv))
          (peval:peval-expression-helper then-branch bs r)
          (peval:peval-expression-helper else-branch bs r)))))
    ((e0:expression-fork h procedure-name actuals)
     (peval:value-unknown (e0:fork* procedure-name (peval:values->expressions (peval:peval-expressions-helper actuals bs r)))))
    ((e0:expression-join h future)
     (peval:value-unknown (e0:join* (peval:value->expression (peval:peval-expression-helper future bs r)))))))
(e1:define (peval:peval-expressions-helper ee bs r)
  (e1:if (list:null? ee)
    list:nil
    (list:cons (peval:peval-expression-helper (list:head ee) bs r)
               (peval:peval-expressions-helper (list:tail ee) bs r))))
