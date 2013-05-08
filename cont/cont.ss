(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (eval-exps-cont
   (exps (list-of expression?))
   (env scheme-value?)
   (cont continuation?))
  (if-cont
   (true-exp expression?)
   (cont continuation?)
   (env list?))
  (if-else-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?))
  (set-cont
   (env environment?)
   (sym symbol?)
   (cont continuation?))
  (extend-env-cont
   (sym symbol?)
   (env environment?)
   (cont continuation?))
  (extend-global-env-cont
   (sym symbol?)
   (cont continuation?)))

(define scheme-value? (lambda (x) #t))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()
		      val]
	   [eval-exps-cont (exps env cont)
			   (eval-exps exps (cons-cont val cont) env)]
	   [cons-cont (v cont)
		      (apply-cont cont (cons v val))]

	   [proc-cont (cont)
		      (apply-proc (car val) (cdr val) cont)]
	   [if-cont (if-true-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env))]
	   [if-else-cont (if-true-exp if-false-exp next-cont env)
		    (if val
			(eval-expression if-true-exp next-cont env)
			(eval-expression if-false-exp next-cont env))]
	   [set-cont (env sym cont)
		     (apply-cont cont (change-env env sym val))]
	   [extend-env-cont (sym env cont)
			    (apply-cont cont (extend-env sym val env))]
	   [extend-global-env-cont (sym cont)
				   (apply-cont cont (extend-global-env sym val))])))
	    
