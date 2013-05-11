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
  (while-cont
   (test expression?)
   (bodies (list-of expression?))
   (cont continuation?)
   (env environment?))
  (begin-while-cont
   (test expression?)
   (bodies (list-of expression?))
   (cont continuation?)
   (env environment?)))

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
	   [while-cont (test bodies cont env)
		       (if val
			   (eval-expression (car bodies) 
					    (begin-while-cont test bodies cont env)
					    env))]
	   [begin-while-cont (test bodies cont env)
			     (cond [(null? bodies) (eval-expression test (while-cont test bodies cont env) env)]
				   [else (eval-expression (car bodies) 
							  (begin-while-cont test (cdr bodies) cont env)
							  env)])])))
