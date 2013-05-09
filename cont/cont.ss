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
   (cont continuation?))
  (while-cont
   (test expression?)
   (bodies (list-of expression?))
   (cont continuation?)
   (env environment?))
  (begin-while-cont
   (orig-test expression?)
   (orig-bodies (list-of expression?))
   (bodies (list-of expression?))
   (cont continuation?)
   (env environment?))
  (begin-list-cont
   (explist (list-of expression?))
   (env environment?)
   (cont continuation?))
  (begin-list-env-ext-cont
   (explist (list-of expression?)) ; the env is passed in from apply-cont as the val
   (cont continuation?))
  (call/cc-cont
   (cont continuation?))
  (map-eval-cont 
   (procedure proc?)
   (args (list-of scheme-value?))
   (cont continuation?))
  (rep-cont))

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
				   (apply-cont cont (extend-global-env sym val))]
	    
	   [while-cont (test bodies cont env)
		       (if val
			   (eval-expression (car bodies) 
					    (begin-while-cont test bodies (cdr bodies) cont env)
					    env)
			   (apply-cont cont (void)))]
	   [begin-while-cont (orig-test orig-bodies cur-bodies cont env)
			     (cond [(null? cur-bodies) (eval-expression orig-test 
									(while-cont orig-test orig-bodies cont env) 
									env)]
				   [else (eval-expression (car cur-bodies) 
							  (begin-while-cont orig-test 
									    orig-bodies 
									    (cdr cur-bodies) 
									    cont 
									    env)
							  env)])]
	   [begin-list-cont (explist env cont)
			    (cond [(null? explist) (apply-cont cont val)]
				  [(and (pair? (car explist)) (eqv? 'define-exp (caar explist)))
				   (eval-expression (car explist) (begin-list-env-ext-cont (cdr explist) cont) env)]
				  [else (eval-expression (car explist) 
							 (begin-list-cont (cdr explist) env cont) 
							 env)])]
	   [begin-list-env-ext-cont (explist cont)
				    (cond [(null? explist) (apply-cont cont (void))]
					  [(and (pair? (car explist)) (eqv? 'define-exp (caar explist)))
					   (eval-expression (car explist) 
							    (begin-list-env-ext-cont (cdr explist) cont) 
							    val)]
					  [else
					   (eval-expression (car explist)
							    (begin-list-cont (cdr explist) val cont)
							    val)])]
	   [map-eval-cont (procedure args cont)
		     (map-eval procedure args (cons-cont val cont))]
	   ;[cons-map-cont (procedure args cont)
	;		  (cons val (apply-proc procedure (cadr args)))]
	   [rep-cont ()
		     (begin (pretty-print val)
			    (display "--> ")
			    (let ([in (read)])
			      (if (not (equal? in '(exit)))
				  (top-level-eval (expand-syntax (parse-expression in)) (rep-cont)))))]
	   [call/cc-cont (cont)
		    (cases proc val
			   [closure-record (id body env)
				    (eval-begin-list body
						     cont
						     (extend-env id (list (acontinuation cont)) env))]
				
			   [else (eopl:error "Must call call/cc with a lambda, not ~s" val)])]
			    )))
