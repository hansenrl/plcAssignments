
;;; Supported Forms
;<program>     ::= <form>* 
;<form>        ::= <expression>  
;<expression>  ::= <constant>  
;              ::= <variable>  
;              ::= (quote <datum>)  
;              ::= (lambda <formals> <expression> <expression>*)  
;              ::= (if <expression> <expression> <expression>)
;              ::= (if <expression> <expression>)
;              ::= <application>  
;              ::= (begin <expression>*)
;<constant>    ::= <boolean> | <number> | <character> | <string> | <vector> 
;<formals>     ::= <variable>  
;              ::= (<variable>*)  
;              ::= (<variable> <variable>* . <variable>)  
;<application> ::= (<expression> <expression>*)  


(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (expand-syntax (parse-expression exp))]
	   [initial-environment global-env]
	   [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [lambda-exp (id body)
		       (make-closure id body env)]
	   [app-exp (operator operand)
		    (let ([procedure (eval-expression operator env)]
			  [arg (eval-expression-list operand env)])
		      (apply-proc procedure arg env))]
	   [if-exp (condition if-true)
		   (if (eval-expression condition env)
		       (eval-expression if-true env))]
	   [ifelse-exp (condition if-true if-false)
		       (if (eval-expression condition env)
			   (eval-expression if-true env)
			   (eval-expression if-false env))]
	   [letrec-exp (defs body)
		       (unparse-let 'letrec defs body)]
	   [namedletrec-exp (id defs body) 
			    (unparse-namedlet 'letrec id defs body)]
	   [let-exp (defs body)
		    (unparse-let 'let defs body)]
	   [namedlet-exp (id defs body) 
			 (unparse-namedlet 'let id defs body)]
	   [let*-exp (defs body)
		    (unparse-let 'let* defs body)]
	   [namedlet*-exp (id defs body) 
			 (unparse-namedlet 'let* id defs body)]
	   [set-exp (body)
		    (cons 'set!
			  (unparse-definition body))]
	   [begin-exp (body)
		      (eval-begin-list body env)]
	   [else (eopl:error 'eval-expression
			     "incorrect expression type ~s" exp)])))

(define expand-syntax
  (lambda (expr)
    (cases expression expr
	   [let-exp (defs body)
		    (app-exp (lambda-exp (map car defs)  (map expand-syntax body))
			     (map expand-syntax (map cadr defs)))]
	   [ifelse-exp (conditional if-true if-false)
		   (if-exp (expand-syntax conditional)
			   (expand-syntax if-true)
			   (expand-syntax if-false))]
	   [if-exp (conditional if-true)
		   (if-exp (expand-syntax conditional)
			   (expand-syntax if-true))]
	   [app-exp (operator operand)
		    (app-exp (expand-syntax operator) (map expand-syntax operand))]
	   [lambda-exp (ids bodies)
		       (lambda-exp ids (map expand-syntax bodies))]
	   [cond-exp (conditions bodies)
		     (if (equal? (car conditions) '(var-exp else))
			 (expand-syntax (car bodies))
			 (if (null? (cdr conditions))
			     (if-exp (expand-syntax (car conditions) )
				     (expand-syntax (car bodies)))
			     (ifelse-exp (expand-syntax (car conditions) )
					 (expand-syntax (car bodies))
					 (expand-syntax (cond-exp (cdr conditions) (cdr bodies))))))]
	   [and-exp (exps)
		    (if (null? exps)
			(lit-exp #t)
			(if (null? (cdr exps))
			    (expand-syntax (car exps))
			    (ifelse-exp (expand-syntax (car exps))
					(expand-syntax (and-exp (cdr exps)))
					(lit-exp #f))))]
	   [or-exp (exps)
		   (if (null? exps)
			(lit-exp #f)
			(if (null? (cdr exps))
			    (expand-syntax (car exps))
			    (ifelse-exp (expand-syntax (car exps))
					(lit-exp #t)
					(expand-syntax (or-exp (cdr exps))))))]
	   [let*-exp (defs body)
		     (if (null? defs)
			 (expand-syntax (let-exp '() body))
			 (expand-syntax (let-exp (list (car defs))
						 (list (let*-exp (cdr defs)
								 body)))))]
	   [case-exp (key conditions bodies)
		     (if (equal? (car conditions) '((var-exp else)))
			 (expand-syntax (car bodies))
			 (if (null? (cdr conditions))
			     (expand-syntax (if-exp (or-exp (map (lambda (exp)
								   (app-exp (var-exp 'eqv?)
									    (list key
										  exp)))
								 (car conditions)))
						    (car bodies)))
			     (expand-syntax (ifelse-exp (or-exp (map (lambda (exp)
								       (app-exp (var-exp 'eqv?)
										(list key
										      exp)))
								     (car conditions)))
							(car bodies)
							(case-exp key (cdr conditions) (cdr bodies))))))]
	   [else expr])))

(define eval-expression-list
  (lambda (explist env)
    (cond [(null? explist) '()]
	  [else 
	   (cons (eval-expression (car explist) env)
		 (eval-expression-list (cdr explist) env))])))

(define eval-begin-list
  (lambda (explist env)
    (cond [(null? explist) (void)]
	  [(null? (cdr explist))
	   (eval-expression (car explist) env)]
	  [else
	   (begin (eval-expression (car explist) env)
		  (eval-begin-list (cdr explist) env))])))

(define make-closure
  (lambda (id body env)
    (closure-record id body env)))

(define-datatype procedure procedure?
  [primitive
   (id symbol?)]
  [closure-record
   (id lambda-parameter-list?)
   (body (list-of expression?))
   (env environment?)])

(define apply-proc
  (lambda (proc args env)
    (if (procedure? proc)
	(cases procedure proc
	       [closure-record (id body env)
			       ;(display body)])
			       (eval-begin-list body (extend-env id args env))]
	       [primitive (id)
			  (apply-primitive-proc id args)])
	(proc arg))))

(define apply-primitive-proc
  (lambda (id args)
    (case id
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(car) (apply car args)]
      [(cdr) (cdar args)]
      [(add1) (apply add1 args)]
      [else 
       (apply (eval id) args)])))
      ;[else
       ;(eopl:error 'apply-primitive-proc
	;	   "primitive not defined ~s" id)])))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 zero? not = < > <= >= cons
      car cdr list null? eq? equal? atom? length list->vector
      list? pair? procedure? vector->list vector make-vector
      vector-ref vector? number? symbol? set-car! set-cdr!
      vector-set! caar cadr cdar cddr caaar caadr cadar
      caddr cdaar cdadr cddar cdddr eqv?))

(define global-env
  (extend-env *prim-proc-names*
	      (map primitive *prim-proc-names*)
	      (empty-env)))