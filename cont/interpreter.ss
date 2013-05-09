(define top-level-eval
  (lambda (form cont)
    (cases expression form
	   [define-exp (sym val)
	     (eval-expression val (extend-global-env-cont sym cont) (empty-env))]
	     ;(extend-global-env sym (eval-expression val (empty-env)))]
	   [else (eval-expression form cont (empty-env))])))

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (expand-syntax (parse-expression exp))]
	   [result (top-level-eval parse-tree (halt-cont))])
      result)))

(define eval-exps
  (lambda (exps cont env)
    (if (null? exps)
	(apply-cont cont '())
	(eval-expression (car exps) (eval-exps-cont (cdr exps) env cont) env))))

(define eval-expression
  (lambda (exp cont env)
    (cases expression exp
	   [var-exp (id) (apply-cont cont (apply-env env id))]
	   [lit-exp (val) (apply-cont cont val)]
	   [lambda-exp (id body)
		       (apply-cont cont (closure-record id body env))]
	   [app-exp (exps)
		    (eval-exps exps (proc-cont cont) env)]
	   [if-exp (condition if-true)
		   (eval-expression condition (if-cont if-true cont env) env)]
	   [ifelse-exp (condition if-true if-false)
		       (eval-expression condition (if-else-cont if-true if-false cont env) env)]
	   [letrec-exp (defs body)
		       (eval-begin-list body 
					cont
					(extend-env-recur (map car defs) 
							  (map (lambda (x) 
								 (eval-expression x (halt-cont) env)) ; does this need to be in cps?
							       (map cadr defs)) 
							  env))]
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
	   [set-exp (sym val)
		    (eval-expression val (set-cont env sym cont) env)]
		    ;(let ([the-val (eval-expression val env)])
		    ;  (change-env env
		;		  sym
		;		  the-val))]
	   [begin-exp (body)
		      (eval-begin-list body cont env)]
	   [while-exp (test-exp bodies)
		      (eval-expression test-exp (while-cont test-exp bodies cont env) env)]
		      ;(while-eval test-exp bodies env)]
	   [define-exp (sym val)
	     (if (exists-in-env? env sym)
		 (change-env env sym val)
		 (eval-expression val (extend-env-cont sym env cont) env))]
		 ;(extend-env sym (eval-expression val env) env))]
	   [call/cc-exp (receiver)
			(eval-expression receiver (call/cc-cont cont) env)]
	   [else (eopl:error 'eval-expression
			     "incorrect expression type ~s" exp)])))

(define expand-syntax
  (lambda (expr)
    (cases expression expr
	   [let-exp (defs body)
		    (app-exp (cons (lambda-exp (map car defs)  (map expand-syntax body))
			     (map expand-syntax (map cadr defs))))]
	   [ifelse-exp (conditional if-true if-false)
		   (ifelse-exp (expand-syntax conditional)
			   (expand-syntax if-true)
			   (expand-syntax if-false))]
	   [if-exp (conditional if-true)
		   (if-exp (expand-syntax conditional)
			   (expand-syntax if-true))]
	   [app-exp (exps)
		    (app-exp (map expand-syntax exps))]
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
			    (expand-syntax (let-exp (list (list 'res (expand-syntax (car exps))) )
						    (list (ifelse-exp (var-exp 'res)
								      (var-exp 'res)
								      (expand-syntax (or-exp (cdr exps)))))))))]
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
	   [define-exp (id exp)
	     (define-exp id (expand-syntax exp))]
	   [letrec-exp (defs exps)
		    (letrec-exp (map (lambda (def) 
				       (list (car def) 
					     (expand-syntax (cadr def)))) 
				     defs) 
				(map expand-syntax exps))]
	   [namedlet-exp (id defs bodies)
			 (letrec-exp (list (cons id 
						 (list (lambda-exp (map car defs) (map expand-syntax bodies))))) 
				     (list (app-exp (cons (var-exp id) (map expand-syntax (map cadr defs))))))]
	   [call/cc-exp (receiver)
			(call/cc-exp (expand-syntax receiver))]
	   [else expr])))

(define while-eval
  (lambda (test bodies env)
    (if (eval-expression test env)
	(begin (eval-begin-list bodies env)
	       (while-eval test bodies env)))))

(define eval-expression-list ; added continuation to this to make a "map" procedure that has continuations
  (lambda (explist env)
    (cond [(null? explist) '()]
	  [else 
	   (cons (eval-expression (car explist) env)
		 (eval-expression-list (cdr explist) env))])))

(define eval-begin-list
  (lambda (explist cont env)
    ;(begin (display env) (newline)
    (cond [(null? explist) (void)]
	  [(null? (cdr explist))
	   (begin ;(display "blahblah")
	   (eval-expression (car explist) cont env))]
	  [else
	   (if (and (pair? (car explist)) (eqv? 'define-exp (caar explist))) 
	       (eval-expression (car explist) (begin-list-env-ext-cont (cdr explist) cont) env)
					;(eval-begin-list (cdr explist) cont (eval-expression (car explist) env))
	       (eval-expression (car explist) (begin-list-cont (cdr explist) env cont) env))])))
	     ;(begin (eval-expression (car explist) cont env) (eval-begin-list (cdr explist) cont env)))])));)

(define make-closure
  (lambda (id body env)
    (closure-record id body env)))

(define-datatype proc proc?
  [primitive
   (id symbol?)]
  [closure-record
   (id lambda-parameter-list?)
   (body (list-of expression?))
   (env environment?)]
  [acontinuation
   (cont continuation?)])

(define apply-proc
  (lambda (procedure args cont)
    (if (proc? procedure)
	(cases proc procedure
	       [closure-record (id body env)
			       ;(display body)])
			       (eval-begin-list body cont (extend-env id args env))]
	       [primitive (id)
			  (apply-primitive-proc id args cont)]
	       [acontinuation (cont)
			      (apply-cont cont (car args))])
	(procedure args))))

(define map-eval
  (lambda (procedure expressions cont)
    (if (null? expressions)
	(apply-cont cont '())
	(apply-proc procedure (list (car expressions)) (map-eval-cont procedure (cdr expressions) cont)))))
	;(cons (apply-proc procedure (car expressions)) (map-eval procedure (cdr expressions)))))

(define apply-primitive-proc
  (lambda (id args cont)
    (case id
      ;[(+) (apply-cont cont (apply + args)]
      ;[(-) (apply - args)]
      ;[(car) (apply car args)]
      ;[(cdr) (cdar args)]
      ;[(add1) (apply add1 args)]
      [(map) (map-eval (car args) (cadr args) cont)]
      ;[(map) (map (lambda (x) (apply-proc (car args) (list x) cont)) (cadr args))]
      [(apply) (apply (eval (cadar args)) (cadr args))]
      [(procedure?) (or (proc? (car args)))]
      [(break) (apply-cont (halt-cont) args)]
			       
			 
      [else 
       (apply-cont cont (apply (eval id) args))])))
      ;[else
       ;(eopl:error 'apply-primitive-proc
	;	   "primitive not defined ~s" id)])))

(define *prim-proc-names* 
  '(+ - * / add1 sub1 zero? not = < > <= >= cons
      car cdr list null? eq? equal? atom? length list->vector
      list? pair? procedure? vector->list vector make-vector
      vector-ref vector? number? symbol? set-car! set-cdr!
      vector-set! caar cadr cdar cddr caaar caadr cadar max
      caddr cdaar cdadr cddar cdddr eqv? set-car! map apply assq assv append break))

(define make-init-env
  (lambda ()
    (map (lambda (name)
	   (cons name (primitive name)))
	 *prim-proc-names*)))

(define global-env
  (make-init-env))