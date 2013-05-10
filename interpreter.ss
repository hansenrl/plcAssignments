(define top-level-eval
  (lambda (form)
    (case (car form)
	   [(define-exp) 
	    (let ([sym (cadr form)]
		  [val (caddr form)])
	      (extend-global-env sym (eval-expression val (empty-env))))]
	   [else (eval-expression form (empty-env))])))

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (expand-syntax (parse-expression exp))]
	   [result (top-level-eval parse-tree)])
      result)))

(define eval-expression
  (lambda (exp env)
    (case (car exp)
      [(var-exp) 
       (let ([id (cadr exp)])
	 (apply-env env id))]
      [(lit-exp) 
       (let ([val (cadr exp)]) 
	 val)]
      [(lambda-exp) 
       (let ([id (cadr exp)]
	     [body (caddr exp)])
	 (make-closure id body env))]
      [(app-exp) 
       (let ([operator (cadr exp)]
	     [operand (caddr exp)])
	 (let ([proc (eval-expression operator env)]
	       [arg (eval-expression-list operand env)])
	   (apply-proc proc arg env)))]
      [(if-exp) 
       (let ([condition (cadr exp)]
	     [if-true (caddr exp)])
	 (if (eval-expression condition env)
	     (eval-expression if-true env)))]
      [(ifelse-exp)
       (let ([condition (cadr exp)]
	     [if-true (caddr exp)]
	     [if-false (cadddr exp)])
	 (if (eval-expression condition env)
	     (eval-expression if-true env)
	     (eval-expression if-false env)))]
       [(letrec-exp) 
	(let ([defs (cadr exp)]
	      [body (caddr exp)])
	  (eval-begin-list body 
			   (extend-env-recur (map car defs) 
					     (map (lambda (x) 
						    (eval-expression x env))
						  (map cadr defs)) 
					     env)))]
       [(set-exp)
	(let ([sym (cadr exp)]
	      [val (caddr exp)])
	  (let ([the-val (eval-expression val env)])
	    (change-env env
			sym
			the-val)))]
       [(begin-exp)
	(let ([body (cadr exp)])
	  (eval-begin-list body env))]
       [(while-exp) 
	(let ([test-exp (cadr exp)]
	      [bodies (caddr exp)]
	      [if-false (cadddr exp)])
	  (while-eval test-exp bodies env))]
       [(define-exp)
	 (let ([sym (cadr exp)]
	       [val (caddr exp)])
	   (if (exists-in-env? env sym)
	       (change-env env sym val)
	       (extend-env sym (eval-expression val env) env)))]
       [(load-exp)
	(let ([filename (cadr exp)])
	  (let ([p (open-input-file filename)])
	    (let f ([x (read p)])
	      (if (eof-object? x)
		  (begin
		    (close-port p)
		    '())
		  (begin (eval-one-exp x) 
			 (f (read p)))))))]
       [else (eopl:error 'eval-expression
			 "incorrect expression type ~s" exp)])))

(define expand-syntax
  (lambda (exp)
    (case (car exp)
      [(let-exp)
       (let ([defs (cadr exp)]
	     [body (caddr exp)])
	 (app-exp (lambda-exp (map car defs)  (map expand-syntax body))
		  (map expand-syntax (map cadr defs))))]
      [(ifelse-exp)
       (let ([conditional (cadr exp)]
	     [if-true (caddr exp)]
	     [if-false (cadddr exp)])
	 (ifelse-exp (expand-syntax conditional)
		     (expand-syntax if-true)
		     (expand-syntax if-false)))]
      [(if-exp) 
       (let ([conditional (cadr exp)]
	     [if-true (caddr exp)])
	 (if-exp (expand-syntax conditional)
		 (expand-syntax if-true)))]
      [(app-exp) 
       (let ([operator (cadr exp)]
	     [operand (caddr exp)])
	 (app-exp (expand-syntax operator) (map expand-syntax operand)))]
      [(lambda-exp) 
       (let ([ids (cadr exp)]
	     [bodies (caddr exp)])
	 (lambda-exp ids (map expand-syntax bodies)))]
      [(cond-exp)
       (let ([conditions (cadr exp)]
	     [bodies (caddr exp)])
	 (if (equal? (car conditions) '(var-exp else))
	     (expand-syntax (car bodies))
	     (if (null? (cdr conditions))
		 (if-exp (expand-syntax (car conditions) )
			 (expand-syntax (car bodies)))
		 (ifelse-exp (expand-syntax (car conditions) )
			     (expand-syntax (car bodies))
			     (expand-syntax (cond-exp (cdr conditions) (cdr bodies)))))))]
      [(and-exp)
       (let ([exps (cadr exp)])
	 (if (null? exps)
	     (lit-exp #t)
	     (if (null? (cdr exps))
		 (expand-syntax (car exps))
		 (ifelse-exp (expand-syntax (car exps))
			     (expand-syntax (and-exp (cdr exps)))
			     (lit-exp #f)))))]
      [(or-exp)
       (let ([exps (cadr exp)])
	 (if (null? exps)
	     (lit-exp #f)
	     (if (null? (cdr exps))
		 (expand-syntax (car exps))
		 (expand-syntax (let-exp (list (list 'res (expand-syntax (car exps))) )
					 (list (ifelse-exp (var-exp 'res)
							   (var-exp 'res)
							   (expand-syntax (or-exp (cdr exps))))))))))]
      [(let*-exp)
       (let ([defs (cadr exp)]
	     [body (caddr exp)])
	 (if (null? defs)
	     (expand-syntax (let-exp '() body))
	     (expand-syntax (let-exp (list (car defs))
				     (list (let*-exp (cdr defs)
						     body))))))]
      [(case-exp)
       (let ([key (cadr exp)]
	     [conditions (caddr exp)]
	     [bodies (cadddr exp)])
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
					    (case-exp key (cdr conditions) (cdr bodies)))))))]
      [(define-exp)
       (let ([id (cadr exp)]
	     [exp (caddr exp)])
	 (define-exp id (expand-syntax exp)))]
      [(letrec-exp)
       (let ([defs (cadr exp)]
	     [exps (caddr exp)])
	 (letrec-exp (map (lambda (def) 
			    (list (car def) 
				  (expand-syntax (cadr def)))) 
			  defs) 
		     (map expand-syntax exps)))]
      [(namedlet-exp)
       (let ([id (cadr exp)]
	     [defs (caddr exp)]
	     [bodies (cadddr exp)])
	 (letrec-exp (list (cons id 
				 (list (lambda-exp (map car defs) (map expand-syntax bodies))))) 
		     (list (app-exp (var-exp id) (map expand-syntax (map cadr defs))))))]
      [else exp])))

(define while-eval
  (lambda (test bodies env)
    (if (eval-expression test env)
	(begin (eval-begin-list bodies env)
	       (while-eval test bodies env)))))

(define eval-expression-list
  (lambda (explist env)
    (cond [(null? explist) '()]
	  [else 
	   (cons (eval-expression (car explist) env)
		 (eval-expression-list (cdr explist) env))])))

(define eval-begin-list
  (lambda (explist env)
    ;(begin (display env) (newline)
    (cond [(null? explist) (void)]
	  [(null? (cdr explist))
	   (eval-expression (car explist) env)]
	  [else
	   (if (and (pair? (car explist)) (eqv? 'define-exp (caar explist))) 
	     (eval-begin-list (cdr explist) (eval-expression (car explist) env))
	     (begin (eval-expression (car explist) env) (eval-begin-list (cdr explist) env)))])));)

(define make-closure
  (lambda (id body env)
    (closure-record id body env)))

#|(define-datatype proc proc?
  [primitive
   (id symbol?)]
  [closure-record
   (id lambda-parameter-list?)
   (body (list-of expression?))
   (env environment?)])|#
   
 (define primitive (lambda (id) (list 'primitive id)))
 (define closure-record
   (lambda (id body env) (list 'closure-record id body env)))

(define apply-proc
  (lambda (item args env)
    (case (car item)
      [(closure-record) 
       (let ([id (cadr item)]
	     [body (caddr item)]
	     [env (cadddr item)])
	 (eval-begin-list body (extend-env id args env)))]
      [(primitive)
       (let ([id (cadr item)])
	 (apply-primitive-proc id args))])))

(define apply-primitive-proc
  (lambda (id args)
    (case id
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(car) (apply car args)]
      [(cdr) (cdar args)]
      [(add1) (apply add1 args)]
      [(map) (map (lambda (x) (apply-proc (car args) (list x) (empty-env))) (cadr args))]
      [(apply) (apply (eval (cadar args)) (cadr args))]
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
      vector-set! caar cadr cdar cddr caaar caadr cadar max
      caddr cdaar cdadr cddar cdddr eqv? set-car! map apply assq assv append))

(define make-init-env
  (lambda ()
    (map (lambda (name)
	   (cons name (primitive name)))
	 *prim-proc-names*)))

(define global-env
  (make-init-env))