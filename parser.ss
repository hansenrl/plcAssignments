(define scheme-value?
  (lambda (v)
    #t))

(define var-exp 
  (lambda (id) 
    (list 'var-exp id))) 
(define lit-exp 
  (lambda (val)
    (list 'lit-exp val)))
 (define lambda-exp
   (lambda (parameters body)
     (list 'lambda-exp parameters body)))
 (define app-exp
   (lambda (operator operand)
     (list 'app-exp operator operand)))
 (define if-exp
   (lambda (condition if-true)
     (list 'if-exp condition if-true)))
 (define ifelse-exp
   (lambda (condition if-true if-false)
     (list 'ifelse-exp condition if-true if-false)))
 (define letrec-exp
   (lambda (defs body) (list 'letrec-exp defs body)))
 (define namedletrec-exp
   (lambda (id defs body)
     (list 'namedletrec-exp id defs body)))
 (define namedlet-exp
   (lambda (id defs body) (list 'namedlet-exp id defs body)))
 (define let-exp
   (lambda (defs body) (list 'let-exp defs body)))
 (define namedlet*-exp
   (lambda (id defs body) (list 'namedlet*-exp id defs body)))
 (define let*-exp
   (lambda (defs body) (list 'let*-exp defs body)))
 (define set-exp (lambda (id val) (list 'set-exp id val)))
 (define begin-exp (lambda (body) (list 'begin-exp body)))
 (define cond-exp
   (lambda (conditions bodies)
     (list 'cond-exp conditions bodies)))
 (define and-exp (lambda (exps) (list 'and-exp exps)))
 (define or-exp (lambda (exps) (list 'or-exp exps)))
 (define while-exp
   (lambda (test-exp bodies)
     (list 'while-exp test-exp bodies)))
 (define case-exp
   (lambda (key conditions bodies)
     (list 'case-exp key conditions bodies)))
 (define define-exp
   (lambda (id exp) (list 'define-exp id exp)))
(define load-exp
  (lambda (filename)
    (list 'load-exp filename)))

(define lambda-parameter-list?
  (lambda (ls)
    (or (null? ls)
	(symbol? ls)
	(and (symbol? (car ls))
	     (lambda-parameter-list? (cdr ls))))))

(define definition?
  (lambda (def)
    (and (list? def)
	 (= (length def) 2)
	 (symbol? (car def))
	 (expression? (cadr def)))))

(define distfromtoplevel 0)

(define parse-expression
  (lambda (datum)
    (set! distfromtoplevel (+ 1 distfromtoplevel))
    (let ([temp (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond [(eq? (car datum) 'lambda)
		  (lambda-exp (parse-lambda-parameters (cadr datum))
			      (if (null? (cddr datum))
				  (eopl:error 'parse-expression
					      "lambda expression: missing body: ~s" datum)
				  (parse-explist (cddr datum))))]
		 [(eq? (car datum) 'if)
		  (cond [(< (length datum) 3)
			 (eopl:error 'parse-expression
				     "if expression: too few arguments: ~s" datum)]
			[(null? (cdddr datum))
			 (if-exp (parse-expression (cadr datum))
				 (parse-expression (caddr datum)))]
			[else 
			 (ifelse-exp (parse-expression (cadr datum))
				     (parse-expression (caddr datum))
				     (parse-expression (cadddr datum)))])]
		 [(eq? (car datum) 'let)
		  (if (symbol? (cadr datum))
		      (parse-namedlet namedlet-exp datum)
		      (parse-let let-exp datum))]
		 [(eq? (car datum) 'let*)
		  (if (symbol? (cadr datum))
		      (parse-namedlet namedlet*-exp datum)
		      (parse-let let*-exp datum))]
		 [(eq? (car datum) 'letrec)
		  (if (symbol? (cadr datum))
		      (parse-namedlet namedletrec-exp datum)
		      (parse-let letrec-exp datum))]
		 [(eqv? (car datum) 'set!)
		  (set-exp (cadr datum) (parse-expression (caddr datum)))]
		 [(eq? (car datum) 'vector)
		  (lit-exp (list->vector (cdr datum)))]
		 [(eq? (car datum) 'quote)
		  (lit-exp (cadr datum))]
		 [(eq? (car datum) 'begin)
		  (if (= distfromtoplevel 1)
		      (begin-exp (parse-explistkeepdefine (cdr datum)))
		      (begin-exp (parse-explist (cdr datum))))]
		 [(eq? (car datum) 'cond)
		  (cond-exp (map parse-expression (map car (cdr datum)))
			    (map parse-expression (map cadr (cdr datum))))]
		 [(eq? (car datum) 'and)
		  (and-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'while)
		  (while-exp (parse-expression (cadr datum)) (map parse-expression (cddr datum)))]
		 [(eq? (car datum) 'or )
		  (or-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'case)
		  (case-exp (parse-expression (cadr datum))
			    (map (lambda (x) 
				   (if (pair? x)
				       (map lit-exp x)
				       (list (lit-exp x))))
				 (map car (cddr datum)))
			    (map parse-expression (map cadr (cddr datum))))]
		 [(eq? (car datum) 'define)
		  (define-exp (cadr datum)
		              (parse-expression (caddr datum)))]
		 [(eq? (car datum) 'load)
		  (load-exp (cadr datum))]
		 [else (app-exp (parse-expression (car datum))
				(if (list? (cdr datum))
				    (parse-explist (cdr datum))
				    (display (list 'parse-expression
						   "app-exp: not proper arg list: ~s" datum))))])]
	  [(scheme-value? datum) (lit-exp datum)]
	  [else (display (list 'parse-expression
			       "Invalid concrete syntac ~s" datum))])])
      (set! distfromtoplevel (- distfromtoplevel 1))
      temp)))

(define parse-definition-list
  (lambda (datum)
    (cond [(null? datum) '()]
	  [(pair? datum)
	   (cons (parse-definition (car datum))
		 (parse-definition-list (cdr datum)))]
	  [else
	   (display (list 'parse-expression
			  "Definition list: invalid definition: ~s" datum))])))

(define parse-definition
  (lambda (datum)
    (cond [(and (list? datum)
		(= (length datum) 2)
		(symbol? (car datum)))
	   (list (car datum)
		 (parse-expression (cadr datum)))]
	  [else
	   (display (list 'parse-expression
			  "Definition: invalid definition: ~s" datum))])))

(define parse-lambda-parameters
  (lambda (datum)
    (cond [(null? datum) '()]
	  [(symbol? datum) datum]
	  [(and (pair? datum)
		(symbol? (car datum)))
	   (cons (car datum) (parse-lambda-parameters (cdr datum)))]
	  [else
	   (display (list 'parse-expression
			  "lambda parameter: must be symbol: ~s" datum))])))

(define parse-namedlet
  (lambda (typeofexp datum)
    (typeofexp (cadr datum)
	       (parse-definition-list (caddr datum))
	       (if (null? (cdddr datum))
		   (display (list 'parse-expression
				  "named let expression: empty body: ~s" datum))
		   (parse-explist (cdddr datum))))))

(define parse-let 
  (lambda (typeofexp datum)
    (typeofexp (parse-definition-list (cadr datum))
	       (if (null? (cddr datum))
		   (display (list 'parse-expression
				  "let expression: empty body: ~s" datum))
		   (parse-explist (cddr datum))))))

(define parse-explist
  (lambda (datum)
    (cond [(null? datum)
	   '()]
	  [(and (pair? (car datum))(eq? (caar datum) 'define))
	   (let ([defbodylist (define-body-split datum '())])
	     (list (letrec-exp (car defbodylist) (cadr defbodylist))))]
	  [else
	   (cons (parse-expression (car datum))
		 (parse-explist (cdr datum)))])))

(define parse-explistkeepdefine
  (lambda (datum)
    (cond [(null? datum)
	   '()]
	  [else
	   (cons (parse-expression (car datum))
		 (parse-explistkeepdefine (cdr datum)))])))

(define define-body-split
  (lambda (datum accu)
    (cond [(null? datum)
	   (list accu '())]
	  [(eq? (caar datum) 'define)
	   (define-body-split (cdr datum) 
                              (cons (list (cadar datum)
					  (parse-expression (caddar datum)))
				    accu))]
	  [else
	   (list accu (parse-explist datum))])))
