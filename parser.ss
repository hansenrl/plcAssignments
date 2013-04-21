(load "chez-init.ss")

(define scheme-value?
  (lambda (v)
    #t))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lit-exp
   (val scheme-value?))
  (lambda-exp
   (parameters lambda-parameter-list?)
   (body (list-of expression?)))
  (app-exp
   (operator expression?)
   (operand (list-of expression?)))
  (if-exp
   (condition expression?)
   (if-true expression?))
  (ifelse-exp
   (condition expression?)
   (if-true expression?)
   (if-false expression?))
  (letrec-exp
   (defs definition-list?)
   (body expression-list?))
  (namedletrec-exp
   (id symbol?)
   (defs definition-list?)
   (body expression-list?))
  (namedlet-exp
   (id symbol?)
   (defs definition-list?)
   (body expression-list?))
  (let-exp
   (defs definition-list?)
   (body expression-list?))
  (namedlet*-exp
   (id symbol?)
   (defs definition-list?)
   (body expression-list?))
  (let*-exp
   (defs definition-list?)
   (body expression-list?))
  (set-exp
   (body definition?))
  (begin-exp
   (body (list-of expression?)))
)

(define lambda-parameter-list?
  (lambda (ls)
    (or (null? ls)
	(symbol? ls)
	(and (symbol? (car ls))
	     (lambda-parameter-list? (cdr ls))))))

(define-datatype symbol-list symbol-list?
  (null-symblist
   (symb null?))
  (base-symblist
   (symb symbol?))
  (mult-symblist
   (symb symbol?)
   (symblist symbol-list?))
)

(define-datatype definition definition?
  (def
   (id symbol?)
   (exp expression?))
)

(define-datatype definition-list definition-list?
  (null-deflist
   (def null?))
  (mult-deflist
   (def definition?)
   (deflist definition-list?))
)

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
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
		 [(eq? (car datum) 'set!)
		  (set-exp (parse-definition (cdr datum)))]
		 [(eq? (car datum) 'vector)
		  (lit-exp (list->vector (cdr datum)))]
		 [(eq? (car datum) 'quote)
		  (lit-exp (cadr datum))]
		 [(eq? (car datum) 'begin)
		  (begin-exp (parse-explist (cdr datum)))]
		 [else (app-exp (parse-expression (car datum))
				(if (list? (cdr datum))
				    (parse-explist (cdr datum))
				    (eopl:error 'parse-expression
						"app-exp: not proper arg list: ~s" datum)))])]
	  [(scheme-value? datum) (lit-exp datum)]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntac ~s" datum)])))

(define parse-definition-list
  (lambda (datum)
    (cond [(null? datum)
	   (null-deflist datum)]
	  [(pair? datum)
	   (mult-deflist (parse-definition (car datum))
			 (parse-definition-list (cdr datum)))]
	  [else
	   (eopl:error 'parse-expression
		       "Definition list: invalid definition: ~s" datum)])))

(define parse-definition
  (lambda (datum)
    (cond [(and (list? datum)
		(= (length datum) 2)
		(symbol? (car datum)))
	   (def (car datum)
		(parse-expression (cadr datum)))]
	  [else
	   (eopl:error 'parse-expression
		       "Definition: invalid definition: ~s" datum)])))

(define parse-lambda-parameters
  (lambda (datum)
    (cond [(null? datum) '()]
	  [(symbol? datum) datum]
	  [(and (pair? datum)
		(symbol? (car datum)))
	   (cons (car datum) (parse-lambda-parameters (cdr datum)))]
	  [else
	   (eopl:error 'parse-expression
		       "lambda parameter: must be symbol: ~s" datum)])))

(define parse-symbol-list
  (lambda (datum)
    (cond [(null? datum)
	   (null-symblist datum)]
	  [(atom? datum)
	   (if (symbol? datum)
	       (base-symblist datum)
	       (eopl:error 'parse-expression
			    "lambda parameter: must be symbol: ~s" datum))]
	  [else
	   (if (symbol? (car datum))
	       (mult-symblist (car datum)
			      (parse-symbol-list (cdr datum)))
	       (eopl:error 'parse-expression
			    "lambda parameter: must be symbol: ~s" datum))])))

(define parse-namedlet
  (lambda (typeofexp datum)
    (typeofexp (cadr datum)
	       (parse-definition-list (caddr datum))
	       (if (null? (cdddr datum))
		   (eopl:error 'parse-expression
			       "named let expression: empty body: ~s" datum)
		   (parse-listofexpressions (cddr datum))))))

(define parse-let 
  (lambda (typeofexp datum)
    (typeofexp (parse-definition-list (cadr datum))
	       (if (null? (cddr datum))
		   (eopl:error 'parse-expression
			       "let expression: empty body: ~s" datum)
		   (parse-listofexpressions (cddr datum))))))

(define parse-explist
  (lambda (datum)
    (cond [(null? datum)
	   '()]
	  [else
	   (cons (parse-expression (car datum))
		 (parse-explist (cdr datum)))])))


(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (val) val]
	   [lambda-exp (parameters body)
		       (cons 'lambda
			     (cons (unparse-symbol-list parameters)
				   (unparse-listofexpressions body)))]
	   [app-exp (operator operand)
		    (cons (unparse-expression operator)
			  (unparse-listofexpressions operand))]
	   [if-exp (condition if-true)
		   (list 'if
			 (unparse-expression condition)
			 (unparse-expression if-true))]
	   [ifelse-exp (condition if-true if-false)
		       (list 'if
			     (unparse-expression condition)
			     (unparse-expression if-true)
			     (unparse-expression if-false))]
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
			  (unparse-definition body))])))

(define unparse-symbol-list
  (lambda (symblist)
    (cases symbol-list symblist
	   [null-symblist (symb) symb]
	   [base-symblist (symb) symb]
	   [mult-symblist (symb symblist)
			  (cons symb
				(unparse-symbol-list symblist))])))

(define unparse-definition-list
  (lambda (deflist)
    (cases definition-list deflist
	   [null-deflist (def) def]
	   [mult-deflist (def deflist)
			 (cons (unparse-definition def)
			       (unparse-definition-list deflist))])))

(define unparse-definition
  (lambda (datum)
    (cases definition datum
	   [def (id exp)
		(list id
		      (unparse-expression exp))])))

(define unparse-let
  (lambda (typeoflet defs body)
    (cons typeoflet
	  (cons (unparse-definition-list defs)
		(unparse-listofexpressions body)))))

(define unparse-namedlet
  (lambda (typeoflet id defs body)
    (cons typeoflet
	  (cons id
		(cons (unparse-definition-list defs)
		      (unparse-listofexpressions body))))))