(load "env.ss")
(load "interpreter.ss")
(load "parser.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([in (read)])
      (if (not (equal? in '(exit)))
	  (begin (write (convertstuff (eval-one-exp in)))
		 (newline)
		 (rep))))))
     
(define convertstuff
  (lambda (stuff)
    (if (pair? stuff)
	(if (eq? (car stuff) 'closure-record)
	    '<interpreter-procedure>
	    (cons (convertstuff (car stuff))
		  (converstuff (cdr stuff))))
	stuff)))