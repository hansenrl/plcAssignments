;;; Erik Sanders
;;; Ross Hansen

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
;              ::= (let ({<variable> <expression>}*) {<expression>}+)
;              ::= (let* ({<variable> <expression>}*) {<expression>}+)
;              ::= (cond {(<expression> <expression>)}+)
;              ::= (case <expression> {[<constant> {<expression>}+]}+) |
;                  (case <expression> {[({<constant>}+) {<expression>}+]}+)
;              ::= (and {<expression>}*)
;              ::= (or {<expression>}*)  
;              ::= (while <expression> {<expression>}+)
;<constant>    ::= <boolean> | <number> | <character> | <string> | <vector> 
;<formals>     ::= <variable>  
;              ::= (<variable>*)  
;              ::= (<variable> <variable>* . <variable>)  
;<application> ::= (<expression> <expression>*)  

(load "env.ss")
(load "interpreter.ss")
(load "parser.ss")
(load "syntax-defines.ss")

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
