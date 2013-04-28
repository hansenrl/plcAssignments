(load "chez-init.ss")

;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

(define extend-global-env
  (lambda (sym val)
    (set! global-env (cons (cons sym val) global-env))))

(define extend-env
  (lambda (syms vals env)
    (cond [(symbol? syms)
	   (cons (cons (list syms) (list->vector (list vals))) env)]
	  [(list? syms)
	   (cons (cons syms (list->vector vals)) env)]
	  [else
	   (extend-env (cdr syms) (cdr vals) 
		       (cons (cons (list (car syms)) (list->vector (list (car vals)))) env))])))

;;; handles letrecs
(define extend-env-recur
  (lambda (syms vals env)
    (cond [(symbol? syms)
	   (extend-env-recur (list syms) (list vals) env)]
;	  [(not (list? syms))
;	   (extend-env-recur (cdr syms) (cdr vals) (extend-env-recur (car syms) (car vals) env))]
	  [else
	   (let* ([vec (list->vector vals)]
		  [new-env (cons (cons syms vec) env)])
	     (for-each (lambda (item pos)
			 (begin (display "**new-env ") (display new-env) (newline) (if (procedure? item)
			     (vector-set! vec
					  pos
					  (cases procedure item
						 [closure-record (ids bodies toss-env)
							  (closure-record ids bodies new-env)]
						 [primitive (id)
							    item])))))
		       vals
		       (make-indices (- (length vals) 1) '()))
	     new-env)])))

(define make-indices
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(make-indices (- n 1) (cons n accu)))))

(define apply-global-env
  (lambda (sym)
    (let ([result (assv sym global-env)])
      (if (not result)
	  (eopl:error 'apply-global-env "No binding for ~s" sym)
	  (cdr result)))))
#|
(define exists-in-global-env?
  (lambda (sym)
    (assv sym global-env)))
|#
(define exists-in-env?
  (lambda (env sym)
    (if (null? env)
	#f ;(exists-in-global-env? sym)
	(let ([syms (car (car env))]
	      [vals (cdr (car env))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		#t
		(exists-in-env? env sym)))))))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(apply-global-env sym)  ;(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (car (car env))]
	      [vals (cdr (car env))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		(vector-ref vals pos)
		(apply-env env sym)))))))

(define change-global-env
  (lambda (sym val)
    (let ([result (assv sym global-env)])
      (if (not result)
	  (eopl:error 'change-global-env "No existing binding for ~s" sym)
	  (set-cdr! result val)))))
    
    
(define change-env
  (lambda (env sym val)
    (if (null? env)
	(change-global-env sym val) ;(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	  (let ((pos (find-position sym syms)))
	    (if (number? pos)
		(vector-set! vals pos val)
		(change-env env sym val)))))))

(define find-position
  (lambda (sym ls)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) 0]
	  [else (let ([index (find-position sym (cdr ls))])
		  (if (number? index)
		      (+ index 1)
		      #f))])))

(define environment?
  (lambda x
    #t))

(define reset-global-env
             (lambda () (set! global-env (make-init-env))))
