(load "chez-init.ss")

;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

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
    (let* ([vec (list->vector vals)]
	   [new-env (cons (cons syms vec) env)])
      (for-each (lambda (item pos)
		  (if (proc? item)
		      (vector-set! vec
				   pos
				   (cases proc item
					  [closure (ids bodies toss-env)
						   (closure ids bodies new-env)]
					  [primitive (id)
						     item]))))
		vals
		(make-indices (- (length vals) 1) '()))
      new-env)))

(define make-indices
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(make-indices (- n 1) (cons n accu)))))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (car (car env))]
	      [vals (cdr (car env))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		(vector-ref vals pos)
		(apply-env env sym)))))))
    
(define change-env
  (lambda (env sym val)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
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