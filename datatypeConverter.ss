(define convert
  (lambda (datum)
    (if (not (null? datum))
	(cons (let ([data (car datum)])
		(let ([name (car data)])
		  (list 'define 
			name
			(list 'lambda
			      (map car (cdr data))
			      (append (list 'list
					    (list 'quote
						  name))
				      (map car (cdr data)))))))
	      (convert (cdr datum)))
	'())))