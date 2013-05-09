(define-syntax for
    (syntax-rules (:)
      [(_ (init : test : update) body) 
       (begin init
         (letrec ([f (lambda ()
                       (if test
                           (begin body update (f))))])
           (f)))]))


(define-syntax return-first
    (syntax-rules ()
      [(_ e1 e2 ...) (let ([a e1])
                       (begin e2 ...)
                       a)]))

