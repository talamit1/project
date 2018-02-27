
(define fun7 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ 2 1)))
                   (define g (lambda () (f 3 4)))
                   (g)
                   )
                 )
    )
(fun7)