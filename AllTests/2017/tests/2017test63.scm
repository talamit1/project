
(define fun4 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define x (+ (f) 3 4))
                   x
                   )
                 )
    )
(fun4)