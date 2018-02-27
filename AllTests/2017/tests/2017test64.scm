
(define fun5 (lambda ()
                 (begin
                   (define f (lambda () (+ 2 1)))
                   (define g (lambda () (+ (f) 3 4)))
                   g
                   )
                 )
    )
((fun5))