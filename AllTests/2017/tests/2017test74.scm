
(define foo5 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            (g)
                            (f x))
                 )
    )
(foo5 11 4)