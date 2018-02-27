
(define foo4 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (define g (lambda () (set! x 5)))
                            a)
                 )
    )
(foo4 31 3)