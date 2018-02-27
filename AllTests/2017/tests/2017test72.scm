
(define foo3 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b x y)))
                            (set! a (f a))
                            (set! a (f a))
                            (set! a (f a))
                            a)
                 )
    )
(foo3 43 3)