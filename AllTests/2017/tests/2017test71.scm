
(define foo2 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (f))
                            (set! a (f))
                            (set! a (f))
                            a)
                 )
    )
(foo2 50 60)