
(define foo6 (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda (b) (+ b a x y)))
                            (define g (lambda () (set! x 5)))
                            (define t (lambda () (set! a y)))
                            (g)
                            (t)
                            (f x))
                 )
    )
(foo6 101 3)