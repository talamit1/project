
(define fool (lambda (x y) (
                            begin
                            (define a 0)
                            (define f (lambda () (+ a x y)))
                            (set! a (+ (f) (f) (f)))
                            a)
                 )
    )
(fool 2 3)