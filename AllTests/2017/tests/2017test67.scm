
(define fun8 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (+ (g f) (g *) (g -) (g +))
                   )
                 )
    )
(fun8)