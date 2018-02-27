
(define fun3 (lambda ()
                 (begin
                   (define x (+ 2 1))
                   (set! x (+ x 3 4))
                   x
                   )
                 )
    )
(fun3)