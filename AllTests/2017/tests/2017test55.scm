(define f3 (lambda () (begin
                         (define foo (lambda (x) (x 5 6)))
                         (define bar (lambda (a b) (+ a b)))
                         (foo bar)
                         )
               )
    )
(f3)
