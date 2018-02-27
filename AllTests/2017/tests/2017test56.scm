(define f4 (lambda (z) (begin
                         (define foo (lambda (x y) (x y 5 6)))
                         (define bar (lambda (op a b) (op a b)))
                         (foo bar z)
                         )
               )
    )
(f4 *)
