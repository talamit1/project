(define f5 (lambda () (begin
                           (define foo (lambda (x y) (x y 5 6)))
                           (define bar (lambda (op a b) (op a b)))
                           (define oop +)
                           (foo bar oop)
                           )
                 )
      )
(f5)
