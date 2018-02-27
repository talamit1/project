
(define fun9 (lambda ()
                 (begin
                   (define f (lambda (a b) (+ a b)))
                   (define g (lambda (f) (f 3 4)))
                   (define t (lambda (a) (
                                         if (eq? a *)
                                             *
                                             a)))
                   (+ (g f) (g (t *)) (g -) (g (t -)))
                   )
                 )
    )
(fun9)