((lambda (x)
      (begin
        (define f1 (lambda (a) (+ a a)))
        (define f2 (lambda (a) (* a a)))
        (if (eq? (f1 x) (f2 x))
            'eq!'
            'no!
            )
        )
      ) 2)
