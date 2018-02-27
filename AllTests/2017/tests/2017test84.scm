
(define bar5 (lambda (a b)
                  (begin
                    (define rec1 (lambda (b) (* b b)))
                    (define rec2 (lambda (b) (- b 1)))
                    (set! b (rec2 b))
                    (if (eq? b 0) a
                        (rec1 (bar5 a b)))

                    )
                  )
    )
	
(bar5 5 3)