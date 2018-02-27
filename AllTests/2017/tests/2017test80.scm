
(define bar1 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define num b)
                  (cond ((eq? num 0) a)
                    (else
                      (bar1 (rec1 a) (- b 1)))
                    )
                  )
                )
    )
	
(bar1 4 3)