
(define bar3 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (bar3 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar3 6 2)