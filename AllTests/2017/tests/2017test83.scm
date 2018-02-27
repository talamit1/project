
(define bar4 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (define rec2 (lambda (b) (- b 1)))
                  (set! b (rec2 b))
                  (cond ((eq? b 0) a)
                    (else
                      (rec1 (bar4 a b)))
                    )
                  )
                )
    )
	
(bar4 5 2)