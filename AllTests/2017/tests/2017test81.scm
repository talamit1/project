
(define bar2 (lambda (a b)
                (begin
                  (define rec1 (lambda (b) (* b b)))
                  (set! b (- b 1))
                  (cond ((eq? b 0) a)
                    (else
                      (bar2 (rec1 a) b))
                    )
                  )
                )
    )
	
(bar2 4 5)