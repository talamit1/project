
(define bar6 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (rac b)
                    )
                  )
    )
	
(bar6 1 2 3)