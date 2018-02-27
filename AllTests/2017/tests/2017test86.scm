
(define bar7 (lambda (a b c)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b b)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar7 (rac a) (- b 1) c))
                    )
                  )
    )
	
(bar7 5 2 6)