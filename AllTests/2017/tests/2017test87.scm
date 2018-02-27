
(define bar8 (lambda (a b c d)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r 5) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar8 (rac a) (- b 1) c d))
                    )
                  )
    )
	
(bar8 1 5 2 6)