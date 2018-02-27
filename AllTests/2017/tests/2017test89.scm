
(define bar10 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar10 (rac (rac1 (rac2 a))) (- b 1) c d e))
                    )
                  )
    )
(bar10 1 5 4 6 1)