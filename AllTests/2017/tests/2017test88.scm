
(define bar9 (lambda (a b c d e)
                  (begin
                    (define r c)
                    (define rac1 (lambda (b) (* b d)))
                    (define rac2 (lambda (b) (/ 1000 b)))
                    (define rac (if (> r e) rac1 rac2))
                    (define num b)
                    (if (= 0 num)
                        a
                        (bar9 (rac a) (- b 1) c d e))
                    )
                  )
    )
(bar9 2 7 3 3 10)