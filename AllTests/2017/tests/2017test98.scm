
(define bin2dec (lambda (x y)
                    (begin
                      (define rem (remainder x 10))
                      (set! y (+ (* y 2) (* y rem)))
                      (if (= x 0)
                          y
                          (bin2dec (remainder x 10) y)
                          )
                      )
                    )
    )
(bin2dec 1000 2)