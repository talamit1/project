
(define tar1 (lambda (a)
                (begin
                  (define r a)
                  (if (= r 1) 1 (+ 1 (tar1 (- r 1)))))))