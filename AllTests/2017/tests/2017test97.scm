
(define tar2 (lambda (a)
                (begin
                  (define r a)
                  (cond ((= r 1) 1)
                   (else (* 2 (tar2 (- r 1))))))))
				  
(tar2 5)