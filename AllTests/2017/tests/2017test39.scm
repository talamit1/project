
(define (a str)
    (define (b x sum)
      (cond
        ((= (string-length str) x) sum)
        (else (b (+ x 1) (+ (char->integer (string-ref str x)) (* 256 sum))))))
    (b 0 0))
(a "hello")