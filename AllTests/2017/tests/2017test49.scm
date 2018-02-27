
(define loop5 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (cond ((= i num) param)
                                        (else
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop5 21 (lambda (x) (* 3 x)) 123)