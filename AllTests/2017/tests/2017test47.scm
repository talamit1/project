
(define loop3 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (func param)
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop3 7 (lambda (x) (+ 8 x)) 123)