
(define loop4 (lambda (num func param)
                  (begin
                    (define i 0)
                    (define subloop (lambda ()
                                      (if (= i num)
                                          param
                                          (begin
                                            (set! i (+ i 1))
                                            (set! param (func param))
                                            (subloop)
                                            )
                                          )
                                      )
                      )
                    )
                  (subloop)
                  )
    )
(loop4 7 (lambda (x) (+ 4 x)) 1213)