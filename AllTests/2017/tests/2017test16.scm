(((lambda (x)
      (begin
        (define func (lambda (y)
                       (x y 5)
                       )
          )
        func)
      ) +) 65)
