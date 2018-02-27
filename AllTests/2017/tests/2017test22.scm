
((lambda (f1 f2 input1 input2 input3)
    (begin
      (define f (lambda () (f1 (f2 input1 input2) input3)))
      (f)
      )
    ) - - 1 2 3)