((lambda (f1 f2 input1 input2 input3 ifval)
          (begin
            (define f (lambda () (f1 (f2 input1 5) 40)))
           (if (ifval input2 input3)
               (f)
               (begin
                 (set! f2 f1)
                 (f)
                 )
               )
           )
        ) * * 1 2 3 =)
