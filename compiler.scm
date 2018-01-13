


(define pipeline
  (lambda (s)
    ((star <sexpr>) s
      (lambda (m r)
        (map (lambda (e)
                (annotate-tc
                  (pe->lex-pe
                    (box-set
                      (remove-applic-lambda-nil
                        (parse e))))))
        m))
       (lambda (f) 'fail))))


(define file->list
    (lambda (in-file)
        (let ((in-port (open-input-file in-file)))
            (letrec ((run
                (lambda ()
                    (let ((ch (read-char in-port)))
                        (if (eof-object? ch)
                            (begin
                                (close-input-port in-port)
                                '())
                        (cons ch (run)))))))
            (run)))))



(define compile-scheme-file
    (lambda (scheme-file nasm-file) 
        (let* 
            ((stringExp (file->list scheme-file))
             (afterPipeline (pipeline stringExp)))

             afterPipeline
            )
        
    )
)