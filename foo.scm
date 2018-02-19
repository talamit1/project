; ((lambda (x) (begin (set! x 5) x)) 3)
; ;((lambda (x . y) y) 1 2 3 4 5 6)
; (define a 8)
; a
; (set! a 55)
; a
((lambda (x . y) y ) 1 2 3 4 5)