(load "qq.scm")


(define  (debugPrint x)
  (display x)
  (newline)
)


;;---------------------identifiers---------------

(define nil?
  (lambda (exp)
    (eq? '() exp) 
  )
)

(define var?
  (lambda (v)
    (and (symbol? v) (not (member v *reserved-words*)))  
  )
)


(define ident
  (lambda (sexpr resWord)
  
  (if (pair? expr)
    (if (equal? (car expr) resWord)
      #t
      #f)
     #f)
    )
)
(define conditional?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `if)
        #t
        #f)
      #f)
  
    )
)

(define define? 
  (lambda (exp)
  (if (pair? exp)
  (if (equal? (car exp) `define)
    #t
    #f)
  #f)
  )
  )

(define lambda? 
  (lambda (exp)
    (if (pair? exp)
    (if (equal? (car exp) `lambda)
      #t
      #f)
    #f)
    )
  )

(define begin?
  (lambda (exp)
  (if (pair? exp)
  (if (equal? (car exp) `begin)
    #t
    #f)
  #f)
  )  
)

(define Assignment?
    (lambda (x)
      (if (pair? x)
        (if (equal? (car x) `set!)
          #t
          #f)
        #f)
      )
)


(define Application?
  (lambda (x)
    (if (pair? x)
      (if (notReserved (car x))
        #t
        #f)
      #f)
    )
)

(define disjunction?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `or)
        #t
        #f)
      #f)
    )
)
  
(define and?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `and)
        #t
        #f)
      #f)
    )
)

(define cond?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `cond)
        #t
        #f)
      #f)
    )
)
  
(define let?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `let)
        #t
        #f)
      #f)
    )
)

(define let*?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `let*)
        #t
        #f)
      #f)
    )
)

(define letrec?
  (lambda(x)
    (if (pair? x)
      (if (equal? (car x) `letrec)
        #t
        #f)
      #f)
    )
)

(define quasiquote? (^quote? 'quasiquote))
    
(define *reserved-words*
  '(and begin cond define do else if lambda
  let let* letrec or quasiquote unquote
  unquote-splicing quote set!))

  (define notReserved
    (lambda(e)
    (not (member e *reserved-words*))
    )
  )

;---------------macro expanding----------------------
;and macro
(define andMacro
  (lambda (lst)
    (cond ( (null? (cdr lst))  (parse '#t))
          ( (null? (cddr lst))  (parse (cadr lst)))
          ( (null? (cdddr lst))  `(if3 ,(parse (cadr lst)) ,(parse (caddr lst)) ,(parse '#f)  ))
          (else  `(if3 ,(parse (cadr lst))  ,(parse `(and ,@(cddr lst)))  ,(parse '#f)))    
    )
  )
)

;cond macro
(define condMacro
  (lambda (lst)
    (if (and (equal? 'else (caadr lst)) (null? (cddr lst)))   
      (if (null? (cdadr lst))
      (error 'parser (format "This is not a valid cond-rib: : ~s" (cadr lst)))
      (parse ( addBegin (cdadr lst)))
      )
      (let ( (first (cadr lst))             
             (rest (cddr lst))
           )
           (if ( pair? (cdr first))
            (if (null? rest)
              (parse `(if ,(car first) ,(addBegin (cdr first))))
              (parse `(if ,(car first) ,(addBegin (cdr first)) (cond ,@rest)))
            )
            (error 'parser (format "This is not a valid cond-rib: : ~s" first))
           )        
        )
    )
  )
)

;;check if list not contain duplicate vars (return #f if does else #t)
(define (checkNoDup lst)
  (cond
    ((nil? lst) #t) 
    ((member (car lst) (cdr lst)) #f)
    (else (checkNoDup (cdr lst)))     
  )
)

;let macro
(define parseLet
  (lambda (lst)
    (let ( (vars (map car (cadr lst)))
           (vals (map cadr (cadr lst)))
           (exprs (cddr lst))
         )
       (if  (and  (checkNoDup vars) (not (nil? exprs))  )          
        (parse `((lambda ,vars ,@exprs) ,@vals))
        
        (if (nil? exprs)
        (error 'parser (format "Unknown form: ~s" lst))
        (error 'parser (format "Invalid parameter list: ~s" vars ))
        )
      )
   )   
  )
)

;let* macro
(define parseLet*
  (lambda (lst)
    (if (null? (cadr lst)) (parse `(let () ,@(cddr lst)))

         (let ( (var1 (caaadr lst))   
                (val1 (car (cdaadr lst)))
                (rest (cdadr lst))
                (exprs (cddr lst))
              ) 
           (if (null? rest)   
            (parse `(let ((,var1 ,val1)) ,@exprs))
            (parse `(let ((,var1 ,val1)) (let* ,rest ,@exprs)))
          
           )
        )
    )
  )
)

;letrec macro
(define parseLetrec
 (lambda (lst)
  (let ( (vars (map car (cadr lst)))
         (vals (map (lambda (x) #f) (cadr lst)))
         (exprs (cddr lst))
         (setters (map (lambda (bind1) `(set! ,(car bind1) ,@(cdr bind1))) (cadr lst))) 
       )
       (if  (and  (checkNoDup vars) (not (nil? exprs))  )          
        (parse `((lambda ,vars ,@setters (let () ,@exprs)) ,@vals ))
        
        (if (nil? exprs)
        (error 'parser (format "Unknown form: ~s" lst))
        (error 'parser (format "Invalid parameter list: ~s" vars ))
        )
      )
  ) 
 )
)

;qusiquoted macro
(define parseQq
  (lambda (lst)
    (parse (expand-qq (cadr lst)))
  )
)

;---------------parsers----------------------
;const
(define parseConst
  (lambda (x)
    `(const ,(unquotify x))
))
;var
(define parseVar
  (lambda (v)
    `(var ,v)
  )
)

;if
(define parseCondition
  (lambda (lst)
    (cond
       ((> (length lst) 3)
          `(if3 ,(parse (unquotify (cadr lst))) 
                ,(unquotify (parse (caddr lst))) ,(unquotify (parse (cadddr lst)) )))
        ((< (length lst) 4)  
          `(if3 ,(parse (unquotify (cadr lst))) 
                ,(unquotify (parse (caddr lst))) ,(unquotify (parse (void)) ))
          )
    )
  )
)

;;checks if a given list is an improper list return #t if it is and #f if not
(define (improper-list? pair)
          
            (if (or (equal? pair '()) (not (pair? pair)))
            #f
            (not (null? (cdr (last-pair pair)))))
)
;;get a improper list as a pramater and returns a proper list
(define improper->proper
  (lambda (lst)
  (set-cdr! (last-pair  lst) (cons (cdr (last-pair lst))'()))
  lst 
  )
)

(define addBegin
  (lambda (body)
    
    (let ((lambdExpr  (list 'begin)) )
      (append lambdExpr body)
      )  
  )
)

(define parseLambda
  (lambda (sexpr)
    (let ((argsList (cadr sexpr))  
          (bodyExpressions (cddr sexpr))
          )
      (cond
        ;;lambda with optional arguments 
        ((improper-list? argsList) 
          (let ((opt (cdr (last-pair argsList))))     
                              `(lambda-opt 
                                  ,(reverse (cdr (reverse (improper->proper argsList))))
                                  ,opt
                                  ;;,(parse bodyExpressions)
                                  ,(parse (addBegin bodyExpressions)) 
                                          )
          ) 
        )
        ;;Variadic lambda
        ((var? argsList)  `(lambda-opt 
                                  ()
                                  ,argsList
                                  ;;,(parse bodyExpressions)
                                  ,(parse (addBegin bodyExpressions))
          ;;,(parse `(begin bodyExpressions)) 
                  ) 
        )
        ;;Regular lambda
        ((andmap var? argsList)  `(lambda-simple
                                          ,argsList
                                          ;;  ,@(map parse bodyExpressions)
                                          ,(parse (addBegin bodyExpressions))
                                        )
        )
              
      )    
        
    )
  )  
  
)


;;prepare arguments for lambda expression
(define prepareLambda
  (lambda (args body)
    (let ((lambdExpr  (list 'lambda args)) )
      (map (lambda (x) 
        (set-cdr! (last-pair lambdExpr) (list x))
            ) 
            body)
      
      lambdExpr
      )  
  )
)

;define
(define parseDefine
  (lambda (sexpr)
    (let ((secondArg (cadr sexpr))  (expr (cddr sexpr))   )
      (cond
        ;;regular define
        ((var? secondArg) `(define ,(parse secondArg) ,(parse (addBegin expr))))
        
        ;;MIT-define
        ((pair? secondArg) 
          `(define ,(parse (car secondArg))
                              ,(parse (prepareLambda (cdr secondArg) expr))              
                            )
        
        )
      )
    )
  )
)

(define clear_begin
  (lambda(lst)
		(cond ((null? lst) '()) 
        ((and(pair? (car lst)) (eq? (caar lst) 'begin))
          (let ((restOfLst (cdar lst)) (rest (cdr lst)))
					(append (clear_begin restOfLst) (clear_begin rest))))
			  (else 
          (cons (car lst) (clear_begin (cdr lst)))))
    
			))

;;seq
(define parseSeq
  (lambda (seq)
    (let ((correctSeq (cdr seq)))
      (cond
        ((null? correctSeq ) (parse (void)))
        ((not (pair? correctSeq)) (parse correctSeq))
        ((null? (cdr correctSeq)) (parse (car correctSeq)) )
        (else `(seq ,(map parse (clear_begin correctSeq))))
      )
    )
  )
)

(define parseAssign
  (lambda (assExp)
    (let ((v (cadr assExp)) (val (caddr assExp)) )
      `(set (var ,v) ,(parse val)) 
      )
  )
)

(define parseApplication
  (lambda (sexpr)
    (let ((e1 (car sexpr)) (rest (cdr sexpr))  )
    `(applic ,(parse e1) ,(map parse rest)  )
    
    )

  )  
)


;or
(define parseDisjunction
  (lambda (lst)
   (cond 
      (( = (length lst) 1)   (parse '#f))
      (( = (length lst) 2)   (parse (cadr lst)))
      (( > (length lst) 2)   `(or ,(map parse (cdr lst))))
   )
  )
)


(define parse
  (lambda (sexpr)
    (cond 
      ((const? sexpr)  (parseConst sexpr))
      ((var? sexpr)     (parseVar sexpr))
      ((conditional? sexpr) (parseCondition sexpr))
      ((disjunction? sexpr) (parseDisjunction sexpr))
      ((and? sexpr) (andMacro sexpr))
      ((cond? sexpr) (condMacro sexpr))
      ((lambda? sexpr) (parseLambda sexpr))
      ((begin? sexpr) (parseSeq sexpr))
      ((define? sexpr) (parseDefine sexpr))
      ((Assignment? sexpr) (parseAssign sexpr))
      ((Application? sexpr) (parseApplication sexpr))
      ((let? sexpr) (parseLet sexpr))
      ((let*? sexpr) (parseLet* sexpr))
      ((letrec? sexpr) (parseLetrec sexpr))   
      ((quasiquote? sexpr) (parseQq sexpr))
      (eq? sexpr (void) (parseConst sexpr))
    ) 
  )
)