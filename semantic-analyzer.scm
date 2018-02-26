(define isRedundantApplications
  (lambda  (exp)
    (let ((applic (car exp))
          (proc   (lambda () (caadr exp)))
          (procArg (lambda () (cadadr exp)))
          (givenArg (lambda () (caddr exp)))
          )

          (and (equal? applic 'applic) 
               (equal? (proc) 'lambda-simple) 
               (null? (procArg))
               (null? (givenArg) )
          )    
    )
  )
)


(define debugPrint
  (lambda (content)
  (display content)
  (newline)
  )
)

(define remove-applic-lambda-nil
  (lambda (parsedExp) 
      (cond 
        ;;in case its an empty list
        ((null? parsedExp)  '())
        ;;in case its a more complicated expression  
        ((list? (car parsedExp)) 
          (cons
          (remove-applic-lambda-nil (car parsedExp))
          (remove-applic-lambda-nil (cdr parsedExp))
          ))
        ;;in case it is an redundant Application
        ((isRedundantApplications parsedExp)  
          (let ((expr (car (cddadr parsedExp))))
            (remove-applic-lambda-nil expr)
          )
        )
        (else
          (cons (car parsedExp) (remove-applic-lambda-nil (cdr parsedExp)) )
          )
      )
  )
)

(define test
  (lambda ()
  (remove-applic-lambda-nil
    '(applic
    (lambda-simple
    (fact)
    (seq ((set (var fact) (box (var fact)))
    (box-set
    (var fact)
    (lambda-simple
    (n)
    (if3 (applic (var zero?) ((var n)))
    (const 1)
    (applic
    (var *)
    ((var n)
    (applic
    (box-get (var fact))
    ((applic (var -) ((var n) (const 1))))))))))
    (applic
    (lambda-simple () (applic (box-get (var fact)) ((const 5))))
    ()))))
    ((const #f))))
    
    )
  )

  
(define checkForDuplicateParam
    (lambda (parameterList var)
        
        (and (member var parameterList) #t)
  )
)

;;return lambda type as a string if it is a lambda
;; return #f if it is not a lambda
(define getLambdaType
  (lambda (exp)
    (if (pair? exp)
      (if (or (equal? (car exp) 'lambda-simple)(equal? (car exp) 'lambda-opt))
          (car exp)  ;;it is a lambda expression return lambda type
          #f  ;;return false if it is not a lambda exp
      )
      #f
    )
  )
)
;;recive lambda type and the expression and return its parameterlist
;;(any*symbol=>list) 
(define getLambdaVars
  (lambda (lambdaType expr)
    
        (if (equal? lambdaType 'lambda-simple) 
        (cadr expr)
        (if (null? (cadr expr))
            (list (caddr expr))
            `(,@(cadr expr) ,(caddr expr))
        )
    )
  )
)



(define getLambdaBody
  (lambda (lambdaType expr)
  
    (if (equal? lambdaType 'lambda-simple) 
      (caddr expr)
      (cadddr expr)
    )
  )
)









(define isSetVar?
  (lambda (exp var)
  

    (cond
      ;;vase we finish search
      ((null? exp) #f)
      ;;this is a symbol and not a set exp
      ((symbol? exp) #f)
      ;;case first exp is a list
      ((list? (car exp)) 
        (or (isSetVar? (car exp) var) (isSetVar? (cdr exp) var)))
      ;;set of null
      
      ((and (equal? (car exp) 'set) (null? (cdr exp)) ) #f)
      ;;true case
      ((and (equal? (car exp) 'set) (equal? (cadadr exp) var ))   #t)
      ;;nested lambda expression and it contain a parameter with same name
      ((getLambdaType  exp)
        (let* 
            ((lambdaType (getLambdaType exp))
            (lambdaBody (getLambdaBody lambdaType exp))
            (lambdaVars (getLambdaVars lambdaType exp)))
            (if(checkForDuplicateParam lambdaVars var)
                #f
                (isSetVar? lambdaBody var)
            )
          )
      )
      (else
        (isSetVar? (cdr exp) var))
    )
  )  
)

(define isReadVar?
  (lambda (exp var)
    (cond
      ;;vase we finish search
      ((null? exp) #f)
      ;;this is a symbol and not a set exp
      ((symbol? exp) #f)
      ;;True Case
      ((equal? var (car exp)) #t)
      ;;it s more complicated expressiom
      ((list? (car exp))  
        (or (isReadVar? (car exp) var) (isReadVar? (cdr exp) var )))
      ;;case the st is null
      
      ((equal?  'set (car exp))
        (if (null? (cdr exp))
            #f
            (isReadVar? (caddr exp) var)
        )
      )
      (else
        (isReadVar? (cdr exp) var))
    
    )
  )  
)

(define isBoundVar?
  (lambda (exp var inLambda)
  
  ;;(debugPrint exp)
  (cond
    ;;vase we finish search
    ((null? exp) #f)

    ;;this is a symbol and not a set exp
    ((symbol? exp) #f)

    ((and (equal? (car exp) 'var) inLambda) (equal? (cadr exp) var ))

    ((list? (car exp)) 
      (or (isBoundVar? (car exp) var inLambda) (isBoundVar? (cdr exp) var inLambda) ))
    
      ((getLambdaType  exp)
        (let* 
          ((lambdaType (getLambdaType exp))
          (lambdaVars (getLambdaVars lambdaType exp))
          (lambdaBody (getLambdaBody lambdaType exp)))
          (if (checkForDuplicateParam lambdaVars var)
              #f
              (isBoundVar? lambdaBody var #t)
          )
        )
     )
     ((equal? (car exp) 'set)
          (if (null? (cdr exp))
            #f
            (or (isBoundVar? (cadr exp) var inLambda) 
              (isBoundVar? (cddr exp) var inLambda))
          )
     )
     (else
      (isBoundVar? (cdr exp) var  inLambda)
      )
  
    )
  )  
)

(define needBoxing?
  (lambda (body var)
  
  (and (isSetVar? body var)
        
       (isReadVar? body var) 
        
      (isBoundVar? body var #f))
  )

)




(define boxVarLambdaBody
  (lambda (body)
    (lambda (var)
      
      (if (needBoxing? body var)
          var
         `() 
      )    
    )
  )
  
)




(define changeVarsInBody
  (lambda (exp var)
  

    (cond
      ((null? exp) '())

      ;;case when we found set exression
      ((and 
        (list?  exp)
        
        (equal? (car exp) 'set)
        
        (equal? (cadr exp) `(var ,var)) )
          
        `(box-set ,(cadr exp) ,@(changeVarsInBody (cddr exp) var)))
      ((getLambdaType (car exp))
      
        (let* 
            ((lambdaType (getLambdaType (car exp)))
            (lambdaVars (getLambdaVars lambdaType (car exp)))
            (lambdaBody (getLambdaBody lambdaType (car exp))))

            
            (if (checkForDuplicateParam lambdaVars var)
                exp
                (if (equal? lambdaType 'lambda-simple) 
                  (cons (cons lambdaType (cons lambdaVars (changeVarsInBody (list lambdaBody) var)))
                 (changeVarsInBody (cdr exp) var))
                  (if (null? (cadar exp))
                  
                      (cons 
                        (cons lambdaType 
                          (cons '() 
                            (cons (caddar exp) (changeVarsInBody (list (cadddr (car exp))) var))))
                      (changeVarsInBody (cdr exp) var))

                      (cons 
                        (cons lambdaType 
                          (cons (cadr (car exp)) 
                            (cons (caddr (car exp)) (changeVarsInBody (list (cadddr (car exp))) var))))
                      (changeVarsInBody (cdr exp) var))               
                            
                  )
                )
           )
        ))
       ((and (list? exp) 
              
              (equal? (car exp) `(var ,var)))
              
              (cons `(box-get ,(car exp)) (changeVarsInBody (cdr exp) var)))

      ((list? (car exp))
      
      (cons (changeVarsInBody (car exp) var) (changeVarsInBody (cdr exp) var)))
      (else
        
        (cons (car exp) (changeVarsInBody (cdr exp) var))
        )
    )  
  )
)


(define putVarInBox
  (lambda (expr var)
   (if (equal? 'seq (car expr))
   `(seq ((set (var ,var) (box (var ,var))) ,@(changeVarsInBody (cadr expr) var)))
   `(seq ((set (var ,var) (box (var ,var))) ,@(list (changeVarsInBody  expr var)))))
  )
)


(define boxVars 
  (lambda (lambdaBody lambdaVars)
    (if (null? lambdaVars)
          lambdaBody
    (boxVars (putVarInBox lambdaBody (car lambdaVars)) (cdr lambdaVars))
          
          
          
      )
    )
)
  



(define boxSetLambda
  (lambda (lambdaType expr)
    (let* 
        ((lambdaVars (getLambdaVars lambdaType expr))
        (lambdaBody (getLambdaBody lambdaType expr))
        (boxVarProc (boxVarLambdaBody lambdaBody))
        (boxedParams (filter (lambda (x) (not (equal? x '() ))) (map boxVarProc (reverse lambdaVars))))
        (boxedBody  (boxVars lambdaBody  boxedParams)))
      
        (if (equal? lambdaType 'lambda-simple)
        `(,lambdaType ,lambdaVars ,(box-set boxedBody)) ;;simple
          (if (null? (cadr expr))
            `(,lambdaType () ,@lambdaVars ,(box-set boxedBody)) ;;variadic
            `(,lambdaType ,(cadr expr) ,(caddr expr) ,(box-set boxedBody)) ;;optional
          )
        )

    )

  )
)

(define box-set
  (lambda (parsedExpr)
    
    (cond 
      ((null? parsedExpr) '())
      
      ((list? (car parsedExpr)) (cons (box-set (car parsedExpr))
                                      (box-set (cdr parsedExpr))))

      ((getLambdaType parsedExpr)
        (let
          ((lambdaType (getLambdaType parsedExpr)))
            (if lambdaType
                (boxSetLambda lambdaType parsedExpr)
                (cons (car parsedExpr) (box-set (cdr parsedExpr)))
            )
          )
        )
        (else
          (cons (car parsedExpr) (box-set (cdr parsedExpr)))
          )
    )
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;; pe->lex-pe;;;;;;;;;;;;;;;;;;;;;;

(define getListOfIndexes
  (lambda (params) 
    (if (null? params) '()
        (append (getListOfIndexes (cdr params))  (list (- (length params) 1)))
    )
  )
)

;;make list of (parameter , index)
(define updt-params-lst
  (lambda (listVars)
    (map (lambda (v i) (list v i)) listVars (getListOfIndexes listVars))
  )
)

;;clear vars from scope that also in params list
(define clearDup
  (lambda (scope params)
    (if (or (null? scope) (null? params))
        scope
       (clearDup (filter (lambda(x) (not (equal? (car x) (caar params)))) scope) (cdr params))
    )
  )
)


(define increaseBound
  (lambda (scope)
  (map (lambda(x) `( ,(car x) ,(+ 1 (cadr x)) ,@(cddr x))) scope)
  )
)

(define addMajorField
  (lambda (params)
    (if (null? params) '()
        (map (lambda(x) (list (car x) '-1 (cadr x))) params)
    
    )
  )
)


;;update the scope list
(define updt-scope-lst
  (lambda (scope params)
    (let* ( (scopeClearDup (clearDup scope params))
            (nScope (increaseBound scope))
            (nParams (addMajorField params))
          )
          (append nParams nScope)
    )
  )
)

;;check if var is in parameter list
(define inParams?
	(lambda (var params)
		(let ((ret (filter (lambda (x) (equal? (car x) var)) params)))
			(if (null? ret)
				#f
        (cdar ret)
      )
    )
  )
)

;;check if var is in scope list          
(define inScope?
	(lambda (var scope)
		(let ((ret (filter (lambda (x) (equal? (car x) var)) scope)))
			(if (null? ret)
				#f
        (cdar ret)
      )
    )
  )
)


(define auxiliary-pe->lex-pe
  (lambda (ast scope params)
    (cond 
      ((null? ast) '())
     
      ;;case var
      ((equal? (car ast) 'var)
        (let (  (parameterIndex (inParams? (cadr ast) params))
                (boundIndex (inScope? (cadr ast) scope))
             )
             (if parameterIndex
                  (append `(pvar ,(cadr ast) ,(car parameterIndex) ) (auxiliary-pe->lex-pe (cddr ast) scope params))
                  (if boundIndex
                  (append `(bvar ,(cadr ast) ,(car boundIndex) ,(cadr boundIndex)) (auxiliary-pe->lex-pe (cddr ast) scope params)) 
                    (append `(fvar ,(cadr ast)) (auxiliary-pe->lex-pe (cddr ast) scope params))
                  )
            )
        )   
      )      

      ((eq? (car ast) 'lambda-simple) 
        (let* ( (lamVrs (getLambdaVars 'lambda-simple ast)) ;;lambda vars
                (lamBody (getLambdaBody 'lambda-simple ast));;lambda body
                (newParams (updt-params-lst lamVrs));;new params list
                (newScope (updt-scope-lst scope newParams));;new scope list
              )
              `(lambda-simple ,lamVrs ,(auxiliary-pe->lex-pe lamBody newScope newParams))
        )
      )     
     
      ;;opt & variadic case
      ((eq? (car ast) 'lambda-opt)
        (let* ( (lamVrs (getLambdaVars 'lambda-opt ast));;lambda vars
                (lamBody (getLambdaBody 'lambda-opt ast));;lambda body
                (newParams (updt-params-lst lamVrs));;new params list
                (newScope (updt-scope-lst scope newParams));;new scope list
              )
              (if (null? (cadr ast))
              `(lambda-opt  () ,@lamVrs ,(auxiliary-pe->lex-pe lamBody newScope newParams))  ;;variadic
              `(lambda-opt ,(cadr ast) ,(caddr ast)  ,(auxiliary-pe->lex-pe lamBody newScope newParams)) ;;optional
              )
        ) 
      ) 
              
      ;;recursive auxiliary-pe->lex-pe on car and cdr       
      ((list? (car ast))
         (cons (auxiliary-pe->lex-pe (car ast) scope params) (auxiliary-pe->lex-pe (cdr ast) scope params))
      )  

      ;;(car ast) is apply or somthing like that
      (else 
        (cons (car ast) (auxiliary-pe->lex-pe (cdr ast) scope params))
      )   

    )
  )
)

(define pe->lex-pe
  (lambda (parsedExp)
    (auxiliary-pe->lex-pe parsedExp '() '())
  )
)



(define varOrConst
  (lambda (x)  
   (or (equal? x 'var)
       (equal? x 'bvar)
       (equal? x 'fvar)
       (equal? x 'const)    
    )
  )
)

(define defineOrSet
  (lambda (exp)
   
    (or (equal?  exp 'define) (equal? exp 'set!))
  
  )  
)

  (define getTestIfExp (lambda (ifExpr) (cadr ifExpr)))
  (define getThenIfExp (lambda (ifExpr) (caddr ifExpr)))
  (define getAltIfExp (lambda (ifExpr) (cadddr ifExpr)))

  (define annotateHelperFalse
    (lambda (exp)
      (annotate-helper exp #f)
    )
)

(define annotate-helper
  (lambda (pe isTailposition?)   
    (cond
      ((null? pe) '())
      ((list? (car pe))
        
        (cons (annotate-helper (car pe) #f) (annotate-helper (cdr pe) #f)))  
      ((varOrConst (car pe))  pe)
      ((equal? (car pe) 'applic)
          (if isTailposition? 
            `(tc-applic ,@(map annotateHelperFalse (cdr pe)))
             `(,(car pe) ,@(map annotateHelperFalse (cdr pe)) )
          ))
      ((or (equal? (car pe) 'or) (equal? (car pe) 'seq) )
          (let* 
            ((reverseElements (reverse (cadr pe))) 
                  (lastElemnt (car reverseElements))
                  (reverseListWithoutLats    (cdr reverseElements))
                  (applyAnnotation  (map annotateHelperFalse reverseListWithoutLats))
                  (annotatedLast   (annotate-helper lastElemnt isTailposition?))
                  (afterAnnotation  (reverse (cons annotatedLast applyAnnotation))))
            
            `(,(car pe)   ,afterAnnotation)
            )
      )

      ((equal? (car pe) 'if3)
        (let ((test (getTestIfExp pe))
              (then (getThenIfExp pe))
              (alternate (getAltIfExp pe)))

              `(if3 ,(annotate-helper test #f)
                    ,(annotate-helper then isTailposition?)
                    ,(annotate-helper alternate isTailposition?)
              )
          )
      )
      ((getLambdaType pe)
            (let*
              ((lambdaType (getLambdaType pe))
              (lambdaVars (getLambdaVars lambdaType pe))
              (lambdaBody (getLambdaBody lambdaType pe)))
              
              (if (equal? lambdaType 'lambda-simple)
              `(,lambdaType ,lambdaVars ,(annotate-helper lambdaBody #t)) ;;simple
                (if (null? (cadr pe))
                  `(,lambdaType () ,@lambdaVars ,(annotate-helper lambdaBody #t)) ;;variadic
                  `(,lambdaType ,(cadr pe) ,(caddr pe) ,(annotate-helper lambdaBody #t)) ;;optional
                )
              )
            ))

        ((defineOrSet (car pe))
          `(,(car pe) ,(cadr pe) ,(annotateHelperFalse (caddr pe)) )
          )
      (else 
        (cons (car pe) (annotateHelperFalse (cdr pe) ))
        )

    )  
  )
  
)

(define annotate-tc
  (lambda  (pe)
    (annotate-helper pe #f)
  )
)


