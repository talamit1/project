(load "semantic-analyzer.scm");
(load "tag-parser.scm");
(load "sexpr-parser.scm");


;;---------------------------------code-genCases----------------------------


;;makes a labels with ascending number
;;string->procedure ()=>string
;;exxample: 
;;(define gl(makeLabel "L"))
;; (gl) ==>L1
;; (gl)==> L2
(define makeLabel
    (lambda (str)
        (let* ((n 0))
            (lambda ()
               (set! n (+ n 1))
                (string-append str (number->string n))))))

;;---------------------------------code-gen-if3--------------------------------

;;create a label to jump to for else if condition is not whitestand                
(define create-else-label (makeLabel "L_if_else_"))

;;if (x) thenCode elseCode
;;cmp(x)
;; jz  "L_if_else_11"
;; [[thenCode]]
;; jpm L_if_end_11
;; L_if_else_11
;; [[elseCode]]
;; L_if_end_11
;;NOTE: 11 is random and it will be different identifier for each if-else statement

;;create a label to jump the end of the statement after we execute the "then" code                
(define create-endIf-label (makeLabel "L_if_end_"))


(define code-gen-if3
    (lambda (if-expr)
        (let* ((else-label (create-else-label))
              (endIf-label (create-endIf-label))
              (test (cadr if-expr))
              (thenExpr  (caddr if-expr))
              (elseExpr  (cadddr if-expr)))
            (string-append
                ";;if-start\n"
                  (begin
                    (code-gen test))
                    "cmp rax,SOB_FALSE\n"
                    "je "
                    else-label "\n"
                    (begin (code-gen thenExpr))
                    "jmp " endIf-label "\n"
                    else-label ":\n"
                    (begin (code-gen elseExpr))
                    endIf-label ":\n"
                    ";end-if" "\n"
            )        
        )
    )    
    
)

;;---------------------------------code-gen-const--------------------------------
(define code-gen-const
    (lambda (constToGen) 
        
        (let ((const  (number->string (cadr constToGen))))
            
            (string-append
                "MAKE_LITERAL(T_INTEGER, "    
                const ")\n"
            )
            
        )   
    )
)



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

;-----------------------------Constatn-table--------------------------------------
;;get an exp and returns its type
(define getConstType
    (lambda (x)
    (cond 
        ((number? x) "Number")
        ((list? x) "List")
        ((pair? x) "Pair")
        ((string? x) "String")
        ((vector? x) "Vector")
        ((symbol? x) "Symbol")
        ((char? x) "Char")
        ((boolean? x) "Boolean")
        ((void? x) "Void")
        ((improper? x) "improper")
    (else "Failed"))
    
    ))




(define getALLConstants
    (lambda (ASTExp)
        (cond  ((null? ASTExp) '())
               ((not (pair? ASTExp)) '())
               ((and (pair? ASTExp) (equal? 'const (car ASTExp))) (list (cadr ASTExp)))
               (else
                `(,@(getALLConstants (cdr ASTExp)) ,@(getALLConstants (car ASTExp)))
                )
            
        )
    )    
)

(define removedup
    (lambda (lst item) 
        
        (if (and (member item lst) #t)
            lst
            (cons item lst)
            )
        )
        
    
)

(define (void? x)
    (eq? x (void))
)

(define remove-primitive-helper
    (lambda (resLst item) 
        (if (or (boolean? item) (null? item) (void? item))
            resLst
            `(,@resLst ,item)
            )
        
        )
    
    )


(define remove-primitives
    (lambda (lst) 
        (fold-left remove-primitive-helper '() lst)
    )
)

(define topo-helper
    (lambda (lst item)
    (cond
        ((or (number? item) (char? item)(string? item) (null? item)(boolean? item))`(,@lst ,item))
        ((symbol? item)
          `(,@(topo-helper lst (symbol->string item)) ,item) )
        ((pair? item)
         `(,@(topo-helper lst (car item)) ,@(topo-helper '() (cdr item) ),item))
         ((vector? item)
          `(,@(apply append
           (fold-left topo-helper '() 
           (vector->list item))) ,item))
         )        
        
    
    )
)


(define topological-sort
    (lambda (constList)
        (if (null? constList) '()
            (fold-left topo-helper '() constList)
                
            
        ) 
    
    )
)


(define (remove-duplicates lst)
     (fold-left removedup '() lst)    
)




(define createConstTable
    (lambda (AST)
        
    
        (let* 
            ((constants  (reverse (getALLConstants  AST)))    ;;get All constants in code
             (no-Dup-Constatns (reverse  (remove-primitives (remove-duplicates constants))))
             (sortedConstLst    
                (reverse (remove-duplicates (remove-primitives(topological-sort no-Dup-Constatns)))))
            (constructed-table (prepareToWriteToAssembly sortedConstLst)) 
            )
            (display "constatns are: ")
            ;;(debugPrint constants)
            ;(debugPrint sortedConstLst)
            (debugPrint constructed-table)
            
            #t

            
            )
    )
)







;;get as an inoput an entry of type (address representation const) and a string to get
;; "addr" - returns address
;; "rep" - returns representation
;; "const" - returns the constants itself
 (define getFromEntry
    (lambda (fieldToGet entry) 
        (cond ((equal? fieldToGet "addr") (car entry))
              ((equal? fieldToGet "rep") (cadr entry))
              ((equal? fieldToGet "const")  (cadr(cdr entry)))
        
        )
    )
)  
(define create-const-pair-label (makeLabel "sobPair"))

(define create-const-reg-label
    (lambda (type val)
        (cond 
            ((number? val) (string-append "sob" type (number->string val)))
            ((symbol? val) (string-append "sob" type (symbol->string val)))
            (else (string-append "sob" type val))
            
        )
    )
)


(define find-rep
    (lambda (constLst repToFind) 
        
        


        (let (
                (onlyWithRep     
                    (filter (lambda (item) (eq? (getFromEntry "rep" item) repToFind)) constLst)
                )
             )
             
             
             (if (eq? (length onlyWithRep) 0)
                 #f
                  (getFromEntry "addr" (car onlyWithRep))
                    
            )
        )
        
    )
    
    
)


(define add-To-Constants-Table
    (lambda (existingConsts item) 
        ;;(debugPrint item)
        
        (cond 
            ((pair? item) 
                (if(not (find-rep existingConsts item))                
                    ;;in case we hadn't already insert this list to the table
                    (let*
                        (
                        (addedCdr (add-To-Constants-Table existingConsts (cdr item)))
						(addedCar (add-To-Constants-Table existingConsts (car item))) 
						(car-label (find-rep addedCar (car item)))
						(cdr-label (find-rep addedCar (cdr item)))
                        (label (create-const-pair-label))
                        )
                    
                            
                        (append addedCar (list (list label item (string-append label ": dq MAKE_LITERAL_PAIR(" car-label "," cdr-label ")"))))
                    )

                    ;;else
                    existingConsts
                
                )
            )





            ((not (pair? item))
             (if (not (find-rep existingConsts item))
                (let*  
                    ((constType (getConstType item))
                     (label  (create-const-reg-label constType item)))
                     (cond 
                        ((void? item) existingConsts)
                        ((boolean? item) existingConsts)
                        ((number? item) (append existingConsts 
                            (list (list label item (string-append label ": dq MAKE_LITERAL(T_INTEGER ," (number->string item) ")")))
                        ))
                        ((char? item) (append existingConsts 
                            (list (list label item (string-append label ": dq MAKE_LITERAL(T_CHAR ," (number->string (char->integer item)) ")" )))
                        ))
                        ((symbol? item) (append existingConsts 
                            (list (list label item (string-append label ": dq MAKE_LITERAL(T_SYMBOL ," (symbol->string item) ")")))
                        ))
                        ((string? item) (append existingConsts 
                            (list (list label item (string-append label ": dq MAKE_LITERAL_STRING " (string-append "\""  item "\""))))
                        ))
                    )
                    
                
                )                 
                 
                 existingConsts)
                        
            )
        )


    )
    
)



(define prepareToWriteToAssembly
    (lambda (const-lst)
        
        (let ((basics 
            (list 
                (list "sobNil" '() "sobNil: dq SOB_NIL")
                (list "sobTrue" #t "sobTrue: dq SOB_TRUE")
                (list "sobFalse" #f "sobFalse: dq SOB_FALSE") 
                (list "sobVoid" 'void "sobVoid: dq SOB_VOID")
            ))
        )
            
         (fold-left add-To-Constants-Table basics const-lst)
        )    
    )
)



(define code-gen
    (lambda (exprToGen) 
        
       (if  (not (list? exprToGen))
            (string-append (symbol->string exprToGen) "\n")
            (let ((tag (car exprToGen)))
                (debugPrint tag)
                (cond 
                    ((equal? tag `if3) (code-gen-if3 exprToGen))
                    ((equal? tag `const) (code-gen-const exprToGen))
                )
            
            ) 
       ) 
    
    )
)


(define write-to-file
    (lambda (file-name contents )
        ;;(print "@@in write " lst)
        (let* ((file (open-output-file file-name 'truncate)))
                ;(display prologue file)
                ;;(display constant-table file)
                ;;(display free-vars file)
                ;;(display cisc-symbols file)
                (display contents file)
                ;;(display epilogue file)
                (close-output-port file))
        ))






(define compile-scheme-file
    (lambda (scheme-file nasm-file) 
        
        (let* 
            ((stringExp (file->list scheme-file))
             (astExpression (car (pipeline stringExp)))
             (constTable (createConstTable astExpression))
             )
             #t

             ;;(write-to-file nasm-file (code-gen (car astExpression)))

             
            )
        
    )
)