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
                    (begin (code-gen thenExpr))
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
            (debugPrint const)
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


(define disectList
    (lambda (constLst item) 
        (debugPrint item)
        (cond
            ((or (number? item)    (char? item)  (string? item) (null? item))
                `(,@constLst ,item))
            ((symbol? item)  `(,@constLst ,(disectList '() (symbol->string item))))
            ((pair? item)
                `(,@constLst ,item ,@(disectList '() (car item)) ,@(disectList '() (cdr item)))
                )
           
            )
    )    
)


(define topologi-sort
    (lambda (lst)
        (if (null? lst) '()
        (fold-left disectList '() (reverse lst) )
        
        )

        ))




    
(define createConstTable
    (lambda (AST)
    
        (let* 
            ((constants (getALLConstants (car AST)))    ;;get All constants in code
            ;;((onstWithNoList  (fold-left disectLists '() constants)     ))
            )
            
            
            #t
           ;; (debugPrint constListWithNoDup)
            ;;listAndVectorSplitConsts
            
            )
        

    )
)



(define code-gen
    (lambda (exprToGen) 
        (debugPrint exprToGen)
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
             (astExpression (pipeline stringExp))
             (constTable (createConstTable astExpression))
             )
             
             (write-to-file nasm-file (code-gen (car astExpression)))

             
            )
        
    )
)