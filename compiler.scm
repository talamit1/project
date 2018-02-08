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
            ((constants  (getALLConstants  AST))    ;;get All constants in code
             (no-Dup-Constatns  (remove-primitives (remove-duplicates constants)))
             (sortedConstLst    
                (reverse (remove-duplicates (remove-primitives(topological-sort no-Dup-Constatns)))))
             )
            
            (debugPrint sortedConstLst)
            #t

            
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