(load "semantic-analyzer.scm");
(load "tag-parser.scm");
(load "sexpr-parser.scm");
    
;;prologue
    (define prologue
    (string-append 
    "%include \"scheme.s\"\n\n"
    "section .data\n"
    "start_of_data:\n\n"
    )
)
    
;epilogue
(define epilogue
    (string-append 
    "\tret\n\n"  
    "section .data\n"
    "newline:\n\t"
        "db CHAR_NEWLINE, 0\n"
    )
)

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


    ;;---------------------------------code-gen-define--------------------------------
    (define code-gen-define 
        (lambda (def-expr constTable freeTable)
            (let ((defExp (caddr def-expr))
                  (defVarLabel (find-rep freeTable (cadadr def-expr)))) 
                    
                  (string-append 
                   "\t; ******start-define*****\n" 
                   (code-gen defExp constTable freeTable)
                   "\tmov [" defVarLabel "], rax\n"
                   "\tmov rax, SOB_VOID\n\t; ******end-define*****\n")
            )
        )
    )


    ;;---------------------------------code-gen-if3--------------------------------
    
    ;;create a label to jump to for else if condition is not whitestand                
    (define create-else-label (makeLabel "L_if_else_"))
       
    ;;create a label to jump the end of the statement after we execute the "then" code                
    (define create-endIf-label (makeLabel "L_if_end_"))
    
    
    (define code-gen-if3
        (lambda (if-expr constTable freeTable)
            (let* ((else-label (create-else-label))
                  (endIf-label (create-endIf-label))
                  (test (cadr if-expr))
                  (thenExpr  (caddr if-expr))
                  (elseExpr  (cadddr if-expr)))
                (string-append
                    "\t;if-start\n"
                      (begin
                        (code-gen test constTable freeTable))
                        "\tcmp rax,SOB_FALSE\n"
                        "\tje "
                        else-label "\n"
                        (begin (code-gen thenExpr constTable freeTable))
                        "\tjmp " endIf-label "\n"
                        "\t" else-label ":\n"
                        (begin (code-gen elseExpr constTable freeTable))
                        "\t"endIf-label ":\n"
                        "\t;end-if" "\n"
                )        
            )
        )    
    )
    

    ;;---------------------------------code-gen-or--------------------------------
    
    ;;create a label to jump the end of the or statment               
    (define create-endOr-label (makeLabel "L_or_end_"))
    
    (define code-gen-or
        (lambda (or-expr constTable freeTable)
            (let* ((endOr-label (create-endOr-label))
                  (orLst1 (cadr or-expr))
                  (bool #f))
                  (letrec ((run (lambda (orLst)
                                   (if (null?  orLst) 
                                        bool 
                                        (if (null? (cdr orLst))
                                            (string-append
                                                
                                                (begin (code-gen (car orLst) constTable freeTable))
                                                "\t" endOr-label":\n\t")
                                            (string-append
                                                (begin (code-gen (car orLst) constTable freeTable))
                                                "\t"
                                                "cmp rax,SOB_FALSE\n\t"
                                                "jne "
                                                endOr-label "\n"
                                                (run (cdr orLst)))
                                        )))))
                     (string-append "\t;or-start\n" (run orLst1) ";or-end\n")
                  ))
        )      
    )
    
    ;;---------------------------------code-gen-seq--------------------------------
    
    (define code-gen-seq
        (lambda (seq-expr constTable freeTable) 
            (debugPrint seq-expr)
             (fold-left string-append "" (map (lambda (x) (code-gen x constTable freeTable)) (cadr seq-expr)))
        )
    )

    ;;---------------------------------code-gen-const--------------------------------
    (define code-gen-const
        (lambda (constToGen constTable freeTable) 
                    (string-append
                        "\tmov rax, qword[" (find-rep constTable (cadr constToGen)) "]\n")                  
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
        
        )
    )
    
 
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
                ;(display "constatns are: ")
                ;;(debugPrint constants)
                ;(debugPrint sortedConstLst)
                ;(debugPrint constructed-table)
                constructed-table             
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
                        (filter (lambda (item) (equal? (getFromEntry "rep" item) repToFind)) constLst)
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
                            (append addedCar (list (list label item (string-append label ":\n\tdq MAKE_LITERAL_PAIR(" car-label "," cdr-label ")\n"))))
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
                                (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_INTEGER ," (number->string item) ")\n")))
                            ))
                            ((char? item) (append existingConsts 
                                (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_CHAR ," (number->string (char->integer item)) ")\n" )))
                            ))
                            ((symbol? item) (append existingConsts 
                                (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_SYMBOL ," (symbol->string item) ")\n")))
                            ))
                            ((string? item) (append existingConsts 
                                (list (list label item (string-append label ":\n\tdq MAKE_LITERAL_STRING " (string-append "\""  item "\"")))) 
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
                    (list "sobNil" '() "sobNil:\n\tdq SOB_NIL\n")
                    (list "sobTrue" #t "sobTrue:\n\tdq SOB_TRUE\n")
                    (list "sobFalse" #f "sobFalse:\n\tdq SOB_FALSE\n") 
                    (list "sobVoid" 'void "sobVoid:\n\tdq SOB_VOID\n")
                ))
            )
                
             (fold-left add-To-Constants-Table basics const-lst)
            )    
        )
    )

;----------------------------Free-Var-Table-----------------------------;
    
    (define getAllFreeVars
        (lambda (ASTExp)
            (cond  ((null? ASTExp) '())
                   ((not (pair? ASTExp)) '())
                   ((and (pair? ASTExp) (equal? 'fvar (car ASTExp))) (list (cadr ASTExp)))
                   (else
                    `(,@(getAllFreeVars (cdr ASTExp)) ,@(getAllFreeVars (car ASTExp)))
                    )
            )
        )    
    )
    
    ;make initial free vars table with elemntary procedures
    (define basicFree
                (list 
                    (list "free_plus" '+ "free_plus:\n\tdq SOB_UNDEFINED\n")
                    (list "free_minus" '- "free_minus:\n\tdq SOB_UNDEFINED\n")
                    (list "free_shave" '= "free_shave:\n\tdq SOB_UNDEFINED\n")
                    (list "free_mul" '* "free_mul:\n\tdq SOB_UNDEFINED\n")
                    (list "free_div" '/ "free_div:\n\tdq SOB_UNDEFINED\n")
                    (list "free_grater" '> "free_grater:\n\tdq SOB_UNDEFINED\n")
                    (list "free_smaller" '> "free_smaller:\n\tdq SOB_UNDEFINED\n")  
                )
            )
    
    (define helpForFree
        (lambda (x)
            (let ((label (string-append "free_" (symbol->string x))))
            
                (list label x (string-append label ":\n\tdq SOB_UNDEFINED\n"))
            )
        )
    )
    
    (define build-free-vars-table
        (lambda (lst)
            (map helpForFree lst)
        )
    )
    
    (define (mergeFree lst1 lst2)
        (cond((null? lst1) lst2)
            (else (cons (car lst1) (mergeFree (cdr lst1) lst2) ))))
    
    (define createFreeTable
        (lambda (AST)
            (let* 
                ((frees  (reverse (getAllFreeVars  AST)))    ;;get All free vars in code
                 (no-Dup-Free (reverse (remove-duplicates frees)))
                 (constructed-free-table (build-free-vars-table no-Dup-Free))
                 (free-table (mergeFree basicFree constructed-free-table))
                )
                (display "free are: ")
                (debugPrint free-table)     
                free-table      
                )
        )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define make-string
        (lambda (lst)
            (fold-right string-append "" lst)
        )
    )

    (define only-rep-list
        (lambda (constLst)
            (map (lambda (x) (car (reverse x))) constLst)
        ) 
    )
    
    (define code-gen
        (lambda (exprToGen constTable freeTable) 
           (if  (not (list? exprToGen))
                (string-append (symbol->string exprToGen) "\n")
                (let ((tag (car exprToGen)))
                     (debugPrint tag)
                     (cond 
                        ((equal? tag `const) (code-gen-const exprToGen constTable freeTable))
                        ((equal? tag `if3) (code-gen-if3 exprToGen constTable freeTable))
                        ((equal? tag `or) (code-gen-or exprToGen constTable freeTable))
                        ((equal? tag `seq) (code-gen-seq exprToGen constTable freeTable))
                        ((equal? tag `define) (code-gen-define exprToGen constTable freeTable))
                     )
                ) 
           ) 
        )
    )
    
    
    (define write-to-file
        (lambda (file-name contents constantTable freeTable)
            ;;(print "@@in write " lst)
            (let* ((file (open-output-file file-name 'truncate)))
                    (display prologue file)
                    (display constantTable file)
                    (display freeTable file)
                    ;;(display cisc-symbols file)
                    (display (string-append  "\nsection .bss\n"
                        "extern exit, printf, scanf\n"
                        ;"global main, write_sob, write_sob_if_not_void\n"
                        "section .text\n\n"
                        "main:\n"
                        "\tnop\n") file)
                    (display contents file)  ;here is the code gen output
                    (display epilogue file)  ;here comes all the implementation fot all global functions
                    (close-output-port file))
        )
    )
    
    
    (define compile-scheme-file
        (lambda (scheme-file nasm-file)    
            (let* 
                ((stringExp (file->list scheme-file))
                 (astExpression  (pipeline stringExp))
                 (constTable (createConstTable astExpression))
                 (consTableRep (make-string (only-rep-list constTable)))
                 (freeTable (createFreeTable astExpression))
                 (freeTableRep (make-string (only-rep-list freeTable)))
                 (codeEpilogue (string-append "\tpush RAX\n"
                    "\tcall write_sob\n"
                    "\tadd rsp,8\n"
                    "\tmov rdi, newline\n\tmov rax, 0\n\tcall printf\n"
                    ))
                 (generated-code (make-string (map (lambda(x) (string-append "\n\n" (code-gen x constTable freeTable) codeEpilogue "\n\n")) astExpression)))
                 )
                 (display "\n\n\n")
                 (debugPrint astExpression)
                 ;(debugPrint  (cddar constTable))
                 (display "\n\n\n")
                 (write-to-file nasm-file generated-code consTableRep freeTableRep)  
                )
        )
    )