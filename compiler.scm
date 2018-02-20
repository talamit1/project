(load "semantic-analyzer.scm");
(load "tag-parser.scm");
(load "sexpr-parser.scm");

;;
(define library_functions
    `(("free_plus" "asm_plus"))
    )

;;create closurer and put it on func_name
(define create-free-func-closure
    (lambda (funcPair)
       (let ((func_free_place (car funcPair))
             (func_label (cadr funcPair))
            )
            (string-append
                "\n;creating closure for library function " func_free_place "\n" 
                "\tpush r15 \n"
                "\tpush rbx \n"
                "\tpush rax\n"
                "\tmy_malloc 16\n"
                "\tmov r15," func_label  "\t\t ;put in r11 tha func_label\n"
                "\tmov rbx,0 \t\t ;put dummy env in rbx \n"
                "\tMAKE_LITERAL_CLOSURE rax,rbx,r15\t\t ;Create closure on targer\n"
                "\tmov rax,[rax] \n"
                "\t mov ["func_free_place"],rax\n"
                "\tpop rax \n"
                "\tpop rbx \n"
                "\tpop r15\n" 
                
                )
       )       
    
    )
)




;;prologue
(define prologue
    (string-append 
        
        "%include \"scheme.s\"\n\n"
        "section .data\n"
        "start_of_data:\n\n"
        
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



;---------------------------------code-gen-applic--------------------------------

(define create-end-applic-label (makeLabel "After_gen_code"))
(define code-gen-applic
    (lambda (procExp constTable freeTable major) 
        (let ((proc (cadr procExp))
             (params (cadr (cdr procExp)))
             (end-label (create-end-applic-label))
             )
            (string-append  
                (apply string-append

                    "\t ; ------ APPLIC_START------------\n"  
                    (map (lambda (bi)
                            (string-append
                                (code-gen bi constTable freeTable major)    
                                "\tpush rax \t\t ;push param generated code to the stack\n"
                            )
                        )
                        (reverse params)
                    )
                    
                )
                "\tpush " (number->string (length params)) "\t\t ; pushe arg_count \n"
                (code-gen proc constTable freeTable major)
                ;";\tTYPE rax \t\t ;checks if proc s closure"
                ;"\tcmp rax,T_CLOSURE\n"
                "\tmov rbx,rax \t\t ;rsx holds the closure \n"
                "\tCLOSURE_ENV rbx \t\t ;rbx holds closure env\n"
                "\tpush rbx \n"
                "\tCLOSURE_CODE rax \t\t ;rax holds closure code\n"
                "\tcall rax \t\t ;call the function\n"
                "\tadd rsp,8\n"
                "\tpop r8\t\t;pop arg count\n" 
                "\timul r8, 8\t\t;r8 holds number of parameters*8\n"
                "\tadd rsp, r8\t\t;clears all args \n" 
            )
        )
    )
)

;;---------------------------------code-gen-tc-applic-----------------------------------

(define create-copy-frame-loop-label (makeLabel "copy_frame_loop_")) 
(define create-copy-frame-loop-exit (makeLabel "copy_frame_exit_")) 
(define create-debug-label (makeLabel "debug_label")) 

(define tc-applic-code-label (makeLabel "tc_applic_code")) 
    (define tc-applic-copy-label (makeLabel "tc_applic_copy")) 
(define code-gen-tc-applic
    (lambda (procExp constTable freeTable major) 
        (let* ((proc (cadr procExp))
             (params (cadr (cdr procExp)))
             (end-label (create-end-applic-label))
             (copy-frame-start (create-copy-frame-loop-label))
             (copy-frame-exit (create-copy-frame-loop-exit))
             (m (number->string (length params)))


             )

             (string-append (apply string-append 
                
                        (map (lambda (bi) 
                            (string-append (code-gen bi constTable freeTable major)
                                            "\tpush rax \n")) (reverse params))) 


                        "\tpush " m "\n\n"
                        (code-gen proc constTable freeTable  major)
                        
                        
                        "\tmov rbx , rax \n"

                        "\tCLOSURE_ENV rbx \n"
                        "\tpush rbx \n"

                       ;from here tc
                       "\tmov r8 , rbp   ;backup old rbp in r8 \n" 
                       "\tpush ret_addr  \t\t ;pushin old return address on the stack\n"
                       "\tmov r9,arg_count  \t\t  ;extract old_argCount to r9 r9=n \n"

                       "\tmov rbp , old_rbp     ;\t\t ;changing the rbp\n"
                                                                     
                        "\tadd r9 , 3 \t\t ;r9 = n+3\n"
                        "\tmov r11 , " m " \t\t ;r11=m\n" ;n= new arguments number
                        "\tadd r11 , 3  \t\t ;r11=m+3\n  "
                        
                        
                        "\tmov rbx , r9 \t\t ;rbx = n+3\n"
                        "\tshl rbx , 3  \t\t ;r9 = 8*(n+3)\n"
                        "\tadd rbx , r8  \t\t ;r8 = r8+8*(n+3)  \n"
                    

                        "\tmov rcx , r8 \n"
                        "\tsub rcx , 8 \n"

                                           
                        copy-frame-start ": \n"
                        
                        "\tcmp r11 , 0 \n"
                        "\tje " copy-frame-exit "\n"

                        "\tmov rdx , qword[rcx] \n"
                        "\tmov qword[rbx] , rdx \n" ;rbx[i]=rcx[i]
                    
                    
                        "\tsub rbx , 8 \n"
                        "\tsub rcx , 8 \n"
                        "\tdec r11 \n"
                
                        "\tjmp "   copy-frame-start "\n"
                        
                        copy-frame-exit ": \n"

                       "\tmov rsp ,rbx \n" ; make sure!!!
                       "\tadd rsp , 8 \n"
                      
                        "\tCLOSURE_CODE rax \n"
                        "\tjmp rax \n"
                                
                       
                )
        )
    )
)

;;---------------------------------code-gen-lambda-simple--------------------------------
 


(define make-new-env
    (lambda (major)        
        (string-append "\t;****new-env****\n"
                       "\tmy_malloc 16\n" ;;fixed from 8
                       "\tmov r14, rax\t\t;rbx holds pointer to cl object\n"
                       "\tmy_malloc (8*" (number->string (+ major 1)) ")\n"
                       "\tmov rbx, rax\t\t;rcx holds pointer to move previous env\n"
                       "\tmov r9, arg_count \n"
                       "\timul r9,8 \n"
                       "\tmy_malloc r9\t\t;rax holds pointer to the extension\n\n"
                       "\tmov rcx, rax\n"
         )    
    )
)
              
(define create-copy-parm-loop-label (makeLabel "copy_param_loop_"))          
(define create-copy-parm-end-label (makeLabel "copy_param_end_"))

(define create-copy-env-loop-label (makeLabel "copy_env_loop_"))
(define create-copy-env-end-label (makeLabel "copy_env_end_"))

(define copy-param-and-env
    (lambda (major)
        (let ((copy-param-loop-label (create-copy-parm-loop-label))
            (copy-param-end-label (create-copy-parm-end-label))
            (copy-env-loop-label (create-copy-env-loop-label))
            (copy-env-end-label (create-copy-env-end-label))
            )

            (string-append "\t;*****copy-param******\n"
                            "\tmov r8, arg_count\t\t;r8 holds arg count\n"
                            "\tmov r9, 0\t\t;r9 holds index for loop guard\n"
                            "\t" copy-param-loop-label ":\n"
                            "\tcmp r8, r9\n"
                            "\tje " copy-param-end-label "\n"
                            "\tmov r10, An(r9)\n"
                            "\tmov qword[rcx + 8*r9], r10\t\t;move old params to rax\n"
                            "\tadd r9,1 \t\t ;increase counter \n"


                            "\tjmp " copy-param-loop-label "\n"
                            "\t" copy-param-end-label ":\n"
                            "\tmov qword[rbx],rcx\t\t;add params from stack to extention\n"
                            
                            "\tmov r8, 0\t\t;'i' counter\n"
                            "\tmov r9,1\t\t; 'j' counter\n"
                            "\tmov r10, " (number->string major) "\n"
                            "\tmov r11, env\t\t;r11 holds env from stack\n"
                            
                            "\t" copy-env-loop-label ":\n"
                            "\tcmp r8,r10\t\t; check if copy ends\n"
                            "\tje " copy-env-end-label"\n"
                            "\tmov r12, qword[r11 + 8*r8]\t\t;r12 holds env[i]\n"
                            "\tmov qword[rbx + 8*r9], r12\t\t;copy env[i] to rbx\n"
                            "\tinc r8\t\t;inc j\n"
                            "\tinc r9\t\t;inc i\n"
                            "\tjmp " copy-env-loop-label "\n"
                            "\t" copy-env-end-label ":\n"
                            
            )
        )
    )        
)



(define create-close-body-label
                (makeLabel "\tL_close_body")
)
(define create-close-exit-label
    (makeLabel "\tlambda_close_exit_")
)

(define code-gen-lambda-simple
     (lambda (lambdaSimple-expr constTable freeTable major)
        (let  ((lambda-body (getLambdaBody 'lambda-simple lambdaSimple-expr))
               (bodyLabel (create-close-body-label))
               (exitLabel (create-close-exit-label))
              )
              ;(debugPrint bodyLabel)
              ;(debugPrint exitLabel)
              ;(debugPrint lambda-body) 
                (string-append  "\t;*****lambda simple start!*****\n\t" 
                (make-new-env major) 
                (copy-param-and-env major)
                "\tmov r15," bodyLabel "\n"
                

                "\tmov rax,r14\n"

                "\tMAKE_LITERAL_CLOSURE rax,rbx,r15\t\t ;Create closure on targer\n"
                "\tmov rax,qword[rax]\t\t    ;put the closure in rax\n"
                "\tjmp " exitLabel "\n"
                bodyLabel ":\n"
                "\tpush rbp\n"
                "\tmov rbp,rsp\n"
                (code-gen lambda-body constTable freeTable (+ major 1))
                "\tleave\n"
                "\tret \n"
                exitLabel ":\n"
                )
        )
    )
 )


;;---------------------------------code-gen-lambda-opt--------------------------------
    
(define create-param-to-list-label
    (makeLabel "\tL_copy_param_toList_loop_")
)
(define create-param-to-list-endLabel
(makeLabel "\tL_copy_param_toList_exit_")
)

(define create-fix-stack-position-label
    (makeLabel "\tL_fix_stack_position_loop_")
)
(define create-fix-stack-position-Endlabel
    (makeLabel "\tL_fix_stack_position_exit_")
)

(define create-nil-fix-loop-label
    (makeLabel "\tL_nil_fix_stack_loop_")
)

(define create-after-stack-fix-label
    (makeLabel "\tL_after_opt_stack_fix_")
)

(define create-after-nil-fix-label
    (makeLabel "\tL_after_nil_stack_fix_")
)


(define code-gen-lambda-opt
    (lambda (lambdaOpt-expr constTable freeTable major)
       (let  ((lambda-body (getLambdaBody 'lambda-opt lambdaOpt-expr))
              (bodyLabel (create-close-body-label))
              (exitLabel (create-close-exit-label))
              (copy_param_to_list_loop (create-param-to-list-label))
              (copy_param_to_list_end (create-param-to-list-endLabel))
              (numOfParams (length (getLambdaVars 'lambda-opt lambdaOpt-expr)))
              (fixStackStartLoop (create-fix-stack-position-label))
              (fixStackEndLoop (create-fix-stack-position-Endlabel))
              (afterAll (create-after-stack-fix-label))
              (afterNil (create-after-stack-fix-label))
              (nilLoop (create-nil-fix-loop-label))
             )
              (debugPrint numOfParams)
              ;(debugPrint exitLabel)
              ;(debugPrint lambda-body) 
               
              
              
               (string-append  "\t;*****lambda opt start!*****\n\t" 
               (make-new-env major) 
               (copy-param-and-env major)
               "\tmov r15," bodyLabel "\n"
               "\tmov rax,r14\n"
               "\tMAKE_LITERAL_CLOSURE rax,rbx,r15\t\t ;Create closure on targer\n"
               "\tmov rax,qword[rax]\t\t    ;put the closure in rax\n"
               "\tjmp " exitLabel "\n"
               bodyLabel ":\n"
               "\tpush rbp\n"
               "\tmov rbp,rsp\n"
      

                ; ;;;;;;;;check if opt is nil and need to insert only nil
                ; "\tmov r8," (number->string numOfParams) "\n"
                ; "\tsub r8, 1\n"
                ; "\tmov r9, qword[rbp + 8*3]\t\t; r8 holds args count\n"
                ; "\tcmp r8, r9\n"
                ; "\tjne " afterNil "\n"
                ; "\tmov r10, r9\n"
                ; "\tadd r10, 1\n" ;;r10 is the arg count +1 fir nil
                ; "\tmov qword[rbp + 8*3], r10\t\t;update arg count\n"
                ; "\tmov r10, rbp\n"
                ; "\tmov r11, r9\n"
                ; "\tadd r11, 4\n"; r11 = i (how many entrys to move down)
                ; nilLoop ":\n"
                ; "\tcmp r11, 0\n"
                ; "\tje " afterAll "\n"



                ; "\tjmp " nilLoop "\n"




               afterNil ":\n"
               ;;;;; TODO - Fix stack code
               "\tmov rdx, qword[sobNil]\t\t;nil for building the list of params recursivly\n"
               "\tmy_malloc 8\n"
               "\tmov qword [rax], rdx\n"
               "\tmov rdx, rax\n"
               "\tmov r8, qword[rbp + 8*3]\t\t; r8 holds args count\n"
               "\tmov r14, r8\n"
               "\tsub r8, 1\t\t ;r8 = i\n"
               "\tmov r9, " (number->string numOfParams) "\t\t; r9 holds number of params (e.g - (lambda (a b c . d)...) --> 4 )\n"
               "\tsub r14, r9\n"
               "\timul r14, 8\t\t;r14 holds the number we need to move every entry in our frame\n"
               "\tsub r9, 1\n"
                
               "\tmov r10, r9\n" 
               "\tadd r10,4\n"
               "\timul r10, 8\n" ;;r10 is where we save the list in stack ---> [ebp +r10] = list
               "\tsub r9, 1\n" ;;loop guard 
               


               ;;this loop makes the list of m parameters
               copy_param_to_list_loop ":\n"
               "\tcmp r8, r9\n"
               "\tje " copy_param_to_list_end "\n"
               "\tmov r11,An(r8)\n" 
               "\tmy_malloc 8\n"
               "\tmov qword[rax], r11\n"
               "\tmov rcx, rax\n"
               "\tmy_malloc 8\n"

               ;"\tmov r15, rax\n"
               "\tMAKE_MALLOC_LITERAL_PAIR rax,rcx,rdx\n"
               "\tmov rdx,rax\n"
               "\tsub r8, 1\t\t;i--\n"
               "\tjmp" copy_param_to_list_loop "\n"
               
               
               copy_param_to_list_end ":\n" 
               "\tmov qword[rbp + 8*3], "  (number->string numOfParams)   "\t\t;update arg count\n"
               "\tmov r15, qword[rdx]\n"
               "\tmov qword[rbp + r10], r15\n" ;;put the opt list in the correct place

               "\tmov r8, 4\n\t\t;  4 for old rbp,ret,env,arg count\n"
               "\tadd r8, " (number->string numOfParams) "\n"
               
               "\tmov r9, 8\n"
               "\timul r9, r8\n"
               "\tsub r9, 8\n"
               "\tadd r9, rbp\n"  ;;r9 hols position of first elemnt to fix his position
               ;;;;;remember r14 holds how much to jump


               fixStackStartLoop ":\n"
               "\tcmp r8, 0\n" ;;r8 = j (elements in stack)
               "\tje" fixStackEndLoop "\n"
               
               "\tmov r10, qword[r9] \n"
               
               "\tmov qword[r9+r14], r10\t\t; the fix\n"
            
               "\tsub r9, 8\n"
               "\tsub r8, 1\t\t; j--\n"
               "\tjmp" fixStackStartLoop "\n"
               
               fixStackEndLoop ":\n"
               "\tadd rbp,r14\n"
               "\tadd rsp, r14\n"
               
               afterAll ":\n"
               
               (code-gen lambda-body constTable freeTable (+ major 1))
               "\tleave\n"
               "\tret \n"
               exitLabel ":\n"
               )
       )
   )
) 

;;---------------------------------code-gen-pvar--------------------------------
(define code-gen-pvar
    (lambda (pvar-expr constTable freeTable major)
    (string-append "\t;****pvar start****\n" "\tmov rax, qword[rbp + (4 + " (number->string (caddr pvar-expr)) ")*8]\n" "\t;****pvar end****\n")
    )
)

;;---------------------------------code-gen-bvar--------------------------------
(define code-gen-bvar
    (lambda (bvar-expr constTable freeTable major)
    (string-append  "\t;****bvar start****\n"
                    "\tmov rax, qword[rbp + (2*8)]\n" ;env
                    "\tmov rax, qword[rax + " (number->string (caddr bvar-expr))   "*8]\n"   ;env[ma]
                    "\tmov rax, qword[rax + " (number->string (cadddr bvar-expr))  "*8]\n"   ;env [ma][mi]
                    "\t;****bvar end****\n")  
    )
)

;;---------------------------------code-gen-set!--------------------------------
;  (define code-gen-set
;      (lambda (set-expr constTable freeTable major)
;      )
;  )

;;---------------------------------code-gen-define--------------------------------
(define code-gen-define 
    (lambda (def-expr constTable freeTable major)
        (let ((defExp (caddr def-expr))
                (defVarLabel (find-rep freeTable (cadadr def-expr)))) 
                
                (string-append 
                "\t; ******start-define*****\n" 
                (code-gen defExp constTable freeTable major)
                ;"\tmov rax, qword[ rax]\n"
                "\tmov qword[" defVarLabel "], rax\n"
                "\tmov rax, sobVoid\n\t; ******end-define*****\n")
        )
    )
)

    ;;---------------------------------code-gen-set!--------------------------------
    (define code-gen-set
        (lambda (set-expr constTable freeTable major)
           (let ((toGen (caddr set-expr))
                 ) 
                 (cond ((equal? 'fvar (caadr set-expr))     (string-append "\t;*****set-fvar-start*****\n" (code-gen toGen constTable freeTable major)   
                                                                           "\tmov qword[" (find-rep freeTable (cadadr set-expr)) "], rax\n"
                                                                           "\tmov rax, sobVoid\n\t;*****set-fvar-end*****\n"))
                       ((equal? 'pvar (caadr set-expr))     (string-append "\t;*****set-pvar-start*****\n" (code-gen toGen constTable freeTable major)
                                                                           "\tmov qword[rbp + 8*(4 + " (number->string (caddr (cadr set-expr))) " )], rax\n" 
                                                                           "\tmov rax, sobVoid\n\t;*****set-pvar-end*****\n"))
                       ((equal? 'bvar (caadr set-expr))     (string-append "\t;*****set-bvar-start*****\n" (code-gen toGen constTable freeTable major)
                                                                           "\tmov rbx, qword[rbp + 8*2]\n" "\tmov rbx, qword[rbx + 8*" (number->string(car (cddadr set-expr))) "]\n" ;;major
                                                                           "\tmov qword[rbx + 8*" (number->string(cadr (cddadr set-expr))) "], rax\n" ;;minor
                                                                           "\tmov rax, sobVoid\n\t;*****set-bvar-end*****\n"))
                 )
   
           )
        )
    )
   
   ;;---------------------------------code-gen-define--------------------------------
   (define code-gen-define 
       (lambda (def-expr constTable freeTable major)
           (let    ((defExp (caddr def-expr))
                   (defVarLabel (find-rep freeTable (cadadr def-expr)))) 
                   
                   (string-append 
                   "\t; ******start-define*****\n" 
                   (code-gen defExp constTable freeTable major)
                   "\tmov qword[" defVarLabel "], rax\n"
                   "\tmov rax, sobVoid\n\t; ******end-define*****\n")
           )
       )
   )


;;---------------------------------code-gen-if3--------------------------------

;;create a label to jump to for else if condition is not whitestand                
(define create-else-label (makeLabel "L_if_else_"))
    
;;create a label to jump the end of the statement after we execute the "then" code                
(define create-endIf-label (makeLabel "L_if_end_"))


(define code-gen-if3
    (lambda (if-expr constTable freeTable major)
        (let* ((else-label (create-else-label))
                (endIf-label (create-endIf-label))
                (test (cadr if-expr))
                (thenExpr  (caddr if-expr))
                (elseExpr  (cadddr if-expr)))
            (string-append
                "\t;if-start\n"
                    (begin
                    (code-gen test constTable freeTable major))
                    "\tcmp rax,SOB_FALSE\n"
                    "\tje "
                    else-label "\n"
                    (begin (code-gen thenExpr constTable freeTable major))
                    "\tjmp " endIf-label "\n"
                    "\t" else-label ":\n"
                    (begin (code-gen elseExpr constTable freeTable major))
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
    (lambda (or-expr constTable freeTable major)
        (let* ((endOr-label (create-endOr-label))
                (orLst1 (cadr or-expr))
                (bool #f))
                (letrec ((run (lambda (orLst)
                                (if (null?  orLst) 
                                    bool 
                                    (if (null? (cdr orLst))
                                        (string-append
                                            
                                            (begin (code-gen (car orLst) constTable freeTable major))
                                            "\t" endOr-label":\n\t")
                                        (string-append
                                            (begin (code-gen (car orLst) constTable freeTable major))
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
    (lambda (seq-expr constTable freeTable major) 
        ;(debugPrint seq-expr)
            (fold-left string-append "" (map (lambda (x) (code-gen x constTable freeTable major)) (cadr seq-expr)))
    )
)

   ;;---------------------------------code-gen-fvar--------------------------------
   (define code-gen-fvar
    (lambda (freeToGen constTable freeTable major) 
                (string-append
                    "\tmov rax, qword[" (find-rep freeTable (cadr freeToGen)) "]\n"
                )         
    )
)

;;---------------------------------code-gen-const--------------------------------
(define code-gen-const
    (lambda (constToGen constTable freeTable major) 
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
    ;(debugPrint item)
    (cond
        ((or (integer? item) (char? item)(string? item) (null? item)(boolean? item) (symbol? item) )`(,@lst ,item))
        ((and (rational? item) (not (integer? item))
                `(,@lst ,(numerator item) ,(denominator item) ,item )))
        ((pair? item)
            `(,@(topo-helper lst (car item)) ,@(topo-helper '() (cdr item) ),item))
            ((vector? item)
            `(,@lst ,@(fold-left topo-helper '() 
            (vector->list item)) ,item))
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
            ;(debugPrint constants)
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
(define create-string-label (makeLabel "sobstr"))
(define create-vec-label (makeLabel "sobvec"))
(define create-int-label (makeLabel "sobInt"))
(define create-frac-label (makeLabel "sobFrac"))
(define create-char-label (makeLabel "sobChar"))

(define create-const-reg-label
    (lambda (type val)
        (integer? val)
        (cond 
            ((integer? val) (create-int-label))
            ((symbol? val) (string-append "sob" type (symbol->string val)))
            ((string? val)   (create-string-label))
            ((vector? val)   (create-vec-label))
            ((char? val)    (create-char-label))
            ((and(rational? val)(not (integer? val)))   (create-frac-label))
            
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


(define getFromTable
    (lambda (lst)
        (lambda (item) 
            (find-rep lst item)
        )
    )
)

(define prepareString
    (lambda (str item) 
        (string-append str ", " item  )
        )
)
(define make-vec-rep
    (lambda (vector table) 
        (let* ((vecLst (vector->list vector))
                (getFromCurrLst (getFromTable table))
                (itemRepList  (map getFromCurrLst vecLst))
                (first (car itemRepList))
                (rest  (cdr itemRepList))
                ) 
                
                (string-append first (fold-left prepareString "" rest))
        )
    )
)
(define specialChar?
    (lambda (char) 
        (cond
            ((equal? char #\space) "CHAR_SPACE" )
            ((equal? char #\newline) "CHAR_NEWLINE")
            ((equal? char #\return) "CHAR_RETURN")
            ((equal? char #\nul) "CHAR_NUL")
            ((equal? char #\tab) "CHAR_TAB")
            ((equal? char #\page) "CHAR_PAGE")
            (else #f)
            
            )
        )
    )

    (define spec?
        (lambda (str) 
            (or
                (equal? str  "CHAR_SPACE" )
                (equal? str  "CHAR_NEWLINE")
                (equal? str  "CHAR_RETURN")
                (equal? str  "CHAR_NUL")
                (equal? str  "CHAR_TAB")
                (equal? str  "CHAR_PAGE")
            )
        )
    )

(define splitSpecialChars
    (lambda (lst ch) 
        (let ((isSpecialspechialChar (specialChar? ch)))
            (if isSpecialspechialChar
                `(,isSpecialspechialChar ,@lst)
                (if(null? lst)
                    `(,(string-append "" (make-string 1 ch))  ,@lst)
                    (if (spec? (car lst))
                        `(,(make-string 1 ch) ,@lst)
                        `(,(string-append (car lst) (make-string 1 ch)) ,@(cdr lst))
                    )
                )
            )
        )
    )
)

(define addQuotes
    (lambda (stringToBeQuoted) 
        (if (spec? stringToBeQuoted)
            stringToBeQuoted
            (string-append "\"" stringToBeQuoted "\"")
        )
    )
)

(define splitString 
    (lambda (str) 
        (let* ((lstStr (string->list str))
            (splitedList (reverse (fold-left splitSpecialChars `() lstStr )))
            (withQuotes (map addQuotes splitedList))
            (first (car withQuotes))
            (rest  (cdr withQuotes))
            )

            (string-append first (fold-left prepareString "" rest))
                                

        )
        )
    )


(define add-To-Constants-Table
    (lambda (existingConsts item) 
        
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
                        ((integer? item) (append existingConsts 
                            (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_INTEGER ," (number->string item) ")\n")))
                            
                        ))
                        ((char? item) (append existingConsts 
                            (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_CHAR ," (number->string (char->integer item )) ")\n" )))
                        ))
                        ((symbol? item)  (append existingConsts 
                            (list (list label item (string-append label ":\n\tdq MAKE_LITERAL(T_SYMBOL ," (symbol->string item) ")\n")))
                        ))
                        ((string? item) 
                            (append existingConsts 
                            (list (list label item (string-append label ":\n\tdq MAKE_LITERAL_STRING " (string-append (splitString item)  "\n"  )))) 
                        ))
                        ((and (rational? item) (not (integer? item)))     
                            (let ((numer  (find-rep existingConsts (numerator item)))
                                  (denom  (find-rep existingConsts (denominator item))))
                                  (append existingConsts
                                  (list (list label item (string-append label ":\n\tdq MAKE_LITERAL_FRACTION(" numer "," denom  ")\n"))))
                            )
                        )
                        ((vector? item)
                        (append existingConsts
                        (list (list label item (string-append label ":\n\tdq MAKE_LITERAL_VECTOR " (string-append  (make-vec-rep item existingConsts)  "\n"  )))) 
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
                (list "sobVoid" (void) "sobVoid:\n\tdq SOB_VOID\n")
            ))
        )
            ;(debugPrint const-lst)  
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
            (if (find-rep basicFree x) #f 
            (list label x (string-append label ":\n\tdq SOB_UNDEFINED\n")))
        )
    )
)

(define build-free-vars-table
    (lambda (lst)
        (filter (lambda (x)  x) (map helpForFree lst))
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
            ;(display "free are: ")
            ;(debugPrint free-table)     
            free-table      
            )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define convert-to-string
    (lambda (lst)
        (fold-right string-append "" lst)
    )
)

(define only-rep-list
    (lambda (constLst)
        (map (lambda (x) (car (reverse x))) constLst)
    ) 
)

;;11codeGen
(define code-gen
    
    (lambda (exprToGen constTable freeTable major) 
        (debugPrint exprToGen)
        (if  (not (list? exprToGen))
            (string-append (symbol->string exprToGen) "\n")
            (let ((tag (car exprToGen)))
                    ;(debugPrint "Tall")
                    (cond 
                    ((equal? tag `const) (code-gen-const exprToGen constTable freeTable major))
                    ((equal? tag `fvar) (code-gen-fvar exprToGen constTable freeTable major))
                    ((equal? tag `define) (code-gen-define exprToGen constTable freeTable major))
                    ((equal? tag `if3) (code-gen-if3 exprToGen constTable freeTable major))
                    ((equal? tag `or) (code-gen-or exprToGen constTable freeTable major))
                    ((equal? tag `seq) (code-gen-seq exprToGen constTable freeTable major))
                    ((equal? tag `set) (code-gen-set exprToGen constTable freeTable major))
                    ((equal? tag `pvar) (code-gen-pvar exprToGen constTable freeTable major))
                    ((equal? tag `bvar) (code-gen-bvar exprToGen constTable freeTable major))
                    ((equal? tag `lambda-simple) (code-gen-lambda-simple exprToGen constTable freeTable major))
                    ((equal? tag `applic) (code-gen-applic exprToGen constTable freeTable major))
                    ((equal? tag `tc-applic) (code-gen-tc-applic exprToGen constTable freeTable major))
                    ((equal? tag `lambda-opt) (code-gen-lambda-opt exprToGen constTable freeTable major))
                    ;((equal? tag `lambda-var) (code-gen-lambda-var exprToGen constTable freeTable major))
                    )
            ) 
        ) 
    )
)


(define write-to-file
    (lambda (file-name contents constantTable freeTable)
        ;;(print "@@in write " lst)
        (let* ((file (open-output-file file-name 'truncate))
                (createLib (apply string-append (map create-free-func-closure library_functions)))
                )
                (display prologue file)
                (display constantTable file)
                (display freeTable file)
                
                ;;(display symbolsTable file)
                (display (string-append  "\nsection .bss\n"
                    "extern exit, printf, scanf\n"
                    ;"global main, write_sob, write_sob_if_not_void\n"
                    "malloc_pointer:\n"
                    "resq 1\n"
                    "start_of_malloc:\n"
                    "resb 2^30\n"
                    "\nsection .text\n\n"
                    "main:\n"
                    "\tnop\n"
                    "\tmov rax, malloc_pointer\n"
                    "\tmov qword [rax], start_of_malloc\n"
                    "\tpush 0\n" ;arg_count
                    "\tpush 0\n" ;env
                    "\tpush 0\n" ;ret
                    "\tpush rbp\n"
                    "\tmov rbp, rsp\n"
                    ) file)
                    (display createLib file)
                (display contents file)  ;here is the code gen output
                (display epilogue file)  ;here comes all the implementation fot all global functions
                
                (close-output-port file))
    )
)

(define show
    (pipeline (file->list "foo.scm"))
    )

(define create-check-void-label (makeLabel "check_void_lable_")) 
 
(define compile-scheme-file
    (lambda (scheme-file nasm-file)    
        (let* 
            ((stringExp (file->list scheme-file))
                (astExpression  (pipeline stringExp))
                (constTable (createConstTable astExpression))
                (consTableRep (convert-to-string (only-rep-list constTable)))
                (freeTable (createFreeTable astExpression))
                (freeTableRep (convert-to-string (only-rep-list freeTable)))
                (codeEpilogue (string-append 
                "\tpush RAX\n"
                "\tcall write_sob\n"
                "\tadd rsp,8\n"
                "\tmov rdi, newline\n\tmov rax, 0\n\tcall printf\n"
                ))
                (generated-code (convert-to-string (map (lambda(x) (let ((checkVoid (create-check-void-label)))
                 (string-append "\n\n" (code-gen x constTable freeTable 0) "\tcmp rax, sobVoid\n" "\tje " checkVoid "\n" codeEpilogue "\t" checkVoid ":\n" "\n\n"))) astExpression)))
                )
                (display "\n\n\n")
                ;(debugPrint 555)
                ;(debugPrint astExpression)
                ;(debugPrint constTable)
                (display "\n\n\n")
                (write-to-file nasm-file generated-code consTableRep freeTableRep)  
            )
    )
)


(define asm_plus
      (string-append
            "\nasm_plus: \n"
            "\tpush rbp \n"
            "\tmov rbp,rsp \n"
            "\tpush rbx \n"
            "\tmov rax,An(0)\n"
            "\tmov rbx,An(1)\n"
            "\tDATA rbx\n" 
            "\tDATA rax\n"           
            "\tadd rax,rbx \n"
            "\tMAKE_INT rax\n"
            "\tpop rbx \n"
            "\tleave \n"
            "\tret \n"
            )
        
        )



        (define library_functions_creation_list
            `(,asm_plus)
            
            )

        ;epilogue
(define epilogue
    (string-append 
        "\t epilouge: \n"
        "\tleave\n"
        "\tpop r12\n"
        "\tpop r13\n"
        "\tpop r14\n"
        "\tret\n\n"  
        "section .data\n"
        "newline:\n\t"
            "db CHAR_NEWLINE, 0\n\n"
        (apply string-append library_functions_creation_list)
        
    )
    
)
