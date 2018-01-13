(load "pc.scm") 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     whiteSpaces     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(define <whiteSpaces>
	(new
		(*parser (range (integer->char 0) (integer->char 32))) 
			*plus
		(*pack 
			(lambda (_) ""))		
	done)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     comments     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <comment>
	(new 
		(*parser (char #\;))
		(*parser <any-char>)
		(*parser (char #\newline))
		(*parser <end-of-input>)
		(*disj 2)
		*diff
		*star
		(*parser (char #\newline))
		(*parser <end-of-input>)
		(*disj 2)
		(*caten 3)
	done)
	
)

(define <sexp-comment>
	(new
		(*parser (word "#;"))
		(*delayed (lambda ()  <sexpr>))
		(*caten 2)		
	done)
)


(define <Ignore>
	(new
		(*parser <whiteSpaces>)
		(*parser <comment>)
		(*parser <sexp-comment>)
		(*disj 3)
		*plus
	done)
	)


;(test-string <comment> "%sdafadfsfsdf" )


(define <digit0-9>
	(range #\0 #\9))

(define <digit1-9>
	(range #\1 #\9))

(define <nat>
  (new (*parser <digit0-9>) *plus 
        (*pack 
       	(lambda (resList) (string->number (list->string `(,@resList)))))
	   done)
)

(define <plus>
	(pack
		 (char #\+)
		 (lambda (res) ( string res))
			))
(define <minus>
	(pack
		 (char #\-)
		 (lambda (res) ( string res))
			))


(define intToRet
	(lambda (sign int)
		(string->number (string-append sign (number->string int)))
		)
)



(define <boolean>
	(new
		(*parser (char #\#))
		(*parser (char-ci #\t))
		(*caten 2)
		(*pack-with 
			(lambda (ch bool) #t))
	
		(*parser (char #\#))
		(*parser (char-ci #\f))
		(*caten 2)
		(*pack-with 
			(lambda (ch bool) #f))
	
		(*disj 2)
	done)
)

;(test-string <Char> "#\\t");

(define <integer>
	(new 
		 (*parser <plus>)
		  (*parser <nat>) 
		  (*caten 2)
		  (*pack-with intToRet)

		  (*parser <minus>)
		  (*parser <nat>) 
		  (*caten 2)
		  (*pack-with intToRet)


		  (*parser <nat>)

		  (*disj 3)
		   done
	)
)

(define <fraction>
	(new
		(*parser <integer>)
		(*parser (char #\/))
		(*parser <nat>)
		(*parser (char #\0))
		(*pack (lambda (a) (char->integer a)))
		*diff

		(*caten 3)
		
		(*pack-with
			(lambda (int ch nat)
				( / int nat))
		)    

		done)
)


(define <Number>
(new 
	(*parser (disj <fraction> <integer>))
	(*delayed (lambda () <SymbolChar>))
	*not-followed-by	
done)	
)


(define <HexChar>
		(disj <digit0-9> (range-ci #\a #\f) )
)

(define <HexUnicodeChar>
	(pack-with
	(caten (char-ci #\x) (plus <HexChar> ) )
	(lambda (x num)
		(integer->char (string->number (list->string num) 16) )
		)
	)
	
)

(define <VisibleSimpleChar>
	(const (lambda (ch)  
		(char<=? #\  ch)
		)
	)
)

(define <CharPrefix>
	(word "#\\")
)

(define buildNamed 
	(lambda (str ch)
		(pack
			(word-ci str) 
			(lambda (_) ch)
		)	
	)
)

(define <NamedChar>
	(new
		(*parser (buildNamed "lambda" (integer->char 955)))
		(*parser (buildNamed "nul" #\nul ))
		(*parser (buildNamed "tab" #\tab ))
		(*parser (buildNamed "newline" #\newline ))
		(*parser (buildNamed "return" #\return ))
		(*parser (buildNamed "page" #\page))
		(*parser (buildNamed "space" #\space))
		(*disj 7)
		done)	
)



(define <Char>
	(new 
		(*parser <CharPrefix>)
		(*parser <HexUnicodeChar>)
		(*parser <NamedChar>)
		(*parser <VisibleSimpleChar>)
		(*disj 3)
		(*caten 2)

		(*parser <any>)
		
		(*parser <Ignore>)
		(*parser (char #\,))
		(*parser (char #\]))
		(*parser (char #\)))
		(*disj 4)
		
		*diff
		
		*not-followed-by

		(*pack-with
			(lambda(charPref ch) ch))
		done
   	)
)



(define <SymbolChar>
	(new
		(*parser <digit0-9>)
		(*parser (range #\a #\z))
		(*parser (range #\A #\Z))
		(*pack 
		(lambda (ch)
			(integer->char (+ (char->integer ch) 32))))

		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\>))
		(*parser (char #\<))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 15)

	done)
	)


(define <symbol>	
	(new
		(*parser <SymbolChar>) *plus
	
		(*pack
			(lambda (cha)
				(string->symbol (list->string cha )))
		)
	done
	)
)


(define <stringHexChar>
	(new
		(*parser (char #\\))
		(*parser <HexUnicodeChar>)
		(*parser (char #\;))
		(*caten 3)

		(*pack-with
			(lambda(strHex str semi) str))
	done
	)
)

;(test-string <stringHexChar> "\\x61;") ;((match #\a) (remaining ""))


(define <StringMetaChar>

	(new
	(*parser (word-ci "\\\\"))
		(*pack (lambda (_) #\\))
	(*parser (word-ci "\\\""))
		(*pack (lambda (_) #\"))
	(*parser (word-ci "\\t"))
		(*pack (lambda (_) #\tab))
	(*parser (word-ci "\\f"))
		(*pack (lambda (_) #\page))
	(*parser (word-ci "\\n"))
		(*pack (lambda (_) #\newline))
	(*parser (word-ci "\\r"))
		(*pack (lambda (_) #\return))
	(*disj 6)
	done
	)
)

;(test-string <StringMetaChar> "\\\\")

(define <StringLiteralChar>
	(new (*parser <any>)
     	 (*parser (char #\\))
      	 (*parser (char #\"))
      	 (*disj 2)
      	*diff
	done)
)
;(test-string <StringLiteralChar> "hfh")

(define <StringChar>
(new 
	(*parser <StringLiteralChar>)
	(*parser <StringMetaChar>)
	(*parser <stringHexChar>)
	(*disj 3)

done)

)

(define <string>
	(new
		(*parser (char #\"))
		(*parser <StringChar>) *star
		(*parser (char #\"))

		(*caten 3)

		(*pack-with
			(lambda(open string close) 
				(list->string string)))

	done)

)

(define <sexpr> 
  (new
	(*parser <Ignore>) *maybe
	
  	(*parser <boolean>)
  	(*parser <Char>)
  	(*parser <Number>)
  	(*parser <string>)
  	(*parser <symbol>)
  	(*delayed (lambda () <ProperList>))
  	(*delayed (lambda () <ImproperList>))
  	(*delayed (lambda () <Vector>))
  	(*delayed (lambda () <Quoted>))
  	(*delayed (lambda () <QuasiQuoted>))
  	(*delayed (lambda () <Unquoted>))
  	(*delayed (lambda () <UnquoteAndSpliced>))
  	(*delayed (lambda () <CBName>))
  	(*delayed (lambda () <InfixExtension>))
	(*disj 14) 
		
	(*parser <Ignore>) *maybe

	(*caten 3)
	(*pack-with
		(lambda (ig1 sx ig2) sx))
  	
  done) 
  ) 


(define <ProperList>

	(new
		(*parser (char #\( ))
		(*parser <sexpr>) *star
		(*parser (char #\) ))
		(*caten 3)
		(*pack-with
			(lambda (pern1 sexp pern2) sexp ))
	done)
)

(define <ImproperList>
	(new
		(*parser (char #\( ))
		(*parser <sexpr>) *plus
		(*parser (char #\. ))
		(*parser <sexpr>)
		(*parser (char #\) ))
		(*caten 5)
		(*pack-with 
			(lambda (pern1 sexplst dot sexp pern2) (append sexplst sexp)))
	done)
)

(define <Vector>
	(new
		(*parser (char #\# ))
		(*parser <ProperList>)
		(*caten 2)
		(*pack-with
		(lambda (pre vec) (list->vector vec)))
	done)
)

;catenate quote, unquote and quasiquote to <sexpr> 
(define catenPreSexp
	(lambda (pre str)
	(new
		(*parser (char pre))
		(*parser <sexpr>)
		(*caten 2)
	(*pack-with
		(lambda (pref sx) (list str sx)))
	done) 
	)
)


(define <Quoted>	
		(catenPreSexp #\' 'quote)
)
(define <QuasiQuoted>
	(catenPreSexp #\` 'quasiquote)
)

(define <Unquoted>
	(new
		(*parser (char #\,))
		(*parser (char #\@))
		*not-followed-by
		(*parser <sexpr>)
		(*caten 2)
		(*pack-with
			(lambda (pref sx) (list 'unquote sx)))
	done)
	;(catenPreSexp #\, 'unquote)
)


(define <UnquoteAndSpliced>
  (new 
  	  (*parser (word ",@"))
      (*parser <sexpr>)
      (*caten 2)
    (*pack-with
        (lambda (pref sx) (list 'unquote-splicing sx)))
done)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Call by Name ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <CBNameSyntax1>
	(new
		(*parser (char #\@))
		(*parser <sexpr>)
		(*caten 2)
		(*pack-with
			(lambda (pref sx) (list 'cbname sx)))
	done)
)

(define <CBNameSyntax2>
	(new
		(*parser (char #\{ ))
		(*parser <sexpr>)
		(*parser (char #\} ))
		(*caten 3)
		(*pack-with
			(lambda (prefCurli1 sx prefCurli2) (list 'cbname sx)))
	done)
)

(define <CBName>
	(new
		(*parser <CBNameSyntax1>)
		(*parser <CBNameSyntax2>)
		(*disj 2)
	done)
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     INFIX     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define <InfixExpression>
	(new 
		(*delayed (lambda () <InfixAddOrSub>) )
	done)
)

(define <InfixPrefixExtensionPrefix>
	(new 
		(*parser (word "##"))
		(*parser (word "#%"))
		(*disj 2)
	done)	
)

(define <InfixExprComment>
	(new 
		(*parser (word "#;"))
		(*parser <InfixExpression>)
		(*caten 2)
	done)
)

(define <InfixIgnore>
	(new
		(*parser <whiteSpaces>)
		(*parser <comment>)
		(*parser <InfixExprComment>)
		(*disj 3)
		*plus
	done)
	)


(define <InfixExtension>
	(new
		(*parser <InfixPrefixExtensionPrefix>)
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with
			(lambda (pref infExp) infExp))				
	done)		
	)

(define <InfixSymbol>
	(new 
		(*parser <SymbolChar> )
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser (char #\* ))
		(*parser (char #\/) )
		(*parser (char #\^ ))
		(*parser (word "**" ))
		(*disj 6)
		*diff
		*plus
		(*pack (lambda (lst) (string->symbol (list->string lst))))
	done)
)


(define <InfixSexprEscape>
	(new (*parser <InfixPrefixExtensionPrefix>)
		(*parser <sexpr>)
		(*caten 2)
		(*pack-with (lambda (pref sexpr) sexpr))
	done)
)


(define <InfixParen> 
	(new
		(*parser (char #\( ))
		(*parser <InfixExpression>)
		(*parser (char #\) ))
		(*caten 3)
		(*pack-with
			(lambda (openPar expr closePar)
				expr
			))
	done)
)

(define <InfixNeg>
	(new
		(*parser (char #\-))
		(*delayed (lambda() <FuncAndArraysParser>))
		(*caten 2)
		(*pack-with
			(lambda (minus expr) `(- ,expr)))
	done)	
)

(define <InfixArgList>
	(new
		(*parser <InfixExpression>)
		
		(*parser <whiteSpaces>) *maybe
		(*parser (char #\,))
		(*caten 2)
		(*pack-with (lambda (ws x) x))
		(*parser <InfixExpression>)
		(*caten 2)
		(*pack-with (lambda (sep exp) exp))
		*star
		(*caten 2)
		(*pack-with cons)


		(*parser <epsilon>)
		(*disj 2)
		
	done)

)

(define (flatten x)
(cond ((null? x) '())
	  ((pair? x) (append (flatten (car x)) (flatten (cdr x))))
	  (else (list x))))



(define <InfixFuncCall>
	(new
		(*parser <whiteSpaces>) *maybe
		(*parser (word "(" ))
		(*caten 2)
		(*pack-with (lambda (w exp) exp))

		(*parser <InfixArgList>)
		
		(*parser <whiteSpaces>) *maybe
		(*parser (word ")" ))
		(*caten 2)
		(*pack-with (lambda (w exp) exp))

		(*caten 3)
		
		(*pack-with
			(lambda (pr1 args pr2)
			  (lambda (fun) `(,fun ,@args))))
	done)
)

(define <InfixArrayGet>
	(new
		(*parser (char #\[))
		(*parser <InfixExpression>)
		(*parser (char #\]))
		(*caten 3)
		(*pack-with 
			(lambda (penBR exp closeBR) 
				(lambda (arr)
				`(vector-ref ,arr ,exp)))
		)
	done)
)

(define <mulOrDiv>
	(new
		(*parser (char #\*))
		(*pack (lambda(_) '*))
		(*parser (char #\/))
		(*pack (lambda(_) '/))
		(*disj 2)
	done)
)


(define <pow>
	(new
		(*parser (word "**"))
		(*pack (lambda(_) '**))
		(*parser (char #\^))
		(*pack (lambda(_) '^))
		(*disj 2)
	done)
)

(define <subOrAdd>
	(new
		(*parser (char #\+))
		(*pack (lambda(_) '+))
		(*parser (char #\-))
		(*pack (lambda(_) '-))
		(*disj 2)
	done)		
)


(define buildInfixOP
	(lambda (<higherPriorityParser> <op>)
		(new
			(*parser <higherPriorityParser> )			
			(*parser <op>)

			(*parser   <higherPriorityParser> )					
			(*caten 2)
			*star

			(*caten 2)
			(*pack-with
				(lambda (first rest)
				  (fold-left
					(lambda (a x) `(,(car x) ,a ,@(cdr x)))
					first
					rest
					)))

		done)
	)
)





(define buildInfixPow
	(lambda (<higherPriorityParser> <op>)
		(new
			
			(*parser <higherPriorityParser> )
			
			(*parser <op>)
			(*caten 2)
			(*pack-with (lambda (x y) x))
			*star
			(*parser   <higherPriorityParser> )	
			
			(*caten 2)
			(*pack-with
				(lambda (restList lastArg)
				  (fold-right
					(lambda (a x) `( expt ,a ,x))
					lastArg
					restList
					)))

		done)
	)
)


(define <notAnumberInfixSymbol>
	(new
		(*parser <InfixSymbol>)
		(*parser (range #\0 #\9))
		*diff
	done)
)

(define <basicValuesParser>
	(new
		(*parser <InfixIgnore>) *maybe
		(*parser <Char>)

		(*parser <fraction>)
		(*parser <integer>)
		(*disj 2)
		(*parser <notAnumberInfixSymbol>)
		*not-followed-by

		(*parser <InfixSymbol>)
		(*parser <InfixParen>)
		(*parser <InfixSexprEscape>)
		(*parser <InfixNeg>)
		(*disj 6)	
		(*parser <InfixIgnore>) *maybe
		(*caten 3)
		(*pack-with (lambda (ignore1 exp ignore2) exp))
	done)	
)

(define <FuncAndArraysParser>
	(new
		(*parser <basicValuesParser>)
		(*parser <InfixArrayGet>)
		(*parser <InfixFuncCall>)
		(*disj 2)
		(*parser <InfixIgnore>) *maybe
		(*caten 2)
		(*pack-with (lambda (expression ignore) expression))
		*star

		(*caten 2)	
			(*pack-with
				(lambda (first rest)
				  (fold-left
					(lambda (a x) (x a))
					first
					rest)))

	done)	
)

(define <InfixPow>
	(buildInfixPow <FuncAndArraysParser> <pow>)
)

(define <InfixMulOrDiv>
	( buildInfixOP  <InfixPow> <mulOrDiv>)
)

(define <InfixAddOrSub>
	( buildInfixOP <InfixMulOrDiv> <subOrAdd>)
)



(test-string <InfixExpression> "1^2^3^4")
(test-string <comment> ";\"\\x61;\\x63;\"")
(test-string <symbol> "num")
;(test-string <SymbolChar> "1234abcd")
;(test-string <SymbolChar> "%33356")
;(test-string <symbol> "1234abcd")	
;(test-string <booleaPt")
;(test-string <Char> "#t");
;(test-string <stringHexChar> "#\\x23")
;(test-string <Char> "#\\x64")
;(test-string <InfixAddOrSub> "1+1")

