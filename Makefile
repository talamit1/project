dependencies:= (load "sexpr-parser.scm") (load "tag-parser.scm") (load "semantic-analyzer.scm") (load "compiler.scm")




%:

	echo '$(dependencies) (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q 
	nasm -g -f elf64 $(MAKECMDGOALS).s -o $(MAKECMDGOALS).o
	gcc $(MAKECMDGOALS).o -o $(MAKECMDGOALS)


#tell make that "clean" is not a file name!
.PHONY: clean

#Clean the build directory
clean:
	rm -f *.o foo.s foo