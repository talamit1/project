dependencies:= (load "sexpr-parser.scm") (load "tag-parser.scm") (load "semantic-analyzer.scm") (load "compiler.scm")




%:

	echo '$(dependencies) (compile-scheme-file "$(MAKECMDGOALS).scm" "$(MAKECMDGOALS).s")' | scheme -q > $(MAKECMDGOALS).s
