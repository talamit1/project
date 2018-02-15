cd $1
awk '{f = $1 "_test" NR".scm"; print $0 > f; close(f)}' $1".scm"
rm $1 ".scm"