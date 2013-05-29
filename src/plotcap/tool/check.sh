: ' date: 07 jan 86 (mje)  See if the named libs exist...'
: 'calling seq:  first.sh name libs... '

NAME=$1	; shift
PROG=$1 ; shift
LIBS="$*"

if test ! -f first.o ; then
	echo 'main() { ; }' > first.c
	cc -c first.c
fi

: ' just let ld look for the libraries -- fake load before running make '
if ld -o first first.o $LIBS >/dev/null 2>&1 ; then
	echo "	Looks like you have $NAME"
	echo "#define DO_$NAME"		>> Makefile.cnf
else
	echo "	$NAME not found."
	echo "#undef DO_$NAME"		>> Makefile.cnf
fi
exit 0
