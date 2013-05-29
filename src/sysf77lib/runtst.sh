#!/bin/sh
: 'Note: if your system cannot handle #!/bin/sh, just remove the'
: ' line above'

: 'Run the xtest program in a "standard" manner'

: 'need the following environment variable defined'
MATCH=somthing
export MATCH

: 'run the test program with the "expected" input'
./xtest aaaa bbb cc d
status=$?

echo '
     One more test.  We will check the program exit status.
     This should return "ok":
'

if test $status ; then
	echo ' Exit status OK'
else
	echo ' Exit status BAD'
fi
echo ' '
