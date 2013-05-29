c
c
	integer PSIZE, XSIZE, YSIZE, ZSIZE
	parameter (PSIZE =  8)
	parameter (XSIZE =  6)
	parameter (YSIZE =  3)
	parameter (ZSIZE =  3)
c
	character*PSIZE xnames(XSIZE)
c........ '1234567890'
	data xnames/
     +    'depth     ',
     +    'time      ',
     +    'Vdd       ',
     +    'Vce       ',
     +    'Vbe       ',
     +    'Vgs       '
     +    /
c
	character*PSIZE xnames(YSIZE)
c........ '1234567890'
	data ynames/
     +    'concentrat',
     +    'netc      ',
     +    'charge    '
     +    /
c
	character*PSIZE xnames(ZSIZE)
c........ '1234567890'
	data znames/
     +    'Vgs       ',
     +    'Vbe       ',
     +    'Vsb       '
     +    /
