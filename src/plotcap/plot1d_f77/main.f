C ----------------------------------------------------------------------
C   Copyright 1986 by
C   The Board of Trustees of the Leland Stanford Junior University
C   All rights reserved.
C 
C   This routine may not be used without the prior written consent of
C   the Board of Trustees of the Leland Stanford University.
C ----------------------------------------------------------------------
C
	program pltest
c
	real	a, b
	real	x(1000), y(1000)
	real	xmin, ymin, xmax, ymax
	integer nxy
	integer ylog, xlog
	logical	doclip
	integer nline, nsymb
	character*40 title
c
c ----------------------------------
	nxy = 1
10	read(5, *, END=20) a,b
	if (nxy.eq.1) then
		xmin = a
		xmax = a
		ymin = b
		ymax = b
	else
		if (a .gt. xmax) xmax = a
		if (a .lt. xmin) xmin = a
		if (b .gt. ymax) ymax = b
		if (b .lt. ymin) ymin = b
	endif
	x(nxy) = a
	y(nxy) = b
	nxy = nxy + 1
	goto 10
c..done (but we counted up by one too many...)
20	nxy = nxy - 1

c................................
	xlog = 0
	ylog = 1
	nline= 3
	nsymb= 0
	doclip=.true.

	title = 'Test run vs. Vds'
c................................

c... Setup the drawing surface
	call fclear

	call p1init(xmin, ymin, xmax*.90, ymax*.90)

c... Draw the axis, now.
	call p1axis(title, xlog, 'This is the X-Axis',
     +		           ylog, 'Y-Axis')


c... Plot the data
	call p1data(x, y, nxy, xlog, ylog, nline, nsymb, doclip)

CC	call p1axis(title, xlog, 'This is the X-Axis',
CC     +	           ylog, 'Y-Axis')

c... Done
	call fpend
	end
c
c
c
	SUBROUTINE ERROR(str)
	character*(*) str
	write(6,10) str
10	format(' Error: ', a)
	return
	end
