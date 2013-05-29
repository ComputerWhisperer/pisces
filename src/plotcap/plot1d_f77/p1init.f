C ----------------------------------------------------------------------
C   Copyright 1986 by
C   The Board of Trustees of the Leland Stanford Junior University
C   All rights reserved.
C 
C   This routine may not be used without the prior written consent of
C   the Board of Trustees of the Leland Stanford University.
C ----------------------------------------------------------------------
c
c "p1init": Setup sizes etc for subsequent p1axis() and p1data() calls
c
c Calling sequence:
c	call P1INIT(xmin, ymin, xmax, ymax)
c
c Where:
c       xmin	-  Minimum value to be displayed on the X axis
c       ymin	-  Minimum value to be displayed on the Y axis
c       xmax	-  Maximum value to be displayed on the X axis
c       ymax	-  Maximum value to be displayed on the Y axis
c
c Notes:
c	+ This MUST be called before any calls to p1axis() or p1data().
c
c Original: Michael Eldredge -- Stanford (jul 88)
c ======================================================================
	SUBROUTINE P1INIT(xmin, ymin, xmax, ymax)
	real  xmin, ymin
	real  xmax, ymax
c
c ---- Need the gplot PARAMETER definitions ----
        include 'gplotF77.inc'
c
c ---- Common Declarations ----
        include 'p1com0.h'
c
c ---- Local Variables ----
	real    fv(10)
	integer iv(10)
	real	avlen
	integer void
c
c ---- Functions ----
	integer	fpgeti
c
c ---------------------------------------------------------------------


C......SET UP DEVICE.....
c...Get the full size of the plot device.
	void = fpgeti(QPSIZE, iv, fv)

	xsize = fv(1)
	ysize = fv(2)
	avlen = (xsize + ysize) / 2.0
	avlen = AMIN1(xsize, ysize)

c... setup some parameters accordingly
	xaorg = 0.20 * xsize
	yaorg = 0.15 * ysize
	xalen = (1.0-0.20-0.10) * xsize
	yalen = (1.0-0.15-0.15) * ysize

	thite = 0.015 * avlen
	tlhite= 0.020 * avlen
	lhite = 0.025 * avlen
	titlsz= 0.030 * avlen

	symbsz= 0.025 * avlen

c................................
	vxmin = xmin
	vxmax = xmax
	vymin = ymin
	vymax = ymax

c... Done
	return
	end
