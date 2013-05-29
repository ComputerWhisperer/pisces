C ----------------------------------------------------------------------
C   Copyright 1986 by
C   The Board of Trustees of the Leland Stanford Junior University
C   All rights reserved.
C 
C   This routine may not be used without the prior written consent of
C   the Board of Trustees of the Leland Stanford University.
C ----------------------------------------------------------------------
c
c "p1axis": Sedan-III plot axis routine.
c
c Calling sequence:
c	 call P1AXIS(titl, xlog, xlab,
c	+                  ylog, ylab )
c
c Where:
c	titl	- character*(*). Use given title, trailing blanks
c                 stipped.
c	xlog	- integer.  1::logarithmic data (take the log of each
c		  X value before plotting), 0::Linear.
c	xlab	- character*(*) The X axis label.
c	ylog	- integer.  1::logarithmic data (take the log of each
c		  Y value before plotting), 0::Linear.
c	ylab	- character*(*) The Y axis label.
c
c Original: Michael Eldredge -- Stanford (apr 88)
c ================================================================
	SUBROUTINE P1AXIS(titl, xlog, xlab, ylog, ylab)
	character*(*) titl
	integer xlog
	character*(*) xlab
	integer ylog
	character*(*) ylab
c
c ---- Common Declarations ----
        include 'p1com0.h'
c
c ---- Local Variables ---
	integer i
	integer ntics
	real	axmin, axmax, axdel
c
c ---- CONSTANTS that could be changed ----
c  In general, these are the fixed parameters for axplt2().
c  They can be (simply) changed here to other values that will
c  make the axes look more to your liking.
	real	XANG, YANG
	parameter (XANG=0.0, YANG=90.0)
	real	XTLANG, YTLANG
	parameter (XTLANG=0.0, YTLANG= -90.0)
	integer XTLOC, YTLOC
	parameter (XTLOC = 0, YTLOC = 1)
	real	XTANG, YTANG
	parameter (XTANG=90.0, YTANG=90.0)
	integer XLFORM, YLFORM
	parameter (XLFORM= -1, YLFORM=1)
c...liniear(I) and Log(R)
	integer	IUTIC, RUTIC
	parameter (IUTIC = 3, RUTIC = -1)
	integer LEXPND
	parameter (LEXPND = 0)
c
c ----------------------------------------------------------------
c ---- START ----
c...Find length of user given title.
	i = LEN(titl)
	if (titl(i:i).ne.' ') goto 21
	do 20 i = LEN(titl), 1, -1
	  if (titl(i:i).ne.' ') goto 21
20	continue
21	continue

c
c...The user given title, name and version:
	call fsymb2(xsize-0.8*titlsz, ysize-2.5*titlsz,
     +		titl(1:i), i, 0.8*titlsz, 0.8*titlsz, 0.0,
     +		0.00, 1.00)
	call fsymb2(titlsz, ysize-2.5*titlsz,
     +		header, LEN(header), titlsz, titlsz, 0.0,
     +		0.00, 0.00)
	call fsymb2(titlsz*(1+LEN(header)+0.70), ysize-2.5*titlsz,
     +		hdvers, LEN(hdvers), 0.70*titlsz, 0.70*titlsz, 0.0,
     +		0.00, 0.00)

c
c...get some nice tic spacings...
	call faxtc2(vxmin, vxmax, LEXPND, xlog,
     +		    axmin, axmax, axdel, ntics)
c
c...draw the X axis
	call faxpt2(xaorg, yaorg, xalen, XANG,
     +		vxmin, vxmax,  axmin, axmax, axdel, ntics,
     +		TLHITE, XTLANG, XTLOC, XTANG, 0.0, THITE,
     +		xlab, LHITE, XLFORM, 'g')

c
c...get some nice tic spacings...
	call faxtc2(vymin, vymax, LEXPND, ylog,
     +		    axmin, axmax, axdel, ntics)
c
c...draw the Y axis
	call faxpt2(xaorg, yaorg, yalen, YANG,
     +		vymin, vymax,  axmin, axmax, axdel, ntics,
     +		TLHITE, YTLANG, YTLOC, YTANG, THITE, 0.0,
     +		ylab, LHITE, YLFORM, 'g')

c
c...close 'da box...
	call fmove(xaorg        , yaorg + yalen)
	call fdraw(xaorg + xalen, yaorg + yalen)
	call fdraw(xaorg + xalen, yaorg        )

	return
	end
C
C =================================================================
	BLOCK DATA P1BLK0
c
        include 'p1com0.h'
c
	data  header/'SEDAN-III'/
	data  hdvers/'8820'/
	data  symbsz/0.2/
c
	end
