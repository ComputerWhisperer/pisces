C ----------------------------------------------------------------------
C   Copyright 1986 by
C   The Board of Trustees of the Leland Stanford Junior University
C   All rights reserved.
C 
C   This routine may not be used without the prior written consent of
C   the Board of Trustees of the Leland Stanford University.
C ----------------------------------------------------------------------
C
c Mon Jul 11 12:11:59 PDT 1988 (dredge-stanford)
c
c "p1data": Plot the given data arrays.
c
c Calling Sequence:
c	call P1DATA(x, y, nxy, xlog, ylog, bylin, bysym, doclip)
c
c Where:
c	x	- real array.  Values to plot along the X-axis.
c	y	- real array.  Values to plot along the Y-axis.
c	nxy	- integer.  Number of points in X,Y
c	xlog	- integer.  1::Take the log of each X-value before
c                 plotting.
c	ylog	- integer.  1::Take the log of each Y-value before
c                 plotting.
c	bylin	- If >= 1 then connect each point with a line of
c		  type 'bylin'.  If == 0 then don't connect points.
c	bysym	- If >= 1 then put symbol 'bysym' at each point.
c		  If == 0 then don't put a symbol at each point.
c	doclip	- If TRUE the clip the data to the axis bounds.
c
c Original: Michael Eldredge -- Stanford (apr 88)
c Modified: Michael Eldredge -- Stanford (jul 88) Added clipping.
c ----------------------------------------------------------------------
	SUBROUTINE P1DATA(X, Y, NXY, XLOG, YLOG, BYLIN, BYSYM, DOCLIP)
	real	x(*), y(*)
	integer nxy
	integer xlog, ylog
	integer bylin, bysym
	logical doclip
c
c ---- Common Declarations ----
        include 'p1com0.h'
c ---- GPLOT constants ----
	integer GxCLIPL, GxCLIPH, GxRESET
	parameter (GxCLIPL=31)
	parameter (GxCLIPH=32)
	parameter (GxRESET=42)
c
c ---- Local variables ----
	character sym
	integer i
	real    xv, yv
	real	xdelta, ydelta
	real	sh, sw
	logical xislog, yislog
c...local copies that may have been log10()ed
	real	vxminl, vyminl
	real	vxmaxl, vymaxl
	real	xclipl, yclipl
	real	xcliph, ycliph
	integer iv(10)
	real    fv(10)
c
c ---------------------------------------
c ---- START ----
c
	xislog = (xlog.ne.0)
	yislog = (ylog.ne.0)
c
	if (xislog) then
		vxmaxl = alog10(vxmax)
		vxminl = alog10(vxmin)
		xdelta = alog10(vxmax) - vxminl
		xv = alog10(x(1))
	else
		vxmaxl = vxmax
		vxminl = vxmin
		xdelta = (vxmax - vxmin)
		xv = x(1)
	endif
	if (yislog) then
		vymaxl = alog10(vymax)
		vyminl = alog10(vymin)
		ydelta = alog10(vymax) - vyminl
		yv = alog10(y(1))
	else
		vymaxl = vymax
		vyminl = vymin
		ydelta = (vymax - vymin)
		yv = y(1)
	endif
c
	if (bysym.gt.0) then
	    sym = CHAR(bysym)
c...        ...Setup scaled symbol sizes.
	    sw = symbsz * xdelta/xalen
	    sh = symbsz * ydelta/yalen
	endif
	if (bylin.gt.0) call fnline(bylin)
c
c...Get into value units
	call ftrans(xaorg, yaorg)
	call fscale(xalen/xdelta, yalen/ydelta)
	call ftrans(-vxminl, -vyminl)
c...Set clipping (if need be).
	if (doclip) then
c.........Set to our (new) units.
	    call fclipl(vxminl, vyminl, 0)
	    call fcliph(vxmaxl, vymaxl, 0)
	endif
c
	call fmove(xv, yv)
	do 100 i = 1, nxy
	    xv = x(i)
	    yv = y(i)
	    if (xislog) then
		if (xv.gt.0.0) xv = ALOG10(xv)
	    endif
	    if (yislog) then
		if (yv.gt.0.0) yv = ALOG10(yv)
	    endif
c
	    if (bylin.gt.0) then
		call fdraw(xv, yv)
	    else
		call fmove(xv, yv)
	    endif
	    if (bysym.gt.0) then
		if (bylin.ne.1) call fnline(1)
      		call fsymb2(xv, yv, sym,-1, sh, sw, 0.0, .5,.5)
		if (bylin.ne.1) call fnline(bylin)
	    endif
100	continue
c
c...restore clip values (if we changed them)
	if (doclip) then
	    call fplot2(GxRESET, GxCLIPL, 0.0, 0.0)
	    call fplot2(GxRESET, GxCLIPH, 0.0, 0.0)
	endif
c...reset transformations.
	call ftrans(vxminl, vyminl)
	call fscale(xdelta/xalen, ydelta/yalen)
	call ftrans(-xaorg, -yaorg)
c
	if (bylin.gt.0) call fnline(1)
	return
	end
