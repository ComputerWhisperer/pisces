C NOTE:
C  1. This is the ratfor/ f77 version of the gplot include file
C     and is supposed to be the same but for ratfor includes.
C  2. All underscores from the C form have been removed, thus:
C     Q_DRAW has become QDRAW
C  3. The macro functions have been commented out.  These functions
C     have been implemented as fortran subroutines and are
C     available in the library.
C  4. This file was generated from `gplot.h' (the C form) by a 
C     program, and as such is not so `pretty'. Sorry.

C *----------------------------------------------------------------------
C *  Copyright 1986 by
C *  The Board of Trustees of the Leland Stanford Junior University
C *  All rights reserved.
C *
C *  This routine may not be used without the prior written consent of
C *  the Board of Trustees of the Leland Stanford University.
C *----------------------------------------------------------------------
C 


C  date: 10 feb 87 (mje)
C  "gplot.h" : definitions for calling 'gplot' routines.
C  written:  Michael Eldredge (nov 84)
C  modified: MJE (may 86) added REVCO call.
C  modified: MJE (dec 86) 
C 	+ added PDEV, PFIL stuff and defs for setpfil(), etc..
C 	+ moved gtoa & atog to NOT_SAVED.
C  modified: MJE (feb 87) added greset().
C

C * the following few have historical numbers (from 'tplot' days) 
	integer QCLEAR
	parameter (QCLEAR = 1)
	integer QDRAW
	parameter (QDRAW = 2)
	integer QMOVE
	parameter (QMOVE = 3)

	integer QSCALE
	parameter (QSCALE = 11)
	integer QTRANS
	parameter (QTRANS = 13)
	integer QROTATE
	parameter (QROTATE = 15)
	integer QANGLE
	parameter (QANGLE = 17)

C * stay out of the 20s for historical reasons, some old tplot stuff used them 
	integer QCLIPL
	parameter (QCLIPL = 31)
	integer QCLIPH
	parameter (QCLIPH = 32)
	integer QLINE
	parameter (QLINE = 33)
	integer QPEN
	parameter (QPEN = 34)
	integer QUSR1
	parameter (QUSR1 = 35)
	integer QUSR2
	parameter (QUSR2 = 36)
	integer QDMODE
	parameter (QDMODE = 37)
	integer QAREA
	parameter (QAREA = 38)
	integer QFILS
	parameter (QFILS = 39)
	integer QRESET
	parameter (QRESET = 42)

	integer QMARK
	parameter (QMARK = 98)
	integer QPEND
	parameter (QPEND = 99)

C * These commands (>= Q_NOT_SAVED) never get saved but are useful 
	integer QNOTSAVED
	parameter (QNOTSAVED = 100)
	integer QNULL
	parameter (QNULL = 100)
	integer QASAVE
	parameter (QASAVE = 101)
	integer QPSIZE
	parameter (QPSIZE = 102)
	integer QCLOC
	parameter (QCLOC = 103)
	integer QDABLE
	parameter (QDABLE = 104)
	integer QREVCO
	parameter (QREVCO = 105)
	integer QPDEV
	parameter (QPDEV = 106)
	integer QPFIL
	parameter (QPFIL = 107)
	integer QATOG
	parameter (QATOG = 108)
	integer QGTOA
	parameter (QGTOA = 109)
	integer QXMAT
	parameter (QXMAT = 110)

C  Sub-commands : 
C    For setting clipping, reset to original values, clip on physical or 
C    on logical axis
C
	integer QONLOG
	parameter (QONLOG = 0)
	integer QONPHS
	parameter (QONPHS = 1)
	integer QDOBREAK
	parameter (QDOBREAK = 10)
	integer QNOBREAK
	parameter (QNOBREAK = 11)

C Begin, stop, toggle on & off of some functions (like auto-save) 
	integer QBEGIN
	parameter (QBEGIN = 0)
	integer QON
	parameter (QON = 1)
	integer QOFF
	parameter (QOFF = 2)
	integer QSTOP
	parameter (QSTOP = 3)

	integer QSET
	parameter (QSET = 1)
	integer QGET
	parameter (QGET = 2)
	integer QMUL
	parameter (QMUL = 3)

C * drawing mode sub-commands 
	integer QMSET
	parameter (QMSET = 0)
	integer QMCLR
	parameter (QMCLR = 1)
	integer QMCMP
	parameter (QMCMP = 2)

C * gpopen() mode values. -- incase anyone needs them 
C	integer QPORD
C	parameter (QPORD = 0)
C	integer QPOWR
C	parameter (QPOWR = 0)
C	integer QPORW
C	parameter (QPORW = (QPO_RD|QPO_WR))
C	integer QPOAP
C	parameter (QPOAP = 0)
C	integer QPOHD
C	parameter (QPOHD = 0)

C * some useful macros to make use of some functions a little simpler 
Cdefine setpdev(S)	gpmisc(Q_PDEV , Q_SET , (int*)0, (float*)0, S)
Cdefine setpfil(S)	gpmisc(Q_PFIL , Q_SET , (int*)0, (float*)0, S)
Cdefine setsfil(S)	gpmisc(Q_ASAVE, Q_SET , (int*)0, (float*)0, S)
Cdefine gpgeti(C,IV,FV)	gpmisc(C      , Q_QET , IV     , FV	  , "")
Cdefine gpseti(C,IV,FV)	gpmisc(C      , Q_SET , IV     , FV	  , "")


Cdefine gclear()       gplot2(Q_CLEAR, 0, 0.,0.)
Cdefine gpend()        gplot2(Q_PEND , 0, 0.,0.)
Cdefine gplot(X,Y,P)   gplot2(P, 0, X, Y)
Cdefine gmove(X,Y)     gplot2(Q_MOVE , 0, X , Y)
Cdefine gdraw(X,Y)     gplot2(Q_DRAW , 0, X , Y)
Cdefine gscale(X,Y)    gplot2(Q_SCALE, 0, X,Y)
Cdefine gtrans(X,Y)    gplot2(Q_TRANS, 0, X,Y)
Cdefine grotat(X,Y,A) (gplot2(Q_ROTATE,0,X,Y),gplot2(Q_ANGLE,0,A,0.))

Cdefine gnline(N)     gplot2(Q_LINE , N ,0.,0.)	/* line style */
Cdefine gnpen(N)      gplot2(Q_PEN  , N ,0.,0.)	/* pen size */
Cdefine gnfill(N)     gplot2(Q_FILS , N ,0.,0.)	/* fill pattern */
Cdefine garea(X,Y,H)  gplot2(Q_AREA , H ,X ,Y )
Cdefine gclipl(X,Y,H) gplot2(Q_CLIPL, H, X ,Y )
Cdefine gcliph(X,Y,H) gplot2(Q_CLIPH, H, X ,Y )

Cdefine greset(C)     gplot2(Q_RESET, C, 0., 0.) /*reset something to default*/

C * All users should make use of ggtoa() calls (e.g. before prompting)
C   but need not call gatog() since this transition is handled automatically
C   by gplot.
Cdefine gatog()	      gplot2(Q_ATOG , 0 ,0.,0.)
Cdefine ggtoa()	      gplot2(Q_QTOA , 0 ,0.,0.)
Cdefine gpost()	      gplot2(Q_QTOA , 0 ,0.,0.)

