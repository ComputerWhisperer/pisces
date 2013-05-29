cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 13:46:10 PST 1990 (dredge--stanford)
c*comdeck ordtmp
c
c	common ordtmp for re-ordering nodes and elements
c
c       nrd = node order array; nrd(i)  = node which will move to slot i
c       inrd= inverse nrd     ; inrd(i) = where node i will move instead
c       trd = triangle order array
c       rtag = values to sort upon
c       rvl  = temporary resting place for real values
c       jvl  = temporary resting place for integer values
c
        common /tmpco/trd,inrd,rtag

	integer nrd(MAXPT), inrd(MAXPT), trd(MAXTRI), jvl(MAXTRI)
	real    rtag(MAXTRI),rvl(MAXTRI)
        double precision dvl(MAXPT)

	equivalence (nrd(1),trd(1)),(rtag(1),jvl(1)),(rtag(1),rvl(1))
        equivalence (dvl(1),rtag(1))

