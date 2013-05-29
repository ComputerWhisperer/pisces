cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
c*comdeck doptmp
c
c common for temporary doping information
c
      common /tmpco/ tsides,dopbuf,matbi,nxbi,nybi,npbi,itbi
c
      real tsides(3,MAXTRI)
      real dopbuf(MAX2DOP,3)
      INTeger itbi(MAX2DOP)
      integer nxbi,nybi,npbi,matbi(MAXCNT)
c
      real consup(MAXSUP),ysup(MAXSUP)
      real xbi(MAX2DOP),ybi(MAX2DOP),r1bi(MAX2DOP)
c
      equivalence (consup,dopbuf(1,1)),(ysup,dopbuf(1,2))
      equivalence (xbi,dopbuf(1,1)),(ybi,dopbuf(1,2))
      equivalence (r1bi,dopbuf(1,3))
c
c NOTE: Variables declared as "INTeger" can be either integer*2
c    or integer.  In previous releases they have been integer*2
c    to save space.  But on some systems it is either not
c    allowed or presents a large performance penalty.  The
c    simplest way to redefine them is by adding a
c    -DINTeger='integer*2' to the cpp options.  (dredge -- mar
c    90)
