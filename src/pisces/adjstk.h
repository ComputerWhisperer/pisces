cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 12:02:31 PST 1990 (dredge--stanford)
c*comdeck adjstk
c
c Common for stack to get doping-contact connectivity
c
      common /tmpco/ nlist
      INTeger nlist(MAXPT)
c
c NOTE: Variables declared as "INTeger" can be either integer*2
c    or integer.  In previous releases they have been integer*2
c    to save space.  But on some systems it is either not
c    allowed or presents a large performance penalty.  The
c    simplest way to redefine them is by adding a
c    -DINTeger='integer*2' to the cpp options.  (dredge -- mar
c    90)
