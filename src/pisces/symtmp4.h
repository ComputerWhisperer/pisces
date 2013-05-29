cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:17:11 PST 1990 (dredge--stanford)
c*comdeck symtmp4
c
c temp common for row ordering
c
      common/tmpco/cia,cja,cja1,cja2,cja3
      integer cia(MAXEQN),cja1(MAXCJA),cja2(MAXCJA),cja3(MAXCJA)
      INTeger cja(MAXCJA)
