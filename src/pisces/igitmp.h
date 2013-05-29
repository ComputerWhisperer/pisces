cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 13:32:59 PST 1990 (dredge--stanford)
c*comdeck igitmp
c
c                   Common for setting up information to create an
c                   IGGI input file
c
      common /tmpco/irb,ipleng,igginn,maxigr
      integer irb(MAXCNT,MAXPT),ipleng(MAXCNT),igginn(MAXPT)
      integer maxigr

