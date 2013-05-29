cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c Thu Mar  8 13:36:00 PST 1990 (dredge--stanford)
c*comdeck monte
c-------------------------------------------------------------
c 
c     common for monte carlo interface(monte common)
c 
      common /monte/ alphmc
c 
      double precision alphmc(3,MAXPT)
