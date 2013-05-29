cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 12:56:52 PST 1990 (dredge--stanford)
c*comdeck extract
c
c......Common for parameter extraction routines.
c
      common /tmpco/ scrat,exmin,exmax,eymin,eymax,
     +        emode,iuelc,iureg,lopen
      logical lopen
      integer emode,iuelc,iureg
      real    exmin,exmax,eymin,eymax
      double precision scrat(MAXPT)
