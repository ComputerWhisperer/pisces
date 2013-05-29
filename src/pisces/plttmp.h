cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Wed Apr  4 14:26:13 PDT 1990 (dredge--stanford)
c*comdeck plttmp
c 
c                   temporary plotting info 
c
      common /tmpco/ itag,fval,adis,cord1d,sc2,sc3,sc4,sply,splx,
     +               vimag,vix,viy
c 
      integer itag(MAXPLT)
c
      real fval(MAXPLT),adis(MAXPLT),cord1d(2,MAXPLT),rtmp(MAXPLT)
      real sc2(MAXPLT)
      real sc3(MAXPLT),sc4(MAXPLT),sply(MAXPLT),splx(MAXPLT)
      real vimag(MAXPLT)
      double precision vix(MAXPLT),viy(MAXPLT)
c
      equivalence (sc4,rtmp)
c 
