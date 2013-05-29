cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 15:08:24 PST 1990 (dredge--stanford)
c*comdeck bitmp
c
c Temporary common for SUPREM-IV, when it is available
c
      common /tmpco/ oxtop,oxbot,nc,itmp2,ctmpx,ctmpy,totcon
      real oxtop(MAXCON),oxbot(MAXCON),nc(MAXPT),totcon(MAXPT),
     +     ctmpx(MAXPT),ctmpy(MAXPT)
      integer itmp2(MAXPT)
