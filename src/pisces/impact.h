cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 12 16:28:41 PDT 1989 (anderson--stanford)
c*comdeck impact
c-----------------------------------------------------------------------
c 
c     common for carrier-carrier scattering dep. mobility
c            and impact ionization
c
c     remember NOT to mix character and other data within the same
c     data block.
c 
      common /impact/ cc,cd,dfdc,dmucon,dmucop,ea,eb,ha,hb,labbas,
     +       limpct,muln,mulp,kimpct,
     +       rlamde,rlamdh,lcsm,lmont,lbreak,lpath
c 
      double precision  cc,cd,dfdc,dmucon,dmucop
      logical           labbas,limpct,lcsm,lmont,lbreak,lpath
      integer           kimpct
      real              ea,eb,ha,hb,muln,mulp,
     +                  rlamde,rlamdh
c
      common /impctc/ mntnam
c.......optional file from which to read MC output.
      character*20      mntnam
