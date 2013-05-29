cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c*comdeck symb
c-------------------------------------------------------------
c 
c     common for symbolic factorization 
c 
      common /symco/ npdim,nedim,nbdim,iadim,ildim,iudim,nadim,
     +       adim,ludim,ipcdim,ixbdim,mapdim,
     +       iasiz,ilsiz,iusiz,nasiz,asiz,lsiz,usiz,mapsiz,ludimg,
     +       adimg,mpgum,mp1fn,mp2fn,iydim,iysiz
c 
      integer npdim,nedim,nbdim,iadim,ildim,iudim,nadim,ludimg,
     +        adim,ludim,ipcdim,ixbdim,mapdim,
     +        adimg,iasiz,ilsiz,iusiz,nasiz,asiz,lsiz,usiz,mapsiz,
     +        mpgum,mp1fn,mp2fn,iydim,iysiz
c 
