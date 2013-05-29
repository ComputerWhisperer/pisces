cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
c*comdeck blank
c-------------------------------------------------------------

c 
c     common for primary global information (blank common)
c 
      common /blank/ nx,ny,np,ne,nb,neq,neqp1,nelect,nmat,
     +          ncarr,nmult,nxymax,nlump,neq0,nepb,ndop,ndreg,
     +          linum,
     +          lgumm,l1hole,lnewt,ldebug,lpoiss,lcpu,
     +          errflg, wrnflg, eofflg, isiact,
     +          maxdbl,mindbl,
     +          maxexp,expcut,dcexp,excrit,exci,lgmaxd,lgmind
c 
      integer nx,ny,np,ne,nb,neq,neqp1,nelect,nxymax,nlump,nepb
      integer nmat,ncarr,nmult,ndop,ndreg,neq0
      logical lgumm,l1hole,lnewt,ldebug,lpoiss,lcpu
      double precision maxdbl,mindbl,maxexp,dcexp,excrit,exci,expcut
      double precision lgmaxd,lgmind
c
      logical  errflg, wrnflg, eofflg
      integer  linum
      logical  isiact
c
      common /blankc/ ititle,idaytm
      character*60 ititle
      character*20 idaytm
