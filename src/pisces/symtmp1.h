cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:09:26 PST 1990 (dredge--stanford)
c*comdeck symtmp1
c                   temporary symbolic info #1  
c                 (pivot,smblc,unblk,fini,vsrt4)
c
         integer MAXEQ1,MAXEQ21
      parameter (MAXEQ1 = MAXEQN+1)
      parameter (MAXEQ21= MAXEQ2+1)


      common /tmpco/ fip,jp,jp1,len,na
c 
      INTeger len(MAXMAP),na(MAXIA),ipc0(MAXEQN)
      integer jp(MAXMAP),fip(MAXMAP),jp1(MAXEQ1)
      equivalence (ipc0,fip)
c
c.VMSP says that kp and ixb can be (MAXEQN+1)/2 ...
c
      integer kp(MAXEQN),ixt(MAXEQN),ixb(MAXEQN)
      equivalence (kp,fip),(ixt,fip(MAXEQ1)),(ixb,fip(MAXEQ21))
c
c NOTE: Variables declared as "INTeger" can be either integer*2
c    or integer.  In previous releases they have been integer*2
c    to save space.  But on some systems it is either not
c    allowed or presents a large performance penalty.  The
c    simplest way to redefine them is by adding a
c    -DINTeger='integer*2' to the cpp options.  (dredge -- mar
c    90)
