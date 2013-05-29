cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:57:41 PST 1990 (dredge--stanford)
c*comdeck soltmpn
c 
c                          temporary solve info
c                              FULL NEWTON
c
c        rhs, deltx, and x MUST come first
c
      common /tmpco/ rhs,deltx,x,rhs0,dll,duu,di,a
c 
      double precision a(MAXADJ)
      real l(MAXLU2),u(MAXLU2)
      double precision dll(MAXLU), duu(MAXLU)
      double precision rhs(MAXEQN),deltx(MAXEQN),rhs0(MAXEQN)
      double precision di(MAXEQN),x(MAXEQN)
      equivalence (l,dll), (u,duu)
c 
c...Temporary arrays used for damping - done on update
c...(after LU, etc. - don't need x array, can use for temp. storage)
      double precision fv0(MAXPT),fn0(MAXPT),fp0(MAXPT)
      equivalence (x(1),fv0),(x(MAXPT1),fn0),(x(MAXPT2),fp0)
c
c...Arrays for ac analysis.  If SOR, overwrite a.  
c...For GCR, must add more storage to emaco (not implemented)
      common /tmpco/ ssrhs, fvss, fnss, fpss
      double precision fvss(MAXPT,2),fnss(MAXPT,2),fpss(MAXPT,2)
      double precision ssrhs(MAXEQ2)
      integer ip2t(MAXNB,MAXPT),ip2tc(MAXPT)
      equivalence (ip2tc,a),(ip2t,a(MAXPT1))
c
c...SOR
      common /tmpco/ x1sor, x2sor, x3sor
      double precision x1sor(MAXEQ2),x2sor(MAXEQ2),x3sor(MAXEQ2)
c
c...GCR dummies (not implemented - must NOT overwrite a)
c      double precision bbr(1),bbap(1,5),bbp(1,5),bbqr(1),bbaqr(1)
c      equivalence (bbr,a),(bbap,a),(bbp,a),(bbqr,a),(bbaqr,a)
c
