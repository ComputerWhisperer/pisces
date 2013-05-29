cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:38:56 PST 1990 (dredge--stanford)
c*comdeck soltmpg
c
c                    Temporary solve info
c			   GUMMEL
c
c
c..........Dimensions :
c
c		np	= 3000
c		a block = 6*np  =18000
c		lu      = 4*a   =72000
c
c...Note : rhs,deltx,x, MUST come first
c
      common /tmpco/ rhs,x,deltx,di,a,l,u

      double precision rhs(MAXPT)
      double precision x(MAXEQN)
      double precision deltx(MAXPT)
      double precision di(MAXPT)
      double precision a(MAXAG)

c...Equivalence l and u to lower and upper halves of luspec
      double precision l(MAXLUG),u(MAXLUG)

c...Equivalence for Gummel damping - done during update, so 
c...can use the x array for temporary storage
      double precision fv0(MAXPT), fn0(MAXPT), fp0(MAXPT)
      equivalence  (fv0,x(1)),
     +             (fn0,x(MAXPT1)),
     +             (fp0,x(MAXPT2))



c...(The following does not even try to be economical, because Gummel
c... uses so much less than the Newton methods. For a start, aw can
c... be overlayed with the direction vectors [t,p,ap,q,aq] since
c... they are needed at phase 1 and phase 2 of the ICCG, respectively. 
c... In fact, aw could be entirely dispensed with by massaging 
c... subroutine ICP1 (the Cholesky decomposition) a little.
c...)

      common /tmpco/ cia,cja,aw,ilsh,resid,p,ap,qinvr,aqinvr
      double precision aw(MAXAG)
      double precision resid(MAXPT)
      double precision p(MAXPT),ap(MAXPT),qinvr(MAXPT),aqinvr(MAXPT)
      integer        cja(MAXAG)
      integer          cia(MAXPT)
      integer          ilsh(MAXPT)

