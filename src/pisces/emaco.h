cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 11:33:47 PST 1990 (dredge--stanford)

c*comdeck emaco
c     common for large arrays.  temporary areas are overlayed by other
c     temporary or permanent areas. 
c 
c...Permanent grid info 
      common /emagr/ cord,r1,ndconc,naconc,itype,nop,ehed,jhjd,es,nbc 
      common /emagr/ imat,lm,ietype,mobn,mobp,lmetal,cnie,lgcnie
      common /emagr/ ecode,eptr,dopsgn,taun,taup,nextel,nresis
      common /emagr/ estot,essem,qss,dintf,snintf,spintf
      common /emagr/ dfxpt,dfxpt0
c 
      INTeger itype(MAXPT),nop(3,MAXTRI),nbc(MAXCON)
      INTeger imat(MAXTRI),lm(MAXPT),ietype(MAXCON)
      INTeger eptr(MAXPT),nextel(3,MAXTRI)
      integer ecode(20),nresis(MAXCNT)
      real cord(2,MAXPT),r1(MAXPT),ndconc(MAXPT),naconc(MAXPT)
      real ehed(3,MAXTRI),jhjd(3,MAXTRI),es(3,MAXTRI) 
      real mobn(MAXPT),mobp(MAXPT),lmetal(MAXCON)
      real cnie(MAXPT),tconc(MAXPT),dopsgn(MAXCNT)
      real taun(MAXPT),taup(MAXPT),natot(MAXPT),ndtot(MAXPT)
      real essem(MAXPT)
      real estot(MAXPT),qss(MAXPT),dintf(MAXPT),snintf(MAXPT)
      real spintf(MAXPT)
      double precision dfxpt(MAXCON),dfxpt0(MAXCON),lgcnie(MAXPT)
      equivalence (mobp,tconc),(natot,r1),(ndtot,qss)
c 
c...Permanent solve info
      common /emaso/ fp,fn,fv,ofp,ofn,ofv,qfn,qfp,wsol
c 
      double precision fp(MAXPT),fn(MAXPT),fv(MAXPT)
      double precision qfp(MAXPT),qfn(MAXPT)
      double precision ofp(MAXPT),ofn(MAXPT),ofv(MAXPT),wsol(MAXEQN)
      double precision fptr(MAXPT),fntr(MAXPT),fvtr(MAXPT)
c
      equivalence (fvtr,wsol),(fntr,wsol(MAXPT1)),(fptr,wsol(MAXPT2))
c
c...Permanent symbolic info 
      common /emasy/ ja,jl,ju,iva,ivl,ivu,ipc,ipri,ia,il,iu
      common /emasy/ ialump
c 
      INTeger ia(MAXIA),il(MAXILU),iu(MAXILU)
c.. ipc and ipri were dimensioned to:
c...     MAX2EQN =1600 (max node full newton, 2 carrier)
c...     MAX2EQ3=3*MAX2EQN
      INTeger ipc(MAXEQN),ipri(MAXEQN)
      integer ja(MAXEQN+1),jl(MAXEQN+1),ju(MAXEQN+1),iva(MAXEQN+1)
      integer ivl(MAXEQN+1),ivu(MAXEQN+1)
      integer jar(MAXEQN+1),ialump(MAXCNT)
      INTeger iar(MAXADJ)
      equivalence (jar,jl),(iar,il)
c 
c
c NOTE: Variables declared as "INTeger" can be either integer*2
c    or integer.  In previous releases they have been integer*2
c    to save space.  But on some systems it is either not
c    allowed or presents a large performance penalty.  The
c    simplest way to redefine them is by adding a
c    -DINTeger='integer*2' to the cpp options.  (dredge -- mar
c    90)
