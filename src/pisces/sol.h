cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon May 20 23:45:03 PDT 1991 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c*comdeck sol
c-------------------------------------------------------------
c 
c     common for solution tolerances, counters, and modes 
c 
      common /solco/ ptolg,ctolg,ptolx,ctolx,dvlmt,
     +       gvmax,gnmax,gpmax,lucrit,ddamp,lu1cri,lu2cri,
     +       cupdt,dvlmt1,dvlmt2,flcri,
     +       dvmax,dnmax,dpmax,dvmax0,dnmax0,dpmax0,acontn,
     +       ctolx0,nfact,amp,namp,oamp,
     +       kdamp,mudamp,fdamp,pdamp,gvmax0,gnmax0,gpmax0,
     +       dflux,dflux0,deltf,res00,pscal,
     +       delt,delt0,stime,stime0,tstop,timtol,dtmin,
     +       tgam,tgam0,tgam1,tgam2,tgame,tramp0,tramp1
      common /solco/ acstrt,acstop,acstep,nbias,pbias
      common /solco/ iterp,iterc,iconv,itlmt,itmode,maxinn,gloops,
     +       mxiccg,ninner,ldamp,nback,nrloop,ncarr0
      common /solco/ lcconv,lmultp,lsolwr,laccel,lpconv,ldjr,luauto,
     +       lscl,icgflg,ltdep,lprntj,ldbug2,lfixqf,lgnorm,lxnorm,
     +       lcontn,lrstrt,ltauto,l2nd,l2bdf,ltrule,lsvrhs,l2norm,
     +       lexqf
c
      logical lcconv,lmultp,lsolwr,laccel,lpconv,ldjr,luauto,lscl,
     +       icgflg,ltdep,lprntj,ldbug2,lfixqf,lgnorm,lxnorm,lcontn,
     +       lrstrt,ltauto,l2nd,l2bdf,ltrule,lsvrhs,l2norm,lexqf
      integer iterp,iterc,iconv,itlmt,itmode,maxinn,gloops,mxiccg,
     +       ninner,ldamp,nback,nrloop,ncarr0
      real acstrt,acstop,acstep,nbias,pbias
      double precision ptolg,ctolg,ptolx,ctolx,dvlmt,
     +       gvmax,gnmax,gpmax,lucrit,ddamp,lu1cri,lu2cri,
     +       cupdt,dvlmt1,dvlmt2,flcri,
     +       dvmax,dnmax,dpmax,dvmax0,dnmax0,dpmax0,acontn,
     +       ctolx0,nfact,amp(MAXCNT),namp(MAXCNT,2),oamp(MAXCNT),
     +       kdamp,mudamp,fdamp,pdamp,gvmax0,gnmax0,gpmax0,
     +       dflux(MAXCNT),dflux0(MAXCNT),deltf,res00,pscal,
     +       delt,delt0,stime,stime0,tstop,timtol,dtmin,
     +       tgam,tgam0,tgam1,tgam2,tgame,tramp0,tramp1
c
      common /solcoc/ namsol
      character*20 namsol
c
c..used only be solver (to get the name) and solgummel & solnewton during
c...debug to call dumpfl().
c      common /solcoc/ namafl,namerr
c      character*20 namafl,namerr
c
