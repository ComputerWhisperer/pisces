cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 11 03:26:16 PDT 1990 (anderson--stanford)
c*comdeck setup
c-------------------------------------------------------------
c 
c     common for setup of initial guesses 
c 
      common /setco/ dktq,dqkt,dni,djcoef,decoef,dnisq,dnii,dniisq,
     +     bias,obias,vstep,rcmax,rcmax2,dni3,depsc,
     +     cresis,ccapac,vres,vres0,
     +     dcscl,dcscli,logni0,barla,barlb,
     +     etrapp,etrapn
      common /setco/ temp,vsn,vsp,
     +     epsmat,semmat,workf,egap,boltzk,
     +     qcharg,ktq,qkt,eps0,
     +     ngap,pgap,
     +     ssdata,gcstar,gvstar
      common /setco/ nsteps,mattyp,nspar,nintrf,nintmx
      common /setco/ lcntnu,linit,lprev,lproj,lsol1,lsol2,lboltz,llogj,
     +        local,lsrh,lauger,lbgn,lconmb,lfldmb,lpjerr,lconlt,lconm2,
     +        lsolst,lscale,lunkwn,lelect,lsetpr,
     +        ldiff,ldcur,schotk,lresis,lcnres,
     +        llogac,lflow,lcurbc,
     +        lincom,lbarl,
     +        lsrfmb, luser1

c 
      logical lcntnu,linit,lprev,lproj,lsol1,lsol2,lboltz,llogj,local,
     +        lsrh,lauger,lbgn,lconmb,lfldmb,lpjerr,lconlt,lconm2,
     +        lsolst,lscale,lunkwn,lelect(MAXCNT),lsetpr,
     +        ldiff,ldcur,schotk(MAXCNT),lresis(MAXCNT),lcnres(MAXCNT),
     +        llogac,lflow,lcurbc(MAXCNT),
     +        lincom,lbarl(MAXCNT),
     +        lsrfmb, luser1

      integer nsteps,mattyp(MAXREG),nspar,nintrf,nintmx

      real temp,vsn(MAXCNT),vsp(MAXCNT),
     +     epsmat(MAXREG),semmat(MAXPAR),workf(MAXCNT),egap,boltzk,
     +     qcharg,ktq,qkt,eps0,stype,epsoq,fni,fmun0,vsat,taup0,taun0,
     +     eg300,egalph,egbeta,affin,vsurfp,vsurfn,ngap,pgap,
     +     fmup0,cnau,cpau,arichn,arichp,ncband,nvband,nc300,nv300,
     +     muco1,muco2,muco3,muco4,etrap,gvband,gcband,edband,
     +     eaband,nsrhn,nsrhp,mudeg,ssdata(MAXINF,7),gcstar,gvstar,
     +     MU1N,MU2N,MU1P,MU2P

      double precision dktq,dqkt,dni,djcoef,decoef,dnisq,dnii,dniisq,
     +     bias(MAXCNT),obias(MAXCNT),vstep,rcmax,rcmax2,dni3,depsc,
     +     cresis(MAXCNT),ccapac(MAXCNT),vres(MAXCNT),vres0(MAXCNT),
     +     jbias(MAXCNT),
     +     dcscl,dcscli,logni0,barla(MAXCNT),barlb(MAXCNT),
     +     etrapp,etrapn
c
      equivalence (bias,jbias)
      equivalence (semmat(1),stype),(semmat(2),epsoq),(semmat(3),fni),
     +            (semmat(4),fmun0),(semmat(5),vsat),(semmat(6),taup0), 
     +            (semmat(7),taun0),(semmat(8),mudeg),(semmat(9),eg300),
     +            (semmat(10),egalph),(semmat(11),egbeta),
     +            (semmat(12),affin),(semmat(13),fmup0),
     +            (semmat(14),cnau),(semmat(15),cpau),
     +            (semmat(16),arichn),(semmat(17),arichp),
     +            (semmat(18),nc300),(semmat(19),nv300),
     +            (semmat(20),vsurfn),(semmat(21),vsurfp),
     +            (semmat(22),muco1),(semmat(23),muco2),
     +            (semmat(24),muco3),(semmat(25),muco4),
     +            (semmat(26),ncband),(semmat(27),nvband),
     +            (semmat(28),nsrhn),(semmat(29),nsrhp),
     +            (semmat(30),etrap),
     +            (semmat(32),gcband),(semmat(33),gvband),
     +            (semmat(34),edband),(semmat(35),eaband),
     +            (semmat(36),MU1N),(semmat(37),MU2N),
     +            (semmat(38),MU1P),(semmat(39),MU2P)
c 
