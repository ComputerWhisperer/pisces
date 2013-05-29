cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Jun 26 23:14:55 PDT 1990 (anderson--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLVR
      include 'p2conf.h'
c
c     Solution controller.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1983
C     MODIFIED : SHIN         UT                        1/88
C                 CALL NXTEL AND SAVE TO COMMON/SHINNX
C                 SHINTF WILL USE THIS INFORMATION 
c     Revision : G. Anderson  Stanford University       Dec, 1989
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'sol.h'
      include     'stat.h'
      include     'trfldmob.h'
c------------------------------------------------------------------
c
c                   type declarations 
c 
      logical lwcurr,lasc,ltone,lacanl,lss(MAXCNT),lsorac,lfstep
      double precision nwbias(MAXCNT),freq,vss,tolsor,omeg,fstep
      integer maxsor,nfstep
c 
c****************************************************************** 
c 
c...Check if symbolic factorization has been done - if not,
c...go home
      if(lsymdn) goto 1
      call erset(55,linum,0)
      return
c
c...Get parameters from solve card
1     call solck(nwbias,lwcurr,lasc,ltone,lacanl,freq,lss,vss,
     +           lsorac,tolsor,maxsor,omeg,fstep,nfstep,lfstep)
      if(errflg) return
c
c...Get element adjacency information for trefld calculation.
C...SHIN
      CALL NXTEL(P2T,P2TC)
c
c...Time-dependent or steady state?
      if(ltdep) then
         call solvt(nwbias,lwcurr,lasc,ltone)
      else
         call solvss(nwbias,lwcurr,lasc,lacanl,freq,lss,vss,
     +               lsorac,tolsor,maxsor,omeg,fstep,nfstep,lfstep)
      endif
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLVSS(nwbias,lwcurr,lasc,lacanl,freq,lss,vss,
     +              lsorac,tolsor,maxsor,omeg,fstep,nfstep,lfstep)
      include 'p2conf.h'
c
c     Steady-state solution module.
c
c     MRP Nov 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'sol.h'
      include     'stat.h'
      include     'impact.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   type declarations 
c 
      logical lwcurr,lback,lasc,lacanl
      logical lss(*),lsorac,lacnwt,lfstep
      integer i,istk,nstk,maxsor,ierr,nfstep
      character*20 itim1,chbias(MAXCNT), chdum
      real elapt1
      double precision nwbias(*),biastk(4,MAXCNT),tolsor,vss,omeg,freq
      double precision outbi,fstep
c..used by mc watch dog
c     double precision tbias(MAXCNT)
      data nstk/4/
c 
c****************************************************************** 
c
c...Initialization
      istk=0
c 
c...Bias step loop continuation?
10    if (.not.lcntnu) goto 20
      do 5 i=1,nelect
      nwbias(i)=bias(i)
      if(lelect(i)) nwbias(i)=nwbias(i)+vstep
5     continue
      nsteps=nsteps-1
c
c----------------
c   BEGIN SOLVE 
c----------------
c
c...Start timing
20    elapt1=0. 
      call xclock(2,itim1,elapt1)
c
c...Initial guess
      call iguess(nwbias)
      if(errflg) return
c 
c...Write bias info
      do 222 i=1,nelect
         if(lcurbc(i)) then
            outbi=djcoef*jbias(i)
            write(chbias(i),2100) 'I',mod(i,10),outbi
         else
            outbi=dktq*bias(i)
            write(chbias(i),2100) 'V',mod(i,10),outbi
         endif
2100     format(1x,a1,i1,' =',1pe15.7)
222   continue
      write(luout,1000) (chbias(i),i=1,nelect) 
      if(luout.ne.lutty) write(lutty,1000) (chbias(i),i=1,nelect)
      if(lcpu) write(lucpu,1000) (chbias(i),i=1,nelect)
1000  format(1x,////' Solution for bias:',/,5(3x,a20,5x,a20/) )
c
      if(linit) then
         write(luout,2000) 
         if(luout.ne.lutty) write(lutty,2000) 
2000     format(/'  Initial solution')
      else if(lprev) then
         write(luout,3000)
         if(luout.ne.lutty) write(lutty,3000)
         if(local) then
            write(luout,3001)
            if(luout.ne.lutty) write(lutty,3001)
         endif
3000     format(/'  Previous solution used as initial guess')
3001     format( '  (local quasi-fermi levels)')
      else if(lproj) then
         write(luout,4000)
         if(luout.ne.lutty) write(lutty,4000)
4000     format(/'  Projection used to find initial guess')
      endif
c
      if(errflg) return
      if(wrnflg) call erite
c
c---------
c  SOLVE 
c---------
c
c...Initialize 
      gvmax=0.d0
      gnmax=0.d0
      gpmax=0.d0
      lback=.false.
c
c...Choose appropriate method
      if(lgumm) goto 110
      call newton(lback,lacnwt)
      goto 120
110   call gummel
c
c...If continuation and we need to go back, do it
120   if(.not.lcontn.or..not.lback) goto 300
      istk=istk+1
      if(istk.le.nstk) goto 121
      call erset(166,linum,nstk)
      return
121   if(ldebug) write(6,*) ' '
      do 130 i=1,nelect
      biastk(istk,i)=nwbias(i)
      nwbias(i)=(nwbias(i)-obias(i))*acontn+obias(i)
      if(ldebug.and.nwbias(i).ne.bias(i)) 
     +   write(6,12000) i,dktq*obias(i),dktq*bias(i),dktq*nwbias(i)
12000 format(' Contact #',i2,' : old',f14.8,', new',f14.8,', rev',f14.8)
130   continue
      write(luout,1300)
      if(luout.ne.lutty) write(lutty,1300)
1300  format(//' **** Convergence problem - ',
     +         'take smaller bias step ****'//)
c
c...Restore old psi,n,p
      do 140 i=1,np
         fv(i)=ofv(i)
         fn(i)=ofn(i)
         fp(i)=ofp(i)
140   continue
      do 141 i=1,nelect
         dflux(i)=dflux0(i)
         vres(i)=vres0(i)
         amp(i)=oamp(i)
141      bias(i)=obias(i)
      do 142 i=1,nb
142      dfxpt(i)=dfxpt0(i)
      if(lproj) call dumpo(.false.,.false.)
      lback=.false.
      goto 20
c 
c-------------
c  TERMINATE
c-------------
c
c...Print dc terminal characteristics
300   if(errflg) return
      call qaprn
c
c...Calculate ionization integral if necessary
      if(lbreak) call break
c
c...Ac analysis
      if(lacanl) call acanal(freq,lss,vss,lsorac,tolsor,maxsor,
     +                       omeg,lacnwt,fstep,nfstep,lfstep)
c
c...Stop timing and print cpu time
      call xclock(2,itim1,elapt1)
      call solpr(llogj,elapt1) 
c
c...Monte Carlo watch dog
c
c      if(lmontc) then
c        do 500 i=1,nelect
c           tbias(i)=nwbias(i)*dktq
c500     continue
c         call watch(tbias)
c      endif
c
      call convqf
c
c...Save results?
      if(lsolwr) call solwr(namsol,lwcurr,lasc)
      lsolst=.true. 
c
c...If continuation method, make sure we are through
      if(.not.lcontn) goto 301
      if(lproj) call fopcl(0,chdum,20,lutmp2,.false.,ierr)
      if(istk.eq.0) goto 301
      do 303 i=1,nelect
303   nwbias(i)=biastk(istk,i)
      istk=istk-1
      if(lunkwn) then
         lproj=.true.
         lprev=.false.
      endif
      goto 20
c
c...If stepping bias, loop back
301   if(nsteps.le.0) goto 400 
      lcntnu=.true. 
      goto 10
c
c...No (more) stepping
400   lcntnu=.false.
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLVT(nwbias,lwcurr,lasc,ltone)
      include 'p2conf.h'
c
c     Time-dependent solution module.
c
c     MRP Nov 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
cCmje: #include     "key.h"
      include     'setup.h'
      include     'logunt.h'
      include     'sol.h'
      include     'stat.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   type declarations 
c 
      logical lwcurr,lback,lasc,ltone,l1step,ldum,lramp
      integer i,itstep,nrl0
      character*20 itim1,chbias(MAXCNT)
      real elapt1
      double precision nwbias(*),savflx(MAXCNT),savv(MAXCNT),stim00,
     +              outbi,delt1,delt2,tmp,fbias(MAXCNT),bias0(MAXCNT),
     +              savfpt(MAXCON)
      character*6 chtd(2)
      data chtd/'(TR)','(BDF) '/
c
c****************************************************************** 
c
c
c...Initialization
      lback=.false.
      l1step=.true.
      nrl0=nrloop
      lramp=tramp1.gt.0.d0
      do 1 i=1,nelect
         fbias(i)=nwbias(i)
         bias0(i)=bias(i)
1     continue
c
c----------------
c   BEGIN SOLVE 
c----------------
c
c...Start timing
10    elapt1=0. 
      call xclock(2,itim1,elapt1)
c
c...Dont let simulation time pass tstop
      delt=dmin1(delt,tstop-stime)
      stim00=stime
c
c...Initial guess.  Should do better than this using some kind
c...of extrapolation!!
11    if(l2nd) then
         itstep=1
         ltrule=.true.
         l2bdf=.false.
         if(.not.lback) call gtrhs0(l1step.or.lwcurr)
         delt0=delt
         tmp=tgam*delt0

         if(lramp) then
            call vramp(bias0,fbias,nwbias,stime+tmp)
            call iguess(nwbias)
            call vramp(bias0,fbias,nwbias,stime+delt0)
         else if(l1step.or.(.not.lexqf)) then
            call iguess(nwbias)
         else
            call extrp2(ofv,ofn,ofp,delt1,fvtr,fntr,fptr,delt2,
     +                   fv,fn,fp,tmp)
            do 6 i=1,nelect
               oamp(i)=amp(i)
               dflux0(i)=dflux(i)
               vres0(i)=vres(i)
6           continue
            do 661 i=1,nb
661            dfxpt0(i)=dfxpt(i)
         endif

         delt1=tmp
         delt=delt1
         lsvrhs=.false.
      else
         itstep=0
         ltrule=.false.
         l2bdf=.false.
         delt0=delt
         if(lramp) call vramp(bias0,fbias,nwbias,stime+delt)
         call iguess(nwbias)
         lsvrhs=.false.
      endif
c
      if(errflg) return
c
c-------------------
c  BEGIN TIME-STEP
c-------------------
c
c...Get new time
20    stime0=stime
      stime=stime+delt
      if(itstep.gt.1) goto 22
c 
c...Write bias info
      do 222 i=1,nelect
         if(lcurbc(i)) then
            outbi=djcoef*jbias(i)
            write(chbias(i),2100) 'I',mod(i,10),outbi
         else
            outbi=dktq*bias(i)
            write(chbias(i),2100) 'V',mod(i,10),outbi
         endif
2100     format(x,a1,i1,' =',1pe15.7)
222   continue
      write(luout,1000) (chbias(i),i=1,nelect) 
      if(luout.ne.lutty) write(lutty,1000) (chbias(i),i=1,nelect)
      if(lcpu) write(lucpu,1000) (chbias(i),i=1,nelect)
1000  format(1x,////' Solution for bias:',/,5(3x,a20,5x,a20/) )
c
22    if(l2nd) then
         write(luout,1001) stime,chtd(itstep)
         if(luout.ne.lutty) write(lutty,1001) stime,chtd(itstep)
         if(lcpu) write(lucpu,1001) stime,chtd(itstep)
1001     format(/'  Time = ',1pe13.6,5x,a6)
      else
         write(luout,1011) stime
         if(luout.ne.lutty) write(lutty,1011) stime
         if(lcpu) write(lucpu,1011) stime
1011     format(/'  Time = ',1pe13.6)
      endif
c
      if(errflg) return
      if(wrnflg) call erite
c
c---------
c  SOLVE 
c---------
c
c...Initialize 
      gvmax=0.d0
      gnmax=0.d0
      gpmax=0.d0
      lback=.false.
      if(l2bdf) then
         deltf=dqkt*tgam2/delt
      else
         deltf=dqkt/delt
      endif
c
c...Choose appropriate method
      if(lgumm) goto 110
      if(luauto) then
         if(l2bdf) then
            nrloop=0
         else 
            nrloop=nrl0
         endif
      endif
      call newton(lback,ldum)
      goto 120
110   call gummel
120   continue
c
c...Done?
      if((.not.l2nd).or.l2bdf) goto 300
      write(luout,*) ' '
      if(luout.ne.lutty) write(lutty,*) ' '
c
c...Second order method and not done - increment counter.  
      itstep=itstep+1
      l2bdf=.true.
      ltrule=.false.
c
c...Adjust flux,bias voltages to be proper linear comb. for BDF-2
      tmp=delt2
      delt2=delt0*(1.d0-tgam)
      delt=delt2
      do 140 i=1,nelect
         savflx(i)=dflux0(i)
         savv(i)=vres0(i)
         dflux0(i)=tgam1*decoef*dflux(i)-tgam0*dflux0(i)
         vres0(i)=tgam1*vres(i)-tgam0*vres0(i)
140   continue
      do 141 i=1,nb
         savfpt(i)=dfxpt0(i)
         dfxpt0(i)=tgam1*decoef*dfxpt(i)-tgam0*dfxpt0(i)
141   continue
c
c...If after first step, extrapolate initial guess.
c...Shift current solution into fvtr,fntr,fptr.
      if(lexqf.and.(.not.l1step).and.(.not.lramp)) then
         call extrp2(fvtr,fntr,fptr,tmp,ofv,ofn,ofp,delt1,
     +               fv,fn,fp,delt2)
      else
         do 130 i=1,np
            fvtr(i)=fv(i)
            fntr(i)=fn(i)
            fptr(i)=fp(i)
130      continue
         if(lramp) call iguess(nwbias)
      endif

      lsvrhs=.true.
      goto 20
c
c-------------
c  TERMINATE
c-------------
c
300   continue
c
c...Get new time step
      delt=delt0
      if(ltauto.and.(lpconv.and.lcconv)) call stept(delt0,delt)
      l1step=.false.
c
c...If time-step cut back too far or if didnt converge, 
c...repeat with smaller time-step
      if((delt/delt0.gt.0.2d0).and.(lpconv.and.lcconv)) goto 320

      stime=stim00
      delt=delt0*0.5d0

      do 322 i=1,np
         fv(i)=ofv(i)
         fn(i)=ofn(i)
         fp(i)=ofp(i)
322   continue

      do 323 i=1,nelect
         dflux(i)=savflx(i)
         vres(i)=savv(i)
323   continue
      do 324 i=1,nb
         dfxpt(i)=savfpt(i)
324   continue

      write(luout,3230) delt
3230  format(/'      ****  Time-step cut back too far   ****'/
     +        '      **** Repeat with smaller time-step ****'/
     +        '      ****     New dt =',1pe13.6,  '     ****')
      if(lutty.ne.luout) write(lutty,3230)

      lback=.true.
      l1step=.true.

      goto 11
c
c...OK - stop timing, print solution, write output file
320   if(errflg) return
      call qaprn
      call xclock(2,itim1,elapt1)
      call solpr(llogj,elapt1) 
      call convqf
      if(lsolwr) call solwr(namsol,lwcurr,lasc)
      lsolst=.true. 
c
c...Restore flux,vres from previous step
      if(l2nd) then
         do 302 i=1,nelect
            dflux0(i)=savflx(i)
            vres0(i)=savv(i)
302      continue
         do 303 i=1,nb
            dfxpt0(i)=savfpt(i)
303      continue
      endif
c
c...Done?
      if(stime.lt.tstop*.999999) goto 10
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NORM(v,num,ndupl,dnorm,idx,nflag)
      include 'p2conf.h'
c
c     The following procedure calculates the infinity norm of
c     the components of an array v.  If ndupl=1, the entire
c     array is used, else we must skip over ndupl-1 entries.
c             nflag =  1.....L1 norm
c             nflag >  1.....Infinity norm
c
c     Original : MRP           Stanford University     Nov, 1983
c     Modified:  GDA           Stanford University     Jun, 1990
c                              Scaling never used, so bag it.
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Common area
c
      include     'sol.h'
c------------------------------------------------------------------
c
c  Local variables
c
      double precision v(1),dnorm,txt1,vi
      integer ndupl,i,ii,ibc,nflag,num,idx
c------------------------------------------------------------------
c
c...Start - initialize
      dnorm=0.d0
      ii=1
      ibc=1
      idx=0
c--------------------------
c  Loop through each node
c--------------------------
      do 10 i=1,num
c
c...Do for each eqn at point
      vi=v(ii)
      if(nflag.eq.1) then
         dnorm=dnorm+dabs(vi)
      else 
       txt1=dabs(vi)
       if(txt1.gt.dnorm) then
          dnorm=txt1
          idx=i
       endif
      endif
      ii=ii+ndupl
   10 continue
c
c...Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RNORM(npde,v,vx,dnorm,nflag)
      include 'p2conf.h'
c
c     The following procedure calculates the infinity norm of
c     the rhs components of an array v for the equation specified
c     by npde.  The norm is scaled to be consistent with the units 
c     on flux or current depending on npde.
c
c     Original : MRP           Stanford University     Apr, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
c------------------------------------------------------------------
c
      double precision v(1),dnorm,vx(1)
      integer npde,idx,nflag
c------------------------------------------------------------------
c
c...Go!
      if(npde.gt.1) goto 10
c
c...Poisson
      call norm(v,np,nmult,dnorm,idx,nflag)
      dnorm=-dnorm*decoef
      goto 99
c
c...Continuity
10    call norm(v,np,nmult,dnorm,idx,nflag)
      dnorm=-dnorm*djcoef
c
c...Done
99    return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SOLCK(nwbias,lwcurr,lasc,ltone,lacanl,freq,lss,vss,
     +                 lsorac,tolsor,maxsor,omeg,fstep,nfstep,lfstep)
      include 'p2conf.h'
c 
c     This routine gets the info off the solve card and checks it 
c     for validity.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1983
c     Revision : G. Anderson  Stanford University       Mar, 1989
c                No longer check for filenames to write a, rhs
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'stat.h'
      include     'setup.h'
      include     'sol.h'
c------------------------------------------------------------------
c 
c 
c                   type declarations 
c 
      logical lwcurr,lasc,ltone,lchng,lacanl,lss(*),lsorac,lfstep
      double precision nwbias(*),freq,vss,omeg,tolsor,fstep
      integer i,j,iel,nestp,maxsor,ibcode,nfstep
c
c FUNCTIONS:
      logical isrval, iscval
      real    gtrval
      logical gtlval
c 
c****************************************************************** 
c 
c                   start 
c 
c...Initialize flags
      lpjerr=.false.
      lcntnu=.false.
c
c...Is mesh done? 
      if(lmshdn) goto 5
      call erset(54,linum,0)
      return
c
c-----------------
c  Initial guess
c-----------------
c
c...Initialize
5     lunkwn=.false.
      local=.false.
      lproj=.false.
      linit=.false.
      lprev=.false.
c
c...Is setup type specified? - if not set to proj and set unknown flag
c...to indicate that type must be determined
      if(gtlval(1).or.gtlval(2).or.gtlval(3).or.gtlval(4)) goto 10
      lunkwn=.true.
      lproj=.true.
      goto 100
c
c...Get setup type and correct any ambiguity
10    if(gtlval(1)) linit=.true. 
      if(gtlval(3).and..not.linit) lproj=.true.
      if(gtlval(4)) local=.true.
      if(.not.linit.and..not.lproj) lprev=.true. 
c
c-------------------
c  First bias pass
c-------------------
c
c...Get any new bias voltages and voltage step, if specified
100   lchng=.false.
      do 125 i=1,nelect 
      nwbias(i)=gtrval(i)
      if(isrval(i)) then
         if(lcurbc(i)) then
            nwbias(i)=nwbias(i)/djcoef
         else
            nwbias(i)=nwbias(i)*dqkt
         endif
         lchng=.true.
      endif
125   continue
      vstep=gtrval(11)
      nsteps=gtrval(12) 
c
c------------------
c  Time-dependent
c------------------
c
c...Check for time-dependency
      ltdep=(gtrval(14).ge.0.d0).or.(gtrval(17).ge.0.d0).or.
     +      (gtrval(24).ge.0.d0).or.(gtrval(25).ge.0.d0)
      if(.not.ltdep) goto 210
c
c...If initial time-step is not specified, check
c...previous solution.
      if(gtrval(14).gt.0.d0) then
         delt=gtrval(14)
      else
         if((l2nd.and.(delt.le.0.d0)).or.(.not.l2nd)) then
            call erset(302,linum,0)
            return
         endif
      endif
c
      if(lgumm) then
         call erset(250,linum,0)
         return
      endif
c
c...For now, cannot time step and voltage step,
c...but can have ramp.  For ramp, can specify end 2 
c...different ways
      if(vstep.ne.0.d0) call erset(234,linum,0)
      if(gtrval(24).gt.0.d0) then
         tramp0=stime
         tramp1=gtrval(24)+tramp0
      else if(gtrval(25).gt.0.d0) then
         tramp0=stime
         tramp1=gtrval(25)
      else
         tramp0=stime
         tramp1=-1.d0
      endif
c
c...Check for legitimate initial guess and time step
      if(delt.lt.0.d0) call erset(235,linum,0)
      if(.not.lunkwn.and.lproj) call erset(-243,linum,0)
      if(linit) call erset(236,linum,0)
      lunkwn=.false.
      lproj=.false.
      lprev=.true.
c
c...Obtain stopping time
      if (isrval(17)) then
         tstop=gtrval(17)
         if(nsteps.ne.0) call erset(249,linum,0)
      else
         tstop=stime+nsteps*delt
         nsteps=0
      endif
      ltone=lchng
      if(errflg) return
      goto 30
c
c----------------
c  Steady-state
c----------------
c
c...Check for bias stepping
210   delt=0.d0
      stime=0.
      if(nsteps.ge.1.and.(vstep.eq.0.d0)) call erset(-74,linum,0)
      iel=gtrval(13) 
      if(vstep.eq.0.d0) goto 30 
c
c...Could be more than one electrode stepped - set appropriate
c...element of lelect array if being stepped
      if(iel.eq.-999) call erset(34,linum,0) 
      if(errflg) return

      ibcode=0
      do 135 i=1,nelect
135   lelect(i)=.false.

      if(iel.lt.0) call erset(190,linum,0)
      if(errflg) return
      nestp=ifix(alog10(float(iel)+.1))+1

      do 130 i=1,nestp
      j=iel-(iel/10)*10
      if(j.gt.nelect) call erset(190,linum,0)
      if(j.eq.0) j=10
      lelect(j)=.true.

      if(ibcode.eq.0) then
         if(lcurbc(j)) then
            ibcode=-1
         else
            ibcode=+1
         endif
      else if(ibcode.gt.0) then
         if(lcurbc(j)) call erset(48,linum,0)
      else if(.not.lcurbc(j)) then
         call erset(48,linum,0)
      endif

      iel=iel/10
130   continue
      if(errflg) return

      if(ibcode.gt.0) then
         vstep=vstep*dqkt
      else
         vstep=vstep/djcoef
      endif
c
c...If no new biases are specified (and looping) then increment
c...on previous bias conditions.  
      do 20 i=1,nelect
      if(nwbias(i).eq.-999.d0) goto 20
      if(errflg) return
      goto 30
20    continue
      do 21 i=1,nelect
      if(lelect(i)) nwbias(i)=bias(i)+vstep
21    continue
      if(.not.ltdep) nsteps=nsteps-1
c
c----------------
c  More biasing
c----------------
c
c.....Code fix- From John Faricelli, at DEC.
30    do 40 i=1,nelect
         if(nwbias(i).eq.-999.d0) then
c           Enter defaults for contact potential.
c           On INITIAL solution, contacts whose potential is unspecified
c           are set to zero. Otherwise, use the previous bias.
            if( linit ) then
               nwbias(i) = 0.0d0
            else
               nwbias(i) = bias(i)
            endif
         endif
40    continue
c...Minority carrier qf potential
      pbias=gtrval(15)
      if(pbias.ne.-999.d0) pbias=pbias*dqkt
      nbias=gtrval(16)
      if(nbias.ne.-999.d0) nbias=nbias*dqkt
c
c---------------
c  Ac analysis
c---------------
c
      lacanl=gtlval(7)
      if(lacanl) then
c
c...For now, error if <2 carriers or resistive/capac bc
         if(ncarr.lt.2) then
            call erset(275,linum,0)
            return
         endif
         do 675 i=1,nelect
            if(lresis(i).or.lcnres(i)) then
               call erset(194,linum,0)
               return
            endif
675     continue
c
c...If not full Newton, bag it
         if(.not.lnewt) then
            call erset(278,linum,0)
            return
         endif
c
c...Get frequency
         freq=gtrval(21)
         if (.not.isrval(21)) then
            call erset(274,linum,0)
            return
         endif
         fstep=0.d0
         nfstep=0
         if (isrval(26)) fstep=gtrval(26)
         if (isrval(27)) nfstep=gtrval(27)
         lfstep=gtlval(9)
c
c...Sor 
c         lsorac=gtlval(8)
         lsorac=.true.
         omeg=1.d0
         tolsor=1.d-5
         maxsor=25
         if(gtrval(20).gt.0.d0) omeg=gtrval(20)
         if(gtrval(19).gt.0.d0) tolsor=gtrval(19)
         if(gtrval(22).gt.0.d0) maxsor=gtrval(22)
c
c...Small-signal voltage
         vss=0.1d0*dktq
         if(gtrval(18).gt.0.d0) vss=gtrval(18)
c
c...Admittance matrix columns - default to all
         iel=gtrval(23)
         if(iel.eq.-999.d0) then
            do 132 i=1,nelect
               lss(i)=.true.
132         continue
         else
            do 134 i=1,nelect
               lss(i)=.false.
134         continue
            nestp=ifix(alog10(float(iel)+.1))+1
            do 133 i=1,nestp
               j=iel-(iel/10)*10
               if(j.eq.0) j=10
               lss(j)=.true.
               iel=iel/10
133         continue
         endif

      endif
c
c-------------------
c  Solution output
c-------------------
c
c...Save solution? - get file name
      lsolwr=.false.
      if (.not.iscval(1)) goto 60
      lsolwr=.true. 
      call gtcval(1, namsol, LEN(namsol))
c
c...Write currents,etc. ?
      lwcurr=gtlval(5)
c
c...Ascii?
      lasc=gtlval(6)
c
60    continue
c
c...Error file, dump a file
c...For power users only; no need to check this normally
c     call gtcval(2, namerr, LEN(namerr))
c     call gtcval(3, namafl, LEN(namafl))
c 
c...Bye!
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE QAPRN
      include 'p2conf.h'
c 
c     This routine prints out terminal flux and current values.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1983
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
      include     'symme.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer i,lu
      double precision jdisp(10)
c 
c****************************************************************** 
c 
c                   start 
c
c...Set ncarr0 indicating that this solution was obtained with
c...ncarr carriers
      ncarr0=ncarr
c
c...Calculate terminal current/flux
      do 10 i=1,nelect
      dflux(i)=dflux(i)*decoef
      namp(i,1)=namp(i,1)*djcoef
      namp(i,2)=namp(i,2)*djcoef
10    amp(i)=namp(i,1)+namp(i,2)
      do 11 i=1,nb
11    dfxpt(i)=dfxpt(i)*decoef
c 
c...luinf first 
      lu=luinf
c
c...Calculate displacement current from fluxes
      do 425 i=1,nelect
      if(ltdep) then
         jdisp(i)=(dflux(i)-dflux0(i))*deltf*dktq
      else
         jdisp(i)=0.d0
      endif
425   continue
c
c...Print it!
420   CONTINUE
      IF (LCYL.or.lwidth) THEN
         WRITE(LU,9044)
      ELSE
         WRITE(LU,9045)
      ENDIF
9044  format(/' Electrode',3x,'Voltage',3x,'Electron Current',3x,
     +       'Hole Current',3x,'Conduction Current'/
     +       13x,'(Volts)',4x,'   (amps)    ',5x,
     +       '   (amps)    ',4x,'   (amps)    ')
9045  format(/' Electrode',3x,'Voltage',3x,'Electron Current',3x,
     +       'Hole Current',3x,'Conduction Current'/
     +       13x,'(Volts)',4x,'(amps/micron)',5x,
     +       '(amps/micron)',4x,'(amps/micron)')
      write(lu,9050) (i,vres(i)*dktq,namp(i,1),namp(i,2),
     +                amp(i),i=1,nelect)
9050  format(i6,0pf14.4,1pe16.5,1pe18.5,1pe17.5)
c
      IF (LCYL.or.lwidth) THEN
         WRITE(LU,9046)
      ELSE
         WRITE(LU,9047)
      ENDIF
9046  format(/' Electrode',10x,'Flux',9x,'Displacement Current',6x,
     +        'Total Current'/16x,'   (coul)    ',7x,
     +        '   (amps)    ',10x,'   (amps)    ')
9047  format(/' Electrode',10x,'Flux',9x,'Displacement Current',6x,
     +        'Total Current'/16x,'(coul/micron)',7x,
     +        '(amps/micron)',10x,'(amps/micron)')
      write(lu,9048) (i,dflux(i),jdisp(i),jdisp(i)+amp(i),i=1,nelect)
9048  format(i6,1pe22.5,1pe20.5,1pe23.5)
c
      write(lu,9040)
9040  format(' ') 
c
c...Have we tried luout yet?
      if(lu.eq.luout) goto 500 
      lu=luout
      goto 420
c
c...Put total current (cond + disp) in amp
500   do 510 i=1,nelect
510   amp(i)=amp(i)+jdisp(i)
c
c...Bye
      lflow=.false.
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SOLPR(lrite,elap1) 
      include 'p2conf.h'
c 
c     This routine merely prints the total elapsed
c     time and the elapsed time of the solution routines and
c     possibly prints terminal currents/voltages to log file. 
c
c     Original : M.E.Law      Stanford University       Aug, 1982
c     Revision : MRP
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'logunt.h'
      include     'setup.h'
      include     'sol.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer lu,i
      real elap1,elap3
      character*20 itim1
      logical lrite
c 
c****************************************************************** 
c 
c                   start 
c 
c...Print to luinf and luout
      lu=luinf
      elap3 = 0
      call xclock(2,itim1,elap3)
10    continue
c
c...How did we get here?
      if(lpconv) write(lu,4000)
      if(lcconv) write(lu,4001)
      if(.not.lcconv.and.iconv.eq.4) write(lu,4002)
4000  format(' Absolute convergence criterion met for Poisson')
4001  format(' Absolute convergence criterion met for continuity')
4002  format(' Relative terminating criterion met for continuity')
c
c...CPU Time
      write(lu,1000) elap1,elap3
1000  format(' Total cpu time for bias point =',f8.2,/,
     +       ' Total cpu time =',f8.2)
c
c...Have we converged?
      if((lpoiss.and..not.lpconv).or.
     +   ((.not.lpoiss).and..not.(lcconv.and.lpconv))) write(lu,1010)
1010  format(' *** Warning ***'/' This solution has not converged',
     +       ' to the selected tolerances!')
c
c...Have we done luout yet? 
      if(lu.eq.luout) goto 99
      lu=luout
      goto 10 
c 
99    if(lcpu) write(lucpu,1000) elap1,elap3
c
c...Log file?
      if(lrite) 
     +   write(lulog,5000) stime,
     +                     (dktq*vres(i),i=1,nelect),
     +                     (dktq*bias(i),i=1,nelect),
     +                     (amp(i),i=1,nelect)
5000  format(1pe12.5,30(1pe16.8))
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CONVQF
      include 'p2conf.h'
c
c     To calculate quasi-fermi potentials from carrier concentrations.
c
c     Original : MRP          Stanford University       Date unknown(HRY)
c     Revision : HRY (links into stats.f )              Jun. 1985
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer i
      double precision QFNN, QFPP
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...If a carrier was not solved for, it had its qf fixed - we therefore
c...dont need to convert it
      if(ncarr.eq.0) goto 999
      if(l1hole) goto 100
      if(ncarr.eq.1) goto 200
c
c...Both carriers were solved for - convert both
      do 11 i=1,np
      if(mattyp(itype(i)).le.0) goto 11
      qfn(i) = QFNN(lboltz,fn(i),fv(i),lgcnie(i),ncband,fp(i))
      qfp(i) = QFPP(lboltz,fp(i),fv(i),lgcnie(i),nvband,fn(i))
11    continue
      goto 999
c
c...Just holes
100   do 111 i=1,np
      if(mattyp(itype(i)).gt.0) then
        qfp(i)=QFPP(lboltz,fp(i),fv(i),lgcnie(i),nvband,fn(i))
      endif
111   continue
      goto 999
c
c...Just electrons
200   do 211 i=1,np
      if(mattyp(itype(i)).gt.0) then
        qfn(i)=QFNN(lboltz,fn(i),fv(i),lgcnie(i),ncband,fp(i))
      endif
211   continue
c
c...Bye
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE LOGCHK
      include 'p2conf.h'
c
c     Set up log files for solution data.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
c------------------------------------------------------------------
c
      integer ierr
      character*20 tmpbuf
c
c FUNCTIONS:
      logical iscval
c------------------------------------------------------------------
c
c...IV file?
      if (iscval(1)) then
         call gtcval(1, tmpbuf, LEN(tmpbuf))
         call fopcl(12,tmpbuf, LEN(tmpbuf),lulog,.false.,ierr)
         if(errflg) return
         write(lulog,*) nelect
         llogj=.true.
      endif
c
c...AC admittance file?
      if (iscval(2)) then
         call gtcval(2, tmpbuf, LEN(tmpbuf))
         call fopcl(12,tmpbuf,LEN(tmpbuf),lulog2,.false.,ierr)
         if(errflg) return
         write(lulog2,*) nelect
         llogac=.true.
      endif
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE STEPT(dt0,dt1)
      include 'p2conf.h'
c
c     Calculate new time step.
c
c     Original : Mark R. Pinto   Nov, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      double precision dt0,dt1,lte,third,tmp,ttr,t0,lteni,ltepi
      double precision ltenwt,theta1,theta2
      integer i,ii
c
      data third/3.333333333333333333d-1/
      data theta1,theta2/0.8d0,0.9d0/
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize coef's for divided difference
      ttr=2.d0*dqkt/(tgam*tgam*dt0)
      t0=(2.d0-tgam)/tgam
c
c...Calculate relative lte
      lte=0.d0
      ii=2
      do 10 i=1,np

      if((lm(i).ne.0).or.(mattyp(itype(i)).lt.0)) goto 11

      lteni=ltenwt(ii,t0)
      ltepi=ltenwt(ii+1,t0)

      lteni=lteni/essem(i)+ttr*(fntr(i)-ofn(i))
      ltepi=ltepi/essem(i)+ttr*(fptr(i)-ofp(i))

      if(l2norm) then
         if(fn(i).gt.0.d0) then
            tmp=lteni/fn(i)
            lte=lte+tmp*tmp
         endif
         if(fp(i).gt.0.d0) then
            tmp=ltepi/fp(i)
            lte=lte+tmp*tmp
         endif
      else
         if(fn(i).gt.0.d0) lte=dmax1(lte,dabs(lteni/fn(i)))
         if(fp(i).gt.0.d0) lte=dmax1(lte,dabs(ltepi/fp(i)))
      endif

11    ii=ii+nmult
10    continue

      if(l2norm) lte=dsqrt(lte)/float(np)
      lte=lte*tgame*dt0*dktq
c
c...Calculate time-step
      dt1=dt0*(timtol/lte)**third
      if(dt1.le.theta1*dt0) then
         dt1=dmax1(theta2*dt1,dtmin)
      else
         dt1=dmax1(dt1,dtmin)
         dt1=dmin1(dt1,dt0*2.d0)
      endif
c
      write(luout,1000) lte,dt0,dt1
      if(luout.ne.lutty) write(lutty,1000) lte,dt0,dt1
1000  format(/' LTE = ',1pe13.6,'   dt0 = ',1pe13.6,'    dt = ',1pe13.6)
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION LTENWT(i,t0)
      include 'p2conf.h'
c
c     A stupid routine to calculate part of the LTE for the auto
c     time-step calculation. (FULL NEWTON)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision t0
      integer i
c------------------------------------------------------------------
c
      ltenwt=x(i)+t0*rhs0(i)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EXTRP2(v0,n0,p0,dt0,v1,n1,p1,dt1,v2,n2,p2,dt2)
      include 'p2conf.h'
c
c     Quadratic extrapolation for time-dependent solutions.
c     Shift v2,n2,p2 to v0,n0,p0.
c
c         |<---- dt0 ---->|<---- dt1 ---->|<---- dt2 ---->|
c        v0              v1              v2              v2
c        n0              n1              n2    ======>   n2
c        p0              p1              p2              p2
c
c     Org: MRP Dec 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
c------------------------------------------------------------------
c
      double precision v0(1),n0(1),p0(1),v1(1),n1(1),p1(1),
     +                 v2(1),n2(1),p2(1),dt0,dt1,dt2
      double precision dta,dtb,dtc,dtd,f0,f1,f2,slope1
      integer i
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initalize
      dtc=dt0+dt1
      dta=dtc+dt2
      dtb=1.d0/dt0
      dtc=(dt1+dt2)/dtc
      dtd=1.d0/dt1
c
c...Loop through nodes
      do 10 i=1,np
c
c...Dont extrapolate potential!
      v0(i)=v2(i)

      f0=n0(i)
      f1=n1(i)
      f2=n2(i)
      slope1=(f1-f0)*dtb
      f2=f0+dta*(slope1+dtc*((f2-f1)*dtd-slope1))
      n0(i)=n2(i)
      if(f2.gt.0.d0) n2(i)=f2

      f0=p0(i)
      f1=p1(i)
      f2=p2(i)
      slope1=(f1-f0)*dtb
      f2=f0+dta*(slope1+dtc*((f2-f1)*dtd-slope1))
      p0(i)=p2(i)
      if(f2.gt.0.d0) p2(i)=f2

10    continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE GTRHS0(l1step)
      include 'p2conf.h'
c
c     Get steady state rhs for TR time step.
c
c     Orig : MRP Dec 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical l1step
      integer i,idum
      double precision savflx(10),savfpt(300)
c------------------------------------------------------------------
c
c************
c**  START **
c************
c
c...If this is the first step on solve card, then we need to 
c...re-assemble, saving flux for displacement calculation
      if(l1step) then

         do 110 i=1,nelect
  110       savflx(i)=dflux(i)
         do 111 i=1,nb
  111       savfpt(i)=dfxpt(i)

         ltdep=.false.
         lsvrhs=.true.
         call assmb(lgumm,idum)
         ltdep=.true.

         do 120 i=1,nelect
  120       dflux(i)=savflx(i)
         do 121 i=1,nb
  121       dfxpt(i)=savfpt(i)

      endif
c
c...Store safely
      call savrn
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SAVRN
      include 'p2conf.h'
c
c     Save ss part of rhs for TR time discretization. (FULL NEWTON)
c     Transfer from x to rhs0.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i
c------------------------------------------------------------------
c
      do 5 i=1,neq
      rhs0(i)=x(i)
5     continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE VRAMP(bias0,fbias,nwbias,nwtime)
      include 'p2conf.h'
c
c     Calculate terminal voltages for ramp input.
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
c------------------------------------------------------------------
c
      double precision nwtime,bias0(1),fbias(1),nwbias(1),fact
      integer i
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      if(nwtime.gt.tramp1) then 
         do 10 i=1,nelect
            nwbias(i)=fbias(i)
10       continue
      else
         fact=(nwtime-tramp0)/(tramp1-tramp0)
         do 20 i=1,nelect
            if(fbias(i).ne.bias0(i)) then
               nwbias(i)=bias0(i)+fact*(fbias(i)-bias0(i))
            else
               nwbias(i)=fbias(i)
            endif
20       continue
      endif
c
      return
      end
