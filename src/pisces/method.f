cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE METHOD
      include 'p2conf.h'
c
c
c     Subroutine to solution variant to be applied to any bias
c     points following the occurance of the specified METHOD card.
c
c     Original : MRP         Stanford University        Nov, 1983
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c----------------------------------------------------------------
c
      include     'blank.h'
      include     'stat.h'
      include     'sol.h'
      include     'setup.h'
c----------------------------------------------------------------
c
c FUNCTIONS:
      logical islval
      real    gtrval
      logical gtlval
c----------------------------------------------------------------
c
c************
c** Start  **
c************
c
c...If symbolic card has not been specified, we've got trouble
      if(lsymdn) goto 10
      call erset(45,linum,0)
      goto 99
c
c...Get iteration limits and tolerances 
10    if(gtrval(1).ge.0.d0) itlmt=gtrval(1)
      if(islval(3)) lgnorm=gtlval(3)
      if(islval(9)) lxnorm=gtlval(9)
      if(lxnorm) then
         if(gtrval(3).ge.0.d0) ptolx=gtrval(3) 
         if(gtrval(4).ge.0.d0) ctolx=gtrval(4)
         if(gtrval(26).ge.0.d0) ctolx0=gtrval(26)
      else
         if(gtrval(3).ge.0.d0) ptolg=gtrval(3) 
         if(gtrval(4).ge.0.d0) ctolg=gtrval(4)
      endif
      if(islval(2)) lprntj=gtlval(2)
      if(islval(5)) ldbug2=gtlval(5)
c
c...Continuation method?
      if(islval(18)) lcontn=gtlval(18)
      if(gtrval(27).gt.0.d0) acontn=gtrval(27)
c
c...Time-discretization and time-step control
      if(islval(17)) ltauto=gtlval(17)
c...mje, 2ndorder always wants the default, this if(GTLVAL
      if(gtlval(21)) l2nd=gtlval(21)
      if(islval(22)) l2norm=gtlval(22)
      if(gtrval(29).gt.0.d0) timtol=gtrval(29)
      if(gtrval(30).gt.0.d0) dtmin=gtrval(30)
      if(islval(11)) lexqf=gtlval(11)
      if(l2nd) then
         tgam=2.d0-dsqrt(2.d0)
         tgam2=2.d0-tgam
         tgam1=1.d0/(tgam*tgam2)
         tgam0=(1.d0-tgam)*(1.d0-tgam)*tgam1
         tgame=dabs(((-3.d0*tgam+4.d0)*tgam-2.d0)/
     +         (6.d0*(2.d0-tgam)*(1.d0-tgam)))
      endif
      if(l2nd.and.(ncarr.ne.2)) then
         if(islval(21)) call erset(-253,linum,0)
         l2nd=.false.
         ltauto=.false.
      endif
      if(.not.l2nd.and.ltauto) then
         call erset(-254,linum,0)
         ltauto=.false.
      endif
c
      lfixqf=gtlval(8)
c
c...Neg conc reduction factor
      if((gtrval(12).gt.0.d0).and.(gtrval(12).lt.1.d0))
     +    nfact=gtrval(12)
c
c...Scale linear system(s) by pivots?
      if(islval(7)) lscl=gtlval(7)
      pscal=gtrval(18)
      if(pscal.le.0.d0) pscal=1.0d0
c
c...Set inner loop criteria 
      lu1cri = gtrval(16)
      if (lu1cri.le.0) lu1cri=3e-3
      lu2cri = gtrval(21)
      if (lu2cri.le.0) lu2cri=3e-2
c
      if(.not.lgumm) goto 100
c----------
c  Gummel
c----------
c 
c...Delta v limit - damping
      if(islval(6)) ldjr=gtlval(6)
      if(ldjr) then
         if(gtrval(6).gt.0.d0) ddamp=gtrval(6)
         if(gtrval(7).gt.0.d0) ldamp=gtrval(7)
         if(gtrval(11).gt.0.d0) fdamp=gtrval(11)
         if(gtrval(25).gt.0.d0) pdamp=gtrval(25)
      endif
      if(gtrval(2).gt.0.d0) dvlmt=gtrval(2)*dqkt
c
c...ICCG?
      if(islval(16)) icgflg = gtlval(16)
      if(gtrval(15).ge.0.d0) mxiccg=gtrval(15)
c 
c...Acceleration
      if(islval(13)) laccel=gtlval(13)
      if(.not.laccel) goto 40 
c 
c...Acceleration start, stop, step iters. 
      if(gtrval(8).gt.0.d0) acstrt=gtrval(8) 
      if(gtrval(9).gt.0.d0) acstop=gtrval(9) 
      if(gtrval(10).gt.0.d0) acstep=gtrval(10) 
c 
c...Multiple poisson per continuity 
40    if(gtlval(3)) lmultp=.true.
      if(gtlval(4)) lmultp=.false.
c 
c...Tolerance scaling 
      if(gtlval(10)) itmode=0
c 
c...Cant do single poisson if doing poisson only - Warning
      if(lmultp.or..not.lpoiss) goto 11
      lmultp=.true. 
      if(gtlval(4)) call erset(-59,linum,0)
c 
c...Cant do acceleration with multiple poisson - Warning
11    if(.not.(laccel.and.lmultp)) goto 99 
      call erset(-60,linum,0) 
      laccel=.false.
      goto 99
c----------
c  Newton
c----------
c
c...Newton-Richardson
100   if(islval(15)) luauto=gtlval(15)
      if(gtrval(14).gt.0.d0) cupdt=gtrval(14)
      if(gtrval(28).gt.0.d0) nrloop=gtrval(28)
c
c...Damping - diasble for now
      ldjr = .false.
cc      if(islval(6)) ldjr=gtlval(6)
cc      if(ldjr) then
cc         if(gtrval(6).gt.0.d0) ddamp=gtrval(6)
cc         if(gtrval(7).gt.0.d0) ldamp=gtrval(7)
cc         if(gtrval(11).gt.0.d0) fdamp=gtrval(11)
cc         if(gtrval(25).gt.0.d0) pdamp=gtrval(25)
cc      endif
c 
ccc...Error for parameter out of range 
cc151   if((cupdt.le.0.).or.(cupdt.ge.1.).or.
cc     +   (ldamp.lt.1).or.(fdamp.lt.1.)) call erset(295,linum,0)
c
c...Adios
99    lmthdn=.true.
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE IGUESS(nwbias)
      include 'p2conf.h'
c 
c     This program segment sets up initial guesses for a solution.
c 
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP            Stanford University       Nov, 1983
c
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c common areas
c 
      include     'blank.h'
      include     'setup.h'
c------------------------------------------------------------------
c
c type declarations 
c 
      double precision nwbias(*),vmax,vmin
c 
c****************************************************************** 
c 
c                   start 
c
c...If there is a carrier we aren't solving for (and its qf is
c...not user fixed), get min and max applied biases (only include
c...non-insulator contacts)
      if(.not.local .and. ncarr.eq.2) goto 110
      call bilims(nwbias,vmin,vmax)
c
c...Do 1 of the 3 kinds of setups (order is import.)
c...If there was an error on projecting, we can fix it here
110   if(lpjerr) then
         lpjerr=.false.
         lproj=.true.
         lprev=.false.
      endif
      if(linit) call inits(nwbias)
      if(lproj) call proj(nwbias)
      if(lprev) call prev(nwbias)
c
c...Set contact boundary conditions and transfer biases 
c...Remember - vres is now the actual bias (qf) at each contact
c...which, if there are resistors/capacitors, may be different
c...from nwbias
cc      write(*,111) (dktq*nwbias(i),dktq*vres(i),i=1,nelect)
cc111   format(2(1pe17.6)/)
      call bound(vres)
c
c...Adjust qfs
c...(either fixed by user or locally constant) for initial guess
c...or to specify minority carriers
      if(ncarr.lt.2.or.local) call adjqf(vmin,vmax,vres)
c
c...If this is not the initial bias point, restore old n,p
c...values at all contact nodes that have surface recombination
c...velocities specified.  If this is not done, the initial 
c...guess for these nodes will be equilibrium conc!
      if(.not.linit) call fixsch
c
c...Setup done, sol1 is setup data
      lsolst=.false.
c 
c...Done!!! 
      continue
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE INITS(nwbias)
      include 'p2conf.h'
c 
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP            Stanford University       Nov, 1983
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c  common area 
c 
      include     'blank.h'
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      integer TMPPAD(1464002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision nwbias(1)
      integer i
c 
c****************************************************************** 
c 
c                   start 
c 
c...Initial potential,n,p - flat qfs
      do 500 i=1,np 
      qfn(i)=0.d0
      qfp(i)=0.d0
      if (mattyp(itype(i)).gt.0) goto 400 
c
c...Insulator 
      fv(i)=0.0d0
      fn(i)=0.0d0
      fp(i)=0.0d0
      goto 500
c
c...Semic., compute conc. and pot. based on doping
c...Force Boltzmann stats. (Not anymore. HRY)
400   call poten(lboltz,i,r1(i),cnie(i),fn(i),fp(i),fv(i))
      if(errflg) return
c
c...Next node 
500   continue
c
c...Set actual contact biases
      do 600 i=1,nelect
      if(lcurbc(i)) then
         vres(i)=0.d0
      else
         vres(i)=nwbias(i)
      endif
      obias(i)=bias(i)
      bias(i)=nwbias(i)
600   continue
c
c...Done, solution 1 now present
      lsol1=.true.
      ldiff=.false.
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PROJ(nwbias)
      include 'p2conf.h'
c 
c     This routine extrapolates a new solution guess
c     based on the new bias and two previous solutions and biases.
c 
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP            Stanford University       Nov, 1983
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
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
      include     'emaco.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer ndelta,i,idelec
      real factor
      double precision dtemp,dpsi0,nwbias(*),fact1,fact2
c 
c****************************************************************** 
c 
c                   start 
c 
c...Solution 1 and 2 present? 
      if (lsol1) goto 3 
      if(lunkwn) then
         call erset(46,linum,0)
      else
         call erset(52,linum,0)
      endif
      return
c
c...Solution 2 doesn't exist - try previous
3     if (lsol2) goto 4
      if(.not.lunkwn) call erset(-51,linum,0) 
      goto 33 
c
c...If there are current sources, always use previous
4     do 5 i=1,nelect
         if(lcurbc(i)) goto 6
5     continue
      goto 10
6     if(.not.lunkwn) call erset(-65,linum,0)
      goto 33
c 
c...Compute extrapolation factor and ensure that
c...only 1 electrode bias (or p-bias) is changing 
10    idelec=-1 
      ndelta=0
      factor=0. 
c
c...Scan electrodes 
      do 20 i=1,nelect
c
c...Constant bias?  - check next electrode
      if ( (nwbias(i).ge.bias(i)-1.d-12).and.
     +     (nwbias(i).le.bias(i)+1.d-12).and.
     +     (obias(i) .ge.bias(i)-1.d-12).and.
     +     (obias(i) .le.bias(i)+1.d-12) ) goto 20
c
c...If this is not the first electrode that has been changed,
c...check if bias changes are same ; compute extrapolation
c...factor if first time
      if(ndelta.eq.1) then
         if((nwbias(i)-bias(i).ne.fact1).or.
     +      (bias(i)-obias(i).ne.fact2)) ndelta=ndelta+1
      else
         fact1=nwbias(i)-bias(i)
         fact2=bias(i)-obias(i)
         if(fact2.ne.0.d0) factor=fact1/fact2
         ndelta=ndelta+1 
      endif
      idelec=i
20    continue
c
c...Check if more than one bias changed - if so try previous
      if(ndelta.ne.1) then
         if(.not.lunkwn) call erset(-191,linum,0)
         goto 33
      endif
c
c...If factor is equal to zero try previous
      if(factor.ne.0.) goto 40 
      if(.not.lunkwn) call erset(-56,linum,idelec)
c
c...Tell user (unless we're responsible for trying proj because the 
c...user didn't specify a mode) you're trying prev. and do it - set
c...flag to recover on next loop through if we are stepping
33    if(.not.lunkwn) call erset(-57,linum,0) 
      lproj=.false. 
      lprev=.true.
      if(nsteps.ge.1) lpjerr=.true.
      return
c
c-----------------
c  EXTRAPOLATION
c-----------------
c 
c...If continuation method, dump old potential to a file
40    if(lcontn) call dumpo(.true.,.false.)
c
c...Loop through nodes
      do 50 i=1,np
c
c...Compute potential change and shift potentials
      dpsi0=ofv(i)
      dtemp=factor*(fv(i)-dpsi0) 
      ofv(i)=fv(i)
      fv(i)=fv(i)+dtemp 
c
c...Compute n conc. and shift
      dtemp=fn(i) 
      if(ofn(i).eq.0.d0) goto 45
      fn(i)=fn(i)*(fn(i)/ofn(i))**factor
45    ofn(i)=dtemp
c
c...Compute p conc. and shift
      dtemp=fp(i) 
      if(ofp(i).eq.0.d0) goto 55
      fp(i)=fp(i)*(fp(i)/ofp(i))**factor
55    ofp(i)=dtemp
c
c...Next node 
50    continue
c
c...Shift fluxes,currents,biases
      do 60 i=1,nelect
         oamp(i)=amp(i)
         dflux0(i)=dflux(i)
         if(lresis(i).or.lcurbc(i)) then
            dtemp=vres(i)
            vres(i)=dtemp+factor*(dtemp-vres0(i))
            vres0(i)=dtemp
         else
            vres0(i)=vres(i)
            vres(i)=nwbias(i)
         endif
         obias(i)=bias(i)
         bias(i)=nwbias(i)
60    continue
      do 61 i=1,nb
         dfxpt0(i)=dfxpt(i)
61    continue
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PREV(nwbias)
      include 'p2conf.h'
c 
c     This routine arranges for the previous solution to be 
c     used as the new guess.  
c 
c     Original : C.H.Price      Stanford University       May, 1982
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
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
      include     'emaco.h'
c------------------------------------------------------------------
c
       double precision nwbias(1)
       integer i
c
c****************************************************************** 
c 
c 
c                   start 
c 
c...Is there a previous solution present? 
      if (lsol1) goto 10
      call erset(52,linum,0)
      return
c
c...Shift sol1 into sol2 (leave qfs where they are)
10    if(.not.ltdep.or.(.not.l2bdf)) then
         do 100 i=1,np 
         ofv(i)=fv(i)
         ofn(i)=fn(i)
100      ofp(i)=fp(i)
      endif
c
c...Shift fluxes,currents,biases
      do 60 i=1,nelect
      if(.not.ltdep.or.(.not.l2bdf)) then
         oamp(i)=amp(i)
         dflux0(i)=dflux(i)
         vres0(i)=vres(i)
      endif
      if(.not.(lresis(i).or.lcurbc(i))) vres(i)=nwbias(i)
      obias(i)=bias(i)
      bias(i)=nwbias(i)
60    continue
      if(.not.ltdep.or.(.not.l2bdf)) then
         do 61 i=1,nb
            dfxpt0(i)=dfxpt(i)
61       continue
      endif
c
c...Done, sol2 now present
      lsol2=.true.
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE BOUND(nwbias)
      include 'p2conf.h'
c 
c     This routine computes the potentials and carrier conc. at the 
c     electrode contacts. It also shifts biases from current to old 
c     and new to curent. 
c 
c     Original : MRP            Stanford University       Apr, 1984
c     Modified : CSR                                      Aug, 1984
c     Revision : HRY  (links into stats.f with NOCCUP ..) Jun, 1985
c 
c     Copyright c 1984 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
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
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   type declarations 
c 
      integer numelc,numpnt,nummat,i,maxdpp(MAXCNT),ib
      double precision phi,cpot,vsref,conpot,nwbias(*)
      double precision NOCCUP, POCCUP
c 
c***********************************************************************
c 
c                              start 
c 
      do 100 i=1,nb 
      numpnt=nbc(i) 
      numelc=lm(numpnt)
      nummat=itype(numpnt)
c
c...If contact (distributed) resistance, dont bother unless initial
c...bias point
      if(.not.linit.and.lcnres(numelc)) goto 100
c
c...Set qf potentials to applied bias
      vsref=nwbias(numelc)
      qfn(numpnt)=vsref
      qfp(numpnt)=vsref
c
c...Insulator or semiconductor contact?
      if(mattyp(nummat).le.0) goto 101
c
c...Get carrier concentrations at contact based on doping.  Also get
c...difference between mid-gap potential and applied potential (phi).
      call poten(lboltz,numpnt,r1(numpnt),cnie(numpnt),
     +           fn(numpnt),fp(numpnt),phi)
      if(errflg) return
c
c...Now calculate total contact potential.
c...Adjust carrier concentrations based on contact potential.
c...Set boundary condition on psi.
      if(workf(numelc).eq.0.) then
         cpot=phi
      else
         cpot=conpot(workf(numelc),affin,ngap,pgap)
         fn(numpnt)= NOCCUP(lboltz,0.0d0,cpot,lgcnie(numpnt),
     1                                        ncband)
         fp(numpnt)= POCCUP(lboltz,0.0d0,cpot,lgcnie(numpnt),
     1                                        nvband)
      endif
      fv(numpnt)=vsref+cpot
      goto 100
c
c...Insulator - use bulk potential to get total work-fn difference
101   cpot=conpot(workf(numelc),affin,ngap,pgap)
      fv(numpnt)=nwbias(numelc)+cpot
c
c...Next boundary node 
100   continue

c
c--------------
c  Loop over insulator contacts in case they contact the semiconductor 
c  somewhere.  If they do, set equal to the max (min) potential on the
c  semiconductor part, if it is p-type (n-type).
c--------------
c

c.........First, find the point of maximum doping on each elec.
c.........(Undefined for pure insulating contacts)
      do 200 numelc = 1,nelect
  200    maxdpp(numelc)= -1

      do 250 ib = 1, nb
       numpnt  = nbc (ib)
       numelc = lm(numpnt)
       nummat  = itype (numpnt)

       if (mattyp (nummat) .gt. 0) then 
          if (maxdpp (numelc) .le. 0) then
             maxdpp (numelc) = numpnt
          else
             if (abs (r1 (numpnt)) .gt. abs (r1 (maxdpp (numelc))))
     +         maxdpp (numelc) = numpnt
          endif
       endif
  250 continue

c.........Now fix part-insulator contacts

      do 300 ib = 1, nb
       numpnt  = nbc (ib)
       numelc = lm(numpnt)
       nummat  = itype (numpnt)

       if (mattyp (nummat) .lt. 0 .and. maxdpp (numelc) .gt. 0) 
     +       fv (numpnt) = fv (maxdpp (numelc))
  300 continue        
c
c...Done
      return
       end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION CONPOT(wkfn,affin,ngap,pgap)
      include 'p2conf.h'
c
c     Calculate contact potential.
c
c     wkfn           = work-fn. metal
c     affin          = semi. electron affinity
c     ngap           = affinity - intrinsic Fermi energy
c     pgap           = egap - ngap
c     conpot         = (affin + ngap) - wkfn
c
c     Original : MRP      Stanford University       April, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real wkfn,affin,ngap,pgap
c
c...Neutral contact? 
      conpot=0.d0
      if(wkfn.eq.0.) goto 99
c
c...Contact has a work-function
      conpot=(affin+ngap)-wkfn
c
c...Pin at band edges 
      if(conpot.gt.ngap) conpot=ngap 
      if(conpot.lt.-pgap) conpot=-pgap 
c
c...Done
99    return
      end


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE POTRHS(inode,psi,rhs,lhs)
      include 'p2conf.h'
c 
c     This routine computes the rhs of the charge neutrality equation
c     and the derivative thereof w.r.t. potential. 
c     NOTE: This psi is referenced to the quasi-fermi level since qf
c            is taken to be zero for this calculation.
c     
c     Called by: POTEN
c     
c     Calls to:  DIRAC, FERMIS
c 
c     Original : H.R.Yeager     Stanford University       July, 1985
c 
c     Copyright c 1985 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c..   common area
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------

c.....type declarations 
      integer inode
      double precision psi, rhs, lhs, dexpu
      
      double precision lgnie, Nc, Nv
      double precision argn, argp, Fnn, Fpp, dFnn, dFpp
      double precision tmp, Ndi, dNdi, Nai, dNai
c------------------------------------------------------------------
      
c..   cnie, nvband, and ncband are stored as reals and are scaled by dcscl
      lgnie = (lgcnie(inode))
      Nv    = dble(nvband)
      Nc    = dble(ncband)
      argn  =  psi + (lgnie - dlog(Nc))
      argp  = -psi + (lgnie - dlog(Nv))

      if (lboltz) then
        Fnn  = dexpu(argn)
        dFnn = Fnn
        Fpp  = dexpu(argp)
        dFpp = Fpp
      else
        call dirac(argn, Fnn, dFnn) 
        call dirac(argp, Fpp, dFpp)
      endif

c.. Get donor and acceptor ionizations. 
c.. Arguments are same for dirac and fermis
      if (lincom) then
c..     for ionized donors/aceptors
        call fermis(argn, dble(gcstar), Ndi, dNdi)
        call fermis(argp, dble(gvstar), Nai, dNai)
        Ndi  =  dble(ndconc(inode))*Ndi
        dNdi =  dble(ndconc(inode))*dNdi
        Nai  =  dble(naconc(inode))*Nai
        dNai = -dble(naconc(inode))*dNai
      else
c..     for no ionized donors/aceptors
        Ndi  =  dble(r1(inode))
        dNdi =  0.0d0
        Nai  =  0.0d0
        dNai =  0.0d0
      endif

c..   write (*,*) 'inode=',inode,'Nd=',Ndi,'Na=',Nai
      rhs   =  Ndi - Nc*Fnn
      tmp   =  Nv*Fpp - Nai
      rhs   =  rhs   + tmp
      lhs   = (-Nv*dFpp - Nc*dFnn + dNdi - dNai )
      return 
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE POTEN(lbstat,inode,doping,cni,dn,dp,dphi) 
      include 'p2conf.h'
c 
c     This routine computes the neutral potential and 
c     carrier conc. based on doping.
c 
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP            Stanford University       Nov, 1983
c     Revision : HRY (Re-written. Links into stats.f and self-consistent
c                    solution for Incomplete-ionization.) Jun. 1985
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c common area 
c 
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c type declarations 
c 
      logical lbstat
      integer inode
      real doping,cni
      double precision dn,dp,dphi
      
      integer           psisgn, Ntits
      double precision  nie, lgnie, Nc, Nv, Nai, Ndi, gamma
      double precision  argn, argp, targ
      double precision  tmp, psi, dpsi, rhs, lhs
      double precision  Tk, Kk, rhs0, dpsi0, intspi, del
      double precision NOCCUP, POCCUP
c 
c****************************************************************** 
c 
c                   start 
c 
c...Calculate psi,n,p using Fermi-Dirac statistics
c
      lgnie = lgcnie(inode)
      nie   = dble(  cnie(inode))
      Nv    = dble(nvband)
      Nc    = dble(ncband)
      del   = 0.5d0
      
c..   for Initial guess for iteration (to follow), Take psi == quasi_fermi
c..   level. This is essential for getting incomplete ionization to work.
c...  If you add more trap levels, Including them here is ESSENTIAL!
      argn =  lgnie - dlog(Nc)
      argp =  lgnie - dlog(Nv)
      if (lincom) then
c..     for ionized donors/aceptors
        call fermis(argn, dble(gcstar), Ndi, tmp)
        call fermis(argp, dble(gvstar), Nai, tmp)
        Ndi  =  dble(ndconc(inode))*Ndi
        Nai  =  dble(naconc(inode))*Nai
      else
c..     for no ionized donors/aceptors
        Ndi  =  dble(r1(inode))
        Nai  =  0.0d0
      endif

c..   Do a bullit proof initial guess. This stupid statement 
c..   is for low temp. The fortran legacy lives on. \/
      
      tmp  = (Ndi-Nai)
      if ( dlog(dabs(tmp)) .GT. (42.0d0+lgnie) ) then
        if (tmp .GT. 0.0D0) then
          psisgn=1
          if (lboltz) then
             targ = 1.0d0
          else 
             targ = gamma((Ndi-Nai)/Nc)
          endif
          psi = dlog(tmp)-dlog(targ)-lgnie
        else
          psisgn=0
          if (lboltz) then
             targ = 1.0d0
          else
             targ = gamma(dabs(Ndi-Nai)/Nv)
          endif
          psi = -(dlog(dabs(tmp))-dlog(targ)-lgnie)
        endif
        goto 90 
      endif

c..   end of this stupid fortran legacy.            /\


      tmp  = (Ndi-Nai)/nie
      if (tmp.GE.0.0d0) then 
        psisgn=1
        if (lboltz) then
           targ = 1.0d0
        else 
           targ = gamma((Ndi-Nai)/Nc)
        endif
        tmp = tmp/targ
        psi = dlog(0.5d0*(tmp+ dsqrt(tmp*tmp+4.0d0/targ)))
      else 
        psisgn=0
        if (lboltz) then
           targ = 1.0d0
        else
           targ = gamma(dabs(Ndi-Nai)/Nv)
        endif
        tmp =  tmp/targ
        psi=-dlog(0.5d0*(-tmp+dsqrt(tmp*tmp+4.0d0/targ)))
      endif 
    
   90 intspi = psi
  
      call potrhs(inode,psi,rhs0,lhs)
      Kk = 0.5d0/(dabs(rhs0)+1.0d0)
      Ntits=0
  
c..   Newton interation loop. Done with, what else?, goto's. arg!
      
  100 call potrhs(inode,psi,rhs,lhs) 
      dpsi  = rhs/lhs
      dpsi0 = dpsi
      Ntits = Ntits+1
    
c..   retry entry point for bank-rose damping. (algorithm Global)
  110 Tk    = 1.0d0/(1.0d0+dabs(Kk*rhs))
      tmp   = psi - Tk*dpsi

c..   Keep Psi within bounds. Must do because the are two basic, valid roots. 
      if (psisgn.EQ.1) then
        if (tmp.LT.0.0d0)  then
           dpsi = psi/(2.0d0*Tk)
        endif
      else
        if (tmp.GT.0.0d0)  then
           dpsi = psi/(2.0d0*Tk) 
        endif
      endif
    
      psi  = psi- Tk*dpsi
      rhs0 = dabs(rhs)
      call potrhs(inode,psi,rhs,lhs)
    
      if ( (1.0d0-dabs(rhs)/(rhs0+1.0d0))/Tk .GT. del ) then
        Kk = Kk/2.0d0
        Tk  = 1.0d0/(1.0d0+dabs(Kk*rhs0))
      else
c..     Reject update, Increse damping, & goto retry. But first see if
c..     converged. (rounding error may cause plateau in rhs.)
        if (dabs(dpsi0/( dabs(psi)+1.0d0)) .LT. 1.0d-10) goto 120
        psi = psi+ Tk*dpsi
        Kk  = Kk*8.0d0
        goto 110
      endif
c..   end Newton loop
      if ( dabs(dpsi0/(dabs(psi)+1.0d0)) .GT. 1.0d-10 ) goto 100

c..   Panic exit point for Newton loop
  120 continue 

c..   Output psi, n, and p thru dphi, dn, dp. 
      dphi=psi
      dn = NOCCUP(lboltz,0.0d0,psi,lgcnie(inode),ncband)
      dp = POCCUP(lboltz,0.0d0,psi,lgcnie(inode),nvband)
c..   write(*,*) 'psi=',psi,'n=',dn,'p=',dp
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ADJQF(vmin,vmax,nwbias)
      include 'p2conf.h'
c
c     Adjust minority carrier quasi-fermi potential.
c
c     Original : MRP        Stanford University       Apr, 1984
c     Modified : CSR  (Change of ecode definition)    Aug, 1984
c     Modified : HRY  (Links into stats.f)            Jun, 1985
c
c     CALLED FROM: IGUESS(method.f)
c     CALLS TO   : POCCUP, NOCCUP (method.f)
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
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical le1,lnbar,lpbar
      integer i,ie,code,n1elec
      double precision locqf(20),vmin,vmax,lqf
      double precision nwbias(1)
      double precision NOCCUP, POCCUP, QFNN, QFPP
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...What AREN'T we solving for?
      lnbar=l1hole.or.ncarr.eq.0
      lpbar=(.not.l1hole.and.ncarr.eq.1).or.ncarr.eq.0
c
c...Electrons or holes?
      if(l1hole) goto 200
c
c-------------
c  FIX HOLES
c-------------
c
c...If user fixed, set to defined value or to minimum bias in
c...the device
      if(.not.lfixqf.and.(pbias.eq.-999.d0)) goto 110
      if(lfixqf) then
         lqf=vmin
      else
         lqf=pbias
      endif
      do 105 i=1,np
         if(mattyp(itype(i)).le.0) goto 105
         if((lm(i).eq.0).or.lpbar) qfp(i)=lqf
105   continue
      if(ncarr.eq.0) goto 200
      goto 998
c
c...We have to do the work - set up local qf array
110   do 120 i=1,ndreg
c
c...Find connecting electrode and use bias - 
c...careful if more than one and they are different 
c...(use min and warn if doped p-type)
      code=ecode(i)
      if(code.gt.nelect) goto 115
       locqf(i)=nwbias(code)
       goto 120
c
115   n1elec = nelect+1
      le1 = .true.
116   ie = mod (code,n1elec)
      code = code / n1elec

      if (ie .ne. 0) then
         if(le1) then
            lqf=nwbias(ie)
            le1=.false.
         else
            if(lqf.ne.nwbias(ie)) then
               if(nwbias(ie).lt.lqf) lqf=nwbias(ie)
               if(dopsgn(ie).lt.0.) call erset(-132,linum,0)
            endif
         endif
      endif
      if (code .gt. 0) goto 116
      locqf(i)=lqf
c
c...Next region
120   continue
c
c...Adjust qfs (only in regions where bias has changed)
c...Also, do not adjust if electrode and we are solving for holes
      do 130 i=1,np
         if((lm(i).gt.0).and.(.not.lpbar)) goto 130
         if(mattyp(itype(i)).le.0) goto 130
         ie=eptr(i)
         if(ie.ne.0) goto 131
         qfp(i)=vmin
         goto 130
131      if(r1(i).gt.0.) then
            qfp(i)=vmin
         else
            qfp(i)=locqf(ie)
c..         Abuse the function QFPP with 2 minus signs; it does work!
            if(local) fv(i) = -QFPP(lboltz,fp(i),-qfp(i),lgcnie(i),
     1                                            nvband,fn(i))
         endif
130   continue
      if(ncarr.eq.1) goto 998
c
c-----------------
c  FIX ELECTRONS
c-----------------
c
c...If user fixed, set to defined value.  If no value was given, set
c...to maximum voltage in system
200   if(.not.lfixqf.and.(nbias.eq.-999.d0)) goto 210
      if(lfixqf) then
         lqf=vmax
      else
         lqf=nbias
      endif
      do 205 i=1,np
        if(mattyp(itype(i)).le.0) goto 205
        if((lm(i).eq.0).or.lnbar) qfn(i)=lqf
205   continue
      goto 998
c
c...We have to do the work - set up local qf array
210   do 220 i=1,ndreg
c
c...Find connecting electrode and use bias - 
c...careful if more than one and they are different 
c...(use max and warn if doped n-type)
      code=ecode(i)
      if(code.gt.nelect) goto 215
      locqf(i)=nwbias(code)
      goto 220
c
215   n1elec=nelect+1
      le1 = .true.
216   ie = mod (code, n1elec)
      code = code / n1elec
      
      if (ie .ne. 0) then
         if(le1) then
            lqf=nwbias(ie)
            le1=.false.
         else
            if(lqf.ne.nwbias(ie)) then
               if(nwbias(ie).gt.lqf) lqf=nwbias(ie)
               if(dopsgn(ie).gt.0.) call erset(-133,linum,0)
            endif
         endif
      endif
      if(code .gt. 0) goto 216
      locqf(i)=lqf
c
c...Next region
220   continue
c
c...Adjust qfs (only if bias for region has not changed)
c...Dont adjust if electrode and solving for electrons
c...Set to vmax for p-type regions
      do 230 i=1,np
         if((lm(i).gt.0).and.(.not.lnbar)) goto 230
         if(mattyp(itype(i)).le.0) goto 230
         ie=eptr(i)
         if(ie.ne.0) goto 231
         qfn(i)=vmax
         goto 230
231      if(r1(i).lt.0.) then
            qfn(i)=vmax
         else
            qfn(i)=locqf(ie)
c..         Abuse the function QFNN with 2 minus signs; it does work!
            if(local) fv(i) = -QFNN(lboltz,fn(i),-qfn(i),lgcnie(i),
     1                                            ncband,fp(i))
         endif
230   continue
c
c...Get n,p conc.
998   do 997 i=1,np
         if(mattyp(itype(i)).le.0) goto 997
         fn(i) = NOCCUP(lboltz,qfn(i),fv(i),lgcnie(i),ncband)
         fp(i) = POCCUP(lboltz,qfp(i),fv(i),lgcnie(i),nvband)
997   continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BILIMS(nwbias,vmin,vmax)
      include 'p2conf.h'
c
c     Find max/min bias voltages at semiconductor contacts only.
c
c     Orig: MRP Sept. 1984
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
      double precision nwbias(1),vmin,vmax,ridic
      integer i,j,ipt
      data ridic /1.e20/
c------------------------------------------------------------------
c
c...Initialize
      vmin=ridic
      vmax=-vmin
c
c...Loop through electrodes
      do 105 i=1,nelect
      if(lcurbc(i)) goto 105

      do 106 j=1,nb
      ipt=nbc(j)
      if(lm(ipt).eq.i) goto 107
106   continue
      write(6,*) 'Internal error in vmin/vmax calculation, elect =',i
      write(6,*) 'nb=',nb,'  nelect=',nelect
      stop
c
c...Oxide or semiconductor contact? - Skip oxide
107   if(mattyp(itype(ipt)).le.0) goto 105
      vmin=dmin1(vmin,nwbias(i))
      vmax=dmax1(vmax,nwbias(i))

105   continue
c
c...Bye
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FIXSCH
      include 'p2conf.h'
c
c     Restore n,p concentrations at all Schottky contacts from
c     previous bias point.
c
c     Orig: MRP Oct 84
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
      integer i,numelc,numpnt
c------------------------------------------------------------------
c
c---------
c  START
c---------
c
      do 10 i=1,nb
      numelc=ietype(i)
      if(.not.schotk(numelc)) goto 10
      numpnt=nbc(i) 
      fn(numpnt)=ofn(numpnt)
      fp(numpnt)=ofp(numpnt)
10    continue
      return
      end
