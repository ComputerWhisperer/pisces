cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NEWTON(lback,lflag)
      include 'p2conf.h'
c
c     Newton method solution controller.
c
c     Original : MRP          Stanford University       Nov, 1983
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'logunt.h'
      include     'sol.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lflag,lback
      real elap
      integer idum,ii,i,ierr
      double precision dum
      character*20  tmpbuf
c
c FUNCTIONS:
      logical iscval
c 
c****************************************************************** 
c 
c 
c...Initialize
      iconv=1
      lflag=.false.
      iterp=0 
      iterc=0 
      ninner=0
csor      if(iknot.ne.0) then
csor         do 5 i=1,neq
csor5        deltx(i)=0.d0
csor      endif
c
c...Matrix and rhs assembly - only done here on first Newton loop
c...(later loops are assembled in subroutine update)
      if(lcpu) call timer(1,lucpu,' ',elap)
      call assmb(lgumm,idum)
       
      if(errflg) return
      if(lcpu) then
         call timer(2,lucpu,'   Assembly ........',elap)
c        call belch(a)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Norms
      call newnrm
      call checkn(dum,dum,dum)
      if(lcpu) call timer(2,lucpu,'   Norm ............',elap)
c
c-----------------------------------------------------------------------
c
c                           NEWTON LOOP
c
 1100 continue
c
c...Scale linear system?
c...(only for full Newton now - not necessary for knot blocks)
      if(lscl) call rescl(a,rhs,x)
c
c...Dump a matrix, if requested
c...Must reinstall relevant code in solck, link dumpa, add parms to pisc.key
c...file if you REALLY want to do this.
c
c      if(namafl(1:1).ne.' ') then
c         call dumpfl(namafl,neq,ia,ja,a,x,lm)
c         call incnm(namafl)
c      endif
c
c...Print rhs?
c      if(namerr(1:1).ne.' ') then
c         call errwr(namerr,rhs(1),rhs(2),rhs(3))
c         call incnm(namerr)
c      endif
c
c...Print error norms, check convergence, print currents
      call prnn(lflag)
      if(iconv.ge.3) goto 300
      if(lback) return
      if(lcpu) write(lucpu,9800) iterp
9800  format(/'Newton loop #',i2)
      if(lprntj) call qaprn
c
c...Block by knots or full Newton
c      if(iknot.ne.0) goto 5000
c
c-------------
c FULL NEWTON
c-------------
c
c...Newton-Richardson - should we skip LU?
      if(luauto) then
         call nwtrch(lflag)
      else
         lflag=.false.
      endif
c
c...LU factorization
      if(.not.lflag) then
         if(lcpu) call timer(1,lucpu,' ',elap)
         call vmnpd(neq,a,dll,duu,di,x,
     +              ja,jl,ju,ia,il,iu,iva,ivl,ivu,ipc,ipri)
         if(lcpu) call timer(2,lucpu,'   LU decomp .......',elap)
         if(errflg) return
      endif
c
c...Back solve
      if(lcpu) call timer(1,lucpu,' ',elap)
      call vmbpcd(neq,dll,duu,rhs,di,x,deltx,
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
      if(errflg) return
      if(lcpu) then
         call timer(2,lucpu,'   Back solve ......',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
      if(ldbug2) then
          write(16,*) 'RHS at iter = ',iterp
          write(16,655) (deltx(i),i=1,neq)
655       format(4(1pe17.6))
      endif
c      goto 6000
c
c-----------------------
c       This is all garbage; should be deleted very soon, 
c       along with the goto 5000 at line 107 and the above
c       goto 6000, which was use to completely skip over
c       this body of code in rev. 8940a, 8822 and earlier.
c-----------------------
c       BLOCK/ILU CGR(6)
c       Use knot-block preconditioning up to the gloop'th newton,
c       then switch to equation-block preconditioning.
c       If the equation-blocks give trouble, swtich back.
c-----------------------
c
c 5000 continue
cc
c      if(iknot.eq.1 .or. iterp.le.gloops) then
c       call iludv
c      else
c       call ilubdv(lstuck)
c       if(errflg) return
c         if(lcontn.and.lstuck) then
c            lback=.true.
c            return
c         endif
c         if(lstuck.and.gloops.gt.-10) call iludv
c      endif
c
c      if(errflg) return
c
cc.......Copy solution (rhs) to a safer place.
c      do 5050 i=1,neq
c 5050   deltx(i)=rhs(i)
cc
cc---------------------------------------------------------------------
cc
cc                      UPDATE AND RE-ASSEMBLY
cc
c 6000 continue
c
c...Dump updates if wanted - for debugging block
      if (iscval(4)) then
         call gtcval(4, tmpbuf, LEN(tmpbuf))
         if(iterp.eq.0)
     +         call fopcl(12,tmpbuf,LEN(tmpbuf),17,.false.,ierr)
         ii=1
       write(16,*) 'XXX',iterp
         do 8787 i=1,np
         write(16,*) ii,deltx(ii),deltx(ii+1),deltx(ii+2)
         ii=ii+nmult
8787     continue
      endif
c
c...Go at it!
      call update(lback)
      goto 1100
c
c...Done 
300   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NWTRCH(lflag)
      include 'p2conf.h'
c
c     Newton-Richardson Jacobian update analysis.
c
c     Original : MRP          Stanford University       Nov, 1983
c     Revision : HRY          Fixed divide by 0 error.  Jun, 1987
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
c------------------------------------------------------------------
c
      logical lflag, dohole, doelec
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      lflag=.false.
c
c...If we haven't reached initial loop requirement, don't bother
      if(iterp.lt.nrloop) return
c
c...If this is loop zero, ok by defn.
      if(iterp.eq.0) then
         lflag=.true.
c
c...First check ratio of Poisson residuals
      else if(gvmax.le.cupdt*gvmax0) then
         lflag=.true.
c
c...If not, check if Poisson has already converged.  
c...If Poisson has not converged, must update Jacobian.
c...If Poisson has converged check continuity residuals
c...and then Poisson updates.
      else if(gvmax.lt.ptolg) then
         dohole = (ncarr.eq.1 .and. l1hole)      .or. (ncarr.eq.2)
         doelec = (ncarr.eq.1 .and. .not.l1hole) .or. (ncarr.eq.2)
         
         if(dohole) then
            if (gpmax.le.cupdt*gpmax0) lflag=.true.
         endif
         if(doelec) then
            if (gnmax.le.cupdt*gnmax0) lflag=.true.
         endif
         
         if(iterp.gt.1) then
            if(lxnorm.and.(dvmax.le.cupdt*dvmax0)) lflag=.true.
         endif

      endif
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE UPDATE(lback)
      include 'p2conf.h'
c
c     The following subroutine updates psi, n (and p) for a full 
c     Newton step.  The routine also calculates the electrode
c     electric flux and current.  
c
c     Original : MRP          Stanford University       Nov, 1983
c     Revision : HRY (link into stats.f )               Jun. 1985
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      logical lback
      double precision deltav,tdamp,relerr,denom,denomn,denomp
      double precision tmp1,dvmax1,dnmax1,dpmax1,dtt
      double precision maxcc,vrest(MAXCNT)
      real elap
      integer ii,i,jdamp,ibc,idum,inneg,ipneg,nodmat,ipmax,inmax,ivmax
      double precision POCCUP, NOCCUP
c
c**********************************************************************
c
c                           start
c
c...Define the maximum physical concentration (10^25 for any material)
      maxcc = 1.e25*dcscli 

c...Initialize
      tdamp=1.d0
      iterp=iterp+1
      iterc=iterc+1
      gvmax0=gvmax
      gnmax0=gnmax
      gpmax0=gpmax
      dvmax1=0.d0
      dnmax1=0.d0
      dpmax1=0.d0
      ivmax=0
      inmax=0
      ipmax=0
c
c...Save old psi,n,p (qfn,qfp)
      do 2 i=1,np
         fv0(i)=fv(i)
         fn0(i)=fn(i)
         fp0(i)=fp(i)
    2 continue
      do 3 i=1,nelect
         vrest(i)=vres(i)
    3 continue
c
c...Initialize damping coeff's
      jdamp=1
c      if(ldjr) then
c         tdamp0=fdamp/(fdamp+kdamp*gvmax0)
c         tdamp=tdamp0
c      endif
c---------------
c  Update loop
c---------------
cCmje: Used for the damping code below....
c40    continue
      if(lcpu) call timer(1,lucpu,' ',elap)
      ii=0
      inneg=0
      ipneg=0
      do 100 i=1,np
      nodmat=mattyp(itype(i))
c
c...If this is a boundary (and non-Schottky/lumped parameter/contact
c...resistance/current bc) node, we don't update
      ibc=lm(i)
      if(ibc.eq.0) goto 101
      if(schotk(ibc).or.lresis(ibc).or.
     +                 lcnres(ibc).or.lcurbc(ibc)) goto 101
      ii=ii+nmult
      goto 100
c
c...Poisson
101   ii=ii+1
      dtt = deltx(ii)
      if(jdamp.eq.1) then
         tmp1=dabs(deltx(ii))
         if(tmp1.gt.dvmax1) then
            dvmax1=tmp1
            ivmax=i
         endif
      endif
      deltav=dmax1(-maxexp,dmin1(maxexp,deltx(ii)*tdamp))
      fv(i)=fv0(i)+deltav
c
c...Electron continuity
      if(.not.l1hole) goto 151
c...  If solving for holes only, fix electron conc.
      if(nodmat.gt.0) fn(i)=NOCCUP(lboltz,qfn(i),fv(i),lgcnie(i),ncband)
      goto 55
c
151   ii=ii+1
      dtt=deltx(ii)
      if (abs(dtt) .gt. maxcc) then
         dtt = dsign (maxcc, dtt)
         if(ldebug) 
     +     write(6,*) 'Warning : max n conc exceeded at node ',i
      endif
      fn(i)=fn0(i)+dtt

      if(fn(i).lt.0.d0) then
         fn(i)=fn0(i)*nfact
         inneg=inneg+1
      endif
c
      if(jdamp.eq.1) then
         tmp1=relerr(fn0(i),fn(i),fp0(i),r1(i),denom)
         if(tmp1.gt.dnmax1) then
            dnmax1=tmp1
            inmax=i
            denomn=denom
         endif
      endif
c
c...Hole continuity
      if(nmult.eq.3) goto 55
c...  If solving for electrons only, fix hole conc.
      if(nodmat.gt.0) fp(i)=POCCUP(lboltz,qfp(i),fv(i),lgcnie(i),nvband)
      goto 100 
c
55    ii=ii+1
      dtt=deltx(ii)
      if (abs(dtt) .gt. maxcc) then
         dtt = dsign (maxcc, dtt)
         if(ldebug) 
     +     write(6,*) 'Warning : max n conc exceeded at node ',i
      endif
      fp(i)=fp0(i)+dtt

      if(fp(i).lt.0.d0) then
         fp(i)=fp0(i)*nfact
         ipneg=ipneg+1
      endif
c
      if(jdamp.eq.1) then
         tmp1=relerr(fp0(i),fp(i),fn0(i),r1(i),denom)
         if(tmp1.gt.dpmax1) then
            dpmax1=tmp1
            ipmax=i
            denomp=denom
         endif
      endif
c
c...Next node
100   continue
c
c...Any lumped-element contacts
      do 111 i=1,nelect
      if(lresis(i).or.lcurbc(i)) then
         ii=nresis(i)
         tmp1=dabs(deltx(ii))
         if(tmp1.gt.dvmax1) then
            dvmax1=tmp1
            ivmax=-ii+neq
         endif
         deltav=dmax1(-maxexp,dmin1(maxexp,deltx(ii)*tdamp))
         vres(i)=vrest(i)+tdamp*deltav
      endif
111   continue
c
      if(ldebug) then
         if(inneg.gt.0) write(6,1001) inneg
1001     format(' Warning - ',i4,
     +          ' electron concentrations less than zero')
         if(ipneg.gt.0) write(6,1002) ipneg
1002     format(' Warning - ',i4,
     +          ' hole concentrations less than zero')

         write(6,*) 'Max. rel. updates at ',ivmax,inmax,ipmax
         if(ivmax.gt.0) then
          write(6,*) 'v0,v               : ',fv0(ivmax),fv(ivmax)
          write(6,*) '    x,y            : ',cord(1,ivmax),cord(2,ivmax)
         else
          write(6,*) 'v0,v               : ',vrest(-ivmax),vres(-ivmax)
         endif
         write(6,*) 'n...n0,n           : ',fn0(inmax),fn(inmax)
         write(6,*) '    r1,p0          : ',r1(inmax),fp0(inmax)
         write(6,*) '    x,y            : ',cord(1,inmax),cord(2,inmax)
         write(6,*) 'p...p0,p           : ',fp0(ipmax),fp(ipmax)
         write(6,*) '    r1,n0          : ',r1(ipmax),fn0(ipmax)
         write(6,*) '    x,y            : ',cord(1,ipmax),cord(2,ipmax)
         write(6,*) 'denomn,denomp,abstl: ',denomn,denomp,dcscli
      endif
      if(lcpu) call timer(2,lucpu,'      Update .......',elap)
c
c---------------
c  Re-assembly
c---------------
c
      if(lcpu) call timer(1,lucpu,' ',elap)
      call assmb(lgumm,idum)
      if(lcpu) then
         call timer(2,lucpu,'      Assembly .....',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Norms
      call newnrm
      call checkn(dvmax1,dnmax1,dpmax1)
      if(lcpu) call timer(2,lucpu,'      Norms ........',elap)
      if(iconv.ge.3) goto 999
c
c...If continuation, check if norms went up - if so, leave
      if(lcontn.and.gvmax.gt.gvmax0.and.gnmax.gt.gnmax0.and.
     +   gpmax.gt.gpmax0.and.iterp.gt.1) lback=.true.
      goto 999
c
c
c Damping - doesnt work now, so skip
c
cCmje: FROM HERE (to below), THIS CODE ISN'T REACHED B/C of the
cCmje:  goto 999 above.
cc
c99999 if(.not.ldjr.or.lback) goto 999
cc
cc...Check ratio of norms to determine if more damping is required
c      tmp=(1.d0-gvmax/gvmax0)/tdamp
c      if(l1hole) goto 441
c      tmp=dmax1(tmp,(1.d0-gnmax/gnmax0)/tdamp)
c      if(ncarr.eq.1) goto 442
c441   tmp=dmax1(tmp,(1.d0-gpmax/gpmax0)/tdamp)
c442   if(ldebug) then
c         write(6,*) ' jdamp,tdamp,ratio : ',jdamp,tdamp,tmp
c         write(6,*) ' gvmax,gnmax,gpmax : ',gvmax,gnmax,gpmax
c      endif
c      if(tmp.ge.ddamp) goto 500
cc
cc...More damping required - 
cc...If first re-try, get norms of non-updated vectors for calculation
cc...of a new damping coefficient
c      jdamp=jdamp+1
c      if(jdamp.gt.ldamp) call erset(288,linum,0)
c      if(errflg) return
cc
cc...Calculate new damping coefficient and go back to update loop
c      tmp=(dble(jdamp-1)/dble(ldamp-1))**pdamp
c      tdamp=tdamp0*(mudamp/dvmax1)**tmp
c      goto 40
cc
cc...Save kdamp/fdamp to calculate initial damping coefficient next time
c500   kdamp=(1.d0/tdamp-1.d0)/gvmax0
cCmje: (from above) TO HERE, THIS CODE ISN'T REACHED!
c
c...Auf wiedersehen
999   if(lxnorm) then
         dvmax0=dvmax
         dnmax0=dnmax
         dpmax0=dpmax
         dvmax=dvmax1
         dnmax=dnmax1
         dpmax=dpmax1
      endif
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION RELERR(cona0,cona1,conb0,dop,denom)
      include 'p2conf.h'
c
c     Calculate relative change in carrier concentration
c     between Newton loops.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'setup.h'
c------------------------------------------------------------------
      real dop
      double precision cona0,cona1,conb0,denom
c------------------------------------------------------------------
c
c
c...Calculate "error"
      denom=dmax1(dcscli,cona0)
      relerr=dabs(cona1-cona0)/denom
c
c...Bye
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CHECKN(dvmax1,dnmax1,dpmax1)
      include 'p2conf.h'
c
c     The following subroutine checks convergence of the rhs
c     for the full or block Newton.
c
c     Original : MRP          Stanford University       Feb, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'sol.h'
      include     'setup.h'
c------------------------------------------------------------------
c
      double precision dvmax1,dnmax1,dpmax1
      logical lcn,lcp
c------------------------------------------------------------------
c
c...Initialize
      lpconv=.false.
      lcconv=.false.
c
c...Which criterion?
      if(lxnorm) goto 100
c
c...Check convergence
      if(gvmax.lt.ptolg) lpconv=.true.
      if((gnmax.lt.ctolg).and.(gpmax.lt.ctolg)) lcconv=.true.
      goto 200
c
100   if(iterp.eq.0.and.itlmt.ne.0) return
      if(dvmax1.lt.ptolx) lpconv=.true.
      lcn=dnmax1.lt.ctolx
      lcp=dpmax1.lt.ctolx
      lcconv=lcn.and.lcp
      goto 200
c
c...Check for convergence, max Newton loops exceeded or just a general
c...bad bias pt.
200   if((lpconv.and.lcconv).or.(iterp.ge.itlmt)) iconv=3
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PRNN(lflag)
      include 'p2conf.h'
c
c     Subroutine to print out error norms.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1983
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'sol.h'
      include     'setup.h'
      include     'logunt.h'
c------------------------------------------------------------------
c
      logical lflag
      integer lu
      character*1 ch
c------------------------------------------------------------------
c
c------------------------
c  PRINT RELATIVE ERROR
c------------------------
c
      ch=' '
      if(lflag) ch='*'
      lu=luinf
c
1020  if(iterp.eq.0) write(lu,1101)
1101  format(/'  iter    psi-error     n-error      p-error')
      if(lxnorm) then
         if(iterp.gt.0) write(lu,1100) iterp,ch,dvmax,dnmax,dpmax,'   '
         if(lgnorm) write(lu,1100) iterp,ch,gvmax,gnmax,gpmax,'RHS'
      else 
         write(lu,1100) iterp,ch,gvmax,gnmax,gpmax,'   '
      endif
1100  format(i5,a1,1x,3(1pe13.4),2x,a3)
      if(lu.eq.luout) goto 1199
      lu=luout
      goto 1020
c
2000  if(iterp.eq.0) write(lu,2101)
2101  format(/'  iter    psi-error     n-error      p-error',
     +           '    inner loops')
      if(lxnorm) then
         if(iterp.gt.0) write(lu,2100) iterp,ch,dvmax,dnmax,dpmax,
     +                                 ninner,'   '
         if(lgnorm) 
     +      write(lu,2100) iterp,ch,gvmax,gnmax,gpmax,ninner,'RHS'
      else
         write(lu,2100) iterp,ch,gvmax,gnmax,gpmax,ninner,'   '
      endif
2100  format(i5,a1,3(1pe13.4),5x,i4,2x,a3)
      if(lu.eq.luout) goto 1199
      lu=luout
      goto 2000
c
c...Bye
1199  return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DMPCK(gmax,gmax0,tdamp,ddamp,ldmp)
      include 'p2conf.h'
c
c------------------------------------------------------------------
      include     'blank.h'
      double precision gmax,gmax0,tdamp,ddamp,tmp
      logical ldmp
c------------------------------------------------------------------
c
      tmp=(1.d0-gmax/gmax0)/tdamp
      if(tmp.lt.ddamp) ldmp=.true.
      if(ldebug) write(6,*) 'tdamp,ratio : ',tdamp,tmp
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NEWNRM
      include 'p2conf.h'
c
c     Calculate full Newton norm, including lumped components and
c     contact resistance.
c
c     Orig: MRP Jan 85
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
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i,ii,iel
      double precision dxtn,dxtp
      logical ltmp
c------------------------------------------------------------------
c
c...Start
      dxtn=0.d0
      dxtp=0.d0
c
c...Poisson - include contact resistance bc eqns as part of
c...continuity norm
      ii=1
      do 10 i=1,np
      iel=lm(i)
c...don't access teh lcnres() array if it will be illegal
      ltmp=.false.
      if (iel.gt.0) ltmp=lcnres(iel)
      if (ltmp) then
         if(r1(i).gt.0.) then
            dxtn=dmax1(dxtn,dabs(rhs(ii)))
         else
            dxtp=dmax1(dxtp,dabs(rhs(ii)))
         endif
         x(ii)=0.d0
      else
         x(ii)=rhs(ii)
      endif
      ii=ii+nmult
10    continue
      call rnorm(1,x,fv,gvmax,99)
c
c...Continuity
      if(l1hole) then
         call rnorm(3,rhs(2),fp,gpmax,99)
      else 
         call rnorm(2,rhs(2),fn,gnmax,99)
         if(ncarr.eq.2) call rnorm(3,rhs(3),fp,gpmax,99)
      endif
c
c...Extra equations for lumped elements
      do 20 i=1,nelect
      if(lresis(i).or.lcurbc(i)) then
         if(dopsgn(i).gt.0.) then
            dxtn=dmax1(dxtn,dabs(rhs(nresis(i))))
         else
            dxtp=dmax1(dxtp,dabs(rhs(nresis(i))))
         endif
      endif
20    continue
c
c...Include extra continuity terms
      gnmax=dmax1(gnmax,-djcoef*dxtn)
      gpmax=dmax1(gpmax,-djcoef*dxtp)
c
c...Bye
      return
      end
