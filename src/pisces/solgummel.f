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
      SUBROUTINE GUMMEL
      include 'p2conf.h'
c
c     Gummel method solution controller.
c
c     Original : MRP          Stanford University       Nov, 1984
c     Modified : CSR (added ICCG)                       Mar, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'soltmpg.h'
      integer TMPPAD(1005002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c 
c                   type declarations 
c 
      logical lnewg,litex
      real elap
      integer npde,icarr,esiz,i
      double precision asfac
c 
c****************** Start ***************************************** 
c 
c 
c...Initialize
      iconv=1
      iterp=0 
      iterc=0 
      res00=0.d0
      lnewg=.true.
      if (icgflg) call ijexp(neq,ja,ia,cia,cja,esiz)
      if(dvlmt.le.0.d0) then
        if(ldjr) then
           dvlmt=dvlmt2*dqkt
        else
           dvlmt=dvlmt1*dqkt
        endif
      endif
c
c-----------
c  POISSON
c-----------
  110 if(lcpu) write(lucpu,9805) iterc+1,' '
 9805 format(/'Gummel loop #',i3/'   Poisson equation :',a1)
c
c...If first iteration in poisson part of gummel loop, do 
c...assembly - after first loop, assembly is
c...done in subroutine updtp
      if(lcpu) call timer(1,lucpu,' ',elap)
      call assmb(lgumm,1)
      if(lcpu) then
         call timer(2,lucpu,'      Assembly .....',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Check norm
      call checkp(rhs,lnewg)
      if(lcpu) call timer(2,lucpu,'      Norms ........',elap)
      lnewg=.false.
c
115   if(lcpu) write(lucpu,9820) iterp+1
9820  format('      Poisson iteration #',i3)
c
c...Dump a matrix, if requested
c      if(namafl(1:1).ne.' ') then
c         call dumpfl(namafl,neq,ia,ja,a,x,lm)
c         call incnm(namafl)
c      endif
c
c...LU or pre-condition for ICCG
      if(lcpu) call timer(1,lucpu,' ',elap)
      if (.not.icgflg) then
         if(lscl) call rescl(a,rhs,x)
         call vmnpd(neq,a,l,u,di,x,
     +              ja,jl,ju,ia,il,iu,iva,ivl,ivu,ipc,ipri)
      else
         call icp1(neq,esiz,cia,ilsh,cja,a,aw,l,di,x)
      endif
      if(errflg) return
      if(lcpu) then
         call timer(2,lucpu,'      LU decomp ....',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Back solve or ICCG.  
c...MUST scale ICCG because it uses 2-norm, squaring the
c...size of everything. Then unscale at end.
c...Remember - if ICCG fails, use direct LU.
      if (.not.icgflg) then
         call vmbpcd(neq,l,u,rhs,di,x,rhs,
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
      else
         litex=.false.
         do 1414 i=1,neq
            x(i)=rhs(i)
1414     continue
         call ascale(.true.,rhs,asfac,neq)
         call icp2(neq,cia,ilsh,cja,a,l,di,rhs,resid,
     +             p,ap,qinvr,aqinvr,asfac,mxiccg,litex)
         if(litex) then
            write(*,*) 'Warning - ICCG not converging'
            write(*,*) 'Switching to direct LU for this iteration'
            do 1415 i=1,neq
               rhs(i)=x(i)
1415        continue
            if(lscl) call rescl(a,rhs,x)
            call vmnpd(neq,a,l,u,di,x,
     +              ja,jl,ju,ia,il,iu,iva,ivl,ivu,ipc,ipri)
            call vmbpcd(neq,l,u,rhs,di,x,rhs,
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
         else
            call ascale(.false.,rhs,asfac,neq)
         endif
      endif
      if(lcpu) call timer(2,lucpu,'      Back solve ...',elap)
c
c...Update and re-assembly
      call updtp
      lnewg=.true.
      goto (115,200,300) iconv
c 
c--------------
c  CONTINUITY
c--------------
c
200   do 249 icarr=1,ncarr
      if(l1hole.or.(icarr.eq.2)) then
         npde=3
         if(lcpu) write(lucpu,9806) ' '
9806     format('   Hole continuity equation :',a1)
      else
         npde=2
         if(lcpu) write(lucpu,9807) ' '
9807     format('   Electron continuity equation :',a1)
      endif
c
c...Assembly
      if(lcpu) call timer(1,lucpu,' ',elap)
      call assmb(lgumm,npde)
      if(lcpu) call timer(2,lucpu,'      Assembly .....',elap)
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
c...LU
      if(lcpu) call timer(1,lucpu,' ',elap)
      if(lscl) call rescl(a,rhs,x)
      call vmnpd(neq,a,l,u,di,x,
     +              ja,jl,ju,ia,il,iu,iva,ivl,ivu,ipc,ipri)
      if (errflg) return
      if(lcpu) then
         call timer(2,lucpu,'      LU decomp ....',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Back solve
      call vmbpcd(neq,l,u,rhs,di,x,rhs,
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
      if(lcpu) then
         call timer(2,lucpu,'      Back solve ...',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Update
      call updtc(npde,rhs)
      if(lcpu)  then
         call timer(2,lucpu,'      Update .......',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Check norms
      call checkc(npde,rhs)
      if(lcpu) call timer(2,lucpu,'      Norms ........',elap)
249   continue
c
c...End of continuity - first get qfs, then where to?
      if((iconv.ne.3).and.lprntj) call qaprn
      lnewg=.true.
      goto (110,110,300),iconv
c
c...Done
300   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CHECKP(rhsx,lnewg)
      include 'p2conf.h'
c 
c     The following routine checks the convergence of the Poisson
c     equation in the Gummel loop and prints error norms.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Jan, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c 
c                   type declarations 
c 
      logical lnewg
      double precision rhsx(1)
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize
      lpconv=.true.
      lcconv=.true.
c
c...Get infinity norm of the residual
      call rnorm(1,rhsx,fv,gvmax,99)
c 
c...Converged?
      if(lxnorm) then
         if(iterp.eq.0.or.dvmax.gt.ptolx) lpconv=.false.
      else if(lgnorm) then
         if(gvmax.gt.ptolg) lpconv=.false.
      endif
c 
c...Determine branch
c...Done if: poisson only and (at iter limit or (converged
c            and not going to limit))
      iconv=3 
      if (lpoiss.and.(iterp.ge.itlmt.or.(lpconv.and.itmode.ne.0)))
     +    goto 1000
c
c...Continuity next if: not poisson only and ((multiple 
c...poisson and converged) or single poisson) 
      iconv=2 
      if (.not.lpoiss.and.((lmultp.and.lpconv).or..not.lmultp))
     +    goto 1000
c
c...Otherwise, back to poisson
      iconv=1 
c 
c...If Poisson did not converge, print norms
1000  if(lpoiss.and.lpconv) goto 1001
      if(((iterp.gt.0).and.(.not.lnewg)).or.(iconv.ne.1)) goto 99
1001  call prnorm(iterp,iterc,.false.)
c
c...Done
99    return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE UPDTP
      include 'p2conf.h'
c 
c     This routine updates p, n and v based on the delta v
c     obtained from the Poisson solution for the Gummel method. 
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'soltmpg.h'
      integer TMPPAD(1005002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c 
c                   type declarations 
c 
      real elap
      double precision tmp,deltav,jdamp,tdamp,tdamp0
      double precision xnorm,unorm
      double precision dexpu
      integer i,ibc,idum
c 
c****************************************************************** 
c 
c                   start 
c
      iterp=iterp+1
      ibc=1
      tdamp=1.d0
      dvmax=0.d0
c
c...Save previous potentials,n,p,rhs
      do 100 i=1,np
      fv0(i)=fv(i)
      fn0(i)=fn(i)
      fp0(i)=fp(i)
100   deltx(i)=rhs(i)
cc      write(16,*) 'XXX RHS'
cc      do 102 i=1,neq
cc102   write(16,*) i,deltx(i)
c
c...Calculate damping coefficient
      jdamp=1
      if(ldjr) then
         gvmax0=gvmax
         tdamp0=fdamp/(fdamp+kdamp*gvmax0)
         tdamp=tdamp0
      endif
c---------------
c  Update Loop
c---------------
300   if(lcpu) call timer(1,lucpu,' ',elap)
      do 400 i=1,np 
c
c...Dont bother if a contact
      if(i.ne.nbc(ibc)) goto 401
      ibc=ibc+1
      goto 400
c
c...Update potential - limit update (do this even if we are damping 
c...the Newton updates to prevent overflow)
401   deltav=deltx(i)*tdamp
      deltav=dmin1(dvlmt,dmax1(deltav,-dvlmt))
      fv(i)=fv0(i)+deltav
      if(jdamp.eq.1) dvmax=dmax1(dvmax,dabs(deltx(i)))
c
c...Update concentrations (except for insulators)
      if (mattyp(itype(i)).lt.0) goto 400 
      tmp=dexpu(deltav) 
      fp(i)=fp0(i)/tmp 
      fn(i)=fn0(i)*tmp 
400   continue
c
      if(lcpu) call timer(2,lucpu,'      Update .......',elap)
c
c...If single Poisson, we don't need to reassemble and can't damp
      if(lmultp) goto 410
      iconv=2
      return
c
c---------------
c  Re-assembly
c---------------
c
410   if(lcpu) call timer(1,lucpu,' ',elap)
      call assmb(.true.,1)
      if(lcpu) then
         call timer(2,lucpu,'      Assembly .....',elap)
         call timer(1,lucpu,' ',elap)
      endif
c
c...Check rhs norm - if converged, get out
      call checkp(rhs,.false.)
      if(lcpu) call timer(2,lucpu,'      Norms ........',elap)
      if(lpconv.or.(.not.ldjr)) goto 600
c
c...Check ratio of norms to determine if more damping is required
      tmp=(1.d0-gvmax/gvmax0)/tdamp
      if(ldebug) write(6,*) 'gvmax,tdamp,ratio : ',gvmax,tdamp,tmp
      if(tmp.ge.ddamp) goto 500
c
c...More damping required
      jdamp=jdamp+1
      if(jdamp.gt.ldamp) call erset(288,linum,0)
      if(errflg) return
      if(jdamp.eq.2) then
         call norm(fv0,np,1,unorm,idum,99)
         call norm(deltx,np,1,xnorm,idum,99)
      endif
      tmp=(dble(jdamp-1)/dble(ldamp-1))**pdamp
      tdamp=tdamp0*(mudamp*unorm/xnorm)**tmp
      goto 300
c
c...Save kdamp/fdamp, shift gvmax
500   kdamp=(1.d0/tdamp-1.d0)/gvmax0
600   gvmax0=gvmax
c 
c...Done
      if(.not.lpconv) call prnorm(iterp,iterc,.false.)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CHECKC(npde,rhsx)
      include 'p2conf.h'
c 
c     This routine checks the convergence if the continuity equation
c     (electrons - npde=2; hole - npde=3) for the Gummel method.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Jan, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   type declarations 
c
      integer npde,icarr
      double precision rhsx(1)
c
c*************
c**  START  **
c*************
c 
c...Get infinity norm of residual and scale
      if(npde.eq.3) goto 10
      call rnorm(npde,rhsx,fn,gnmax,99)
      if(lxnorm) then
         lcconv=lcconv.and.(dnmax.le.ctolx)
      else 
         lcconv=lcconv.and.(gnmax.le.ctolg)
      endif
      goto 100
c
10    call rnorm(npde,rhsx,fp,gpmax,99)
      if(lxnorm) then
         lcconv=lcconv.and.(dpmax.le.ctolx)
      else 
         lcconv=lcconv.and.(gpmax.le.ctolg)
      endif
c
c...Check convergence
100   icarr=npde-1
      if(icarr.lt.ncarr) goto 999
      iterc=iterc+1
c
c...Determine branch value
c...Done if: at limit or (converg. and not going to limit 
c            and not first continuity solution) 
      iconv=3
      if(iterc.eq.itlmt) goto 998
      if(lpconv.and.lcconv.and.itmode.ne.0.and.iterc.ne.1) goto 998 
c
c...Back to poisson otherwise
      iconv=1
c
c...Done
998   call prnorm(iterp,iterc,.true.)
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE UPDTC(npde,rhsx)
      include 'p2conf.h'
c 
c     This routine updates the electron conc. based on the results of
c     the continuity eq. for the Gummel method.  It also calculates 
c     the contact currents.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP/CSR      Stanford University       Nov, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   type declarations 
c
      logical lpcont
      integer npde,icarr,i,ibc
      double precision rhsx(1)
c 
c****************************************************************** 
c 
c                   start 
c 
c...Update n/p conc.
      lpcont=(npde.eq.3)
      icarr=npde-1
      if(lpcont) goto 45
c
c...Electrons (protect against negative concentrations)
      dnmax=0.d0
      do 40 i=1,np
      ibc=lm(i)
      if((ibc.ne.0).and.(.not.schotk(ibc))) goto 40
      if (mattyp(itype(i)) .le. 0) goto 40
      if(rhsx(i).lt.-fn(i)) then
         fn(i)=fn(i)*nfact
      else
         fn(i)=fn(i)+rhsx(i)
      endif
      if (fn(i).ne.0.d0) dnmax=dmax1(dnmax,dabs(rhsx(i)/fn(i)))
40    continue
      goto 999
c
c...Holes (protect against negative concentrations)
45    dpmax=0.d0
      do 46 i=1,np
      ibc=lm(i)
      if((ibc.ne.0).and.(.not.schotk(ibc))) goto 46
      if (mattyp(itype(i)) .le. 0) goto 46
      if(rhsx(i).lt.-fp(i)) then
         fp(i)=fp(i)*nfact
      else
         fp(i)=fp(i)+rhsx(i)
      endif
      if (fp(i).ne.0.d0) dpmax=dmax1(dpmax,dabs(rhsx(i)/fp(i)))
46    continue
c 
c...Done
999   return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PRNORM(itp,itc,cflag)
      include 'p2conf.h'
c
c     Subroutine to print out error norms.
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'sol.h'
      include     'logunt.h'
c------------------------------------------------------------------
c
      logical cflag
      integer itc,itp,lu
c------------------------------------------------------------------
c
c...Print out iteration info to luinf (and opt. luout)
      lu=luinf
      if((lgnorm.and.(itp.eq.0)).or.
     +   (.not.lgnorm.and.lxnorm.and.(itp.eq.1))) write(lu,1002)
1002  format(/,1x,'  p-iter  c-iter    psi-error     n-error',
     +           '      p-error')
c
c...Are we writing to luout 
420   if(cflag) goto 430
      if(lxnorm) then
         if(itp.gt.0) write(lu,4100) itp,dvmax,'   '
         if(lgnorm) write(lu,4100) itp,gvmax,'RHS'
      else
         write(lu,4100) itp,gvmax,'   '
      endif
4100  format(1x,i5,11x,1pe13.4,30x,a3)
      goto 440
c
430   if(lxnorm) then
         if(itc.gt.0) write(lu,4300) itp,itc,dvmax,dnmax,dpmax,'   '
         if(lgnorm) 
     +      write(lu,4300) itp,itc,gvmax,gnmax,gpmax,'RHS'
      else
         write(lu,4300) itp,itc,gvmax,gnmax,gpmax,'   '
      endif
4300  format(1x,i5,i8,3x,3(1pe13.4),4x,a3)
c
c...Tried luout yet?
440   if (lu.eq.luout) goto 999
      lu=luout
      goto 420 
c
c...Done
999   return
      end
