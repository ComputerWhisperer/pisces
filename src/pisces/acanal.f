cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Aug  9 15:44:47 PDT 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ACANAL(freq0,lss,vss0,lsorac,tol,maxsor,omeg,ldolu,
     +                  fstep,nfstep,lfstep)
      include 'p2conf.h'
c
c     AC phasor analysis.  See Laux, TED October 1985.
c
c     Original : MRP    February 1985.
c     Modified : GDA    August, 1989. (cylindrical coords)
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'symme.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
      logical lsorac,lss(*),ldolu,lfstep
      integer i,ipt,iss,neq2,maxsor,ncol,nfstep,ifstep
      double precision freqx,freq,vss,ampss(MAXCNT,2),acflux(MAXCNT,2)
      double precision dmagac,omeg,tol,rad,gij(MAXCNT),cij(MAXCNT),fstep
      double precision vss0,freq0,yij(MAXCNT)
c
c*************
c**  START  **
c*************
c
c...Initialize
      rad=2.d0*3.1415926d0
      vss=vss0*dqkt
      neq2=neq+neq
      ifstep=0
c
c...Must do LU if NR was used and last Jacobian factorization 
c...was skipped
      if(ldolu) then
         if(ldebug) write(6,*) 'Assemble A and compute LU'
         call assmb(lgumm,i)
         call vmnpd(neq,a,dll,duu,di,x,
     +              ja,jl,ju,ia,il,iu,iva,ivl,ivu,ipc,ipri)
      endif
c
c...How many columns of the admittance matrix are we after?
      ncol=0
      do 5 i=1,nelect
         if(lss(i)) ncol=ncol+1
5     continue
c
c------------------
c  Frequency loop
c------------------
c
10    continue
      freq=freq0*rad
      freqx=freq*dqkt
c
c...Write header
      write(luinf,9040) vss0,freq0
      if(luinf.ne.luout) write(luout,9040) vss0,freq0
9040  format(1x,//'Ac analysis :'/
     +        5x,'Ac voltage =',1pe13.6/
     +        5x,'Frequency  =',1pe13.6,' Hz'/)
      if(llogac) then
         write(lulog2,8001) ncol,vss0,freq0,
     +                      (dktq*bias(i),i=1,nelect)
8001     format('*',i3,12e16.8)
      endif
c
c...Loop thru electodes
      do 700 iss=1,nelect
      if(.not.lss(iss)) goto 700
c
c...Assembly 
      do 100 i=1,neq2
         ssrhs(i)=0.d0
100   continue
      do 105 i=1,nb
         ipt=nbc(i)
         if(lm(ipt).eq.iss) ssrhs(nmult*(ipt-1)+1)=vss
105   continue
c
c...GCR or SOR
      if(lsorac) then
         call sorac(omeg,tol,maxsor,ssrhs,x1sor,x2sor,x3sor,
     +              x,dll,duu,di,freqx)
cc      else
cc         call gcrac(neq2,tol,ssrhs,freqx,bbr,bbp,bbap,bbqr,bbaqr,a)
      endif

cdb      ii=0
cdb      do 878 i=1,np
cdb         write(*,*) ' '
cdb         ii=ii+1
cdb         write(*,*) 'pt #',i,'  psi = ',ssrhs(ii),ssrhs(ii+neq)
cdb         ii=ii+1
cdb         write(*,*) '         n   = ',ssrhs(ii),ssrhs(ii+neq)
cdb         ii=ii+1
cdb         write(*,*) '         p   = ',ssrhs(ii),ssrhs(ii+neq)
cdb878   continue

c
c...Calculate currents 
      call accurr(freqx,ampss,acflux)
c
c...Output 
      write(luinf,9041) iss
      if(luinf.ne.luout) write(luout,9041) iss
9041  format(/'Electrode #',i2)
      IF (LCYL.or.lwidth) THEN
         write(luinf,9044)
         if(luinf.ne.luout) write(luout,9044)
      ELSE
         write(luinf,9045)
         if(luinf.ne.luout) write(luout,9045)
      ENDIF
9044  format(/' Electrode',7x,'Conduction Current',
     +       15x,'Displacement Current'/19x,'   (amps)    ',
     +       21x,'   (amps)    ')
9045  format(/' Electrode',7x,'Conduction Current',
     +       15x,'Displacement Current'/19x,'(amps/micron)',
     +       21x,'(amps/micron)')
      write(luinf,9050) (i,ampss(i,1),ampss(i,2),-freq*acflux(i,2),
     +                  freq*acflux(i,1),i=1,nelect)
      if(luinf.ne.luout) write(luout,9050) (i,ampss(i,1),ampss(i,2),
     +                  -freq*acflux(i,2),freq*acflux(i,1),i=1,nelect)
9050  format(10(i6,2x,2(1pe16.5),2x,2(1pe16.5)/))
c
c...Calculate total current and admittance
      do 805 i=1,nelect
         ampss(i,1)=ampss(i,1)-acflux(i,2)*freq
         ampss(i,2)=ampss(i,2)+acflux(i,1)*freq
         gij(i)=dqkt*ampss(i,1)/vss
         cij(i)=dqkt*ampss(i,2)/(vss*freq)
         yij(i)=dmagac(gij(i),freq*cij(i))
805   continue
      IF (LCYL.or.lwidth) THEN
         WRITE(LUINF,9046)
         if(luinf.ne.luout) write(luout,9046)
      ELSE
         WRITE(LUINF,9047)
         if(luinf.ne.luout) write(luout,9047)
      ENDIF
C
9046  format(/'  Element ',9x,'Total Current',13x,
     +        'Conductance',7x,'Capacitance'/
     +        19x,'   (amps)    ',10x,'   (siemens)    ',3x,
     +        '   (farads)    ')
9047  format(/'  Element ',9x,'Total Current',13x,
     +        'Conductance',7x,'Capacitance'/
     +        19x,'(amps/micron)',10x,'(siemens/micron)',3x,
     +        '(farads/micron)')
      write(luinf,9048) (' ',i,iss,ampss(i,1),ampss(i,2),
     +                      gij(i),cij(i),i=1,nelect)
      if(luinf.ne.luout) write(luout,9048) (' ',i,iss,ampss(i,1),
     +                      ampss(i,2),gij(i),cij(i),i=1,nelect)
9048  format(10(a1,'   Y',2i1,1x,2(1pe16.5),1pe16.5,1pe18.5/))
c
c...Write to log file?
      if(llogac) then
         write(lulog2,8002) iss,
     +                      (gij(i),i=1,nelect),
     +                      (cij(i),i=1,nelect),
     +                      (yij(i),i=1,nelect)
8002     format(i4,30e16.8)
      endif
c
c...Next electrode
700   continue
c
ckumar  Check for errflg or wrnflg set by sorac
c
      if(errflg.or.wrnflg) return
c
c...Step frequency?
      if(lfstep) then
         freq0=freq0*fstep
      else
         freq0=freq0+fstep
      endif
      ifstep=ifstep+1
      if(ifstep.le.nfstep) goto 10
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION DMAGAC(vr,vi)
c
      double precision vi,vr
c
      dmagac=dsqrt(vi*vi+vr*vr)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ACCURR(freq,ampss,acflux)
      include 'p2conf.h'
c
c     Calculate ac terminal currents/flux.
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(144002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
      logical lpcont,lsurf,lplug
      integer i,ipt,iel,ni(5),i1,i2,in,n,j
      integer npde,elmat,itmp,nmati,ii
      double precision gs1(2),gs2(2),djgs,e21,dedv,freq
      double precision dgs1(2),dgs2(2)
      double precision ampss(MAXCNT,2),acflux(MAXCNT,2),essi
      double precision gs11,gs21,dgs11,dgs21
      equivalence (gs11,gs1),(gs21,gs2),(dgs11,dgs1),(dgs21,dgs2)
c.. Added by Z. Yu on 6/26/90
      double precision sign
c.. end of 6/26/90
      data ni/1,2,3,1,2/
c
c*************
c**  START  **
c*************
c
c...Initialize
      lplug=.not.lnewt
      do 10 i=1,nelect
         ampss(i,1)=0.d0
         ampss(i,2)=0.d0
         acflux(i,1)=0.d0
         acflux(i,2)=0.d0
10    continue
c
c...Copy solution to separate arrays
      ii=0
      do 11 i=1,np
         ii=ii+1
         fvss(i,1)=ssrhs(ii)
         fvss(i,2)=ssrhs(ii+neq)
         if(l1hole) then
            ii=ii+1
            fpss(i,1)=ssrhs(ii)
            fpss(i,2)=ssrhs(ii+neq)
         else
            ii=ii+1
            fnss(i,1)=ssrhs(ii)
            fnss(i,2)=ssrhs(ii+neq)
            if(ncarr.eq.2) then
               ii=ii+1
               fpss(i,1)=ssrhs(ii)
               fpss(i,2)=ssrhs(ii+neq)
            endif
         endif
11    continue
c
c...Get adjacency info
      call nxtel(ip2t,ip2tc)
c
c...Loop thru electrode nodes, adding current contributions
      do 500 i=1,nb
         ipt=nbc(i)
         iel=lm(ipt)
c
c...Find each element containing node ipt
         do 100 in=1,ip2tc(ipt)
            n=ip2t(in,ipt)
            nmati=imat(n)
            elmat=mattyp(nmati)
c
c...Loop through sides of element - skip if node not on side
            do 400 j=1,3
               i1=nop(ni(j+1),n)
               i2=nop(ni(j+2),n)
               if((i1.ne.ipt).and.(i2.ne.ipt)) goto 400
               if(i2.eq.ipt) then
c.. Added by Z. Yu on 6/26/90
c                 itmp=i2
c                 i2=i1
c                 i1=itmp
c.. Current from substrate to contact
                 sign=-1.0d0
               else
c.. Current from contact to substrate
                 sign=1.0d0
c.. end of 6/26/90
               endif
c
c...Flux (for displacement current)
c.. Changed by Z. Yu on 6/27/90
c              acflux(iel,1)=acflux(iel,1)+ehed(j,n)*(
               acflux(iel,1)=acflux(iel,1)-sign*ehed(j,n)*(
     +                       fvss(i1,1)-fvss(i2,1))
c              acflux(iel,2)=acflux(iel,2)+ehed(j,n)*(
               acflux(iel,2)=acflux(iel,2)-sign*ehed(j,n)*(
     +                       fvss(i1,2)-fvss(i2,2))
c.. end of 6/27/90
c
c...Conduction current (skip if insulator)
               if(elmat.le.0) goto 400
               do 402 npde=2,3
                  lpcont=npde.eq.3
c
                  lsurf=.false.
                  if(nextel(j,n).gt.0) 
     +               lsurf=mattyp(imat(nextel(j,n))).lt.0
                  call assmbj(lpcont,elmat,lsurf,jhjd(j,n),n,
     +                        ni(j+1),ni(j+2),djgs,gs1,gs2,lplug,
     +                        dgs1,dgs2,e21,dedv,lfldmb,itmp)
                  if(lpcont) then
c.. Changed by Z. Yu on 6/26/90
c                    ampss(iel,1)=ampss(iel,1)-(
                     ampss(iel,1)=ampss(iel,1)+sign*(
     +                         gs21*fpss(i2,1)+gs11*fpss(i1,1)+
     +                         dgs21*fvss(i2,1)+dgs11*fvss(i1,1))
c                    ampss(iel,2)=ampss(iel,2)-(
                     ampss(iel,2)=ampss(iel,2)+sign*(
     +                         gs21*fpss(i2,2)+gs11*fpss(i1,2)+
     +                         dgs21*fvss(i2,2)+dgs11*fvss(i1,2))
                  else
c                    ampss(iel,1)=ampss(iel,1)+(
                     ampss(iel,1)=ampss(iel,1)-sign*(
     +                         gs21*fnss(i2,1)+gs11*fnss(i1,1)+
     +                         dgs21*fvss(i2,1)+dgs11*fvss(i1,1))
c                    ampss(iel,2)=ampss(iel,2)+(
                     ampss(iel,2)=ampss(iel,2)-sign*(
     +                         gs21*fnss(i2,2)+gs11*fnss(i1,2)+
     +                         dgs21*fvss(i2,2)+dgs11*fvss(i1,2))
c.. end of 6/26/90
                  endif
 
402            continue
c
c...Next side
400         continue
c
c...Next element
100      continue
c
c...Carrier concentration dependent stuff - only for SRV since
c...this is the only case where fnss,fpss can be non-zero
         if(schotk(iel).and.(mattyp(itype(ipt)).gt.0)) then
            essi=essem(ipt)

            acflux(iel,1)=acflux(iel,1)+essi*(fnss(ipt,1)-fpss(ipt,1))
            acflux(iel,2)=acflux(iel,2)+essi*(fnss(ipt,2)-fpss(ipt,2))

            if(lpcont) then
               ampss(iel,1)=ampss(iel,1)-essi*freq*fpss(ipt,2)
               ampss(iel,2)=ampss(iel,2)+essi*freq*fpss(ipt,1)
            else
               ampss(iel,1)=ampss(iel,1)+essi*freq*fnss(ipt,2)
               ampss(iel,2)=ampss(iel,2)-essi*freq*fnss(ipt,1)
            endif

         endif
c
c...Next node
500   continue
c
c...Scale factor
      do 600 iel=1,nelect
         ampss(iel,1)=ampss(iel,1)*djcoef
         ampss(iel,2)=ampss(iel,2)*djcoef
         acflux(iel,1)=acflux(iel,1)*decoef
         acflux(iel,2)=acflux(iel,2)*decoef
600   continue
c
c...Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SORAC(omeg,tol,maxsor,ssrhs0,ssrhs,r,bb,
     +                 x,dll,duu,di,freq)
      include 'p2conf.h'
c
c     GS/SOR to solve ac system.  
c     Inefficient - rewrite in residual form if kept.
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'logunt.h'
      include     'emaco.h'
c
      integer i,ii,n2,neq2,it,maxsor
      double precision freq,ssrhs(*),bb(*),omeg,res01,tol
      double precision x(*),dll(*),duu(*),di(*),r(*),ssrhs0(*)
      double precision gpsir,gpsii,gnr,gni,gpi,gpr,tmp,omeg1
c
c*************
c**  START  **
c*************
c
c...Initialize
      it=0
      n2=neq+1
      neq2=neq+neq
      do 5 i=1,neq2
         bb(i)=0.d0
         r(i)=0.d0
5     continue
      tmp=1.d0/float(np)
      omeg1=1.d0-omeg
c
c SOR Loop
c
10    it=it+1
      if(it.gt.maxsor) then
         call erset(-264,linum,0)
         return
      endif
c
c...Real part
      ii=0
      do 20 i=1,np
         ii=ii+1
         ssrhs(ii)=ssrhs0(ii)
         ii=ii+1
         ssrhs(ii)=ssrhs0(ii)-essem(i)*freq*bb(ii+neq)
         if(ncarr.eq.2) then
            ii=ii+1
            ssrhs(ii)=ssrhs0(ii)-essem(i)*freq*bb(ii+neq)
         endif
20    continue
c
      call vmbpcd(neq,dll,duu,ssrhs,di,x,r,
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
c
      gpsir=0.d0
      gnr=0.d0
      gpr=0.d0
      ii=0
      do 40 i=1,np

cdebug         write(*,*) i,r(ii+1),r(ii+2),r(ii+3)

         ii=ii+1
         if(r(ii).ne.0.d0) gpsir=gpsir+dabs((r(ii)-bb(ii))/r(ii))
         bb(ii)=omeg*r(ii)+omeg1*bb(ii)
         if(l1hole) then
            ii=ii+1
            if(r(ii).ne.0.d0) gpr=gpr+dabs((r(ii)-bb(ii))/r(ii))
            bb(ii)=omeg*r(ii)+omeg1*bb(ii)
         else
            ii=ii+1
            if(r(ii).ne.0.d0) gnr=gnr+dabs((r(ii)-bb(ii))/r(ii))
            bb(ii)=omeg*r(ii)+omeg1*bb(ii)
            if(ncarr.eq.2) then
               ii=ii+1
               if(r(ii).ne.0.d0) gpr=gpr+dabs((r(ii)-bb(ii))/r(ii))
               bb(ii)=omeg*r(ii)+omeg1*bb(ii)
            endif
         endif
40    continue
      gpsir=gpsir*tmp
      gnr=gnr*tmp
      gpr=gpr*tmp

      if(ldebug) then
         write(lutty,1401) it,gpsir,gnr,gpr
1401     format(' error (real), iter =',i3,' :',3(1pe10.2))
      endif
c
c...Imaginary part
      ii=neq
      do 30 i=1,np
         ii=ii+1
         ssrhs(ii)=ssrhs0(ii)
         ii=ii+1
         ssrhs(ii)=ssrhs0(ii)+essem(i)*freq*bb(ii-neq)
         if(ncarr.eq.2) then
            ii=ii+1
            ssrhs(ii)=ssrhs0(ii)+essem(i)*freq*bb(ii-neq)
         endif
30    continue
c
      call vmbpcd(neq,dll,duu,ssrhs(n2),di,x,r(n2),
     +            jl,ju,il,iu,ivl,ivu,ipc,ipri)
c
      gpsii=0.d0
      gni=0.d0
      gpi=0.d0
      ii=neq
      do 50 i=1,np

cdebug         write(*,*) i,r(ii+1),r(ii+2),r(ii+3)

         ii=ii+1
         if(r(ii).ne.0.d0) gpsii=gpsii+dabs((r(ii)-bb(ii))/r(ii))
         bb(ii)=omeg*r(ii)+omeg1*bb(ii)
         if(l1hole) then
            ii=ii+1
            if(r(ii).ne.0.d0) gpi=gpi+dabs((r(ii)-bb(ii))/r(ii))
            bb(ii)=omeg*r(ii)+omeg1*bb(ii)
         else
            ii=ii+1
            if(r(ii).ne.0.d0) gni=gni+dabs((r(ii)-bb(ii))/r(ii))
            bb(ii)=omeg*r(ii)+omeg1*bb(ii)
            if(ncarr.eq.2) then
               ii=ii+1
               if(r(ii).ne.0.d0) gpi=gpi+dabs((r(ii)-bb(ii))/r(ii))
               bb(ii)=omeg*r(ii)+omeg1*bb(ii)
            endif
         endif
50    continue
      gpsii=gpsii*tmp
      gni=gni*tmp
      gpi=gpi*tmp

      if(ldebug) then
         write(lutty,1402) it,gpsii,gni,gpi
1402     format(' error (imag), iter =',i3,' :',3(1pe10.2))
      endif
c
c...Done?
      res01=(gpsir+gpsii+gnr+gni+gpr+gpi)/6.d0
      if(res01.gt.tol) goto 10
c
c...Put solution in ssrhs
      do 900 i=1,neq2
900   ssrhs0(i)=bb(i)
c
      return
      end
