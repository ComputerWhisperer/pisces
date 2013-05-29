cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 12 22:45:17 PDT 1989 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE VECTCK(vflag,vscl,vlog,lintyp,clip,vmax,vmin)
      include 'p2conf.h'
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
c------------------------------------------------------------------
c
      logical vlog
      integer vflag,lintyp
      real clip
      double precision vscl,vmax,vmin
c
c FUNCTIONS:
      logical isrval
      real    gtrval
      logical gtlval
c------------------------------------------------------------------
c
c...If difference (and not e-field), no can do
      if(.not.ldiff.or.gtlval(6)) goto 1
      call erset(149,linum,0)
      return
c 
c...Quantity
1     if(gtlval(1)) then
         if(gtlval(2).or.gtlval(3).or.gtlval(4).or.
     +   gtlval(5).or.gtlval(6))  call erset(50,linum,0)
         vflag=1
      else if(gtlval(2)) then
         if(gtlval(3).or.gtlval(4).or.gtlval(5).or.gtlval(6)) 
     +      call erset(50,linum,0)
         vflag=2
      else if(gtlval(3)) then
         if(gtlval(4).or.gtlval(5).or.gtlval(6)) call erset(50,linum,0)
         vflag=3
      else if(gtlval(4)) then
         if(gtlval(5).or.gtlval(6)) call erset(50,linum,0)
         vflag=4
      else if(gtlval(5)) then
         if(gtlval(6)) call erset(50,linum,0)
         vflag=5
      else if(gtlval(6)) then
         vflag=6
      else
         call erset(33,linum,0)
      endif
c
c...Scaling
      vscl=-1.d0
      if(isrval(1)) vscl=gtrval(1)
      vlog=gtlval(7)
      lintyp=1
      if(isrval(2)) lintyp=gtrval(2)
      clip=0.1
      if(isrval(3)) clip=gtrval(3)
      if(isrval(4)) vmax=gtrval(4)
      if(isrval(5)) vmin=gtrval(5)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE VECPLT(vflag,vscl,vlog,lintyp,clip,vmax,vmin)
      include 'p2conf.h'
c 
c     Original : MRP           Stanford University        Apr, 1984
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
      include     'plot.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   type declarations 
c 
      logical vlog,lvmax,lvmin
      integer i,vflag,lintyp
      real x1,y1,scx,scy,scx2,scy2,x0,y0,scxy,vth
      real clip,vx15,vx1,vy15,vy1,vx,vy,scyx1,scxy1
      double precision vpnt,vmag,vmin,vmax,vscl,tvmin,tvmax
c
c******************* Start **************************************** 
c
c....Initialize
      scx=.01*(xpwid/xpwid0)*(xpmax-xpmin)/xpwid
      scy=.01*(ypwid/ypwid0)*(ypmax-ypmin)/ypwid
      scxy=scx/scy
      scxy1=0.25*scxy
      scyx1=0.25/scxy
      scx2=scx*0.5
      scy2=scy*0.5
      vth=clip*scy2
      call fnline(lintyp)
c
c...Scan all points
      lvmax=vmax.le.0.d0
      lvmin=vmin.le.0.d0
      tvmin=1.d20
      tvmax=0.d0
      do 100 i=1,np
        vmag=vpnt(i,vflag,vix(i),viy(i),.false.)
        tvmax=dmax1(tvmax,vmag)
        if(vmag.gt.0.d0) tvmin=dmin1(tvmin,vmag)
        vimag(i)=vmag
100   continue
      if(lvmax) vmax=tvmax
      if(lvmin) vmin=tvmin
      call fgtoa
      write(luout,4555) vmin,vmax
      if(lutty.ne.luout) write(lutty,4555) vmin,vmax
4555  format(/'  Minimum (non-zero) magnitude = ',1pe13.6/
     +        '  Maximum magnitude            = ',1pe13.6/)
c
c...Get scale factors
      if(vmax.eq.0.d0) return
      if(.not.vlog) then
         if(vscl.eq.0.d0) vscl=amin1(xpmax-xpmin,ypmax-ypmin)*0.1d0
         vscl=vscl/vmax
      else if(vscl.lt.0.d0) then
         if(vmin.eq.0.d0) return
      endif
c
c...Scale and plot all data
      do 200 i=1,np
      if(vlog) then
         vmag=vimag(i)
         if(vmag.gt.vscl) then
            vmag=dlog10(vmag/vmin)/vmag
            vx=scx2*vix(i)*vmag*vscl
            vy=scy2*viy(i)*vmag*vscl
         else
            vx=0.d0
            vy=0.d0
         endif
      else
         vx=scx2*vix(i)*vscl
         vy=scy2*viy(i)*vscl
      endif
      if((abs(vx).lt.vth).and.(abs(vy).lt.vth)) goto 200
c
c...Plot vector at point i
      x0=cord(1,i)
      y0=cord(2,i)
      vx=0.5*vx
      vy=0.5*vy
      x1=x0-vx
      y1=y0-vy
      call zmove(x1,y1)
      x1=x0+vx
      y1=y0+vy
      call zline(x1,y1,ipendn,0)
c
c...Arrow
      vx15=vx*0.15
      vy15=vy*0.15
      vx1=vx*scyx1
      vy1=vy*scxy1
      x0=x1-vx15+vy1
      y0=y1-vy15-vx1
      call zline(x0,y0,ipendn,0)
      call zmove(x1,y1)
      x0=x1-vx15-vy1
      y0=y1-vy15+vx1
      call zline(x0,y0,ipendn,0)
c 
c...Next node
200   continue
c 
c...Done; alpha mode
      call fnline(1)
      call fgtoa
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION VPNT(inode,flag,vx,vy,lesol2)
      include 'p2conf.h'
c
c     Get quantity from current continuity equation(s) at a node.
c
c         inode....node no.
c         flag.....1 - total conduction current
c                  2 - electron current
c                  3 - hole current
c                  4 - displacement current
c                  5 - total current
c                  6 - electric field
c                  7 - total recombination (u)
c                  8 - generated carrier density due to impact ionization
c                  9 - ionization rate alpha(n)
c                  10- ionization rate alpha(p)
c
c     Note - we depend on a clockwise ordering of nodes within 
c     triangles (remember also that we have a LH coordinate
c     system - if we plot RH, ordering is ccw)
c
c     I think there are still probs with obtuse triangles in here.
c
c     Original : MRP Jun 84
c     Modified : CSR Sep 84 - Normalized to perimeter.
c     Modified : MRP May 85 - exact vector at node 
c     Modified : A.Yabuta July 87 - Add flag 8 (generation)
c     Modified : A.Yabuta Sep  87 - Add flag 9,10 (ionization rate)
c     Modified : A.Yabuta Feb  88 - Symmetrical coordination
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'setup.h'
      include     'sol.h'
      include     'plot.h'
      include     'impact.h'
      include     'symme.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'adjtmp.h'
      include     'difftmp.h'
      integer TMPPAD(1338602)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      logical lpcont,lesol2,lsurf
      integer inode,flag,i,n,ni(5),idxnod,i1,i2,in,i3
      integer npde,elmat,nmati,iside,ihi,ilo,idum
      double precision gs1(2),gs2(2),dum,djgs,e21,epsel,deetee
      double precision ui,dedv,vx,vy,ai,ael,dgs1(2),dgs2(2)
      double precision vside,divadj,djc2,dec2,epsel2
      real xaw,yaw,xawi,yawi,wsgn,cc1,cc2
      real xcc,ycc,xpt,ypt,xi,yi,vintg
      double precision gi(3),gii,areag
      double precision ei(3),jside(3),heigh(3),tmp1,tmp2,tmp3
      double precision area(3),dev(3,3)
      double precision gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3)
      double precision xi1,yi1,xj1,yj1,xk1,yk1
      double precision xcentr,ycentr,volume
c
      data ni/1,2,3,1,2/
c
c*************
c**  START  **
c*************
c
c...Initialize
      vpnt=0.d0
      vx=0.d0
      vy=0.d0
      gii=0.d0
      areag=0.d0
      ui=0.d0
      ai=0.d0
      xaw=0.
      yaw=0.
      xpt=cord(1,inode)
      ypt=cord(2,inode)
c
      ilo=2
      ihi=3
      if(flag.eq.2) then
         ihi=2
      else if(flag.eq.3) then
         ilo=3
      else if(flag.eq.4.or.flag.eq.5) then
         deetee=stime-stime0
         if(deetee.gt.0.d0) then
            deetee=1.d0/deetee
         else
            deetee=0.d0
         endif
      endif
c
c...Find each element containing inode
      if(flag.eq.7) goto 500
      do 100 in=1,p2tc(inode)

      iside=0
      n=p2t(in,inode)
      nmati=imat(n)
      elmat=mattyp(nmati)
      epsel=epsmat(nmati)
      epsel2=epsel*qcharg/depsc

      dec2=decoef*1.d4/epsel2
      djc2=djcoef*1.d4
c
c...Find index of node and add area component
      if(nop(1,n).eq.inode) then
         idxnod=1
      else if(nop(2,n).eq.inode) then
         idxnod=2
      else
         idxnod=3
      endif
      ael=es(idxnod,n)
      ai=ai+ael
c...If generation by impact ionization
      if((flag.eq.8).or.(flag.eq.9).or.(flag.eq.10)) goto 410
c
c...This is where we have probs w/ obtuse conditions
      if(flag.eq.6) then
         call isecpt(cord(1,nop(1,n)),cord(2,nop(1,n)),ehed(1,n),
     +               cord(1,nop(2,n)),cord(2,nop(2,n)),ehed(2,n),
     +               cord(1,nop(3,n)),cord(2,nop(3,n)),ehed(3,n),
     +               xcc,ycc)
      else
         call isecpt(cord(1,nop(1,n)),cord(2,nop(1,n)),jhjd(1,n),
     +               cord(1,nop(2,n)),cord(2,nop(2,n)),jhjd(2,n),
     +               cord(1,nop(3,n)),cord(2,nop(3,n)),jhjd(3,n),
     +               xcc,ycc)
      endif
      xcc=xcc-xpt
      ycc=ycc-ypt
c
c...Loop through sides of element
      do 400 i=1,3
      if(i.eq.idxnod) goto 400

      i1=nop(ni(i+1),n)
      i2=nop(ni(i+2),n)
      i3=nop(i,n)
      if(i2.eq.inode) then
         i2=i1
         i1=inode
         wsgn=+1.
      else
         wsgn=-1.
      endif
      iside=iside+1

      xi=0.5*(cord(1,i2)-xpt)
      yi=0.5*(cord(2,i2)-ypt)
      vside=0.d0
c
c  ELECTRIC FIELD
c
      if(flag.ne.6) goto 401

      if(lesol2) then
         vside=-dec2*ehed(i,n)*(ofv(i1)-ofv(i2))
      else
         vside=-dec2*ehed(i,n)*(fv(i1)-fv(i2))
      endif

      cc1=ehed(i,n)/epsel
      cc2=ehed(6-i-idxnod,n)/epsel

      goto 440
c
c CURRENT
c
c
c...Conduction
401   continue
      if((flag.eq.4).or.(elmat.le.0)) goto 441
      do 402 npde=ilo,ihi
      lpcont=npde.eq.3
      lsurf=.false.
      if(nextel(i,n).gt.0) lsurf=mattyp(imat(nextel(i,n))).lt.0
      call assmbj(lpcont,elmat,lsurf,jhjd(i,n),n,ni(i+1),ni(i+2),
     +            djgs,gs1,gs2,
     +            .true.,dgs1,dgs2,e21,dedv,lconmb,idum)
      if(.not.lpcont) then
         vside=vside+djc2*djgs
      else
         vside=vside-djc2*djgs
      endif
402   continue
c
c...Displacement current
441   continue
      if((flag.eq.4).or.(flag.eq.5)) vside=vside-
     +   dec2*epsel2*ehed(i,n)*deetee*(fv(i1)-fv(i2)-ofv(i1)+ofv(i2))
c
c...Save coupling coef's
      continue
      cc1=jhjd(i,n)
      cc2=jhjd(6-i-idxnod,n)
c
c...Sum vector contributions to line integrals
440   continue

      vx=vx+vside*(xi+xcc)*0.5d0
      vy=vy+vside*(yi+ycc)*0.5d0

      xawi=wsgn*(vintg(0.,0.,xi,yi)+vintg(xi,yi,xcc,ycc)+
     +           vintg(xcc,ycc,0.,0.))
      yawi=wsgn*(vintg(0.,0.,xi,yi)+vintg(yi,xi,ycc,xcc)+
     +           vintg(ycc,xcc,0.,0.))
cc      write(*,*) 'node,el,xaw,yaw : ',inode,n,xawi,yawi
      xaw=xaw+xawi
      yaw=yaw+yawi
c
c...Next side
400   continue
c..To skip over the loop
410   continue
c
c...Calculate generation by impact ionization(if necessary, Si only)
c----Calculate current along side
      if(((flag.eq.8).or.(flag.eq.9).or.(flag.eq.10))
     +.and.(elmat.eq.1)) then
c
c...Get the center of circumscribed circle for triangle(element)
        if(limpct.and.lcyl) then
           xi1=cord(1,nop(1,n))
           yi1=cord(2,nop(1,n))
           xj1=cord(1,nop(2,n))
           yj1=cord(2,nop(2,n))
           xk1=cord(1,nop(3,n))
           yk1=cord(2,nop(3,n))
           call centr(xi1,yi1,xj1,yj1,xk1,yk1,xcentr,ycentr)
        endif
        if(flag.eq.9) then
          lpcont=.false.
        else
          lpcont=.true.
        endif
601     continue
        do 600 i=1,3
        i1=nop(ni(i+1),n)
        i2=nop(ni(i+2),n)
        lsurf=.false.
        if(nextel(i,n).gt.0) lsurf=mattyp(imat(nextel(i,n))).lt.0
        if(jhjd(i,n).ne.0.d0) then
        call assmbj(lpcont,elmat,lsurf,jhjd(i,n),n,ni(i+1),ni(i+2),
     +            djgs,gs1,gs2,
     +            .true.,dgs1,dgs2,e21,dedv,lconmb,idum)
        endif
c
c----Store values
c    Be aware that electric field must be calculated here
        ei(i)=e21
        tmp1=cord(1,i2)-cord(1,i1)
        tmp2=cord(2,i2)-cord(2,i1)
        tmp3=dsqrt(tmp1*tmp1+tmp2*tmp2)
        if(jhjd(i,n).eq.0.d0) then
         heigh(i)=0.d0
         area(i)=0.d0
        else
         heigh(i)=jhjd(i,n)*tmp3
         if(limpct.and.lcyl) then
           xi1=cord(1,i1)
           yi1=cord(2,i1)
           xj1=cord(1,i2)
           yj1=cord(2,i2)
           area(i)=volume(xi1,yi1,xj1,yj1,xcentr,ycentr)
         else
           area(i)=0.5d0*heigh(i)*tmp3
         endif
         tmp3=heigh(i)
         if(lpcont) tmp3=-tmp3
         jside(i)=djgs/tmp3
        endif
600     continue
c
c----Calculate generation by impact ionization
        if(flag.eq.8) then
         if(lmont) then
         call impac2(n,lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.false.)
         else if(lcsm) then
         call impac1(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.false.)
         else
         call impace(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.false.)
         endif
c...if normarization by area is necessary
         areag=areag+area(idxnod)
        else
         if(lmont) then
         call impac2(n,lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
         else if(lcsm) then
         call impac1(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
         else
         call impace(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
         endif
         areag=areag+area(idxnod)
        endif
        gii=gii+gi(idxnod)
        if((lpcont).and.(flag.eq.8)) then
          lpcont=.false.
          goto 601
        endif
      endif
c
c...Next element
100   continue
c
c...Calculate recombination (if necessary)
500   ui=0.d0
      if((mattyp(itype(inode)).gt.0).and.
     +  (flag.ne.4).and.(flag.ne.5).and.(flag.ne.6)) then
        if(lesol2) then
           call recomb(inode,ofn(inode),ofp(inode),ui,dum,dum,dum)
        else
           call recomb(inode,fn(inode),fp(inode),ui,dum,dum,dum)
        endif
      endif
c
c...Return result in A/cm-2 for current, V/cm for E-field
      if(flag.eq.8) then
         vpnt=gii*dcscl*dktq
c...Normarize by area and return positive value 
         if(areag.ne.0.) then
          vpnt=vpnt/areag
          vpnt=dabs(vpnt)
         else
          vpnt=0.
         endif
         goto 999
      endif
      if((flag.eq.9).or.(flag.eq.10)) then
         if(areag.ne.0.) then
          vpnt=gii/areag
         else
          vpnt=0.
         endif
         goto 999
      endif
      if(flag.eq.7) then
         vpnt=ui*dcscl*dktq
      else 
         divadj=0.d0
         if((flag.eq.1).or.(flag.eq.4)) then
            if(deetee.gt.0.d0) 
     +       divadj=qcharg*dcscl*(fn(inode)-ofn(inode)-
     +                   fp(inode)+ofp(inode))*deetee
         else if(flag.eq.2) then
            divadj=-ui*djc2
            if(deetee.gt.0.d0) 
     +       divadj=divadj+qcharg*dcscl*(fn(inode)-ofn(inode))*deetee
         else if(flag.eq.3) then
            divadj=ui*djc2
            if(deetee.gt.0.d0) 
     +       divadj=divadj+dcscl*qcharg*(ofp(inode)-fp(inode))*deetee
         else if(flag.eq.6) then
            if(lesol2) then
               divadj=-dec2*(ofp(inode)-ofn(inode)+r1(inode))
            else
               divadj=-dec2*(fp(inode)-fn(inode)+r1(inode))
            endif
         else 
            divadj=0.d0
         endif

         vx=(vx-divadj*xaw)/ai
         vy=(vy-divadj*yaw)/ai

         if(lxcomp) then
            vpnt=vx
         else if(lycomp) then
            vpnt=vy
         else
            vpnt=dsqrt(vx*vx+vy*vy)
         endif

      endif
c
c...Done
999   continue
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION VINTG(x1,y1,x2,y2)
      include 'p2conf.h'
c
c     Evaluate 
c
c      |-x2  |-ax+b
c      |     | 
c      |     |     x dy dx
c      |     |
c     -| x1 -| 0
c
c     where y=ax+b is defined by the coordinates (x1,y1),(x2,y2)
c
c     Orig: MRP  May 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real x1,y1,x2,y2,a,b,x22,x12
c
      vintg=0.
      if(x2.eq.x1) return
      a=(y2-y1)/(x2-x1)
      b=y2-a*x2
      x22=x2*x2
      x12=x1*x1
      vintg=a*(x22*x2-x12*x1)/3.+b*(x22-x12)*.5
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ISECPT(x1,y1,cc1,x2,y2,cc2,x3,y3,cc3,xi,yi)
      include 'p2conf.h'
c
c     Find intersection of perpendicular bisectors (remember
c     special obtuse condition).
c
c     Original: Mark R. Pinto   Stanford University  May,1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real x1,x2,x3,y1,y2,y3,cc1,cc2,cc3,xi,yi
c
      if(cc1.eq.0.) then
         xi=0.5*(x2+x3)
         yi=0.5*(y2+y3)
      else if(cc2.eq.0.) then
         xi=0.5*(x1+x3)
         yi=0.5*(y1+y3)
      else if(cc3.eq.0.) then
         xi=0.5*(x2+x1)
         yi=0.5*(y2+y1)
      else
         call ccentr(x1,y1,x2,y2,x3,y3,xi,yi)
      endif
c
      return
      end
