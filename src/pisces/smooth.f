cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  $Date: 90/08/26 13:27:43 $  ($Author: pisces $)  $Revision: 9009.4 $
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SMOOTH(smoky,nregio)
      include 'p2conf.h'
c
c                   Smooth the triangle and point distribution to
c                   improve the simulation properties of a grid.
c
c                   smoky is a coded key to indicate what smoothing to
c                   do :
c                   1=flip triangles
c                   2=flip triangles disregarding in-region boundaries
c                   3=averaged smoothing
c                   4=R.E. Bank's jiggle routine [PLTMG package]
c           5=angle optimization routine
c     
c     common :   |  emaco   |
c                +----------+
c                |  adjtmpp |
c                +----------+
c                |  smhtmp  |
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c     Revised  :        MRP Aug 84 (ignore regions)
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'smhtmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
       integer i,n,smoky,k,mmod,nregio(1)
c
c***********************************************************************
c
       if (smoky.le.0) return
       call nxtel(p2t,p2tc)

       n=1+int(alog10(real(smoky)))
       do 500 i=1,n
         if(smoky.eq.0) goto 500
         k=mmod(smoky,10)
         smoky=smoky/10
         if(k.eq.1) call flip(1,nregio)
         if(k.eq.2) call flip(2,nregio)
         if(k.eq.3) call massg(3,nregio)
         if(k.eq.4) call massg(4,nregio)
         if(k.eq.5) call massg(5,nregio)
  500  continue
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FLIP(fkey,nregio)
      include 'p2conf.h'
c
c                   Flip triangles according to the Lawson criterion
c               [Lawson "Mathematical Software III" Ed. Rice]
c                   If fkey=1 maintain all region boundaries
c                      fkey=2 maintain material boundaries only
c
c                   Do not flip if elements are in region i and
c                   nregio(i) < 0
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'smhtmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
       integer ie,inx,in,fkey,itc,nregio(1),emat
       logical aldone,lflip
c
c******************** Start ********************************************
c
       do 100 ie=1,ne
       todo(ie)=nregio(imat(ie)).ge.0
  100  continue
       itc=0

c...................while(not all done) do...
c                   At each step, done is the list of elements not to examine,
c...................todo is the list of elements to get on the next pass
c
  500  aldone=.true.
       
         do 200 ie=1,ne
               done(ie)=.not.todo(ie)
  200          todo(ie)=.false.

           do 1000 ie=1,ne
             if (done(ie)) goto 1000
             do 1001 inx=1,3
                in=nextel(inx,ie)
                if(in.le.0) goto 1001
                emat=imat(in)
                if(nregio(emat).lt.0) goto 1001
                if(fkey.eq.1.and.(imat(ie).ne.emat)) goto 1001
                if(fkey.eq.2.and.
     +                    (mattyp(imat(ie)).ne.mattyp(emat))) goto 1001
                if (lflip(ie,in)) then
                     call dflip(ie,in)
                     aldone=.false.
                endif
 1001          continue
 1000      continue
         itc = itc + 1
         if (.not.aldone.and.itc.le.30) goto 500
c
c...................Reassemble adjacency arrays
      call nxtel(p2t,p2tc)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION LFLIP(ie,in)
      include 'p2conf.h'
c
c                   decide whether a given triangle pair deserves
c                   to be flipped
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'blank.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   local types
      integer p1,p2,p3,p4,ie,in,pa,pb,next(3),last(3),ixn,ixe
      logical lelct
      real x1,x2,x3,x4, y1,y2,y3,y4, r,r4, xm,ym, dx,dy
        data next/2,3,1/,last/3,1,2/
c
c******************** Start ********************************************
c
      lflip=.false.

      p1=nop(1,ie)
      p2=nop(2,ie)
      p3=nop(3,ie)

c..........Get ixn so that nop (ixn,in) is not in triangle ie, 
c..........and ixe so that nop (ixe,ie) is not in triangle in.
c..........Then reorder nodes so that p1=nop(ixe,ie)

      ixn=1
      p4=nop(ixn,in)
      if ((p4.eq.p1).or.(p4.eq.p2).or.(p4.eq.p3)) ixn=2
      p4=nop(ixn,in)
      if ((p4.eq.p1).or.(p4.eq.p2).or.(p4.eq.p3)) ixn=3
      p4=nop(ixn,in)
      if ((p4.eq.p1).or.(p4.eq.p2).or.(p4.eq.p3)) goto 999

      pa = nop(next(ixn),in)
      pb = nop(last(ixn),in)
      ixe=1
      p1=nop(ixe,ie)
      if ((p1.eq.p4).or.(p1.eq.pa).or.(p1.eq.pb)) ixe=2
      p1=nop(ixe,ie)
      if ((p1.eq.p4).or.(p1.eq.pa).or.(p1.eq.pb)) ixe=3
        p1=nop(ixe,ie)
        p2=nop(next(ixe),ie)
        p3=nop(last(ixe),ie)
 
c......Check first if the edge is an electrode edge, and skip if so.
      if (lelct(p2,p3)) return


c......Store geometry locally (using p1-p3 order)
      x1=cord(1,p1)
      x2=cord(1,p2)
      x3=cord(1,p3)
      x4=cord(1,p4)
      y1=cord(2,p1)
      y2=cord(2,p2)
      y3=cord(2,p3)
      y4=cord(2,p4)

      call ccentr(x1,y1,x2,y2,x3,y3,xm,ym)

        dx=x1-xm
        dy=y1-ym
      r =dx*dx + dy*dy

        dx=x4-xm
        dy=y4-ym
      r4=dx*dx + dy*dy

c......If p4 is closer to the circumcentre of triangle ie than 
c      one of its own nodes, it's time to FLIP!

      if (r4.le.0.999*r) lflip=.true.
      return

c.........How did this happen?
  999   write(6,*) 'Duplicate triangle passed to LFLIP!'
        return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CCENTR(x1,y1,x2,y2,x3,y3,xm,ym)
      include 'p2conf.h'
c
c                   circumcentre of a triangle
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real x1,y1,x2,y2,x3,y3,xm,ym,p(2),dp(2),q(2),dq(2),alph(2)
      logical parll
c
c******************** Start ********************************************
c
      p(1) = 0.5*(x1+x2)
      p(2) = 0.5*(y1+y2)
      q(1) = 0.5*(x3+x2)
      q(2) = 0.5*(y3+y2)
      dp(1) = y1-y2
      dp(2) = x2-x1
      dq(1) = y2-y3
      dq(2) = x3-x2
      call lil(p,dp,q,dq,alph,parll)
      xm=p(1)+alph(1)*dp(1)
      ym=p(2)+alph(1)*dp(2)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DFLIP(ie,in)
      include 'p2conf.h'
c
c                   Perform flip of triangles ie and in, and update
c                   adjacency array, todo array to reflect change
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'smhtmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer ie,in,kt,kt1,kt2,p1,p2,p3,p4,nbrs(4),ix,kn,i,ta,ke
      integer tmod
      logical lnabor,lgtmp
c
c******************** Start ********************************************
c
c...................Step 1 - memorize the neighbours
      ix=0
      do 10 i=1,3
          if (nextel(i,ie).eq.in) then
        kt=i
        goto 10
          endif
          ix=ix+1
          nbrs(ix)=nextel(i,ie)
  10    continue
      do 20 i=1,3
          if (nextel(i,in).eq.ie) then
         kn=i
         goto 20
          endif
          ix=ix+1
          nbrs(ix)=nextel(i,in)
  20    continue

c...................Step 2 - create new triangles
      kt1 = tmod(kt+1)
      kt2 = tmod(kt-1)
      p1 = nop(kt,ie)
      p2 = nop(kt1,ie)
      p3 = nop(kt2,ie)
      p4 = nop(1,in)
      ix=0
      if ((p4.eq.p1).or.(p4.eq.p2).or.(p4.eq.p3)) p4=nop(2,in)
      if ((p4.eq.p1).or.(p4.eq.p2).or.(p4.eq.p3)) p4=nop(3,in)
      nop(1,ie)=p1
      nop(2,ie)=p4
      nop(3,ie)=p3
      nop(1,in)=p1
      nop(2,in)=p2
      nop(3,in)=p4

c...................Step 3 - reassemble the shattered adjacency array
      do 50 ix=1,3
      nextel(ix,ie)=0
   50   nextel(ix,in)=0

      lgtmp=lnabor(ie,in,ke,kn) 
        nextel(ke,ie)=in
        nextel(kn,in)=ie

      do 70 ix=1,4
          ta=nbrs(ix)
          if(ta.le.0) goto 70
          if(.not.lnabor(ie,ta,ke,kt)) goto 60
          nextel(ke,ie)=ta
          nextel(kt,ta)=ie
          goto 70
   60       lgtmp=lnabor(in,ta,kn,kt)
          nextel(kn,in)=ta
          nextel(kt,ta)=in
   70  continue
c
c      write(luout,*) (nextel(i,ie),i=1,3)
c      write(luout,*) (nextel(i,in),i=1,3)
c       
c...................Step 4 - mark the whole barrel as worth further 
c...................         attention
c
       todo(ie)=.true.
       todo(in)=.true.
       do 100 ix=1,4
           if(nbrs(ix).le.0) goto 100
         todo(nbrs(ix))=.true.
  100  continue

       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       LOGICAL FUNCTION LNABOR(ie,in,ke,kn)
      include 'p2conf.h'
c
c                   Determine whether two triangles are neighbours
c                   If so, return ke=side of ie on which in abuts
c                                 kn=side of in of which ie abuts
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c------------------------------------------------------------------
c
c                   local types
      integer ie,in,ke,kn,ic,i1,i2,a(2,2)
c
c******************** Start ********************************************
c
      ic=0
      ke=0
      kn=0
      do 100 i1=1,3
      do 100 i2=1,3
          if (nop(i1,ie).ne.nop(i2,in)) goto 100
        ic=ic+1
        a(ic,1)=i1
        a(ic,2)=i2
  100 continue
      lnabor=.false.
      if (ic.lt.2) return
      lnabor=.true.
      ke = 6 - a(1,1) - a(2,1)
      kn = 6 - a(1,2) - a(2,2)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MASSG(mkey,nregio)
      include 'p2conf.h'
c                   
c                   Massage points to improve distribution
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'smhtmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      logical aldone,ldont
      integer mkey,ip,ie,emat,in,jp1,jp2,itc,j,tmod
      integer itmax,itmax5,nregio(1)
      real sumx,sumy,tol,rdel

      data tol/5.0e-03/,itmax/30/,itmax5/5/

c
c******************** Start ********************************************
c
c...................Mark boundary points which cannot be moved
c...................Also, do not move points in regions which are
c...................to be ignored (nregio(i)<0)
      do 100 ip=1,np
  100 lbndry(ip)=.false.

      do 200 ie=1,ne
       emat=imat(ie)
         ldont=nregio(emat).lt.0
       do 200 j=1,3
           in=nextel(j,ie)
           if (in.le.0) goto 210
           if (imat(in).ne.emat .or. ldont) goto 210
           goto 200
  210        jp1=tmod(j+1)
           jp2=tmod(j+2)
           ip=nop(jp1,ie)
           lbndry(ip)=.true.
           ip=nop(jp2,ie)
           lbndry(ip)=.true.
  200 continue
c
c...................Massage the remaining points :
c           To-do is the list of points to be examined on the
c           next iteration (cleared at the start of each), done
c           is the ones that the previous iteration didn't mark
c..................."to-do."
c
      itc=1
      do 300 ip=1,np
  300     todo(ip)=.true.
c
c....................Main loop, same for all three methods.
  400 continue
      aldone = .true.
      do 410 ip=1,np
          done(ip)=.not.todo(ip)
  410     todo(ip)=.false.
      
c....................Scan nodes
      do 500 ip=1,np
        if (lbndry(ip)) goto 500
        if (done(ip)) goto 500
        sumx=cord(1,ip)
        sumy=cord(2,ip)
        if (mkey.eq.3) call averp(ip,sumx,sumy,rdel)
        if (mkey.eq.4) call rjig(ip,sumx,sumy,rdel)
        if (mkey.eq.5) call ojig(ip,sumx,sumy,itc,rdel)
        if (rdel.gt.tol) then
            call finger(ip)
            aldone=.false.
        endif
        cord(1,ip)=sumx
        cord(2,ip)=sumy
  500 continue
       itc = itc+1
c...................Limit iteration count (ojig is v.expensive)
       if ((.not.aldone.and.itc.le.itmax).and.
     +       (mkey.ne.5.or.itc.le.itmax5)) goto 400
      
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FINGER(ip)
      include 'p2conf.h'
c
c                   Finger the nodes that haven't converged and
c                   implicate their neighbours for good measure.
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'smhtmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer ip
      integer ix,ixlt,ie,ipp,j
c
c******************** Start ********************************************
c
      ixlt=p2tc(ip)
      do 100 ix=1,ixlt
        ie=p2t(ix,ip)
        do 100 j=1,3
            ipp=nop(j,ie)
            todo(ipp)=.true.
  100 continue
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE AVERP(ip,sumx,sumy,rdel)
      include 'p2conf.h'
c                   
c                   Compute average of polygon around ip
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   Local types   
      integer ip,ixlt,ix,j,ipn,tmod,ie,in
      real sumx,sumy,plxma,plyma,plxmi,plymi,lsumx,lsumy,delx,dely,rdel
c
c******************* Start *********************************************
c
        lsumx=0.0
        lsumy=0.0
        plxma=-1e37
        plyma=plxma
        plxmi=1e37
        plymi=plxmi

        ixlt=p2tc(ip)
        do 400 ix=1,ixlt
            ie=p2t(ix,ip)
            if (nop(1,ie).eq.ip) j=1
            if (nop(2,ie).eq.ip) j=2
            if (nop(3,ie).eq.ip) j=3
            do 400 in=1,2
                  ipn=nop(tmod(j+in),ie)
                lsumx=lsumx+cord(1,ipn)
                lsumy=lsumy+cord(2,ipn)
                if (cord(1,ipn).gt.plxma) plxma=cord(1,ipn)
          if (cord(1,ipn).lt.plxmi) plxmi=cord(1,ipn)
          if (cord(2,ipn).gt.plyma) plyma=cord(2,ipn)
          if (cord(2,ipn).lt.plymi) plymi=cord(2,ipn)
  400    continue
       lsumx=lsumx/2.0/real(ixlt)
       lsumy=lsumy/2.0/real(ixlt)
       delx=abs(lsumx-sumx)/(plxma-plxmi)
       dely=abs(lsumy-sumy)/(plyma-plymi)
       rdel=amax1(delx,dely)
       sumx=lsumx
       sumy=lsumy
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE OJIG(ip,xnew,ynew,itc,rdel)
      include 'p2conf.h'
c
c                   Compute new coordinates for ip using the
c           simplex optimization algorithm
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c                   special common to pass info to optimization function
      common /cjigl/ring,nbrc
c
c                   local types
      integer ndim,itmax,iprint,nbrc,ix,ie,j,tmod,jp1,jp2,
     +        ring(10),ip,ka,kb,in,xel,itc

      real rk(2),eps,rdel,delx,dely,
     +     plxma,plxmi,plyma,plymi,xxa,yya,orient,aot,xnew,ynew,epsy,
     +     rstep
c
c******************** Start ********************************************
c
c                   Set up a ring of neighbours of ip
c                   1. Take two nodes from the first triangle and
c...................order them in counterclockwise fashion

       ie = p2t(1,ip)

       j  = xel(ip,ie)
       jp1 = tmod(j+1)
       jp2 = tmod(j-1)

       ka=nop(jp2,ie)
       kb=nop(jp1,ie)

       orient=aot(cord(1,ip),cord(2,ip),cord(1,kb),cord(2,kb),
     +            cord(1,ka),cord(2,ka))
       if(orient.lt.0) then
         call swapi(ka,kb)
         call swapi(jp1,jp2)
       endif

       ring(1)=kb
       ring(2)=ka

c...................Now cycle around and pick up the rest
c                   Coming into the loop :
c                         ie=last triangle
c...................    jp1=more clockwise of ie's nodes
       nbrc=p2tc(ip)
       do 20 ix=3,nbrc
         in = nextel(jp1,ie)
         j  = xel(ip,in)
         jp1= xel(ring(ix-1),in)
         jp2= 6-j-jp1
         ring(ix)=nop(jp2,in)
         ie=in
   20  continue

c
c...................Get length scales.
       plxma=cord(1,ring(1))
       plxmi=plxma
       plyma=cord(2,ring(1))
       plymi=plyma
       do 30 ix=2,nbrc
       xxa=cord(1,ring(ix))
       yya=cord(2,ring(ix))
       if (xxa.ge.plxma) plxma=xxa
       if (xxa.le.plxmi) plxmi=xxa
       if (yya.ge.plyma) plyma=yya
   30  if (yya.le.plymi) plymi=yya

       rstep=0.05/(4.**(itc-1))
       epsy = 3e-4/(4.**(itc-1))

      eps=rstep*amin1((plxma-plxmi),(plyma-plymi))


c
c...................Initialise variables and call splx
      rk(1)=xnew
      rk(2)=ynew

      ndim=2
      iprint=0
      itmax=50

      call splx(rk,eps,ndim,itmax,epsy,iprint)

c...................Compute relative changes and set new values.
      delx=abs(xnew-rk(1))/(plxma-plxmi)
      dely=abs(ynew-rk(2))/(plyma-plymi)
      rdel=amax1(delx,dely)
      xnew = rk(1)
      ynew = rk(2)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SPLX(init,a,n,itmax,acc,iprint)
      include 'p2conf.h'
c
c                   Investigate simplex method of optimization
c
c       Reference : "Optimization Techniques with Fortran"
c                   J.L.Kuester J.H.Mize McGraw-Hill c1973
c
c-----------------------------------------------------------------------
c
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'logunt.h'
c------------------------------------------------------------------
      integer n,itmax,iprint
      integer np1,i,m,j,itr,k,l
      real init(2),a,acc
      real x(3,2),xcen(3,2),xref(3,2),xcon(3,2),xex(3,2),z(3),x00,y00
      real alfa,beta,gam,q,p,ap,zhi,zlo,en,sum,zcen,ej,zref,zcon,zex
c------------------------------------------------------------------

      np1=n+1
      alfa=1
      beta=0.5
      gam=2

      do 60 i=1,n
60     x(1,i)=init(i)
      x00=init(1)
      y00=init(2)


c...................End of input section

      q=(a/n*(2.**.5))*((n+1)**.5-1.)
      p=(a/n*(2.**.5))*((n+1)**.5+n-1.)
      m=n+1
      do 130 i=2,m
       ap=1.0
       do 120 j=1,n
      ap=ap+1
      if(i.eq.ap) goto 135
      x(i,j)=x(1,j)+q
      goto 120
135     x(i,j)=x(1,j)+p
120    continue
130   continue

c140   continue

      itr=0
150   do 155 i=1,np1
       call object(i,x,z,n,np1)
155   continue
      
      itr=itr+1
      if (itr.ge.itmax) goto 145

      if(iprint)158,162,158
158   write(luout,8) itr
  8   format(//,2x,'ITERATION NUMBER ',i3)
      do 160 j=1,np1
160   write(luout,6) (j,i,x(j,i),i=1,n)
      write(luout,9) (i,z(i),i=1,np1)
  6   format(/,2(2x,'x(',i2,',',i2,') = ',1pe12.5))
  9   format(/,3(2x,'f(',i2,') = ',1pe16.8))

162   zhi=amax1(z(1),z(2),z(3))
      zlo=amin1(z(1),z(2),z(3))
      do 165 i=1,np1
       if (zhi.eq.z(i)) goto 170
165   continue
170   k=i
  
      en=n
      do 180 j=1,n
       sum=0.
       do 175 i=1,np1
        if (k.eq.i) goto 175
        sum=sum+x(i,j)
175    continue
180   xcen(k,j)=sum/en

      i=k
      call object(i,xcen,z,n,np1)
      zcen=z(i)
      sum=0.
      do 185 i=1,np1
       if (k.eq.i) goto 185
       sum=sum+(z(i)-zcen)*(z(i)-zcen)/en
185   continue
      
      ej=sqrt(sum)
      if (ej.lt.acc) goto 998

      do 190 j=1,n
       xref(k,j)=xcen(k,j)+alfa*(xcen(k,j)-x(k,j))
190   continue
      
      i=k
      call object(i,xref,z,n,np1)
      zref=z(i)

      do 200 i=1,np1
       if (zlo.eq.z(i)) goto 205
200   continue
205   l=i

      if (zref.le.z(l)) goto 240
      do 207 i=1,np1
       if(zref.lt.z(i)) goto 208
207   continue
      goto 215

208   do 210 j=1,n
210    x(k,j)=xref(k,j)
      goto 150

215   do 220 j=1,n
220    xcon(k,j)=xcen(k,j)+beta*(x(k,j)-xcen(k,j))
    
      i=k
      call object(i,xcon,z,n,np1)
      zcon=z(i)

      if (zcon.lt.z(k)) goto 230

      do 225 j=1,n
       do 225 i=1,np1
225     x(i,j)=(x(i,j)+x(l,j))/2.
      goto 150

230   do 235 j=1,n
235    x(k,j)=xcon(k,j)
      goto 150

240   do 245 j=1,n
245    xex(k,j)=xcen(k,j)+gam*(xref(k,j)-xcen(k,j))

      i=k
      call object(i,xex,z,n,np1)
      zex=z(i)

      if (zex.lt.z(l)) goto 255

      do 250 j=1,n
250    x(k,j)=xref(k,j)
      goto 150

255   do 260 j=1,n
260   x(k,j)=xex(k,j)
      goto 150

145   continue
c          write(luout,011) itmax
c011   format(///,10x,'DID NOT CONVERGE IN ',i5,' ITERATIONS')
998   continue
c          write(luout,12) zlo,itr
c12    format(//,2x,'OPTIMUM VALUE OF F = ',1pe16.8,' FOUND IN ',i3,
c     +        ' ITERATIONS ',/,70('='))
      do 300 i=1,n
       init(i)=x(np1,i)
300   continue
c       write(luout,14) i,x(np1,i)
c14    format(/,2x,'x(',i2,')= ',1pe16.8)
      if (itr.ge.itmax) then
        init(1)=x00
        init(2)=y00
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE OBJECT(oai,oax,oaz,oan,oanp1)
      include 'p2conf.h'
c
c                   Function to be optimized : the maximum internal
c           angle inside the polygon around x
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c------------------------------------------------------------------
c                   common passed from ojig
       common /cjigl/ring,nbrc
       integer nbrc,ring(10)
c
c                   local types
      real oax(3,2),oaz(3),oqual,qq,q
      integer oan,ix,ita,itb,cmod,oanp1,oai
c
c******************** Start ********************************************
c
      qq=1.0
      do 90 ix=1,nbrc
          itb=ring(ix)
          ita=ring(cmod(ix+1,nbrc))
          q=oqual(oax(oai,1),oax(oai,2),cord(1,itb),cord(2,itb),
     +                       cord(1,ita),cord(2,ita))
          qq=amin1(qq,q)
   90   continue
      oaz(oai)=-qq
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       REAL FUNCTION OQUAL(xa,ya,xb,yb,xc,yc)
      include 'p2conf.h'
c
c       Return a value inversely proportional to the largest
c       angle in the triangle.
c       For equilateral  oqual=1
c       For right angle  oqual=0
c       For obtuse       oqual<0
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   local types
       integer j,j1,j2,ttmod(5)
       real xa,ya,xb,yb,xc,yc,s(3),
     +      tp1     ,s1,s2,s3,d1x,d1y,d2x,d2y,d3x,d3y
       equivalence (s(1),s1),(s(2),s2),(s(3),s3)
       data ttmod/1,2,3,1,2/
c
c******************** Start ********************************************
c
c                   Oqual = 1-cosine(largest angle)
       d1x=xc-xb
       d1y=yc-yb
       d2x=xa-xc
       d2y=ya-yc
       d3x=xb-xa
       d3y=yb-ya
       s1=d1x*d1x+d1y*d1y
       s2=d2x*d2x+d2y*d2y
       s3=d3x*d3x+d3y*d3y
       j=1
       if (s2.gt.s1) j=2
       if (s3.gt.s(j)) j=3
      
       j1=ttmod(j+1)
       j2=ttmod(j+2)
       tp1=s(j)-s(j1)-s(j2)
       tp1 = -abs(tp1)*tp1
       oqual=   tp1/ (s(j1)*s(j2))
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RJIG(ip,axnew,aynew,rdel)
      include 'p2conf.h'
c
c                   Compute new coordinates for ip using Randy Bank's
c                   smoothing algorithm
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer ie,ix,ka,kb,la,lb,ip,j,jp1,jp2,tmod,i,ip1,ip2,
     +        ring(10),cmod,xel,ita,itb,nbrc,in,itmax,itc
      real  xmin,ymin,xmax,ymax,dxk,dyk,xmk,ymk,rk,aot,rdel,
     +      dxl,dyl,xml,yml,rl,r,a,b,c,beta,xck,yck,xcl,ycl,zx,zy,qq,q,
     +      dy,tol,xnew,ynew,qual,qmin,qmin2,s3o2,xm,ym,dx,orient,
     +      xxa,yya,xxb,yyb,xxp,yyp,qk,ql,plxmi,plxma,plymi,plyma,
     +      rtol,delx,dely,axnew,aynew

      data rtol/0.4e-02/,s3o2/0.8660254038/,itmax/30/
c
c******************** Start ********************************************
c
c                   Set up a ring of neighbours of ip
c                   1. Take two nodes from the first triangle and
c                   order them in counterclockwise fashion

       ie = p2t(1,ip)

       j  = xel(ip,ie)
       jp1 = tmod(j+1)
       jp2 = tmod(j-1)

       ka=nop(jp2,ie)
       kb=nop(jp1,ie)

       orient=aot(cord(1,ip),cord(2,ip),cord(1,kb),cord(2,kb),
     +            cord(1,ka),cord(2,ka))
       if(orient.lt.0) then
         call swapi(ka,kb)
         call swapi(jp1,jp2)
       endif

       ring(1)=kb
       ring(2)=ka

c                   Now cycle around and pick up the rest
c                   Coming into the loop :
c                         ie=last triangle
c               jp1=more clockwise of ie's nodes
       nbrc=p2tc(ip)
       do 20 ix=3,nbrc
         in = nextel(jp1,ie)
         j  = xel(ip,in)
         jp1= xel(ring(ix-1),in)
         jp2= 6-j-jp1
         ring(ix)=nop(jp2,in)
         ie=in
   20  continue

c
c                   Get length scales.
       plxma=cord(1,ring(1))
       plxmi=plxma
       plyma=cord(2,ring(1))
       plymi=plyma
       do 30 ix=2,nbrc
       xxa=cord(1,ring(ix))
       yya=cord(2,ring(ix))
       if (xxa.ge.plxma) plxma=xxa
       if (xxa.le.plxmi) plxmi=xxa
       if (yya.ge.plyma) plyma=yya
   30  if (yya.le.plymi) plymi=yya
       tol=rtol*amax1((plxma-plxmi),(plyma-plymi))

c                   Finger the two worst triangles containing ip
c                      qmin <= qmin2 <= all other q's
c                     (ka-kb)   (la-lb)      (all p2t(ix,ip))
       qmin=1.0
       qmin2=1.0
       ka=0
       kb=0
 
       do 80 ix=1,nbrc
          itb=ring(ix)
          ita=ring(cmod(ix+1,nbrc))
         q=qual(cord(1,ip),cord(2,ip),
     +            cord(1,itb),cord(2,itb),
     +            cord(1,ita),cord(2,ita))
         if (q.le.qmin) then
              qmin2=qmin
              qmin=q
             la=ka
             lb=kb
             ka=ita
             kb=itb
         else
             if (q.le.qmin2) then
           qmin2=q
           la=ita
           lb=itb
             endif
         endif
   80  continue
 
c                   Compute midpoint and side vectors  for each of the 
c                   worst triangles.  Side vectors in the c-cl sense.
c                   abbreviated convention : k is the worst, l 2nd worst
      
       xnew=axnew
       ynew=aynew
       xxa=cord(1,ka)
       yya=cord(2,ka)
       xxb=cord(1,kb)
       yyb=cord(2,kb)
       xxp=cord(1,ip)
       yyp=cord(2,ip)
       xmk=0.5*(xxa+xxb)
       ymk=0.5*(yya+yyb)
       dxk=(xxa-xxb)*s3o2
       dyk=(yya-yyb)*s3o2
       xmax=xmk-dyk
       ymax=ymk+dxk
       rk=sqrt(dxk*dxk+dyk*dyk)

       xxa=cord(1,la)
       yya=cord(2,la)
       xxb=cord(1,lb)
       yyb=cord(2,lb)
       xxp=cord(1,ip)
       yyp=cord(2,ip)
       xml=0.5*(xxa+xxb)
       yml=0.5*(yya+yyb)
       dxl=(xxa-xxb)*s3o2
       dyl=(yya-yyb)*s3o2
       rl=sqrt(dxl*dxl+dyl*dyl)

 
c                   Compute good initial guess for bisection iteration
       xm=xmk-xml
       dx=dxk-dxl
       ym=ymk-yml
       dy=dyk-dyl
       r=rk+rl
       a=r*r-dx*dx-dy*dy
       b=ym*dx-xm*dy
       c=xm*xm+ym*ym+r*r
       beta=1.0
       if (a.gt.0.0) beta=(b+sqrt(b*b+a*c))/a
       xck=xmk-beta*dyk
       yck=ymk+beta*dxk
       xcl=xml-beta*dyl
       ycl=yml+beta*dxl
       xmin=xnew
       ymin=ynew
       xmax=(xck*rl+xcl*rk)/r
       ymax=(yck*rl+ycl*rk)/r
       qk = qual(xmax,ymax,cord(1,ka),cord(2,ka),cord(1,kb),cord(2,kb))
       ql = qual(xmax,ymax,xxa,yya,xxb,yyb)

c                   the bisection loop
c
       itc=0
   85  zx=abs(xmin-xmax)/(plxma-plxmi)
       zy=abs(ymin-ymax)/(plyma-plymi)
       itc = itc+1
       if (amax1(zx,zy).lt.tol.or.itc.gt.itmax) goto 100
       xnew=(xmin+xmax)/2.0e0
       ynew=(ymin+ymax)/2.0e0
       qq=1.0e0
       do 90 i=1,nbrc
         ip1=ring(i)
         ip2=ring(cmod(i+1,nbrc))
         q=qual(xnew,ynew,cord(1,ip1),cord(2,ip1),
     +                      cord(1,ip2),cord(2,ip2))
         if(q.lt.qmin) then
             xmax=xnew
             ymax=ynew
             goto 85
         endif
         qq=amin1(qq,q)
   90 continue
      xmin=xnew
      ymin=ynew
      goto 85
c
c                   Good enough for now.
  100  delx=abs(xmin-axnew)/(plxma-plxmi)
       dely=abs(ymin-aynew)/(plyma-plymi)
       rdel=amax1(delx,dely)
       axnew=xmin
       aynew=ymin
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       REAL FUNCTION QUAL(xa,ya,xb,yb,xc,yc)
      include 'p2conf.h'
c
c                   Compute a constant indicating the quality of
c                   a triangle.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       real xa,ya,xb,yb,xc,yc,dx1,dx2,dy1,dy2,det,dd
c
c******************** Start ********************************************
c
       dx1=xc-xa
       dy1=yc-ya
       dx2=xb-xa
       dy2=yb-ya
       det=dx2*dy1-dx1*dy2
       dd=dx1*dx1+dy1*dy1+dx2*dx2+dy2*dy2+(dx1-dx2)*(dx1-dx2)+
     +                                    (dy1-dy2)*(dy1-dy2)
       qual=abs(det*3.464101/dd)
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       INTEGER FUNCTION XEL(ip,ie)
      include 'p2conf.h'
c
c                   Convenience function, returns index of ip in ie
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c------------------------------------------------------------------
       integer ip,ie,j
c
c******************** Start ********************************************
c
       xel=0
       do 100 j=1,3
       if (nop(j,ie).eq.ip) then
         xel=j
         return
       endif
  100  continue
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       INTEGER FUNCTION CMOD(i,mdlus)
      include 'p2conf.h'
c
c                   Return mod(i,mdlus) in the range 1..mdlus
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       integer i,mdlus,mmod
c***********************************************************************
c
       cmod = 1+(mmod(i-1,mdlus))
       return
       end
