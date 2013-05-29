cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Jun 26 23:14:55 PDT 1990 (anderson--stanford)
c
c Modified: Michael Eldredge -- Stanford (feb 90) remove dead code
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE NXTEL(p2t,p2tc)
      include 'p2conf.h'
c 
c                   this routine generates element adjacency info 
c                   in three arrays
c 
c     Original : C.H.Price     Stanford University        May, 1982
c     Revision : CSR           Stanford University        Nov, 1983
c 
c     nextel(i,j)=number of element sharing side i with element j 
c                 (=-n where n is an index to that boundary edge)
c     p2t(i,j)   =list of triangles 1..i at point j.
c     p2tc(i)    =how many at a given point
c
c        
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
      include    'blank.h'
      include    'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer jelema,jelemb,nodea(3),nodeb(3),noda,nodb,nodca(3),
     +        nodcb(3),nsidea,nsideb,ncnt,ix,p,t,iopn, ilist,ppp,
     +        i,j,maxnbr
c
      integer p2t(MAXNB,*), p2tc(*)
      data maxnbr/MAXNB/
c 
c***************************** Start ****************************** 
c 
      do 10 j=1,ne
      do 10 i=1,3 
       nextel(i,j)=0 
  10  continue
c
      do 11 j=1,np
       p2tc(j)=0
       do 11 i=1,maxnbr
          p2t(i,j) = 0
  11  continue
c 
c..........Set up the point-->triangle pointer array 
      do 13 t=1,ne
      do 131 ix=1,3
        ppp = nop(ix,t)
        p2tc (ppp) = p2tc (ppp) + 1
        if (p2tc(ppp).gt.maxnbr) then
            call erset(187,linum,maxnbr)
            return
        endif
        p2t (p2tc(ppp), ppp) = t
  131   continue
  13  continue
c
c..........Now use it to set up nextel : 
c          For each triangle, we need only search for neighbours
c..........in the list at each vertex
c
c
      do 100 jelema=1,ne-1
  
      nodea(1)=nop(1,jelema)
      nodea(2)=nop(2,jelema)
      nodea(3)=nop(3,jelema)
 
      do 89 iopn = 1,3
      p = nodea(iopn)
      do 88 ilist = 1,p2tc(p)
 
       jelemb =  p2t(ilist, p)
       if (jelemb .eq. 0) goto 88
       if (jelemb .eq. jelema) goto 88

       nodeb(1) = nop(1, jelemb)
       nodeb(2) = nop(2, jelemb)
       nodeb(3) = nop(3, jelemb)
  
  
c.........find common nodes 
c
      ncnt=0
      do 50 noda=1,3
      do 50 nodb=1,3
       if (nodea(noda).ne.nodeb(nodb)) goto 50 
       ncnt=ncnt+1 
       nodca(ncnt)=noda
       nodcb(ncnt)=nodb
   50 continue
  
c.........if less than 2 common nodes, no common side 
c
      if (ncnt.lt.2) goto 88
  
c.........side 1 is side opposite node 1, thus use this nifty 
c.........algorithm to get common side number 
c
      nsidea=6-nodca(1)-nodca(2)
      nsideb=6-nodcb(1)-nodcb(2)
  
c.........make adjacency entries
c
      nextel(nsidea,jelema)=jelemb
      nextel(nsideb,jelemb)=jelema
  
c.........no need to keep looking if all 3 sides already matched
c
      if (nextel(1,jelema).ne.0.and.nextel(2,jelema).ne.0.and.
     +    nextel(3,jelema).ne.0)  goto 100
  
c.........next element b
c
   88  continue
 
c.........next list of potential neighbours (iopn list)
c
   89  continue
  
c.........next element a
c
  100 continue
c
c...Now put in negative indicies for boundary elements (used in JPOTL)
      nepb=ne
      do 210 i=1,ne
      do 210 iopn=1,3
         if(nextel(iopn,i).eq.0) then
            nepb=nepb+1
            nextel(iopn,i)=-nepb
         endif
210   continue
c
      if(nepb.gt.MAXNEPB) then
         write(*,*) 'Internal error in NXTEL - seek help!'
         stop
      endif
c
c..........See ya later
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       SUBROUTINE DEVLTS(dxmin,dxmax,dymin,dymax)
      include 'p2conf.h'
c
c                    Calculate device limits
c
c     Original : CSR           Stanford University        Nov, 1983
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include    'blank.h'
      include    'emaco.h'
c------------------------------------------------------------------
c
       integer i
       real dxmin,dxmax,dymin,dymax
c 
c*********************** Start *****************************************
c 
c                   initialize to extreme values
      dxmin=32767. 
      dxmax=-32767.
      dymin=32767. 
      dymax=-32767.
c 
c                   scan nodes
      do 200 i=1,np 
      if (cord(1,i).lt.dxmin) dxmin=cord(1,i) 
      if (cord(1,i).gt.dxmax) dxmax=cord(1,i) 
      if (cord(2,i).lt.dymin) dymin=cord(2,i) 
      if (cord(2,i).gt.dymax) dymax=cord(2,i) 
  200 continue
c 
c                   done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      LOGICAL FUNCTION LOUTS(xcord,ycord) 
      include 'p2conf.h'
c 
c                   this function determines whether an element is
c                   totally outside the plotting window 
c 
c     true if outside 
c     false if not
c 
c                   to be considered outside, the element must be 
c                   completely to the right of the window, or completely
c                   above the window, etc.   if an element is partially 
c                   to the right and partially above, it may cross the edge 
c                   of the window so it is considered inside. 
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
      include    'blank.h'
      include    'plot.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lleft,lright,labove,lbelow
      real xcord(3),ycord(3)
      integer i
c 
c****************************************************************** 
c 
c                   data
c 
c****************************************************************** 
c 
c                   start 
c 
c                   assume outside
      louts=.true.
c 
c                   assume all to right and all to left and all above etc.
      lright=.true. 
      lleft=.true.
      labove=.true. 
      lbelow=.true. 
c 
c                   check all 3 nodes 
c 
      do 20 i=1,3 
c 
c                   compare with assumptions
c                   note that ymax is at bottom 
      if(xcord(i).le.xpmax) lright=.false. 
      if(xcord(i).ge.xpmin) lleft=.false.
      if(ycord(i).le.ypmax) lbelow=.false. 
      if(ycord(i).ge.ypmin) labove=.false. 
c 
20    continue
c 
c                   if any condition is still true, it's outside
      if(lright.or.lleft.or.labove.or.lbelow) return 
c 
c                   if you get here you must be inside window 
      louts=.false. 
c 
c                   done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      LOGICAL FUNCTION LELCT(nodea,nodeb) 
      include 'p2conf.h'
c 
c                   this function determines whether two nodes are part 
c                   of one electrode. 
c 
c     true if they are
c     false if not
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
      include    'emaco.h'
c------------------------------------------------------------------
c 
      integer nodea,nodeb
c 
c*******************Start ***************************************** 
c 
      lelct= lm(nodea).ne.0 .and. lm(nodeb).ne.0 .and. 
     +       lm(nodea).eq.lm(nodeb)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE TICMK(ltop)
      include 'p2conf.h'
c 
c     date code: 800501 
c 
c                   this routine plots the tic marks along the
c                   screen (or device) edges. if ltop is false
c                   then the tics are not plotted along top edge. 
c 
c     Original : C.H.Price     Stanford University        May, 1982
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
      include    'plot.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical ltop
      integer nticx,nticy,i
      real x,y,x1,y1,xtic,ytic,fxtic,fytic,xtcsz,ytcsz,xtcszi,ytcszi
      real lxtic,lytic
c 
c****************************************************************** 
c 
c                   start 
c 
c                   scale distance mark sizes 
c...base sizes on global tic size.
      xtcszi = htic
      ytcszi = htic
c
      xtcsz=-xtcszi/ypdel 
      ytcsz=ytcszi/xpdel
c 
c                   calc. dist. between major tics and number of
c                   minor tics per major
      call faxtc2(xpmin,xpmax,.false.,.false.,fxtic,lxtic,xtic,nticx) 
      call faxtc2(ypmin,ypmax,.false.,.false.,fytic,lytic,ytic,nticy) 
      nticx=nticx+1 
      nticy=nticy+1 
      xtic=xtic/float(nticx)
      ytic=ytic/float(nticy)
c 
c......plot the distance marks along the right side of the device.
      if(ytcsz.eq.0.) goto 70 
      x1=amin1(devxmx,xpmax)
      y1=amax1(devymn,ypmin)
      y=aint(y1/ytic) 
      if(y1.gt.0.) y=y+1. 
      i=int(y)
      y=ytic*y
c 
60    x=x1
      if(y.gt.amin1(devymx,ypmax)) goto 70
      call zmove(x,y)
      x=x-ytcsz 
      if(mod(i,nticy).eq.0) x=x-ytcsz 
      call zline(x,y,ipendn,0)
      y=y+ytic
      i=i+1 
      goto 60 
c 
c......plot the distance marks along the bottom side of the device. 
70    if(xtcsz.eq.0.) goto 90 
      y1=amin1(devymx,ypmax)
      x1=amin1(devxmx,xpmax)
      x=aint(x1/xtic) 
      i=int(x)
      x=xtic*x
c 
80    y=y1
      if(x.lt.amax1(devxmn,xpmin)) goto 90
      call zmove(x,y)
      y=y-xtcsz 
      if(mod(i,nticx).eq.0) y=y-xtcsz 
      call zline(x,y,ipendn,0)
      x=x-xtic
      i=i-1 
      goto 80 
c 
c......plot the distance marks along the left side of the device. 
90    if(ytcsz.eq.0.) goto 110
      x1=amax1(devxmn,xpmin)
      y1=amin1(devymx,ypmax)
      y=aint(y1/ytic) 
      if(y1.lt.0.) y=y-1. 
      i=int(y)
      y=ytic*y
c 
100   x=x1
      if(y.lt.amax1(devymn,ypmin)) goto 110 
      call zmove(x,y)
      x=x+ytcsz 
      if(mod(i,nticy).eq.0) x=x+ytcsz 
      call zline(x,y,ipendn,0)
      y=y-ytic
      i=i-1 
      goto 100
c 
c......plot the distance marks along top side of the device if required.
110   if(.not.ltop) goto 180 
      if(xtcsz.eq.0.) goto 180
      x1=amax1(devxmn,xpmin)
      y1=amax1(devymn,ypmin)
      x=aint(x1/xtic) 
      i=int(x)
      x=xtic*x
c 
120   y=y1
      if(x.gt.amin1(devxmx,xpmax)) goto 180 
      call zmove(x,y)
      y=y+xtcsz 
      if(mod(i,nticx).eq.0) y=y+xtcsz 
      call zline(x,y,ipendn,0)
      x=x+xtic
      i=i+1 
      goto 120
c 
c                   done
180   return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION FEVAL(node,mode,labsol)
      include 'p2conf.h'
c
c     return a function value (specified by "mode") at the 
c     mesh point "node". 
c
c     Modified: A.Yabuta Stanford University        Jul, 1987
c               (added branch 24-26 for impact ionization)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include    'blank.h'
      include    'setup.h'
      include    'emaco.h'
c....the magic TMP common memory overlays ....
      include    'adjtmp.h'
      include    'difftmp.h'
      integer TMPPAD(1338602)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision vpnt,vx,vy
      integer node, mode
      logical labsol
c
c *** Start ***
c
c
c...Branch based on mode
      goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
     +24,25,26), mode
c
c-----------------------------------------------------------------------
c 
c...Potential 
    1 feval=fv(node)*dktq
      goto 99
c 
c...electron qf
    2 continue
      feval=qfn(node)*dktq
      goto 99
c 
c...hole qf
    3 continue
      feval=qfp(node)*dktq
      goto 99
c 
c...doping
    4 feval=r1(node)
      if(lscale) feval=r1(node)*sngl(dcscl)
      goto 99
c 
c...electron concentration
    5 feval=fn(node)*dcscl
      goto 99
c 
c...hole concentration
    6 feval=fp(node)*dcscl
      goto 99
c 
c...net charge concentration
    7 if(ldiff) then
         feval=(fp(node)-fn(node))*dcscl*qcharg
      else
         feval=dcscl*qcharg*(fp(node)-fn(node)+r1(node)+
     +                     qss(node)*dintf(node))
      endif
      goto 99
c 
c...net carrier concentration 
    8 feval=(fp(node)-fn(node))*dcscl
      goto 99
c
c...valence band 1) only works in semiconductors
    9 feval=0.
      if (mattyp(itype(node)).gt.0)
     +   feval=(fv(node)+dlog(dble(nvband))-lgcnie(node))*dktq
c.. Changed/Added by Z. Yu on 1/23/91
c   Bug
c    +   feval=(fv(node)+pgap-lgcnie(node))*dktq
c.. End of 1/23/91
      goto 99
c
c...conduction band
   10 feval=0.
      if (mattyp(itype(node)).gt.0)
     +   feval=(fv(node)-dlog(dble(ncband))+lgcnie(node))*dktq
c.. Changed/Added by Z. Yu on 1/23/91
c   Bug
c    +   feval=(fv(node)-ngap+lgcnie(node))*dktq
c.. End of 1/23/91
      goto 99
c
c...node current/field/recombination
   11 continue
   12 continue
   13 continue
   14 continue
   15 continue
   16 continue
   17 continue
      if(ldiff) then
         feval=jxi(node,mode-10)
      else
         feval=vpnt(node,mode-10,vx,vy,.false.)
      endif
      goto 99
c
c...flowlines
   18 continue
      feval=wsol(node)
      goto 99
c
c...these codes are not assigned yet
   19 continue
   20 continue
   23 write(6,*) 'Warning - bad index in feval'
      feval=0.
      goto 99
c
c...electron depletion edges
   21 feval=0. 
      if(r1(node).gt.0.) feval=fn(node)/r1(node)
      goto 99
c 
c...hole depletion edges
   22 feval=0. 
      if(r1(node).lt.0.) feval=-fp(node)/r1(node) 
      goto 99
c
c 23  goto 19     ! Undefined
c
c...generation by impact ionization
   24 continue
      feval=vpnt(node,8,vx,vy,.false.)
      goto 99
c
c...ionization rate(n) by impact ionization
   25 continue
      feval=vpnt(node,9,vx,vy,.false.)
      goto 99
c
c...ionization rate(p) by impact ionization
   26 continue
      feval=vpnt(node,10,vx,vy,.false.)
      goto 99
c
c-----------------------------------------------------------------------
c
c...If absolute value flag set, do it - then leave
   99 if(labsol) feval=abs(feval)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE LIL(p,dp,q,dq,alph,parll)
c 
c     Original : CSR           Stanford University        Oct, 1982
c
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real p(2),dp(2),q(2),dq(2),alph(2),denom,dz1,dz2
      logical parll
c
c     Find the intersection of two lines given in
c     point,vector form
c
           parll = .false.
           denom = -(dp(1)*dq(2) - dq(1)*dp(2))
           if(abs(denom).le.1e-30) goto 100
           denom = 1.d0/denom
           dz1 = q(1)-p(1)
           dz2 = q(2)-p(2)
           alph(1) = dz2*dq(1) - dz1*dq(2)
           alph(1) = alph(1) * denom
           alph(2) = dp(1)*dz2 - dp(2)*dz1
           alph(2) = alph(2) * denom
           return
100        parll = .true.
           alph(1) = 0
           alph(2) = 0
           return
           end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION MMOD( k, b )
c
      integer k,b,k1
c
c...f77 doesn't implement mod correctly
      if(k.gt.0) k1 = mod(k,b)
      if (k.eq.0) k1 = 0
      if (k.lt.0) k1 = b - mod(-k,b)
      mmod = k1
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION TMOD(k)
c
      integer k,mmod,k1
c
      k1 = k-1
      tmod = 1+mmod(k1,3)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION DIST( x1,y1, x2,y2 )
      real x1,y1,x2,y2,y3,x3
c
      x3=x2-x1
      y3=y2-y1
      dist = sqrt(x3*x3+y3*y3)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION AOT( a1,a2,b1,b2,c1,c2)
c
c     calculate area of triangle specified by nodes a,b,c
c     Counter-clockwise triangle gives positive
c
      real a1,a2,b1,b2,c1,c2
      aot = (b1*(c2-a2) + c1*(a2-b2) + a1*(b2-c2) ) * 0.5
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BARYC ( x,y, trin, ksi1, ksi2, ksi3 )
      include 'p2conf.h'
c 
c     Calculate the barycentric coordinates of (x,y) in triangle trin
c
c     Original : CSR           Stanford University        Oct, 1983
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    common area
c
      include    'emaco.h'
c------------------------------------------------------------------
c
c                    Local types
c
      integer trin
      real x,y, ksi1,ksi2,ksi3, x1,x2,x3, y1,y2,y3, denom
c
c********************** Start ***********************************
c
c                    Decode triangle trin
c
      x1 =  cord(1, nop(1,trin) )
      y1 =  cord(2, nop(1,trin) )
      x2 =  cord(1, nop(2,trin) )
      y2 =  cord(2, nop(2,trin) )
      x3 =  cord(1, nop(3,trin) )
      y3 =  cord(2, nop(3,trin) )
c
c                    Calculate barycords
c
      denom = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
      ksi2  = (x -x1)*(y3-y1) - (x3-x1)*(y -y1)
      ksi3  = (x2-x1)*(y -y1) - (x -x1)*(y2-y1)
      ksi2  = ksi2/denom
      ksi3  = ksi3/denom
      ksi1  = 1.0 - ksi2 - ksi3
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ARBND(fval,nval1,lmin,lmax,fmin,fmax)
c
c     Find min/max of array fval containing nval1 points
c 
c     Original : CSR           Stanford University        Oct, 1983
c     Revision : MRP           Stanford University        Feb, 1984
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      real fval(1),fmin,fmax
      integer nval1, i
      logical lmin,lmax
c
c...Min value
      if(lmin) goto 150
      fmin=fval(1)
      do 100 i=2,nval1
100   fmin=amin1(fmin,fval(i))
c
c...Max value
150   if(lmax) goto 999
      fmax=fval(1)
      do 200 i=2,nval1
200   fmax=amax1(fmax,fval(i))
c
c...Done
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DRAWSQ(x0,y0,delx,dely)
      include 'p2conf.h'
c
c     The following subroutine draws a square around the specified
c     coordinate (x0,y0) with sides of length delx and dely.
c
c     Original : MRP           Stanford University        Feb, 1984
c 
c     Copyright c 1984 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c...Common
      include    'plot.h'
c
c...Dcl
      real x0,y0,delx,dely,xp1,xm1,yp1,ym1
c
c *** Start ***
c
      xp1=x0+delx
      xm1=x0-delx
      yp1=y0+dely
      ym1=y0-dely
      call zmove(xm1,ym1)
      call zline(xp1,ym1,ipendn,0)
      call zline(xp1,yp1,ipendn,0)
      call zline(xm1,yp1,ipendn,0)
      call zline(xm1,ym1,ipendn,0)
      call zmove(x0,y0)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE V5SORT(ilen,itag,ind)
c
c                partition sorting algorthm for real abcissa
c           reference collected algorthms of the acm - 63,64,65
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c**********************************************************************
c
c              type declarations
c
      integer  ihigh(32),ilow(32),nsegs,il1,ih,sepx,ixh,ixl
      integer  ilen,itag(*),it
      real     ind(*),sep
c
c
c****************************************************************** 
c
c     intialize
      nsegs = 1
      il1= 1
      ih= ilen
c
c                    if no elements in this segment do nothing 
c
  10  if(il1.ge.ih) goto 70
c
c                    choose sep (separation entry): 
c                    make ind(il1) <= ind((il1+ih)/2) <= ind(ih) 
c                    by interchange
c
  20  sepx= (ih+il1)/2 
      ixl=il1 
      ixh=ih
      call rorder(ind(il1),ind(sepx),ind(ih), 
     +            itag(il1),itag(sepx),itag(ih)) 
      sep= ind(sepx)
  30  ixh=ixh-1 
      ixl=ixl+1 
  31  if(ind(ixh).le.sep) goto 32
         ixh=ixh-1
      goto 31
  32  if(ind(ixl).ge.sep) goto 33
         ixl=ixl+1
      goto 32
  33  if(ixl.le.ixh) then 
         call swapr(ind(ixh),ind(ixl))
         call swapi(itag(ixh),itag(ixl))
         goto 30
      endif 
      if(ixh+ixl.le.ih+il1) then
         ilow(nsegs)=ixl
         ihigh(nsegs)=ih
         ih=ixh 
      else
         ilow(nsegs)=il1
         ihigh(nsegs)=ixh 
         il1=ixl
      endif 
      nsegs=nsegs+1 
  80  if(ih-il1.ge.11) goto 20
      if(il1.eq.1) goto 10
  90  if(il1.eq.ih) goto 70 
  91  if(ind(il1).gt.ind(il1+1)) goto 92
         il1=il1+1
         if(il1.eq.ih) goto 70
      goto 91
  92  sep=ind(il1+1) 
      ixl=il1 
      it=itag(il1+1)
  100 ind(ixl+1)=ind(ixl) 
      itag(ixl+1)=itag(ixl) 
      ixl=ixl-1 
      if(sep.lt.ind(ixl)) goto 100 
      ind(ixl+1)=sep 
      itag(ixl+1)=it
      il1=il1+1 
      goto 90 
  70  nsegs=nsegs-1 
      if(nsegs.eq.0) return 
      il1=ilow(nsegs) 
      ih=ihigh(nsegs) 
      goto 80 
      end 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE SWAPR(a,b) 
c
      real a,b,temp 
      temp=b
      b=a 
      a=temp
      return
      end 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RORDER(a,b,c,pa,pb,pc)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      integer pa,pb,pc
      real    a,b,c 
c
      if(c.lt.a) then 
         call swapr(a,c)
         call swapi(pa,pc)
      endif 
      if(b.lt.a) then 
         call swapr(a,b)
         call swapi(pa,pb)
      endif 
      if(c.lt.b) then 
         call swapr(b,c)
         call swapi(pb,pc)
      endif 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE UNIQ(nval1)
      include 'p2conf.h'
c
c               Remove duplicate entries from fval/adis
c 
c     Original : CSR           Stanford University        Oct, 1983
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               common area
c
c....the magic TMP common memory overlays ....
      include    'adjtmp.h'
      include    'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c               Parameter
c
       integer nval1
c
c               Local types
c
       integer ix,i
       real scale,aold,del
c
c******************** Start ********************************************
c
       scale = adis(nval1) - adis(1)
       ix = 1
       aold=adis(1)
       do 100 i=2,nval1
       del = abs(adis(i)-aold)/scale
       if(del.gt.1e-6) then
         aold = adis(i)
           ix = ix + 1
           adis(ix) = adis(i)
           fval(ix) = fval(i)
           cord1d(1,ix) = cord1d(1,i)
           cord1d(2,ix) = cord1d(2,i)
       endif
  100  continue
       nval1 = ix
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PL1SPL(nsplin,nval1)
      include 'p2conf.h'
c
c     The following subroutine interpolates the data points in
c     (adis,fval) using piecewise cubic splines.
c 
c     Original : MRP           Stanford University        Feb, 1984
c 
c     Copyright c 1984 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               common area
c
c....the magic TMP common memory overlays ....
      include    'adjtmp.h'
      include    'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
       integer i,nval1,nsplin
       real dx,pcubic
c
c**** Start ****
c
c...Set up spline coefficient arrays
       call spline(nval1,adis,fval,sc2,sc3,sc4,splx,sply)
c
c...Get interpolated values
       dx=(adis(nval1)-adis(1))/float(nsplin)
       splx(1)=adis(1)
       do 3000 i=1,nsplin
       sply(i)=pcubic(splx(i),nval1,adis,fval,sc2,sc3,sc4)
       if(i.ne.nsplin) splx(i+1)=splx(i)+dx
 3000  continue
c
c...Done
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       REAL FUNCTION FEVAL2(node,mode,logar,labsol)
c
c               Same function as feval, but when called with 
c               logar=.true., it computes the asinh (base 10) of 
c               the result - convenient for things that go negative.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       real feval,asnh10,val
       integer node,mode
       logical logar,labsol
c
c******************** Start ********************************************
c
      val = feval(node,mode,labsol)
      if(.not.logar) then
         feval2 = val
      else if (labsol) then
         if(val.eq.0) then
            feval2=-999.
         else
            feval2=alog10(val)
         endif
      else
         feval2=asnh10(val)
      endif
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION PCUBIC(xbar,n,xi,c1,c2,c3,c4)
c
c-----------------------------------------------------------------------
c
c     The following calculates the value of a piece-wise cubic 
c     polynomial, defined by c1-c4 (obtained by spline interpolation),
c     at a point xbar.  (See Conte and de Boor, "Elementary
c     Numerical Analysis : An Algorithmic Approach", Second Edition
c     (1972), McGraw-Hill, pp. 233-240).
c
c     Original: MRP       Stanford University             Jan, 1982
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      real xi(1),c1(1),c2(1),c3(1),c4(1)
      real dx,xbar,ddx
      integer n,i
c
      i=1
      dx=xbar-xi(1)
c
      if(dx) 10,30,20
10    if(i.eq.1) goto 30
      i=i-1
      dx=xbar-xi(i)
      if(dx) 10,30,30
19    i=i+1
      dx=ddx
20    if(i.eq.n) goto 30
      ddx=xbar-xi(i+1)
      if(ddx) 30,19,19
c
30    pcubic=c1(i)+dx*(c2(i)+dx*(c3(i)+dx*c4(i)))
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SPLINE(n,xi,c1,c2,c3,c4,d,diag)
c 
c-----------------------------------------------------------------------
c
c     Calculate spline polynomial coeff's given data points and 
c     curve slopes at endpoints.  (See Conte and de Boor, "Elementary
c     Numerical Analysis : An Algorithmic Approach", Second Edition
c     (1972), McGraw-Hill, pp. 233-240).
c
c     Original: MRP       Stanford University             Jan, 1982
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
c 
      real xi(1),c1(1),c2(1),c3(1),c4(1),d(1),diag(1) 
      real g,divdf1,divdf3,dx
      integer n,np1,m,nj
c
      diag(1)=1.0
      d(1)=0.0
c
      np1=n+1 
      do 10 m=2,np1 
         d(m)=xi(m)-xi(m-1) 
10       diag(m)=(c1(m)-c1(m-1))/d(m) 
c
      do 20 m=2,n 
         c2(m)=3.0*(d(m)*diag(m+1)+d(m+1)*diag(m)) 
20       diag(m)=2.0*(d(m)+d(m+1))
c
      do 30 m=2,n 
         g=-d(m+1)/diag(m-1)
         diag(m)=diag(m)+g*d(m-1) 
30       c2(m)=c2(m)+g*c2(m-1) 
c
      nj=np1
      do 40 m=2,n 
         nj=nj-1
40       c2(nj)=(c2(nj)-d(nj)*c2(nj+1))/diag(nj) 
c
      do 50 m=1,n 
         dx=xi(m+1)-xi(m) 
         divdf1=(c1(m+1)-c1(m))/dx
         divdf3=c2(m)+c2(m+1)-2.0*divdf1
         c3(m)=(divdf1-c2(m)-divdf3)/dx 
50       c4(m)=divdf3/dx/dx 
c
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
        REAL FUNCTION ASNH10(z)
c
c               Calculate log(10/e) * asinh(z)
c               Useful for log10 of variables which can go negative
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c.................local types
        real z,zab
c
c******************** Start ********************************************
c
c.................Actually compute something else.
      zab = abs(z)
      if (zab.gt.1e5) then
           asnh10 = sign(1.0,z) * alog10(zab)
      else
           asnh10 = sign(1.0,z) * alog10( 1.0 + zab )
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PCROSS
      include 'p2conf.h'
c
c     The following subroutine prints crosses at grid points for 2D
c     plots.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include    'blank.h'
      include    'plot.h'
      include    'emaco.h'
c------------------------------------------------------------------
c
      integer i
      real delx,dely,x,y,c1,c2

      delx=(xpwid/xpwid0)*(xpmax-xpmin)/400.
      dely=(ypwid/ypwid0)*(ypmax-ypmin)/200.
      do 2000 i=1,np
      x=cord(1,i)
      y=cord(2,i)
      c1=x-delx
      c2=x+delx
      call zmove(c1,y)
      call zline(c2,y,ipendn,0)
      c1=y-dely
      c2=y+dely
      call zmove(x,c1)
      call zline(x,c2,ipendn,0)
2000  continue
c
c...Done; return to alpha mode
      call fgtoa
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE XSIDE(conval,funct,xe,ye,x1,y1,x2,y2,j1,j2)
c
c     Find the intersections of a contour with a triangle.
c     conval      - the contour
c     funct      - 3 triangle function values
c     xe      - 3 xcoords of triangle
c     ye      - 3 ycoords of triangle
c     x1,y1     - first intersection found (in order of side number)
c     x2,y2     - second
c     j1,j2      - intersected sides.
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real conval,funct(3),xe(3),ye(3),x1,y1,x2,y2,funfr,funto,tau,x,y
      integer j1,j2,inext(3),iprev(3),is,isn,isp,count
      data inext/2,3,1/,iprev/3,1,2/
c
c******************** Start ********************************************
c
      j1=0
      j2=0
      count=0
      if( (conval.lt.funct(1).and.conval.lt.funct(2)
     +     .and.conval.lt.funct(3)) .or.
     +     (conval.gt.funct(1).and.conval.gt.funct(2)
     +     .and.conval.gt.funct(3)) ) return

      do 100 is=1,3
       isn=inext(is)
       isp=iprev(is)

c...........See if this side intersects contour
       funfr=funct(isp)
       funto=funct(isn)
       if((funfr.gt.conval .and. funto.gt.conval) .or.
     +       (funfr.lt.conval .and. funto.lt.conval)) goto 100

       if(funfr.eq.funto) then
c..............oops, function equals conval on entire side.
          j1=isp
          j2=isn
          x1=xe(isp)
          y1=ye(isp)
          x2=xe(isn)
          y2=ye(isn)
          return
       endif

c..............normal case - function values span conval
       tau=(conval - funfr) / (funto - funfr)
         x=(xe(isn) - xe(isp))*tau + xe(isp)
       y=(ye(isn) - ye(isp))*tau + ye(isp)
       count=count+1
       if(count.eq.1) then
          x1=x
          y1=y
          j1=is
       else
          x2=x
          y2=y
          j2=is
       endif

  100 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PCLIP(np,poly,xmin,xmax,ymin,ymax)
c
c...Polygon clip, using Sutherland-Hodgeman algorithm.
c   Ivan E. Sutherland & Gary W. Hodgman, Comm. ACM, Vol.17 (Jan.74)
c
c   Input:
c          np   - number of points in the polygon 
c       poly    - nodes of poly
c       xmin,xmax,ymin,ymax - bounds of screen
c   Output:
c      np       - number of nodes in clipped polygon
c      poly    - new nodes
c
c...Original : CSR Sep 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real poly(2,10),xmin,xmax,ymin,ymax,swap,sign
      integer np,ip
c
c******************** Start ********************************************
c
c.......Clip against left edge : 
      sign=1.0
      call plane(np,poly,xmin,sign)
      if(np.eq.0) return

c.......Clip against right edge :
      sign=-1.0
      call plane(np,poly,xmax,sign)
      if(np.eq.0) return

c.......Swap x and y coords
      do 100 ip=1,np
         swap=poly(1,ip)
         poly(1,ip)=poly(2,ip)
       poly(2,ip)=swap
  100 continue

c........Clip against bottom
      sign=1.0
      call plane(np,poly,ymin,sign)
      if(np.eq.0) return

c........Clip against top
      sign=-1.0
      call plane(np,poly,ymax,sign)
      if(np.eq.0) return

c........Swap back
      do 200 ip=1,np
         swap=poly(1,ip)
         poly(1,ip)=poly(2,ip)
       poly(2,ip)=swap
  200 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PLANE (np,poly,bound,sign)
c
c...Clip polygon against a coordinate plane.
c...(Phase 2 of S-H algorithm)
c   np          = number of points
c   poly       = coordinates (may be switched around to clip y coords)
c   bound      = clipping plane is x=bound
c   sign       =+1 then x>bound should be visible
c                 -1 then x<bound should be visible
c
c...Original : CSR Sep 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real poly(2,10),bound,px,py,tx,ty,sign,npoly(2,10),fx,fy
      real delt,delp,delf,alpha
      integer np,ip,newnp
      logical first
c
c******************** Start ********************************************
c
c......tx=this x, px=previous x, etc.

c......Create new polygon from old by clipping.
      first=.true.
      newnp=0

      do 100 ip=1,np
       tx=poly(1,ip)
       ty=poly(2,ip)
       delt=sign*(tx-bound)
       if(first) then
          px=tx
          py=ty
          delp=delt
          fx=tx
          fy=ty
          delf=delt
       else
         if( (delt.gt.0) .eqv. (delp.le.0) ) then
             newnp=newnp+1
             alpha=delt / (delt-delp)
             npoly(1,newnp)=bound
             npoly(2,newnp)=ty + alpha*(py-ty)
          endif
       endif
       px=tx
       py=ty
       delp=delt
       if(delt .gt. 0) then
          newnp=newnp+1
          npoly(1,newnp)=tx
          npoly(2,newnp)=ty
       endif
       first=.false.
  100 continue

      if(np.gt.0) then
       if( (delf.gt.0) .eqv. (delt.le.0) ) then
          newnp=newnp+1
          alpha=delf / (delf-delt)
          npoly(1,newnp)=bound
          npoly(2,newnp)=fy + alpha*(ty-fy)
       endif
      endif
      
c......Now copy back to old
      np=newnp
      do 200 ip=1,np
       poly(1,ip)=npoly(1,ip)
       poly(2,ip)=npoly(2,ip)
  200 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IGRATE(fy,fx,nrec,scalx,LIVER)
c
      include 'p2conf.h'
c
c     Integrate (line integral) a function y (fy) wrt x (fx).  
c     Return as fy.  Use trapezoidal rule for now.
c
c     Original : MRP Oct 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'symme.h'
      real fy(1),fx(1),y0,sum,x0,scalx,fact
      integer i,nrec
      logical liver
c
c Start
c
      fact=0.5/scalx
      if (lwidth) then
         fact=fact*width
      endif
      y0=fy(1)
      x0=fx(1)
      sum=0.
      fy(1)=sum
      do 10 i=2,nrec
         if (LCYL.AND..NOT.LIVER) then
            sum=sum+3.1415927*(fx(i)+x0)*fact*(fx(i)-x0)*(fy(i)+y0)
         else
            sum=sum+fact*(fx(i)-x0)*(fy(i)+y0)
         endif
         y0=fy(i)
         x0=fx(i)
         fy(i)=sum
10    continue
c
c...Bye
      return
      end
