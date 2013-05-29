cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SMOBL(nel,n1,n2,mu21,lmup,ldmu)
      include 'p2conf.h'
c
c     Compute surface mobility
c
c        nel.......element number
c        n1........source node
c        n2........sink node
c        mu21......mobility between nodes 1 and 2
c        lmup......flag to indicate hole mobility
c        ldmu......flag to indicate need to calculate derivative of
c                  mobility w.r.t. psi
c
c     Original:    Jeffrey T. Watt                     May,   1987
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include 'blank.h'
      include 'setup.h'
      include 'emaco.h'
c
c
      logical lmup,ldmu
      integer j,nelmat,melmat
      integer nel,n1,n2,n3,n1node,n2node,n3node
      integer mel,m1,m2,m3,m1node,m2node,m3node
      double precision xn1,yn1,xn2,yn2,xn3,yn3
      double precision dxn1,dyn1,dxn2,dyn2,dxn3,dyn3
      double precision dn1,dn2,dn3,sn,hn,fhn,en
      double precision xm1,ym1,xm2,ym2,xm3,ym3
      double precision dxm1,dym1,dxm2,dym2,dxm3,dym3
      double precision dm1,dm2,dm3,sm,hm,fhm,em
      double precision mu21,mus,eins,esem
     

c*************
c**  START  **
c*************
c
c
c...Calculate field in element nel
      nelmat=mattyp(imat(nel))
      n3=6-n1-n2
      n1node=nop(n1,nel)
      n2node=nop(n2,nel)
      n3node=nop(n3,nel)
      xn1=cord(1,n1node)
      yn1=cord(2,n1node)
      xn2=cord(1,n2node)
      yn2=cord(2,n2node)
      xn3=cord(1,n3node)
      yn3=cord(2,n3node)
      dxn1=xn3-xn2
      dyn1=yn3-yn2
      dxn2=xn1-xn3
      dyn2=yn1-yn3
      dxn3=xn2-xn1
      dyn3=yn2-yn1
      dn1=dsqrt(dxn1*dxn1+dyn1*dyn1)
      dn2=dsqrt(dxn2*dxn2+dyn2*dyn2)
      dn3=dsqrt(dxn3*dxn3+dyn3*dyn3)
      sn=es(1,nel)+es(2,nel)+es(3,nel)
      hn=2.0d0*sn/dn3
      fhn=dsqrt(dabs(dn1*dn1-hn*hn))/dn3
      en=dktq*(fv(n3node)-(fv(n1node)+fhn*(fv(n2node)-fv(n1node))))/hn
c...Calculate field in adjacent element
      mel=nextel(n3,nel)
      do 100 j=1,3
      if(nop(j,mel).eq.n1node) m1=j
      if(nop(j,mel).eq.n2node) m2=j
  100 continue
      melmat=mattyp(imat(mel))
      m3=6-m1-m2
      m1node=nop(m1,mel)
      m2node=nop(m2,mel)
      m3node=nop(m3,mel)
      xm1=cord(1,m1node)
      ym1=cord(2,m1node)
      xm2=cord(1,m2node)
      ym2=cord(2,m2node)
      xm3=cord(1,m3node)
      ym3=cord(2,m3node)
      dxm1=xm3-xm2
      dym1=ym3-ym2
      dxm2=xm1-xm3
      dym2=ym1-ym3
      dxm3=xm2-xm1
      dym3=ym2-ym1
      dm1=dsqrt(dxm1*dxm1+dym1*dym1)
      dm2=dsqrt(dxm2*dxm2+dym2*dym2)
      dm3=dsqrt(dxm3*dxm3+dym3*dym3)
      sm=es(1,mel)+es(2,mel)+es(3,mel)
      hm=2.0d0*sm/dm3
      fhm=dsqrt(dabs(dm1*dm1-hm*hm))/dm3
      em=dktq*(fv(m3node)-(fv(m1node)+fhn*(fv(m2node)-fv(m1node))))/hm
c...Determine effective field and mobility
      if(nelmat.le.0) then
         eins=en*epsmat(imat(nel))/epsmat(imat(mel))
         esem=-em
      else
         eins=em*epsmat(imat(mel))/epsmat(imat(nel))
         esem=-en
      endif
      if(lmup) then
         if ( esem+(eins-esem)/3.0d0 .eq. 0.0 ) then
             mus = mu21
         else
             mus=84.5d0*(dabs(esem+(eins-esem)/3.0d0)/1.0d6)**(-.315)
         endif
      else
         if ( esem+(eins-esem)/2.0d0 .eq. 0.0 ) then
             mus = mu21
         else
             mus=389.0d0*(dabs(esem+(eins-esem)/2.0d0)/1.0d6)**(-.285)
         endif
      endif
c...Limit maximum mobility value to bulk mobility
      if (mus.lt.mu21) mu21=mus
      return
      end 

