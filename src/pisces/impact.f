cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Feb 24 01:03:46 PST 1991 (anderson-stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IMPACE(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +dgic,dgip,dev,lalph)
      include 'p2conf.h'
c
c     The following routine calculates the net generation (gi) at
c     element(triangle) as well as the derivatives with respect to psi,
c     n and p (dgip,dgic). 
c
c     Original:  Akira Yabuta  Stanford University    April. 1987
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'impact.h'
c------------------------------------------------------------------
c
      logical lpcont,lalph
      double precision gi(3)
      double precision jside(3),heigh(3),ei(3)
      double precision api(3),bpi(3),ani(3),bni(3)
      double precision area(3),tmp1,tmp2,tmp3
      double precision jnm(3)
      integer i,i1,eiflg
      double precision 
     +     gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3),
     +     djmc(3,3),djmp(3,3),alph(3),dev(3,3)
c
c...Coefficients for impact ionization(by GRANT)
      data api(1)/5.6e5/,api(2)/2.0e6/,api(3)/2.0e6/
      data bpi(1)/1.32e6/,bpi(2)/1.97e6/,bpi(3)/1.97e6/
      data ani(1)/5.0e5/,ani(2)/6.2e5/,ani(3)/2.6e6/
      data bni(1)/0.99e6/,bni(2)/1.08e6/,bni(3)/1.43e6/
c
c...Coefficients for impact ionization(by Overstraeten)
c      data api(1)/6.71e5/,api(2)/1.582e6/
c      data bpi(1)/1.693e6/,bpi(2)/2.036e6/
c      data ani(1)/7.03e5/,ani(2)/7.03e5/
c      data bni(1)/1.231e6/,bni(2)/1.231e6/
c
c...Coefficients for impact ionization(by Lee)
c      data api(1)/2.25e7/
c      data bpi(1)/3.26e6/
c      data ani(1)/3.8e6/
c      data bni(1)/1.75e6/
c
c********************
c**  START IMPACT  **
c********************
c
c...Impact ionization
      if(.not.limpct) goto 999
c 
c******************** Start ********************************************
c
c...If ei=0 or heigh(i)=0 then go easy way
      do 111 i=1,3 
c       write(*,7008) ei(i)
c7008   format(1x,'ei=',e12.4)
      if ((ei(i).eq.0.d0).or.(heigh(i).eq.0.d0)) then
         gi(i)=0.0d0
         dgip(i,1)=0.d0
         dgip(i,2)=0.d0
         dgip(i,3)=0.d0
         dgic(i,1)=0.d0
         dgic(i,2)=0.d0
         dgic(i,3)=0.d0
         goto 120
      endif
c
c...Change impact ionization coefficients by Electric field(by Grants)
      eiflg=3
      if (ei(i).gt.(2.4e5)) eiflg=2
      if (ei(i).gt.(5.3e5)) eiflg=1
c
c...Change impact ionization coefficients by Electric field(by Overstraeten)
c      eiflg=2
c      if (ei(i).gt.(4.0e5)) eiflg=1
c
c...Change impact ionization coefficients by Electric field(by Lee)
c      eiflg=1
c
c...Caluculate Alph 
      if (lpcont) then
        alph(i)=api(eiflg)*dexp(-1.d0*bpi(eiflg)/ei(i))
      else
        alph(i)=ani(eiflg)*dexp(-1.d0*bni(eiflg)/ei(i))
      endif
c
c...Calculate effective current(along side of triangle)
      jnm(i)=dabs(jside(i))
      do 121 i1=1,3
       if(jside(i).ge.0.d0) then
       djmc(i,i1)=gsc(i,i1)
       djmp(i,i1)=gsp(i,i1)
       else
       djmc(i,i1)=-gsc(i,i1)
       djmp(i,i1)=-gsp(i,i1)
       endif
121   continue
c
c...Calculate generation term
      gi(i)=alph(i)*jnm(i)*area(i)
      if(lalph) gi(i)=alph(i)*area(i)
c
c...Calculate derivative of Alph / potential
      do 117 i1=1,3
      if(lpcont) then
       dgip(i,i1)=dev(i,i1)*alph(i)*bpi(eiflg)/(ei(i)*ei(i))
      else
       dgip(i,i1)=dev(i,i1)*alph(i)*bni(eiflg)/(ei(i)*ei(i))
      endif
      dgip(i,i1)=(dgip(i,i1)*jnm(i)+alph(i)*djmp(i,i1))*area(i)
      dgic(i,i1)=alph(i)*djmc(i,i1)*area(i)
117   continue
c
120   continue
c
c
111   continue
c
c
c...Rearrange gi(i) because gi(1) should be generation term 
c           in node, and reverse sign
c   Rearrange gi(i) and normalized by area
c         if lalph=.true. (gi is alpha here)
       tmp1=gi(1)
       tmp2=gi(2)
       tmp3=gi(3)
      if(lalph) then
       gi(1)=tmp2+tmp3
       gi(2)=tmp1+tmp3
       gi(3)=tmp1+tmp2
       tmp1=area(1)
       tmp2=area(2)
       tmp3=area(3)
       area(1)=tmp2+tmp3
       area(2)=tmp1+tmp3
       area(3)=tmp1+tmp2
      else
       gi(1)=(tmp2+tmp3)/(-2.0d0)
       gi(2)=(tmp1+tmp3)/(-2.0d0)
       gi(3)=(tmp1+tmp2)/(-2.0d0)
      endif
c
c...Rearrange dgic,p(i) because dgic,p(1) should be term 
c           in node
      do 118 i1=1,3
      tmp1=dgic(1,i1)
      tmp2=dgic(2,i1)
      tmp3=dgic(3,i1)
      dgic(1,i1)=(tmp2+tmp3)/2.0d0
      dgic(2,i1)=(tmp1+tmp3)/2.0d0
      dgic(3,i1)=(tmp1+tmp2)/2.0d0
118   continue
c
      do 119 i1=1,3
      tmp1=dgip(1,i1)
      tmp2=dgip(2,i1)
      tmp3=dgip(3,i1)
      dgip(1,i1)=(tmp2+tmp3)/2.0d0
      dgip(2,i1)=(tmp1+tmp3)/2.0d0
      dgip(3,i1)=(tmp1+tmp2)/2.0d0
119   continue
c
c
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IMPAC1(lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +dgic,dgip,dev,lalph)
      include 'p2conf.h'
c
c     The following routine calculates the net generation (gi) at
c     element(triangle) as well as the derivatives with respect to psi,
c     n and p (dudv,dudn,dudp)  by Crowell Sze Model(CSM).
c
c     Original:  Akira Yabuta  Stanford University  June. 1987
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'impact.h'
c------------------------------------------------------------------
c
      logical lpcont,lalph
      double precision gi(3)
      double precision jside(3),heigh(3),ei(3)
      double precision area(3),tmp1,tmp2,tmp3
      double precision jnm(3)
      integer i,i1
      double precision 
     +     gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3),
     +     djmc(3,3),djmp(3,3),alph(3),dev(3,3),
     +     energ6,c0,c1,c2,rfact,xfact,rlamda
c
c********************
c**  START IMPACT  **
c********************
c
c...Impact ionization
      if(.not.limpct) goto 999
c 
c******************** Start ********************************************
c
c...If ei=0 then go easy way
      do 111 i=1,3 
      if ((ei(i).eq.0.d0).or.(heigh(i).eq.0.d0)) then
         gi(i)=0.0d0
         dgip(i,1)=0.d0
         dgip(i,2)=0.d0
         dgip(i,3)=0.d0
         dgic(i,1)=0.d0
         dgic(i,2)=0.d0
         dgic(i,3)=0.d0
         goto 120
      endif
c
c...Calculate coefficients
      if (lpcont) then
        energ6=1.8
        rlamda=rlamdh
      else
        energ6=1.1
        rlamda=rlamde
      endif
      rfact=0.063/energ6
      c0=-1.92+75.5*rfact-757.0*(rfact**2.)
      c1=1.75*0.01-11.9*rfact+46.0*(rfact**2.)
      c2=3.9*0.0001-1.17*rfact+11.5*(rfact**2.)
      xfact=energ6/(rlamda*ei(i))
c
c...Caluculate Alph 
      alph(i)=(dexp(c0+c1*xfact+c2*(xfact**2.)))/rlamda
c
c...Calculate effective current(along side of triangle)
      jnm(i)=dabs(jside(i))
      do 121 i1=1,3
       if(jside(i).ge.0.d0) then
       djmc(i,i1)=gsc(i,i1)
       djmp(i,i1)=gsp(i,i1)
       else
       djmc(i,i1)=-gsc(i,i1)
       djmp(i,i1)=-gsp(i,i1)
       endif
121   continue
c
c...Calculate generation term
      gi(i)=alph(i)*jnm(i)*area(i)
      if(lalph) gi(i)=alph(i)*area(i)
c
c...Calculate derivative of Alph / potential
      do 117 i1=1,3
       dgip(i,i1)=-1.*energ6/(rlamda*(ei(i)**2.))
       dgip(i,i1)=dgip(i,i1)-2.*c2*(energ6**2.)/((rlamda**2.)*
     *(ei(i)**3.))
       dgip(i,i1)=dev(i,i1)*alph(i)*dgip(i,i1)
      dgip(i,i1)=(dgip(i,i1)*jnm(i)+alph(i)*djmp(i,i1))*area(i)
      dgic(i,i1)=alph(i)*djmc(i,i1)*area(i)
117   continue
c
120   continue
c
111   continue
c
c
c...Rearrange gi(i) because gi(1) should be generation term 
c           in node, and reverse sign
c   Rearrange gi(i) and normalized by area
c         if lalph=.true. (gi is alpha here)
       tmp1=gi(1)
       tmp2=gi(2)
       tmp3=gi(3)
      if(lalph) then
       gi(1)=tmp2+tmp3
       gi(2)=tmp1+tmp3
       gi(3)=tmp1+tmp2
       tmp1=area(1)
       tmp2=area(2)
       tmp3=area(3)
       area(1)=tmp2+tmp3
       area(2)=tmp1+tmp3
       area(3)=tmp1+tmp2
      else
       gi(1)=(tmp2+tmp3)/(-2.0d0)
       gi(2)=(tmp1+tmp3)/(-2.0d0)
       gi(3)=(tmp1+tmp2)/(-2.0d0)
      endif
c
c...Rearrange dgic,p(i) because dgic,p(1) should be term 
c           in node
      do 118 i1=1,3
      tmp1=dgic(1,i1)
      tmp2=dgic(2,i1)
      tmp3=dgic(3,i1)
      dgic(1,i1)=(tmp2+tmp3)/2.0d0
      dgic(2,i1)=(tmp1+tmp3)/2.0d0
      dgic(3,i1)=(tmp1+tmp2)/2.0d0
118   continue

      do 119 i1=1,3
      tmp1=dgip(1,i1)
      tmp2=dgip(2,i1)
      tmp3=dgip(3,i1)
      dgip(1,i1)=(tmp2+tmp3)/2.0d0
      dgip(2,i1)=(tmp1+tmp3)/2.0d0
      dgip(3,i1)=(tmp1+tmp2)/2.0d0
119   continue
c
c
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IMPAC2(ie,lpcont,ei,jside,heigh,area,gi,gsc,gsp,
     +dgic,dgip,dev,lalph)
      include 'p2conf.h'
c
c     The following routine calculates the net generation (gi) at
c     element(triangle) as well as the derivatives with respect to psi,
c     n and p (dudv,dudn,dudp). 
c     Alpha values are extracted by Monte Carlo Simulation.
c
c     Original:  Akira Yabuta  Stanford University    SEP. 1987
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'impact.h'
      include     'monte.h'
c------------------------------------------------------------------
c
c
      logical lpcont,lalph
      double precision gi(3)
      double precision jside(3),heigh(3),ei(3)
      double precision area(3),tmp1,tmp2,tmp3
      double precision jnm(3)
      integer i,i1
      integer ie
      double precision 
     +     gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3),
     +     djmc(3,3),djmp(3,3),alph(3),dev(3,3)
c
c********************
c**  START IMPACT  **
c********************
c
c...Impact ionization
      if(.not.limpct) goto 999
c 
c******************** Start ********************************************
c
c...If heigh=0 then go easy way
      do 111 i=1,3 
      if (heigh(i).eq.0.d0) then
         gi(i)=0.0d0
         dgip(i,1)=0.d0
         dgip(i,2)=0.d0
         dgip(i,3)=0.d0
         dgic(i,1)=0.d0
         dgic(i,2)=0.d0
         dgic(i,3)=0.d0
         goto 120
      endif
c
c...Store Alph 
c   Alpha(n) and Alpha(p) are same (should be changed later)
      if (lpcont) then
        alph(i)=alphmc(i,ie)
      else
        alph(i)=alphmc(i,ie)
      endif
c
c...Calculate effective current(along side of triangle)
      jnm(i)=dabs(jside(i))
      do 121 i1=1,3
       if(jside(i).ge.0.d0) then
       djmc(i,i1)=gsc(i,i1)
       djmp(i,i1)=gsp(i,i1)
       else
       djmc(i,i1)=-gsc(i,i1)
       djmp(i,i1)=-gsp(i,i1)
       endif
121   continue
c
c...Calculate generation term
      gi(i)=alph(i)*jnm(i)*area(i)
      if(lalph) gi(i)=alph(i)*area(i)
c
c...Calculate derivative of Alph / potential
      do 117 i1=1,3
      dgip(i,i1)=alph(i)*djmp(i,i1)*area(i)
      dgic(i,i1)=alph(i)*djmc(i,i1)*area(i)
117   continue
c
120   continue
c
c
111   continue
c
c
c...Rearrange gi(i) because gi(1) should be generation term 
c           in node, and reverse sign
c   Rearrange gi(i) and normalized by area
c         if lalph=.true. (gi is alpha here)
       tmp1=gi(1)
       tmp2=gi(2)
       tmp3=gi(3)
      if(lalph) then
       gi(1)=tmp2+tmp3
       gi(2)=tmp1+tmp3
       gi(3)=tmp1+tmp2
       tmp1=area(1)
       tmp2=area(2)
       tmp3=area(3)
       area(1)=tmp2+tmp3
       area(2)=tmp1+tmp3
       area(3)=tmp1+tmp2
      else
       gi(1)=(tmp2+tmp3)/(-2.0d0)
       gi(2)=(tmp1+tmp3)/(-2.0d0)
       gi(3)=(tmp1+tmp2)/(-2.0d0)
      endif
c
c...Rearrange dgic,p(i) because dgic,p(1) should be term 
c           in node
      do 118 i1=1,3
      tmp1=dgic(1,i1)
      tmp2=dgic(2,i1)
      tmp3=dgic(3,i1)
      dgic(1,i1)=(tmp2+tmp3)/2.0d0
      dgic(2,i1)=(tmp1+tmp3)/2.0d0
      dgic(3,i1)=(tmp1+tmp2)/2.0d0
118   continue

      do 119 i1=1,3
      tmp1=dgip(1,i1)
      tmp2=dgip(2,i1)
      tmp3=dgip(3,i1)
      dgip(1,i1)=(tmp2+tmp3)/2.0d0
      dgip(2,i1)=(tmp1+tmp3)/2.0d0
      dgip(3,i1)=(tmp1+tmp2)/2.0d0
119   continue
c
c
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       SUBROUTINE IMPCCK
      include 'p2conf.h'
c
c     The following routine set values for impact ionization
c
c     Original:  Akira Yabuta  Stanford University    April. 1987
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'impact.h'
c------------------------------------------------------------------
c
c FUNCTIONS:
       logical iscval
       logical gtlval
       real    gtrval
c
c********************
c**  START IMPCCK  **
c********************
c
c...impact ionization flag on    
c   Be carefull this flag is also turned on by model card
        limpct=.true.
c--------------------------------------------------------------------------
c                     Key-Values for impact ionization 
c  rlamde   : mean free path for electron        ; default 6.2e-7cm
c  rlamdh   : mean free path for hole            ; default 3.8e-7cm
c
c------------------------------------------------------------------------------
      rlamde = gtrval(1) 
      rlamdh = gtrval(2)
c
      lcsm   = gtlval(1)
      lbreak = gtlval(2)
      lpath  = gtlval(3)
c
      mntnam = ' '
      if (iscval(1)) call gtcval(1, mntnam, LEN(mntnam))
      lmont = (mntnam(1:1).ne.' ')
c
      if((rlamde.le.0.0).and.lcsm) call erset(303,linum,0)
      if((rlamdh.le.0.0).and.lcsm) call erset(303,linum,0)
c
      return
      end
