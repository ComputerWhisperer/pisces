cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 15:10:20 PST 1990 (dredge--stanford)
c*comdeck difftmp
c
c Common for obtaining difference between two solutions
c
      common /tmpco/ jxi,dfx11,dfx10,dfx21,dfx20
c
      double precision jxi(MAXPT,MAXCNT)
      double precision fv1(MAXPT),fn1(MAXPT),fp1(MAXPT),qfn1(MAXPT),
     +                 qfp1(MAXPT),fv2(MAXPT),fn2(MAXPT),fp2(MAXPT),
     +                 qfn2(MAXPT),qfp2(MAXPT),jxi1(7),jxi2(7),
     +                 bias1(MAXCNT),amp1(MAXCNT),dfl1(MAXCNT),
     +                 dfl01(MAXCNT),bias2(MAXCNT),amp2(MAXCNT),
     +                 dfl2(MAXCNT),dfl02(MAXCNT),jxic(MAXPT),
     +                 jxin(MAXPT),jxip(MAXPT),jxid(MAXPT),
     +                 jxit(MAXPT),exi(MAXPT),uxi(MAXPT),wxi(MAXEQN),
     +                 vres1(MAXCNT),vres10(MAXCNT),vres2(MAXCNT),
     +                 vres20(MAXCNT),dfx11(MAXCON),dfx10(MAXCON),
     +                 dfx21(MAXCON),dfx20(MAXCON)
      real             tbias(MAXCNT),tamp(MAXCNT),x1i(MAXPT),y1i(MAXPT)
c
      equivalence      (fv1,jxi(1,1)),(fn1,jxi(1,2)),(fp1,jxi(1,3))
      equivalence      (qfn1,jxi(1,4)),(qfp1,jxi(1,5))
      equivalence      (fv2,jxi(1,6)),(fn2,jxi(1,7)),(fp2,jxi(1,8))
      equivalence      (qfn2,jxi(1,9)),(qfp2,jxi(1,10))
      equivalence      (bias1,fv1),(amp1,fn1),(dfl1,fp1),(dfl01,fp1(30))
      equivalence      (bias2,fv2),(amp2,fn2),(dfl2,fp2),(dfl02,fp2(30))
      equivalence      (vres1,qfn1),(vres10,qfn1(50))
      equivalence      (vres2,qfp1),(vres20,qfp1(50))
      equivalence      (tbias,qfp2),(tamp,qfp2(50)),(x1i,fv2),(y1i,fn2)
      equivalence      (jxic,jxi(1,1)),(jxin,jxi(1,2)),(jxip,jxi(1,3))
      equivalence      (jxid,jxi(1,4)),(jxit,jxi(1,5)),(exi,jxi(1,6))
      equivalence      (uxi,jxi(1,7)),(jxi1,jxi(1,8)),(jxi2,jxi(1,9))
      equivalence      (wxi,jxi(1,8))
c
