cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 12 16:31:29 PDT 1989 (anderson-stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BREAK
c
c     Calculate breakdown voltage by using the ionization
c     integral.
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      call breakn
      call breake
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BREAKN
      include 'p2conf.h'
c
c     Calculate breakdown voltage by using the ionization
c     integral based on node.
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'impact.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      integer ni(4),i,i1,i2,i3,i4,istop(2),indx(2,100)
      integer node1,node2
      logical nflg
      double precision efd,efdmx(2),tmp,tmp1
      double precision alphp(2,100),alphn(2,100)
      double precision ionp,ionn
      data ni/1,2,3,1/
c
c...Initialize
      nflg=.false.
      efdmx(1)=-1.0d20
      indx(1,1)=1
c
c...Get adjacent information
      call nxtel(p2t,p2tc)
c
c...Find out the point which has maximum electric field
      do 100 i=1,ne
        do 101 i1=1,3
        node1=nop(ni(i1),i)
        node2=nop(ni(i1+1),i)
        if((mattyp(itype(node1)).ne.1).or.(mattyp(itype(node2)).ne.1))
     +   goto 101
        tmp=cord(1,node2)-cord(1,node1)
        tmp1=cord(2,node2)-cord(2,node1)
        efd=dktq*(fv(node2)-fv(node1))
        efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
        efd=dabs(efd)
        if(efd.gt.efdmx(1)) then
          efdmx(1)=efd
          indx(1,1)=node1
          indx(2,1)=node2
        endif
101     continue
100   continue
c
c...Store first value to arrays
      node1=indx(1,1)
      node2=indx(2,1)
      tmp=cord(1,node2)-cord(1,node1)
      tmp1=cord(2,node2)-cord(2,node1)
      efd=dktq*(fv(node2)-fv(node1))
      efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
      efdmx(1)=dabs(efd)
      efdmx(2)=dabs(efd)
c
c...Get impact ionization rate alpha
      call alpha(efdmx(1),alphp(1,1),alphn(1,1))
      if(lpath) then
        write(luout,*) ' '
        write(luout,*) '****** Ionization integral path(node) ******'
        write(luout,*) 'index node x-address   y-address   e.field
     +     alpha(p)    alph(n)'
        write(luout,9999) 1,1,indx(1,1),cord(1,indx(1,1)),
     +  cord(2,indx(1,1)),efdmx(1),alphp(1,1),alphn(1,1)
9999    format(2i3,i5,5e12.4)
        write(luout,9999) 2,1,indx(2,1),cord(1,indx(2,1)),
     +  cord(2,indx(2,1)),efdmx(1),alphp(1,1),alphn(1,1)
      endif
c
c...Define which direction is positive or negative potential gradient
      if(efd.gt.0.) nflg=.true.
c
c...Search potential gradient path at the first point(1,1)
      efdmx(1)=-1.0d20
c      write(*,*) 'indx(1,1)=',indx(1,1)
c      write(*,*) 'p2tc=',p2tc(indx(1,1))
      do 102 i=1,p2tc(indx(1,1)) 
        do 103 i1=1,3
c       write(*,*) 'i=',i
c        write(*,*) 'indx(1,1)=',indx(1,1)
c       write(*,*) 'p2t=',p2t(i,indx(1,1))
        node1=nop(ni(i1),p2t(i,indx(1,1)))
        node2=nop(ni(i1+1),p2t(i,indx(1,1)))
c       write(*,*) 'node1=',node1,'   node2=',node2
        if((mattyp(itype(node1)).ne.1).or.(mattyp(itype(node2)).ne.1))
     +   goto 103
        if((node1.ne.indx(2,1)).and.(node2.ne.indx(2,1))) then
          if(node1.eq.indx(1,1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node2)-fv(node1))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if(nflg) efd=-1.*efd 
            if(efd.gt.efdmx(1)) then
              efdmx(1)=efd
              indx(1,2)=node2
            endif
c           write(*,*) 'electric field 1=',efd
          else if(node2.eq.indx(1,1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node1)-fv(node2))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if(nflg) efd=-1.*efd 
            if(efd.gt.efdmx(1)) then
              efdmx(1)=efd
              indx(1,2)=node1
            endif
c           write(*,*) 'electric field 2=',efd
          endif
        endif
103     continue
102   continue
c
c...Search potential gradient path at the first point(2,1)
      efdmx(2)=-1.0d20
      do 104 i=1,p2tc(indx(2,1)) 
        do 105 i1=1,3
        node1=nop(ni(i1),p2t(i,indx(2,1)))
        node2=nop(ni(i1+1),p2t(i,indx(2,1)))
        if((mattyp(itype(node1)).ne.1).or.(mattyp(itype(node2)).ne.1))
     +   goto 105
        if((node1.ne.indx(1,1)).and.(node2.ne.indx(1,1))) then
          if(node1.eq.indx(2,1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node2)-fv(node1))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if(.not.nflg) efd=-1.*efd 
            if(efd.gt.efdmx(2)) then
              efdmx(2)=efd
              indx(2,2)=node2
            endif
          else if(node2.eq.indx(2,1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node1)-fv(node2))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if(.not.nflg) efd=-1.*efd 
            if(efd.gt.efdmx(2)) then
              efdmx(2)=efd
              indx(2,2)=node1
            endif
          endif
        endif
105     continue
104   continue
c
c...Get impact ionization rate alpha
      efdmx(1)=dabs(efdmx(1))
      efdmx(2)=dabs(efdmx(2))
      call alpha(efdmx(1),alphp(1,2),alphn(1,2))
      call alpha(efdmx(2),alphp(2,2),alphn(2,2))
      if(lpath) then
        write(luout,9999) 1,2,indx(1,2),cord(1,indx(1,2)),
     +  cord(2,indx(1,2)),efdmx(1),alphp(1,2),alphn(1,2)
        write(luout,9999) 2,2,indx(2,2),cord(1,indx(2,2)),
     +  cord(2,indx(2,2)),efdmx(2),alphp(2,2),alphn(2,2)
      endif
c
c...Search potential gradient path
      do 106 i2=1,2
      do 107 i3=3,100
      efdmx(i2)=-1.0d20
      do 108 i=1,p2tc(indx(i2,i3-1)) 
        do 109 i1=1,3
        node1=nop(ni(i1),p2t(i,indx(i2,i3-1)))
        node2=nop(ni(i1+1),p2t(i,indx(i2,i3-1)))
        if((mattyp(itype(node1)).ne.1).or.(mattyp(itype(node2)).ne.1))
     +   goto 109
        if((node1.ne.indx(i2,i3-2)).and.(node2.ne.indx(i2,i3-2))) then
          if(node1.eq.indx(i2,i3-1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node2)-fv(node1))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if((i2.eq.1).and.(nflg)) efd=-1.*efd 
            if((i2.eq.2).and.(.not.nflg)) efd=-1.*efd 
            if(efd.gt.efdmx(i2)) then
              efdmx(i2)=efd
              indx(i2,i3)=node2
            endif
          else if(node2.eq.indx(i2,i3-1)) then
            tmp=cord(1,node2)-cord(1,node1)
            tmp1=cord(2,node2)-cord(2,node1)
            efd=dktq*(fv(node1)-fv(node2))
            efd=efd/(dsqrt(tmp*tmp+tmp1*tmp1))
            if((i2.eq.1).and.(nflg)) efd=-1.*efd 
            if((i2.eq.2).and.(.not.nflg)) efd=-1.*efd 
            if(efd.gt.efdmx(i2)) then
              efdmx(i2)=efd
              indx(i2,i3)=node1
            endif
          endif
        endif
109     continue
108   continue
c
c...Do we need to continue calculating according to electic field?
          if(efdmx(i2).lt.1.0e3) then
            istop(i2)=i3-1
            goto 106
          endif
          do 110 i4=1,i3-1
          if(indx(i2,i3).eq.indx(i2,i4)) then
            istop(i2)=i3-1
            goto 106
          endif
110       continue
c
c...Get impact ionization rate alpha 
      efdmx(i2)=dabs(efdmx(i2))
      call alpha(efdmx(i2),alphp(i2,i3),alphn(i2,i3))
      if(lpath) then
        write(luout,9999) i2,i3,indx(i2,i3),cord(1,indx(i2,i3)),
     +  cord(2,indx(i2,i3)),efdmx(i2),alphp(i2,i3),alphn(i2,i3)
      endif
107   continue
      istop(i2)=100
106   continue
c
c...Now calculate ionization integral
      call ioninn(indx,alphp,alphn,istop,ionp,ionn)
      write(luout,*) '-----RESULT(node)-----'
      write(luout,*) 'Ionization integral for holes    =',ionp
      write(luout,*) 'Ionization integral for electrons=',ionn
      write(luout,*) ' '
      return
      end
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BREAKE
      include 'p2conf.h'
c
c     Calculate breakdown voltage by using the ionization
c     integral based on element.
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'impact.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer i,i1,i2,i3,istop(2),indx(2,100),node1,node2,node3
      double precision efd,efdmx(2),efdcm(3),efdaj(3),efdtri,tmp
      double precision alphp(2,100),alphn(2,100)
      double precision xcntr(2,100),ycntr(2,100),xcn(3),ycn(3)
      double precision ionp,ionn,fvcn(2,100),fvtmp(3)
      double precision xi,yi,xj,yj,xk,yk,ksi1,ksi2,ksi3
c
c...Initialize
      efdmx(1)=-1.0d20
      indx(1,1)=1
c
c...Find out the point which has maximum electric field
      do 100 i=1,ne
      if(mattyp(imat(i)).ne.1) goto 100
      node1=nop(1,i)
      node2=nop(2,i)
      node3=nop(3,i)
      efd=dktq*efdtri(node1,node2,node3,.true.,tmp,tmp,tmp)
      if(efd.gt.efdmx(1)) then
        efdmx(1)=efd
        indx(1,1)=i
      endif
100   continue
c
c...Get center of circumscribed circle for triangle
      i=indx(1,1)
      node1=nop(1,i)
      node2=nop(2,i)
      node3=nop(3,i)
      xi=cord(1,nop(1,i))
      yi=cord(2,nop(1,i))
      xj=cord(1,nop(2,i))
      yj=cord(2,nop(2,i))
      xk=cord(1,nop(3,i))
      yk=cord(2,nop(3,i))
      call centr(xi,yi,xj,yj,xk,yk,xcntr(1,1),ycntr(1,1))
c
c...The start point is same!
      xcntr(2,1)=xcntr(1,1)
      ycntr(2,1)=ycntr(1,1)
      indx(2,1)=indx(1,1)
      efdmx(2)=efdmx(1)
c
c...Calculate potential on the center of circumscribed circle
      call barycm(xcntr(1,1),ycntr(1,1),xi,yi,xj,yj,xk,yk,
     +            ksi1,ksi2,ksi3)
      fvcn(1,1)=fv(node1)*ksi1+fv(node2)*ksi2+fv(node3)*ksi3
      fvcn(2,1)=fvcn(1,1)
c            write(*,*) '----Initial point ----'
c            write(*,*) 'potential',fvcn(1,1)
c            write(*,*) 'center',xcntr(1,1),ycntr(1,1)
c            write(*,*) 'potential on node',fv(node1),fv(node2),fv(node3)
c            write(*,*) 'weghiting',ksi1,ksi2,ksi3
c
c...Get impact ionization rate alpha 
      call alpha(efdmx(1),alphp(1,1),alphn(1,1))
      alphp(2,1)=alphp(1,1)
      alphn(2,1)=alphn(1,1)
      if(lpath) then
        write(luout,*) ' '
        write(luout,*) '****** Ionization integral path(element) ******'
        write(luout,*) 'index elem x-address   y-address   e.field
     +     alpha(p)    alph(n)'
        write(luout,9999) 1,1,indx(1,1),xcntr(1,1),ycntr(1,1),
     +  efdmx(1),alphp(1,1),alphn(1,1)
      endif
c
c...Search 2 adjacent elements which has larger potential gradient 
c                                        than the last element
      do 101 i=1,3
      node1=nop(1,nextel(i,indx(1,1)))
      node2=nop(2,nextel(i,indx(1,1)))
      node3=nop(3,nextel(i,indx(1,1)))
      if((nextel(i,indx(1,1)).le.0.)
     +.or.(mattyp(imat(nextel(i,indx(1,1)))).ne.1)) then
        efdcm(i)=0.0
        goto 101
      endif
      efdaj(i)=dktq*efdtri(node1,node2,node3,.true.,tmp,tmp,tmp)
      xi=cord(1,node1)
      yi=cord(2,node1)
      xj=cord(1,node2)
      yj=cord(2,node2)
      xk=cord(1,node3)
      yk=cord(2,node3)
      call centr(xi,yi,xj,yj,xk,yk,xcn(i),ycn(i))
      call barycm(xcn(i),ycn(i),xi,yi,xj,yj,xk,yk,ksi1,ksi2,ksi3)
      fvtmp(i)=fv(node1)*ksi1+fv(node2)*ksi2+fv(node3)*ksi3
      efdcm(i)=fvtmp(i)-fvcn(1,1)
      tmp=(xcn(i)-xcntr(1,1))*(xcn(i)-xcntr(1,1))
     +   +(ycn(i)-ycntr(1,1))*(ycn(i)-ycntr(1,1))
c            write(*,*) '********'
c            write(*,*) 'distance',tmp
      if(tmp.gt.0) then
        efdcm(i)=efdcm(i)/tmp
      else
        tmp=fv(node1)+fv(node2)+fv(node3)-2.*fvtmp(i)
        if(tmp.gt.fvtmp(i)) then
          efdcm(i)=1.0d20
        else
          efdcm(i)=-1.0d20
        endif
      endif
c            write(*,*) 'flag(initial)',i,nextel(i,indx(1,1))
c            write(*,*) 'center',xcn(i),ycn(i)
c            write(*,*) 'prev poten',fvcn(1,1)
c            write(*,*) 'potential',fvtmp(i)
c            write(*,*) 'potential on node',fv(node1),fv(node2),fv(node3)
c            write(*,*) 'weghiting',ksi1,ksi2,ksi3
c            write(*,*) 'electric field:',efdaj(i)
c            write(*,*) 'grad of poten  ',efdcm(i)
101   continue
c
c...Compare 3 gradient of potentials of ajacent elements
c   Be careful max(efdcm)-->1  min(efdcm)-->2
      if(efdcm(1).gt.efdcm(2)) then
        efdmx(1)=efdaj(1)
        efdmx(2)=efdaj(2)
        indx(1,2)=nextel(1,indx(1,1))
        indx(2,2)=nextel(2,indx(1,1))
        xcntr(1,2)=xcn(1)
        ycntr(1,2)=ycn(1)
        xcntr(2,2)=xcn(2)
        ycntr(2,2)=ycn(2)
        fvcn(1,2)=fvtmp(1)
        fvcn(2,2)=fvtmp(2)
      else
        efdmx(1)=efdaj(2)
        efdmx(2)=efdaj(1)
        indx(1,2)=nextel(2,indx(1,1))
        indx(2,2)=nextel(1,indx(1,1))
        xcntr(1,2)=xcn(2)
        ycntr(1,2)=ycn(2)
        xcntr(2,2)=xcn(1)
        ycntr(2,2)=ycn(1)
        fvcn(1,2)=fvtmp(2)
        fvcn(2,2)=fvtmp(1)
        tmp=efdcm(1)
        efdcm(1)=efdcm(2)
        efdcm(2)=tmp
      endif
      if(efdcm(3).gt.efdcm(2)) then
        if(efdcm(3).gt.efdcm(1)) then
          efdmx(1)=efdaj(3)
          indx(1,2)=nextel(3,indx(1,1))
          xcntr(1,2)=xcn(3)
          ycntr(1,2)=ycn(3)
          fvcn(1,2)=fvtmp(3)
          tmp=efdcm(2)
          efdcm(2)=efdcm(3)
          efdcm(3)=tmp
          tmp=efdcm(2)
          efdcm(2)=efdcm(1)
          efdcm(1)=tmp
        else
          tmp=efdcm(2)
          efdcm(2)=efdcm(3)
          efdcm(3)=tmp
        endif
      else
          efdmx(2)=efdaj(3)
          indx(2,2)=nextel(3,indx(1,1))
          xcntr(2,2)=xcn(3)
          ycntr(2,2)=ycn(3)
          fvcn(2,2)=fvtmp(3)
      endif
c
c...efdcm(1) should be more than 0 and efdcm(2) should be less than 0
      if((efdcm(1).lt.0.).or.(efdcm(3).gt.0.))
     +  write(6,*) 'Warning: potential gradient path is not correct'
c            write(*,*) '**********************************'
c            write(*,*) 'element no=',indx(1,2),indx(2,2)
c            write(*,*) 'electric field:',efdmx(1),efdmx(2)
c            write(*,*) 'grad of poten  ',efdcm(1),efdcm(3)
c
c...Get impact ionization rate alpha
      call alpha(efdmx(1),alphp(1,2),alphn(1,2))
      call alpha(efdmx(2),alphp(2,2),alphn(2,2))
c
c...Search the path of maximum electric field gradient for 2 directions
c...Get electric field of ajacent elements
      do 102 i=1,2
        if(lpath) then
        write(luout,9999) i,2,indx(i,2),xcntr(i,2),ycntr(i,2),
     +  efdmx(i),alphp(i,2),alphn(i,2)
        endif
       do 103 i1=3,100
        do 104 i2=1,3
          if((nextel(i2,indx(i,i1-1)).eq.indx(i,i1-2)).or.
     +    (nextel(i2,indx(i,i1-1)).le.0.).or.
     +    (mattyp(imat(nextel(i2,indx(i,i1-1)))).ne.1)) then
            efdcm(i2)=-1.0d20
            goto 104
          endif
          node1=nop(1,nextel(i2,indx(i,i1-1)))
          node2=nop(2,nextel(i2,indx(i,i1-1)))
          node3=nop(3,nextel(i2,indx(i,i1-1)))
          efdaj(i2)=dktq*efdtri(node1,node2,node3,.true.,tmp,tmp,tmp)
          xi=cord(1,node1)
          yi=cord(2,node1)
          xj=cord(1,node2)
          yj=cord(2,node2)
          xk=cord(1,node3)
          yk=cord(2,node3)
          call centr(xi,yi,xj,yj,xk,yk,xcn(i2),ycn(i2))
          call barycm(xcn(i2),ycn(i2),xi,yi,xj,yj,xk,yk,ksi1,ksi2,ksi3)
          fvtmp(i2)=fv(node1)*ksi1+fv(node2)*ksi2+fv(node3)*ksi3
          efdcm(i2)=fvtmp(i2)-fvcn(i,i1-1)
          tmp=(xcn(i2)-xcntr(i,i1-1))*(xcn(i2)-xcntr(i,i1-1))
     +       +(ycn(i2)-ycntr(i,i1-1))*(ycn(i2)-ycntr(i,i1-1))
          if(tmp.gt.0.) then
            efdcm(i2)=efdcm(i2)/tmp
          else
            tmp=fv(node1)+fv(node2)+fv(node3)-2.*fvtmp(i2)
            if(tmp.gt.fvtmp(i2)) then
              efdcm(i2)=1.0d20
            else
              efdcm(i2)=-1.0d20
            endif
          endif
c
c...If i=2 then efdcm should be reversed in order to evaluate later
          if(i.ge.2) efdcm(i2)=-1.*efdcm(i2)
c            write(*,*) '**********************************'
c            write(*,*) 'flag=',i,i1,i2
c            write(*,*) 'element no=',nextel(i2,indx(i,i1-1))
c            write(*,*) 'weghiting factor  ',ksi1,ksi2,ksi3
c            write(*,*) 'center         =',xcn(i2),ycn(i2)
c            write(*,*) 'previous center=',xcntr(i,i1-1),ycntr(i,i1-1)
c            write(*,*) 'distance       ',tmp
c            write(*,*) 'electric field:',efdaj(i2)
c            write(*,*) 'previous poten ',fvcn(i,i1-1)
c            write(*,*) 'potential:     ',fvtmp(i2)
c            write(*,*) 'grad of poten  ',efdcm(i2)
104       continue
c
c...Compare 3 electric field of ajacent elements
          if(efdcm(2).lt.efdcm(1)) then
            if(efdcm(3).lt.efdcm(1)) then
              efdmx(i)=efdaj(1)
              indx(i,i1)=nextel(1,indx(i,i1-1))
              xcntr(i,i1)=xcn(1)
              ycntr(i,i1)=ycn(1)
              fvcn(i,i1)=fvtmp(1)
            else
              efdmx(i)=efdaj(3)
              indx(i,i1)=nextel(3,indx(i,i1-1))
              xcntr(i,i1)=xcn(3)
              ycntr(i,i1)=ycn(3)
              fvcn(i,i1)=fvtmp(3)
              efdcm(1)=efdcm(3)
            endif
          else
            if(efdcm(3).lt.efdcm(2)) then
              efdmx(i)=efdaj(2)
              indx(i,i1)=nextel(2,indx(i,i1-1))
              xcntr(i,i1)=xcn(2)
              ycntr(i,i1)=ycn(2)
              fvcn(i,i1)=fvtmp(2)
              efdcm(1)=efdcm(2)
            else
              efdmx(i)=efdaj(3)
              indx(i,i1)=nextel(3,indx(i,i1-1))
              xcntr(i,i1)=xcn(3)
              ycntr(i,i1)=ycn(3)
              fvcn(i,i1)=fvtmp(3)
              efdcm(1)=efdcm(3)
            endif
          endif
c
c...Do we need to continue calculating according to electic field?
          if((efdmx(i).lt.1.0e3).or.(efdcm(1).lt.0.)) then
            istop(i)=i1-1
            goto 102
          endif
          do 105 i3=1,i1-1
          if(indx(i,i1).eq.indx(i,i3)) then
            istop(i)=i1-1
            goto 102
          endif
105       continue
c
c...Get impact ionization rate alpha
          call alpha(efdmx(i),alphp(i,i1),alphn(i,i1))
      if(lpath) then
        write(luout,9999) i,i1,indx(i,i1),xcntr(i,i1),ycntr(i,i1),
     +  efdmx(i),alphp(i,i1),alphn(i,i1)
      endif
9999    format(2i3,i5,5e12.4)
103     continue 
        istop(i)=100
102   continue 
c        write(*,9998) istop(1),istop(2)
c9998    format('  stop counter1=',i3,'     stop counter2=',i3)
c
c...Now calculate ionization integral
      call ionine(indx,xcntr,ycntr,alphp,alphn,istop,ionp,ionn)
      write(luout,*) '-----RESULT(element)-----'
      write(luout,*) 'Ionization integral for holes    =',ionp
      write(luout,*) 'Ionization integral for electrons=',ionn
      write(luout,*) ' '
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ALPHA(efdalp,alpp,alpn)
      include 'p2conf.h'
c
c     Calculates the ionization rates(alpha). 
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'impact.h'
c------------------------------------------------------------------
c
      integer i
      double precision efdalp,alpp,alpn
      double precision gi(3),jside(3),heigh(3),ei(3),area(3)
      double precision 
     +     gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3),dev(3,3)
c
c...Initialize
c   Be careful area should be 0.5 because ionization rate will be
c   returned as alpha(1)*area(1)+alpha(2)*area(2)
      do 100 i=1,3
        ei(i)=efdalp
        heigh(i)=1.0d0
        area(i)=0.5d0
100   continue
c
c...Get impact ionization rate alpha for hole
      if(lcsm) then
        call impac1(.true.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      else if(lmont) then
        call impac2(.true.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      else
        call impace(.true.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      endif
      alpp=gi(1)
c
c...Get impact ionization rate alpha for electron
      do 101 i=1,3
        area(i)=0.5d0
101   continue
      if(lcsm) then
        call impac1(.false.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      else if(lmont) then
        call impac2(.false.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      else
        call impace(.false.,ei,jside,heigh,area,gi,gsc,gsp,
     +              dgic,dgip,dev,.true.)
      endif
      alpn=gi(1)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IONINN(indx,alphp,alphn,istop,intp2,intn2)
      include 'p2conf.h'
c
c     Calculates the ionization integral based on nodes.
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical pflg
      integer i,i1,istop(2),indx(2,100)
      double precision tmp,intp1,intp2,intn1,intn2
      double precision alphp(2,100),alphn(2,100)
c
c...Initialize
      intp1=0.
      intp2=0.
      intn1=0.
      intn2=0.
c
c
c...Judge p or n type
c         p:r1<0  n:r1>0
      i=indx(1,istop(1))
      i1=indx(2,istop(2))
      if(r1(i).le.0.) then
        if(r1(i1).ge.0.) then
          pflg=.true.
        else
          write(6,*)
     +    'Warning:Ionization integral path is not valid(p->p)!'
          return
        endif
      else if(r1(i1).ge.0) then
        write(6,*)
     +    'Warning:Ionization integral path is not valid(n->n)!'
        return
      endif
c
c...Calculate integral for holes
c...Be sure ionization integral for hole should start at n-type
      i=1
      if(pflg) i=2
c
c...Iteration for integral ---1---
      if(istop(i).lt.2) goto 200
      do 100 i1=istop(i),2,-1
      tmp=(cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))*
     +    (cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))
      tmp=tmp+(cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))*
     +    (cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))
      tmp=dsqrt(tmp)
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
100   continue
200   continue
c
c...At the point which has maximum electric field
      tmp=(cord(1,indx(1,1))-cord(1,indx(2,1)))*
     +    (cord(1,indx(1,1))-cord(1,indx(2,1)))
      tmp=tmp+(cord(2,indx(1,1))-cord(2,indx(2,1)))*
     +    (cord(2,indx(1,1))-cord(2,indx(2,1)))
      tmp=dsqrt(tmp)
      intp1=intp1+(alphp(1,1)-alphn(1,1))*tmp
      intp2=intp2+alphp(1,1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
c
c...Now switch to the other side
      i=2
      if(pflg) i=1
c
c...Iteration for integral ---2---
      if(istop(i).lt.2) goto 201
      do 101 i1=2,istop(i)
      tmp=(cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))*
     +    (cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))
      tmp=tmp+(cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))*
     +    (cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))
      tmp=dsqrt(tmp)
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
101   continue
201   continue
c
c...Calculate integral for electrons 
c...Be sure ionization integral for electron should start at p-type
      i=2
      if(pflg) i=1
c
c...Iteration for integral ---1---
      if(istop(i).lt.2) goto 202
      do 102 i1=istop(i),2,-1
      tmp=(cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))*
     +    (cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))
      tmp=tmp+(cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))*
     +    (cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))
      tmp=dsqrt(tmp)
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
102   continue
202   continue
c
c...At the point which has maximum electric field
      tmp=(cord(1,indx(1,1))-cord(1,indx(2,1)))*
     +    (cord(1,indx(1,1))-cord(1,indx(2,1)))
      tmp=tmp+(cord(2,indx(1,1))-cord(2,indx(2,1)))*
     +    (cord(2,indx(1,1))-cord(2,indx(2,1)))
      tmp=dsqrt(tmp)
      intn1=intn1+(alphn(1,1)-alphp(1,1))*tmp
      intn2=intn2+alphn(1,1)*tmp*dexp(-1.0*intn1)
c
c...Now switch to the other side
      i=1
      if(pflg) i=2
c
c...Iteration for integral ---2---
      if(istop(i).lt.2) goto 203
      do 103 i1=2,istop(i)
      tmp=(cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))*
     +    (cord(1,indx(i,i1))-cord(1,indx(i,i1-1)))
      tmp=tmp+(cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))*
     +    (cord(2,indx(i,i1))-cord(2,indx(i,i1-1)))
      tmp=dsqrt(tmp)
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
103   continue
203   continue
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IONINE(indx,xcntr,ycntr,alphp,alphn,istop,intp2,intn2)
      include 'p2conf.h'
c
c     Calculates the ionization integral based on elements.
c
c     Original : A.Yabuta Stanford University   Feb, 1988
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical pflg
      integer i,i1,istop(2),indx(2,100)
      double precision tmp,tmp1,intp1,intp2,intn1,intn2
      double precision alphp(2,100),alphn(2,100)
      double precision xcntr(2,100),ycntr(2,100)
c
c...Initialize
      intp1=0.
      intp2=0.
      intn1=0.
      intn2=0.
c
c
c...Judge p or n type
c         p:r1<0  n:r1>0
      i=indx(1,istop(1))
      i1=indx(2,istop(2))
      if(r1(nop(1,i)).le.0.) then
        if(r1(nop(1,i1)).ge.0.) then
          pflg=.true.
        else
          write(6,*)
     +    'Warning:Ionization integral path is not valid(p->p)!'
          return
        endif
      else if(r1(nop(1,i1)).ge.0) then
          write(6,*)
     +    'Warning:Ionization integral path is not valid(n->n)!'
        return
      endif
c
c...Calculate integral for holes
c...Be sure ionization integral for hole should start at n-type
      i=1
      if(pflg) i=2
c
c...At the start point, distance is half between next and this point
      i1=istop(i)
      tmp=((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))*
     +     ((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))
      tmp=tmp+((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))*
     +     ((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))
      tmp=dsqrt(tmp)
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
c
c...Iteration for integral ---1---
      if(istop(i).le.2) goto 200
      do 100 i1=istop(i)-1,2,-1
      tmp=(xcntr(i,i1)-xcntr(i,i1+1))*(xcntr(i,i1)-xcntr(i,i1+1))+
     +    (ycntr(i,i1)-ycntr(i,i1+1))*(ycntr(i,i1)-ycntr(i,i1+1))
      tmp1=(xcntr(i,i1)-xcntr(i,i1-1))*(xcntr(i,i1)-xcntr(i,i1-1))+
     +    (ycntr(i,i1)-ycntr(i,i1-1))*(ycntr(i,i1)-ycntr(i,i1-1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
100   continue
200   continue
c
c...At the point which has maximum electric field
      tmp=(xcntr(1,2)-xcntr(1,1))*(xcntr(1,2)-xcntr(1,1))+
     +    (ycntr(1,2)-ycntr(1,1))*(ycntr(1,2)-ycntr(1,1))
      tmp1=(xcntr(2,2)-xcntr(1,1))*(xcntr(2,2)-xcntr(1,1))+
     +    (ycntr(2,2)-ycntr(1,1))*(ycntr(2,2)-ycntr(1,1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intp1=intp1+(alphp(1,1)-alphn(1,1))*tmp
      intp2=intp2+alphp(1,1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
c
c...Now switch to the other side
      i=2
      if(pflg) i=1
c
c...Iteration for integral ---2---
      if(istop(i).le.2) goto 201
      do 101 i1=2,istop(i)-1
      tmp=(xcntr(i,i1)-xcntr(i,i1+1))*(xcntr(i,i1)-xcntr(i,i1+1))+
     +    (ycntr(i,i1)-ycntr(i,i1+1))*(ycntr(i,i1)-ycntr(i,i1+1))
      tmp1=(xcntr(i,i1)-xcntr(i,i1-1))*(xcntr(i,i1)-xcntr(i,i1-1))+
     +    (ycntr(i,i1)-ycntr(i,i1-1))*(ycntr(i,i1)-ycntr(i,i1-1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
101   continue
201   continue
c
c...At the end point, distance is half between next and this point
      i1=istop(i)
      tmp=((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))*
     +     ((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))
      tmp=tmp+((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))*
     +     ((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))
      tmp=dsqrt(tmp)
      intp1=intp1+(alphp(i,i1)-alphn(i,i1))*tmp
      intp2=intp2+alphp(i,i1)*tmp*dexp(-1.0*intp1)
c      write(*,*) 'flag=',i1,'dist=',tmp
c      write(*,*) 'Integral1=',intp1,'Integral2=',intp2
c
c...Calculate integral for electrons 
c...Be sure ionization integral for electron should start at p-type
      i=2
      if(pflg) i=1
c
c...At the start point, distance is half between next and this point
      i1=istop(i)
      tmp=((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))*
     +     ((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))
      tmp=tmp+((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))*
     +     ((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))
      tmp=dsqrt(tmp)
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
c
c...Iteration for integral ---1---
      if(istop(i).le.2) goto 202
      do 102 i1=istop(i)-1,2,-1
      tmp=(xcntr(i,i1)-xcntr(i,i1+1))*(xcntr(i,i1)-xcntr(i,i1+1))+
     +    (ycntr(i,i1)-ycntr(i,i1+1))*(ycntr(i,i1)-ycntr(i,i1+1))
      tmp1=(xcntr(i,i1)-xcntr(i,i1-1))*(xcntr(i,i1)-xcntr(i,i1-1))+
     +    (ycntr(i,i1)-ycntr(i,i1-1))*(ycntr(i,i1)-ycntr(i,i1-1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
102   continue
202   continue
c
c...At the point which has maximum electric field
      tmp=(xcntr(1,2)-xcntr(1,1))*(xcntr(1,2)-xcntr(1,1))+
     +    (ycntr(1,2)-ycntr(1,1))*(ycntr(1,2)-ycntr(1,1))
      tmp1=(xcntr(2,2)-xcntr(1,1))*(xcntr(2,2)-xcntr(1,1))+
     +    (ycntr(2,2)-ycntr(1,1))*(ycntr(2,2)-ycntr(1,1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intn1=intn1+(alphn(1,1)-alphp(1,1))*tmp
      intn2=intn2+alphn(1,1)*tmp*dexp(-1.0*intn1)
c
c...Now switch to the other side
      i=1
      if(pflg) i=2
c
c...Iteration for integral ---2---
      if(istop(i).le.2) goto 203
      do 103 i1=2,istop(i)-1
      tmp=(xcntr(i,i1)-xcntr(i,i1+1))*(xcntr(i,i1)-xcntr(i,i1+1))+
     +    (ycntr(i,i1)-ycntr(i,i1+1))*(ycntr(i,i1)-ycntr(i,i1+1))
      tmp1=(xcntr(i,i1)-xcntr(i,i1-1))*(xcntr(i,i1)-xcntr(i,i1-1))+
     +    (ycntr(i,i1)-ycntr(i,i1-1))*(ycntr(i,i1)-ycntr(i,i1-1))
      tmp=0.5*(dsqrt(tmp)+dsqrt(tmp1))
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
103   continue
203   continue
c
c...At the end point, distance is half between next and this point
      i1=istop(i)
      tmp=((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))*
     +     ((xcntr(i,i1)+xcntr(i,i1-1))*0.5-xcntr(i,i1))
      tmp=tmp+((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))*
     +     ((ycntr(i,i1)+ycntr(i,i1-1))*0.5-ycntr(i,i1))
      tmp=dsqrt(tmp)
      intn1=intn1+(alphn(i,i1)-alphp(i,i1))*tmp
      intn2=intn2+alphn(i,i1)*tmp*dexp(-1.0*intn1)
      return
      end
