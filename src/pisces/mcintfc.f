cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 12 21:17:26 PDT 1989 (dredge--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MCINT(mntfil)
      include 'p2conf.h'
      character*(*) mntfil
c
c     char mntfil is the Monte Carlo output file to read.
c 
c     Calculate alpha value for impact ionization on each PISCES node
c     by using Monte Carlo simulater's output file(which contains
c     uniform rectangular grid)
c
c     Original : A.Yabuta        Stanford University        Sep, 1987
c 
c     Copyright c 1987 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    common area
c
c------------------------------------------------------------------
      include     'blank.h'
      include     'logunt.h'
      include     'monte.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c
c                     Local types
c
      integer nmc,ie,iside,nmin1,nmin2,nmin3,imc,ntmp
      integer ni(5),ni1,ni2
      integer ierr
      double precision xmc(1500),ymc(1500),xpc(1500),ypc(1500)
      double precision min1,min2,min3,tmp,ksi1,ksi2,ksi3
      double precision alphin(1500)
c
c
c                     Data
c
      data ni/1,2,3,1,2/
c
c******************Start*********************
c
      nmc=0
c---Read output file of MC
      call fopcl(2,mntfil,LEN(mntfil),lutmp,.false., ierr)
      if (ierr.ne.0) return
c..<<we probably need to do more than this if there is an error. mje>>
c
103   continue
      nmc=nmc+1
      read(lutmp,*,end=104) xmc(nmc),ymc(nmc),alphin(nmc)
c       write(*,*) xmc(nmc),ymc(nmc),alphin(nmc)
      goto 103
104   continue
      nmc=nmc-1
      call fopcl(0, ' ',0, lutmp, .true.,0)
      
c---Sort all triangular elements
      do 100 ie=1,ne
c
c---Sort all node in one element
c   Be sure (xpc,ypc) is the half point of 2 nodes
        do 101 iside=1,3
        ni1=nop(ni(iside+1),ie)
        ni2=nop(ni(iside+2),ie)
        xpc(iside)=0.5*(cord(1,ni1)+cord(1,ni2))
        ypc(iside)=0.5*(cord(2,ni1)+cord(2,ni2))
c          write(*,*) ni1,ni2,xpc(iside),ypc(iside)
c
c---Initial set 
          min1=1.0e10
          min2=1.0e10
          min3=1.0e10
          nmin1=-999
          nmin2=-999
          nmin3=-999
c
c---Find out adjacent 3 monte carlo nodes
          do 102 imc=1,nmc
          tmp=(xpc(iside)-xmc(imc))*(xpc(iside)-xmc(imc))
          tmp=tmp+(ypc(iside)-ymc(imc))*(ypc(iside)-ymc(imc))
c          write(*,*) nmc,imc,tmp
          if (tmp.lt.min3) then
            min3=tmp
            nmin3=imc
c              write(*,2003)
c2003           format(/,'*********')
c               write(*,*) nmin1,nmin2,nmin3
c               write(*,*) min1,min2,min3
            if (min3.lt.min1) then
              tmp=min3
              min3=min1
              min1=tmp
              ntmp=nmin3
              nmin3=nmin1
              nmin1=ntmp
c              write(*,2001)
c2001           format('min1 less than min3')
c               write(*,*) nmin1,nmin2,nmin3
c               write(*,*) min1,min2,min3
            endif
            if (min3.lt.min2) then
              tmp=min3
              min3=min2
              min2=tmp
              ntmp=nmin3
              nmin3=nmin2
              nmin2=ntmp
c              write(*,2002)
c2002           format('min2 less than min3')
c               write(*,*) nmin1,nmin2,nmin3
c               write(*,*) min1,min2,min3
            endif
          endif
102       continue
c
c---Get weighting factor(linear interpolation)
        call barycm(xpc(iside),ypc(iside),xmc(nmin1),ymc(nmin1),
     +              xmc(nmin2),ymc(nmin2),xmc(nmin3),ymc(nmin3),
     +              ksi1,ksi2,ksi3)
c
c---Calculate alpha value
c   If MC grid doesn'd contain PISCES node, alpha=0.0
        if ((ksi1.lt.0.0).or.(ksi2.lt.0.0).or.(ksi3.lt.0.0)) then
          alphmc(iside,ie)=0.0
        else
          alphmc(iside,ie)=ksi1*alphin(nmin1)+ksi2*alphin(nmin2)+
     $                     ksi3*alphin(nmin3)
        endif
               if(alphmc(iside,ie).ne.0.0) then
c               write(*,2004) 
c2004           format(/'****************************************')
c               write(*,*) xpc(iside),ypc(iside)
c               write(*,*) nmin1,nmin2,nmin3
c               write(*,*) min1,min2,min3
c               write(*,*) xmc(nmin1),ymc(nmin1)
c               write(*,*) xmc(nmin2),ymc(nmin2)
c               write(*,*) xmc(nmin3),ymc(nmin3)
c               write(*,*) ksi1,ksi2,ksi3
c               write(*,*) alphin(nmin1),alphin(nmin2),alphin(nmin3)
c               write(*,*) ie,iside
c               write(*,*) xpc(iside),ypc(iside),alphmc(iside,ie)
               endif
101     continue
100   continue
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BARYCM ( x,y, x1,y1,x2,y2,x3,y3, ksi1, ksi2, ksi3 )
      include 'p2conf.h'
c 
c     Calculate the barycentric coordinates of (x,y) in triangle trin
c
c     Original : CSR           Stanford University        Oct, 1983
c     Modified : A.Yabuta      Stanford University        Sep, 1987
c                Just changed variables to hand
c 
c     Copyright c 1987 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c     
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    Local types
c
      double precision x,y, ksi1,ksi2,ksi3, x1,x2,x3, y1,y2,y3, denom
c
c********************** Start ***********************************
c
c                    Calculate barycords
c
      denom = (x2-x1)*(y3-y1) - (x3-x1)*(y2-y1)
      ksi2  = (x -x1)*(y3-y1) - (x3-x1)*(y -y1)
      ksi3  = (x2-x1)*(y -y1) - (x -x1)*(y2-y1)
      if(denom.ne.0.0d0) then
      ksi2  = ksi2/denom
      ksi3  = ksi3/denom
      ksi1  = 1.0 - ksi2 - ksi3
      else
      ksi1=-999.0
      endif
      return
      end
