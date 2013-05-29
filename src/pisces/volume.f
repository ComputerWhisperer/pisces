cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION VOLUME(xi,yi,xj,yj,xk,yk)
      include 'p2conf.h'
c    
c     This function calculates the volume of integrated triangle
c     which is symmetrical with the axis x=0.
c     So the 3-d shape of this integrated triangle is the conbination
c     of 3 cones.
c
c     Original:  A.Yabuta  Stanford University   Jan. 88
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      double precision xi,yi,xj,yj,xk,yk,tmp
      double precision xi1,yi1,xj1,yj1,xk1,yk1
      double precision vol1,vol2,vol3
c
c...Push x,y values
      xi1=xi
      yi1=yi
      xj1=xj
      yj1=yj
      xk1=xk
      yk1=yk
c
c...Re-arrange points according to y value
      if(yj.gt.yi) then
        tmp=yi
        yi=yj
        yj=tmp
        tmp=xi
        xi=xj
        xj=tmp
      endif
      if(yk.gt.yj) then
        tmp=yj
        yj=yk
        yk=tmp
        tmp=xj
        xj=xk
        xk=tmp
        if(yj.gt.yi) then
          tmp=yi
          yi=yj
          yj=tmp
          tmp=xi
          xi=xj
          xj=tmp
        endif
      endif
c
c...Calculate volume
      vol1=1.0471976*(xi*xi+xi*xj+xj*xj)*abs(yi-yj)
      vol2=1.0471976*(xj*xj+xj*xk+xk*xk)*abs(yj-yk)
      vol3=1.0471976*(xk*xk+xk*xi+xi*xi)*abs(yk-yi)
c
c...Calculate the intersection of line y=yj and (xk-xi)(y-yi)=(yk-yi)(x-xi)
      tmp=xi+(xk-xi)*(yj-yi)/(yk-yi)
c
c...Total volume can be calculated in 2 cases
      if(tmp.gt.xj) then
        volume=vol3-vol1-vol2
      else
        volume=vol1+vol2-vol3
      endif
c
c...Pop x,y values
      xi=xi1
      yi=yi1
      xj=xj1
      yj=yj1
      xk=xk1
      yk=yk1
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CENTR(xi,yi,xj,yj,xk,yk,xcentr,ycentr)
      include 'p2conf.h'
c    
c     This subroutine calculates the center of circumscribed
c     circle of triangle(element).
c
c     Original:  A.Yabuta  Stanford University   Feb. 88
c
c     Copyright c 1988 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      double precision xi,yi,xj,yj,xk,yk,xij,yij,xjk,yjk,xki,yki
      double precision xcentr,ycentr
      double precision r2ij,r2jk,r2ki
c
c......Calculate parameters 
         xij=xi-xj
         yij=yi-yj
         xjk=xj-xk
         yjk=yj-yk
         xki=xk-xi
         yki=yk-yi
         R2IJ=XI*XI+YI*YI-XJ*XJ-YJ*YJ
         R2JK=XJ*XJ+YJ*YJ-XK*XK-YK*YK
         R2KI=XK*XK+YK*YK-XI*XI-YI*YI
C
C......(XCENTR,YCENTR) IS CENTER OF CIRCUMSCRIBED CIRCLE
C                                      FOR TRIANGLE
         XCENTR=0.5*(YI*R2JK+YJ*R2KI+YK*R2IJ)
         XCENTR=XCENTR/(YI*XJK+YJ*XKI+YK*XIJ)
         YCENTR=0.5*(XI*R2JK+XJ*R2KI+XK*R2IJ)
         YCENTR=YCENTR/(XI*YJK+XJ*YKI+XK*YIJ)
         return
         end
