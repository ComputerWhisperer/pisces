cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ZLINE(x,y,ipen,isub)
      include 'p2conf.h'
c
c     Draw from the current location to (x,y).  We can also define an
c        area in scaled units.
c     Calling sequence:
c         call zline(x, y, ipendn, 0)   - to draw to current point.
c
c         call zline(x, y, iparea, 0)   - to start an area define.
c         call zline(x, y, iparea, 1)   - to define subsequent points.
c
c     Original : M.Law Sep 83 (IGGI)
c     Modified : CSR Aug 84 (Pascal -> Fortran)
c     Rewritten: CSR Sep 84 (New clipping algorithm)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common
      include  'plot.h'

c......local types
      real x,y,rx,ry,trx,try,tlrx,tlry
      integer ipen,isub
      logical inside,lclip

c******************** Start ********************************************
c
c.....{* convert the input points to standard coordinates *}
      rx = (x-xpoff)*xpdel
      ry = (y-ypoff)*ypdel

c.....{* check to see if this point is in the window *}
      inside = (rx.ge.0.).and.(rx.le.xpwid).and.
     +         (ry.ge.0.).and.(ry.le.ypwid)

c.....{* if both this and last inside, draw *}
      if (inside .and. linsid) then
         call fplot2(ipen,isub,rx,ry)

c.....{* otherwise call clipper - color polygons have already *}
c.....{* been clipped, so this is only for line clipping.     *}
      else
       tlrx=lrx
       tlry=lry
       trx=rx
       try=ry
       if (lclip (tlrx,tlry, trx,try, 0., xpwid, 0., ypwid)) then
             call fmove (tlrx,tlry)
             call fplot2(ipen,isub,trx,try)
       endif
      endif

c.....{* update the previous point data *}
      lrx = rx
      lry = ry
      linsid = inside

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ZLINE1(x,y,ybot0)
      include 'p2conf.h'
c
c     Same as zline except limits y to ybot0 (bottom of y-axis).
c
c     Original : MRP Aug 84
c     Modified : CSR Sep 84 to use new clipper
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common
      include 'plot.h'

c......local types
      real x,y,rx,ry,ybot0,rybot0,trx,try,tlrx,tlry
      logical inside,lclip

c******************** Start ********************************************
c
c.....{* convert the input points to standard coordinates *}
      rx = (x-xpoff)*xpdel
      ry = (y-ypoff)*ypdel
      rybot0 = (ybot0-ypoff)*ypdel

c.....{* check to see if this point is in the window *}
      inside = (rx.ge.0.).and.(rx.le.xpwid).and.
     +         (ry.ge.rybot0).and.(ry.le.ypwid)

c.....{* if both this and previous in window, draw *}
      if (inside .and. linsid) then
       call fdraw (rx,ry)

c.....{* call clipper *}
      else
       tlrx=lrx
       tlry=lry
       trx=rx
       try=ry
       if (lclip (tlrx,tlry, trx,try, 0., xpwid, rybot0, ypwid)) then
          call fmove (tlrx,tlry)
          call fdraw (trx,try)
       endif
      endif
   
c.....{* update the previous point data *}
      lrx = rx
      lry = ry
      linsid = inside

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ZMOVE(x,y)
      include 'p2conf.h'
c
c     Move to (x,y)
c     Original : M.Law Sep 83 (IGGI)
c     Modified : CSR Aug 84 (C-> Pascal -> Fortran)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common
      include 'plot.h'

c......local types
      real x,y,rx,ry
      logical inside,here

c******************** Start ********************************************
c
c.....{* convert the input points to standard coordinates *}
      rx = (x-xpoff)*xpdel
      ry = (y-ypoff)*ypdel

c.....{* check to see if this point is in the window         *}
c.....{* also check we're not moving to where we already are *}
      inside = (rx.ge.0).and.(rx.le.xpwid).and.
     +         (ry.ge.0.).and.(ry.le.ypwid)

c      here = (abs(rx-lrx).lt.rndoff*xpwid) .and. 
c     +       (abs(ry-lry).lt.rndoff*ypwid)
c     
      here = .false.
c.....{* plot, unless outside or no change. *}
      if (inside .and. .not.here) call fmove(rx,ry)

c.....{* leave marks for  posterity *}
      lrx = rx
      lry = ry
      linsid = inside

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION LCLIP(ax,ay,bx,by,xmin,xmax,ymin,ymax)
c
c...Clip the line from (ax,ay) -> (bx,by)
c   Returns true : line does not intersect screen
c   Returns false:      does.
c
c   Algorithm from anonymous graphics textboox.
c   Original : CSR Sep 84
c   Copyright c 1984 The board of trustees of the Leland Stanford 
c                    Junior University. All rights reserved.
c   This subroutine may not be used outside of the PISCES computer
c   program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real ax,ay,bx,by,xmin,xmax,ymin,ymax,tx,ty
      logical aleft,aright,atop,abot,bleft,bright,btop,bbot,
     +        cleft,cright,ctop,cbot,lafix
c
c******************** Start ********************************************
c
      aleft  = ax.lt.xmin
      aright = ax.gt.xmax
      abot   = ay.lt.ymin
      atop   = ay.gt.ymax

      bleft  = bx.lt.xmin
      bright = bx.gt.xmax
      bbot   = by.lt.ymin
      btop   = by.gt.ymax

c......While not both in window do:
  100 if ( .not.aleft .and. .not.aright .and. .not.atop .and. .not.abot 
     +.and..not.bleft .and. .not.bright .and. .not.btop .and. .not.bbot)
     +goto 1000
      
         if ( (aleft .and. bleft) .or.
     +        (aright.and.bright) .or.
     +        (atop  .and.  btop) .or.
     +        (abot  .and.  bbot) ) then
           lclip=.false.
           return
         endif

         if (aleft.or.aright.or.atop.or.abot) then
          cleft = aleft
          cright = aright
          ctop = atop
          cbot = abot
          lafix = .true.
         else
          cleft = bleft
          cright = bright
          ctop = btop
          cbot = bbot
          lafix = .false.
         endif

         if (cleft) then
            ty = ay + (by-ay)*(xmin-ax)/(bx-ax)
            tx = xmin
         else if (cright) then
            ty = ay + (by-ay)*(xmax-ax)/(bx-ax)
            tx = xmax
         else if (cbot) then
          tx = ax + (bx-ax)*(ymin-ay)/(by-ay)
          ty = ymin
       else if (ctop) then
          tx = ax + (bx-ax)*(ymax-ay)/(by-ay)
          ty = ymax
       endif

       if (lafix) then
          ax = tx
          ay = ty
          aleft  = ax.lt.xmin
            aright = ax.gt.xmax
            abot   = ay.lt.ymin
            atop   = ay.gt.ymax
       else
          bx = tx
          by = ty
          bleft  = bx.lt.xmin
            bright = bx.gt.xmax
            bbot   = by.lt.ymin
            btop   = by.gt.ymax
       endif

       goto 100
 1000 continue

      lclip = .true.
      return
      end
