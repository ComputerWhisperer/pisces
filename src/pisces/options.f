cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Oct  5 10:40:40 PDT 1989 (pisces--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE OPTCK
      include 'p2conf.h'
c
c     Get options for a run.
c
c     Original : CSR Aug 84
c     Modified: Michael Eldredge -- stanford (oct 89)
c         Changed all the gplot subroutine names to the new,
c         non-C conflicting names.  (Boy, will this cause havock!)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c.........common area
      include     'plot.h'
      include     'blank.h'
      include     'sol.h'
      include     'logunt.h'
c------------------------------------------------------------------
c
c.........local types
      integer ierr
      character*20 cpufil
      character*20 pltdev
      character*60 pltfil
      logical newdev
c
c FUNCTIONS:
      logical islval, iscval, isrval
      logical gtlval
      real    gtrval
      integer stpdev, stpfil, stsfil

c******************** Start ********************************************
c
c.......Debug, cpu flag 
      if (islval(2)) ldebug=gtlval(2)
      if (islval(3)) ldbug2=gtlval(3)
      if (islval(1)) lcpu=gtlval(1)
      if (lcpu) then
       if (iscval(2)) call gtcval(2, cpufil, LEN(cpufil))
       call fopcl(12,cpufil,20,lucpu,.false.,ierr)
       write(lucpu,1000) 
1000   format('PISCES-II CPU PROFILE')
      endif

c.......PLOT OPTIONS
      newdev = .false.
      ierr   = 0
c........NOTE: (mje apr 88) should check that we didn't get
c.........the lvals AND the cval, but for now....
      if (iscval(1)) then
          call gtcval(1, pltdev, LEN(pltdev))
          ierr = stpdev(pltdev)
          newdev = .true.
      else if (islval(4) .or. islval(5) .or. islval(6)) then
          if (gtlval(4)) then 
              ierr = stpdev ('tek4107')
          else if (gtlval(5)) then
              ierr = stpdev ('hp2648')
          else if (gtlval(6)) then
              ierr = stpdev ('hp2623')
          endif
          newdev = .true.
      endif
c
c...on error the gplot code will set to a "null" device, so
c....we just warn and continue on.
      if (ierr.lt.0) call erset(-157, linum, ierr)

c.....(test-mje) asave file.
      if (iscval(4)) then
          call gtcval(4, pltfil, LEN(pltfil))
          ierr = stsfil(pltfil)
      endif
c
c........plot file?
      if (iscval(3)) then
          call gtcval(3, pltfil, LEN(pltfil))
          ierr = stpfil(pltfil)
c...special case for compatibility with the old days.
c....if an output file given, but no plot device, assume they meant
c....create a binary save file.
          if (.not.newdev) ierr = stpdev('gpsave')
      endif
c
c...if new plot device then make sure that new values are done
      if (newdev) then
          initpl=.false.
          uxpwid = 0.0
          uypwid = 0.0
          uxoff  = 0.0
          uyoff  = 0.0
      endif
c
c........plot size from the user?
      if (isrval(1)) uxpwid = gtrval(1)
      if (isrval(2)) uypwid = gtrval(2)
c
c........Offsets from the user?
      if (isrval(3)) uxoff = gtrval(3)
      if (isrval(4)) uyoff = gtrval(4)
c

c........That's all for now.
      return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE SIZPLT(lclear)
      include 'p2conf.h'
      logical lclear
c
c "sizplt": Define the size of the plottting surface.  Also, if
c           lclear is true then clear and prepare for a new plot.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c.........common area
      include     'plot.h'
c.........local types
      real    xoff,yoff
      integer it
      real    titlsz
      real    avlen
      integer hlen, vlen
c....fpgeti() wants the next two; dim 10 is always engough (today anyway)
      real    fv(10)
      integer iv(10)
c
c...FUNCTIONS:
      integer fpgeti
c------------------------------------------------------------------
c
c........How big the device?
      it = fpgeti (gpsize, iv, fv)
      xpwid0 = fv(1)
      ypwid0 = fv(2)
c...avoid divide by zero later
      if (xpwid0 .le. 0.0) xpwid0 = 1.0
      if (ypwid0 .le. 0.0) ypwid0 = 1.0

c........Plot size in inches
      if (uxpwid.gt.0.0) xpwid0 = uxpwid
      if (uypwid.gt.0.0) ypwid0 = uypwid

c........Make some room for the Program name.
      if (lclear) call fclear

      avlen  = (xpwid0+ypwid0)/2.0
      titlsz = 0.030 * avlen

c........Setup sizes for axis drawing
      hnumb = 0.020 * avlen
      htic  = 0.015 * avlen
      htitle= 0.025 * avlen

      hlen = LEN(header)
      if (header(hlen:hlen).ne.' ') goto 21
      do 20 it = hlen, 1, -1
        if (header(it:it).ne.' ') then
            hlen = it
            goto 21
        endif
20    continue
21    continue

      vlen = LEN(hdvers)
      if (header(vlen:vlen).ne.' ') goto 31
      do 30 it = vlen, 1, -1
        if (hdvers(it:it).ne.' ') then
            vlen = it
            goto 31
        endif
30    continue
31    continue

c.......make sure titles are drawn in linetype=1
      call fnline(1)
      call fsymb2(titlsz, ypwid0-2.5*titlsz,
     +          header, hlen, titlsz, titlsz, 0.0,
     +          0.00, 0.00)
      call fsymb2(titlsz*(hlen+1.00), ypwid0-2.5*titlsz,
     +          hdvers, vlen, 0.60*titlsz, 0.60*titlsz, 0.0,
     +          0.00, 0.00)

c.... remove the room used for the headers.
c     ypwid0 = (1.0-0.15-0.15) * ypwid0
      ypwid0 = ypwid0 - 2.8*titlsz

c........Offset in inches
      xoff = 0.0
      yoff = 0.0
      if (uxoff.gt.0.0) xoff=uxoff
      if (uyoff.gt.0.0) yoff=uyoff
      call ftrans(xoff, yoff)
c
      return
      end
