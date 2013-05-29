cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Feb 14 14:24:42 PST 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Modified: Michael Eldredge -- Stanford (feb 89) Fix extract.h ordering
c    Now extrtmp.h since it is an "emaco overlay"
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EXTRAC
      include 'p2conf.h'
c
c     Parameter extraction segment.
c
c     Original : CSR Jun 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......Common area
      include     'blank.h'
      include     'logunt.h'
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c......Local types
      real answer
      character*30 enames(10)
      character*10 units(10)
      integer lu,i
      data enames/'Integrated net charge         ',
     +            'Integrated free carriers      ',
     +            'Integrated electron conc.     ',
     +            'Integrated hole conc.         ',
     +            'Contact charge                ',
     +            'n-Resistance                  ',
     +            'p-Resistance                  ',
     +            'Current integral(n)           ',
     +            'Current integral(p)           ',
     +            '                              '/

      data  units/'charges   ',
     +            'carriers  ',
     +            'electrons ',
     +            'holes     ',
     +            'coulombs  ',
     +            'ohm       ',
     +            'ohm       ',
     +            'amps      ',
     +            'amps      ',
     +            '          '/
      

c******************** Start ********************************************
c
c......What does the customer want?
      call extck

      if (errflg) return

      goto (100,100,100,100,200,300,300,200,200) emode


c......Integrated charge/free carriers/electrons/holes
c
  100 continue

      call eint (answer)
      goto 1000

c......Contact charge/current
c
  200 continue

      if (iuelc.ge.1) call efn (answer)
      if (iureg.ge.1) call jarea (answer)
      goto 1000

c......Resistivity
c
  300 continue
      
      call eres (answer)
      goto 1000

c......Print the requested number
c
 1000 continue
      lu = luout

 1010 continue
      write(lu,11) enames(emode),answer,units(emode),
     +             1e4*exmin,1e4*exmax,1e4*eymin,1e4*eymax

      if (lu.ne.lutty) then
       lu=lutty
       goto 1010
      endif

   11 format(/,a30,' is ',1pe15.8,1x,a10,' per micron of depth'/
     +       10x,'between X=',1pe10.3,' and X=',1pe10.3,/,
     +       10x,'between Y=',1pe10.3,' and Y=',1pe10.3,/)

c......Save if in a file?
c
      if (lopen) write(luxtra,*) answer,(bias(i)*dktq,i=1,nelect)
       
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EXTCK
      include 'p2conf.h'
c
c     Check the extract card
c
c     Original : CSR Jun 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common area
      include     'blank.h'
      include     'logunt.h'
      include     'setup.h'
      include     'stat.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c......local types
      integer mxmode,nmodes,i,ierr
      character*20  tmpbuf
c
c FUNCTIONS:
      logical isrval, iscval
      real    gtrval
      logical gtlval
c
      data mxmode/9/
c
c******************** Start ********************************************
c
c......If we don't have a mesh, no way this turkey can fly.
      if (.not.lmshdn) then 
       call erset(162,linum,0)
       return
      endif

c......What variable?
      
      nmodes = 0

      do 10 i = 1,mxmode
       if (gtlval(i)) then
          emode = i
          nmodes = nmodes+1
       endif
   10 continue

      if (nmodes.eq.0.or.nmodes.gt.mxmode) then
       call erset(160,linum,nmodes)
       return
      endif 


c......Do we have this variable present?
c      Linear variables can take a difference solution, 
c.......but currents can not.
c
      if ( (emode.ge.1.and.emode.le.7) .and.
     +     (.not.(lsol1.or.lsolst.or.ldiff))          )  then     
       call erset(161,linum,emode)
       return
      endif
      if ( (emode.ge.8.and.emode.le.9) .and.
     +     (.not.(lsol1.or.lsolst))         ) then
      call erset(161,linum,emode)
      return
      endif


c.......If this is a contact variable, get the electrode number
c
      if (emode.eq.5) then
       if (.not.isrval(5)) call erset(163,linum,0)
       iuelc=gtrval(5)
       if (iuelc .lt.1 .or. iuelc .gt. nelect) call erset(164,linum,0)
      endif

      if (emode.eq.8 .or. emode.eq.9) then
       if (isrval(5) .eqv. isrval(6))
     +       call erset(163,linum,0)
       iuelc = gtrval(5)
       iureg = gtrval(6)
       if ((iuelc .lt.1 .or. iuelc .gt. nelect) .and.
     +       (iureg .lt.1 .or. iureg .gt. nmat*10)) 
     +       call erset(164,linum,0)

      endif



c      Box (scale from microns to cm)
c......If the limits are messed up - well 0.0 gets printed is all.
      call devlts(exmin,exmax,eymin,eymax)
      if (isrval(1)) exmin = gtrval(1)*1e-4
      if (isrval(2)) exmax = gtrval(2)*1e-4
      if (isrval(3)) eymin = gtrval(3)*1e-4
      if (isrval(4)) eymax = gtrval(4)*1e-4


c......File i/o?
      if (iscval(1)) then
         call gtcval(1, tmpbuf, LEN(tmpbuf))
         call fopcl(22,tmpbuf,20,luxtra,.false.,ierr)
         lopen = .true.
      else
         lopen = .false.
      endif


c......Zow! Ay-way!
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EINT(answer)
      include 'p2conf.h'
c
c     Integrate a simple variable.
c
c     The integral is the sum, over all points included in the
c     box, of the integrated value at those points - no attempt
c     is made to window the nodal areas. If somebody really wants 
c     the integral that lies precisely inside the box, they can 
c     modify this code (but it is messy).
c
c     Original : CSR Jun 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common area
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c......local types
      integer ip,ie,j
      real answer,area,val,feval

c******************** Start ********************************************
c
c......Integrate the value at each point.

      do 10 ip = 1,np
   10    scrat(ip) = 0.e0

      do 100 ie = 1,ne
       
       do 100 j=1,3
          ip = nop(j,ie)
          area = es(j,ie)
          if (emode.eq.1) val = feval( ip, 7, .false.)
          if (emode.eq.2) val = feval( ip, 8, .false.)
          if (emode.eq.3) val = feval( ip, 5, .false.)
          if (emode.eq.4) val = feval( ip, 6, .false.)

          scrat(ip) = scrat(ip) + area*val
  100 continue

c.......Now sum all the points in the given box.

      answer = 0.e0

      do 200 ip = 1,np
       
       if (cord(1,ip).lt.exmin .or. cord(1,ip).gt.exmax .or.
     +       cord(2,ip).lt.eymin .or. cord(2,ip).gt.eymax) goto 200
       
       answer = answer + scrat(ip)

  200 continue

c......Scale to micron length
      answer = answer*1e-4

      if (ldebug) then
       write(16,*) 'Emode=',emode
       do 989 ip=1,np
  989      write(16,*) ip,scrat(ip)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EFN (answer)
      include 'p2conf.h'
c
c     Integrate current/charge over part of a contact.
c
c     Original : CSR Jun 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......Common area
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c......Local types
c
      real answer

      integer ip,npde,idxnod(3),nj,elmat,ie,nk
c...dummy for debugging...
      integer nmobct
      real esnod(3)
      double precision estifm(9,9),wrhs(9)
c
c******************** Start ********************************************
c
c........Clear the scratch array.
      do 100 ip=1,np
  100    scrat(ip)=0.e0
      nmobct=0


c........Prepare to call equation assembly.

      if (emode.eq.5) npde = 1
      if (emode.eq.8) npde = 2
      if (emode.eq.9) npde = 3


c........Compute divergence of displacement/current everywhere.

      do 200 ie = 1,ne

       elmat=mattyp(imat(ie))
       if (npde.gt.1 .and. elmat.le.0) goto 200
       do 210 nj = 1,3
          idxnod(nj) = nop(nj,ie)
          esnod (nj) = es (nj,ie)
  210    continue

       do 215 nj=1,3
          wrhs(nj)=0
          do 215 nk=1,3
  215          estifm(nj,nk)=0
       
       call elemn(ie,.true.,wrhs,estifm,3,elmat,esnod,idxnod,
     +              1.d0,1,npde,nmobct)

       do 220 nj=1,3
  220       scrat(idxnod(nj)) = scrat(idxnod(nj)) + wrhs(nj)


  200 continue


c.......Use charge/current only at electrode nodes lying in the box
      answer = 0.e0

      do 300 ip=1,np
       if (lm(ip).ne.iuelc) goto 300
       if (cord(1,ip).lt.exmin.or.cord(1,ip).gt.exmax.or.
     +       cord(2,ip).lt.eymin.or.cord(2,ip).gt.eymax) goto 300
       answer = answer + scrat(ip)
  300 continue

c.......Scale and split.
      if (npde.gt.1) answer = answer*djcoef
      if (npde.eq.1) answer = answer*decoef

c.......Debungle
      if (ldebug) then
       write(16,*) 'Emode=',emode,'Elec=',iuelc
       write(16,*) exmin,exmax,eymin,eymax
       do 989 ip=1,np
  989      write(16,*) ip,scrat(ip)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ERES (answer)
      include 'p2conf.h'
c
c     Integrate the conductivity over a region and return the resistance
c
c     Original : CSR Jun 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common area
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays

c------------------------------------------------------------------
c......local types
c
      real answer
      integer ip,ie,pj,pk,k,sides(5),j,elmat
      double precision p21,tmob,dum
      real cond,feval,cconc
      logical lpcont,no
c...debugging info
      integer nmobct

      data sides/1,2,3,1,2/,no/.false./,dum/0.d0/
c
c******************** Start ********************************************
c
c......Clear out the scratch array

      do 10 ip = 1,np
   10 scrat(ip) = 0.e0
      nmobct=0


c......Electrons or holes?
      lpcont = (emode.eq.7)
      


c......Scan triangles and skip insulators
c
      do 100 ie = 1,ne

       elmat = mattyp(imat(ie))
       if (elmat.le.0) goto 100
       

       do 101 j=1,3

          pj = nop(j,ie)

c............For each of its nodes, average the mobility going to 
c            each of the other nodes. This is only O(h) accurate,
c            but the individual sub-areas for each node are not
c............in the database, so we can't squeeze out that O(h**2)!

          cond = 0
          do 110 k=1,2
             pk = nop(sides(j+k),ie)

             p21 = feval(pk, 1, .false.) - feval(pj, 1, .false.)

             call dmobl(ie,j,sides(j+k),dum,p21,tmob,lpcont,elmat, 
     +                  no,dum,.false. , nmobct)
             cond = cond + tmob
  110       continue
          cond = cond / 2.e0

c............Multiply by the carrier concentration, by
c............the nodal area and sum it.

          if (lpcont) then
             cconc = feval( pj, 6, .false.)
          else
             cconc = feval( pj, 5, .false.)
          endif

          cond = cond * cconc * es(j,ie)

          scrat(pj) = scrat(pj) + cond


  101    continue

  100 continue


          
c.......Now sum all the points in the given box.

      answer = 0.e0

      do 200 ip = 1,np
       
       if (cord(1,ip).lt.exmin .or. cord(1,ip).gt.exmax .or.
     +       cord(2,ip).lt.eymin .or. cord(2,ip).gt.eymax) goto 200
       
       answer = answer + scrat(ip)

  200 continue

      answer = answer * qcharg

c.......Convert to resistivity and scale to microns
      if (answer.ne.0) answer = 1.e0 / answer
      answer = answer*1e-4


      if (ldebug) then
       write(16,*) 'Emode=',emode
       do 989 ip=1,np
  989      write(16,*) ip,scrat(ip)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE JAREA (answer)
      include 'p2conf.h'
c
c     Integrate current flow between two regions.
c
c     Original : CSR Aug 84
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......Common area
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'extrtmp.h'
      integer TMPPAD(1457994)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c......Local types
c
      real answer

      integer ip,idxnod(3),nj,elmat,ie,ir1,ir2,j,j1,j2,itp
      integer idum
      integer next(3), last(3)
      logical lpcont, lsurf
      double precision ddum, currnt
      data next/2,3,1/, last/3,1,2/
c
c******************** Start ********************************************
c
c........Clear the scratch array.
      do 100 ip=1,np
  100    scrat(ip)=0.e0

c
c........What regions?  Make sure at least ir1 is semiconductor.
      ir1 = iureg / 10
      ir2 = mod (iureg, 10)

      if  (mattyp (ir1) .lt. 0) then 
       itp = ir1
       ir1 = ir2
       ir2 = itp
      endif


c...........Include only current components 
c...........FROM region ir1 INTO ir2.
      lpcont = emode .eq. 9

      do 200 ie = 1,ne

       elmat=mattyp(imat(ie))
       if (elmat.le.0) goto 200

       if (imat(ie).ne.ir1) goto 200

       do 210 nj = 1,3
          idxnod(nj) = nop(nj,ie)
  210    continue

       do 250 j = 1,3
          if (nextel(j,ie).le.0) goto 250
          if (imat (nextel (j,ie)) .ne. ir2) goto 250
          j1 = next (j)
          j2 = last (j)


          lsurf = mattyp (ir2) .lt. 0
          call assmbj (lpcont,elmat,lsurf,jhjd(j2,ie),ie,j,j1,
     +                 currnt,ddum,ddum,.true.,ddum,ddum,ddum,ddum,
     +                 lfldmb,idum)

          scrat (idxnod (j1)) = scrat (idxnod (j1)) + currnt

          call assmbj (lpcont,elmat,lsurf,jhjd(j2,ie),ie,j,j2,
     +                 currnt,ddum,ddum,.true.,ddum,ddum,ddum,ddum,
     +                 lfldmb,idum)

          scrat (idxnod (j2)) = scrat (idxnod (j2)) + currnt

  250    continue

  200 continue


c.......Use current only at nodes lying in the box
      answer = 0.e0

      do 300 ip=1,np
       if (cord (1,ip).lt.exmin.or.cord (1,ip).gt.exmax.or.
     +       cord (2,ip).lt.eymin.or.cord (2,ip).gt.eymax) goto 300
       answer = answer + scrat(ip)
  300 continue

c.......Scale and split.
      answer = 2.d0 * answer*djcoef

c.......Debungle
      if (ldebug) then
       write(16,*) 'Emode=',emode,'Elec=',iureg
       write(16,*) exmin,exmax,eymin,eymax
       do 989 ip=1,np
  989      write(16,*) ip,scrat(ip)
      endif
      return
      end
