cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sat Sep  8 13:10:41 PDT 1990 (chin--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE S4READ(namfil,nregio)
      include 'p2conf.h'
c 
c     Reads SUPREM-IV geometry files and interpolates doping onto
c         an existing PISCES-IIB mesh.
c
c     Original : G. Chin      Stanford University       September, 1990
c 
c     copyright c 1990 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'symb.h'
      include     'stat.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'igitmp.h'
      integer TMPPAD(1367991)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                    local types & paramters
c
      logical lsup4
      character*20 namfil
      integer i, j, lu, ierr
      real xp, yp
c
c     temporary arrays for interpolation
      INTeger snmat
      INTeger snop(3,MAXTRI), simat(MAXTRI)
      real scord(2,MAXPT),sr1(MAXPT),stconc(MAXPT)
      real x1,x2,x3,y1,y2,y3,z1,z2,z3,norx,nory,norz
      real tarea,artol,tsign,bgconc,btconc
      integer smatyp(MAXREG), snp, sne, snb, snelec
      integer nregio(10),imin
      INTeger ijunk 
      real expu
      real abs
c
c
c***********************************************************************
c                   Read.                                              *
c***********************************************************************
c
      do 1 i=1,nedim
       simat(i)=0
         do 1 j=1,3
          snop(j,i)=0
    1 continue
        
      lsup4 = .false.
      lu=lutmp
      call fopcl(2,namfil,20,lu,.false.,ierr)
      if (errflg) return
      read(lu,*,err=999,end=999) snp,sne,snb
      read(lu,*,err=999,end=999) snelec,snmat
c
      if(snmat.lt.0) then
        snmat = -snmat
        lsup4 = .true.
        ldopcd = .true.
      endif
      if(nelect.le.0) call erset(265,linum,0)
      if(lsup4 .eqv. .false.) call erset(136,linum,0)
      if(errflg) return
c
      do 422 i=1,snp
      read(lu,*,err=999,end=999) scord(1,i),scord(2,i),sr1(i),stconc(i)
 422  continue
c
      do 425 j=1,sne 
 425  read(lu,*,err=999,end=999) simat(j),(snop(i,j),i=1,3)
c
      do 429 i=1,snb 
      read(lu,*,err=999,end=999) ijunk,ijunk
 429  continue
c
      read(lu,*,err=999,end=1000) (smatyp(i),i=1,snmat) 
      do 500 i=1,snmat
      nregio(i)=1
      if (mattyp(i).lt.0) nregio(i)=0
 500  continue
      call fopcl(0,namfil,20,lu,.false.,ierr)
      write(luout,*) ' '
      write(luout,*) 'Read doping from file ',namfil
      goto 1000
c
c......Error on read
  999 write(lutty,'(''Unexpected EOF in geometry file'')')
      call erset(289,linum,0)
 1000 continue


c.....get background concentration for points that aren't in
c       in the SUPREM-IV mesh
      bgconc=1.0e10
      btconc=1.0e20
      do 1100 i=1,snp
      if ((stconc(i).lt.btconc).and.(stconc(i).gt.1.0e10)) then
        btconc=stconc(i)
        imin=i
      endif
 1100 continue
      bgconc=sr1(imin)

c......Fill r1, tconc arrays now so that we don't have to keep the
c        temporary arrays
      do 2000 i=1,np
      if (nregio(itype(i)).eq.0) goto 2000
      xp=cord(1,i)*1.0e4
      yp=cord(2,i)*1.0e4
      
c........loop over triangles in SUPREM-IV mesh
      do 1500 j=1,sne
        x1=scord(1,snop(1,j))*1.0e4
        y1=scord(2,snop(1,j))*1.0e4
        x2=scord(1,snop(2,j))*1.0e4
        y2=scord(2,snop(2,j))*1.0e4
        x3=scord(1,snop(3,j))*1.0e4
        y3=scord(2,snop(3,j))*1.0e4
        tarea=0.5*( x2*(y3-y1) + x3*(y1-y2) + x1*(y2-y3) )
        if (tarea .gt. 0) then
          tsign=1.0
        else
          tsign=-1.0
        endif
        artol=1.0e-4*abs(tarea)

c..........now we check to see if yp,xp is in this triangle.  if
c           not we go onto the next triangle
        if ((tsign*0.5*( x2*(yp-y1) + xp*(y1-y2) + x1*(y2-yp)))
     +      .le. -artol) goto 1500
        if ((tsign*0.5*( x3*(yp-y2) + xp*(y2-y3) + x2*(y3-yp)))
     +      .le. -artol) goto 1500
        if ((tsign*0.5*( x1*(yp-y3) + xp*(y3-y1) + x3*(y1-yp)))
     +      .le. -artol) goto 1500

c........interpolate
        tsign=sr1(snop(1,j))
        if (tsign .lt. 0) then
          z1=-alog(abs(tsign))
        else
          z1=alog(tsign)
        endif
        tsign=sr1(snop(2,j))
        if (tsign .lt. 0) then
          z2=-alog(abs(tsign))
        else
          z2=alog(tsign)
        endif
        tsign=sr1(snop(3,j))
        if (tsign .lt. 0) then
          z3=-alog(abs(tsign))
        else
          z3=alog(tsign)
        endif

        norx=(y1-y2)*(z3-z2) - ((z1-z2)*(y3-y2))
        nory=(z1-z2)*(x3-x2) - ((x1-x2)*(z3-z2))
        norz=(x1-x2)*(y3-y2) - ((y1-y2)*(x3-x2))

        tsign=z1 + ((x1-xp)*norx + (y1-yp)*nory)/norz
        if (tsign .lt. 0) then
          r1(i)=-expu(abs(tsign))
        else
          r1(i)=expu(tsign)
        endif

        z1=alog(stconc(snop(1,j)))
        z2=alog(stconc(snop(2,j)))
        z3=alog(stconc(snop(3,j)))
        norx=(y1-y2)*(z3-z2) - ((z1-z2)*(y3-y2))
        nory=(z1-z2)*(x3-x2) - ((x1-x2)*(z3-z2))
        norz=(x1-x2)*(y3-y2) - ((y1-y2)*(x3-x2))

        tconc(i)=expu(z1 + ((x1-xp)*norx + (y1-yp)*nory)/norz)

c....    next point in PISCES mesh
        goto 2000
 1500 continue
c...if we get here then we haven't filled r1, tconc. use background values
      r1(i)=bgconc
      tconc(i)=btconc
 2000 continue


      return
      end
