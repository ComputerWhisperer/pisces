cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:37:31 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MRDWR(namfil,lrdwrt,lold,lasin,lasout)
      include 'p2conf.h'
c 
c     Reads or writes binary mesh files including geometry, 
c     coupling coefficients and doping 
c
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP & CSR      Stanford University       Nov, 1983
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
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
      include     'logunt.h'
      include     'symb.h'
      include     'setup.h'
      include     'stat.h'
      include     'emaco.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lrdwrt,lold,lasin,lasout
      integer lu,i,j,ierr
      character*20 namfil
      character*20 itime 
      character*60 itit1
c 
c****************************************************************** 
c 
      if (lrdwrt) goto 500
c
c......Write! set lu pointing to outfil
      lu=lutmp

      if(.not.lasout) then
         call fopcl(11,namfil,20,lu,.false.,ierr)
         if (errflg) return
         write(lu,err=991) ititle
         write(lu,err=991) nx,ny,np,ne,nb,nelect,nmat,nepb
         write(lu,err=991) 
     +             ((cord(j,i),j=1,2),r1(i),dintf(i),itype(i),eptr(i),
     +             mobn(i),mobp(i),lm(i),essem(i),estot(i),i=1,np)
         write(lu,err=991) ((nop(i,j),i=1,3),(ehed(i,j),i=1,3),
     +             (jhjd(i,j),i=1,3),(es(i,j),i=1,3),
     +             (nextel(i,j),i=1,3),imat(j),j=1,ne)
         write(lu,err=991) (nbc(i),ietype(i),lmetal(i),i=1,nb)
         write(lu,err=991) ndreg,(dopsgn(i),ecode(i),i=1,ndreg)
         write(lu,err=991) (mattyp(i),i=1,nmat)
         write(lu,err=991) idaytm
      else
         call fopcl(12,namfil,20,lu,.false.,ierr)
         if (errflg) return
         write(lu,556,err=991) ititle
         write(lu,*,err=991) nx,ny,np,ne,nb,nelect,nmat,nepb
         write(lu,*,err=991) 
     +             ((cord(j,i),j=1,2),r1(i),dintf(i),itype(i),eptr(i),
     +             mobn(i),mobp(i),lm(i),essem(i),estot(i),i=1,np)
         write(lu,*,err=991) ((nop(i,j),i=1,3),(ehed(i,j),i=1,3),
     +             (jhjd(i,j),i=1,3),(es(i,j),i=1,3),
     +             (nextel(i,j),i=1,3),imat(j),j=1,ne)
         write(lu,*,err=991) (nbc(i),ietype(i),lmetal(i),i=1,nb)
         write(lu,*,err=991) ndreg,(dopsgn(i),ecode(i),i=1,ndreg)
         write(lu,*,err=991) (mattyp(i),i=1,nmat)
         write(lu,557,err=991) idaytm
      endif

      call fopcl(0,namfil,20,lu,.false.,ierr)
      write(luout,3001) namfil,' '
 3001 format(' Grid written to ',a20/a1)

      return
c****************************************************************** 
c 
c......Read! set lu pointing to infil
  500 lu=lutmp
      if(.not.lasin) then
         call fopcl(1,namfil,20,lu,.false.,ierr)
         if (errflg) return
         read(lu,err=992,end=993) itit1
         read(lu,err=992,end=993) nx,ny,np,ne,nb,nelect,nmat,nepb

         if (np.gt.npdim) call erset(69,linum,npdim) 
         if (ne.gt.nedim) call erset(70,linum,nedim) 
         if (nb.gt.nbdim) call erset(71,linum,nbdim) 
         if (errflg) goto 510
 
         read(lu,end=993,err=992) 
     +         ((cord(j,i),j=1,2),r1(i),dintf(i),itype(i),eptr(i),
     +         mobn(i),mobp(i),lm(i),essem(i),estot(i),i=1,np)
         read(lu,end=993,err=992) 
     +         ((nop(i,j),i=1,3),(ehed(i,j),i=1,3),
     +         (jhjd(i,j),i=1,3),(es(i,j),i=1,3),
     +         (nextel(i,j),i=1,3),imat(j),j=1,ne)
         read(lu,end=993,err=992) (nbc(i),ietype(i),lmetal(i),i=1,nb)
         read(lu,end=993,err=992) ndreg,(dopsgn(i),ecode(i),i=1,ndreg)
         read(lu,end=993,err=992) (mattyp(i),i=1,nmat)
         read(lu,end=993,err=992) itime
      else
         call fopcl(2,namfil,20,lu,.false.,ierr)
         if (errflg) return
         read(lu,556,err=992,end=993) itit1
         read(lu,*,err=992,end=993) 
     +             nx,ny,np,ne,nb,nelect,nmat,nepb

         if (np.gt.npdim) call erset(69,linum,npdim) 
         if (ne.gt.nedim) call erset(70,linum,nedim) 
         if (nb.gt.nbdim) call erset(71,linum,nbdim) 
         if (errflg) goto 510
 
         read(lu,*,end=993,err=992) 
     +         ((cord(j,i),j=1,2),r1(i),dintf(i),itype(i),eptr(i),
     +         mobn(i),mobp(i),lm(i),essem(i),estot(i),i=1,np)
         read(lu,*,end=993,err=992) 
     +         ((nop(i,j),i=1,3),(ehed(i,j),i=1,3),
     +         (jhjd(i,j),i=1,3),(es(i,j),i=1,3),
     +         (nextel(i,j),i=1,3),imat(j),j=1,ne)
         read(lu,*,end=993,err=992) (nbc(i),ietype(i),lmetal(i),i=1,nb)
         read(lu,*,end=993,err=992) ndreg,(dopsgn(i),ecode(i),i=1,ndreg)
         read(lu,*,end=993,err=992) (mattyp(i),i=1,nmat)
         read(lu,557,end=993,err=992) itime
      endif
c
c......Done. close the file! 
  510 call fopcl(0,namfil,20,lu,.false.,ierr)
c
c......Write message
      write(luout,3000) namfil,itime,' '
 3000 format(//' Grid read from ',a20/' Date code = ',a20/a1)
 556  format(a60)
 557  format(a20)
c
c......Bye!!!
      return

c......Here be dragons
  991 call erset(9,linum,0)
      return
  992 call erset(5,linum,0)
      return
  993 call erset(7,linum,0)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MGRDWR(namfil,lrdwrt,lflip,scaly)
      include 'p2conf.h'
c
c                   Read/write ascii files for grid editor
c
c     Original : CSR Oct'83
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
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
      logical lrdwrt,lflip,lsup4
      character*20 namfil
      integer i, j, lu, ir, ip, ipl, ibx, ib, it, ieln, jelec, ierr
      real scaly
c
c******************** Start ********************************************
c
c                   read or write?
c
      if(.not.lrdwrt) goto 2000
c
c***********************************************************************
c                   Read.                                              *
c***********************************************************************
c
      do 1 i=1,nedim
       imat(i)=0
         do 1 j=1,3
          nop(j,i)=0
    1 continue
        
      lsup4 = .false.
      lu=lutmp
      call fopcl(2,namfil,20,lu,.false.,ierr)
      if (errflg) return
      read(lu,*,err=999,end=999) np,ne,nb
      read(lu,*,err=999,end=999) nelect,nmat
c
      if(nmat.lt.0) then
        nmat = -nmat
        lsup4 = .true.
        ldopcd = .true.
      endif
      if(nelect.le.0) call erset(265,linum,0)
      if(errflg) return
c
      do 422 i=1,np
      if (lsup4) then
          read(lu,*,err=999,end=999) cord(1,i),cord(2,i),r1(i),tconc(i)
      else
          read(lu,*,err=999,end=999) cord(1,i),cord(2,i)
      endif
      cord(1,i)=cord(1,i)*scaly
      cord(2,i)=cord(2,i)*scaly
      if(lflip) cord(2,i)=-cord(2,i)
 422  continue
c
      do 425 j=1,ne 
 425  read(lu,*,err=999,end=999) imat(j),(nop(i,j),i=1,3)
c
      do 429 i=1,nb 
      read(lu,*,err=999,end=999) nbc(i),ietype(i)
 429  continue
c
      read(lu,*,err=999,end=999) (mattyp(i),i=1,nmat) 
      call fopcl(0,namfil,20,lu,.false.,ierr)
      write(luout,*) ' '
      write(luout,*) 'Read geometry from file ',namfil
      return
c
c......Error on read
  999 write(lutty,'(''Unexpected EOF in geometry file'')')
      call erset(289,linum,0)
      return
c       
c*********************************************************************
c                                                                    *
c                   Write mesh geometry in IGGI format               *
c                                                                    *
c*********************************************************************
 2000 continue

c........Open file
      lu = lutmp
      call fopcl(12,namfil,20,lu,.false.,ierr)
      if (errflg) return
      
c
c                   Initialise: 
c                   Set up adjacency matrices, then
c                   set up outer perimeter, then 
c...................the perimeters of the subregions.
      call nxtel(p2t,p2tc)

      maxigr=30
      call iperim(maxigr+1,lflip)
      if (errflg) return

      do 2001 ir=1,nmat
       call iperim(ir,lflip)
       if (errflg) return
 2001 continue

c
c                   Fix boundary nodes according to the IGGI
c...................convention : any node on a boundary is region 0
      do 2050 ip=1,np
 2050    igginn(ip)=itype(ip)
      
      do 2100 ir=1,nmat
      ipl=ipleng(ir)
      do 2100 ibx=1,ipl
        ib=irb(ir,ibx)
 2100     igginn(ib)=0

      ipl=ipleng(maxigr+1)
      do 2150 ibx=1,ipl
       ib=irb(maxigr+1,ibx)
 2150    igginn(ib)=0

c                   Write the grid.
c                   Nodes first: 
c                   Scale x,y coords to microns and write region numbers.

      do 2200 ip=1,np
      if(lflip) then
         write(lu,2201) 1.0d4*cord(1,ip)*scaly,
     +                 -1.0d4*cord(2,ip)*scaly,igginn(ip)
      else
         write(lu,2201) 1.0d4*cord(1,ip)*scaly, 
     +                  1.0d4*cord(2,ip)*scaly,igginn(ip)
      endif
 2200 continue
 2201 format('c  ',1pe12.5,3x,1pe12.5,3x,i5)
c
c...................Outer perimeter
      write(lu,2202) ' '
 2202 format('p  ',a1)
      ipl=ipleng(maxigr+1)
      do 2300 ib=1,ipl
 2300 write(lu,2301) irb(maxigr+1,ib)
 2301 format('b  ',i5)
c
c.............The regions
      do 2400 ir=1,nmat
          write(lu,2401) mattyp(ir)
 2401     format('r  ',i5,3x,'  0.0 ')

          ipl=ipleng(ir)
        do 2400 ib=1,ipl
            ip=irb(ir,ib)
            write(lu,2402) ip
 2400 continue
 2402 format('b  ',i5)



c...................Electrodes
      do 2500 jelec=1,nelect
       write(lu,2501) ' '
       do 2500 ieln=1,nb
         if (ietype(ieln).ne.jelec) goto 2500
          write(lu,2402) nbc(ieln)
 2500 continue
 2501 format('e  ',a1)



c...................Triangles
      do 2700 it=1,ne
 2700 write(lu,2701) (nop(j,it),j=1,3), imat(it)
 2701 format('t  ',5i5)


c...................Tidy up and exit
      write(lu,2601)
2601  format('d  ')
c
c......Done. close the file! 
      call fopcl(0,namfil,20,lu,.false.,ierr)
      write(luout,3001) namfil,' '
 3001 format(/'ASCII format file written to ',a20/a1)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IPERIM(ireg,lflip)
      include 'p2conf.h'
c
c                   Set up the counter clockwise boundary of a region. 
c           (Clockwise if lflip)
c                   Given is the set of triangles comprising the region. 
c                   The outer perimeter is coded in
c                   as region number (max_iggi_region+1)
c
c     Original : CSR Oct'83
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'blank.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'igitmp.h'
      integer TMPPAD(1367991)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer  ireg,estart,ip,ib1,ib2,ib3,ixlt,ie,ix,ilocal
      logical  edgelt,lflip
c
c
c******************** Start ********************************************
c
c                   Find somewhere to start in the region
      estart = 0
      do 100 ie=1,ne
       if (edgelt(ireg,ie,ib1,ib2,ib3,lflip)) then
           estart = ie
           goto 110
       endif
  100 continue
  110 continue
c
c                   Assert : estart is not 0
c                   neighbour(j,estart) is 0 or has a different
c                   material type, where j is the side joinig ib1-ib2
c                   Also, ib1-ib2 is counter-clockwise 
c                   Initialise irb(ir,*)

      irb(ireg,1)=ib1
      irb(ireg,2)=ib2
      ipleng(ireg)=2
      if (ib3.ne.0) then
        irb(ireg,3)=ib3
        ipleng(ireg)=3
      endif
c
c                   While(irb(ireg,ipleng(ireg)) is not irb(ireg,1)) do...
c                   ipleng is the current length of the region
c                   irb(*,ipleng) is the latest boundary node.
c                   We search among the triangles at irb(*,ipleng) for one 
c                   that contains another more counterclockwise node
c
  500 continue
      ilocal=ipleng(ireg)
      if (ilocal.ge.1000) call erset(257,0,ilocal)
      if (errflg) return
      ip = irb(ireg,ipleng(ireg))
      if (ip.eq.irb(ireg,1)) goto 1000

      ixlt=p2tc(ip)
      do 800 ix=1,ixlt
        ie=p2t(ix,ip)

          if (edgelt(ireg,ie,ib1,ib2,ib3,lflip)) then
          if (ib1.eq.ip) then
        ipleng(ireg)=ipleng(ireg)+1
        irb(ireg,ipleng(ireg))=ib2
        if (ib3.ne.0) then
            ipleng(ireg)=ipleng(ireg)+1
            irb(ireg,ipleng(ireg))=ib3
        endif
          else
             if (ib2.eq.ip) then
           if (ib3.ne.0) then
               ipleng(ireg)=ipleng(ireg)+1
               irb(ireg,ipleng(ireg))=ib3
           endif
             endif
          endif
        endif
  800 continue
      
      goto 500
c
c                   Done : node(pleng)=node(1). Reduce pleng by 1 & exit
 1000 continue
      ipleng(ireg)=ipleng(ireg)-1
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION EDGELT(ireg,ie,ib1,ib2,ib3,lflip)
      include 'p2conf.h'
c
c           Return :
c               FALSE if the element ie does not have an edge on the 
c                     boundary of region ireg (ib1,ib2,ib3=0)
c                  
c               TRUE  if it does.
c                     if only one edge, then ib3=0 and ib1-ib2 is the 
c                     counterclockwise edge 
c                     if two edges, then ib1-ib2-ib3 are the three 
c                     nodes in counterclockwise sequence. 
c                     if three edges, an error has occurred.
c
c     Original : CSR Oct'83
c     Modified : CSR Aug'84  Works for clockwise grids too
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c                             
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'igitmp.h'
      integer TMPPAD(1367991)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer  ireg,ie,ib1,ib2,ib3,jj(2),jx,jcorn,tmod,emat,i,in,ilocal
      real     orient,aot
      logical  ltemp,lcc,lflip
c
c******************** Start ********************************************
c
      jx=0
      emat = imat(ie)
      ilocal=maxigr+1
      if (ireg.ne.(maxigr+1).and.emat.ne.ireg) goto 199
      do 100 i=1,3
          in=nextel(i,ie)
          if (ireg.ne.(maxigr+1)) then
            ltemp=(in.le.0)
              if (.not.ltemp) ltemp = (imat(in).ne.emat)
          else
            ltemp=(in.le.0)
          endif
        
        if (ltemp) then 
            jx=jx+1
            if (jx.ge.3) write(6,*) 'Isolated triangle??'
            jj(jx)=i
        endif
  100 continue

  199 if (jx.eq.0) then
        edgelt=.false.
        ib1=0
        ib2=0
        ib3=0
        return
      else
        edgelt=.true.
        orient=aot(cord(1,nop(1,ie)),cord(2,nop(1,ie)),
     +               cord(1,nop(2,ie)),cord(2,nop(2,ie)),
     +               cord(1,nop(3,ie)),cord(2,nop(3,ie)) )
          lcc=(orient.gt.0)
        if (lflip) lcc=.not.lcc
        if (jx.eq.1) then
            ib3=0
            if (lcc) then
          ib1=nop(tmod(jj(1)+1),ie)
          ib2=nop(tmod(jj(1)+2),ie)
            else
          ib1=nop(tmod(jj(1)+2),ie)
          ib2=nop(tmod(jj(1)+1),ie)
            endif
        else
            jcorn=6-jj(1)-jj(2)
            ib2=nop(jcorn,ie)
            if (lcc) then
          ib1=nop(tmod(jcorn-1),ie)
          ib3=nop(tmod(jcorn+1),ie)
            else
          ib1=nop(tmod(jcorn+1),ie)
          ib3=nop(tmod(jcorn-1),ie)
            endif
        endif
      endif
      return
      end
