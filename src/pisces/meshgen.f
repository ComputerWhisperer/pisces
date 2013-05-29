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
      SUBROUTINE MSHGEN 
      include 'p2conf.h'
c 
c     This routine generates two dimensional finite element mesh
c 
c 
c                 8/31/78      S.Y.Oh
c                11/01/79      C.H.Price 
c                 8/31/82      M.E.Law (Vax conversion)
c                   10/83      CSR (re-organised, general mesh)
c                   11/83      MRP (x-dir)
c                    2/84      CSR (regrid)
c                    2/84      MRP (SUPREM-III,mobility)
c                    3/84      MRP (redope)
c                    3/84      CSR (new region allocation algorithm)
c                    4/84      MRP (doping-electrode connectivity)
c
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'stat.h'
      include     'key.h'
      include     'logunt.h'
      include     'setup.h'
      include     'rgrid.h'
      include     'emaco.h'
c
c                   common overlay in meshgen is
c   (square) or  (laplace)   |  (geometry) and (doping)     | (regrid)
c   -------------------------|--------------|---------------|---------
c    emaco      emaco=laplace|            emaco             |   emaco
c    mshtmp                  |            adjtmp            |  adjtmp
c                            |            doptmp            |  rgdtmp
c                            |                              |  ordtmp
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     local types
c
      logical  lgeomf,loutfl,lascut,lrdwrt,lunif,lxerfc,lprefl,lflipy,
     +         lxdir,lsuprm,ldopfl,lde,lold,lbidop,lasin,lasout,
     +         lfdiag,lerfdp,lgauss
      logical  ls4geo
      integer  lunit,ibug,smoky,nsup,i,nregio(MAXREG),ierr
      real     conc,xdev,ydev,xleft,xright,ytop,ybot,scaly
      character*20 infil,noutfl,dopfl,nascfl,chdum
c
c FUNCTIONS:
      logical gtkey
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c 
c...Initialize
      dopfl='                    '
      loutfl=.false.
      ldopfl=.false.
      lascut=.false.
      lde=lmshdn
      goto 100
c                   return point if next card not fetched 
c ................  terminate cleanly on eof, exit on error 
  99  errflg = .not.gtkey(keyid)
      eofflg = errflg
      if(errflg) return
      if(eofflg) goto 200
c
c                   return point if next card already fetched
c
 100  if(keyid.eq.kcomme) goto 99
      if(keyid.eq.kmesh)  goto 1
      if(keyid.eq.kdopin) goto 2
c
c     not a mesh-category card, write mesh & exit
c     Set 'green-file' name from input, override with output.
c
  200 call finde

      ttname = ' '
      if (lprefl) call extnm(infil,ttname)
      if (loutfl) call extnm(noutfl,ttname)

      lmshdn=.true.
      lcrdrd=.true.
      if(ldopfl) call fopcl(0,chdum,20,lutmp,.false.,ierr)
      if (loutfl) then
          lrdwrt=.false.
          call mrdwr(noutfl,lrdwrt,.false.,lasin,lasout)
      endif
      if(errflg) return
      if (lascut) then
          lrdwrt=.false.
          call mgrdwr(nascfl,lrdwrt,lflipy,scaly)
      endif

      if((nx.eq.0).or.(ny.eq.0)) lrect=.false.

c..   Calculate Ndconc and Naconc from r1. This is a temporary kludge. (hry)
      do 210 i=1,np
      if (r1(i) .GT. 0.0d0) then
        ndconc(i)=r1(i)
        naconc(i)=0.0d0
      else
        ndconc(i)=0.0d0
        naconc(i)=abs(r1(i))
       endif
  210 continue

      return
c
c***********************************************************************
c              Decide on mesh geometry and i/o forms
c
   1  call mshck(ibug,lunit,lgeomf,lprefl,infil,loutfl,noutfl,
     +        lascut,nascfl,smoky,lold,lflipy,scaly,lasin,lasout,lfdiag)
      lcrdrd=.false.
      if (errflg) return
c
c...............If previous, then read and return
c               Else if geometry then read
c...............     else create mesh
      if (lprefl) then
          lrdwrt=.true.
          call mrdwr(infil,lrdwrt,lold,lasin,lasout)
      else 
          if (lgeomf) then
              lrdwrt=.true.
              call mgrdwr(infil,lrdwrt,lflipy,scaly)
              if (errflg) return
          else
              if (lrect) then 
                  call rectm(lfdiag)
              else 
                  call iregm(infil)
              endif
          endif
      endif
      if (errflg) return
c
c...............Improve the point distribution?
      if ((smoky.le.0).and.lprefl) then
         lmshdn=.true.
         lde=.true.
         goto 60
      endif
      if (smoky.gt.0) then
         do 43 i=1,nmat
43       nregio(i)=1
         call smooth(smoky,nregio)
      endif
c
c...............By line 60 we must know the geometry : points,
c               elements, electrodes, regions (in terms of nodes).
c...............Routine mend finishes the mesh.
  60  call mend
      if(errflg) return
c
c...............Return to controller
      if (     lcrdrd) goto 100
      if (.not.lcrdrd) goto 99
c
c***********************************************************************
c               Doping card. 
c
   2  lde=.false.
      if (.not.ldopcd) call initdp
      call dopck(nregio,conc,xdev,ydev,xleft,xright,ytop,ybot,lunif,
     +           lxerfc,lxdir,lsuprm,nsup,dopfl,ldopfl,lbidop,lerfdp,
     +           lgauss,ls4geo)
      if (errflg) return
      call dopng(nregio,conc,xdev,ydev,xleft,xright,ytop,ybot,
     +           lunif,lxerfc,lxdir,lsuprm,nsup,lbidop,lerfdp,lgauss,
     +           ls4geo)
      goto 99
c
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MSHCK(ibug,lunit,lgeomf,lprefl,infil,loutfl,noutfl,
     +      lascut,nascfl,smoky,lold,lflipy,scaly,lasin,lasout,lfdiag)
      include 'p2conf.h'
c 
c                   mesh card check routine 
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'stat.h'
      include     'logunt.h'
      include     'symme.h'
c
c                   type declarations 
c 
      logical lgeomf,loutfl,lascut,lprefl,ldesf,lold,lflipy
      logical lasin,lasout,lfdiag
      integer ibug,lunit,smoky,ierr
      real scaly
      character*20 infil,noutfl,nascfl,cpufil
c
c FUNCTIONS:
      logical isrval, iscval, islval
      real    gtrval
      logical gtlval
c
c
      data cpufil/'pisces.cpu          '/
c 
c****************************************************************** 
c
c...Debug, cpu flag 
      if(islval(4)) ldebug=gtlval(4)
      lcpu=gtlval(3)
      if(.not.lcpu) goto 10
      if(iscval(4)) call gtcval(4, cpufil, LEN(cpufil))
      call fopcl(12,cpufil,20,lucpu,.false.,ierr)
      write(lucpu,1000) 
1000  format('PISCES-II CPU PROFILE')
c
c...Old format?
10    lold=gtlval(5)
      lflipy=gtlval(8)
      if (isrval(5)) then
         scaly=gtrval(5)
      else
         scaly=1.0
      endif
c 
c...Set mesh card true, others false
      lmshcd=.true. 
      lmshdn=.false.
      lsymdn=.false.
      lmthdn=.false.
      lregcd=.false.
      lelecd=.false.
      lxmscd=.false.
      lymscd=.false.
      ldopcd=.false.
      lprefl =.false.
      lgeomf=.false.
c
      lgeomf=gtlval(6)
      ldesf=gtlval(7)
      lrect =gtlval(2).and..not.(lgeomf.or.ldesf)
      smoky = gtrval(6)
c
c..........Symmetrical coordination
      lcyl=gtlval(12)
c
c..........Width parameter?
      if (isrval(3)) then
         lwidth=.true.
         width = gtrval(3)
      endif
c
c..........Read input filename
      if (.not.iscval(1)) goto 2
      call gtcval(1, infil, LEN(infil))
      lprefl=.not.lgeomf.and..not.ldesf
c
c..........See if file specified for binary output
c..........lascut -> IGGI input format
c..........lasin  -> PISCES/ASCII format (input file)
c..........lasout -> PISCES/ASCII format (output file)
   2  loutfl=.false.
      lascut=.false.
      lasin=gtlval(9)
      lasout=gtlval(10)
      if (.not.iscval(2)) goto 4
c
c..........Yes, get filename 
      loutfl=.true. 
      call gtcval(2, noutfl, LEN(noutfl))
c
c..........See if file specified for ascii output
   4  if (.not.iscval(3)) goto 6
      lascut=.true.
      call gtcval(3, nascfl, LEN(nascfl))
c
c..........Get any neccesary mesh-creation parameters
   6  ibug=gtrval(3)
      if (lprefl) return
      if (.not.lrect) goto 7
c
c..........Get rectangular grid size 
      nx=gtrval(1)
      ny=gtrval(2)
c
c..........nx and ny must be specified and be within bounds
      if (nx.le.0.or.ny.le.0.or.nx.gt.nxymax.or.ny.gt.nxymax) 
     +    call erset(14,linum,nxymax) 
c
c..........flip diagonals?
      lfdiag=gtlval(11)
c 
   7  return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MEND
      include 'p2conf.h'
c     
c     date code 101483 CSR
c               4/84   MRP
c
c     This routine is the final step in any mesh 
c     generation(rect,irreg,regrid)
c
c     input  : nodes, elements, electrodes, regions(in terms of nodes)
c
c     output : coupling coefficients for matrix assembly
c              contact metal lengths  "    "     "
c              node region numbers
c              sorted contact arrays
c              lm array.
c 
c     copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'stat.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c                   type declarations 
c 
      integer ntemp1,i,j,nbcnt,ix,elmat,ipt
c 
c******************** Start ********************************************
c 
c............Every element must have a valid region number
      do 20 i=1,ne
      if (imat(i).ge.1.and.imat(i).le.nmat) goto 20 
      call erset(-72,linum,i)
  20  continue
      if (errflg) return

c
c............Define node region numbers. 
      if(.not.lbicep) call noderg

c
c............Compute geometrical factors 
c............Coupling coefficients, triangle sides, node areas)
c............Adjacency info is assembled and doping-electrode
c............connectivity is determined
      call geom
c
c............Get total and semiconductor area at each node 
c............(estot,essem)
      do 884 i=1,np
         estot(i)=0.
884      essem(i)=0.

      do 885 i=1,ne 
         elmat=mattyp(imat(i))
         do 885 j=1,3
            ipt=nop(j,i)
            estot(ipt)=estot(ipt)+es(j,i)
            if(elmat.gt.0) essem(ipt)=essem(ipt)+es(j,i)
885      continue

c 
c............Sort nbc (with ietype) into numerical order 
      nbcnt=1 
      do 1 i=1,np 
          do 2 j=nbcnt,nb 
              if (nbc(j).ne.i) goto 2 
              ntemp1=nbc(nbcnt) 
              nbc(nbcnt)=nbc(j) 
              nbc(j)=ntemp1 
              ntemp1=ietype(nbcnt)
              ietype(nbcnt)=ietype(j) 
              ietype(j)=ntemp1
              nbcnt=nbcnt+1 
   2      continue
   1  continue
      if (nbcnt.ne.nb+1) call erset(62,linum,0) 
      if (errflg) return
c
c...See if the turkey has overlapped electrodes.
c...(he dies only if the node is defined twice with differing no.'s)
      nbcnt=nb-1
      j=1
3     if(j.gt.nbcnt) goto 44
         if(nbc(j).ne.nbc(j+1)) goto 333
         if(ietype(j).eq.ietype(j+1)) then
            ix=j+1
            do 33 i=ix,nbcnt
               nbc(i)=nbc(i+1)
               ietype(i)=ietype(i+1)
33          continue
            nbcnt=nbcnt-1
            nb=nb-1
         else
            ix=ietype(j)
            call erset(128,linum,ix)
         endif
333   j=j+1
      goto 3
c
c..............Generate lm 
c....New defn -
c....lm(i) = contact# if i is an electrode node
c....lm(i) = 0        if i isnt an electrode
44    if(errflg) return
      nbcnt=1
      do 5 j=1,np
         if (j.eq.nbc(nbcnt)) then
            lm(j)=ietype(nbcnt)
            nbcnt=nbcnt+1
         else
            lm(j)=0
         endif
    5 continue

c
c............Generate temp. adjacency info.
      call nxtel(p2t,p2tc)
c
c............Compute metal lengths and interface
      call cmetal
      call intfln
c
c............Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE GEOM
      include 'p2conf.h'
c 
c       calculation of the geometrical data in the elements
c       using a discretization similar to winslow[66] but with
c       a less arbitrary area distribution.
c       
c          1. ehed : the coupling coefficients between nodes
c          2. es   : the areas belonging to the nodes
c 
c 
c                               4/11/79 
c                  modified     8/04/80   by   C. Price 
c                               4/84           MRP
c     Cylindrical coordinates  11/87           R. Lowther (Harris)
c     cylindrical volume calc added  1/88      A.Yabuta
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'logunt.h'
      include     'symme.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c------------------------------------------------------------------
c                   type declarations 
c 
      logical swpflg
      integer i,j,k,ie,swpcnt,obct
      double precision xij,yij,xjk,yjk,xki,yki,disq,djsq,dksq,
     +     di,dj,dk,s,s4,r,rsq,den,tani,tanj,tank,hi,hj,hk,ai,aj,ak,
     +     si,sj,sk,xi,yi,xj,yj,xk,yk,
     +     XCENTR,YCENTR,TPIRI,TPIRJ,TPIRK,VOLUME, 
     +     tmp,xcntr1,ycntr1,xcntr2,ycntr2
      real obpct
c 
c******************** Start ********************************************
c
c       1. scan the elements
c
      obct = 0
      do 1000 ie=1,ne 
c 
c......Set the swap flag and counter 
      swpcnt = 1
      swpflg =.true.
c 
10    i=nop(1,ie) 
      j=nop(2,ie) 
      k=nop(3,ie) 
c 
      xi=cord(1,i)
      yi=cord(2,i)
      xj=cord(1,j)
      yj=cord(2,j)
      xk=cord(1,k)
      yk=cord(2,k)
c 
      xij=xi-xj 
      yij=yi-yj 
      xjk=xj-xk 
      yjk=yj-yk 
      xki=xk-xi 
      yki=yk-yi 
c
c......Calculate parameters of cylindrical coordination
C......(XCENTR,YCENTR) IS CENTER OF CIRCUMSCRIBED CIRCLE
C                                      FOR TRIANGLE
      IF (LCYL) then
         call centr(xi,yi,xj,yj,xk,yk,xcentr,ycentr) 
C
C......Calculate the surface area of sides
         TPIRI=3.1415927*(XCENTR+0.5*(XJ+XK))
         TPIRJ=3.1415927*(XCENTR+0.5*(XK+XI))
         TPIRK=3.1415927*(XCENTR+0.5*(XI+XJ))
C
C......Calculate the volume of trianges integral
      ENDIF
c
c......Compute coupling coefficients (cotangents of opposite angles)
c......and get denominator term
      den=xki*yjk-yki*xjk 
c 
c......Triangle area is -1/2 of this value 
      s=-.5d0*den 
      if (s.le.0.d0) then 
        if (swpcnt.gt.6) call erset(63,linum,ie)
        if (errflg) goto 9999 
        swpcnt = swpcnt+1 
        if (swpflg) then
c
c......Swap second and third nodes 
          nop(2, ie) = k
          nop(3, ie) = j
        else
c
c......Swap first and second nodes 
          nop(1, ie) = j
          nop(2, ie) = i
        endif 
        swpflg = .not.swpflg
        goto 10 
      endif 
c 
c......We want 1/2 of cotangent so invert and scale
      den=.5d0/den
c 
      ehed(1,ie)=(xki*xij+yki*yij)*den
      ehed(2,ie)=(xij*xjk+yij*yjk)*den
      ehed(3,ie)=(xjk*xki+yjk*yki)*den
c
c......Cylindrical coordination
      IF (LCYL) THEN
         EHED(1,IE)=TPIRI*EHED(1,IE)
         EHED(2,IE)=TPIRJ*EHED(2,IE)
         EHED(3,IE)=TPIRK*EHED(3,IE)
      ENDIF
c 
c                   compute area weighting
c 
c***    2. calculate the distances between the nodes
c          (store in temporary common doptmp for cmetal)
c
      disq=xjk*xjk+yjk*yjk
      djsq=xki*xki+yki*yki
      dksq=xij*xij+yij*yij
      di=dsqrt(disq)
      dj=dsqrt(djsq)
      dk=dsqrt(dksq)
      if (di.eq.0.or.dj.eq.0.or.dk.eq.0) call erset(129,linum,ie)
      tsides(1,ie)=real(di)
      tsides(2,ie)=real(dj)
      tsides(3,ie)=real(dk)
c
c......Cylindrical coordination
      IF (LCYL) THEN
         TSIDES(1,IE)=TSIDES(1,IE)*3.1415927*(XJ+XK)
         TSIDES(2,IE)=TSIDES(2,IE)*3.1415927*(XK+XI)
         TSIDES(3,IE)=TSIDES(3,IE)*3.1415927*(XI+XJ)
      ENDIF
c 
c***    3. calculate the area, radius for outer circle and tangents 
c
      s4=s*4.d0 
      r=di*dj*dk/s4 
      rsq=r*r 
      den=djsq+dksq-disq
      if(den.eq.0.0d0) goto 400 
      tani=s4/den 
      den=dksq+disq-djsq
      if(den.eq.0.0d0) goto 400 
      tanj=s4/den 
      den=disq+djsq-dksq
      if(den.eq.0.0d0) goto 400 
      tank=s4/den 
c 
c***    4. check for obtuse angle 
c
      if(tani.lt.0.0d0.or.tanj.lt.0.0d0.or.tank.lt.0.0d0) goto 500
c 
c***    5. acute triangle case (jhjd=ehed)
c
400   jhjd(1,ie)=ehed(1,ie)
      jhjd(2,ie)=ehed(2,ie)
      jhjd(3,ie)=ehed(3,ie)
c
      ai=0.25d0*di*dsqrt(dabs(rsq-0.25d0*disq)) 
      aj=0.25d0*dj*dsqrt(dabs(rsq-0.25d0*djsq)) 
      ak=0.25d0*dk*dsqrt(dabs(rsq-0.25d0*dksq)) 
      si=aj+ak
      sj=ak+ai
      sk=ai+aj
c
c.....Cylindrical coodination(volume)
      IF(LCYL) THEN
        SI=VOLUME(XI,YI,(XI+XJ)/2.,(YI+YJ)/2.,XCENTR,YCENTR)
     +    +VOLUME(XI,YI,(XI+XK)/2.,(YI+YK)/2.,XCENTR,YCENTR)
        SJ=VOLUME(XJ,YJ,(XJ+XK)/2.,(YJ+YK)/2.,XCENTR,YCENTR)
     +    +VOLUME(XJ,YJ,(XJ+XI)/2.,(YJ+YI)/2.,XCENTR,YCENTR)
        SK=VOLUME(XK,YK,(XK+XI)/2.,(YK+YI)/2.,XCENTR,YCENTR)
     +    +VOLUME(XK,YK,(XK+XJ)/2.,(YK+YJ)/2.,XCENTR,YCENTR)
      ENDIF
      goto 901
c 
c***    6. obtuse triangle case (must calculate jhjd)
c
500   obct = obct+1
      if(tani.ge.0.0d0) goto 600
      hi=0. 
      hj=0.5d0*dj*tank
      hk=0.5d0*dk*tanj
      sj=0.25d0*dj*hj 
      sk=0.25d0*dk*hk 
      si=s-(sj+sk)
c.....Bugs:area assignment should be as follows
      HI=SJ
      SJ=SK
      SK=HI
      HI=0.
c.....Cylindrical coodination(volume)
      IF(LCYL) THEN
        tmp=2.0*(xij*xjk+yij*yjk)
        xcntr1=xjk*yij*yij+(xi+xj)*xij*xjk+2.*xj*yij*yjk
        xcntr1=xcntr1/tmp
        ycntr1=yjk*xij*xij+(yi+yj)*yij*yjk+2.*yj*xij*xjk
        ycntr1=ycntr1/tmp
        tmp=2.0*(xki*xjk+yki*yjk)
        xcntr2=xjk*yki*yki+(xi+xk)*xki*xjk+2.*xk*yki*yjk
        xcntr2=xcntr2/tmp
        ycntr2=yjk*xki*xki+(yi+yk)*yki*yjk+2.*yk*xki*xjk
        ycntr2=ycntr2/tmp
        tpirj=3.1415927*(xcntr1+0.5*(xi+xj))
        tpirk=3.1415927*(xcntr2+0.5*(xi+xk))
        SI=VOLUME(XI,YI,(XI+XJ)/2.,(YI+YJ)/2.,XCNTR1,YCNTR1)
     +    +VOLUME(XI,YI,(XI+XK)/2.,(YI+YK)/2.,XCNTR2,YCNTR2)
     +    +VOLUME(XI,YI,XCNTR1,YCNTR1,XCNTR2,YCNTR2)
        SJ=VOLUME(XJ,YJ,(XJ+XI)/2.,(YJ+YI)/2.,XCNTR1,YCNTR1)
        SK=VOLUME(XK,YK,(XK+XI)/2.,(YK+YI)/2.,XCNTR2,YCNTR2)
      ENDIF
      goto 900
c 
600   if(tanj.ge.0.0d0) goto 700
      hi=0.5d0*di*tank
      hj=0. 
      hk=0.5d0*dk*tani
      sk=0.25d0*dk*hk 
      si=0.25d0*di*hi 
      sj=s-(sk+si)
c.....Bugs:area assignment should be as follows
      HJ=SI
      SI=SK
      SK=HJ
      HJ=0.
c.....Cylindrical coodination(volume)
      IF(LCYL) THEN
        tmp=2.0*(xjk*xki+yjk*yki)
        xcntr1=xki*yjk*yjk+(xj+xk)*xjk*xki+2.*xk*yjk*yki
        xcntr1=xcntr1/tmp
        ycntr1=yki*xjk*xjk+(yj+yk)*yjk*yki+2.*yk*xjk*xki
        ycntr1=ycntr1/tmp
        tmp=2.0*(xij*xki+yij*yki)
        xcntr2=xki*yij*yij+(xj+xi)*xij*xki+2.*xi*yij*yki
        xcntr2=xcntr2/tmp
        ycntr2=yki*xij*xij+(yj+yi)*yij*yki+2.*yi*xij*xki
        ycntr2=ycntr2/tmp
        tpirk=3.1415927*(xcntr1+0.5*(xj+xk))
        tpiri=3.1415927*(xcntr2+0.5*(xj+xi))
        SJ=VOLUME(XJ,YJ,(XJ+XK)/2.,(YJ+YK)/2.,XCNTR1,YCNTR1)
     +    +VOLUME(XJ,YJ,(XJ+XI)/2.,(YJ+YI)/2.,XCNTR2,YCNTR2)
     +    +VOLUME(XJ,YJ,XCNTR1,YCNTR1,XCNTR2,YCNTR2)
        SK=VOLUME(XK,YK,(XK+XJ)/2.,(YK+YJ)/2.,XCNTR1,YCNTR1)
        SI=VOLUME(XI,YI,(XI+XJ)/2.,(YI+YJ)/2.,XCNTR2,YCNTR2)
      ENDIF
      goto 900
c 
700   if(tank.ge.0.0d0) call erset(63,linum,ie) 
      hi=0.5d0*di*tanj
      hj=0.5d0*dj*tani
      hk=0. 
      si=0.25d0*di*hi 
      sj=0.25d0*dj*hj 
      sk=s-(si+sj)
c.....Bugs:area assignment should be as follows
      HK=SJ
      SJ=SI
      SI=HK
      HK=0.
c.....Cylindrical coodination(volume)
      IF(LCYL) THEN
        tmp=2.0*(xki*xij+yki*yij)
        xcntr1=xij*yki*yki+(xk+xi)*xki*xij+2.*xi*yki*yij
        xcntr1=xcntr1/tmp
        ycntr1=yij*xki*xki+(yk+yi)*yki*yij+2.*yi*xki*xij
        ycntr1=ycntr1/tmp
        tmp=2.0*(xjk*xij+yjk*yij)
        xcntr2=xij*yjk*yjk+(xk+xj)*xjk*xij+2.*xj*yjk*yij
        xcntr2=xcntr2/tmp
        ycntr2=yij*xjk*xjk+(yk+yj)*yjk*yij+2.*yj*xjk*xij
        ycntr2=ycntr2/tmp
        tpiri=3.1415927*(xcntr1+0.5*(xk+xi))
        tpirj=3.1415927*(xcntr2+0.5*(xk+xj))
        SK=VOLUME(XK,YK,(XK+XI)/2.,(YK+YI)/2.,XCNTR1,YCNTR1)
     +    +VOLUME(XK,YK,(XK+XJ)/2.,(YK+YJ)/2.,XCNTR2,YCNTR2)
     +    +VOLUME(XK,YK,XCNTR1,YCNTR1,XCNTR2,YCNTR2)
        SI=VOLUME(XI,YI,(XI+XK)/2.,(YI+YK)/2.,XCNTR1,YCNTR1)
        SJ=VOLUME(XJ,YJ,(XJ+XK)/2.,(YJ+YK)/2.,XCNTR2,YCNTR2)
      ENDIF
c
900   jhjd(1,ie)=hi/di
      jhjd(2,ie)=hj/dj
      jhjd(3,ie)=hk/dk
c
c......Cylindrical coordination
      IF (LCYL) THEN
         JHJD(1,IE)=TPIRI*JHJD(1,IE)
         JHJD(2,IE)=TPIRJ*JHJD(2,IE)
         JHJD(3,IE)=TPIRK*JHJD(3,IE)
      ENDIF
c
c......Width parameter
      if (lwidth) then
         ehed(1,ie)=width*ehed(1,ie)
         ehed(2,ie)=width*ehed(2,ie)
         ehed(3,ie)=width*ehed(3,ie)
         jhjd(1,ie)=width*jhjd(1,ie)
         jhjd(2,ie)=width*jhjd(2,ie)
         jhjd(3,ie)=width*jhjd(3,ie)
         si = si*width
         sj = sj*width
         sk = sk*width
      ENDIF
c
c......Store area contributions
901   es(1,ie)=si 
      es(2,ie)=sj 
      es(3,ie)=sk 
c
c......Next element 
 1000 continue
c
c......Done
      obpct=float(obct)/float(ne)*100.
      write(luout,*) ' '
      write(luout,5000) np,ne,obct,obpct
 5000 format(/' Mesh statistics :'/
     +       4x,' Total grid points =',i5/
     +       4x,' Total no. of triangles =',i5/
     +       4x,' Obtuse triangles =',i5,'  (',f4.1,'%)'/)
      if (lwidth) then
         write(luout,5001) width
 5001    format(4x,' Width =',f6.1/)
      endif
 9999 return
      end
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE NODERG
      include 'p2conf.h'
c 
c   This routine determines the region number to be 
c   associated with each node in the grid. 
c   The convention is: 
c
c      region # of node = region # of element with highest 
c                         material type which contains the node.
c                 In the case of a tie, give to region of
c                 highest index, to be consistent.
c
c      This means that triangles with oxide nodes are oxide
c      triangles. Alternatively, a silicon triangle contains
c      only SILICON nodes. This is important!
c
c      If we are given node region numbers, overwrite them. They
c      must obey the convention!
c     
c     Original :  CSR     Feb 84
c
c     copyright c 1984 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c
c------------------------------------------------------------------
      integer node,ie,j,ielmat,elmat
c 
c****************************************************************** 
c 
c                   start 
c 
c...Initial assignment
      do 10 ie=1,ne 
         ielmat=imat(ie)
         elmat=mattyp(ielmat)
       do 10 j=1,3
          node = nop(j,ie)
          if (itype(node).eq.0) then
             itype(node)=ielmat
          else
             if (elmat.gt.mattyp(itype(node))) then
          itype(node)=ielmat
             else 
          if (elmat.eq.mattyp(itype(node)).and.
     +                ielmat.gt.itype(node) ) itype(node)=ielmat
             endif
          endif
10    continue
c
c...Bye
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CMETAL
      include 'p2conf.h'
c
c     date code 101483 CSR
c
c                   From geometrical data generated by geom, 
c                   extract the metal length associated with 
c                   each boundary node. 
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
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
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c                   local types
c
      integer  ie,ib,ilnode,jnode,jprime,jpp,tmod,ixlt,ix,ielec
      integer  in,j,k,i4
      real     frac
c
c******************** Start ********************************************
c
c..................Initialise 
c
      do 100 ib=1,nb
  100 lmetal(ib) = 0.0
c
c..................Scan electrode nodes
c 
      do 200 ib=1,nb
      ilnode=nbc(ib)
      ielec=ietype(ib)
c..................Use node->triangle list to search for neighbours
      ixlt =p2tc(ilnode)
      do 200 ix=1,ixlt
      ie=p2t(ix,ilnode)
c
      j=0
      do 110 k=1,3
      if (nop(k,ie).eq.ilnode) j=k
  110 continue
      do 198 in=1,2
      jprime=tmod(j+in)
      jpp=tmod(j-in)
      jnode=nop(jprime,ie)
c...................Add 0.5*triangle side (0.25 for shared sides)
c                   At this point, nop(j,ie)=metal node
c                   nop(jprime,ie)=candidate metal neighbour
c...................nextel(jpp,ie)=neighbour triangle on side j-jprime
      if (lm(jnode).eq.ielec) then
          if (nextel(jpp,ie).le.0) frac=0.5
          if (nextel(jpp,ie).gt.0) frac=0.25
          lmetal(ib) = lmetal(ib)+frac*tsides(jpp,ie)
      endif
 198  continue
 200  continue  
c
c..................Look before we leap 
      do 30 ib=1,nb
      if (lmetal(ib).ne.0.0e0) goto 30
      i4=nbc(ib)
      call erset(-268,linum,i4)
30    continue
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FINDE
      include 'p2conf.h'
c
c     Find electrode(s) connected via same doping polarity to each 
c     semiconductor node.  Code as follows -
c      
c     Let 
c       pelect            = 1+total no. of electrodes
c       ne(1), ..., ne(n) = electrode numbers contacting 
c                           doped region i (assuming n electrodes
c                           are connected to the region)
c     Then
c       eptr(j)  = doped region containing node j  
c       ecode(i) = ne(1) + pelect*ne(2) + ... + (pelect**(n-1))*ne(n)
c
c     Original : MRP       Stanford University       April, 1984
c     Revised  : CSR                                 Aug,   1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'adjstk.h'
      integer TMPPAD(1398002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i,ibc,eroot,itmp,j,istack,n1elec
      logical efnd(MAXCNT)
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize pointer arrays and doping polarity array
      n1elec = nelect+1
      ndreg=0
      do 10 i=1,np
10    eptr(i)=0
      do 20 i=1,ndop
20    ecode(i)=0
      do 25 i=1,nelect
25    dopsgn(i)=0.
c
c...Go through electrode nodes
      do 100 ibc=1,nb
      eroot=nbc(ibc)
      i=lm(eroot)
c
c...If this node has already been assigned a doping region or
c...it's an insulator contact, go to next electrode
      if((eptr(eroot).gt.0).or.(mattyp(itype(eroot)).le.0)) goto 100
c
c...We've found the root of a new doping region - mark it and get
c...dopant polarity
      ndreg=ndreg+1
      eptr(eroot)=ndreg
      if(dopsgn(i).eq.0.) then
         dopsgn(i)=sign(1.,r1(eroot))
      else if(dopsgn(i).ne.sign(1.,r1(eroot))) then
         call erset(-130,linum,i)
      endif
c
c...Initialize stack pointer and electrode-found array
      istack=0
      do 130 j=1,nelect
130   efnd(j)=.false.
      efnd(i)=.true.
c
c...Expand node
140   call expnod(eroot,dopsgn(i),efnd,istack)
      if(errflg) return
c
c...Check stack - if empty, we've exhausted this region
c...Otherwise, pop stack and go back
      if(istack.eq.0) goto 150
      eroot=nlist(istack)
      istack=istack-1
      goto 140
c
c...Set electrode code = sigma 1->nelect exist(j)*n1elec**(j-1)
  150 itmp=1
      do 200 j=1,nelect
        if(.not.efnd(j)) goto 200
        ecode(ndreg)=ecode(ndreg)+itmp*j
        itmp=itmp*n1elec
  200 continue
      
c
c...Next electrode node
  100 continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE EXPNOD(root,dpsgn,efnd,istack)
      include 'p2conf.h'
c
c     Checks all nodes adjacent to root to find those with similar
c     doping (as indicated by dpsgn).  Marks all candidates as 
c     found and places them on stack for later expansion.
c
c     Original : MRP         Stanford University       April, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'adjstk.h'
      integer TMPPAD(1398002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer root,i,j,newpt,istack,lmn
      logical efnd(*)
      real dpsgn
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Go through all adjacent nodes looking for ones that aren't assigned
c...and are semicondutor nodes
      do 10 i=1,p2tc(root)
      do 10 j=1,3
      newpt=nop(j,p2t(i,root))
      if((eptr(newpt).ne.0).or.(mattyp(itype(newpt)).le.0)) goto 10
c
c...Found a new node - mark it if doping is the same.
c...If it's a new electrode with same doping, label it as found.
c...If it's an electrode that has been located in this region before,
c...but has a different dopant type from predecessor - warning!
      lmn=lm(newpt)
      if(sign(1.,r1(newpt)).eq.dpsgn) then
         if(lmn.gt.0) efnd(lmn)=.true.
         eptr(newpt)=ndreg
         istack=istack+1
         nlist(istack)=newpt
      else
         if(lmn.gt.0) then
             if (efnd(lmn)) call erset(-130,linum,lmn)
         endif
      endif
c
c...Go on
10    continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION BDNODE(inode)
      include 'p2conf.h'
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c------------------------------------------------------------------
      integer inode
c------------------------------------------------------------------
c
      bdnode=1
10    if(nbc(bdnode).eq.inode) return
      bdnode=bdnode+1
      goto 10
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NODEA(inode,sarea,iarea)
      include 'p2conf.h'
c
c     Get semiconductor/insulator area associated with node inode.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer inode,n,i1,elmat,nmati,in
      real    sarea,iarea
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      sarea=0.
      iarea=0.
c
c...Find each element containing inode
      do 100 in=1,p2tc(inode)
      n=p2t(in,inode)
      nmati=imat(n)
      elmat=mattyp(nmati)
c
c...Find node
      if(inode.eq.nop(1,n)) then
         i1=1
      else if(inode.eq.nop(2,n)) then
         i1=2
      else
         i1=3
      endif
c
c...Get area component
      if(elmat.lt.0) then
         iarea=es(i1,n)+iarea
      else
         sarea=es(i1,n)+sarea
      endif
c
c...Next element
100   continue
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IREGM(fname)
      include 'p2conf.h'
       character*(*) fname
      include 'blank.h'
      errflg=.true.
      write(*,11) fname
11    format(' ERROR: called IREGM(',a,') but it is not implemented')
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DUMPNX
      return
      end
