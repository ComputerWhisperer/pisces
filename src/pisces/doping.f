cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Wed Sep 20 22:38:32 PDT 1989 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE DOPCK(nregio,conc,xdev,ydev,xleft,xright,ytop,ybot,
     +                 lunif,lxerfc,lxdir,lsuprm,nsup,doutfl,ldsave,
     +                 lbidop,lerfdp,lgauss,ls4geo)
      include 'p2conf.h'
c 
c     Doping card check routine.
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
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
      include     'setup.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lunif,laccep,ldonor,lxerfc,lxdir,lsuprm,ldsave,
     + lascfl,lsupn,l1dfil,lerfdp,lgauss
      logical lbidop
      logical ls4geo
      integer nregio(*),nrkey,i,nnr,mmod,imptyp,nsup,itemp,ierr
      integer nls
      integer s3type
      real conc,xdev,ydev,xleft,xright,ytop,ybot,dose,yjunc,xyrat,
     +     sqrtpi,dop,gtdop,tmp,slice,dxmin,dxmax,dymin,dymax,
     +     jconc,fx,dfx,expu,abs,erfc
      character*20 doutfl
      character*20 tmpbuf
c
c FUNCTIONS:
      logical iscval, isrval
      logical gtlval
      real    gtrval
c 
c****************************************************************** 
c 
c                   data
c 
      data sqrtpi/1.77245/
c 
c******************* Start **************************************** 
c 
c
c...Check for output file - if not first doping card, warn user
c...that we missed something
      if(.not.iscval(3)) goto 2
      if(ldopcd) call erset(-99,linum,0)
c
cC mje: when did doutfl get set?????
      if(doutfl(1:1).ne.' ') call erset(-79,linum,0)
      ldsave=.true.
      call gtcval(3, doutfl, LEN(doutfl))
      call fopcl(11,doutfl,20,lutmp,.false.,ierr)
c
2     lunif=.false.
      ldopcd=.true. 
      lxerfc=.false.
      lxdir=.false.
c
c...Check for specific region - if none thrn set to all
      nrkey=gtrval(11)
      if (isrval(11)) goto 1
      do 3 i=1,nmat
      nregio(i)=1
      if(mattyp(i).lt.0) nregio(i)=0
3     continue
      goto 11

1     if (nrkey.le.0) call erset(241,linum,nrkey)
      if (errflg) return
      do 5 i=1,nmat
5     nregio(i)=0

      nnr=1+int(alog10(real(nrkey)))
      do 10 i=1,nnr
         itemp=mmod(nrkey,10)
       nregio(itemp)=1
       nrkey=nrkey/10
         if (itemp.lt.1.or.itemp.gt.nmat) 
     +            call erset(-241,linum,nregio(i))
10    continue
      if (errflg) return
c 
c...Get species type
11    laccep=gtlval(3)
      ldonor=gtlval(4)
      if(gtlval(10)) then
         laccep=.true.
         imptyp=1
      else if(gtlval(11)) then
         ldonor=.true.
         imptyp=2
      else if(gtlval(12)) then
         ldonor=.true.
         imptyp=3
      else if(gtlval(13)) then
         ldonor=.true.
         imptyp=4
      endif
c
c mje: just count to see if there are too many.
      nls = 0
      do 234 i = 10, 13
234   if (gtlval(i)) nls = nls + 1
c
      if (nls.gt.1) call erset(283, linum, 0)
c
c...Profile type
      lbidop=gtlval(14)
      lsuprm=gtlval(9).or.gtlval(16)
      lsupn=gtlval(16)
      lascfl=gtlval(15)
      lgauss=gtlval(1)
      lunif=gtlval(2)
      lerfdp=gtlval(17)
      ls4geo=gtlval(18)
      l1dfil=lsuprm.or.lascfl
c...check for cases where two file types are specified
      if((lbidop.and.lsuprm).or.(lbidop.and.lascfl).or.
     +   (ls4geo.and.lsuprm).or.(ls4geo.and.lbidop).or.
     +   (ls4geo.and.lascfl).or.
     +   (lsuprm.and..not.lsupn.and.lascfl)) 
     +          call erset(192,linum,0)
      if(errflg) return

c....Types of 1d profiles:
c.. 1 - Old suprem3 structure files.
c.. 2 - suprem3 binary export files.
c.. 3 - suprem3 ascii  export files.
c.. 4 - simple ascii text files.
      s3type = 4
      if (lsuprm) then
          s3type = 3
          if (.not.lsupn)  s3type = 1
          if (.not.lascfl) s3type = 2
      endif
      if (lsuprm.and.lascfl) lascfl = .false.

c
c...Impurity type should not be specified for 2d or ascii doping
      if(lbidop.or.lascfl.or.ls4geo) then
         if(laccep.or.ldonor) call erset(-189,linum,0)
         ldonor=.true.
         laccep=.false.
      endif
c
c...Should now be n-type or p-type, but not both
      if (laccep.and.ldonor.or..not.(laccep.or.ldonor)) 
     +   call erset(26,linum,0) 
      if(errflg) return
c
c...Must specify actual impurity for SUPREM.
c...Get functional form for analytical profile.
      if(l1dfil) then
         if(lsuprm .and. nls.eq.0) call erset(284,linum,0)
      else if((.not.lbidop).and.(.not.ls4geo)) then
         if (.not.((lunif.and.(.not.lgauss).and.(.not.lerfdp))
     +           .or.((.not.lunif).and.lgauss.and.(.not.lerfdp))
     +           .or.((.not.lunif).and.(.not.lgauss).and.lerfdp)))
     +       call erset(21,linum,0)
      endif
      if(errflg) return
c 
c...Get origins and direction
      lxdir=.false.
      if (iscval(1)) then
          call gtcval(1, tmpbuf, LEN(tmpbuf))
          if((tmpbuf(1:1).eq.'x').or.(tmpbuf(1:1).eq.'X')) lxdir=.true.
      endif
      xleft=gtrval(4) 
      xright=gtrval(8)
      ytop=gtrval(10) 
      ybot=gtrval(5)
c
c...If bottom of box is not defined yet, try peak/start parameter
      if(.not.isrval(5)) ybot=gtrval(13)
c 
c...Set origins 
      call devlts(dxmin,dxmax,dymin,dymax)
      if(xleft.ne.-999.) xleft=xleft*1e-4
      if(xleft.eq.-999.) xleft=dxmin-10.0*dxmin
      if(xright.ne.-999.) xright=xright*1e-4 
      if(xright.eq.-999.) xright=dxmax + 10*dxmax
      if(ybot.ne.-999.) ybot=ybot*1e-4 
      if(ybot.eq.-999..and..not.lunif) ybot=0. 
      if(ybot.eq.-999..and.lunif) ybot=1e4 
      if(ytop.ne.-999.) ytop=ytop*1e-4
      if(ytop.eq.-999..and..not.lunif) ytop=ybot 
      if(ytop.eq.-999..and.lunif) ytop=-1e4
      if(xleft.gt.xright.or.ytop.gt.ybot) call erset(121,linum,0)
c
c...If profile in x direction, interchange left/right 
c   with top/bottom (the convention used internally is 
c   that the profile direction is always y and the 
c...lateral direction is x)
      if(lxdir) then
         tmp=xleft
         xleft=ytop
         ytop=tmp
         tmp=xright
         xright=ybot
         ybot=tmp
      endif
c
c...Write to file?
      if(ldsave) write(lutmp) (nregio(i),i=1,nmat),lunif,l1dfil,
     +                        lbidop,xleft,xright,ytop,ybot,lxdir,
     +                        .false.,lgauss,lerfdp,ls4geo
c
c...Check profile type
      if(l1dfil) goto 3000
      if(lbidop) goto 4000
      if(ls4geo) goto 5000
c
c--------------------
c  ANALYTIC PROFILE
c--------------------
c 
c...Get conc (and scale if scaling in force)
      conc=gtrval(1)
      if((conc.lt.0..and.conc.ne.-999.).or.(conc.eq.-999..and.lunif))
     +    call erset(20,linum,0)
      if(errflg) goto 9999
c 
c
c---------
c UNIFORM
c---------
c
      if(.not.lunif) goto 500
c
c...Trap zero and set polarity
      xdev=1.0
      ydev=1.0
      if (laccep) conc=-conc
c
c...Writing to a file?
      if(ldsave) write(lutmp) conc,ydev,xdev
      goto 9999
c
c----------
c GAUSSIAN
c----------
c 
c...Get x,y characteristic length 
500   if(.not.lgauss) goto 600
      ydev=gtrval(2)
      xdev=gtrval(3)
c
c...Examine y characteristic length
      if(.not.isrval(2)) goto 50
      if(ydev.lt.0.) goto 40 
      ydev=ydev*1e-4
      goto 100
c
c...Oops, neg. char. length not allowed
   40 call erset(111,linum,0) 
      goto 9999 
c
c...ydev not specified, is it needed? 
   50 if(conc.ne.-999.) goto 75
      call erset(112,linum,0) 
      goto 9999 
c
c...Get ydev from yjunc and get any point in box
75    if(.not.isrval(7)) call erset(113,linum,0) 
      yjunc=gtrval(7) 
      yjunc=yjunc*1e-4
c
c...Junction is an absolute location. It must can be greater than
c...ybot or less than ytop.
      tmp=yjunc-ybot
      if(tmp.gt.0.) goto 76
      tmp=ytop-yjunc
      if(tmp.gt.0.) goto 76
      if(lxdir) then
         call erset(134,linum,0) 
      else
         call erset(114,linum,0) 
      endif
      goto 9999 
c
c...Along what slice are we looking for junction?
76    if (.not.isrval(12)) then
         slice=(xleft+xright)*0.5
      else 
         slice=gtrval(12)
      endif
      dop=gtdop(yjunc,slice,lxdir) 
      if(errflg) goto 9999 
c
c...Can we even find a junction?
      if(laccep) then
         if(sign(1.,conc).ne.sign(1.,dop)) call erset(110,linum,0)
      else
         if(sign(1.,conc).eq.sign(1.,dop)) call erset(110,linum,0)
      endif
      if(errflg) goto 9999 
      if(abs(dop).gt.abs(conc)) call erset(131,linum,0)
      if(errflg) goto 9999 
c
c...Now convert junction depth into char. length
      ydev=tmp/sqrt(alog(abs(conc/dop)))
c 
c...Do we need to look at dose? 
c...If so, compute conc from dose and ydev 
  100 if(conc.eq.-999.) then 
       dose=gtrval(6)
       if(dose.le.0.) then 
          call erset(115,linum,0) 
          goto 9999 
       else
          conc=dose/(ydev*sqrtpi) 
       endif
      endif
c
c .......... Either way, we must have conc by now.
      if (laccep) conc=-conc

c 
c...Examine x char. length
      if(xdev.ge.0.) goto 300
      if(xdev.eq.-999.) goto 250 
      call erset(111,linum,0) 
      goto 9999 
c
c...Not specified get it from ydev
  250 xyrat=gtrval(9) 
      if(xyrat.ge.0.) goto 260 
      call erset(116,linum,0) 
      goto 9999 
  260 xdev=xyrat*ydev 
      goto 310
c 
c...Convert to centimeters 
  300 xdev=xdev*1e-4
c 
c...See if x direction is error function
  310 lxerfc=gtlval(5)
c
c...Write to file?
      if(ldsave) write(lutmp) conc,ydev,xdev,lxerfc
      goto 9999
c
c
c----------
c ERFC
c----------
c 
c...Get x,y characteristic length 
600   ydev=gtrval(2)
      xdev=gtrval(3)
c
c...Examine y characteristic length
      if(.not.isrval(2)) goto 350
      if(ydev.lt.0.) goto 340 
      ydev=ydev*1e-4
      goto 400
c
c...Oops, neg. char. length not allowed
  340 call erset(111,linum,0) 
      goto 9999 
c
c...ydev not specified, is it needed? 
  350 if(conc.ne.-999.) goto 375
      call erset(112,linum,0) 
      goto 9999 
c
c...Get ydev from yjunc and get any point in box
 375  if(.not.isrval(7)) call erset(113,linum,0) 
      yjunc=gtrval(7) 
      yjunc=yjunc*1e-4
c
c...Junction is an absolute location. It must can be greater than
c...ybot or less than ytop.
      tmp=yjunc-ybot
      if(tmp.gt.0.) goto 376
      tmp=ytop-yjunc
      if(tmp.gt.0.) goto 376
      if(lxdir) then
         call erset(134,linum,0) 
      else
         call erset(114,linum,0) 
      endif
      goto 9999 
c
c...Along what slice are we looking for junction?
 376  if (.not.isrval(12)) then
         slice=(xleft+xright)*0.5
      else 
         slice=gtrval(12)
      endif
      dop=gtdop(yjunc,slice,lxdir) 
      if(errflg) goto 9999 
c
c...Can we even find a junction?
      if(laccep) then
         if(sign(1.,conc).ne.sign(1.,dop)) call erset(110,linum,0)
      else
         if(sign(1.,conc).eq.sign(1.,dop)) call erset(110,linum,0)
      endif
      if(errflg) goto 9999 
      if(abs(dop).gt.abs(conc)) call erset(131,linum,0)
      if(errflg) goto 9999 
c
c...Now convert junction depth into char. length
      jconc=gtrval(14)
c  note : tmp = yjunc-ybot (defined above)
c
      if (.not.isrval(14)) then
          ydev=tmp/1.82
      else
c...    do a newton loop
        ydev=tmp
 380    fx=jconc/conc-erfc(tmp/ydev)
        dfx=-tmp*1.128379e-4/ydev/ydev*expu(-(tmp*tmp/ydev/ydev))
        if (abs(fx*1.0e-4/dfx).gt.ydev) then
            fx=-ydev*dfx
        endif
        ydev=abs(ydev-1.0e-4*fx/dfx)
        if (abs(fx/dfx).gt.1.0e-3) goto 380
      endif
c 
c .......... Either way, we must have conc by now.
 400  if (laccep) conc=-conc

c 
c...Examine x char. length
      if(xdev.ge.0.) goto 700
      if(xdev.eq.-999.) goto 550 
      call erset(111,linum,0) 
      goto 9999 
c
c...Not specified get it from ydev
  550 xyrat=gtrval(9) 
      if(xyrat.ge.0.) goto 560 
      call erset(116,linum,0) 
      goto 9999 
  560 xdev=xyrat*ydev 
      goto 610
c 
c...Convert to centimeters 
  700 xdev=xdev*1e-4
c 
c...See if x direction is error function
  610 lxerfc=gtlval(5)
c
c...Write to file?
      if(ldsave) write(lutmp) conc,ydev,xdev,lxerfc
      goto 9999
c
c---------------------------------
c  SUPREM-III (OR ASCII) PROFILE
c---------------------------------
c
c...Read profile corresponding to impurity index imptyp from 
c...file (name in cval(2)) - nsup is no. of nodes in SUPREM
c...simulation
3000  call gtcval(2, tmpbuf, LEN(tmpbuf))
      call s3read(s3type,tmpbuf,imptyp,nsup)
c
c...Now, if ascii, fool into thinking data actually came 
c...from SUPREM-III
      lsuprm=.true.
c
c...Set lateral diffusion ratio, dopant ion polarity, and scaling
      xdev=1./gtrval(9)
      conc=1.
      if(laccep) conc=-conc
c
c...Save doping?
      if(ldsave) write(lutmp) conc,xdev,nsup,
     +                        (ysup(i),consup(i),i=1,nsup)
      goto 9999
c
c------------
c  2D INPUT
c------------
c
c...Read doping info into arrays in doptmp
4000  continue
      call gtcval(2, tmpbuf, LEN(tmpbuf))
      call fopcl(2,tmpbuf,20,lusup,.false.,ierr)
      if (errflg) return
      call read2d(lusup,ldsave,.false.)
coldbi      call red2df(lu,ldsave)
      call fopcl(0,tmpbuf,20,lusup,.false.,ierr)
      write(luout,4001) tmpbuf,' '
4001  format(//' 2D doping read from ',a20/a1)
      goto 9999

c

c... SUPREM-IV geometry file
5000  continue
c...Read profile corresponding from file (name in cval(2)) 
      call gtcval(2, tmpbuf, LEN(tmpbuf))

c...Save doping?
      if(ldsave) write(lutmp) tmpbuf

      call s4read(tmpbuf,nregio)
c...Do the rest somewhere else
9999  return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE DOPNG(nregio,conc,xdev,ydev,xleft,xright,ytop,ybot,
     +                 lunif,lxerfc,lxdir,lsuprm,nsup,lbidop,lerfdp,
     +                 lgauss,ls4geo)
      include 'p2conf.h'
c 
c     This routine calculates the doping levels for gaussian
c     or uniform doping profiles
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lxerfc,lunif,lxdir,lsuprm,lbidop,lerfdp,lgauss
      logical ls4geo
      integer nregio(*),k,jj,nsup,nend
      real conc,xdev,ydev,xleft,xright,ytop,ybot,conc1,cpt1,cpt2
      real xp,yp,argl,argr,argt,argb,fx,fy,erfc,ctmp,dintrp
      real expu
c 
c****************************************************************** 
c 
c                   scan nodes in region
c
c...for a suprem4 geometry file we ignore this routine since
c      the internal arrays have been filled in s4read. 
      if (ls4geo) goto 9999
      if(lbidop) nend=npbi-nybi+1
      do 500 k=1,np
      if(nregio(itype(k)).eq.0) goto 500
c 
c...Get node coord. 
      xp=cord(1,k)
      yp=cord(2,k)
c
c...Interchange x,y if profile in x direction
      if(lxdir) then
         xp=yp
         yp=cord(1,k)
      endif
c
c...Check profile type
      if(lsuprm) goto 3000
      if(lbidop) goto 4000
c
c----------------------
c  ANALYTICAL PROFILE
c----------------------
c
      argl=(xp-xleft)/xdev
      argr=(xp-xright)/xdev 
      argt=(yp-ytop)/ydev 
      argb=(yp-ybot)/ydev 
      fx=0. 
      fy=0. 
c 
c...X function
c...Error funct.? 
      if (.not.lxerfc) goto 100 
      fx=(erfc(argr)-erfc(argl))*.5 
      goto 200
c 
c...Get relative x position 
100   if (xp.lt.xleft) goto 130 
      if (xp.gt.xright) goto 160
c 
c...Middle region, fx=1 regardless
      fx=1. 
      goto 200
c 
c...Left region. if uniform, already set to 0 
130   if (lunif) goto 200 
      fx=expu(-argl*argl)
      goto 200
c 
c...Right region
160   if (lunif) goto 200 
      fx=expu(-argr*argr)
c 
c...Get y function
200   if (yp.lt.ytop) goto 230
      if (yp.gt.ybot) goto 260
c 
c...Middle region fy=1. regardless
      fy=1. 
      goto 300
c 
c...Top region. if uniform, already set to 0. 
230   if (lunif) goto 300 
      if (lgauss) then
          fy=expu(-argt*argt)
      else
          fy=erfc(argt)
      endif
      goto 300
c 
c...Bottom region 
260   if (lunif) goto 300 
      if (lgauss) then
          fy=expu(-argb*argb)
      else
          fy=erfc(argb)
      endif
c 
c...Compute contribution and move on
300   continue
      conc1=conc*fx*fy
      goto 400
c----------------------
c  SUPREM-III PROFILE
c----------------------
c
c...Where are we?
3000  if(xp.lt.xleft) goto 3010
      if(xp.le.xright) goto 3020
c
c...To the right/left of box - normalize depth
      fx=(xp-xright)*xdev
      goto 3015
3010  fx=(xleft-xp)*xdev
3015  fy=amax1(yp-ybot,ytop-yp,0.)
      yp=sqrt(fx*fx+fy*fy)
      goto 3025
c
c...Find concentration at depth yp by interpolation
3020  yp=amax1(yp-ybot,ytop-yp)
3025  conc1=consup(nsup)
      jj=1
3030  if(jj.ge.nsup) goto 3050
      if(yp.le.ysup(jj)) goto 3040
      jj=jj+1
      goto 3030
3040  if(jj.eq.1) then
         conc1=consup(jj)
      else
         cpt1=consup(jj-1)
         cpt2=consup(jj)
         if((cpt1.gt.0.).and.(cpt2.gt.0.)) then
            ctmp=alog(cpt1)
            conc1=ctmp+(alog(cpt2)-ctmp)*
     +                 (yp-ysup(jj-1))/(ysup(jj)-ysup(jj-1))
            conc1=expu(conc1)
         else
            conc1=cpt1+(cpt2-cpt1)*
     +                 (yp-ysup(jj-1))/(ysup(jj)-ysup(jj-1))
         endif
      endif
3050  conc1=conc*conc1
      goto 400
c
c-------
c  2D
c-------
c
c...Interpolate on 2D grid
4000  conc1=dintrp(xp,yp,nend)
c
c---------
c  STORE
c---------
c
c...Sum into net and total arrays 
400   r1(k)=r1(k)+conc1 
      tconc(k)=tconc(k)+abs(conc1)
c 
c...Next node in region 
500   continue
c 
c...Done
9999  return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      REAL FUNCTION GTDOP(xyloc,xloc,lxdir)
      include 'p2conf.h'
c 
c     This routine obtains the value of the current doping
c     level at a given x,y location.
c         xloc= [+-]10000 represent x=extreme left/right
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
Cmje:c
Cmje:c....the magic TMP common memory overlays ....
Cmje:#include     "adjtmp.h"
Cmje:#include     "doptmp.h"
Cmje:      integer TMPxPAD(1282989)
Cmje:      common/tmpco/ TMPPAD
Cmje:c....NOTE that the above padding MUST be here for the overlays
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lfound,lxdir
      integer ie,mater1,ip,icord
      real xyloc,dop1,dop2,dop3,k1,k2,k3,xloc,toler,yloc
      real sg,dtemp,xmin,xmax,xp
      real expu
      data toler/-1.0e-06/
c 
c******************* Start **************************************** 
c 
      if(lxdir) then
         icord=2
      else 
         icord=1
      endif
c
c...Get bounds/defaults
      xmin = 1.e6
      xmax =-1.e6
      do 50 ip=1,np
      xp = cord(icord,ip)
      if (xp.gt.xmax) xmax=xp
      if (xp.lt.xmin) xmin=xp
50    continue
      if (xloc.lt.xmin) xloc=xmin
      if (xloc.gt.xmax) xloc=xmax
c
c.......If x direction profile, swap xloc and yloc 
c.......(xloc and yloc must be real x,y coordinates)
      yloc = xyloc
      if (lxdir) then
       yloc = xloc
       xloc = xyloc
      endif

c
c.......find an element containing (xloc,yloc).
c          (k1,k2,k3) are all >0 and <1 for (x,y) in the
c.......triangle, so no need to scale the tolerance
c 
      lfound=.false.
      do 100 ie=1,ne
      call baryc(xloc,yloc,ie,k1,k2,k3)
      if(k1.lt.toler.or.k2.lt.toler.or.k3.lt.toler) goto 100
      lfound=.true.
      goto 101
  100 continue
  101 continue
c
c                   if not found or oxide, complain.
c
      mater1 = mattyp( imat(ie) )
      if (.not.lfound.or.mater1.lt.0) then 
          call erset(122,linum,0)
          return
      endif
c 
c                   get adjacent dopings, check for 0 or junction.
c
      dop1=r1(nop(1,ie)) 
      dop2=r1(nop(2,ie)) 
      dop3=r1(nop(3,ie))
      if (dop1.eq.0..or.dop2.eq.0..or.dop3.eq.0) call erset(124,linum,0) 
      if (errflg) goto 9999
      if (sign(1.,dop1).eq.sign(1.,dop2).and.
     +    sign(1.,dop2).eq.sign(1.,dop3) ) goto 400 
c 
c                   junction already in element
      call erset(123,linum,0) 
      goto 9999 
c 
c                   interpolate 
  400 sg=sign(1.,dop1)
      dop1 = abs(dop1)
      dop2 = abs(dop2)
      dop3 = abs(dop3)
      dtemp= k1*alog(dop1) + k2*alog(dop2) + k3*alog(dop3)
      gtdop= expu(dtemp)*sg
c 
c                   done
9999  return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION ERFC(x) 
        real    x 
c     date code: july 07, 1980
c---------------------------------------------------------------------
c 
c     erfc   : calculates the complementary error function of the 
c              argument x.
c 
c     Original : james a. greenfield                         july 1980
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c     local variables 
c 
c---------------------------------------------------------------------
      integer i
      real    y,tmp,cofer(7)
c 
      data cofer/.430638e-4, .2765673e-3, .1520143e-3,
     +           .9270527e-2, .4228201e-1, .7052308e-1, 1./ 
c---------------------------------------------------------------------
c 
c     start of erfc 
c 
c---------------------------------------------------------------------
      erfc=0. 
      y=abs(x)
      if(y.gt.5.) goto 20 
      tmp=cofer(1)
      do 10 i=2,7 
10    tmp=tmp*y + cofer(i)
      erfc=1./(tmp**16) 
20    if(x.lt.0.) erfc=2.-erfc
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE REDOPE(doutfl)
      include 'p2conf.h'
c
c     The following subroutine redopes a mesh using a doping 
c     specification stored in a data file (doutfl).
c
c     Original: MRP      Stanford University        Mar,1984
c
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
      include     'logunt.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lunif,lxerfc,lxdir,lsuprm,lbidop,lold,lgauss, lerfdp
      logical ls4geo
      integer nregio(MAXREG),i,nsup,ierr
      real conc,xdev,ydev,xleft,xright,ytop,ybot
      character*20 doutfl,dinfl
c
c*************
c**  START  **
c*************
c
c...Open/rewind file
      call fopcl(1,doutfl,20,lutmp,.false.,ierr)
c
c...Loop through doping cards
10    read(lutmp,err=9998,end=100) (nregio(i),i=1,nmat),lunif,lsuprm,
     +                             lbidop,xleft,xright,ytop,ybot,lxdir,
     +                             lold,lgauss,lerfdp,ls4geo
      lxerfc=.false.
      lerfdp=.false.
c
c...Qss used to be here - warn user about it
      if(lold) then
         call erset(300,linum,0)
         return
      endif

c...Check profile type
      if(lsuprm) goto 30
      if(lbidop) goto 40
      if(ls4geo) goto 50
c
c
c...Uniform?
      if(.not.lunif) goto 20
      read(lutmp,err=9998,end=9998) conc,ydev,xdev
      goto 70
c
c...Gaussian or Erfc?
20    read(lutmp,err=9998,end=9998) conc,ydev,xdev,lxerfc
      goto 70
c
c...SUPREM-III
30    read(lutmp,err=9998,end=9998) conc,xdev,nsup,
     +                              (ysup(i),consup(i),i=1,nsup)
      goto 70
c
c...2D rectangular
40    call read2d(lutmp,.false.,.true.)
      goto 70

c...SUPREM-IV Geometry File
50    read(lutmp,err=9998,end=9998) dinfl
      call s4read(dinfl,nregio)
c
c...Process data
70    call dopng(nregio,conc,xdev,ydev,xleft,xright,ytop,ybot,
     +           lunif,lxerfc,lxdir,lsuprm,nsup,lbidop,
     +           lerfdp,lgauss,ls4geo)

      goto 10
c
c...Sorry, we got an error
9998  call erset(155,linum,0)
      goto 9999
 
c...Close file 
100   call fopcl(0,doutfl,20,lutmp,.false.,ierr)
 
9999  return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     
      SUBROUTINE INITDP
      include 'p2conf.h'
c
c     Initialize doping arrays
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'emaco.h'
c
c----------------------------------------------------------------------
      integer i
c----------------------------------------------------------------------
c
      do 110 i=1,np
         r1(i)=0.0    
         tconc(i)=0.0
  110 continue
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RED2DF(lu,ldsave)
      include 'p2conf.h'
c
c     Read 2D doping profile and store in temporary arrays for
c     interpolation routine.  2D file has already been 
c     pre-processed to look like a PISCES mesh file (with one
c     exception - no integer  ).
c
c        MRP     July 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'logunt.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c-----------------------------------------------------------------------
      character*60 itit1
      integer idum,lu,nebi,nbbi,nelbi,nmatbi,i,ndbi,j,idum2
c      integer   idum2
      real rdum
      logical ldsave
c-----------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      read(lu,err=992,end=993) 
     +             nxbi,nybi,npbi,nebi,nbbi,itit1,nelbi,nmatbi,idum
      read(lu,end=993,err=992) 
     +         (xbi(i),ybi(i),r1bi(i),
     +          rdum,itbi(i),idum2,rdum,rdum,idum2,i=1,npbi)
c
      read(lu,end=993,err=992) 
     +         (idum2,idum2,idum2,(rdum,i=1,9),idum2,j=1,nebi)
      read(lu,end=993,err=992) (idum2,idum2,rdum,i=1,nbbi)
      read(lu,end=993,err=992) ndbi,(rdum,idum,i=1,ndbi)
      read(lu,end=993,err=992) (matbi(i),i=1,nmatbi)
c
      if(.not.ldsave) return
      write(lutmp) nxbi,nybi,npbi,nmatbi
      write(lutmp) (xbi(i),ybi(i),r1bi(i),itbi(i),i=1,npbi)
      write(lutmp) (matbi(i),i=1,nmatbi)
      return
c
  992 call erset(5,linum,0)
      return
  993 call erset(7,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE READ2D(lu,ldsave,ldload)
      include 'p2conf.h'
c
c     Read 2D doping profile and store in temporary arrays for
c     interpolation routine.  
c
c        MRP     Jan 1985
c        GC      Sep 1990
c            modified interface for SIMPL-2 instead of BICEPS
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'logunt.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c-----------------------------------------------------------------------
      integer lu,i,j,jj
      real xval,yval
      logical ldsave,ldload
c-----------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      matbi(1)=1
      if(ldload) then
         read(lu,end=993,err=992) nxbi,nybi
         npbi=nxbi*nybi
         do 10,i=1,nxbi
           read(lu,end=993,err=992) xval
           do 15,j=1,nybi
             jj=(i-1)*nybi+j
             xbi(jj)=xval
             itbi(jj)=1
 15        continue
 10      continue
         do 20,i=1,nybi
           read(lu,end=993,err=992) yval
           do 25,j=1,nxbi
             ybi((j-1)*nybi+i)=yval
 25        continue
 20      continue
c  data ordered in columns
         read(lu,end=993,err=992) (r1bi(j),j=1,npbi)
      else
         read(lu,*,end=993,err=992) nxbi,nybi
         if (ldsave) write(lutmp) nxbi,nybi
         npbi=nxbi*nybi
         do 30,i=1,nxbi
           read(lu,*,end=993,err=992) xval
           if (ldsave) write(lutmp) xval
           do 35,j=1,nybi
             jj=(i-1)*nybi+j
             xbi(jj)=xval
             itbi(jj)=1
 35        continue
 30      continue
         do 40,i=1,nybi
           read(lu,*,end=993,err=992) yval
           if (ldsave) write(lutmp) yval
           do 45,j=1,nxbi
             ybi((j-1)*nybi+i)=yval
 45        continue
 40      continue
c  data ordered in columns
         read(lu,*,end=993,err=992) (r1bi(j),j=1,npbi)
         if (ldsave) write(lutmp) (r1bi(j),j=1,npbi)
      endif
      return
c
  992 call erset(5,linum,0)
      return
  993 call erset(7,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION DINTRP(xp,yp,nend)
      include 'p2conf.h'
c
c     Interpolate doping at grid point (xp,yp) using 2D
c     grid/doping.
c
c        MRP     July 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'doptmp.h'
      integer TMPPAD(1282989)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c-----------------------------------------------------------------------
c
      logical loxlo1,loxlo2,loxhi1,loxhi2
      real xp,yp,del,deli,dopexp,r1sgn,delmin,deltot,dist
      integer ixhi,ixlo,ilo1,ilo2,ihi1,ihi2,ixhi1,ixlo1,nend
      real expu
c-----------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Find box containing xp,yp
      call bsrch(xp,xbi,1,nend,nybi,ixlo,ixhi)
      ixlo1=ixlo+nybi-1
      ixhi1=ixhi+nybi-1
      call bsrch(yp,ybi,ixlo,ixlo1,1,ilo1,ilo2)
      call bsrch(yp,ybi,ixhi,ixhi1,1,ihi1,ihi2)
c
c...If we only have one point, use it and terminate
      if(.not.((ilo1.eq.ilo2).and.(ilo1.eq.ihi1).and.
     +         (ilo1.eq.ihi2))) goto 1
      dintrp=r1bi(ilo1)
      return
c
c...Are any oxide nodes?
1     loxlo1=matbi(itbi(ilo1)).le.0
      loxlo2=matbi(itbi(ilo2)).le.0
      loxhi1=matbi(itbi(ihi1)).le.0
      loxhi2=matbi(itbi(ihi2)).le.0
      if(.not.(loxlo1.and.loxlo2.and.loxhi1.and.loxhi2)) goto 10
      dintrp=0.
      return
c
c...Get interpolating coefficients
c...Interpolate the LOG of doping, using the sign of the node
c...closest to (xp,yp) - do not include oxide nodes
10    delmin=1.e20
      deltot=0.
      if(loxlo1) then
         dopexp=0.
      else
         del=dist(xp,yp,xbi(ilo1),ybi(ilo1))
         deli=1./del
         dopexp=alog(abs(r1bi(ilo1)))*deli
         deltot=deli
         delmin=del
         r1sgn=sign(1.,r1bi(ilo1))
      endif
c
      if(.not.loxlo2) then
         del=dist(xp,yp,xbi(ilo2),ybi(ilo2))
         deli=1./del
         dopexp=dopexp+alog(abs(r1bi(ilo2)))*deli
         deltot=deltot+deli
         if(del.lt.delmin) then
            delmin=del
            r1sgn=sign(1.,r1bi(ilo2))
         endif
      endif
c
      if(.not.loxhi1) then
         del=dist(xp,yp,xbi(ihi1),ybi(ihi1))
         deli=1./del
         dopexp=dopexp+alog(abs(r1bi(ihi1)))*deli
         deltot=deltot+deli
         if(del.lt.delmin) then
            delmin=del
            r1sgn=sign(1.,r1bi(ihi1))
         endif
      endif
c
      if(.not.loxhi2) then
         del=dist(xp,yp,xbi(ihi2),ybi(ihi2))
         deli=1./del
         dopexp=dopexp+alog(abs(r1bi(ihi2)))*deli
         deltot=deltot+deli
         if(del.lt.delmin) then
            delmin=del
            r1sgn=sign(1.,r1bi(ihi2))
         endif
      endif
c
c...Take it and run
      dintrp=expu(dopexp/deltot)*r1sgn
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BSRCH(xp,xbi,ilo,ihi,idel,i0,i1)
c
c     Search 2D coordinate array for interval containing the 
c     point xp.
c
c     MRP   July 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      integer ilo,ihi,idel,i0,i1,imid
      real xp,xhi,xlo,xmid,xbi(1)
c
c*************
c**  START  **
c*************
c
c...Initialize
      i0=ilo
      i1=ihi
      xlo=xbi(i0)
      xhi=xbi(i1)
c
c...Outside bounds?
      if(xp.gt.xlo) goto 10
      i1=ilo
      return
c
10    if(xp.lt.xhi) goto 20
      i0=ihi
      return
c
c...Its in the 2D grid somewhere
20    if(i1-i0.le.idel) goto 999
      imid=idel*(((i1-ilo)/idel+(i0-ilo)/idel)/2)+ilo
      xmid=xbi(imid)
      if(xmid-xp) 30,50,40
30    i0=imid
      goto 20
40    i1=imid
      goto 20
50    i0=imid
      i1=imid
c
c...Done
999   return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION EXPU(arg)
      include 'p2conf.h'
c
c     Prevent underflow on large negative arguments to exponential.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      real arg
c
      if(arg.gt.-maxexp) then
         expu=exp(arg)
      else
         expu=0.
      endif
      return
      end
