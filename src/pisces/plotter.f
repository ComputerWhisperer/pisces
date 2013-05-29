cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Nov  2 23:35:52 PST 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PLOTTR
      include 'p2conf.h'
c 
c     PISCES plot segment
c
c     Original : C.H.Price     Stanford University        May, 1982
c     Modified : CSR (Faster adjacency calculation)       May, 1983
c     Modified : CSR (1-D plotter)                        Oct, 1983
c     Revision : MRP           Stanford University        Nov, 1983
c     Modified : CSR (Color contours)                     Aug, 1984
c     Modified : Michael Eldredge -- Stanford             May, 1988
c          Fixed and cleanup the interface to gplot
c     Modified : MJE - stanford                           Oct, 1989
c          Changed plot calls to the non-C conflicting names,
c          (Boy, will this cause havock!)
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'plot.h'
      include     'key.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   common overlay in plotter is
c
c                          |  emaco  |
c                          |---------|
c                          | adjtmp  |
c                          |---------|
c                          | plttmp  |
c                          |---------|
c                          | difftmp |
c
c****************************************************************** 
c 
c                   type declarations 
c 
c LOCALS:
      logical lbound,lgrid,ldepl,ljunc,lclear,logar,ltic,ltop,lcolor
      logical lmin,lmax,lpnts,labsol,lcross,vlog,ladj,laxis,logarx
      logical liver,lnewfl,lsplin,lflip,lunch,lintgl,lnegat,lcmax
      logical lnord
      integer lintyp,modec,fmode,nval1,i,nsplin,vflag
      integer ncont,ivqx,ivqy,ilabel,icolac(2)
      real cmin,cmax,cdel,pa(2),pb(2),clip,xscalf
      double precision vscl,vmax,vmin
      character*20 outfil,clogfl,chx,chy
c 
c FUNCTIONS:
      logical gtkey
c****************************************************************** 
c 
c                   start 
c 
c...Get device limits and mark the extra adjacency info as undefined.
      call devlts(devxmn,devxmx,devymn,devymx)
      ladj=.false.
c 
c  Plot controller section 
c 
      goto 100
c
c...Return point if next card not already fetched exit on error, 
c...terminate cleanly on eof 
99    if (.not.gtkey(keyid)) goto 200
c 
c...Return point if next card already fetched 
100   if(keyid.eq.kplot2) goto 1 
      if(keyid.eq.kcntur) goto 2 
      if(keyid.eq.keline) goto 3 
      if(keyid.eq.ktitle) goto 4 
      if(keyid.eq.kplot1) goto 5
      if(keyid.eq.kvectp) goto 6
      if(keyid.eq.kcomme) goto 99
c 
c...Not a plot category card, terminate and set card read flag, exit
200   call fgtoa
      lcrdrd=.true. 
      return
c 
c****************************************************************** 
c 
c                   plot.2d card
c 
1     call pl2ck(lbound,lgrid,lcross,ldepl,ljunc,lclear,
     +           ltic,ltop,lflip)
      if(errflg) goto 200
c 
c...Scale plot 
      xpdel=(xpmax-xpmin)
      xpmin=xpmin - rndoff*xpdel
      xpmax=xpmax + rndoff*xpdel
      if(.not.lflip) then
       xpdel=xpwid/(xpmax-xpmin)
       xpoff=xpmin 
      else
         xpdel=-xpwid/(xpmax-xpmin)
         xpoff=xpmax
      endif
      ypdel=(ypmax-ypmin)
      ypmin=ypmin -rndoff*ypdel
      ypmax=ypmax +rndoff*ypdel
      ypdel=ypwid/(ypmin-ypmax) 
      ypoff=ypmax 
c 
c...Plot
      if(lbound.or.lgrid) call bndpl(lbound,lgrid,ltic,ltop)
      if(ldepl) call contr(0.5,0.5,0.,1,idepln,.false.,.false.,21)
      if(ldepl) call contr(0.5,0.5,0.,1,idepln,.false.,.false.,22)
      if(ljunc) call contr(0.,0.,0.,1,juncln,.false.,.false.,4)
      if(lcross) call pcross
c 
c...Done - wait for word to proceed
      call fgtoa
      if(lpause) call ppause
      goto 99 
c 
c****************************************************************** 
c 
c                   contour card
c 
  2   call ctrck(cmin,cmax,cdel,lintyp,logar,labsol,
     +           modec,lcolor,ncont,ladj)
      if(errflg) goto 200
c
      call sizplt(.false.)
c
c...Plot
      if(.not.lcolor) then
       call contr(cmin,cmax,cdel,ncont,lintyp,logar,labsol,modec) 
      else
       call color(cmin,cmax,cdel,ncont,logar,labsol,modec)
      endif
c 
c...Done - wait for user to say go
      call fgtoa
      if(lpause) call ppause
      goto 99 
c 
c****************************************************************** 
c 
c                   vector plot
c 
  6   call vectck(vflag,vscl,vlog,lintyp,clip,vmax,vmin)
      if(errflg) goto 200
c
      call sizplt(.false.)
c 
c...Always need extra adjacency arrays for vectors
      call nxtel(p2t,p2tc)
      ladj=.true.
c
c...Plot
      call vecplt(vflag,vscl,vlog,lintyp,clip,vmax,vmin)
c 
c...Done - wait for user to say go
      call fgtoa
      if(lpause) call ppause
      goto 99 
c 
c****************************************************************** 
c 
c                   e.lines card
c 
cfix
3     continue
c3    call elnck(xfirst,xlast,yfirst,ylast,numlin,lintyp, 
c    +           alphan,betan,alphap,betap) 
c     if(errflg) goto 200
c 
c                   plot
cfix
c     call eplot(xfirst,xlast,yfirst,ylast,numlin,lintyp,alphan,betan,
c    +           alphap,betap)
c 
c                   done
      goto 99 
c 
c****************************************************************** 
c 
c                   title card
c 
4     call gtcval(1, ititle, LEN(ititle))
      goto 99
c 
c***********************************************************************
c
c                   1-d plot card
c
  5   call pl1ck(pa,pb,fmode,logar,logarx,labsol,outfil,lsplin,nsplin,
     +           lmin,lmax,lclear,laxis,lpnts,lintyp,liver,clogfl,
     +           lnewfl,ivqx,ivqy,ilabel,chx,chy,lunch,lintgl,lnegat,
     +           icolac,lcmax,cmax,lnord)
      if(errflg) goto 200
c
      call sizplt(lclear)
c
c...IV plot?
      xscalf=1.0
      if(liver) then
         call plotiv(ivqx,ivqy,lnewfl,clogfl,nval1,logar,logarx,
     +         lmin,lmax,labsol,chx,chy,icolac,lcmax,cmax,xscalf)
c
c...1-D section plot: generate extra adjacency info if necessary 
c...and interpolate.
      else
         if(fmode.ge.11 .and. .not.ladj) then
          call nxtel(p2t,p2tc)
          ladj=.true.
       endif
         call intrp1(pa,pb,fmode,logar,labsol,nval1,n1max)
      endif
c
c...Negate?
      if(lnegat) then
         do 333 i=1,n1max
333      fval(i)=-fval(i)
      endif
c
c...Sort and remove duplicate points
      if(.not.lnord) then
         call asort(nval1)
         call uniq(nval1)
      endif
c
c...Do we need to integrate?
      if(lintgl) call igrate(fval,adis,nval1,xscalf,liver)
      if(errflg) goto 200
c
c...Spline fit?
      if(lsplin) call pl1spl(nsplin,nval1)
      if(errflg) goto 200
c
c...Plot
      call d1plot(nval1,logar,logarx,labsol,outfil,lsplin,nsplin,
     +            lmin,lmax,laxis,lpnts,lintyp,liver,ilabel,
     +            chx,chy,lunch)
      if(errflg) goto 200
      call fgtoa
      if(lpause) call ppause
      goto 99 
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PL2CK(lbound,lgrid,lcross,ldepl,ljunc,lclear,ltic,ltop,
     +                 lflip)
      include 'p2conf.h'
c 
c     This routine gets the plot.2d card parameters 
c 
c     Original : C.H.Price     Stanford University        May, 1982
c     Revision : MRP           Stanford University        Nov, 1983
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
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
      include     'plot.h'
      include     'stat.h'
      include     'setup.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lbound,lgrid,ldepl,ljunc,lclear,ltic,ltop,lcross,lflip
      real wratio
c
c FUNCTIONS:
      logical isrval, iscval, islval
      real    gtrval
      logical gtlval
c 
c****************************************************************** 
c 
c                   start 
c 
c...Must have had mesh card 
      if(lmshdn) goto 10 
      call erset(78,-1,0)
      return
c 
c...Check for plot file name
10    if (iscval(1)) then
        call gtcval(1, nplout, LEN(nplout) )
        call stpfil(nplout)
      endif
c 
c...Get plot bounds - if not set, default to edge of device
      xpmin=devxmn
      xpmax=devxmx
      ypmin=devymn
      ypmax=devymx
      if(isrval(1)) xpmin=gtrval(1)*1.e-4
      if(isrval(2)) xpmax=gtrval(2)*1.e-4
      if(isrval(3)) ypmin=gtrval(3)*1.e-4
      if(isrval(4)) ypmax=gtrval(4)*1.e-4
c 
c...Left<right and bot<top
      if(xpmin.ge.xpmax.or.ypmin.ge.ypmax) call erset(80,-1,0)
      if(errflg) return
c 
c...Get mode flags
      lbound=gtlval(1)
      lgrid=gtlval(2) 
      ldepl=gtlval(3) 
      ljunc=gtlval(4) 
      lcross=gtlval(10)
      lflip=gtlval(12)
c
c...Line types
      if(isrval(11)) ielcln=gtrval(11)
      if(isrval(12)) idepln=gtrval(12)
      if(isrval(13)) juncln=gtrval(13)
      if(isrval(14)) ibndln=gtrval(14)
      if(isrval(15)) igrdln=gtrval(15)
c 
c...If depletion edges, must have a solution present
      if(.not.ldepl.or.lsol1.or.ldiff) goto 12 
      call erset(-88,-1,0) 
      ldepl=.false. 
c 
c...Get plotter clear flag and tic mark flags 
12    lclear=.not.gtlval(5) 
      ltic=.not.gtlval(6) 
      ltop=.not.gtlval(7) 
c
c...Pause after plot?
      lpause=gtlval(8)
c
c...prepare the drawing surface for drawing.
c....Must be done before using xpwid0, ypwid0
      call sizplt(lclear)
c
c...Fill screen? - set real terminal width
      xpwid=xpwid0
      ypwid=ypwid0
      wratio=(xpmax-xpmin)/(ypmax-ypmin)
c......... 9==no.fill
      if(gtlval(9)) then
         if(wratio.gt.xpwid/ypwid) then
            ypwid=xpwid0/wratio
         else
            xpwid=ypwid0*wratio
         endif
      endif
c
c...If space desired for labels, scale down
      lmargn=.false.
      if(islval(11)) lmargn=gtlval(11)
      if(lmargn) then
       xpwid=0.7*xpwid
       ypwid=0.7*ypwid
      endif
      
c 
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE BNDPL(lbound,lgrid,ltic,ltop)
      include 'p2conf.h'
      logical lbound,lgrid,ltic,ltop
c 
c                   this routine plots: 
c     if lbound:    device outer boundaries, material boundaries, electrodes
c     if lgrid:     grid lines
c 
c     Original : C.H.Price     Stanford University        May, 1982
c     Revision : MRP           Stanford University        Nov, 1983
c     Modified: MJE -- Stanford (Nov 89) removed many *move* calls
c        by checking to see if we are already at the point to
c        which we want to move.  Make sure that if the line type
c        changed, move anyway/explicitly.
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
c 
      include     'blank.h'
      include     'plot.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c  Local variables:
c 
      logical lelct
Cx    integer nside(3)
      integer nodnum(3)
      integer nodefr,nodeto
      integer lincur,linprv
      integer iside,nn,ie,i,ieladj
      real xcord(3),ycord(3),xat,yat,xt,yt,xf,yf
c
c  Functions:
c
      logical louts
c 
c****************************************************************** 
c 
c                   data
c 
Cx     data nside/2,3,1/ 
c 
c******************* Start **************************************** 
c 
c...Initialize current linetype and pen loc. to unknown 
      lincur=0
      linprv=0
      xat=-999. 
      yat=-999. 
c
c...Scan all elements 
      do 500 ie=1,ne
c 
c...Get node numbers/coordinates
      do 20 i=1,3 
      nn=nop(i,ie) 
      nodnum(i)=nn
      xcord(i)=cord(1,nn)
      ycord(i)=cord(2,nn)
20    continue
c 
c...See if the entire element is outside the plot window
      if(louts(xcord,ycord)) goto 500
c 
c...Set "from" node to 3
      nodefr=3
c 
c...Scan nodes 1,2,3 (sides 2,3,1)
      do 400 nodeto=1,3 
c 
c...Grid plot?
      if (lgrid) then
        lincur=igrdln
        goto 300
      endif
c 
c...Must be a boundary plot 
c...Are these two nodes part of one electrode?
      if(lelct(nodnum(nodefr),nodnum(nodeto))) then
          lincur=ielcln
          goto 300
      endif
c 
c...Get side number and adjacent element number 
      iside=6-nodeto-nodefr
      ieladj=nextel(iside,ie) 
c 
c...Is this side an outer boundary? 
      if (ieladj.gt.0) then
          if (imat(ie).eq.imat(ieladj)) goto 350
      endif
c
c... boundary plot
      lincur=ibndln
c
c...Check pen type
300   if (lincur.ne.linprv) call fnline(lincur)

c...Check current pen location
      xt=xcord(nodeto)
      yt=ycord(nodeto)
      xf=xcord(nodefr)
      yf=ycord(nodefr)
c 
c...Move pen to line start and plot.
c.....(only move if we need to)
      if(xat.ne.xf .or. yat.ne.yf .or. lincur.ne.linprv)
     +      call zmove(xf,yf)
      call zline(xt,yt,ipendn,0)
c 
c...Update pen location 
      xat=xt
      yat=yt
      linprv=lincur
c 
c...Update "from" node
350   nodefr=nodeto 
c 
c...Next node 
400   continue
c 
c...Next element
500   continue
c 
c...Plot tic marks if not rejected
      if(.not.ltic) goto 600 
      call fnline(ibndln)
      call ticmk(ltop)
c 
c...Done; return to alpha mode
600   call fnline(1)
      call fgtoa
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CTRCK(cmin,cmax,cdel,lintyp,logar,labsol,modec,
     +                 lcolor,ncont,ladj)
      include 'p2conf.h'
c 
c     this routine checks the contour card parameters 
c 
c     mode          contour type
c 
c     1             potential 
c     2             electron qf
c     3             hole qf
c     4             doping
c     5             electron concentration
c     6             hole concentration
c     7             net charge concentration
c     8             net carrier concentration 
c     9             valence band
c     10            conduction band
c     11-15         current
c     16            e-field
c     17            recombination
c     18            flow lines
c     19            (reserved)
c     21,22         (reserved)
c     23            (reserved)
c     24            generated carrier density due to impact ionizaton
c     25            inization rate for electrons
c     26            inization rate for holes
c 
c     Original : C.H.Price     Stanford University        May, 1982
c     Revision : MRP           Stanford University        Nov, 1983
c     Modified : CSR (added bands)                        Mar, 1985
c     Modified : MRP (contour calc)                       Apr, 1985
c     Modified : A.Yabuta      Stanfore Univeristy        Jul, 1988
c                (Added 24-26 for impact ioniztion)
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
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
      include     'plot.h'
      include     'setup.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical logar,labsol,lcolor,ladj
      integer lintyp,modec,numc,i,ncont
      real cmin,cmax,cdel,feval2,ccc
      real r
c 
cFUNCTIONS:
      logical islval
      logical gtlval
      real    gtrval
c****************************************************************** 
c 
c                   start 
c 
c...See if values specified are logarithmic  or absolute value
      logar=gtlval(23)
      labsol=gtlval(20)
c 
c...Get linetype, must be positive and in valid range 
      lintyp=gtrval(4)
      if(lintyp.ge.0) goto 71
      call erset(-94,-1,0) 
      lintyp=-lintyp
71    if(lintyp.ge.1.and.lintyp.le.lntmax) goto 72 
      call erset(-95,-1,lntmax)
      lintyp=1
c
c...Pause after plot?
72    lpause=gtlval(19)
c 
c...Get contour type, exactly one allowed 
      numc=0
      do 20 i=1,26
c........reserved numbers? skip them.
      if (i.gt.18 .and. i.lt.24) goto 20

      if(.not.islval(i)) goto 20 
      numc=numc+1 
      modec=i 
c
c...If current or field and diff -> solution files must have
c...been written with current flag on
      if(i.ge.11) then
         if(.not.ldcur.and.ldiff) call erset(165,-1,0)
      endif
20    continue

      if(numc.eq.0) call erset(147,-1,0)
      if(numc.ne.1) call erset(85,-1,numc)
      if(errflg) return
c 
c...Need to generate extra adjacency information?
      if(.not.ladj .and. (modec.ge.11)) then
         call nxtel(p2t,p2tc)
         ladj=.true.
      endif
c 
c...Must have solution present for all but dop.
      if(modec.eq.4) goto 65 
      if(.not.(lsol1.or.ldiff)) call erset(89,-1,0) 
c
c...For flow lines, must calculate potentials
65    if(modec.eq.18) then
         if(.not.lflow) then
            call jpotl
            call wnode
         endif
         if(errflg.or.wrnflg) return
      endif
c 
c...Get values - to avoid interpolation hassles with uniform doping,
c...multiply by a number very close to 1
      ncont=gtrval(5)
c
c...If no. contours not given, use specified max and min
      if(ncont.le.0) then
         cmin=gtrval(1)*1.0001
         cmax=gtrval(2)*1.0001
         cdel=gtrval(3)
c
c...must specify cmin; cmax defaults to cmin; cdel defaults to 0, but
c...cmax must equal cmin if cdel=0 - any warnings mean cdel=0, cmax=cmin 
         if(cmin.eq.-999.) call erset(81,-1,0) 
         if(errflg) return
         if(cmax.eq.-999.) cmax=cmin
         if(cmax.lt.cmin) call erset(-83,-1,0) 
         if(cdel.eq.-999.) cdel=0.
         if(cdel.lt.0.) call erset(-82,-1,0) 
         if(cmax.ne.cmin.and.cdel.eq.0.) call erset(-84,-1,0)
c
         if(wrnflg) then
            cdel=0. 
            cmax=cmin 
         endif
c
         ncont=1 
         if(cdel.ne.0.) ncont=aint((cmax-cmin)/cdel+1.+rndoff)
c
c...Automatic selection of cmax,cmin,cdel
      else
         cmin=feval2(1,modec,logar,labsol)
         cmax=cmin
         do 92 i=2,np
            ccc=feval2(i,modec,logar,labsol)
            cmin=amin1(cmin,ccc)
            cmax=amax1(cmax,ccc)
92       continue
         cdel=(cmax-cmin)/float(ncont-1)
         write(luout,9200) cmin,cmax
         if(lutty.ne.luout) write(lutty,9200) cmin,cmax
9200     format(/'  Minimum contour value = ',1pe14.6/
     +           '  Maximum contour value = ',1pe14.6/)
      endif
c
c...Color plot?
      lcolor=.false.
      if(islval(21).and.gtlval(21)) lcolor=.true.
      if(lcolor) then
      do 80 i=21,30
         r = gtrval(i)
80       if(r.ge.1..and.r.le.20.) palett(i-20) = r
      endif
      if(lcolor.and.ncont-1.gt.ncolor) call erset(156,-1,ncont-1)
c
c...X or Y component of vectors
      lxcomp=.false.
      lycomp=.false.
      if(islval(29)) lxcomp=gtlval(29)
      if(islval(30)) lycomp=gtlval(30)
      if(lxcomp.and.lycomp) then
         lxcomp=.false.
         lycomp=.false.
      else if(ldiff.and.(lxcomp.or.lycomp)) then
         lxcomp=.false.
         lycomp=.false.
         call erset(-40,-1,0)
      endif
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CONTR(cmin,cmax,cdel,ncont,lintyp,logar,labsol,modec)
      include 'p2conf.h'
c 
c     this routine plots the two-dimensional contours of: 
c 
c     mode: 
c     1             potential 
c     2             electron qf
c     3             hole qf
c     4             doping
c     5             electron concentration
c     6             hole concentration
c     7             net charge concentration
c     8             net carrier concentration 
c     9             valence band
c     10            conduction band
c     11-15         current
c     16            e-field
c     17            recombination
c     18            flow lines
c     19            (reserved)
c     21,22         (reserved)
c     23            (reserved)
c     24            generalized carrier density due to impact ionization
c     25            ionization rate for electrons
c     26            ionization rate for holes
c 
c     the contour spacing is cmin+i*cdel to cmax for linear 
c     spacing and cmin*i*cdel to cmax for logarithmic.
c 
c     Original : C.H.Price     Stanford University        May, 1982
c     Revision : MRP           Stanford University        Nov, 1983
c     Rewritten: CSR           Stanford University        Aug, 1984
c     Modified : A.Yabuta      Stanford University        Jul, 1988
c                (added 24-26 for impact ionization)
c 
c     Copyright c 1981 the board of trustees of the Leland Stanford 
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
      include     'plot.h'
      include     'setup.h'
      include     'emaco.h'
Cc....the magic TMP common memory overlays ....
C#include     "adjtmp.h"
C#include     "plttmp.h"
C      integer TMPPAD(1386002)
C      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical logar,louts,labsol
      integer lintyp,modec,ncont,ie,i,nodnum(3),node,icont,idum
      real cmin,cmax,cdel,xcord(3),ycord(3),funct(3),func1,func2, 
     +     func3,conval,feval2,x1,y1,x2,y2
      equivalence (funct(1),func1),(funct(2),func2),(funct(3),func3)
c
c******************* Start **************************************** 
c 
c..........Set line type
      call fnline(lintyp)
c 
c..........Scan all elements 
      do 1000 ie=1,ne
c 
c..........Set node numbers and coordinates
      do 30 i=1,3 
       nodnum(i)=nop(i,ie) 
       xcord(i)=cord(1,nodnum(i))
       ycord(i)=cord(2,nodnum(i))
   30 continue
  
c..........See if all nodes are to one side of window
      if(louts(xcord,ycord)) goto 1000
  
c..........Skip insulators for charge dependent contours 
      if(mattyp(imat(ie)).lt.0 .and. modec.ne.1 .and. modec.ne.16)
     +    goto 1000
  
c..........Get function values at the nodes - remember for flow lines,
c..........potential is defined for each element, not each node.
      do 60 i=1,3 
         node=nodnum(i)
         funct(i)=feval2(node,modec,logar,labsol)
  60  continue
 
c.........See if any contours in range
      if((func1.gt.cmax.and.func2.gt.cmax.and.
     +     func3.gt.cmax).or.(func1.lt.cmin.and.
     +     func2.lt.cmin.and.func3.lt.cmin)) goto 1000 
  
c........Scan contour values 
      conval=cmin 
      do 800 icont=1,ncont
c 
c........See if this contour is in this element
      if((func1.gt.conval.and.func2.gt.conval.and. 
     +     func3.gt.conval).or.(func1.lt.conval.and.
     +     func2.lt.conval.and.func3.lt.conval)) goto 800

c........Check against the degenerate case:
      if(func1.eq.func2 .and. func2.eq.func3) then
       call zmove(xcord(1),ycord(1))
       call zline(xcord(2),ycord(2),ipendn,0)
       call zline(xcord(3),ycord(3),ipendn,0)
       call zline(xcord(1),ycord(1),ipendn,0)
       goto 1000
      endif

c........Otherwise find the intersections and go for it
      call xside(conval,funct,xcord,ycord,x1,y1,x2,y2,idum,idum)
      call zmove(x1,y1)
      call zline(x2,y2,ipendn,0)
c 
c...Compute new contour value 
  800 conval=conval+cdel
c 
c...Next element
 1000 continue
c 
c...Done; prepare for alpha out
      call fnline(1)
      call fgtoa
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE COLOR(cmin,cmax,cdel,ncont,logar,labsol,modec)
      include 'p2conf.h'
c
c.......Do colour plots
c       Basic version  : CSR 25 Aug 84 -- by node
c       Second version : CSR 26 Aug 84 -- by contour.
c
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......common area
      include     'blank.h'
      include     'plot.h'
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------

c......local types
      integer ncont,modec
      real cmin,cmax,cdel
      logical logar,labsol

      integer ie,j,pj,icont,icolor,ncont1
Cx    integer pnext(3),pprev(3)
      integer plen,j1,j2,j3,j4,ipoly
      integer nch
      real fval,fval2,xe(3),ye(3),feval2
      real poly(2,10),fz(3),x1,x2,x3,x4,y1,y2,y3,y4,tsiz,lsiz,xloc,yloc
      character*21 labels(20),yform,ylinf,ylogf
c
c -- FUNCTIONS:
      integer fnumb2
c
Cx    data pnext/2,3,1/
Cx    data pprev/3,1,2/
      data ylinf /'(1pe9.2)            '/
      data ylogf /'(f6.2)              '/
      data labels/'    Potential             ',
     +            ' Electron QF Level        ',
     +            '   Hole QF Level          ',
     +            '     Doping               ',
     +            '  Electron Conc.          ',
     +            ' Hole Concentration       ',
     +            '   Net Charge             ',
     +            '  Net Carriers            ',
     +            '                          ',
     +            '                          ',
     +            ' Conduction Current       ',
     +            '  Electron Current        ',
     +            '    Hole Current          ',
     +            'Displacement Current      ',
     +            '   Total Current          ',
     +            '  Electric Field          ',
     +            '   Recombination          ',
     +            '                          ',
     +            '                          ',
     +            '                          '/

c******************** Start ********************************************
c
      ncont1=ncont-1
c
      if(.not.lmargn) goto 50
c...........Label locations.
c           The plot should have been specified as 40% smaller than
c           the physical screen so the calls to fractions >= 1
c           of xpwid,ypwid will work. If not, no labels - sorry.
c...........Top label:
      xloc=xpwid/2.
      yloc=1.10*ypwid
      tsiz=amin1(xpwid/25., ypwid/20)
      call fnline(1)
      call fsymb2(xloc,yloc,labels(modec),0,tsiz,tsiz,0.,0.5,0.5)

c...........Marker labels:
      lsiz=tsiz*0.6666
      xloc=1.20*xpwid
      yloc=ypwid * 0.8
      call fmove(xloc, yloc)
      if(logar) then
       yform=ylogf
      else
       yform=ylinf
      endif

      fval=cmin
      do 10 icont=1,ncont1
       call fplot2(iarchg,palett(icont), 0., 0.)
       call fplot2(iparea, 0, xloc-1.5*tsiz,yloc)
       call fplot2(iparea, 1, xloc-0.5*tsiz,yloc)
       call fplot2(iparea, 1, xloc-0.5*tsiz,yloc+tsiz)
       call fplot2(iparea, 1, xloc-1.5*tsiz,yloc+tsiz)
       fval2=fval+cdel
       nch=fnumb2(xloc,yloc,fval,lsiz,0.0,0.0,0.0,yform)
       yloc=yloc - 1.5*tsiz
       fval=fval2
   10 continue

c
c...........Main colour plotter starts here.

   50 continue
       
      do 1000 ie=1,ne

c...........Skip insulator triangles for charge-dependent contours
       if(mattyp(imat(ie)).lt.0 .and. modec.ne.1 .and.
     +       modec.ne.16) goto 1000

c...........Get local coords.
c
       do 100 j=1,3
          pj=nop(j,ie)
          xe(j)=cord(1,pj)
          ye(j)=cord(2,pj)
          fz(j)=feval2(pj,modec,logar,labsol)
  100    continue

c...........Step through the color bands.

       fval=cmin
         do 900 icont=1,ncont1
          fval2=fval + cdel

c..............Does either lower or upper edge of band cross?
          if( (fval.gt.fz(1).and.fval.gt.fz(2).and.fval.gt.fz(3))
     +       .or.(fval2.lt.fz(1).and.fval2.lt.fz(2).and.fval2.lt.fz(3)))
     +      goto 900

c..............Watch out for the degenerate case.
          if(fz(1).eq.fz(2) .and. fz(2).eq.fz(3)) then
             plen=3
             poly(1,1)=xe(1)
             poly(2,1)=ye(1)
             poly(1,2)=xe(2)
             poly(2,2)=ye(2)
             poly(1,3)=xe(3)
             poly(2,3)=ye(3)
             goto 800
          endif

c..............Interpolate edges.
          call xside(fval,  fz,xe,ye, x1,y1, x2,y2, j1,j2)
          call xside(fval2, fz,xe,ye, x3,y3, x4,y4, j3,j4)

c..............Determine what polygon to draw.
c..............Can be off either end, intersect two of the sides,
c..............or intersect all three sides, hence the multiple cases.
          if(j1.eq.0 .and. j3.eq.0) then 
             plen=3
             poly(1,1)=xe(1)
             poly(2,1)=ye(1)
             poly(1,2)=xe(2)
             poly(2,2)=ye(2)
             poly(1,3)=xe(3)
             poly(2,3)=ye(3)
          else if(j1 .eq. 0) then
             if(fz(6-j3-j4).lt.fval2) then
          plen=3
          poly(1,1)=xe(6-j3-j4)
          poly(2,1)=ye(6-j3-j4)
          poly(1,2)=x3
          poly(2,2)=y3
          poly(1,3)=x4
          poly(2,3)=y4
             else
          plen=4
          poly(1,1)=xe(j3)
          poly(2,1)=ye(j3)
          poly(1,2)=xe(j4)
          poly(2,2)=ye(j4)
          poly(1,3)=x3
          poly(2,3)=y3
          poly(1,4)=x4
          poly(2,4)=y4
             endif
          else if(j3 .eq. 0) then
             if(fz(6-j1-j2).gt.fval) then
          plen=3
          poly(1,1)=xe(6-j1-j2)
          poly(2,1)=ye(6-j1-j2)
          poly(1,2)=x1
          poly(2,2)=y1
          poly(1,3)=x2
          poly(2,3)=y2
             else
          plen=4
          poly(1,1)=xe(j1)
          poly(2,1)=ye(j1)
          poly(1,2)=xe(j2)
          poly(2,2)=ye(j2)
          poly(1,3)=x1
          poly(2,3)=y1
          poly(1,4)=x2
          poly(2,4)=y2
             endif
          else if( (j1.eq.j3) .and. (j2.eq.j4) ) then
             plen=4
             poly(1,1)=x1
             poly(2,1)=y1
             poly(1,2)=x2
             poly(2,2)=y2
             poly(1,3)=x4
             poly(2,3)=y4
             poly(1,4)=x3
             poly(2,4)=y3
          else if(j1.eq.j3) then
             plen=5
             poly(1,1)=x1
             poly(2,1)=y1
             poly(1,2)=x2
             poly(2,2)=y2
             poly(1,3)=xe(j1)
             poly(2,3)=ye(j1)
             poly(1,4)=x4
             poly(2,4)=y4
             poly(1,5)=x3
             poly(2,5)=y3
          else if(j1.eq.j4) then
             plen=5
             poly(1,1)=x1
             poly(2,1)=y1
             poly(1,2)=x2
             poly(2,2)=y2
             poly(1,3)=xe(j1)
             poly(2,3)=ye(j1)
             poly(1,4)=x3
             poly(2,4)=y3
             poly(1,5)=x4
             poly(2,5)=y4
          else if(j2.eq.j4) then
             plen=5
             poly(1,1)=x2
             poly(2,1)=y2
             poly(1,2)=x1
             poly(2,2)=y1
             poly(1,3)=xe(j2)
             poly(2,3)=ye(j2)
             poly(1,4)=x3
             poly(2,4)=y3
             poly(1,5)=x4
             poly(2,5)=y4
          else if(j2.eq.j3) then
             plen=5
             poly(1,1)=x2
             poly(2,1)=y2
             poly(1,2)=x1
             poly(2,2)=y1
             poly(1,3)=xe(j2)
             poly(2,3)=ye(j2)
             poly(1,4)=x4
             poly(2,4)=y4
             poly(1,5)=x3
             poly(2,5)=y3
          else
             call erset(159,-1,0)
          endif

c.............clip polygon to screen window
          call pclip (plen, poly, xpmin, xpmax, ypmin, ypmax)

c.............draw polygon, changing color if necc.
  800       continue
          icolor=palett (icont)
          call fplot2(iarchg, icolor, 0., 0.)

          call zline (poly(1, 1), poly(2, 1), iparea, 0)
          do 850 ipoly=2,plen
  850          call zline(poly(1,ipoly),poly(2,ipoly),iparea,1)

c..............Next contour
  900   fval=fval2
          

c...........Next triangle
 1000 continue


c........Next problem.
      call fgtoa
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       SUBROUTINE PL1CK(pa,pb,fmod,logar,logarx,labsol,outfil,lsplin,
     +                  nsplin,lmin,lmax,lclear,laxis,lpnts,lintyp,
     +                  liver,clogfl,lnewfl,ivqx,ivqy,ilabel,chx,chy,
     +                  lunch,lintgl,lnegat,icolac,lcmax,cmax,lnord)
      include 'p2conf.h'
c
c      Subroutine to check 1-d plot card.
c      Parameters : 
c
c      pa        - first point of 1-d section
c      pb        - last    "    "  "   "
c      fmod      - what variable (defined as in contour plot)
c      logar     - whether a logarithmic plot is desired (y-axis)
c      logarx    - whether a logarithmic plot is desired (x-axis)
c      labsol    - absolute value flag
c      outfil    - file for printed output
c      lsplin    - spline flag
c      nsplin    - no. of spline interpolated values
c      lmin      - flag indicating min function bound set
c      lmax      - flag indicating max function bound set
c      lclear    - flag to indicate screen is to be cleared
c      laxis     - flag to indicate axis is to be drawn
c      lpnts     - flag to indicate data points are to be marked
c      lintyp    - line type
c      liver     - iv curve to be plotted
c      clogfl    - log file name
c      lnewfl    - flag for giving new log file name(clogfl)
c      ivqx,ivqy - iv plot quantities
c      ilabel    - label #
c      chx       - x label if iv
c      chy       - y label if iv
c      lunch     - dont change axis
c      lintgl    - integrate wrt x
c      lnegat    - negate function values
c      icolac    - column index for ac admittance quantity
c      lcmax     - flag to indicate max x value for iv
c      cmax      - max x value for iv
c      lnord     - dont order iv points
c
c 
c     Original : CSR           Stanford University        Oct, 1983
c     Revision : MRP           Stanford University        Nov, 1983
c     Revision : Greg Anderson Stanford University        Aug, 1989
c     
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    common area
c
      include     'plot.h'
      include     'stat.h'
      include     'blank.h'
      include     'setup.h'
      include     'symme.h'
c 
c------------------------------------------------------------------
c
c                    type declarations
c
      logical logar,lsplin,lmin,lmax,lclear,laxis,lpnts,labsol
      logical lnewfl,liver,lunch,lintgl,lnegat,lcmax,logarx,lnord
      integer fmod, numc,i,nsplin,lintyp,ivq(2),ivqx,ivqy
      integer ilabel,icolac(2)
      real    pa(2),pb(2),cmax
      character*(20) xydata
      character*(4)  xyname
      character*(4)  xynumb
      integer        namln, numln
      character*17 chvolt,champ,chcond,chcap,chintg
      character*20 outfil,clogfl,chx,chy,chsec,chfreq
      character*20 chxy(2)
c
c FUNCTIONS:
      integer ch2i
      logical islval, isrval, iscval
      real    gtrval
      logical gtlval
c
c      equivalence (ch,ich),(ch0,ich0),(ch1,ich1)
c
      data chsec /'time ( sec)         '/
      data chfreq/'Frequency ( Hz)     '/
      data chvolt/' (Volts)         '/
      data champ /' (Amp/um)        '/
      data chcap /' ( F/um)         '/
      data chcond/' ( S/um)         '/
      data chintg/' (Integrated)    '/
c
c  If we're using cylindrical coordinates, we're not dealing with 
C  current densities, etc.  Same for width.
c
      IF (LCYL.or.lwidth) THEN
          champ =' (Amp)        '
          chcap =' ( F)         '
          chcond=' ( S)         '
      ENDIF
c
c************************ Start ****************************************
c
c                    Check we've got a mesh
c
      if(lmshdn) goto 1
      call erset(78,-1,0)
      return
   1  continue
c
c...Output file(s) specified? (Note: Always call gtcval().  Other places
c....   check for IF(OUTFIL(1:1).eq.' ') (mje jul88)
      call gtcval(1, outfil, LEN(outfil) )
c.......plot output file?  (if (not ascii and have outfile))
      if (.not.gtlval(24) .and. iscval(1)) then
        call stpfil(outfil)
c..........handled by us.  Not an ascii file.
        outfil = ' '
      endif
c
c...Flags
      logar =gtlval(20)
      logarx=gtlval(31)
      labsol=gtlval(18)
      lsplin=gtlval(21)
      nsplin=gtrval(5)
      if(nsplin.gt.n1max) then
         call erset(269,-1,-1)
         nsplin=n1max
      endif
      lclear=.not.gtlval(28)
      laxis=.not.gtlval(19)
      lunch=gtlval(25)
      if(lunch) then
         laxis=.false.
         lclear=.false.
      endif
      lpnts=gtlval(22)
      lintyp=gtrval(8)
c
c...Pause after plot?
      lpause=gtlval(23)
c
c...Integrate?
      lintgl=gtlval(26)
      lnegat=gtlval(27)
c
c...Function bounds
      if(.not.lunch) then
         lmin=.false.
         lmax=.false.
         fmin=gtrval(6)
         fmax=gtrval(7)
         if(fmin.ne.-999.) lmin=.true.
         if(fmax.ne.-999.) lmax=.true.
      endif
c
c...IV plot ?
      lnewfl = iscval(2)
      liver=lnewfl .or. iscval(4) .or. iscval(3)
      if(.not.liver) goto 300
      if(lnewfl) call gtcval(2, clogfl, LEN(clogfl))
      if(.not.lunch) then
         lcmax=isrval(9)
         if(lcmax) cmax=gtrval(9)
      endif
      lnord=gtlval(32)
c
c...Parse the parameter. CVAL(3)::xdata, CVAL(4)::ydata
      do 210 i = 1, 2
c........Grab the [xy]axis character values.
          call gtcval(2+i, xydata, LEN(xydata))
c........Parse the value
          call xypars(xydata, xyname, namln, xynumb, numln)
          if (namln.le.0) then
              call erset(721, -1, 0)
              goto 210
          endif
          if (numln.ge.3) then
              call erset(721, -1, 0)
              goto 210
          endif
c........
          icolac(i)=0
          if (xyname(1:3).eq.'VA ') then
              ivq(i)=nelect+ch2i(xynumb(1:1))
              chxy(i)=xyname(1:2)//xynumb(1:1)//' '//chvolt
          else if (xyname(1:2).eq.'V ') then
              ivq(i)=ch2i(xynumb(1:1))
              chxy(i)=xyname(1:1)//xynumb(1:1)//' '//chvolt
          else if (xyname(1:2).eq.'I ') then
              ivq(i)=2*nelect+ch2i(xynumb(1:1))
              chxy(i)=xyname(1:1)//xynumb(1:1)//' '//champ
          else if (xyname(1:2).eq.'G ') then
              ivq(i)=ch2i(xynumb(1:1))
              icolac(i)=ch2i(xynumb(2:2))
              chxy(i)=xyname(1:1)//xynumb(1:2)//chcond
          else if (xyname(1:2).eq.'C ') then
              ivq(i)=nelect+ch2i(xynumb(1:1))
              icolac(i)=ch2i(xynumb(2:2))
              chxy(i)=xyname(1:1)//xynumb(1:2)//chcap
          else if (xyname(1:2).eq.'Y ') then
              ivq(i)=nelect+nelect+ch2i(xynumb(1:1))
              icolac(i)=ch2i(xynumb(2:2))
              chxy(i)=xyname(1:1)//xynumb(1:2)//chcond
          else if (xyname(1:1).eq.'F') then
              ivq(i)=0
              icolac(i)= -1
              chxy(i) = chfreq
          else if (xyname(1:1).eq.'T') then
              ivq(i)=0
              chxy(i) = chsec
          else
              call erset(185, -1, 0)
              return
          endif
210   continue

      ivqx=ivq(1)
      ivqy=ivq(2)

      chx = chxy(1)
      chy = chxy(2)

      if(lintgl) chy(4:20)=chintg

      return
c
c...Find start, end of plot, scaled to centimeters
300   if(.not.isrval(1) .or. .not.isrval(2) .or. 
     +   .not.isrval(3) .or. .not.isrval(4)) then
       call erset(263,-1,0)
       return
      endif
      pa(1)=gtrval(1)*1e-04
      pa(2)=gtrval(2)*1e-04
      pb(1)=gtrval(3)*1e-04
      pb(2)=gtrval(4)*1e-04
c??      if((pa(1).ge.devxmn).and.(pa(1).le.devxmx).and.
c??     +   (pa(2).ge.devymn).and.(pa(2).le.devymx).and.
c??     +   (pb(1).ge.devxmn).and.(pb(1).le.devxmx).and.
c??     +   (pb(2).ge.devymn).and.(pb(2).le.devymx) ) goto 4
c??      call erset(260,-1,0)
c??      return
c??   4  continue
c
c                    Get value to be plotted, only one allowed
c
      numc=0
      do 10 i=1,17
      if(.not.gtlval(i)) goto 10
      numc=numc+1
      fmod=i
      if(i.gt.8) then
         if(.not.ldcur.and.ldiff) call erset(165,-1,0)
      endif
10    continue
      if(numc.ne.1) call erset(262,-1,numc)
      if(errflg) return
c
c                    Check we're ready to do plots
c
      if(fmod.eq.4) goto 20
      if(.not.(lsol1.or.ldiff)) call erset(89,-1,0)
20    continue
c
c...Labels
      if(fmod.le.3.or.(fmod.eq.9).or.(fmod.eq.10)) then
         ilabel=1
      else if(fmod.le.8) then
         ilabel=2
      else if(fmod.le.15) then
         ilabel=3
      else if(fmod.eq.16) then
         ilabel=4
      else
         ilabel=5
      endif
c
c...X or Y component of vectors
      lxcomp=.false.
      lycomp=.false.
      if(islval(29)) lxcomp=gtlval(29)
      if(islval(30)) lycomp=gtlval(30)
      if(lxcomp.and.lycomp) then
         lxcomp=.false.
         lycomp=.false.
      else if(ldiff.and.(lxcomp.or.lycomp)) then
         lxcomp=.false.
         lycomp=.false.
         call erset(-40,-1,0)
      endif
c
c........Bye
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE D1PLOT(nval1,logar,logarx,labs,outfil,lsplin,nsplin,
     +                  lmin,lmax,laxis,lpnts,lintyp,
     +                  liver,ilabel,chx,chy,lunch)
      include 'p2conf.h'
c
c     Subroutine to perform 1-d plot of values in fval against 
c     ordinates in adis.
c     
c     Input parameters :
c     fval  - array of functional values
c     adis  - array of ordinates (distances from pa )
c     nval1 - number of values to plot
c     logar - whether y axis is logarithmic
c     logarx- whether x axis is logarithmic
c     labs  - whether y axis is absolute 
c     outfil- output file name
c     lsplin- spline flag
c     nsplin- no. of interpolated spline points
c     lmin  - flag to indicate min function value specified
c     lmax  - flag to indicate max function value specified
c     laixs - flag to indicate axis is to be drawn
c     lpnts - flag to indicate data points are to be marked
c     lintyp- line type
c     liver - iv curve to be drawn
c     ilabel- label #
c     chx   - x label if iv
c     chy   - y label if iv
c     lunch - dont change axis
c 
c     Original : CSR           Stanford University        Oct, 1983
c     Revision : MRP           Stanford University        Feb, 1984
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    common area
c
      include     'plot.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                    Type declarations
c
      logical logar,lmin,lmax,lsplin,lpnts,labs
      logical logplt,laxis,liver,lunch,logarx
      integer nval1,i,lu,nsplin,isplin,lintyp,ntic,ilabel,ierr
      real aaa,xxx,yyy,xorg,yorg,fff
      real delx,dely,ftic,stic,xtmin,xtmax,ytmin,ytmax
      real ltic,axex0,axey0,axelen
      real dtic

      character*20 outfil,chx,chy
      character*30 xlabel,ylabel
      character*21 chflab(5)
      character*10 xform,ylinf,ylogf,yform,xlinf
c
      data  dtic/0.0/
      data xlinf/'7.2f      '/,ylinf/'9.2g      '/,ylogf/'4.0f        '/
      data chflab / 'Potential (Volts)     ' , 'Concentration (cm-3) ' ,
     +              'Current (Amps/cm2)    ' , 'Electric field (V/cm)' ,
     +              'Recombination (cm-3/s)' /
     
c
c***************************** Start ***********************************
c
c
c...Labels
      if(.not.liver) then
         xlabel='Distance (um)'
      else
         xlabel=chx
      endif
      if(.not.liver) then
         ylabel=chflab(ilabel)
      else
         ylabel=chy
      endif
c
c...If printed output required, do it
      if(outfil(1:1).eq.' ') goto 100
      lu=lutmp
      call fopcl(12,outfil,20,lu,.false.,ierr)
      write(lu,1031)
1031  format(20x,'*** 1-D Plot Output ***'////)
      if(liver) then
         write(lu,1132) xlabel,ylabel
1132     format(2a30)
         do 21 i=1,nval1
            aaa=adis(i)
            fff=fval(i)
            if(logarx) aaa=10.**aaa
            if(logar) fff=10.**fff
            write(lu,1133) aaa,fff
1133        format(1pe13.6,1pe30.6/)
21       continue
      else
         write(lu,1032)
1032     format('Distance from a   function',10x,
     +              'x,y coordinates')
         do 20 i=1,nval1
            aaa=1.0e+04*adis(i)
            fff=fval(i)
            if(logar) fff=10.**fff
            xxx=1.0e+04*cord1d(1,i)
            yyy=1.0e+04*cord1d(2,i)
            write(lu,1033) aaa,fff,xxx,yyy
1033        format(4(1pe12.5,5x))
   20     continue
      endif
      call fopcl(0,outfil,20,lu,.false.,ierr)
c
c...Scale a to microns.
  100 continue

      if(nval1.eq.1) then
         call erset(259,-1,0)
         return
      endif

      if(liver) goto 35
      do 30 i=1,nval1
   30    adis(i)=1.e4*adis(i)
      if(lsplin) then 
         do 3030 i=1,nsplin
 3030       splx(i)=1.e4*splx(i)
      endif
c
c...Bounds first; adis is already sorted.
35    if(lunch) goto 36
      if(lsplin) then
         call arbnd(sply,nsplin,lmin,lmax,fmin,fmax)
      else
         call arbnd(fval,nval1,lmin,lmax,fmin,fmax)
      endif
      call mxarr(-1,adis,nval1,amin)
      call mxarr(+1,adis,nval1,amax)
c
c...If bounds are equal, must adjust
      if(fmin.eq.fmax) then
         if(fmax .eq. 0.) then
          fmax=1.
          fmin=-1.
       else if(fmax.gt.0.) then
          fmax=2. *fmax
          fmin=0.
       else if(fmax.lt.0.) then
          fmin=2. *fmax
          fmax=0.
       endif
      endif
c
c...Plot bounds :
c...[x/y]t[min/max] represent the bounds on values plotted
c...[x/y]p[min/max] are the bounds used to get a frame.
36    xtmin=amin
      xtmax=amax
      ytmin=fmin
      ytmax=fmax
      xpwid=xpwid0
      ypwid=ypwid0
c
c...Make sure we have a solid, black line.
      call fnline(1)
c
c...Add a frame for axis space
c
c   x-axis
c      <--margin-->|<----picture--->|<--margin-->
c          15%             80%            5%
c   y-axis
c      <--margin-->|<----picture--->|<--margin-->
c          10%             80%           10%
c
c...Decide on marks and draw axes. 

c......Offset is 1/15 (1/10) of x(y) screen - in inches
      axex0=0.15*xpwid
      axey0=0.10*ypwid

c......Distance axis (8/10 of screen) in inches
      axelen= 0.8* xpwid
      logplt=logarx
      if(logplt) then
         xtmin=10.**xtmin
         xtmax=10.**xtmax
      endif
      call faxtc2(xtmin,xtmax,.true.,logplt,ftic,ltic,stic,ntic)

      if(logplt) then
         xtmin=alog10(ftic)
         xtmax=alog10(ltic)
         xform=ylogf
      else
         xtmin=(ftic)
         xtmax=(ltic)
         xform=xlinf
      endif

      if(laxis) call faxpt2(axex0,axey0,
     +            axelen,0.0,
     +            ftic,ltic,ftic,ltic,
     +            stic,ntic,
     +            hnumb, 0.0,0,
     +            90.0,htic,dtic,
     +            xlabel,htitle,-1,xform)

c...Function axis
c...Axplt calls fdraw() directly without using zline or zmove, so
c...it doesn't need scaling.
c...Do a log axis only for a true log plot, not for an asinh plot.
c...Note : faxpt2 wants true values, axtcs wants logs.
      logplt= logar .and. (labs .or. (ytmin.gt.0))

      axelen= 0.8* ypwid
      if(logplt) then
         ytmin=10.**ytmin
         ytmax=10.**ytmax
      endif
      call faxtc2(ytmin, ytmax, .true., logplt, ftic, ltic, stic,ntic)

      if(logplt) then
         ytmin=alog10(ftic)
         ytmax=alog10(ltic)
         yform=ylogf
      else
         ytmin=ftic
         ytmax=ltic
         yform=ylinf
      endif

      if(laxis) call faxpt2(axex0,axey0,
     +            axelen,90.0,
     +            ftic,ltic,ftic,ltic,
     +            stic,ntic,
     +            hnumb,-90.0,1,
     +            -90.0,htic,dtic,
     +            ylabel,htitle,1,yform)


c...Set up graph curve - set physical bounds 1/8 bigger 
c...to get that frame correct.
      xpdel=0.125*(xtmax-xtmin)
      ypdel=0.125*(ytmax-ytmin)

      xpmin=xtmin-1.5*xpdel
      xpmax=xtmax+0.5*xpdel
      ypmin=ytmin-ypdel
      ypmax=ytmax+ypdel

c......B. Calculate offset and scaling for zline/zmove routines
      delx=xpmax-xpmin
      dely=ypmax-ypmin
      xpdel=xpwid/delx
      ypdel=ypwid/dely

      xpoff=xpmin
      ypoff=ypmin

c
c...Origin
      xorg=xtmin
      yorg=ytmin
      call zmove(xorg,yorg)
c
c-------------
c Draw graph
c-------------
c
c...Use zline1 to limit excursions below the x-axis
      call fnline(lintyp)
      delx=delx*0.004*ypwid/xpwid
      dely=dely*0.004
      call zmove(adis(1),fval(1))
      if(lpnts) call drawsq(adis(1),fval(1),delx,dely)
c
c...Non-spline
      if(lsplin) goto 300
      do 200 i=2,nval1
      call zline1(adis(i),fval(i),ytmin)
      if(lpnts) call drawsq(adis(i),fval(i),delx,dely)
200   continue
      goto 999
c
c...Spline
300   i=2
      do 400 isplin=1,nsplin
      if(splx(isplin).le.adis(i)) then
         call zline1(splx(isplin),sply(isplin),ytmin)
      else
         call zline1(adis(i),fval(i),ytmin)
         if(lpnts) call drawsq(adis(i),fval(i),delx,dely)
         i=i+1
      endif
400   continue
c
c...Done - post device and exit
999   call fnline(1)
      call fgtoa
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE INTRP1(pa,pb,fmode,logar,labsol,nval1,n1max)
      include 'p2conf.h'
c
c     Subroutine to interpolate values for a 1-d plot
c     Parameters :
c
c     Input
c       pa,pb   - ends of plot 
c       fmode   - what variable we're plotting (see contour)
c       logar   - whether logarithmic plot
c       labsol  - absolute value flag
c       n1max   - max number of plot entries
c     Output
c       fval    - array of interpolated values   (in common)
c       adis    - distance of points from a      (  "      )
c       cord1d  - x,y coordinates of points used (  "      )
c       nval1   - how many values found.
c
c 
c     Original : CSR           Stanford University        Oct, 1983
c     Revision : MRP           Stanford University        Feb, 1984
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                    common area  
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                    Parameters
c
      real pa(2),pb(2)
      integer fmode,nval1,n1max
      logical logar
c
c                    Local types
c
      logical   labsol
      integer   i,nexti(3),ip,ie,ix,ixx,pntx,pntxx
      real      x(3),y(3),ll,lmm,ln,llen,l(3),s(3),fx,fxx,alx,alxx
      real      p(2),dp(2),toler,f,feval2,dx,dy
c
      data nexti/2,3,1/

c
c********************* Start *******************************************
c
c                    Clear abcissa,ordinate arrays
c
       do 10 i=1,n1max
       adis(i) = 0
       fval(i) = 0
 10    continue
       nval1 = 0
c
c                    Line coefficients of 1-d slice
c
       dp(1) = pb(1) - pa(1)
       dp(2) = pb(2) - pa(2)
       ll = -dp(2)
       lmm =  dp(1)
       ln = -ll*pa(1) - lmm*pa(2)
       llen = sqrt(ll*ll+lmm*lmm)
c
c                    Loop through triangles looking
c                    for intersections
c
       do 900 ie = 1,ne

c                    Skip insulator triangles as needs be.
c
       if(mattyp(imat(ie)).lt.0.and.fmode.ne.1.and.fmode.ne.16) 
     +    goto 900

c                    Calculate signed distances (l(i)) of each node
c                    from the line
       do 100 ix=1,3
        ip = nop(ix,ie)
        x(ix) = cord(1,ip)
        y(ix) = cord(2,ip)
        l(ix) = (ll*x(ix) + lmm*y(ix) + ln) / llen
  100  continue

c                    Calculate approximate side lengths (by Manhattan
c                    metric)  and thus tolerance to use.
       do 120 ix=1,3
        ixx=nexti(ix)
        dx = x(ix)-x(ixx) 
        dy = y(ix)-y(ixx)
          s(ix) = abs(dx) + abs(dy)
  120 continue

       toler = 1.e-5 * amin1(s(1),s(2),s(3))
 
c.......Check for nodes lying right on the slice
c.......For each one, call vappnd to include its value, and make sure
c.......it won't be seen as an intersection later (l := 0).
       do 200 ix=1,3
       if(abs(l(ix)).lt.toler) then
           l(ix) = 0
           p(1) = x(ix)
           p(2) = y(ix)
           pntx =nop(ix,ie)
           f = feval2(pntx,fmode,logar,labsol)
           call vappnd(pa,pb,ie,p,f,nval1,n1max,fmode,logar,labsol)
       endif
200    continue

c.......Check for intersections.  (Exact nodes are excluded by strict < )
c.......For each one, linearly interpolate the function on the side,
c.......using equal angles trick, and call vappnd to store it.
       do 300 ix=1,3
       ixx = nexti(ix)
       if(l(ix)*l(ixx).lt.0) then
           pntx  = nop(ix,ie)
           pntxx = nop(ixx,ie)
         fx    = feval2(pntx ,fmode,logar,labsol)
         fxx   = feval2(pntxx,fmode,logar,labsol)
         alx   = abs(l(ix))
         alxx  = abs(l(ixx))
         p(1)  = (alx*x(ixx) + alxx*x(ix)) / (alx+alxx)
         p(2)  = (alx*y(ixx) + alxx*y(ix)) / (alx+alxx)
         f     = (alx*fxx + alxx*fx) / (alx+alxx)
           call vappnd(pa,pb,ie,p,f,nval1,n1max,fmode,logar,labsol)
       endif
  300  continue
c
c 
  900  continue
c
c...Done
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       SUBROUTINE VAPPND(pa,pb,ie,p,f,nval1,n1max,fmode,logar,labsol)
      include 'p2conf.h'
c
c              Append value f for point p, provided p is between
c              pa and pb, otherwise use the appropriate end of the
c              line, provided it lies in the triangle.
c 
c     Original : CSR           Stanford University        Oct, 1983
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c              Parameters
c
       real    pa(2),pb(2),p(2),f
       integer nval1,ie,fmode,n1max,p1,p2,p3
       logical logar,labsol
c
c              Local types 
c
       real    dp(2),alph,k1,k2,k3,feval2,dist
c
c********************* Start ******************************************
c
       if(nval1.ge.n1max) then
         call erset(-267,0,0)
         return
       endif
       dp(1)=pb(1)-pa(1)
       dp(2)=pb(2)-pa(2)
       alph=dp(1)*(p(1)-pa(1)) + dp(2)*(p(2)-pa(2))
       alph=alph / (dp(1)*dp(1)+dp(2)*dp(2))
c
c                   In segment
c
       if((alph.ge.0).and.(alph.le.1)) then
           nval1=nval1 + 1
           adis(nval1)=dist(p(1),p(2),pa(1),pa(2))
           fval(nval1)=f
         cord1d(1,nval1)=p(1)
         cord1d(2,nval1)=p(2)
       endif
c
c                   Off 'a' end of segment 
c
       if(alph.lt.0) then
           call baryc(pa(1),pa(2), ie, k1,k2,k3)
           if((k1.ge.0).and.(k2.ge.0).and.(k3.ge.0)) then
               nval1=nval1 + 1
               adis(nval1)=0
             cord1d(1,nval1)=pa(1)
             cord1d(2,nval1)=pa(2)
               p1=nop(1,ie)
               p2=nop(2,ie)
               p3=nop(3,ie)
               fval(nval1)=k1*feval2(p1,fmode,logar,labsol)
     +                      +k2*feval2(p2,fmode,logar,labsol)
     +                      +k3*feval2(p3,fmode,logar,labsol)
           endif
       endif
c
c                   Off 'b' end of segment 
c
       if(alph.gt.1) then
          call baryc( pb(1),pb(2), ie, k1,k2,k3)
        if((k1.ge.0).and.(k2.ge.0).and.(k3.ge.0)) then
              nval1 =nval1 + 1
              adis(nval1)=dist(pa(1),pa(2),pb(1),pb(2))
            cord1d(1,nval1)=pb(1)
            cord1d(2,nval1)=pb(2)
              p1=nop(1,ie)
              p2=nop(2,ie)
              p3=nop(3,ie)
              fval(nval1)=k1*feval2(p1,fmode,logar,labsol)
     +                     +k2*feval2(p2,fmode,logar,labsol)
     +                     +k3*feval2(p3,fmode,logar,labsol)
          endif
       endif
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       SUBROUTINE ASORT(nval1)
      include 'p2conf.h'
c
c      Sort adis,fval,cord1d according to adis
c 
c     Original : CSR           Stanford University        Oct, 1983
c 
c     Copyright c 1983 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c               common area
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c               Local types
c
       integer  nval1,i
c
c************************* Start ***************************************
c
       do 75 i=1,nval1
       rtmp(i)=0
   75  itag(i)=i
c
c              Sort according to adis; output is itag 
c
       call v5sort( nval1, itag, adis )
c
c              Reorder fvals according to itag
c
       do 100 i=1,nval1
  100  rtmp(i)=fval(itag(i))
       do 150 i=1,nval1
  150  fval(i)=rtmp(i)
c
c              Reorder coordinates 
c
       do 200 i=1,nval1
  200  rtmp(i)=cord1d(1,itag(i))
       do 250 i=1,nval1
  250  cord1d(1,i)=rtmp(i)
       do 300 i=1,nval1
  300  rtmp(i)=cord1d(2,itag(i))
       do 350 i=1,nval1
  350  cord1d(2,i)=rtmp(i)
       return
       end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PLOTIV(iquanx,iquany,lnewfl,cnewfl,irec,logar,logarx,
     +           lmin,lmax,labsol,chx,chy,icolac,lcmax,cmax,scalx)
      include 'p2conf.h'
c
c     Plot iv(t) curves from log file.
c
c     Original : MRP Aug 84
c     Modified : Michael Eldredge -- Stanford (mar 89)
c       Fixed EQUAN declaration bug (dimension 30 not 20) as reported
c       by F.C.Rock (fcr%gte.com@relay.cs.net)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'logunt.h'
      include     'blank.h'
      include     'plot.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      integer TMPPAD(1386002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      logical lnewfl,logar,labsol,lcmax,laset,lfset,logarx,lmin,lmax
      character*20 cnewfl,chx,chy
      character*1 chscal(10),ch
      integer irec,iquanx,iquany,nel2,i,lu,spos,ilog,icolac(2),j
      integer mmod,nel,iss,ncol,sposy,ierr,sposy1,sposy2
      double precision equan(30),dum,dfq
      real rtime,scalx,cgmax(2),cmax,xdata
c
c...Position of time/admittance scale factor in axes label
      data spos,sposy1,sposy2/7,6,12/
c
c...Labels for scaling of x-axis if time-dep.
      data chscal/'G','M','K',' ','m','u','n','p','f','a'/
c
c*************
c**  START  **
c*************
c
c...Ac or dc file?
      if((icolac(1).ne.0).or.(icolac(2).ne.0)) goto 500
c
c Dc
c
c
c...New file or one we've been working with?
      if(lnewfl) then
         lu=lusup
         call fopcl(2,cnewfl,20,lu,.false.,ierr)
      else
         lu=lulog
         rewind(lu)
      endif
      if(errflg) return
c
c...Read it
      irec=0
      read(lu,*) nel
      if(nel.ne.nelect) call erset(-179,-1,0)
      nel2=nel+nel+nel

10    read(lu,5000,end=20) ch,rtime,(equan(i),i=1,nel2)
5000  format(a1,1pe11.5,30(1pe16.8))
      if(ch.ne.' ') goto 10
      if(iquanx.eq.0) then
         xdata=rtime
      else
         xdata=equan(iquanx)
      endif
      if(lcmax.and.(xdata.gt.cmax)) goto 10
      irec=irec+1
      adis(irec)=xdata
      if(iquany.eq.0) then
         fval(irec)=rtime
      else
         fval(irec)=equan(iquany)
      endif
      if(labsol) fval(irec)=abs(fval(irec))
      if(logar) fval(irec)=alog10(fval(irec))
      goto 10
c
c...If time-dep., scale properly and fix axis label
20    if(logarx.or.(iquanx.ne.0).or.(adis(irec).ge.1.)) goto 998
      ilog=int(alog10(adis(irec)))
      ilog=-max0(ilog-mmod(ilog,3),-18)
      ilog=max0(-9,ilog)
      scalx=10.**ilog
      do 30 i=1,irec
30    adis(i)=adis(i)*scalx
      chx(spos:spos)=chscal((ilog+12)/3)
      if(lmin) fmin=fmin*scalx
      if(lmax) fmax=fmax*scalx
      goto 998
c
c Ac
c
c
c...New file or one we've been working with?
500   if(lnewfl) then
         lu=lusup
         call fopcl(2,cnewfl,20,lu,.false.,ierr)
      else
         lu=lulog2
         rewind(lu)
      endif
      if(errflg) return
      cgmax(1)=0.
      cgmax(2)=0.
c
c...Read it
      irec=0
      read(lu,*) nel
      if(nel.ne.nelect) call erset(-179,-1,0)
      nel2=nel+nel+nel
c
510   read(lu,8002,end=120) ncol,dum,dfq,(equan(i),i=1,nel)
8002  format(1x,i3,12e16.8)
      lfset=.false.
      laset=.false.
      irec=irec+1
      if(icolac(1).eq.0) then
         adis(irec)=equan(iquanx)
         laset=.true.
      else if(icolac(1).lt.0) then
         adis(irec)=dfq
         laset=.true.
      endif
      if(icolac(2).eq.0) then
         fval(irec)=equan(iquany)
         lfset=.true.
      else if(icolac(2).lt.0) then
         fval(irec)=dfq
         lfset=.true.
      endif

      do 515 j=1,ncol
         read(lu,8003) iss,(equan(i),i=1,nel2)
8003     format(i4,30e16.8)
         if(iss.eq.icolac(1)) then
            adis(irec)=equan(iquanx)
            laset=.true.
         endif
         if(iss.eq.icolac(2)) then
            fval(irec)=equan(iquany)
            lfset=.true.
         endif
515   continue

      if(lcmax.and.(xdata.gt.cmax)) goto 10
      if(.not.(laset.and.lfset).or.
     +  (lcmax.and.(adis(irec).gt.cmax))) then
         irec=irec-1
      else
         cgmax(1)=amax1(cgmax(1),abs(adis(irec)))
         cgmax(2)=amax1(cgmax(2),abs(fval(irec)))
         if(labsol) fval(irec)=abs(fval(irec))
         if(logar) fval(irec)=alog10(fval(irec))
      endif
      goto 510
c
c...Check consistency
120   continue
      do 121 i=1,irec
         do 122 j=1,irec
            if(i.eq.j) goto 122
            if(adis(i).eq.adis(j)) then
               if(icolac(1).lt.0) then
                  call erset(301,-1,0)
               else
                  call erset(300,-1,0)
               endif
               return
            endif
122      continue
121   continue
c
c...Scale properly and fix axis label
      do 600 i=1,2
         if((logarx.and.(i.eq.1)).or.(logar.and.(i.eq.2))) goto 600
         if(icolac(i).ne.0) then
            ilog=int(alog10(cgmax(i)))
            ilog=-max0(ilog-mmod(ilog,3),-18)
            ilog=max0(-9,ilog)
            scalx=10.**ilog
            if(icolac(i).lt.0) then
               sposy=sposy2
            else
               sposy=sposy1
            endif
            if(i.eq.1) then
               do 130 j=1,irec
130            adis(j)=adis(j)*scalx
               chx(sposy:sposy)=chscal((ilog+12)/3)
            else
               do 131 j=1,irec
131            fval(j)=fval(j)*scalx
               chy(sposy:sposy)=chscal((ilog+12)/3)
               if(lmin) fmin=fmin*scalx
               if(lmax) fmax=fmax*scalx
            endif
         endif
600   continue
c
c...Log x-axis?
998   if(logarx) then
         do 134 j=1,irec
134      adis(j)=alog10(adis(j))
      endif
c
c...Close file if new - do nothing if current file
      if(lnewfl) call fopcl(0,cnewfl,20,lu,.false.,ierr)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MXARR(iflag,adis,nval1,amax)
      include 'p2conf.h'
c
      integer iflag,i,nval1
      real adis(1),amax
c
      if(iflag.gt.0) then
         amax=adis(1)
         do 10 i=2,nval1
10          amax=amax1(amax,adis(i))
      else
         amax=adis(1)
         do 20 i=2,nval1
20          amax=amin1(amax,adis(i))
      endif
c
      return
      end
c
c ================================================================
c "xypars": Parse the [XY]DATA = string parameter for the plot.1d
c     card.  If error, ns && nn are -1.
c
c called as:
c       call xypars('Va12', name, nname, numb, nnumb)
c gives:
c       name = 'VA  ';  nname = 2
c       numb = '12  ';  nnumb = 2
c
c Original: Michael Eldredge -- Stanford University (oct 87)
c
        SUBROUTINE XYPARS(str, name, nname, numb, nnumb)
      include 'p2conf.h'
          character*(*) str
          character*(*) name
          integer       nname
          character*(*) numb
          integer       nnumb
c ----------------------------------------------------------------
        integer  strmax, nammax, nummax
        character    c
        integer  n, i

        character*26 upps, lows
        character*10 digs

        data upps/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
        data lows/'abcdefghijklmnopqrstuvwxyz'/
        data digs/'0123456789'/
c ----------------------------------------------------------------

c....init some things.
        name = ' '
        numb = ' '
        nname= 0
        nnumb= 0
        strmax = LEN(str)
        nammax = LEN(name)
        nummax = LEN(numb)
        n = 1
c
c....Look for leading characters.
10      if (n.gt.strmax) return
        c = str(n:n)
        i = INDEX(lows, c)
        if (i.gt.0) c = upps(i:i)

        i = INDEX(upps, c)
        if (i.gt.0) then
            if (nname.ge.nammax) goto 20
            nname = nname + 1
            name(nname:nname) = c
            n = n + 1
            goto 10
        endif
c
c....Look for numbers
20      if (n.gt.strmax) return
        c = str(n:n)
        i = INDEX(digs, c)
        if (i.gt.0) then
            if (nnumb.ge.nummax) goto 29
            nnumb = nnumb + 1
            numb(nnumb:nnumb) = c
            n = n + 1
            goto 20
        endif

29      continue
        if (n.gt.strmax) return
        if (str(n:n).ne.' ') then
            nname = -1
            nnumb = -1
        endif
        return
        end
c
c ================================================================
c "ppause": Plot pause funtion.  Let user say when to continue.
c
c call as:
c       call ppause
c
c Original: Michael Eldredge -- Stanford University (jun 88)
       SUBROUTINE PPAUSE
      include 'p2conf.h'
       integer ierr
       write(6,11)
11     format(/' Hit RETURN to continue... ', $ )
       read(5,*, err=900, end=901, iostat=ierr)
       return
900    write(6,910)
910    format(' EOF found in PPAUSE read.')
       return
901    write(6,911) ierr
911    format(' ERR ',i7,' found in PPAUSE read.')
       return
       end

c ================================================================
c "ch2i": Character string to integer.
c
c call as:
c       i = ch2i(str)
c
c Original: Michael Eldredge -- Stanford University (oct 87)
c
        INTEGER FUNCTION CH2I(str)
      include 'p2conf.h'
          character*(*) str
c ----------------------------------------------------------------
        integer   i, n
        character c

        character*(10) digs
        data digs/'0123456789'/
c ----------------------------------------------------------------

        ch2i = 0
        i = 1
10      if (i.gt.LEN(str)) goto 20
        c = str(i:i)
        n = INDEX(digs, c)
        if (n.le.0) goto 20

        ch2i = 10*ch2i + n - 1
        i = i + 1
        goto 10

20      return
        end
c ================================================================
