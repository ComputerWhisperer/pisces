cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RECTM(lfdiag)
      include 'p2conf.h'
c
c     Generate rectangularly-connected mesh.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                common area
c
      include     'blank.h'
      include     'stat.h'
      include     'key.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c 
c                type declarations 
c 
      logical lupfix,leftrt,lfdiag
      integer numyup,numylo,igbnd
      real xloc,yuploc,yloloc,encroa,grad1,grad2,ygbnd
c 
c FUNCTIONS:
      logical gtkey
c****************************************************************** 
c 
c                   Initialize & get the basic rectangular mesh
      nb=0
      nmat=0
      nelect=0
      nrcd=0
      call square
      if (errflg) return
c                   Read auxiallary cards
c 
      goto 100
c                   return point if next card not fetched 
c                   terminate cleanly on eof, exit on error 
99    eofflg = .not.gtkey(keyid)
      errflg = eofflg
      if (errflg) return
      if (eofflg) goto 200
c                   return point if next card already fetched 
 100  if (keyid.eq.kcomme) goto 99
      if (keyid.eq.kregio) goto 1 
      if (keyid.eq.kelect) goto 2 
      if (keyid.eq.kspred) goto 3 
c
c                   not a rectangle-category card, finish the
c                   rectangular mesh details & exit
 200  lcrdrd=.true. 
c
c                    must have had region and electrode cards
c
      if (.not.(lregcd.and.lelecd)) call erset(18,linum,0)
      if (errflg) return
c 
c                   generate elements from rectangular grid
      call gnelm(lfdiag)
      return
c****************************************************************** 
c 
c                   region card 
c 
   1  call regck
      if (errflg) return
      goto 99 
c****************************************************************** 
c 
c                   electrode card
c 
   2  call eleck
      if (errflg) return
      goto 99 
c****************************************************************** 
c 
c                   spread card (vertical grid shifting)
c 
   3  call sprck(leftrt,xloc,numyup,numylo,yuploc,yloloc,lupfix,
     +                 encroa,grad1,igbnd,ygbnd,grad2) 
      if (errflg) return
c                   do the spreading
      call spred(leftrt,xloc,numyup,numylo,yuploc,yloloc,lupfix,
     +                 encroa,grad1,igbnd,ygbnd,grad2) 
      goto 99 
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SQUARE
      include 'p2conf.h'
c 
c     this routine generates a rectangular mesh based on a user specified 
c     subset of the whole mesh and the spacing ratio between nodes
c 
c     input:        sequence of x or y mesh cards with: 
c 
c                   node #, location, spacing ratio 
c 
c 
c     output:       x and y coordinates of every node in mesh 
c 
c                         10/18/78     s.y.oh 
c                         11/02/79     c.h.price
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'stat.h'
      include     'key.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lxyflg,lxdone,lydone,lxysta 
      integer node,i,nbx,nby,k,j 
      real floc,ratio
c 
c FUNCTIONS:
      logical gtkey
c****************************************************************** 
c 
c                   initialization
c 
      i=0 
      lxdone=.false.
      lydone=.false.
      lxysta=.false.
c                   preview card to see if its x or y mesh
10    eofflg = .not.gtkey(keyid)
      errflg = eofflg
      if (eofflg) call erset(10,linum,0)
      if (errflg) return
      if (keyid.eq.kcomme) goto 10
c                   lxysta is true for x mesh specifications
      if (keyid.eq.kxmesh) lxysta=.true.
      goto 1
c 
c                   x.mesh or y.mesh card expected
c 
2     eofflg = .not.gtkey(keyid)
      errflg = eofflg
      if  (eofflg) call erset(10,linum,0)
      if (errflg) return
      if(keyid.eq.kcomme) goto 2
c
1     i=i+1 
      call xymck(lxyflg,node,floc,ratio)
      if (errflg) return
c                   should be in proper state (x or y)
      if (lxysta.neqv.lxyflg) call erset(11,linum,0)
      if (errflg) return
c                   which is it?
      if (lxysta.eqv..false.) goto 3
c                   x mesh state
      ni(i)=node
      xi(i)=floc
      rx(i)=ratio 
c                   is this last node? if not loop back 
      if (node.ne.nx) goto 2
c                   done with x, set flags and jump to end
      lxdone=.true. 
      nbx=i 
      goto 4
c                   y mesh state
3     nj(i)=node
      yi(i)=floc
      ry(i)=ratio 
c                   is this the last node? if not loop back 
      if (node.ne.ny) goto 2
c                   done with y, set flags and see if all done
      lydone=.true. 
      nby=i 
4     if (lxdone.and.lydone) goto 5 
c                   one direction done prepare for other
      lxysta=.not.lxysta
      i=0 
      goto 2
c 
c                    all done, interpolate the specified subset of mesh 
5     call expnd(ni,xi,rx,xcord,nbx,nxymax) 
      call expnd(nj,yi,ry,ycord,nby,nxymax) 
      if (errflg) return
c 
c     handle the elimination of points
      call elimin
      if (errflg) return
c 
c     generate x,y => node number map and the coordinate array 
      k = 1 
      do 200 i=1,nx 
        do 200 j=1,ny 
          if (mshmap(i,j).lt.0) goto 200  
          mshmap(i,j) = k 
          cord(1,k)=xcord(i)
          cord(2,k)=ycord(j)
          k = k + 1 
200   continue
c 
      np = k - 1
      if (np.ne.(nx*ny)) lrect=.false.
c     check to make sure there are less than max nodes
      if (np.gt.npdim) call erset(69,linum,npdim) 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE XYMCK(lxyflg,node,floc,ratio)
      include 'p2conf.h'
c 
c                   x y mesh card check routine 
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'key.h'
      include     'stat.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lxyflg
      integer node,nxory
      real floc,ratio 
c
c FUNCTIONS:
      logical isrval
      real    gtrval
c
c 
c****************************************************************** 
c 
c                   check card type 
c 
      if (keyid.eq.kxmesh) goto 1 
      if (keyid.eq.kymesh) goto 2 
      call erset(23,linum,0)
      if (errflg) return 
c                   x mesh
1     lxmscd=.true. 
      lxyflg=.true. 
      nxory=nx
      goto 3
c                   y mesh
2     lymscd=.true. 
      lxyflg=.false.
      nxory=ny
c                   must have node, location and ratio
3     node=gtrval(1)
      floc=gtrval(2)
      ratio=gtrval(3) 
      if (.not.isrval(1).or. .not.isrval(2))
     +    call erset(24,linum,0)
      if (node.lt.1.or.node.gt.nxory.or.ratio.le.0.or.ratio.gt.10.)
     +    call erset(24,linum,0)
c                   convert microns to centimeters 
      floc=floc*1e-4
c                   done! 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE EXPND(ni,xi,rx,xcord,nbx,nxymx) 
      include 'p2conf.h'
c 
c     this routine expand input data and interpolate node 
c     coordinates 
c 
c     input : ni(nxymx),xi(nxymx),rx(nxymx),nbx,nxymx 
c 
c     output: xcord(nxymx) 
c 
c                   ni is array of node numbers 
c                   xi is array of node locations 
c                   rx is array of node spacing ratios
c                   nbx is number of entries in these arrays
c                   nxymx is dimension of these arrays 
c 
c                  09/07/78    s.y.oh 
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
c 
c                   type declarations 
c 
      integer ni(nxymx),nbx,nxymx,ii,is,ie,ies,is1,ie1,jj
      real xi(nxymx),rx(nxymx),xcord(nxymx),dl0,rt,dx,hxim
c 
c****************************************************************** 
c 
c***  expand input data to evaluate coordinates 
      xcord(1)=xi(1)
      do 100 ii=2,nbx 
      is=ni(ii-1) 
      ie=ni(ii) 
      ies=ie-is 
      if(ies.gt.0) goto 1 
      write(6,*) ' index ',ii,ni(ii-1),ni(ii),ies
      call erset(12,linum,0)
      return
1     dl0=xi(ii)-xi(ii-1) 
      if(dl0.gt.0.0) goto 2 
      write(6,*) ' loc ',ii,xi(ii-1),xi(ii),dl0
      call erset(12,linum,0)
      return
c 
2     xcord(ie)=xi(ii)
      if(ies.le.1) go to 100
c 
      rt=rx(ii) 
      ie1=ie-1
      is1=is+1
      if(rt.eq.1.0.or.rt.le.0.0) go to 150
c 
      hxim=dl0*(1.-rt)/(1.-rt**ies) 
      do 110 jj=is1,ie1 
      xcord(jj)=xcord(is)+hxim*(1-rt**(jj-is))/(1.-rt)
110   continue
      go to 100 
c 
150   dx=dl0/ies
      do 160 jj=is1,ie1 
160   xcord(jj)=xcord(is)+dx*(jj-is)
100   continue
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SPRCK(leftrt,xloc,numyup,numylo,yuploc,yloloc,lupfix,
     +                 encroa,grad1,igbnd,ygbnd,grad2) 
      include 'p2conf.h'
c 
c 
c     This routine gets and checks the input from the spread card 
c
c     Original : CHP
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'stat.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c 
c                   type declarations 
c 
      logical leftrt,lupfix,lfixlo
      integer numyup,numylo,ncnt,npoint,igbnd
      real xloc,width,yuploc,yloloc,thick,volrat,encroa,grad1,grad2,
     +     ygbnd,erfc80,cthick,dthick,uthick
c 
c FUNCTIONS:
      logical isrval
      real    gtrval
      logical gtlval
c
      data erfc80/1.812386/ 
c 
c****************************************************************** 
c 
c                   start 
c 
      lupfix=.false.
c                   must have had x and y mesh but no doping
      if (.not.lxmscd.or..not.lymscd.or.ldopcd) call erset(98,linum,0)
      if (errflg) return
c 
c                   left or right side but not both 
      ncnt=0
      if (.not.gtlval(1)) goto 10 
      leftrt=.true. 
      ncnt=ncnt+1 
10    if (.not.gtlval(2)) goto 20 
      leftrt=.false.
      ncnt=ncnt+1 
20    if (ncnt.ne.1) call erset(99,linum,0) 
      if (errflg) return
c 
c                   get width, convert to centimeters and check
      width=gtrval(1)*1e-4
      if (width.lt.0.) call erset(100,linum,0)
      if (errflg) return
c 
c                   left or right 
      if (leftrt) goto 50 
c 
c                   right, get node pointer and transition midpoint 
      npoint=nx 
      xloc=cord(1,mshmap(nx,1)) 
      xloc=xloc-width 
      goto 100
c 
c                   left, get node pointer and transition midpoint
50    npoint=1
      xloc=cord(1,mshmap(npoint,1)) 
      xloc=xloc+width 
c 
c                   both must be specified
100   if (.not.isrval(2) .or. .not.isrval(3)) call erset(101,linum,0) 
      if (errflg) return
c
c                   get line numbers of placed lines. 
      numyup=gtrval(2)
      numylo=gtrval(3)
c 
c 
c                   check validity
      if (numyup.lt.1.or.numylo.gt.ny.or.numyup.ge.numylo)
     +    call erset(102,linum,0) 
c 
c                   get current spacing of these lines at this edge 
      cthick=cord(2,mshmap(npoint,numylo))-cord(2,mshmap(npoint,numyup))
c 
c                   get new line locations
c 
      yloloc=gtrval(4)
      lfixlo=gtlval(3)
      thick=gtrval(5) 
      volrat=gtrval(6)
c 
c                   low location spec? - can be specified by 
c                   location or by fixing at previous value
      if (isrval(4)) goto 148 
      if (.not.lfixlo) goto 150
      yloloc=cord(2,mshmap(npoint,numylo))
      goto 149
148   yloloc=yloloc*1e-4
149   continue
c                   yes, then thick must not be spec. 
cMJE: was if .not.have(thickness) which is wrong.
      if (isrval(5)) call erset(103,linum,0) 
      if (errflg) return
c                   all ok, get locations and set upper-fixed flag
      lupfix=.true. 
      yuploc=cord(2,mshmap(npoint,numyup))  
      thick=yloloc-yuploc 
      goto 200
c 
c                   thickness must be specified 
c 
150   if (.not.isrval(5)) call erset(104,linum,0) 
      if (errflg) return
c                   check volume ratio
      if (volrat.lt.0..or.volrat.gt.1.) call erset(105,linum,0) 
      if (errflg) return
c                   check and convert new thickness 
      thick=thick*1e-4
      if (thick.le.0.) call erset(106,linum,0)
      if (errflg) return
c                   get up and down fractions 
      dthick=volrat*(thick-cthick)
      uthick=thick-dthick-cthick
c                   compute locations 
      yuploc=cord(2,mshmap(npoint,numyup))-uthick 
      yloloc=cord(2,mshmap(npoint,numylo))+dthick 
c 
c                   check location validity 
c 
200   if (yloloc.lt.yuploc.or.yloloc.ge.cord(2,mshmap(npoint,ny)))  
     +    call erset(107,linum,0) 
      if (errflg) return
c 
c                   get encroachment param. and check 
c 
      encroa=gtrval(7)
      if (encroa.lt..1) call erset(108,linum,0) 
c                   scale so that 80% of thickness change occurs in a 
c                   distance equal to the thickness change (for an
c                   encroachment factor of 1) 
      if(thick.ne.cthick) encroa=abs(erfc80/((thick-cthick)*encroa))
c 
c                   get grading parameter(s)
      grad1=gtrval(8)
      grad2=gtrval(9)
      igbnd=gtrval(10)
      ygbnd=gtrval(11)

      if(.not.isrval(10) .and. .not.isrval(11)) goto 999
      if(.not.isrval(10)) call erset(170,linum,0)
      if(.not.isrval(11)) call erset(171,linum,0)
      if((igbnd.ge.numylo).or.(igbnd.le.numyup)) 
     +     call erset(172,linum,0)
      if((ygbnd.ge.1.).or.(ygbnd.le.0.)) 
     +     call erset(173,linum,0)
c 
c                   done
999   return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SPRED(leftrt,xloc,numyup,numylo,yuploc,yloloc,lupfix,
     +                 encroa,grad1,igbnd,ygbnd,grad2) 
      include 'p2conf.h'
c 
c                   this routine performs the spreading of lines. 
c                   lines above the upline are moved up with upline 
c                   lines below loline are squashed between the bottom
c                   if lupfix is true, upline is not moved
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c 
c****************************************************************** 
c 
c                   type declarations 
c 
      logical leftrt,lupfix 
      integer numyup,numylo,npoint,i,j,nodnum(3),igbnd,igpnt
      real xloc,yuploc,yloloc,encroa,spmdol,spmdnw,xco,yco,
     +     spbtol,spbtnw,deltup,deltlo,erfval,yupold,yupnew,
     +     yloold,ylonew,ybot,rat,erfarg,grad1,ygbnd,grad2,
     +     yloc2(3),gratio(3),ycord2(30),ycordg,erfvl2,erfar2,
     +     yupanc,yloanc,ybtanc,erfc
c 
c****************************************************************** 
c 
c                   start 
c     initialize y up ancient, y lo ancient, y bottom ancient by finding
c     the first valid node on the line from the left edge 
      ybtanc = cord(2, mshmap(1,ny))  
      i = 1 
10    if (i.ge.nx.or.mshmap(i, numyup).gt.0) goto 20 
        i = i + 1 
        goto 10
20    yupanc = cord(2, mshmap(i, numyup)) 
      i = 1 
30    if (i.ge.nx.or.mshmap(i, numylo).gt.0) goto 40 
        i = i + 1 
        goto 30
40    yloanc = cord(2, mshmap(i, numylo)) 
c 
c                   scan x lines
      do 900 npoint=1,nx
c 
c                   get pointer to last node of prev. col.
c 
c                   get x coord 
      j = 1 
50    if (j.gt.ny.or.mshmap(npoint, j).gt.0) goto 60 
        j = j + 1 
        goto 50
c     the entire line was deleted 
60    if (j.eq.(ny+1)) goto 900 
      xco=cord(1,mshmap(npoint,j))  
c 
c                   evaluate error function for this coord. 
      erfarg=(xco-xloc)*encroa
      erfar2=erfarg-.6
      if (.not.leftrt) erfar2=erfarg+.6 
      erfar2=erfar2*1.5 
      if (leftrt) erfval=erfc(erfarg) 
      if (leftrt) erfvl2=erfc(erfar2) 
      if (.not.leftrt) erfval=erfc(-erfarg) 
      if (.not.leftrt) erfvl2=erfc(-erfar2) 
      erfval=erfval*.5
      erfvl2=erfvl2*.5
c 
c                   compute new node locations on this column 
c 
c                   get upper, lower, bottom current loc. 
      if (mshmap(npoint,numyup).gt.0) then  
        yupanc = yupold 
        yupold = cord(2,mshmap(npoint,numyup))  
        endif 
      if (mshmap(npoint,numylo).gt.0) then  
        yloanc = yloold 
        yloold = cord(2,mshmap(npoint,numylo))  
        endif 
      if (mshmap(npoint,ny).gt.0) then  
        ybtanc = ybot 
        ybot = cord(2,mshmap(npoint,ny))  
        endif 
c 
c                   compute upward shift and downward 
      deltup=erfval*(yupold-yuploc) 
      deltlo=erfval*(yloloc-yloold) 
c 
c                   if upper is fixed, set upward shift to 0
      if (lupfix) deltup=0. 
c 
c                   compute new locations 
      yupnew=yupold-deltup
      ylonew=yloold+deltlo
c 
c                   compute old and new spreads of middle, bottom 
      spmdol=yloold-yupold
      spmdnw=spmdol+deltup+deltlo 
      spbtol=ybot-yloold
      spbtnw=spbtol-deltlo
c 
c                   scan y nodes and move as necessary
c 
      do 800 j=1,ny 
c 
c                   get node number and y-coord.
      if (mshmap(npoint, j).le.0) goto 800  
      yco=cord(2,mshmap(npoint,j))  
c 
c                   consider top, middle, and bottom regions
      if (j.gt.numyup.and.j.lt.numylo) goto 400 
      if (j.ge.numylo) goto 600 
c 
c                   top region (j.le.numyup) shift all by deltup
      yco=yco-deltup
      goto 700
c 
c                   middle region spread proportionally unless new grading
c                   is requested. 
400   rat=(yco-yupold)/spmdol 
      yco=yupnew+rat*spmdnw 
c 
c                   new grading requested?
      if (grad1.eq.-999.) goto 700 
c 
c                   yes. first time in middle?
      if (j.ne.numyup+1) goto 500 
c 
c                   yes, get new grading. 
      nodnum(1)=1 
      yloc2(1)=yupnew 
      if(igbnd.gt.0) then
         igpnt=3
         nodnum(2)=igbnd-numyup+1 
         nodnum(3)=numylo-igbnd+nodnum(2) 
         yloc2(2)=ygbnd*(ylonew-yupnew)+yupnew
      else
         igpnt=2
         nodnum(2)=numylo-numyup+1 
      endif
      yloc2(igpnt)=ylonew 
      gratio(1)=0.
      gratio(2)=grad1
      gratio(3)=grad2
      call expnd(nodnum,yloc2,gratio,ycord2,igpnt,30) 
      if(errflg) return
c 
c                   get postion based on new grading
500   ycordg=ycord2(j-numyup+1) 
c 
c                   vary from new grading to proportional grading 
c                   based on erfvl2 (2/3 the spread of erfval and 
c                   centered at 30% point of erfval instead of
c                   50% point.
      yco=yco+erfvl2*(ycordg-yco) 
      goto 700
c 
c                   bottom region, spread proportionally
600   rat=(yco-yloold)/spbtol 
      yco=ylonew+rat*spbtnw 
c 
c                   set new y-coord., next node 
700   cord(2,mshmap(npoint,j))=yco  
800   continue
c 
c                   next column 
900   continue
c 
c                   done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ELIMIN 
      include 'p2conf.h'
c 
c 
c  This subroutine reads in the node areas and marks nodes as out of the
c  array space.  The parameters provided by genii on the elim card are
c 
c  nxlo,nxhi,nylo,nyhi   the area in which nodes are to be marked out 
c  dir                   the direction in which to mark nodes out 
c                        true marks out in the y direction
c 
c       9/3/82     mark e. law
c       4/1/84     MRP (modified bounds)
c
c     Copyright c 1982 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'key.h'
      include     'stat.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c           type declarations 
c 
      integer nxlo,nxhi,nylo,nyhi,i,j 
      logical elim,test,dir 
c 
c FUNCTIONS:
      logical gtkey
c**************************************************************************** 
c 
c 
c...Can not do eliminate if this is not a rectangular mesh 
      if(lrect) goto 5
      call erset(135,linum,0)
      return
c
c     zero out the map array, and set start flags for initialization
5     do 1000 i = 1,nx
        do 1000 j = 1,ny
          start(i,j) = .false.
          mshmap(i,j) = 0 
1000    continue
c 
c     make it illegal to eliminate corner nodes 
      start(1,1) = .true. 
      start(1,ny) = .true.
      start(nx,1) = .true.
      start(nx,ny) = .true. 
c 
c     read in the next card 
1     eofflg = .not.gtkey(keyid)
      errflg = eofflg
      if (errflg.or.eofflg) goto 999
c 
c     if this is a comment card loop back
      if (keyid.eq.kcomme) goto 1
c
c     if this is not an elimination card return 
      if (keyid.ne.kelim) then
         lcrdrd=.true.
         goto 999
      endif
c 
      call elimck(nxlo, nxhi, nylo, nyhi, dir)
      if (errflg) goto 999
c 
c 
c     separate out x going and y going deletes
      if (dir) goto 10
c 
c     x deletions here .... 
c 
c     set the elimination flag true, so first legal row can be eliminated 
      elim = .true. 
c 
c     repeat over for each row in the y direction 
      do 100 i=nylo,nyhi
c 
c       check the row to see if it has deletions in it
        test = .false.
        do 110 j=nxlo,nxhi
110       test = test.or.(mshmap(j,i).lt.0) 
c 
c         if there are no deletions and row is not along an edge
          if (.not.test.and.(i.ne.1).and.(i.ne.ny)) then
c 
c           if it is a elimation turn find legal start/stop points and do it
            if (elim) then
c 
c             find the start location 
300           if (nxlo.ge.nxhi.or..not.start(nxlo,i)) goto 310
                nxlo = nxlo + 1 
                goto 300
c 
c             find a legal end location 
310           if (nxhi.le.nxlo.or..not.start(nxhi,i)) goto 320
                nxhi = nxhi - 1 
                goto 310
c 
c             make sure there are legal points to waste 
320           if (.not.start(nxlo,i)) then
                do 120 j = nxlo, nxhi 
120               mshmap(j,i) = -1  
c               set the start flags (MRP rev)
                if(nxlo.gt.1)  call setstr(nxlo,i) 
                if(nxhi.lt.nx) call setstr(nxhi,i) 
              endif 
            endif 
c           toggle the elimination flag 
            elim = .not.elim
          endif 
c 
100     continue
c 
c     go read the next card to see if there is any more elimination to do 
      goto 1
c 
c 
c     y deletions are here
c     set the elimination flag to true
10    elim = .true. 
c 
c     repeat for each row 
      do 200 i=nxlo,nxhi
c 
c       check the row to see if it has deletions in it
        test = .false.
        do 210 j=nylo,nyhi
210       test = test.or.(mshmap(i,j).lt.0) 
c 
c         if there are no deletions and is not along and edge 
          if (.not.test.and.(i.ne.1).and.(i.ne.nx)) then
c 
c           if it is a elimation turn find legal start/stop points and do it
            if (elim) then
c 
c             find the start location 
400           if (nylo.ge.nyhi.or..not.start(i,nylo)) goto 410
                nylo = nylo + 1 
                goto 400
c 
c             find a legal end location 
410           if (nyhi.le.nylo.or..not.start(i,nyhi)) goto 420
                nyhi = nyhi - 1 
                goto 410
c 
c             make sure there are legal points to waste 
420           if (.not.start(i,nyhi)) then
                do 220 j = nylo, nyhi 
220               mshmap(i,j) = -1  
c               set the start flags (MRP rev)
                if(nylo.gt.1) call setstr(i,nylo) 
                if(nyhi.lt.ny) call setstr(i,nyhi) 
              endif 
            endif 
c           toggle the elimination flag 
            elim = .not.elim
          endif 
200         continue
c 
c     done with this card see if there are any more 
        goto 1
c 
999   return
cdebug      write(ludia,1001) errflg, eofflg, keyid
cdebug1001  format (2l5,i5)
      end 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SETSTR(x,y)
      include 'p2conf.h'
c 
c 
c     this subroutine sets the start flags for given node.  the start array 
c     is kept to keep track of legal starting locations for a elimation 
c     row.  another row cannot start to the left, right, up or, down
c     direction from the any row that has already been deleted. 
c 
c 
c     9/3/82           mark e. law
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c*****************************************************************************
c 
c                           type declarations 
c 
      integer x,y,i,j 
c 
c*****************************************************************************
c 
c     find first existing node to the left and set start flag 
      if (x.ne.1) then
        i = x-1 
10      if (i.le.1.or.(mshmap(i,y).ge.0))  goto 20
          i = i - 1 
          goto 10
20      start(i,y) = .true. 
        endif 
c 
c     find first node to the right and set flag 
      if (x.ne.nx) then 
        i = x + 1 
30      if (i.ge.nx.or.(mshmap(i,y).ge.0)) goto 40 
          i = i + 1 
          goto 30
40      start(i,y) = .true. 
        endif 
c 
c     find first node below and set flag
      if (y.ne.ny) then 
        j = y + 1 
50      if (j.ge.ny.or.(mshmap(x,j).ge.0)) goto 60 
          j = j + 1 
          goto 50
60      start(x,j) = .true. 
        endif 
c 
c     find first node above and set flag
      if (y.ne.1) then
        j = y - 1 
70      if (j.le.1.or.(mshmap(x,j).ge.0)) goto 80 
          j = j - 1 
          goto 70
80      start(x,j) = .true. 
        endif 
c 
c     mark out starting node itself 
      start(x,y) = .true. 
c 
      return
      end 
c 
c 
c 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c 
      SUBROUTINE ELIMCK(nxlo, nxhi, nylo, nyhi, dir)
      include 'p2conf.h'
c 
c     this sunroutine reads the information out of the genii arrays into
c     the named variables.  it does checking to be sure all specified 
c     variables are in range. 
c 
c     9/3/82          mark e. law 
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
  
c                   common areas
c 
      include     'blank.h'
c 
c**************************************************************************** 
c 
c                                type declarations
c 
      integer nxlo, nxhi, nylo, nyhi
      logical dir 
c
c FUNCTIONS:
      real    gtrval
      logical gtlval
c 
c**************************************************************************** 
c 
c     get the values and then do checking 
      nxlo = gtrval(1)
      nxhi = gtrval(2)
      nylo = gtrval(3)
      nyhi = gtrval(4)
c 
c     check the x values for errors 
      if ((nxlo.lt.1).or.(nxlo.gt.nx)) then 
        call erset(-231,linum,0)
        nxlo = 1
        endif 
      if ((nxhi.lt.1).or.(nxhi.gt.nx)) then 
        call erset(-232,linum,0)
        nxhi = nx 
        endif 
      if (nxlo.gt.nxhi) call erset(233,linum,0) 
c 
c     check the y values
      if ((nylo.lt.1).or.(nylo.gt.ny)) then 
        call erset(-231,linum,0)
        nylo = 1
        endif 
      if ((nyhi.lt.1).or.(nyhi.gt.ny)) then 
        call erset(-232,linum,0)
        nyhi = ny 
        endif 
      if (nylo.gt.nyhi) call erset(197,linum,0) 
c 
c     check for legal directions and set dir flag 
      if (gtlval(1).and.gtlval(2)) call erset(237,linum,0)
      if ((.not.gtlval(1)).and.(.not.gtlval(2))) 
     +       call erset(244,linum,0)
      dir = gtlval(2) 
c 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE GNELM(lfdiag)
      include 'p2conf.h'
c 
c 
c     this subroutine is responsible for calculating element information. 
c     it scans the grid and looks for the smallest rectangle based to 
c     the lower right of the current node.  upon finding it, it checks to 
c     if there are more than one node internal to the rectangle.  if
c     there is, it flags an error.  if there is one or no internal nodes
c     it generates the proper triangles.
c 
c     8/29/82          mark e. law
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c                   common areas
c 
      include     'blank.h'
      include     'symb.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c**************************************************************************** 
c 
c                           type declarations 
c 
      integer pos(2),fdwn(2),fsid(2),sdwn(2),ssid(2),corn(2), 
     +      endsid(2),enddwn(2),clos(2),far(2),i,j,k
      logical any,mult,lfdiag
c 
c     variable definitions
c     done  flags when a small rectangle has had its area assigned
c           to an element.  prevents nodes in the middle of a five
c           point scheme from creating errors 
c     pos   marks the location of the cureent node being checked
c     fdwn, sdwn mark the location of the first and second real nodes 
c                   below the current one repectively.
c     fsid, ssid mark the location of nodes to the side like fdwn, sdwn 
c     corn    marks the location of the corner of the smallest rect.
c     endsid  limit to look to for nodes to the side
c     enddwn  limit to look to for nodes below
c     clos    marks the closest of the corners of the rectangles made by
c             pos,fdwn,ssid; pos,sdwn,fsid
c     far     farthest of the corners mentioned in close
c     any     flag for the existence of nodes below or to side
c     mult    flag for the existence of more than one node to side or below 
c 
c 
c     equivalence pos, and i,j so that pos can be used as loop counter
      equivalence (pos(1),i),(pos(2),j) 
c 
c**************************************************************************** 
c 
c     set the current number of elements equal to zero
      ne = 0
c 
c     initialize the done array as all false
      do 200 i=1,nx 
        do 200 j=1,ny 
200       done(i,j) = .false. 
c 
c     repeat for all the elements that have area to the lower right 
      do 100 i = 1,nx-1 
        do 100 j = 1,ny-1 
c 
c 
c         if this is a non-existent position, or rectangle is done
c         skip to the next rectangle
          if ((mshmap(i,j).lt.0).or.done(i,j)) goto 100 
c 
c         set up the ending positions to search to
          endsid(1) = nx
          endsid(2) = pos(2)
          enddwn(1) = pos(1)
          enddwn(2) = ny
c 
c         search for the first and second real nodes to the right 
          call search(pos, endsid, fsid, ssid, any, mult, .true.) 
c 
c         if their are no nodes that way, advance 
          if (.not.any) goto 100
c 
c         search for the first and second real nodes below this node
          call search(pos, enddwn, fdwn, sdwn, any, mult, .true.) 
c 
c         if their are no nodes below advance to next node
          if (.not.any) goto 100
c 
c 
c         check to see if corner opposite the firsts exists 
          if (mshmap(fsid(1), fdwn(2)).gt.0) then 
c           it does exist, create corner pair and call doelm
            corn(1) = fsid(1) 
            corn(2) = fdwn(2) 
            call doelm(pos, fsid, corn, fdwn, lfdiag)
c 
c         since it doesn't, find closer second corner and check those 
          else
            call clschk(pos, fsid, fdwn, ssid, sdwn, clos, far) 
c 
c           see if the closer pair exists, if so, then doelm
            if (clos(1).le.0) then
            write(6,6) pos(1),pos(2)
6             format('error number 239',2i5)
              call erset(239,linum,0) 
            else
              if (mshmap(clos(1),clos(2)).gt.0) then  
                call doelm(pos, fsid, clos, fdwn, lfdiag)
              else
                if (far(1).le.0.or.(mshmap(far(1),far(2)).lt.0)) then 
                  call erset(239,linum,0) 
          write(6,6) pos(1),pos(2)
                else
                  call doelm(pos, ssid, far, sdwn, lfdiag)
                endif 
              endif 
            endif 
          endif 
c 
c     ready for the next node 
100   continue
c 
c     run through the done array to make sure all area is included
      do 300 i=1,nx-1 
        do 300 j=1,ny-1 
          k = mshmap(i,j)
        if (.not.done(i,j)) call erset(238,linum,k) 
300       continue
c 
c     check to make sur enumber of elements is les than maximum 
      if (ne.gt.nedim) call erset(70,linum,nedim) 
c 
      return
      end 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SEARCH(strt, end, frst, scnd, any, mult, corn)
      include 'p2conf.h'
c 
c 
c     this subroutine searches the nodes between start and end for those
c     that are real.  it returns its first match in frst, and second in 
c     scnd.  any is a flag indicating the existence of nodes in the range,
c     and mult is flag for more than one node in the range.  corn tells 
c     search whether or not nodes located on the corner of the area are 
c     considered legal matches. 
c 
c 
c     9/1/82       mark e. law
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c*****************************************************************************
c 
c                            type declarations
c 
      integer strt(2),end(2),frst(2),scnd(2),i,j 
      logical any,mult,corn 
c 
c*****************************************************************************
c 
c     set up flags as false initially and the frst, scnd nodes as non-exist 
      any = .false. 
      mult = .false.
      frst(1) = -1
      scnd(1) = -1
c 
c     repeat for each node in the range 
      do 100 i = strt(1),end(1)
        do 100 j = strt(2),end(2)
c 
c         if this is a corner node and corners don't count skip to next node
          if (.not.corn.and.(i.eq.strt(1).or.i.eq.end(1)).and. 
     +       (j.eq.strt(2).or.j.eq.end(2))) goto 100 
c 
c         if we are at the initial node goto to end regardless of corn flag 
          if (i.eq.strt(1).and.j.eq.strt(2)) goto 100 
c 
c         if the node exists, set proper flag and store in frst or scnd 
          if (mshmap(i, j).gt.0) then 
            if (.not.any) then
              frst(1) = i 
              frst(2) = j 
            else if (.not.mult) then
                   scnd(1) = i
                   scnd(2) = j
            endif 
            mult = any
            any = .true.
          endif 
c 
100       continue
      return
      end 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE DOELM(pos, side, corn, dwn, lfdiag)
      include 'p2conf.h'
c 
c     this subroutine is responsible for deciding the element connections 
c     neccessary for the rectangle formed by the four nodes passed.  it 
c     first checks to see if their is a fifth node inside. if so, it
c     determines if that node is coline, and acts accordingly.
c 
c     Original M.E.Law Aug 82
c     Modified CSR     Mar 84 (Calculate element regions on the fly)
c     Modified MRP     Mar 84 (no diagonal flip)
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c**************************************************************************** 
c 
c                        type declarations
c 
      integer pos(2), side(2), corn(2), dwn(2), point(2,4), fifth(2), 
     +    i, j, temp(2) ,idmod
      logical any, mult, coll, coline , lfdiag
c 
c*****************************************************************************
c 
c     define a statement function that allows a circular count through
c     the sides of a rectangle
      idmod(i) = mod(i-1,4) + 1
c 
c 
c     set the point array equal to the values passed
      point(1,1) = pos(1) 
      point(2,1) = pos(2) 
      point(1,2) = side(1)
      point(2,2) = side(2)
      point(1,3) = corn(1)
      point(2,3) = corn(2)
      point(1,4) = dwn(1) 
      point(2,4) = dwn(2) 
c 
c 
c     search through the rectangle block for a fifth node 
      call search(pos, corn, fifth, temp, any, mult, .false.) 
c 
c     if there are more than one internal node flag an error
      if (mult) then
        call erset(239,linum,0) 
        return
        endif 
c 
c     set done equal to true for the entire rectangle since no errors 
      do 200 i = pos(1),corn(1)-1 
        do 200 j = pos(2),corn(2)-1 
200       done(i,j) = .true.
c 
c 
c     if there is a fifth point handle the five point case
      if (any) then 
c       there is a fifth point, determine if it is coline with a side 
        i = 1 
        coll = .false.
10      if (i.gt.4.or.coll) goto 20
          j = idmod(i + 1) 
          coll = coline(point(1,i),point(2,i),point(1,j),point(2,j),
     +                 fifth) 
          i = i + 1 
          goto 10
c 
20      i = i - 1 
c       i,j now hold the points that end up the coline side 
c 
c       if the point is coline the rectangle requires three elements
c       else it requires four 
c 
        if (coll) then
c 
c         three element case
          ne = ne + 1 
          nop(1,ne) = mshmap(point(1,i),point(2,i)) 
          nop(2,ne) = mshmap(point(1,idmod(j+2)),point(2,idmod(j+2))) 
          nop(3,ne) = mshmap(fifth(1),fifth(2)) 
        call setrg( ne,point(1,i),point(2,i),
     +                   point(1,idmod(j+2)),point(2,idmod(j+2)),
     +                   fifth(1),fifth(2) ) 

c 
          ne = ne + 1 
          nop(1,ne) = mshmap(point(1,j),point(2,j)) 
          nop(2,ne) = mshmap(point(1,idmod(j+1)),point(2,idmod(j+1))) 
          nop(3,ne) = mshmap(fifth(1),fifth(2)) 
        call setrg( ne,point(1,j),point(2,j),
     +                   point(1,idmod(j+1)),point(2,idmod(j+1)),
     +                   fifth(1),fifth(2) ) 
c 
          ne = ne + 1 
          nop(1,ne) = mshmap(point(1,idmod(j+1)),point(2,idmod(j+1))) 
          nop(2,ne) = mshmap(point(1,idmod(j+2)),point(2,idmod(j+2))) 
          nop(3,ne) = mshmap(fifth(1),fifth(2)) 
        call setrg( ne,point(1,idmod(j+1)),point(2,idmod(j+1)),
     +                   point(1,idmod(j+2)),point(2,idmod(j+2)),
     +                   fifth(1),fifth(2) )
c 
c 
          else
c           four point case 
c           repeat for each combination of the sides and fifth point
            do 150 i = 1, 4 
              ne = ne + 1 
              nop(1,ne) = mshmap(point(1,i),point(2,i)) 
              nop(2,ne) = mshmap(point(1,idmod(i+1)),
     +                           point(2,idmod(i+1)))
              nop(3,ne) = mshmap(fifth(1),fifth(2)) 
            call setrg( ne,point(1,i),point(2,i),
     +                    point(1,idmod(i+1)),point(2,idmod(i+1)),
     +                    fifth(1),fifth(2) )
150           continue
          endif 
c 
c 
c       this handles the case of no fifth point 
        else
c         call subroutine to properly handle this particular mess 
          call fourpt(pos,side,corn,dwn,lfdiag)
        endif 
      return
      end 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE FOURPT(pos,side,corn,dwn,lfdiag)
      include 'p2conf.h'
c
c     this subroutine handles the element division for a simple?
c     square
c 
c     Original MEL 9/12/82 
c     Modified CSR Mar 84 (element region #s on the fly)
c     Modified MRP Mar 85 (no diagonal flipping)
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c 
c                   common areas
c 
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c 
c***************************************************************************
c 
c                type declarations
      logical lfdiag
      integer pos(2),side(2),corn(2),dwn(2)
      real diag1,diag2 
c 
c***************************************************************************
c 
c     decide which split makes less of an obtuse angle.  the diagonal 
c     with the shorter y distance is the one that will do 
      diag1 = abs(cord(2,mshmap(pos(1),pos(2)))-
     +              cord(2,mshmap(corn(1),corn(2))))
      diag2 = abs(cord(2,mshmap(side(1),side(2)))-
     +              cord(2,mshmap(dwn(1),dwn(2))))
c 
      if (diag1-diag2) 15,12,20 
c     if (.not.lfdiag) then
c         if (diag1-diag2) 15,12,20 
c     endif
  
12        if(lfdiag.and.(1.0e4*cord(1,mshmap(pos(1),pos(2))).lt. 
     +       0.999 * ((1.0e4*cord(1,mshmap(nx,1)) + 
     +              1.0e4*cord(1,mshmap(1,1)))/2.))) goto 20

c 
c 
15        ne = ne + 1 
          nop(1,ne) = mshmap(pos(1),pos(2)) 
          nop(2,ne) = mshmap(side(1),side(2)) 
          nop(3,ne) = mshmap(corn(1),corn(2)) 
        call setrg(ne,pos(1),pos(2),side(1),side(2),corn(1),corn(2))

c 
          ne = ne + 1 
          nop(1,ne) = mshmap(pos(1),pos(2)) 
          nop(2,ne) = mshmap(corn(1),corn(2)) 
          nop(3,ne) = mshmap(dwn(1),dwn(2)) 
        call setrg( ne,pos(1),pos(2),corn(1),corn(2),dwn(1),dwn(2) )
          goto 999
c 
c 
20        ne = ne + 1 
          nop(1,ne) = mshmap(pos(1),pos(2)) 
          nop(2,ne) = mshmap(side(1),side(2)) 
          nop(3,ne) = mshmap(dwn(1),dwn(2)) 
        call setrg(ne,pos(1),pos(2),side(1),side(2),dwn(1),dwn(2))
c 
          ne = ne + 1 
          nop(1,ne) = mshmap(side(1),side(2)) 
          nop(2,ne) = mshmap(corn(1),corn(2)) 
          nop(3,ne) = mshmap(dwn(1),dwn(2)) 
        call setrg(ne,side(1),side(2),corn(1),corn(2),dwn(1),dwn(2))
999       return
      end 
c 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CLSCHK(pos, fsid, fdwn, ssid, sdwn, clos, far) 
c 
c 
c     this routine checks the second corners to see which is closer.
c     it also checks for the existence of the second points.  on
c     finding the closer pair it reorients the frst, scnd pairs so
c     that both frst correspond to the closer corners.  for example,
c     if the rectangle fsid,sdwn,pos is closer :
c           close contains the corner sdwn(1), fsid(2)
c           fsid remains the same 
c           fdwn now contains the values of sdwn vice-versa 
c 
c     9/2/82       mark e. law
c      
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c 
      integer pos(2), fsid(2), fdwn(2), sdwn(2), ssid(2), clos(2),
     +       far(2), norm1, norm2
c------------------------------------------------------------------
c 
c 
c     the following is fortran's version of a case statement==>else if
c 
c     check to make sure at least one of the seconds exist
      if (ssid(1).le.0.and.sdwn(1).le.0) then 
c       since neither exists, return as no closer pair
        clos(1) = -1
        far(1) = -1 
c 
c     check for second side existence 
      else if (ssid(1).le.0) then 
c            make only existing corner close, far non existent, swap downs
             clos(1) = fsid(1)
             clos(2) = sdwn(2)
             far(1) = -1
             call swap(fdwn,sdwn) 
c 
c     check for the second down existence 
      else if (sdwn(1).le.0) then 
c            make only existing corner close, far non existent, swap sides
             clos(1) = ssid(1)
             clos(2) = fdwn(2)
             far(1) = -1
             call swap(fsid,ssid) 
c 
c     if here, they both exist, calculate norm and find closer pair 
      else
c 
c       calaculate the 2 norm for both corners
        norm1 = (fsid(1)-pos(1))**2 + (sdwn(2)-pos(2))**2 
        norm2 = (ssid(1)-pos(1))**2 + (fdwn(2)-pos(2))**2 
c 
c       choose the close corner, make it close, the farther one goes into 
c       far, and swap the points that correspond
c 
        if (norm1.le.norm2) then
          clos(1) = fsid(1) 
          clos(2) = sdwn(2) 
          far(1) = ssid(1)
          far(2) = fdwn(2)
          call swap(fdwn,sdwn)
c 
        else
          clos(1) = ssid(1) 
          clos(2) = fdwn(2) 
          far(1) = fsid(1)
          far(2) = sdwn(2)
          call swap(fsid,ssid)
c 
        endif 
      endif 
      return
      end 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      LOGICAL FUNCTION COLINE(x1, x2, y1, y2, test) 
      include 'p2conf.h'
c 
c 
c     this function returns true if the point test is on the line formed
c     by the points x1,x2 ; y1,y2.
c     this uses the formula : test = lambda*x + (1-lambda)*y
c     where test,x,y are vectors and lambda is a scalar 
c 
c     9/2/82       mark e. law
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c**************************************************************************** 
c 
c                               type declarations 
c 
      integer x1, x2, y1, y2, test(2) 
c 
c*****************************************************************************
c 
c     if horizontal x and y trap divide by zero 
      coline = .false.
      if (x1.eq.y1) coline = test(1).eq.x1
      if (x2.eq.y2) coline = test(2).eq.x2
      return
      end 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SWAP(a,b)
c 
c     this subroutine swaps the 2 poitn arrays a and b
c 
c     9/2/82       mark e. law
c 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c 
c 
      integer a(2),b(2),temp(2) 
c 
      temp(1) = a(1)
      temp(2) = a(2)
      a(1) = b(1) 
      a(2) = b(2) 
      b(1) = temp(1)
      b(2) = temp(2)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SETRG(ie,i1,j1,i2,j2,i3,j3)
      include 'p2conf.h'
c
c......Set the region numbers of elements generated in the triangular
c......mesh.
c
c     Original :      CSR Feb 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......Common area
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------

c......Local types
      integer i1,j1,i2,j2,i3,j3,ir,ie
      real avi,avj

c******************** Start ********************************************
c
c......Find the average i-j coordinates of this triangle.
      avi = float(i1+i2+i3) / 3.0e0
      avj = float(j1+j2+j3) / 3.0e0

c......Find the region card which contains this average
c......If several regions overlap, the last one wins.
      do 100 ir=1,nrcd
       if (      avi.gt.rmap(2,ir) .and. avi.lt.rmap(3,ir)
     +        .and.avj.gt.rmap(4,ir) .and. avj.lt.rmap(5,ir) ) 
     +      imat(ie) = rmap(1,ir)
  100 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ELECK
      include 'p2conf.h'
c 
c     Electrode card check routine
c
c     Original : C.H.Price       Stanford University       May, 1982
c     Revision : MRP             Stanford University       Nov, 1983
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'stat.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer i,j,k,numelc,nxlo,nxhi,nylo,nyhi
c 
c FUNCTIONS:
      real    gtrval
c****************************************************************** 
c 
c...Set electrode card true and get no.
      lelecd=.true. 
      numelc=gtrval(1)
      if (numelc.lt.1) call erset(16,linum,0) 
c 
c...Bounds
      nxlo=gtrval(4)
      nxhi=gtrval(5)
      nylo=gtrval(6)
      nyhi=gtrval(7)
      if (nxlo.lt.1.or.nxhi.gt.nx.or.nxhi.lt.nxlo.or.nylo.lt.1
     +    .or.nyhi.gt.ny.or.nyhi.lt.nylo) call erset(17,linum,0)
      if (errflg) return
c 
c...Scan region, store node numbers into nbc
c...and store electrode number into ietype
      do 1 j=nxlo,nxhi
      do 1 i=nylo,nyhi
      k=mshmap(j,i) 
      if (k.lt.0) goto 1
      nb=nb+1 
      if (nb.le.nbdim) goto 2 
      call erset(71,linum,nbdim)
      return
  2   nbc(nb)=k 
      ietype(nb)=numelc 
  1   continue
c
c...Num. of electrodes is max of numelc 
      nelect=max0(numelc,nelect)
c
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE REGCK
      include 'p2conf.h'
c 
c     Region card check routine 
c
c     Original : C.H.Price   Stanford University    May, 1982
c     Revision : MRP         Stanford University    Nov, 1983
c     Modified : CSR         Stanford Univeristy    Mar, 1984
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'stat.h'
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'mshtmp.h'
      integer TMPPAD(978801)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c type declarations 
c 
      integer i,j,k,nregio,nxlo,nxhi,nylo,nyhi
c
c FUNCTIONS:
      logical isrval
      real    gtrval
      logical gtlval
c------------------------------------------------------------------
c 
c ........ Set region card true and get region no.
      lregcd=.true. 
      nregio=gtrval(1)
      if (.not.isrval(1)) then
         call erset(178,linum,0)
      else if(nregio.lt.1.or.nregio.gt.MAXREG) then
         call erset(16,linum,MAXREG)
      endif
      if(errflg) return
c 
c ........ Bounds
      nxlo=gtrval(2)
      nxhi=gtrval(3)
      nylo=gtrval(4)
      nyhi=gtrval(5)
      if (nxlo.lt.1.or.nxhi.gt.nx.or.nxhi.lt.nxlo.or.nylo.lt.1
     +   .or.nyhi.gt.ny.or.nyhi.lt.nylo) call erset(17,linum,0) 
c
c ........ Store information to be used in gnelm.
      nrcd=nrcd+1
      rmap(1,nrcd) = nregio
      rmap(2,nrcd) = nxlo
      rmap(3,nrcd) = nxhi
      rmap(4,nrcd) = nylo
      rmap(5,nrcd) = nyhi
c
c ........ Get material type
      j=0
      do 20 i=1,7
      if(gtlval(i)) then
         j=j+1
         if(i.le.3) then
            k=i
         else
            k=3-i
         endif
      endif
20    continue
c
      if(j.gt.1) call erset(32,linum,nregio)
      if(j.eq.0) call erset(38,linum,nregio)
      if((mattyp(nregio).ne.0).and.(k.ne.mattyp(nregio)))
     +    call erset(32,linum,nregio)
      mattyp(nregio)=k
c
c ........ Set number of materials to max. region number 
      nmat=max0(nmat,nregio)
c 
      return
      end 
