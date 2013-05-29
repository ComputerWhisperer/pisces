cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Aug 25 09:43:11 PDT 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE REGRID
      include 'p2conf.h'
c
c
c     Refine triangles wherever the change in a
c     variable, specified by the user, is
c     greater than an upper bound on the change,
c     also user-specified.
c
c  Common overlay :     | emaco  |
c                       +--------+
c                       | adjtmp |
c                       +--------+
c                       | rgdtmp |
c                       +--------+
c                       | ordtmp |
c
c-----------------------------------------------------------------------
c
c     Original :        CSR Dec 83
c     Revised  :        MRP Feb 84 (20 ch names)
c     Revised  :        MRP Mar 84 (doping file)
c     Revised  :        CSR Mar 84 (field regrid)
c     Revised  :        MRP Apr 84 (connect doped regions)
c     Revised  :        CSR May 84 (change/magnitude option, bounding box)
c     Revised  :        CSR Aug 84 (automatic green files)
c     Revised  :        CSR Aug 84 (non-recursive neighbour routine)
c     Revised  :        MJE May 88 (use xmktmp() now)
c     Revised  :        MJE Aug 89 (new xmktmp() calling seq)
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'key.h'
      include     'rgrid.h'
      include     'logunt.h'
      include     'setup.h'
      include     'stat.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
        integer smoky,ip,l,i,j
        character*20 noutfl,ndopfl
        real    elap,etemp
        logical loutfl,ldopfl,ldopth,lrdwrt,lasc
c
c********************* Start *******************************************
c
c...Read control card
      call rfnck(smoky,loutfl,noutfl,ldopfl,ndopfl,ldopth,lasc)
      if(errflg) return
c
c...Read previous triangle tree (or initialize if needed) 
c...Previous comes from file of similar name as input mesh,
c...and the new t.t. is stored in a similar name to output.
c...If no output is saved, make up a name and leave it
c...in ttname for the next time in.
      if(lcpu) call timer(1,lucpu,' ',elap)
      call itree(ttname,lasc)
      if(lcpu) then
         write(lucpu,*) ' '
         call timer(2,lucpu,'Initial triangle tree...',elap)
      endif
      if(errflg) return
      if(loutfl) then
         call extnm(noutfl,ttname)
      else
c...Get a temp file name....
         call xmktmp(ttname, LEN(ttname), 'p2rgrd', 6)
      endif
c
c...Refine triangles
      call refine(ttname,ldopth,lasc)
      if(errflg) return
c
c...Re-order elements.
      if(lcpu) call timer(1,lucpu,' ',elap)
      call nuordr(2)
      if(lcpu) call timer(2,lucpu,'Re-ordering elements...',elap)
c
c...Recreate nbc and ietype
      l=0
      do 3000 ip=1,np
         if(lm(ip).ne.0) then
            l=l+1
            nbc(l)=ip
            ietype(l)=lm(ip)
         endif
 3000 continue
      nb=l
c
c...Do any requested smoothing
      if(lcpu) call timer(1,lucpu,' ',elap)
      call smooth(smoky,lregio)
      if(lcpu) call timer(2,lucpu,'Smoothing...',elap)
c
c...Tidy mesh
      if(lcpu) call timer(1,lucpu,' ',elap)
      call mend
      if(lcpu) call timer(2,lucpu,'Mesh cleanup...',elap)
c
c...Re-read doping and scale it
c...If no doping file specified, assume total conc = |net|
      if(ldopfl) then
         call initdp
         call redope(ndopfl)
         if(errflg) return
         call finde
         if(errflg) return
      else
         do 882 i=1,np
            if(lscale) r1(i)=r1(i)*sngl(dcscl)
882         tconc(i)=abs(r1(i))
      endif
c
c...Extract naconc and ndconc from r1
      do 210 i=1,np
      if (r1(i).GT.0.0d0) then
        ndconc(i)=r1(i)
        naconc(i)=0.0d0
      else
        ndconc(i)=0.0d0
        naconc(i)=abs(r1(i))
      endif
  210 continue


c
c...Write mesh BEFORE SCALING
      if(loutfl) then
          lrdwrt=.false.
          call mrdwr(noutfl, lrdwrt, .false., .false., lasc)
      endif
c
c...Rescale ehed - if new doping, scale it too!
c...Also get conc. dep. parameters (must do AFTER doping scale)
      if(lscale) then

         do 85 i=1,ne 
         etemp=epsmat(imat(i))
         do 85 j=1,3
   85    ehed(j,i)=ehed(j,i)*etemp

         do 82 i=1,np
            ndconc(i)=ndconc(i)/sngl(dcscl)
            naconc(i)=naconc(i)/sngl(dcscl)
   82       r1(i)=r1(i)/sngl(dcscl)

         call condep
         call setqss

      endif
c
c...Make sure everyone knows this is not a square mesh!
      lrect=.false.
      nx=0
      ny=0
c
c...Must re-do symbolic
      lsymdn=.false.
c
c...Set next card read flag and leave
      lcrdrd=.false.
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE RFNCK(smoky,sflag,oname,dflag,dname,ldopth,lasc)
      include 'p2conf.h'
c 
c     date code: 831221
c 
c                   this routine checks the regrid card parameters 
c 
c     mode          function type
c 
c     1             potential 
c     2             quasi-fermi potential (n)
c     3             quasi-fermi potential (p)
c     4             doping
c     5             electron concentration
c     6             hole concentration
c     7             net charge concentration
c     8             net carrier concentration 
c     9             minority carrier density
c    10             discontinuity in electric field
c 
c     copyright c 1983 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      include     'blank.h'
      include     'setup.h'
      include     'rgrid.h'
c
      logical sflag,dflag,ldopth,lasc
      integer numc,i,smoky,nnr,itemp,nrkey,igkey
      integer mmod
      character*20 oname,dname
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
c...get binary output file name
      call gtcval(1, oname, LEN(oname))
      sflag = iscval(1)
      lasc=gtlval(20)
c
c...get doping input file name
      call gtcval(2, dname, LEN(dname))
      dflag=iscval(2)
c
c...extravagent green triangle limit?
      lxg=gtrval(9)
c
c...smoothing key
      smoky=gtrval(2)
      if(smoky.le.0) smoky=0
c
c...maximum level
      llimit=gtrval(3)
c
c...get function type, no more than one allowed
      numc=0
      fmode=0
      do 20 i=1,10
         if(.not.gtlval(i)) goto 20 
         numc=numc+1 
         fmode=i 
   20 continue
      if(numc.gt.1) call erset(85,linum,numc)
      if(errflg) return
c
c...must have solution present for all but doping
      if(fmode.ne.4.and.fmode.ne.0 .and. .not.lsol1) 
     +        call erset(177,linum,0) 
c
c...step; must be set and positive
      fdel=gtrval(1)
      if(fmode.eq.9) then
         ldopth=gtlval(19)
         if(ldopth) fdel=1.
      endif
      if(fmode.gt.0.and.fdel.eq.-999.) call erset(154,linum,0)
      if(fmode.gt.0.and.fdel.lt.0.) call erset(154,linum,0) 
      if(errflg) return
c
c...see if values specified are logarithmic and/or absolute
      logar=gtlval(15) 
      labs=gtlval(17)
c
c...refine on change or magnitude?
      if(fmode.ne.9) then
         lchang=.true.
      else
         lchang=.false.
      endif
      if(islval(18)) lchang=gtlval(18)
c
c...Default - regrid on all regions
      do 90 i=1,nmat
         lregio(i)=1
         if(fmode.ge.2.and.fmode.le.9 .and.mattyp(i).lt.0) 
     +      lregio(i)=0
90    continue
c
c...find regions to ignore - default is none
      igkey=gtrval(10)
      if(igkey.le.0) goto 95

      nnr=1+int(alog10(real(igkey)))
      do 99 i=1,nnr
         itemp=mmod(igkey,10)
         lregio(itemp)=-999
         igkey=igkey/10
         if(itemp.lt.1.or.itemp.gt.nmat) 
     +            call erset(-241,linum,lregio(i))
99    continue
c
c...find regions to refine - if user specifies some regions, ones
c...that are not specified will not be refined
95    nrkey=gtrval(4)
      if(nrkey.le.0) goto 100

      do 77 i=1,nmat
77       if(lregio(i).ge.0) lregio(i)=0
      nnr=1+int(alog10(real(nrkey)))
      do 80 i=1,nnr
         itemp=mmod(nrkey,10)
         if(lregio(itemp).lt.0) then
             call erset(188,linum,itemp)
             return
         endif
         lregio(itemp)=1
         nrkey=nrkey/10
         if(itemp.lt.1.or.itemp.gt.nmat) 
     +            call erset(-241,linum,lregio(i))
   80 continue
c
c...Get optional bounds
100   continue
      call devlts(rgbox(1),rgbox(2),rgbox(3),rgbox(4))
      if(isrval(5)) rgbox(1)=gtrval(5)*1e-4
      if(isrval(6)) rgbox(2)=gtrval(6)*1e-4
      if(isrval(7)) rgbox(3)=gtrval(7)*1e-4
      if(isrval(8)) rgbox(4)=gtrval(8)*1e-4
c 
c...done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ITREE(gfname,lasc)
      include 'p2conf.h'
c
c     Initialize the triangle tree.
c
c     Original :      CSR Dec 83
c     Last modified : CSR Feb 85 - use negative llimits referenced from max.
c
c     copyright c 1983 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'rgrid.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
        integer ie,j,maxlev
        logical ldummy,lasc
        character*20 gfname
c
c******************** Start ********************************************
c
c...............Check out green file.
      call gfrdwr(gfname,.true.,ldummy,lasc)

      if(ldummy) goto 100

c......Got something - set llimit if negative
c......If -999, set to default=maxlev+1 otherwise set to maxlev-|llimit|
      if(llimit.le.0) then 
         maxlev=0
         do 10 ie=1,ne
   10       if(level(ie).gt.maxlev) maxlev=level(ie)
         if(llimit.eq.-999) then
            llimit=maxlev+1
         else
            llimit=maxlev+llimit
         endif
      endif
      return

c......Nope, initialize.
      
  100 ioff=3-mod(ne,4)

      do 20 ie=1,ne
         level(ie)=0
         fath(ie)=0
         son(ie)=0
         imat2(ie)=imat(ie)
       do 20 j=1,3
            tnop(j,ie)=nop(j,ie)
   20 continue

      if(llimit.le.0) llimit=1

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE REFINE(gfname,ldopth,lasc)
      include 'p2conf.h'
c
c     Refine triangulation. 
c     Reference : R.E.Bank & T.Dupont
c                 'Algorithms for Multi-Grid' Mathematics Department,
c                 University of Texas, Oct.78
c
c     Original : CSR dec 83
c     Revised  : MRP Jul 84
c
c     copyright c 1983 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'symb.h'
      include     'logunt.h'
      include     'rgrid.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer nregul,ie,j,nj,k,nk,iee,nhn,rlnab
      character*20 gfname
      logical dvtest,xgreen,ldopth,ldummy,lasc
      real    elap
c
c******************* Start *********************************************
c
      if(lcpu) call timer(1,lucpu,' ',elap)
c
c...Triangles are added continuously during this while loop.
      ie=1
 1000 if(ie.gt.ne) goto 1001
c
c...Test whether neighbour needs refinement first.
c...Neighbours get hit if they have >one refined 
c...brother, or if they are more than one level
c...below this triangle.
      do 400 j=1,3

         nj=rlnab(j,ie)
         if(nj.le.0) goto 400
         if(son(nj).ne.0) goto 400

         if(level(ie).gt.level(nj)+1) then
            call divide(nj)
            if(errflg) return
         else
            nhn=0
            do 450 k=1,3
               nk=rlnab(k,nj)
               if(nk.eq.0) goto 450
               if(son(nk).eq.0) goto 450
               nhn=nhn+1
  450       continue
            if(nhn.gt.1) then
               call divide(nj)
               if(errflg) return
            else if(nhn.eq.1.and.lxg.lt.1) then
               if(xgreen(nj)) call divide(nj)
            endif
         endif

  400 continue
c
c...Use the user-specified refinement criterion
c...to refine ie itself.
      if(dvtest(ie,ldopth)) call divide(ie)
      if(errflg) return
      ie=ie+1
      goto 1000
c
c...End of regular refinement
 1001 continue
      if(lcpu) call timer(2,lucpu,'Regular refinement...',elap)
      if(lcpu) call timer(1,lucpu,' ',elap)
c
c...Reorder the nodes.
c...(Can't postpone it because the green file will have
c...the old ordering.)
      call nuordr(1)
      if(lcpu) call timer(2,lucpu,'Node re-order...',elap)
      if(lcpu) call timer(1,lucpu,' ',elap)
c
c...Save the triangle tree so we can come back
c...and resume refinement. 
      call gfrdwr(gfname,.false.,ldummy,lasc)
      if(lcpu) call timer(2,lucpu,'Tree write...',elap)
      if(lcpu) call timer(1,lucpu,' ',elap)
c
c...Mop up green triangles.
      nregul=ne
      do 2000 ie=1,nregul
         if(son(ie).ne.0) goto 2000

         do 1900 j=1,3
            nj=rlnab(j,ie)
            if(nj.eq.0) goto 1900
            if(son(nj).gt.0) then 
               call green(ie,j,nj)
               if(errflg) return
               goto 1901
            endif
 1900    continue
 1901    continue

 2000 continue
c
c...Condense nop into the usual form
      iee=0
      do 3000 ie=1,ne
         if(son(ie).ne.0) goto 3000
         iee=iee+1
         imat(iee)=imat2(ie)
         do 3001 j=1,3
            nop(j,iee)=tnop(j,ie)
 3001    continue
 3000 continue
      ne=iee
      if(lcpu) call timer(2,lucpu,'Green mop-up...',elap)
c
c...Look before we leap
      if(ne.ge.nedim) call erset(118,linum,nedim)
      if(np.gt.npdim) call erset(69,linum,npdim)
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION RLNAB(j,ie)
      include 'p2conf.h'
c
c     Find the j'th neighbour of triangle ie, where neighbour means
c     a generalized neighbour in the triangle tree, a la R.E.Bank.
c
c     Original    CSR Dec 83
c     Modified    CSR Aug 84 Non recursive rewrite.
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
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      logical tnabor
      integer kson,ixc,ixe,j,ie,k,stack(20),sp,top,sneib,nj
c
c******************** Start ********************************************
c
c......Walk backwards upward, until we reach a triangle with a 
c......simple neighbour. Since the root triangulation has simple
c......neighbours, this loop is guaranteed to terminate.
      sp=1
      stack(1)=ie

  100 continue
         
       top = stack(sp)
       nj = sneib( j, top)

       if(nj .lt. 0) then
          sp=sp+1
          stack(sp)=fath(top)
       else if(nj .eq. 0) then
          rlnab=0
          return
       else
          goto 101
       endif
      goto 100

  101 if(sp.eq.1) then
       rlnab=nj
       return
      endif

      stack(sp)=nj

c.......The stack now contains the path upwards from ie, except for
c.......the top element which has just been set to the high level
c.......neighbour on the side j. (Loop invariant) 
c.......We now walk back down that side.
  200 continue
       top=stack(sp)
       if(son(top).le.0) goto 201
       do 205 k=1,3
          kson=son(top)+k
          if(tnabor(kson,stack(sp-1),ixc,ixe)) goto 206
  205    continue
  206    continue
       sp=sp-1
       stack(sp)=kson
         if(sp.gt.1) goto 200

c.........We fell through to a genuine neighbour
      rlnab=kson
      return

c.........No exact neighbour ; nearest is an uncle.
  201 continue
      rlnab=top
      return
      end
C-Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-Cc 
C-C      INTEGER FUNCTION RLNAB(j,ie)
C-Cc
C-Cc     Recursive procedure to return neighbour j of ie for 
C-Cc     any ie in the triangle tree. 
C-Cc     This is the original, and executes faster than the iterative.
C-Cc     It has been commented out to allow for simple-minded compilers.
C-Cc     
C-Cc
C-Cc     Original :        CSR Dec 83
C-Cc
C-Cc     Copyright c 1983 The board of trustees of the Leland Stanford 
C-Cc                      Junior University. All rights reserved.
C-Cc     This subroutine may not be used outside of the PISCES computer
C-Cc     program without the prior written consent of Stanford University. 
C-Cc
C-Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C-Cc
C-Cc...............common area
C-C      include 'common/com.blank'
C-C      include 'common/com.emaco'
C-C      include 'common/com.adjtmp'
C-C      include 'common/com.rgdtmp'
C-C      include 'common/com.rgrid'
C-Cc
C-Cc...............local types
C-C      logical tnabor
C-C      integer json,jones,ixc,ixe,j,ie,k
C-Cc
C-Cc******************** Start ********************************************
C-Cc
C-Cc
C-Cc......Macro triangles
C-C      if(fath(ie).eq.0) then 
C-C         rlnab=max0(0,nextel(j,ie))
C-C
C-Cc..........Regular son. First look among the immediate family;
C-C      else
C-C         if(mod(ie+ioff,4).eq.0) then 
C-C            rlnab=ie+j
C-C         else
C-C             if(j.eq.mod(ie+ioff,4)) then 
C-C                rlnab=ie-j
C-C
C-Cc................now try the father's j-neighbour (recursive call)
C-C             else
C-C                jones=rlnab(j,fath(ie))
C-C                if(jones.eq.0) then 
C-C                    rlnab=0
C-C                else
C-C                    if(son(jones).le.0) then 
C-C                       rlnab=jones
C-C
C-Cc.......................genuine neighbour triangle.
C-C                   else
C-C                       do 100 k=1,3
C-C                          json=son(jones)+k
C-C                          if(tnabor(json,ie,ixc,ixe)) then
C-C                              rlnab=json
C-C                              return
C-C                          endif
C-C  100                  continue
C-C                   endif
C-C                endif
C-C            endif
C-C         endif
C-C      endif
C-C      return
C-C      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION SNEIB( j, ie)
      include 'p2conf.h'
c
c     Test side j of triangle ie for a simple triangle
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'rgrid.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer j,ie

c******************** Start ********************************************
c
c......Macro triangle?
      if(fath(ie).eq.0) then
         sneib=max0(0,nextel(j,ie))

c......Regular son. Look among the immediate family.
      else
         if(mod(ie+ioff,4).eq.0) then
            sneib=ie+j
         else
            if(j.eq.mod(ie+ioff,4)) then
               sneib=ie-j

c......no simple neighbour
            else
               sneib=-1
            endif
         endif
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DIVIDE(ie)
      include 'p2conf.h'
c
c     Divide a triangle ie by regular refinement.
c
c     Original :        CSR Dec 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'symb.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------

      integer ie,j,nj,j1,j2,newp(3),l,snj,tmod,rlnab

c******************** Start ********************************************
c
c...Is this a duplicate call?
      if(son(ie).ne.0) return
c
c...Check if there's room.
      if(ne+4.gt.2*nedim) call erset(118,linum,2*ne)
      if(errflg) return
c
c...Scan sides to determine midside node numbers.
      do 700 j=1,3
         nj=rlnab(j,ie)
         j1=tmod(j+1)
         j2=tmod(j+2)
c
c...No neighbour; create a new node.
         if(nj.eq.0) then
             call enode(tnop(j1,ie),tnop(j2,ie))
             newp(j)=np
c
c...Unrefined neighbour; create a new node.
         else
            if(son(nj).eq.0) then
               call enode(tnop(j1,ie),tnop(j2,ie))
               newp(j)=np
c
c...Refined neighbour; extract the node number of the
c...midside node. The numbering convention is such 
c...that if ie is neighbour l of nj, then node l of the
c...zero son of nj is the shared midside node.
            else
               snj=son(nj)

               do 100 l=1,3
  100             if(rlnab(l,nj).eq.ie) goto 101
  101          continue

               newp(j)=tnop(l,snj)

            endif
         endif
  700 continue
c
c...Create new elements 
      son(ie)= ne+1

      ne=ne+1
      tnop(1,ne)=newp(1)
      tnop(2,ne)=newp(2)
      tnop(3,ne)=newp(3)

      ne=ne+1
      tnop(1,ne)=tnop(1,ie)
      tnop(2,ne)=newp(3)
      tnop(3,ne)=newp(2)

      ne=ne+1
      tnop(1,ne)=newp(3)
      tnop(2,ne)=tnop(2,ie)
      tnop(3,ne)=newp(1)

      ne=ne+1
      tnop(1,ne)=newp(2)
      tnop(2,ne)=newp(1)
      tnop(3,ne)=tnop(3,ie)

      do 740 l=0,3
         son(ne-l)=0
         fath(ne-l)=ie
         level(ne-l)=level(ie)+1
740      imat2(ne-l)=imat2(ie)
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE GREEN(ie,j,nj)
      include 'p2conf.h'
c
c     Divide a triangle ie by green refinement
c
c     Original :        CSR Dec 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'symb.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer ie,j,nj,snj,l,icen,tmod,rlnab
c
c******************** Start ********************************************
c
c...............Check overflow
      if(ne+2.gt.2*nedim) call erset(118,linum,ne)
      if(errflg) return

c...............Midside node algorithm revisited.
      snj=son(nj)

      do 100 l=1,3
100      if(rlnab(l,nj).eq.ie) go to 101
101   continue

      icen=tnop(l,snj)

c................create two green triangles 
      son(ie)= -(ne+1)

      ne=ne+1
      tnop(1,ne)=tnop(j,ie)
      tnop(2,ne)=tnop(tmod(j+1),ie)
      tnop(3,ne)=icen

      ne=ne+1
      tnop(1,ne)=tnop(j,ie)
      tnop(2,ne)=icen
      tnop(3,ne)=tnop(tmod(j+2),ie)

      do 1840 l=0,1
         imat2(ne-l)=imat2(ie)
         son(ne-l)=0
1840     fath(ne-l)=-ie

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ENODE(ej1,ej2)
      include 'p2conf.h'
c                       
c                       Create a new node on the edge ej1-ej2 and
c                       interpolate values.
c
c     Original CSR Dec 1983
c
c     copyright c 1983 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'symb.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer ej1,ej2
      real    t1,t2,t3
c
c******************** Start ********************************************
c
      np=np+1
      if(np.gt.npdim) call erset(69,linum,np)
      cord(1,np)=0.5*(cord(1,ej1)+cord(1,ej2))
      cord(2,np)=0.5*(cord(2,ej1)+cord(2,ej2))

c...............Type : easy but for interfaces
c               An interface belongs to the material of higher material
c               code; if the interface is not to move we must give
c...............the new node to the lower material
      if(mattyp(itype(ej1)).lt.mattyp(itype(ej2))) then
         itype(np)=itype(ej1)
      else 
         if(mattyp(itype(ej1)).gt.mattyp(itype(ej2))) then
            itype(np)=itype(ej2)
         else
            if(itype(ej1).le.itype(ej2)) then
               itype(np)=itype(ej1)
            else
               itype(np)=itype(ej2)
            endif
         endif
      endif

c...............Electrode number
c
      if(lm(ej1).eq.lm(ej2)) then
         lm(np)=lm(ej1)
      else
         lm(np)=0
      endif
     
c...............Doping : geometric average.
c               One is oxide => doping=0 (convention that Si/Ox interface 
c                                         has silicon nodes)
c               Remember to set pointers to doped region that new node
c               belongs in
      t1=abs(r1(ej1))
      t2=abs(r1(ej2))
      if(t1.gt.t2) then
         t3=sign(1.0,r1(ej1))
         eptr(np)=eptr(ej1)
      else 
         t3=sign(1.0,r1(ej2))
         eptr(np)=eptr(ej2)
      endif
      r1(np)=sqrt(t1)*sqrt(t2)*t3


c...............Solution variables
c
      if(lsol1) then
         fv(np)=0.5d0*(fv(ej1)+fv(ej2))
         fn(np)=dsqrt(fn(ej1))*dsqrt(fn(ej2))
         fp(np)=dsqrt(fp(ej1))*dsqrt(fp(ej2))
         qfn(np)=0.5d0*(qfn(ej1)+qfn(ej2))
         qfp(np)=0.5d0*(qfp(ej1)+qfp(ej2))
      endif
      if(lsol2) then
         ofv(np)=0.5d0*(ofv(ej1)+ofv(ej2))
         ofn(np)=dsqrt(ofn(ej1))*dsqrt(ofn(ej2))
         ofp(np)=dsqrt(ofp(ej1))*dsqrt(ofp(ej2))
      endif

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE GFRDWR(gfname,lrdwrt,ldummy,lasc)
      include 'p2conf.h'
c
c     Read or write a green-file. This saves the triangle tree 
c     so that we can come back later and continue, without refining
c     green triangles. 
c
c-----------------------------------------------------------------------
c     Original :        CSR Dec 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'rgrid.h'
      include     'logunt.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c     
      logical lrdwrt,ldummy,lexist,lasc
      character*20 gfname
      integer lu,gne,gnp,gnb,ie,j,ip,ierr
c
c******************** Start ********************************************
c
c               Format :
c               ne, np, nb, ioff  (np,nb only for checking)
c               tnop, nextel, imat, son, father, flevel
c
c...............lrdwrt=true means read.
      if(.not.lrdwrt) goto 1000

c........Avoid error message from fopcl by doing an inquire first.
c        (Because the file never exists for an initial grid)
c        Also need a patch because inquire, open and read on a blank 
c        filename WORK! (in Unix 4.1BSD). Naturally
c........the values that get 'read' are bogus.
      lu=lutmp
      lexist=.false.
      call filinq(gfname,20,lexist,j)
      if(lexist.and.gfname(1:1).ne.' ') goto 100
      ldummy=.true.
      write(6,*) 'Warning - green file not found'
      return

  100 continue
      if(lasc) then
         call fopcl(2,gfname,20,lu,.false.,ierr)
      else
         call fopcl(1,gfname,20,lu,.false.,ierr)
      endif
      if(errflg) then
         call erset(151,linum,0)
         return
      endif
      rewind(lu)

      ldummy=.false.
      if(lasc) then
         read(lu,*,err=999,end=999) gne,gnp,gnb, ioff
      else
         read(lu,err=999,end=999) gne,gnp,gnb, ioff
      endif
      if(gne.eq.0.or.gnp.eq.0) then
         ldummy=.true.
         call erset(152,linum,0)
         return
      endif

      if(gnp.ne.np.or.gnb.ne.nb) then
          call erset(153,linum,0)
          write(6,*) 'Problem with file',gfname
          return
      endif
      ne=gne
      if(lasc) then
         read(lu,*,err=999,end=999) 
     +          ((tnop(j,ie),nextel(j,ie),j=1,3),imat2(ie),son(ie),
     +          fath(ie),level(ie),ie=1,ne),(lm(ip),ip=1,np)
      else
         read(lu,err=999,end=999) 
     +          ((tnop(j,ie),nextel(j,ie),j=1,3),imat2(ie),son(ie),
     +          fath(ie),level(ie),ie=1,ne),(lm(ip),ip=1,np)
      endif
      call fopcl(0,gfname,20,lu,.false.,ierr)
      return

c................write 
1000  lu=lutmp
      if(lasc) then
         call fopcl(12,gfname,20,lu,.false.,ierr)
      else
         call fopcl(11,gfname,20,lu,.false.,ierr)
      endif
      if(errflg) then 
         call erset(152,linum,0)
         return
      endif
      rewind(lu)

c
c......(nb has not yet been updated so do it )
      nb=0
      do 1010 ip=1,np
1010     if(lm(ip).ne.0) nb=nb+1

      if(lasc) then
         write(lu,*,err=999) ne,np,nb,ioff
         write(lu,*,err=999) 
     +          ((tnop(j,ie),nextel(j,ie),j=1,3),imat2(ie),son(ie),
     +          fath(ie),level(ie),ie=1,ne),(lm(ip),ip=1,np)
      else
         write(lu,err=999) ne,np,nb,ioff
         write(lu,err=999) 
     +          ((tnop(j,ie),nextel(j,ie),j=1,3),imat2(ie),son(ie),
     +          fath(ie),level(ie),ie=1,ne),(lm(ip),ip=1,np)
      endif
      call fopcl(0,gfname,20,lu,.false.,ierr)
      return
c
c......Uh oh.
  999 call erset(152,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION DVTEST(ie,ldopth)
      include 'p2conf.h'
c
c...............Test whether a triangle deserves to be refined.
c
c     Original :        CSR Nov 83
c     Revised  :        MRP Jul 84
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'rgrid.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer p1,p2,p3,ie,it,tj,j,j1,j2,ffmode,tmod,rlnab,mincar
      real    f1,f2,f3,t1,t2,t3,t4,feval2,xx(3),yy(3),t(3),
     +        ef,efj,fj1,fj2,fj3,xxj(3),yyj(3),dx,dy,efield,
     +        x1,x2,x3,y1,y2,y3,xj1,xj2,xj3,yj1,yj2,yj3
      logical inside,ldopth
      equivalence (t(1),t1),(t(2),t2),(t(3),t3)
      equivalence (xx(1),x1),(xx(2),x2),(xx(3),x3)
      equivalence (yy(1),y1),(yy(2),y2),(yy(3),y3)
      equivalence (xxj(1),xj1),(xxj(2),xj2),(xxj(3),xj3)
      equivalence (yyj(1),yj1),(yyj(2),yj2),(yyj(3),yj3)
c
c******************** Start ********************************************
c
c...............Limit the refinement.
      dvtest=.false.
      if(level(ie).ge.llimit) return

c................Omit test for this region?
      it=imat2(ie)
      if(lregio(it).le.0) return

      p1=tnop(1,ie)
      p2=tnop(2,ie)
      p3=tnop(3,ie)

c................Omit test for this box?
      inside=.false.
      x1=cord(1,p1)
      x2=cord(1,p2)
      x3=cord(1,p3)
      y1=cord(2,p1)
      y2=cord(2,p2)
      y3=cord(2,p3)

      if(x1.ge.rgbox(1).and.x1.le.rgbox(2) .and. 
     +    y1.ge.rgbox(3).and.y1.le.rgbox(4)   ) inside=.true.
      if(x2.ge.rgbox(1).and.x2.le.rgbox(2) .and. 
     +    y2.ge.rgbox(3).and.y2.le.rgbox(4)   ) inside=.true.
      if(x3.ge.rgbox(1).and.x3.le.rgbox(2) .and. 
     +    y3.ge.rgbox(3).and.y3.le.rgbox(4)   ) inside=.true.

      if(.not.inside) return

c......If no mode specified, nothing more to test.
      if(fmode.eq.0) then
       dvtest=.true.
       return
      endif



      ffmode=fmode
      if(ffmode.eq.10) ffmode=1
      if(ffmode.ne.9) then
         f1=feval2(p1,ffmode,logar,labs)
         f2=feval2(p2,ffmode,logar,labs)
         f3=feval2(p3,ffmode,logar,labs)
      else
         ffmode=mincar(p1)
         f1=feval2(p1,ffmode,logar,labs)
         if(ldopth) f1=f1/abs(r1(p1))
         ffmode=mincar(p2)
         f2=feval2(p2,ffmode,logar,labs)
         if(ldopth) f2=f2/abs(r1(p2))
         ffmode=mincar(p3)
         f3=feval2(p3,ffmode,logar,labs)
         if(ldopth) f3=f3/abs(r1(p3))
      endif

c...............Refinement on scalars: 
c               Find the magnitude at each node or the change along 
c               each edge and compare with 
c...............the tolerance. 
      if(fmode.le.9) then
       if(lchang) then
          t1=abs((f3-f2)/fdel)
          t2=abs((f1-f3)/fdel)
          t3=abs((f1-f2)/fdel)
       else
          t1=abs(f1)/fdel
          t2=abs(f2)/fdel
          t3=abs(f3)/fdel
       endif
       t4=amax1(t1,t2,t3) 
       if(t4.gt.1) dvtest=.true.

c...............Refinement on electric field discontinuity:
c               Find the change in normal electric field and
c...............compare with the tolerance 
      else
       do 100 j=1,3
            j1=tmod(j+1)
          j2=tmod(j+2)
          dx=xx(j2)-xx(j1)
          dy=yy(j2)-yy(j1)
          ef=efield(x1,y1,x2,y2,x3,y3,f1,f2,f3,-dy,dx)
          tj=rlnab(j,ie)
          if(tj.ne.0.and.lchang) then
             xj1=cord(1,tnop(1,tj))
             xj2=cord(1,tnop(2,tj))
             xj3=cord(1,tnop(3,tj))
             yj1=cord(2,tnop(1,tj))
             yj2=cord(2,tnop(2,tj))
             yj3=cord(2,tnop(3,tj))
             fj1=feval2(tnop(1,tj),1,logar,labs)
             fj2=feval2(tnop(2,tj),1,logar,labs)
             fj3=feval2(tnop(3,tj),1,logar,labs)
             efj=efield(xj1,yj1,xj2,yj2,xj3,yj3,fj1,fj2,fj3,dy,-dx)
          else
             efj=0
          endif

          t(j)=abs((efj-ef)/fdel)
  100    continue
         t4=max(t1,t2,t3)
       if(t4.gt.1) dvtest=.true.
      
      endif
          
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE NUORDR(ikey)
      include 'p2conf.h'
c
c     Reorder nodes and/or elements from left to right, and from 
c     top to bottom in each column. 
c
c     With ikey=1 reorder nodes.
c
c     With ikey=2 reorder elements.
c
c     Original :        CSR Dec 83
c
c     copyright c 1983 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      include     'ordtmp.h'
      integer TMPPAD(1302002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer ip,ie,j,minx,maxx,midx,mini,maxi,midi,ikey
      real    snp
c
c******************** Start ********************************************
c

      if(ikey.ne.1) goto 500
c...............Index, then sort nodes. Major sort is on x.

      do 100 ip=1,np
         nrd(ip)=ip
         rtag(ip)=cord(1,ip)
  100 continue
      call v5sort(np,nrd,rtag)
c
      ip=1
      j=1
      snp=cord(1,nrd(1))
      rtag(1)=cord(2,nrd(1))
  101 j=j+1
      if(j.gt.np) goto 102
      rtag(j)=cord(2,nrd(j))
      if(cord(1,nrd(j)).eq.snp) goto 101
      call v5sort(j-ip,nrd(ip),rtag(ip))
      ip=j
      snp=cord(1,nrd(j))
      goto 101
  102 call v5sort(j-ip,nrd(ip),rtag(ip))


      do 205 j=1,2
       do 201 ip=1,np
  201       rvl(ip)=cord(j,nrd(ip))
       do 202 ip=1,np
  202       cord(j,ip)=rvl(ip)
  205 continue
     
      do 211 ip=1,np
  211    jvl(ip)=itype(nrd(ip))
      do 212 ip=1,np
  212    itype(ip)=jvl(ip)

      do 216 ip=1,np
  216    jvl(ip)=lm(nrd(ip))
      do 217 ip=1,np
  217    lm(ip)=jvl(ip)

      do 221 ip=1,np
  221    rvl(ip)=r1(nrd(ip))
      do 222 ip=1,np
  222    r1(ip)=rvl(ip)

      do 2221 ip=1,np
 2221    jvl(ip)=eptr(nrd(ip))
      do 2222 ip=1,np
 2222    eptr(ip)=jvl(ip)

      do 236 ip=1,np
  236    dvl(ip)=fv(nrd(ip))
      do 237 ip=1,np
  237    fv(ip)=dvl(ip)
      
      do 241 ip=1,np
  241    dvl(ip)=fn(nrd(ip))
      do 242 ip=1,np
  242    fn(ip)=dvl(ip)

      do 246 ip=1,np
  246    dvl(ip)=fp(nrd(ip))
      do 247 ip=1,np
  247    fp(ip)=dvl(ip)

      do 2256 ip=1,np
 2256    dvl(ip)=qfn(nrd(ip))
      do 2257 ip=1,np
 2257    qfn(ip)=dvl(ip)

      do 2261 ip=1,np
 2261    dvl(ip)=qfp(nrd(ip))
      do 2262 ip=1,np
 2262    qfp(ip)=dvl(ip)

      do 251 ip=1,np
  251    dvl(ip)=ofv(nrd(ip))
      do 252 ip=1,np
  252    ofv(ip)=dvl(ip)

      do 256 ip=1,np
  256    dvl(ip)=ofn(nrd(ip))
      do 257 ip=1,np
  257    ofn(ip)=dvl(ip)

      do 261 ip=1,np
  261    dvl(ip)=ofp(nrd(ip))
      do 262 ip=1,np
  262    ofp(ip)=dvl(ip)

c...................correct element indices.
      do 200 ip=1,np
  200    inrd(nrd(ip))=ip

      do 300 ie=1,ne
      do 300 j=1,3
  300    tnop(j,ie)=inrd(tnop(j,ie))
      
      return


c...............Index, then sort triangles.
c               (ONLY sort nop (user triangles),
c................never     tnop(triangle tree) )
  500 continue
      snp=float(np)**2
      do 600 ie=1,ne
          trd(ie)=ie
          minx=1
          maxx=1
          do 590 j=2,3
             if(nop(j,ie).gt.nop(maxx,ie)) maxx=j
             if(nop(j,ie).lt.nop(minx,ie)) minx=j
  590     continue
          midx=6-maxx-minx
          mini=nop(minx,ie)
          midi=nop(midx,ie)
          maxi=nop(maxx,ie)
          rtag(ie)=snp*float(mini)+float(np)*float(midi)+float(maxi)
  600 continue
        
      call v5sort(ne,trd,rtag)

      do 625 j=1,3
       do 621 ie=1,ne
  621       jvl(ie)=nop(j,trd(ie))
       do 622 ie=1,ne
  622       nop(j,ie)=jvl(ie)
  625 continue

      do 631 ie=1,ne
  631   jvl(ie)=imat(trd(ie))
      do 632 ie=1,ne
  632   imat(ie)=jvl(ie)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
       LOGICAL FUNCTION TNABOR(ie,in,ke,kn)
      include 'p2conf.h'
c
c                   Determine whether two triangles are neighbours
c                   If so, return ke=side of ie on which in abuts
c                                 kn=side of in of which ie abuts
c                   (Same as lnabor in smooth.f but uses a different 
c                    common block.)
c
c
c-----------------------------------------------------------------------
c     Original :      CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------

      integer ne(3),nn(3),ke,kn,ic,i1,i2,a(2,2),ie,in
c
c******************** Start ********************************************
c
      ne(1)=tnop(1,ie)
      ne(2)=tnop(2,ie)
      ne(3)=tnop(3,ie)
      nn(1)=tnop(1,in)
      nn(2)=tnop(2,in)
      nn(3)=tnop(3,in)
      ic=0
      ke=0
      kn=0
      do 100 i1=1,3
      do 100 i2=1,3
          if(ne(i1).ne.nn(i2)) goto 100
        ic=ic+1
        a(ic,1)=i1
        a(ic,2)=i2
  100 continue
      if(ic.lt.2)  then
       tnabor=.false.
       return
      else
       tnabor=.true.
       ke=6 - a(1,1) - a(2,1)
       kn=6 - a(1,2) - a(2,2)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION EFIELD(x1,y1,x2,y2,x3,y3,u1,u2,u3,nx,ny)
      include 'p2conf.h'
c
c                   Calculate the electric field component 
c                   in triangle (x1,y1,x2,y2,x3,y3),
c                   parallel to direction (nx,ny).
c                   (Field is defined as NEGATIVE gradient u)
c                   
c     Original :      CSR Oct 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      real x1,y1,x2,y2,x3,y3,u1,u2,u3
      real s1(2),s2(2),s3(2),nl,denom,nx,ny
c
c******************** Start ********************************************
c
c                   Calculate vector sides and lengths
      s1(1)=x3-x2
      s1(2)=y3-y2
      s2(1)=x1-x3
      s2(2)=y1-y3
      s3(1)=x2-x1
      s3(2)=y2-y1
      nl   =sqrt(nx*nx+ny*ny)
c
c                   Calculate denominator and then electric field.
c                   Orientation dependence cancels above and below
      denom=s2(2)*s3(1)-s2(1)*s3(2)

      efield=- ((u2-u1) * (s2(2)*nx-s2(1)*ny) 
     $           -(u1-u3) * (s3(2)*nx-s3(1)*ny)) / denom / nl
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION XGREEN (ie)
      include 'p2conf.h'
c
c     See whether leaving this triangle green would cause awful angles
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'rgrid.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'rgdtmp.h'
      integer TMPPAD(1317002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer ie
      integer sj,j,nj,p1,p2,p3,tmod,rlnab,j1,j2
      logical green
      real x1,x2,x3,x4,y1,y2,y3,y4,x14,y14,x43,y43,ccos
      
c****************** Start **********************************************
c

      xgreen=.false.

      if(son(ie).ne.0 .or. lregio(imat2(ie)).lt.0) return

      green=.false.
      do 100 j=1,3
       nj=rlnab(j,ie)
       if(nj.eq.0) goto 100
       if(son(nj).ne.0) then 
          green=.true.
          sj=j
       endif
  100 continue

      if(.not.green) return

      j1=tmod(sj+1)
      j2=tmod(sj+2)

      p1=tnop(sj,ie)
      p2=tnop(j1,ie)
      p3=tnop(j2,ie)

      x1=cord(1,p1)
      x2=cord(1,p2)
      x3=cord(1,p3)
      y1=cord(2,p1)
      y2=cord(2,p2)
      y3=cord(2,p3)

      x4=0.5*(x2+x3)
      y4=0.5*(y2+y3)

      x14=x1-x4
      y14=y1-y4

      x43=x4-x3
      y43=y4-y3

      ccos=(x14*x43 + y14*y43) / 
     +       (sqrt(x14*x14+y14*y14)*sqrt(x43*x43+y43*y43))

      if(abs(ccos).gt.lxg) xgreen=.true.

      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION MINCAR(inode)
      include 'p2conf.h'
c
c     Return index for feval of minority carrier at node inode.
c        5......n is minority (p-type region)
c        6......p is minority (n-type region)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c------------------------------------------------------------------
      integer inode
c------------------------------------------------------------------
c
      if(r1(inode).gt.0.) goto 50
      mincar=5
      goto 100
50    mincar=6
100   return
      end
