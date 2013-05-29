cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Mar 12 14:35:25 PST 1991 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE WCURR
      include 'p2conf.h'
c
c     Calculate terminal currents using current potential method.
c
c     Original: MRP  April, 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
      include     'setup.h'
      include     'logunt.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      double precision jterm(MAXCNT),wpnt,jwpnt
      integer i,iel,inode
c------------------------------------------------------------------
c*************
c**  START  **
c*************
c
c...Initialize
      do 10 i=1,nelect
10    jterm(i)=0.d0
c
c...Get potentials
      call jpotl
      if(errflg) return
c
c...Integrate around each electrode node
      do 20 i=1,nb
         inode=nbc(i)
         iel=lm(inode)
         jwpnt=wpnt(inode)
cc         write(lutty,*) 'Node ',inode,'   curr=',jwpnt
         jterm(iel)=jterm(iel)+jwpnt
20    continue
c
c...Print it
      if(ldebug) then
         write(lutty,*) ' '
         do 30 iel=1,nelect
            write(lutty,*) 'Terminal #',iel,'     current=',jterm(iel)
30       continue
      endif
c
c...Evaluate potentials at nodes
      call wnode
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION WPNT(inode)
      include 'p2conf.h'
c
c     Calculate current about a node.
c
c     Original: MRP  April, 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      double precision wsign,jadd
      integer i,inode,j,ie,ni(5),i1,i2,ie2
      data ni/1,2,3,1,2/
c
c------------------------------------------------------------------
c*************
c**  START  **
c*************
c
c...Loop through each element this node is connected to
      wpnt=0.d0
      do 10 j=1,p2tc(inode)
         ie=p2t(j,inode)
c
c...Check each side for inode
         do 20 i=1,3
            i1=nop(ni(i+1),ie)
            i2=nop(ni(i+2),ie)
            if((i1.ne.inode).and.(i2.ne.inode)) goto 20
            if(i1.eq.inode) then
               wsign=+1.d0
            else
               wsign=-1.d0
            endif
c
c...Find current flow along side by subtracting potential of
c...adjacent element.  If there is a real element there, take
c...only 1/2 since the component will be added in again when
c...we find that other element.  If there is a boundary element,
c...add whole term.
            ie2=wnxtel(i,ie)
            if(ie2.gt.0) then
               jadd=0.5d0*wsign*(wsol(ie)-wsol(ie2))
               wpnt=wpnt+jadd
            else
               jadd=wsign*(wsol(ie)-wsol(-ie2))
               wpnt=wpnt+jadd
            endif
cc            write(*,*) 'ie,ie2,j :',ie,ie2,jadd

20       continue

10    continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE JPOTL
      include 'p2conf.h'
c
c     Calculate current potentials for every element.
c     Note that this algorithm depends on a consistent node
c     ordering within triangles.
c
c     Reference: Palm and Van de Wiele, TED, Oct. 1985.
c
c     Original: MRP  April, 1985
c     Fixed determination of lsurf:  REL, Harris Semi.,  Nov, 1990
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
      include     'setup.h'
      include     'logunt.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c------------------------------------------------------------------
      logical lsurf,ljdisp,lbnds,litex,lwiccg,lembed
      integer ie,i,j,elmat,i1,i2,nmati,mxsor,inxt,k,esiz,iok
      integer ni(5),iwsrt(4),nepb0,idum
      integer ludim,jldim,judim,lsiz,usiz,jlsiz,jusiz,adim
      double precision deetee,djgs,e21,dedv,epsel,wrhsi,wsmin
      double precision gs1(2),gs2(2),dgs1(2),dgs2(2),third
      double precision asfac,wlucr,tmp1,tmp2
      real             ddd
      integer          iysiz
      INTEGER IINXT
c
      data ni/1,2,3,1,2/
      data mxsor/10000/,wlucr/1.d-5/,lwiccg/.false./
      data adim,ludim,jldim,judim/MAXADJ,MAXLU,MAXILU,MAXILU/
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
cc      write(*,'("ICCG(0) or direct(1)? ",$)')
cc      read(*,*) i
cc      lwiccg=i.eq.0
c
c...Initialize
      nepb0=nepb
      third=1.d0/3.d0
      if(ldebug) write(lutty,1001)
1001  format(/' Computing current potentials (assembly)....')
c
c...Set up temporary nextel
      do 303 ie=1,ne
         wnxtel(1,ie)=nextel(1,ie)
         wnxtel(2,ie)=nextel(2,ie)
         wnxtel(3,ie)=nextel(3,ie)
303   continue
c
c...Temporary code for making oxide edges look like a real 
c...device edge.  Used to allow embedded contacts on semic-
c...insulator interfaces, but only if not time-dep (in which
c...case there can be displacement current in the insulator)
      if(.not.ltdep) then
         do 304 ie=1,ne
            nmati=mattyp(imat(ie))
            do 304 i=1,3
               j=wnxtel(i,ie)
               if(j.gt.0) then
                  if(nmati.ne.mattyp(imat(j))) then
                     nepb=nepb+1
                     wnxtel(i,ie)=-nepb
                  endif
               endif
304      continue
      endif
c
c...Now check for totally embedded contacts.  Only ok if fixed
c...by fudge above.
      if(lembed(ie)) then
         call erset(193,linum,ie)
         return
      endif
c
c...Make pseudo-boundaries at internal electrode edges so
c...that an external flux path exists for electrodes which
c...do reach device edge at a single node
      do 220 ie=1,ne
      do 220 i=1,3
         if((lm(nop(ni(i+1),ie)).gt.0).and.
     +      (lm(nop(ni(i+2),ie)).gt.0).and.
     +      (wnxtel(i,ie).gt.0)) then
            nepb=nepb+1
            wnxtel(i,ie)=-nepb
         endif
220   continue
c
c...Find a set of common edge nodes for reference
      j=nepb-ne
      do 2 i=1,j
2        wzero(i)=.false.
      call wedge
c
c-------------
c Set up RHS
c-------------
c
c...Time-dependent?
      deetee=stime-stime0
      ljdisp=deetee.gt.0.d0
      if(ljdisp) deetee=1.d0/deetee
c
c...Initialize
      do 65 ie=1,nepb
         wsol(ie)=0.d0
65    continue
c
c...Loop through elements
      do 100 ie=1,ne

         nmati=imat(ie)
         elmat=mattyp(nmati)
         if(ljdisp) epsel=-decoef*epsmat(nmati)

         do 10 i=1,3
            wrhsi=0.d0
 
            i1=nop(ni(i+1),ie)
            i2=nop(ni(i+2),ie)
            inxt=wnxtel(i,ie)

            lsurf=.false.
CRL         lbnds=inxt.le.0
            inxt=iabs(inxt)
            ddd=jhjd(i,ie)
CRL         if(.not.lbnds) lsurf=mattyp(imat(inxt)).lt.0
CRL
CRL         Wnxtel has already been screwed up for this purpose
CRL         Must use nextel to find lsurf.
CRL
            IINXT=NEXTEL(I,IE)
            LBNDS=IINXT.LE.0
            IINXT=IABS(IINXT)
            IF(.NOT.LBNDS) LSURF=MATTYP(IMAT(IINXT)).LT.0
CRL
            if((elmat.gt.0).and.(ddd.ne.0.0)) then
               if(l1hole) then
                  call assmbj(.true.,elmat,lsurf,ddd,ie,
     +                        ni(i+1),ni(i+2),djgs,gs1,gs2,.true.,
     +                        dgs1,dgs2,e21,dedv,lfldmb,idum)
                  wrhsi=wrhsi+djcoef*djgs
               else 
                  call assmbj(.false.,elmat,lsurf,ddd,ie,
     +                        ni(i+1),ni(i+2),djgs,gs1,gs2,.true.,
     +                        dgs1,dgs2,e21,dedv,lfldmb,idum)
                  wrhsi=wrhsi-djcoef*djgs
                  if(ncarr0.eq.2) then
                      call assmbj(.true.,elmat,lsurf,ddd,ie,
     +                        ni(i+1),ni(i+2),djgs,gs1,gs2,.true.,
     +                        dgs1,dgs2,e21,dedv,lfldmb,idum)
                     wrhsi=wrhsi+djcoef*djgs
                  endif
               endif
            endif

            if(ljdisp) then
               e21=(fv(i1)-ofv(i1)-fv(i2)+ofv(i2))
               wrhsi=wrhsi-decoef*ehed(i,ie)*e21*deetee
            endif

            wsol(ie)=wsol(ie)+wrhsi
            wsol(inxt)=wsol(inxt)-wrhsi

10       continue

100   continue

cc       do 166 i=1,nepb
cc 166      write(*,*) 'i,wrhs : ',i,wsol(i)
c
c-----------------------
c  Linear system solve
c-----------------------
c
c...Set up pointers to a for potential in real elements.
c...Uncompressed Calahan sparsity pattern using Conor's (backwards)
c...convention for ia,ja.
      esiz=0
      do 200 i=1,ne
         cia(i)=esiz+1
         civa(i)=cia(i)
c
c...Find all adjacent elements that need to be solved for.
         iwsrt(1)=i
         iok=1
         do 201 j=1,3
            ie=wnxtel(j,i)
            if(ie.lt.0) then
               ie=-ie
               iwelpt(ie-ne)=i
               if(wzero(ie-ne)) then
                  wsol(ie)=0.d0
               else
                  iok=iok+1
                  iwsrt(iok)=ie
               endif
            else
               iok=iok+1
               iwsrt(iok)=ie
            endif
201      continue
c
c...Sort them by index and set up pointers for this column.
         call wsort(iwsrt,iok)
         do 205 j=1,iok
            esiz=esiz+1
            ie=iwsrt(j)
            cja(esiz)=ie
205      continue
200   continue
c
c...Now do columns for boundary elements.  
      j=ne+1
      do 210 i=j,nepb
         esiz=esiz+1
         cia(i)=esiz
         civa(i)=esiz
         if(.not.wzero(i-ne)) then
           cja(esiz)=iwelpt(i-ne)
           esiz=esiz+1
         endif
         cja(esiz)=i
210   continue
      cia(nepb+1)=esiz+1
      civa(nepb+1)=esiz+1
      if(esiz.gt.adim) then
         write(lutty,*) 
     +      'Array bounds exceeded for LHS in flow-line calculation'
         stop
      endif
c
c...If direct solve, do symbolic
      if(.not.lwiccg) then
         if(ldebug) write(lutty,1003)
1003     format(/' Computing current potentials (symbolic)....')
         i=0
         call md(nepb,cia,cja,iysiz,ludim,tv,tl,head,cil,ciu,tv,i)
         if(i.ne.0) then
            write(lutty,*) 
     +         'Array bounds exceeded in minimum degree for flow-lines'
            stop
         endif
         do 701 i=1,nepb
            iwpc(i)=cil(i)
            iwpri(i)=ciu(i)
701      continue

         do 702 i=1,esiz
            cja(i)=-cja(i)
702      continue
         call vmsp(nepb,iwpc,iwpri,cia,cil,ciu,cja,cjl,cju,
     +             civa,civl,civu,
     +             ludim,jldim,judim,lsiz,usiz,jlsiz,jusiz,
     +             kp,ixt,ixb)
         if(ldebug) then
            write(lutty,*) 'jlsiz = ',jlsiz,'(',jldim,')'
            write(lutty,*) 'jusiz = ',jusiz,'(',judim,')'
            write(lutty,*) ' lsiz = ', lsiz,'(',ludim,')'
            write(lutty,*) ' usiz = ', usiz,'(',ludim,')'
         endif
         if((jlsiz.gt.jldim).or.(jusiz.gt.judim).or.
     +      (lsiz.gt.ludim).or.(usiz.gt.ludim)) then
            write(lutty,*) 
     +         'Symbolic array bounds exceeded in flow-line calculation'
            stop
         endif
         if(errflg) goto 999
      endif
c
c...Assemble lhs (a)
      do 600 i=1,nepb
         i1=cia(i)
         i2=cia(i+1)-1
         do 600 j=i1,i2
            k=iabs(cja(j))
            if(i.eq.k) then
               if(i.le.ne) then
                  a(j)=3.d0
               else
                  a(j)=1.d0
               endif
            else 
               a(j)=-1.d0
            endif
600   continue
c
c...ICCG
      if(lwiccg) then
         if(ldebug) write(lutty,1002)
1002     format(/' Computing current potentials (ICCG)....')
         litex=.false.
         res00=0.d0
         tmp1=ptolg
         tmp2=lu1cri
         ptolg=0.d0
         lu1cri=wlucr
         call icp1(nepb,esiz,cia,ilsh,cja,a,aw,l,di,x)
         if(errflg) goto 999
         call ascale(.true.,wsol,asfac,nepb)
         call icp2(nepb,cia,ilsh,cja,a,l,di,wsol,resid,
     +             p,ap,qinvr,aqinvr,asfac,mxsor,litex)
         if(errflg) goto 999
         call ascale(.false.,wsol,asfac,nepb)
         if(litex) then
            write(lutty,*) 
     +         'ICCG iteration limit exceeded in flow-line calculation'
            stop
         endif
         ptolg=tmp1
         lu1cri=tmp2
c
c...Direct solve
      else
         if(ldebug) write(lutty,1007)
1007     format(/' Computing current potentials (decomposition)....')
         call vmnpd(nepb,a,l,u,di,x,
     +              cia,cil,ciu,cja,cjl,cju,civa,civl,civu,iwpc,iwpri)
         if(errflg) goto 999
         call vmbpcd(nepb,l,u,wsol,di,x,wsol,
     +              cil,ciu,cjl,cju,civl,civu,iwpc,iwpri)
         if(errflg) goto 999
      endif
c
c...Converged - offset all potentials so they are positive
c...and set flag
      wsmin=wsol(1)
cc      write(*,*) 'i,wsol : ',1,wsol(1)
      do 300 i=2,nepb
cc         write(*,*) 'i,wsol : ',i,wsol(i)
         wsmin=dmin1(wsmin,wsol(i))
300   continue
      do 310 i=1,nepb
         wsol(i)=wsol(i)-wsmin
310   continue
      lflow=.true.
c
c...Bye!
999   nepb=nepb0
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE WEDGE
      include 'p2conf.h'
c
c     Find a set of common edge nodes for reference potential.
c
c     Orig: MRP May 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      integer i,j,k,ileft,iright,ie,nxt0,ni(5),ie0,ie2,irx,ir0
      data ni/1,2,3,1,2/
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Find a non-electrode edge
      do 10 ie=1,ne
      do 10 i=1,3
         nxt0=wnxtel(i,ie)
         if(nxt0.gt.0) goto 10
         ileft=nop(ni(i+1),ie)
         iright=nop(ni(i+2),ie)
         if((lm(ileft).le.0).or.(lm(iright).le.0)) goto 20
10    continue
      write(*,*) 'Internal error 1 in WEDGE - seek help!'
      stop
c
20    ie0=ie
      wzero(-nxt0-ne)=.true.
      if(ldbug2) write(*,*) '(0) edge at ',-nxt0,
     +                      '   ie,ileft,iright=',ie,ileft,iright
      ir0=iright
      irx=ileft
c
c...First search "right" for either an electrode or "left" node
30    if(iright.eq.ileft) goto 99
      if(lm(iright).gt.0) goto 40
      do 35 i=1,p2tc(iright)
         ie2=p2t(i,iright)
         j=1
         if(nop(1,ie2).eq.iright) goto 31
         j=2
         if(nop(2,ie2).eq.iright) goto 31
         j=3
31       k=ni(j+1)
         nxt0=wnxtel(k,ie2)
         if((nxt0.lt.0).and.
     +     ((ie2.ne.ie).or.(nop(k,ie2).eq.irx))) goto 36
         k=ni(j+2)
         nxt0=wnxtel(k,ie2)
         if((nxt0.lt.0).and.
     +     ((ie2.ne.ie).or.(nop(k,ie2).eq.irx))) goto 36
35    continue
      write(*,*) 'Internal error 2 in WEDGE - seek help!'
      stop
36    continue
      irx=iright
      iright=nop(6-j-k,ie2)
      ie=ie2
      wzero(-nxt0-ne)=.true.
      if(ldbug2) 
     +   write(*,*) '(R) edge at ',-nxt0,'   ie,iright=',ie,iright
      goto 30
c
c...Found an electrode going right.  Now look "left" until we hit
c...another electrode.
40    ie=ie0
      irx=ir0
44    if(lm(ileft).gt.0) goto 99
      do 45 i=1,p2tc(ileft)
         ie2=p2t(i,ileft)
         j=1
         if(nop(1,ie2).eq.ileft) goto 41
         j=2
         if(nop(2,ie2).eq.ileft) goto 41
         j=3
41       k=ni(j+1)
         nxt0=wnxtel(k,ie2)
         if((nxt0.lt.0).and.
     +      ((ie2.ne.ie).or.(nop(k,ie2).eq.irx))) goto 46
         k=ni(j+2)
         nxt0=wnxtel(k,ie2)
         if((nxt0.lt.0).and.
     +      ((ie2.ne.ie).or.(nop(k,ie2).eq.irx))) goto 46
45    continue
      write(*,*) 'Internal error 3 in WEDGE - seek help!'
      stop
46    continue
      irx=ileft
      ileft=nop(6-j-k,ie2)
      ie=ie2
      wzero(-nxt0-ne)=.true.
      if(ldbug2) 
     +   write(*,*) '(L) edge at ',-nxt0,'   ie,ileft=',ie,ileft
      goto 44
c
c...Got it all - bye!
99    return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE WSORT(isort,n)
c
c     Sort array.
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      integer n,isort(1),i,j,j1,itmp
c
      do 10 i=2,n
         j=i
5        j1=j-1
         if(isort(j).ge.isort(j1)) goto 10
         itmp=isort(j)
         isort(j)=isort(j1)
         isort(j1)=itmp
         j=j1
         if(j.gt.1) goto 5
10    continue 
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE WNODE
      include 'p2conf.h'
c
c     Weight by areas unless node is on edge and is not electrode
c
c     Orig : MRP April, 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      integer i,ie,j,k,i1,i2,ni(5)
      integer nusael
      logical lelct,lxedge,lsnod
      double precision ddd,xedge
      data ni/1,2,3,1,2/
c------------------------------------------------------------------
c
      do 400 i=1,np

         lelct=lm(i).gt.0
         lsnod=mattyp(itype(i)).gt.0
         lxedge=.false.
         x(i)=0.d0
         ddd=0.d0

         do 410 j=1,p2tc(i)
            ie=p2t(j,i)
            if(.not.ltdep.and.lsnod.and.
     +         (mattyp(imat(ie)).lt.0)) goto 410
            k=1
            if(nop(1,ie).ne.i) then
               k=2
               if(nop(2,ie).ne.i) k=3
            endif

            i1=wnxtel(ni(k+1),ie)
            i2=wnxtel(ni(k+2),ie)
            if(.not.lelct.and.((i1.lt.0).or.(i2.lt.0))) then
               if(.not.lxedge.or.(mattyp(imat(ie)).gt.0)) then
                  lxedge=.true.
                  if(i1.lt.0) xedge=wsol(-i1)
                  if(i2.lt.0) xedge=wsol(-i2)
               endif
            else
               x(i)=x(i)+es(k,ie)*wsol(ie)
               ddd=ddd+es(k,ie)
            endif

410      continue

         if(lxedge) then
            x(i)=xedge
         else
            x(i)=x(i)/ddd
         endif

400   continue
c
      do 420 i=1,np
c
c...c.c.abbas
c...Find electrode ends and assign correct values to them
         lelct=lm(i).gt.0
         if(lelct) then
c- Check for ends of electrodes, which by the logic in the 410 loop got a
c- WRONG averaged value!
            nusael=0
            do 421 j=1,p2tc(i)
c- Loop over all triangles which share node i
               ie=p2t(j,i)
               do 421 k=1,3
c- Loop over all nodes of triangles which share node i and check whether
c- such a node is part of the same electrode as node i
                  if(nop(k,ie).ne.i.and.
     &               lm(nop(k,ie)).eq.lm(i)) nusael=nusael+1
  421       continue      

c      write(*,*) ' i=',i,' nusael=',nusael
            if(nusael.eq.1) then
               do 422 j=1,p2tc(i)
c- Loop over all triangles which share node i
                  ie=p2t(j,i)
                  k=1
                  if(nop(1,ie).ne.i) then
                     k=2
                     if(nop(2,ie).ne.i) k=3
                  endif

                  i1=wnxtel(ni(k+1),ie)
                  i2=wnxtel(ni(k+2),ie)
                  if(i1.lt.0.and.lm(nop(ni(k+2),ie)).ne.lm(i))
     &                                              x(i)=wsol(-i1)
                  if(i2.lt.0.and.lm(nop(ni(k+1),ie)).ne.lm(i))
     &                                              x(i)=wsol(-i2)

  422          continue      
          
            endif
         endif
         wsol(i)=x(i)
420   continue
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION LEMBED(iel)
      include 'p2conf.h'
c
c     Original: Mark R. Pinto   Stanford University  May,1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'emaco.h'
c
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'flowtmp.h'
      integer TMPPAD(2)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
      integer iel,i,ipt,ielec,j,ie,ii,ni(5)
      logical lembar(MAXCNT),lthis
      data lembar/MAXCNT*.false./,ni/1,2,3,1,2/
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      lembed=.false.
      iel=0
c
c...First check all electode nodes
      do 100 i=1,nb
         ipt=nbc(i)
         ielec=lm(ipt)

         if(.not.lembar(ielec)) then
            lthis=.false.
            do 110 j=1,p2tc(ipt)
               ie=p2t(j,ipt)
               ii=1
               if(nop(ii,ie).ne.ipt) then
                  ii=2
                  if(nop(ii,ie).ne.ipt) ii=3
               endif
               lthis=(wnxtel(ni(ii+1),ie).le.0).or.
     +               (wnxtel(ni(ii+2),ie).le.0)
               if(lthis) goto 120
110         continue
120         lembar(ielec)=lthis
         endif

100   continue
c
c...Now check electrode flags
      do 200 i=1,nelect
         if(.not.lembar(i)) goto 250
200   continue
      goto 999
250   lembed=.true.
      iel=i
c
c...Bye
999   return
      end
