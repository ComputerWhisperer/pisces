cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ASAVEN(estifm,wrhs,inode,lenod,imult)
      include 'p2conf.h'
c
c     Subroutine to store the assembled window (estifm=LHS, wrhs=RHS)
c     in the a vector for the full Newton. 
c
c     Structure:
c
c         1    2    3
c
c     1   x    x    x       The window is made up of 9 sub-blocks,
c                           for the node-to-node dependencies of
c     2   x    x    x       each triangle.
c   
c     3   x    x    x
c
c
c     Each sub-block (denoted by an x) can be as large as a 3x3
c     (if 3 variables are being solved for - psi,n,p).  Since the
c     a matrix is ordered by column and since each column for a
c     particular node is identical (in storage) for each variable, 
c     we need only find the address of the (1,1) element in each 
c     sub-block. The other elements in each sub-block can be found 
c     by offsets (iva from column to column, 1 from row to row).
c
c     Original : MRP      Stanford University           Mar, 1984
c     Revised  : MRP  (lumped elements)                 Dec, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'soltmpn.h'
      integer TMPPAD(219007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision estifm(9,9),wrhs(9),tmp,dddiag
      integer i,j,ii,ipde,irow(3),inode(3),idx(3),ie
      integer jpde,ix2,jj,icol,imult,mapa,ilenj,irow1,irow2,irow3
      integer idx1,idx2,idx3,ix3,ilbc(3),nlbc,ibcdia
      logical lenod(3),iskip(3),iskip1,iskip2,iskip3,ldirj
      equivalence (irow1,irow(1)),(irow(2),irow2),(irow(3),irow3)
      equivalence (idx1,idx(1)),(idx(2),idx2),(idx(3),idx3)
      equivalence (iskip1,iskip(1)),(iskip(2),iskip2),(iskip(3),iskip3)
c
      data dddiag/1.d0/
c
c*************
c**  START  **
c*************
c
c...Find row/col numbers and column lengths
      irow1=imult*(inode(1)-1)+1
      irow2=imult*(inode(2)-1)+1
      irow3=imult*(inode(3)-1)+1
c
c-------
c  LHS 
c-------
c
c...Loop through node columns
      jj=0
      do 350 j=1,3
      icol=irow(j)
      ilenj=iva(icol+1)-iva(icol)
      idx1=mapa(irow1,icol)
      iskip1=idx1.le.0
      idx2=mapa(irow2,icol)
      iskip2=idx2.le.0
      idx3=mapa(irow3,icol)
      iskip3=idx3.le.0
c
c...Loop through each variable col's in sub-block
      do 350 jpde=1,imult
      jj=jj+1
      if(jpde.gt.1) then
         idx1=idx1+ilenj
         idx2=idx2+ilenj
         idx3=idx3+ilenj
      endif
c
c...Is this a Dirichlet column? (new - MRP)
c...Was a good idea but won't work w/ AC analysis
      ldirj=.false.
cc      ldirj=lenod(j)
cc      if(ldirj) then
cc         ie=lm(inode(j))
cc         if(jpde.gt.1) then
cc            ldirj=.not.schotk(ie)
cc         else
cc            ldirj=.not.(lcnres(ie).or.lcurbc(ie).or.lresis(ie))
cc         endif
cc      endif
c
c...Loop through node rows
      ii=0
      do 350 i=1,3
      if(iskip(i)) goto 349
c
c...Store column of sub-block
      ix2=idx(i)
      do 50 ipde=1,imult
      ii=ii+1
c
c...If electrode, special case
      if(lenod(i)) then
         ie=lm(inode(i))
         nlbc=nresis(ie)
         ibcdia=ialump(ie)

         if(ipde.eq.1) then

            if(lcnres(ie).or.lresis(ie).or.lcurbc(ie)) then
               if(lresis(ie).or.lcurbc(ie)) then
                  if(jpde.eq.1) then
                     ilbc(i)=mapa(nlbc,icol)
                     if(lm(inode(j)).eq.ie) then
                        ix3=ibcdia
                     else
                        ix3=ilbc(i)
                     endif
                  else
                     ilbc(i)=ilbc(i)+ilenj
                     ix3=ilbc(i)
                  endif
               else
                  ix3=ix2
               endif
               if(ltdep) then
                  tmp=estifm(ii,jj)*deltf
                  if(ltrule) tmp=tmp+tmp
                  a(ix3)=a(ix3)-tmp
               endif
            endif

            if(.not.lcnres(ie).and.(ii.eq.jj)) then
               a(ix2)=dddiag
               if(lresis(ie).or.lcurbc(ie)) 
     +            a(mapa(irow(i),nlbc))=-dddiag
            endif

         else
            if(schotk(ie)) then
               a(ix2)=a(ix2)+estifm(ii,jj) 
            else
               if(ii.eq.jj) a(ix2)=dddiag
            endif

            if(lresis(ie).or.lcnres(ie).or.lcurbc(ie)) then
               if(ipde.eq.2) then
                  if(l1hole) then
                     a(ix3)=a(ix3)+estifm(ii,jj)
                  else
                     a(ix3)=a(ix3)-estifm(ii,jj)
                  endif
               else
                  a(ix3)=a(ix3)+estifm(ii,jj)
               endif
            endif

         endif
c
c...Regular node (i) - but check j too! (new - MRP)
      else if(ldirj) then
         a(ix2)=0.d0
      else
         a(ix2)=a(ix2)+estifm(ii,jj) 
      endif

      ix2=ix2+1
50    continue
      goto 350
c
c...End of loop
349   ii=ii+imult
350   continue
c
c-------
c  RHS
c-------
c
      ii=0
      do 351 i=1,3
      ix2=irow(i)
      do 351 j=1,imult
      ii=ii+1
      rhs(ix2)=rhs(ix2)+wrhs(ii)
      ix2=ix2+1
351   continue
c
c...Done
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ASAVEG(ipde,estifm,wrhs,inode,lenod)
      include 'p2conf.h'
c
c     Subroutine to store the assembled window (estifm=LHS, wrhs=RHS)
c     in the a vector for the Gummel method.
c
c     Original : MRP      Stanford University           Mar, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'setup.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'soltmpg.h'
      integer TMPPAD(1139007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision estifm(9,9),wrhs(9),dddiag
      integer i,j,irow(3),inode(3),idx2,icol,mapa,ipde,ie
      logical lenod(3)
c
      data dddiag/1.d0/
c
c*************
c**  START  **
c*************
c
c...Find row/col numbers (the same as node no.'s for Gummel)
      do 10 i=1,3
10    irow(i)=inode(i)
c
c-------
c  LHS 
c-------
c
c...Loop through node columns
      do 350 j=1,3
      icol=irow(j)
c
c...Loop through node rows
      do 350 i=1,3
c
c...Get address and store
      idx2=mapa(irow(i),icol)
      if(idx2.le.0) goto 350
      if(lenod(i)) then
         ie=lm(inode(i))
         if(schotk(ie).and.(ipde.gt.1)) then
            a(idx2)=a(idx2)+estifm(i,j) 
         else
            if(i.eq.j) a(idx2)=dddiag
         endif
      else if(lenod(j)) then
         if(schotk(lm(inode(j)))) then
            a(idx2)=a(idx2)+estifm(i,j) 
         else
            a(idx2)=0.d0
         endif
      else
         a(idx2)=a(idx2)+estifm(i,j) 
      endif
c
c...End of loop
350   continue
c
c-------
c  RHS
c-------
c
      do 351 i=1,3
      idx2=irow(i)
      rhs(idx2)=rhs(idx2)+wrhs(i)
351   continue
c
c...Done
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE INITN
      include 'p2conf.h'
c
c     Subroutine to initialize LHS,RHS for full Newton.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'symb.h'
c....the magic TMP common memory overlays ....
      include     'soltmpn.h'
      integer TMPPAD(219007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i
c
c...rhs
      do 201 i=1,neq
201   rhs(i)=0.d0 
c
c...lhs
      do 250 i=1,asiz 
250   a(i)=0.d0 
c
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE INITG
      include 'p2conf.h'
c
c     Subroutine to initialize LHS,RHS for Gummel.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'symb.h'
c....the magic TMP common memory overlays ....
      include     'soltmpg.h'
      integer TMPPAD(1139007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i
c
      do 200 i=1,neq
200   rhs(i)=0.d0 
      do 250 i=1,asiz 
250   a(i)=0.d0 
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RESCL(a,rhs,xwork)
      include 'p2conf.h'
c
c     Diagonal rescaling of A and RHS for full Newton.
c
c     Orig : CSR
c     Rev  : MRP 
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer ci,ri,its,ite,aix,clo,chi,ix
      double precision xwork(1),a(1),rhs(1)
c
c******************** Start ********************************************
c
c......With column-ordered scheme, we have a two pass alogrithm, as usual.
c
c......Get pivots
      aix=0
      do 1000 ci=1,neq
         clo = ja(ci)
         chi = ja(ci+1) -1
         ix=clo
  900    if (ix.gt.chi) goto 1000
            its = ia(ix)
            if (its.lt.0) then
               its = -its
               ite = its
               ix=ix+1
            else
               ite=ia(ix+1)
               ix=ix+2
            endif
            do 800 ri=its,ite
               aix=aix+1
               if (ri.eq.ci) then 
                  xwork(ri) = 1.d0/a(aix) 
          if (mod(ri,nmult).eq.1) xwork(ri)=xwork(ri)*pscal
               endif
  800       continue
         go to 900
 1000 continue
c
c......Scale by pivots
      aix=0
      do 1001 ci=1,neq
         clo = ja(ci)
         chi = ja(ci+1) -1
         ix=clo
  901    if (ix.gt.chi) goto 1001
            its = ia(ix)
            if (its.lt.0) then
               its = -its
               ite = its
               ix=ix+1
            else
               ite=ia(ix+1)
               ix=ix+2
            endif
            do 801 ri=its,ite
               aix=aix+1
               a(aix)=a(aix)*xwork(ri) 
  801       continue
         go to 901
 1001 continue
c
      do 1500 ri=1,neq
 1500     rhs(ri)=rhs(ri)*xwork(ri) 
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BCNEWT
      include 'p2conf.h'
c
c     Boundary conditions for full Newton.
c
c     Orig: MRP Dec 84
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
      include     'sol.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      include     'soltmpn.h'
      integer TMPPAD(219007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer i,nlbc,ipt,iel,idxa,mapa,j
      double precision cap,jdisp,cres,xx1,xx2,phi,jctot
c
c...Poisson
      call pbound(rhs)
c
c...Continuity
c
c...Terminal currents
      if(l1hole) then
         call cjterm(3,2,rhs,rhs0)
      else
         call cjterm(2,2,rhs,rhs0)
         if(ncarr.eq.2) call cjterm(3,3,rhs,rhs0)
      endif
c
c...Adjust for contact resistance
      do 9 i=1,nb
      ipt=nbc(i)
      iel=lm(ipt)
      if(lcnres(iel)) then
         cres=cresis(iel)*lmetal(i)
         nlbc=nmult*(ipt-1)+1
         idxa=mapa(nlbc,nlbc)
         a(idxa)=a(idxa)+cres
         call poten(lboltz,ipt,r1(ipt),cnie(ipt),xx1,xx2,phi)
         if(l1hole) then
            jctot=-rhs(nlbc+1)
         else
            jctot=rhs(nlbc+1)
            if(ncarr.eq.2) jctot=jctot-rhs(nlbc+2)
         endif
         rhs(nlbc)=cres*(bias(iel)-fv(ipt)+phi)-jctot
      endif
9     continue
c
c...Adjust for lumped-element (ss)
      do 10 i=1,nelect
      if(lresis(i)) then
         nlbc=nresis(i)
         cres=cresis(i)
         a(ialump(i))=a(ialump(i))+cres
         rhs(nlbc)=cres*(bias(i)-vres(i))-
     +                  (namp(i,1)+namp(i,2))
      else if(lcurbc(i)) then
         nlbc=nresis(i)
         rhs(nlbc)=jbias(i)-(namp(i,1)+namp(i,2))
      endif
10    continue
c
c...Save rhs?
      if(lsvrhs) then
         do 20 i=1,neq
20       x(i)=rhs(i)
      endif
c
c...Finish continuity
      if(l1hole) then
         call cfinsh(3,2,a,rhs,rhs0)
      else
         call cfinsh(2,2,a,rhs,rhs0)
         if(ncarr.eq.2) call cfinsh(3,3,a,rhs,rhs0)
      endif
c
c...Time-dep for lumped element bc or contact res
      if(.not.ltdep) goto 999
      do 30 i=1,nelect
         if(lresis(i).or.lcurbc(i)) then
            nlbc=nresis(i)
            cap=ccapac(i)*deltf*dktq
            jdisp=cap*(vres0(i)-vres(i))-
     +            deltf*(dflux(i)-dflux0(i)/decoef)
            idxa=ialump(i)
            if(ltrule) then
               rhs(nlbc)=rhs(nlbc)+rhs0(nlbc)+jdisp+jdisp
               a(idxa)=a(idxa)+cap+cap
            else
               rhs(nlbc)=rhs(nlbc)+jdisp
               a(idxa)=a(idxa)+cap
            endif
         else if(lcnres(i)) then
            do 31 j=1,nb
               ipt=nbc(j)
               iel=lm(ipt)
               if(iel.eq.i) then
                  nlbc=nmult*(ipt-1)+1
                  jdisp=-deltf*(dfxpt(j)-dfxpt0(j)/decoef)
                  idxa=mapa(nlbc,nlbc)
                  if(ltrule) then
                     rhs(nlbc)=rhs(nlbc)+rhs0(nlbc)+jdisp+jdisp
                  else
                     rhs(nlbc)=rhs(nlbc)+jdisp
                  endif
               endif
31          continue
         endif

30    continue
c
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BCGUMM(npde)
      include 'p2conf.h'
c
c     Boundary conditions for Gummel.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c....the magic TMP common memory overlays ....
      include     'soltmpg.h'
      integer TMPPAD(1139007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      double precision dum
      integer npde
c------------------------------------------------------------------
c
      if(npde.eq.1) then
         call pbound(rhs)
      else
         call cjterm(npde,1,rhs,dum)
         call cfinsh(npde,1,a,rhs,dum)
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PBOUND(rhs)
      include 'p2conf.h'
c 
c     This routine calculates terminal fluxes from a just assembled
c     RHS.  The RHS elements used are replaced with 0's to implement
c     Dirchlet boundary conditions.
c
c     Original : MRP          Stanford University       Mar, 1984
c     Revised  : MRP  (lumped elements)                 Dec, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c 
c                   type declarations 
c 
      double precision rhs(1)
      integer i,ii,j,ipt
c 
c****************************************************************** 
c 
c                   start 
c
c...Zero array and sum flux
      do 1200 i=1,nelect
1200  dflux(i)=0.d0
      do 1250 i=1,nb
         ipt=nbc(i)
         j=lm(ipt)
         ii=nmult*(ipt-1)+1
         dflux(j)=dflux(j)+rhs(ii)
         dfxpt(i)=rhs(ii)
         rhs(ii)=0.d0
1250  continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CJTERM(npde,idxpde,rhs,rhs0)
      include 'p2conf.h'
c 
c     Calculate terminal currents for carrier specified by npde/idxpde.
c
c     Original : MRP          Stanford University       Mar, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical lpcont
      integer npde,icarr,i,ii,j,inode,idxpde
      double precision rhs(1),rhs0(1),jctot
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize
      icarr=npde-1
      lpcont=npde.eq.3
c
c...Loop through electrode nodes - skip Schottkys (already
c...accouted for earlier)
      do 200 i=1,nb 
      inode=nbc(i)
      j=lm(inode)
      if(schotk(j)) goto 200
      ii=nmult*(inode-1)+idxpde
c
      jctot=rhs(ii)
cc      if(ltdep.and.ltrule) jctot=0.5d0*(jctot+rhs0(ii))
      if(lpcont) then
         namp(j,icarr)=namp(j,icarr)-jctot
      else
         namp(j,icarr)=namp(j,icarr)+jctot
      endif
c
200   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CFINSH(npde,idxpde,a,rhs,rhs0)
      include 'p2conf.h'
c 
c     Finish continuity equation.  
c     Put in time dependency and Dirchlet bc's.
c
c     Original : MRP          Stanford University       Mar, 1984
c     Revised  : MRP  (lumped elements)                 Dec, 1984
c     Revised  : MRP  (2nd order time disc.)            Dec, 1984
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
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
      include     'sol.h'
c....the magic TMP common memory overlays ....
      include     'emaco.h'
      integer TMPPAD(1589007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   type declarations 
c
      logical lpcont,lenod,ldodi
      integer npde,icarr,ii,j,inode,idxa,mapa,idxpde
      double precision rhs(1),a(1),rhs0(1)
      double precision djdn,dndt,jrhs
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize
      icarr=npde-1
      lpcont=npde.eq.3
c
c--------
c  Loop
c--------
c
      ii=idxpde
      do 200 inode=1,np
c
c...If time dependent, get carrier derivative
      if(ltdep) then
         djdn=essem(inode)*deltf
         if(lpcont) then
            if(l2bdf) then
               dndt=(fp(inode)-tgam1*fptr(inode)+
     +                         tgam0*ofp(inode))*djdn
            else
               dndt=(fp(inode)-ofp(inode))*djdn
            endif
         else
            if(l2bdf) then
               dndt=(fn(inode)-tgam1*fntr(inode)+
     +                         tgam0*ofn(inode))*djdn
            else
               dndt=(fn(inode)-ofn(inode))*djdn
            endif
         endif
      else
         dndt=0.d0
      endif

      if(ltdep.and.ltrule) then
         djdn=djdn+djdn
         jrhs=rhs(ii)+dndt+dndt+rhs0(ii)
      else
         jrhs=rhs(ii)+dndt
      endif
c
c...If a contact node with finite srv, adjust terminal current
c...since dndt is not zero (as for Dirchlet)
      j=lm(inode)
      lenod=j.gt.0
      if(lenod) then
         if(schotk(j)) then
            if(lpcont) then
               namp(j,icarr)=namp(j,icarr)-dndt
            else
               namp(j,icarr)=namp(j,icarr)+dndt
            endif
         endif
      endif
c
c...RHS
      if(lenod) then
         if(schotk(j)) then
            rhs(ii)=jrhs
         else
            rhs(ii)=0.d0
         endif
      else
         rhs(ii)=jrhs
      endif
c
c...LHS
      if(ltdep) then
         ldodi=.not.lenod
         if(lenod) ldodi=schotk(j)
         if(ldodi) then
            idxa=mapa(ii,ii)
            a(idxa)=a(idxa)-djdn
         endif
      endif

      ii=ii+nmult
200   continue
c
      return
      end
