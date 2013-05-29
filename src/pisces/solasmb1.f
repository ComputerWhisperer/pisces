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
      SUBROUTINE ASSMB(lplug,ipde)
      include 'p2conf.h'
c 
c     This routine formulates the equations (a matrix) for 
c     Poisson's eq. and the n/p continuity eqs.
c 
c     If plug-in iteration is taking place (lplug=.true.), then
c     only one equation is being assembled as specified by ipde - 
c     
c            ipde =  1    Poisson's equation
c                    2    Electron continuity equation
c                    3    Hole continuity equation
c
c 
c     Original :  MRP          Stanford University       Nov, 1983
C     MODIFIED :  SHIN         UT                        4/88
C                   ADD COMMON/SHINTM AND CALL E FIELD CALCULATION
C                   SUBROUTINE (SHINTF) FOR THE TRAN. E DEP.
C                   MOBILITY MODEL
c     Modified :  G. Anderson  Stanford University       Nov, 1989
c                   SHINTF renamed TREFLD, common moved into 
c                   include file.
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
      include     'trfldmob.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c type declarations 
c 
      logical lplug,lenod(3),lreglr
      integer mater,iedim,ipde,npde,i,j,n,imult
      integer elmat,idxnod(3),nmobct
      double precision estifm(9,9),factor,wrhs(9)
      real esnod(3)
c 
c****************************************************************** 
c 
c                       start 
c
      nmobct=0
      lreglr=.true.
c
c...Use npde to carry ipde since it can be changed
      npde=ipde
c 
c...Initialize matrices for full Newton/Gummel
      imult=nmult
      if(lgumm) then
         call initg
      else 
         call initn
      endif
c
c...Initialize current(s)
      if(lplug) then
         if(ipde.gt.1) then
            j=ipde-1
            if(l1hole) j=2
            do 1 i=1,nelect
               namp(i,j)=0.d0
1           continue
         endif
      else
         do 2 i=1,nelect
            namp(i,1)=0.d0
            namp(i,2)=0.d0
2        continue
      endif
c
c...Window dimension
      iedim=imult*3
c 
c...Compute acceleration factor (if needed) 
      factor=1.d0 
      if(.not.laccel) goto 305
      factor=acstrt+float(iterp)*acstep 
      if (sngl(factor).gt.acstop) factor=acstop 
c 
c-------------------------------------------------------------
c 
c                  ASSEMBLE BY ELEMENT
c 
c...initialize transverse-field mobilities to zero and call
c...TREFLD to calculate transverse e-fields.  All the action
c...in TREFLD is in common, hence no passed parameters (I know...)
c
305   IF(LTFLDMB) THEN
        DO 101 N=1,NP
          TMOBN(1,N)=0.0
          TMOBN(2,N)=0.0
          TMOBP(1,N)=0.0
          TMOBP(2,N)=0.0
          LTFM(N)=.FALSE.
101     CONTINUE
        CALL TREFLD
      ENDIF
      do 400 n=1,ne
c 
c...Get material number and type for element and set flag for
c...each node if an electrode
      mater=imat(n) 
      elmat=mattyp(mater)
      do 401 i=1,3
      esnod(i)=es(i,n)
      idxnod(i)=nop(i,n)
401   lenod(i)=lm(idxnod(i)).ne.0
c 
c...Initialize estifm, wrhs
      do 310 i=1,iedim
      wrhs(i)=0.d0
      do 310 j=1,iedim
      estifm(i,j)=0.d0
310   continue
c
c---------------------
c  EQUATION ASSEMBLY
c---------------------
c
      call elemn(n,lplug,wrhs,estifm,iedim,elmat,esnod,idxnod,factor,
     +           imult,npde,nmobct)
c
c--------------------------------
c  STORE ASSEMBLY (estifm,wrhs)
c--------------------------------
c 
c...Store estifm in a with map as pointer by columns.
c...Mapping must be done in same order as done in symbolic
c...(ie. element order, nodes 1 to 3) 
c
c...Dirchlet boundary conditions are partially implemented here.  
c...If a row corresponds to a boundary node, only the diagonal element 
c...is put into a.  All the other LHS terms for the row are ignored
c...(i.e., will be 0).
c
      if(lnewt) then
          call asaven(estifm,wrhs,idxnod,lenod,imult)
      else 
         call asaveg(npde,estifm,wrhs,idxnod,lenod)
      endif
c
c...Next element
400   continue
c
c-----------------------------------------------------------------------
c
c                           MORE BC's
c
c...Dirchlet bc's were implemented above in the LHS only.
c...For both Schottky and Dirchlet bc's, we did nothing to the RHS.  
c...That leaves the contact conduction current/electric flux in the 
c...RHS of each respective electrode node.
c
c...After getting the current/flux, we must adjust the RHS 
c...A pure Dirchlet
c...must have a 0 on the RHS so its Newton update is forced to 0. 
c...Lumped R/C, dist R and I bc's are implemented here.
c
      if(lgumm) then
         call bcgumm(npde)
      else
         call bcnewt
      endif
c
c...Debug.
      if(ldebug) then
         if(nmobct.gt.0) 
     +      write(luout,*) nmobct,' negative mobility derivatives'
      endif
c
c...Done
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE ELEMN(ie,lplug,wrhs,estifm,iedim,elmat,esnod,idxnod,
     +                 factor,imult,npde,nmobct)
      include 'p2conf.h'
c 
c       This routine computes the matrix equation entries 
c       (a and rhs) for an element. 
c 
c       For plug-in (lplug=.true.) :
c
c             npde = 1   Poisson eqn.
c                    2   Electron continuity eqn.
c                    3   Hole continuity eqn.
c
c
c       input : ie     : the current element no. 
c               elmat  : material type of element
c               factor : iteration accelerating factor 
c               lplug  : flag to indicate plug-in iteration
c               elmat  : material type of element
c               esnod  : area associated with each node
c               idxnod : node numbers for triangle verticies
c               imult  : no. of equations at each node
c               nmobct : debug 
c 
c       output: estifm : (iedim)x(iedim) matrix elements array 
c               wrhs   : window to the right hand side
c
c     Original :  MRP          Stanford University       Nov, 1983
c     Modified :  HRY          Stanford University       July,1985
c              => Incomplete Inonization and Fermi-Dirac statistics.
c     Modified :  C.C.Abbas (Carrier-carrier scattering) Sep, 1987
c     Modified :  A. Yabuta  (impact ionization)         Oct, 1987
c     Modified :  G. Anderson (photogeneration)         June, 1989
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'impact.h'
      include     'symme.h'
      include     'photo.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lplug,lpcont,ldmu,lsurf
      integer ni(5),ie,i1,i2,inode,jnode,i1node,i2node,npde,iedim
      integer nidisp(5),i,j,i1p,i2p,imult,elmat,idxpde,idxnod(3)
      integer ni1,ni2,nmobct,iel,i3,i3p
      double precision estifm(9,9),factor,djgs,tmp,gs1(2),gs2(2),hd
      double precision ui(3),dudv(3),dudn(3),dudp(3)
      double precision dgs1(2),dgs2(2),wrhs(9),dnc1,dpc1
      double precision e21,dedv,dnc2,dpc2,dbdn,dbdv1,dbdv2,dbdv3
      double precision djdc1,djdc2,djdfdc,dpdpsi,dndpsi 
      double precision Ndi, Nai, dNdi, dNai, Vargn, Vargp,dVargn,dVargp
      double precision gi(3),ei(3),jside(3),heigh(3),tmp1,tmp2,tmp3
      double precision area(3),dev(3,3)
      double precision gsc(3,3),gsp(3,3),dgic(3,3),dgip(3,3)
      double precision xcentr,ycentr,volume
      double precision xi,yi,xj,yj,xk,yk
      real esnod(3),ccoef,esi1
c 
c****************************************************************** 
c 
c                   data
c 
      data ni/1,2,3,1,2/
c 
c*************
c**  START  **
c*************
c 
c...For plug-in, is this Poisson or continuity?
      if(lplug.and.(npde.gt.1)) goto 500
c
c------------------------------------------------------------------
c 
c                          Poisson's eq.
c
      idxpde=1
      do 5 i=1,5
5     nidisp(i)=(ni(i)-1)*imult+idxpde
c
c-------
c  LHS 
c-------
c
c...Scan sides - i1,i2 are nodes of side i (ie. side opp. node i)
      do 300 i=1,3
      i1=nidisp(i+1)
      i2=nidisp(i+2)
      hd=ehed(i,ie)
      estifm(i1,i1)=estifm(i1,i1)+hd
      estifm(i1,i2)=estifm(i1,i2)-hd
      estifm(i2,i2)=estifm(i2,i2)+hd
      estifm(i2,i1)=estifm(i2,i1)-hd
300   continue
c
c-------
c  RHS
c-------
c
      do 330 i=1,3
      i1=nidisp(i)
      inode=idxnod(i)
c
c...Electric flux
      do 320 j=1,3
      i2=nidisp(j)
      jnode=idxnod(j) 
      wrhs(i1)=wrhs(i1)-estifm(i1,i2)*fv(jnode)
320   continue
c
c...Put space charge into RHS 
c...If insulator, only fixed charge (multiply by length/area)
      esi1=esnod(i)
      if(elmat.le.0) then
         if(essem(inode) .eq. estot(inode)) 
     +          write(6,*) ' Huh? ',inode, estot(inode), essem(inode),
     +                     cord(1,inode), cord(2,inode)
         wrhs(i1)=wrhs(i1)+esi1*qss(inode)*
     +            dintf(inode)/(estot(inode)-essem(inode))
         goto 330 
      endif
c
c...  get derrivatives and quasi fermi levels. We can't escape!
      if (lboltz) then
c...    Boltzmann statistics
        Vargn = dlog(dabs(fn(inode)/dble(ncband)) +mindbl)
        Vargp = dlog(dabs(fp(inode)/dble(nvband)) +mindbl)
        dVargn = 1.0d0/(fn(inode)+mindbl)
        dVargp = 1.0d0/(fp(inode)+mindbl)
      else
c...    Fermi-Dirac statistic
        call diraci(dble(fn(inode)/ncband), Vargn, dVargn)
        call diraci(dble(fp(inode)/nvband), Vargp, dVargp)
        dVargn = dVargn/dble(ncband)
        dVargp = dVargp/dble(nvband)
      endif
      dpdpsi = -1.0d0/dVargp
      dndpsi =  1.0d0/dVargn
c
      if (lincom) then
        call fermis(Vargn, dble(gcstar), Ndi, dNdi)
        call fermis(Vargp, dble(gvstar), Nai, dNai)
        Ndi  =  dble(ndconc(inode))*Ndi
        dNdi =  dble(ndconc(inode))*dNdi
        Nai  =  dble(naconc(inode))*Nai
        dNai =  dble(naconc(inode))*dNai
      else
c..     for no ionized donors/aceptors
        Ndi  =  dble(r1(inode))
        dNdi =  0.0d0
        Nai  =  0.0d0
        dNai =  0.0d0
      endif
c
      dnc1=fn(inode)
      dpc1=fp(inode)
      wrhs(i1)=wrhs(i1)+esi1*((dpc1-Nai)-(dnc1-Ndi))
c
c---------------
c  ADJUSTMENTS
c---------------
c
c...Gummel method -
c...must add term to diagonal for psi dependence of n/p
c...(only valid for Boltzmann statistics)
c...("factor" accounts for acceleration)
      if(.not.lplug) goto 100
      tmp=esi1
      if(laccel) tmp=tmp*factor
      dnc2=  tmp*(1.0d0-dNdi*dVargn)
      dpc2= -tmp*(1.0d0-dNai*dVargp)
      estifm(i1,i1)=estifm(i1,i1)+dnc2*dndpsi+dpc2*dpdpsi
      goto 330
      
c
c...Newton 
100   dnc2=  esi1*(1.0d0-dNdi*dVargn)
      dpc2= -esi1*(1.0d0-dNai*dVargp)
c
c...Newton (one-carrier) - have diagonal term for psi 
c...dependence of min. carrier (if psi,n,p are used) and have 
c...jacobian term for maj. carrier 
      if(ncarr.eq.2) goto 110
      if(l1hole) then
         estifm(i1,i1+1)=estifm(i1,i1+1)+dpc2
         estifm(i1,i1)=estifm(i1,i1)+dnc2*dndpsi
      else
         estifm(i1,i1+1)=estifm(i1,i1+1)+dnc2
         estifm(i1,i1)=estifm(i1,i1)+dpc2*dpdpsi
      endif
      goto 330
c
c...Newton (two-carriers - the whole ball of wax) -
c...two additional off-diagonal terms for n and p
110   estifm(i1,i1+1)=estifm(i1,i1+1)+dnc2
      estifm(i1,i1+2)=estifm(i1,i1+2)+dpc2
c 
330   continue
      if(lplug) return
c 
c-------------------------------------------------------------
c 
c                   Continuity eq.
c 
500   if(lplug) then
         idxpde=1
         lpcont=npde.eq.3
      else
         idxpde=idxpde+1
         lpcont=l1hole.or.(idxpde.eq.3)
      endif
      do 505 i=1,5
505   nidisp(i)=(ni(i)-1)*imult+idxpde
c
c...Insulator? - dont bother with carriers
      if(elmat.gt.0) goto 501
      do 335 i=1,3
      i1=nidisp(i)
      if(mattyp(itype(idxnod(i))).lt.0) estifm(i1,i1)=1.d0 
335   continue
      goto 800
c
c...Semiconductor
c
c...Should mobility derivatives be used? - only if not plug-in
c...and field-dependent mobility 
501   ldmu=(.not.lplug).and.lfldmb.and.iterp.gt.0
c
c...Get the center of circumscribed circle for triangle(element)
c   xi,yi,... should be substituted because of type of valuables for centr
      if(limpct.and.lcyl) then
        xi=cord(1,idxnod(1))
        yi=cord(2,idxnod(1))
        xj=cord(1,idxnod(2))
        yj=cord(2,idxnod(2))
        xk=cord(1,idxnod(3))
        yk=cord(2,idxnod(3))
        call centr(xi,yi,xj,yj,xk,yk,xcentr,ycentr)
      endif
c
c...Scan sides (i1,i2 are nodes of side i - side opposite node i)
      do 400 i=1,3
c
c...Get coupling-coefficient for side - if 0 (right angle or
c...obtuse angle opposite side)
      ccoef=jhjd(i,ie)
      if((.not.limpct).and.(ccoef.eq.0.)) goto 400
c
c...Get useful indicies
      i1=i+1
      i2=i+2
      ni1=ni(i1)
      ni2=ni(i2)
      i1node=idxnod(ni1) 
      i2node=idxnod(ni2) 
      i1=nidisp(i1)
      i2=nidisp(i2)
c
c...Assemble current for side i (going from node i1node to node i2node)
c...find out whether side is at insulator interface
      lsurf=.false.
      if(nextel(i,ie).gt.0) lsurf=mattyp(imat(nextel(i,ie))).lt.0
      if(ccoef.ne.0) then
      call assmbj(lpcont,elmat,lsurf,ccoef,ie,ni(i+1),ni(i+2),
     +            djgs,gs1,gs2,lplug,dgs1,dgs2,e21,dedv,ldmu,nmobct)
      endif
c
c...Store current and height(hi,hj,hk) for impact ionization
      if (limpct) then
        ei(i)=e21
        tmp1=cord(1,i2node)-cord(1,i1node)
        tmp2=cord(2,i2node)-cord(2,i1node)
        tmp3=dsqrt(tmp1*tmp1+tmp2*tmp2)
        if(ccoef.eq.0.d0) then
         heigh(i)=0.d0
        else
         heigh(i)=ccoef*tmp3
         if(lcyl) then
           xi=cord(1,i1node)
           yi=cord(2,i1node)
           xj=cord(1,i2node)
           yj=cord(2,i2node)
           area(i)=volume(xi,yi,xj,yj,xcentr,ycentr)
         else
           area(i)=0.5d0*heigh(i)*tmp3
         endif
         if (lwidth) then
           area(i)=area(i)*width
         endif
         tmp3=heigh(i)
         if(lpcont) tmp3=-tmp3
         jside(i)=djgs/tmp3
         gsc(i,ni1)=gs1(1)/tmp3
         gsc(i,ni2)=gs2(1)/tmp3
         gsc(i,i)=0.0d0
         gsp(i,ni1)=dgs1(1)/tmp3
         gsp(i,ni2)=dgs2(1)/tmp3        
         gsp(i,i)=0.0d0
         dev(i,ni1)=-dedv
         dev(i,ni2)=dedv
         dev(i,i)=0.0d0
c
        endif
      endif
c
c...Sum into window array 
      if(ccoef.eq.0) goto 400
      estifm(i1,i1)=estifm(i1,i1)+gs1(1)
      estifm(i1,i2)=estifm(i1,i2)+gs2(1)
      estifm(i2,i1)=estifm(i2,i1)+gs1(2)
      estifm(i2,i2)=estifm(i2,i2)+gs2(2)
c
c...ADDITIONAL derivatives due to cc-scattering
c...dfn/dn, dfp/dp in ASSMBJ
      if(labbas.and.(.not.lplug).and.ncarr.gt.1) then
         djdfdc=djgs*dfdc
         if(lpcont) then
c...dfp/dn
            djdc1=djdfdc/fn(i1node)
            djdc2=djdfdc/fn(i2node)
            i1p=i1-1
            i2p=i2-1
         else
c...dfn/dp
            djdc1=djdfdc/fp(i1node)
            djdc2=djdfdc/fp(i2node)
            i1p=i1+1
            i2p=i2+1
         endif
         estifm(i1,i1p)=estifm(i1,i1p)+djdc1
         estifm(i1,i2p)=estifm(i1,i2p)+djdc2
         estifm(i2,i1p)=estifm(i2,i1p)-djdc1
         estifm(i2,i2p)=estifm(i2,i2p)-djdc2
      endif
c
c
c...Store psi Jacobi terms
      if(lplug) goto 440
      i1p=i1-idxpde+1
      i2p=i2-idxpde+1
      estifm(i1,i1p)=estifm(i1,i1p)+dgs1(1)
      estifm(i1,i2p)=estifm(i1,i2p)+dgs2(1)
      estifm(i2,i1p)=estifm(i2,i1p)+dgs1(2)
      estifm(i2,i2p)=estifm(i2,i2p)+dgs2(2)
c
c...RHS
440   wrhs(i1)=wrhs(i1)-djgs
      wrhs(i2)=wrhs(i2)+djgs
c
c...Next side 
400   continue
c
c
c------------------------------------
c  Generation (Impact ionization)
c------------------------------------
c...Impact ionization?  now for Si only
         if(limpct.and.(elmat.eq.1)) then
            if(lmont) then
            call impac2(ie,lpcont,ei,jside,heigh,area,
     +               gi,gsc,gsp,dgic,dgip,dev,.false.)
            else if(lcsm) then
            call impac1(lpcont,ei,jside,heigh,area,
     +               gi,gsc,gsp,dgic,dgip,dev,.false.)
            else
            call impace(lpcont,ei,jside,heigh,area,
     +               gi,gsc,gsp,dgic,dgip,dev,.false.)
            endif
c
c...Add to rhs
      do 402 i=1,3
         i1=nidisp(i)
         wrhs(i1)=wrhs(i1)+gi(i)
         if ((.not.lplug).and.(ncarr.eq.2)) then
            if (lpcont) then
              wrhs(i1-1)=wrhs(i1-1)+gi(i)
            else
              wrhs(i1+1)=wrhs(i1+1)+gi(i)
            endif
         endif
402    continue
c
c...Add to estifm
       do 403 i=1,3
        i1=nidisp(i)
        do 404 i2=1,3
         if(ncarr.eq.1) then
         i3=i2*2-1
         estifm(i1,i3+1)=estifm(i1,i3+1)+dgic(i,i2)
         if(.not.lplug) estifm(i1,i3)=estifm(i1,i3)+dgip(i,i2)
         else
         i3=i2*3-2
          if(lpcont) then
           estifm(i1,i3+2)=estifm(i1,i3+2)+dgic(i,i2)
           if(.not.lplug) then
           estifm(i1-1,i3+2)=estifm(i1-1,i3+2)+dgic(i,i2)
           estifm(i1,i3)=estifm(i1,i3)+dgip(i,i2)
           estifm(i1-1,i3)=estifm(i1-1,i3)+dgip(i,i2)
           endif
          else
           estifm(i1,i3+1)=estifm(i1,i3+1)+dgic(i,i2)
           if(.not.lplug) then
           estifm(i1+1,i3+1)=estifm(i1+1,i3+1)+dgic(i,i2)
           estifm(i1,i3)=estifm(i1,i3)+dgip(i,i2)
           estifm(i1+1,i3)=estifm(i1+1,i3)+dgip(i,i2)
           endif
          endif
         endif
404     continue
403    continue
c
         endif
c
c
c------------------------------------
c  Recombination (SRH and/or Auger)
c------------------------------------
c
      do 600 i=1,3
         i1=nidisp(i)
         inode=idxnod(i)
         i1p=i1-idxpde+1
         esi1=esnod(i)
c...If not SRH or Auger, skip
c...If not SRH or Auger or Photogen
         if((.not.lsrh).and.(.not.lauger).and.(.not.lphgen)) goto 605
c
c...Get recombination and derivatives - do only 1st time through
         if(idxpde.lt.3) then
            call recomb(inode,fn(inode),fp(inode),ui(i),
     +               dudv(i),dudn(i),dudp(i))
            ui(i)=ui(i)*esi1
            dudn(i)=-dudn(i)*esi1
            dudp(i)=-dudp(i)*esi1
            dudv(i)=-dudv(i)*esi1
         endif
c
c...Add to rhs and enter Jacobian terms
         wrhs(i1)=wrhs(i1)+ui(i)
         if(lpcont) then
            estifm(i1,i1)=estifm(i1,i1)+dudp(i)
            if(.not.lplug) then
               if(ncarr.gt.1) estifm(i1,i1-1)=estifm(i1,i1-1)+dudn(i)
            endif
         else
            estifm(i1,i1)=estifm(i1,i1)+dudn(i)
            if(.not.lplug) then
               if(ncarr.gt.1) estifm(i1,i1+1)=estifm(i1,i1+1)+dudp(i)
            endif
         endif
c
c...Schottky SRV?
605      continue
         iel=lm(inode)
         if(iel.gt.0) then
            if(schotk(iel)) then
               i2=ni(i+1)
               i3=ni(i+2)
               call srvbc(inode,iel,elmat,esi1,idxnod(i2),idxnod(i3),
     +            lplug,lpcont,djgs,dbdn,dbdv1,dbdv2,dbdv3)
               wrhs(i1)=wrhs(i1)+djgs
               estifm(i1,i1)=estifm(i1,i1)+dbdn
               if(.not.lplug) then
                  i2p=nidisp(i2)-idxpde+1
                  i3p=nidisp(i3)-idxpde+1
                  estifm(i1,i1p)=estifm(i1,i1p)+dbdv1
                  estifm(i1,i2p)=estifm(i1,i2p)+dbdv2
                  estifm(i1,i3p)=estifm(i1,i3p)+dbdv3
               endif
c
c...Save currents because rhs isn't net terminal current
c...for a Schottky w/ SRV
               if(lpcont) then
                  namp(iel,2)=namp(iel,2)+djgs
               else
                  namp(iel,1)=namp(iel,1)-djgs
               endif
            endif
         endif
c
c...Next node
600   continue
c
c...Next carrier?
800   if(idxpde.lt.imult) goto 500
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ASSMBJ(lpcont,elmat,lsurf,ccoef,ie,i1,i2,djgs,
     +                  gs1,gs2,lplug,dgs1,dgs2,e21,dedv,ldmu,nmobct)
      include 'p2conf.h'
c
c     Assemble current along a side of a triangle.
c
c        lpcont....true if p cont eqn. 
c        elmat.....element material
c        lsurf.....flag to indicate side is at insulator interface
c        ccoef.....coupling-coefficient for side
c        i1node....source node
c        i2node....sink node
c        djgs......current
c        gs1.......derivative of current w.r.t carrier at i1node
c        gs2.......derivative of current w.r.t carrier at i2node
c        lplug.....flag to indicate plug-in iteration (no derivs. 
c                  w.r.t. psi are needed in this case)
c        dgs1......derivative of current w.r.t psi at i1node
c        dgs2......derivative of current w.r.t psi at i2node
c        e21.......electric field (magnitude) between i1node and i2node
c        dedv......derivative of e21 w.r.t. psi at i2node
c        ldmu......flag to indicate need to calculate derivative of
c                  mobility w.r.t. psi
c        nmobct....for debugging purposes
c
c     Original:    Mark R. Pinto                       June,  1984
c     Revision:    MRP           (improved accuracy)   March, 1985
c     Revision:    HRY  (Link into stats.f & cleaned)  July,  1985
c                       (up current switching for Bernoulli fntn.)
c     Revision:    C.C.Abbas                           Sep,   1987
c                       (Carrier-carrier scattering)
c     Revision:    A. Yabuta                           Oct,   1987
c                       (only added flag for impact ionization)
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
      include     'impact.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical lpcont,lplug,ldmu,lsurf,lswtch
      integer ie,i1,i2
      integer i1node,i2node,elmat,i,nmobct
      double precision p1v,p2v,p1c,p2c,p21,e21,dedv,dfdv2
      double precision g1,g2,dg1,dg2,gs1(2),gs2(2),dgs1(2),dgs2(2)
      double precision tmp,tmp2,uhd,dnc1,dnc2,dpc1,dpc2,delc,djgs
      double precision djdc1, djdc2, dp2dn2, dp1dn1,Lngs, dLngs
      real ccoef
     

c*************
c**  START  **
c*************
c
       i1node=nop(i1,ie)
       i2node=nop(i2,ie)
c...Gummel-Scharfetter discretization
c...Get pot. diff. (including bgn, if requested)
c...Always evaluate Bernoulli of a positive argument! So what we do is to
c...change the current reference convention if the argument to the bernoulli
c...function is negative; then evaluate the current and derivatives; and then
c...change back to the original reference convention by negating the current
c...and derivatives.
c...
C... Boltzmann Statistics 
      p2c    = 0.0d0
      dp2dn2 = 0.0d0
      p2v    = fv(i2node)
      p1c    = 0.0d0
      dp1dn1 = 0.0d0
      p1v    = fv(i1node)
      if (lboltz) goto 10
c
C... Fermi Statistics 
      if(lpcont) then
c..     Generalized expression for holes
        tmp=fp(i2node)/dble(nvband)
        call lngamma(tmp,Lngs,dLngs)
        p2c=-Lngs
        dp2dn2=-dLngs/dble(nvband)
        p2v=fv(i2node)
c
        tmp=fp(i1node)/dble(nvband)
        call lngamma(tmp,Lngs,dLngs)
        p1c=-Lngs
        dp1dn1=-dLngs/dble(nvband)
        p1v=fv(i1node)
        p21=p2c-p1c
      else
c..     Generalized expression for electrons
        tmp=fn(i2node)/dble(ncband)
        call lngamma(tmp,Lngs,dLngs)
        p2c=Lngs
        dp2dn2=dLngs/dble(ncband)
        p2v=fv(i2node)
c       
        tmp=fn(i1node)/dble(ncband)
        call lngamma(tmp,Lngs,dLngs)
        p1c=Lngs
        dp1dn1=dLngs/dble(ncband)
        p1v=fv(i1node)
        p21=p2c-p1c
      endif
c..
   10 p21=p2c-p1c
      tmp=p2v-p1v
      p21=p21+tmp
c..
      if(lbgn) then
        if(lpcont) then
           tmp=-(lgcnie(i2node)-lgcnie(i1node))
        else
           tmp=(lgcnie(i2node)-lgcnie(i1node))
        endif
        p21=p21+tmp
      endif
c..
c..   Here is where we do the switch trick.
      lswtch = p21.lt.0.0d0
      if(lswtch) then
         i=i2node
         i2node=i1node
         i1node=i
         p21=-p21
         tmp=dp2dn2
         dp2dn2=dp1dn1
         dp1dn1=tmp
      endif
c..
      if(lfldmb.or.limpct) then
         tmp=cord(1,i2node)-cord(1,i1node)
         tmp2=cord(2,i2node)-cord(2,i1node)
         dedv=dktq/dsqrt(tmp*tmp+tmp2*tmp2)
         tmp=fv(i2node)-fv(i1node)
         e21=dabs(tmp)*dedv
         if(tmp.lt.0.d0) dedv=-dedv
Chry     if(lswtch) dedv=-dedv
      endif
c
c...Mobility
      call dmobl(ie,i1,i2,e21,dedv,uhd,lpcont,elmat,
     +           ldmu,dfdv2,lsurf,nmobct)
      uhd=uhd*ccoef
      if(lpcont) uhd=-uhd
c
c...Get Bernoulli fn. terms 
      continue
      call bernf(p21,g2,dg2,lplug)
      g2=g2*uhd
      p21=p21*uhd
c
c...Psi,n,p
c...(Remember to reverse p coefficients)
      if(lpcont) then
         g1=g2
         g2=-(g2+p21)
         dpc1=fp(i1node)
         dpc2=fp(i2node)
         delc=dpc1-dpc2
         djgs=g1*delc-p21*dpc2
      else
         g1=-(g2+p21)
         dnc1=fn(i1node)
         dnc2=fn(i2node)
         delc=dnc2-dnc1
         djgs=g2*delc-p21*dnc1
      endif
c
c...Derivatives with respect to carrier concentrations 
c...(new for fermi-dirac)
      if(lpcont) then
        djdc1=g1+uhd*(delc*dg2-dpc2)*(-dp1dn1)
        if(labbas) djdc1=djdc1+djgs*dfdc/dpc1
        djdc2=g2+uhd*(delc*dg2-dpc2)*(dp2dn2)
        if(labbas) djdc2=djdc2+djgs*dfdc/dpc2
      else
        djdc1=g1+uhd*(delc*dg2-dnc1)*(-dp1dn1)
        if(labbas) djdc1=djdc1+djgs*dfdc/dnc1
        djdc2=g2+uhd*(delc*dg2-dnc1)*dp2dn2
        if(labbas) djdc2=djdc2+djgs*dfdc/dnc2
      endif
c
c...Derivatives with respect to psi
      if(lplug) goto 998
      if(lpcont) then
         dg2=(delc*dg2-dpc2)*uhd
         if(ldmu) dg2=dg2+djgs*dfdv2
         dg1=-dg2
      else
         dg2=(delc*dg2-dnc1)*uhd
         if(ldmu) dg2=dg2+djgs*dfdv2
         dg1=-dg2
      endif
c
c...Adjust for qfs
c
998   if(lswtch) then
         i=i2node
         i2node=i1node
         i1node=i
         djgs=-djgs
         dedv=-dedv
         gs1(1)=-djdc2
         gs1(2)=djdc2
         gs2(1)=-djdc1
         gs2(2)=djdc1
         dgs1(1)=-dg2
         dgs1(2)=dg2
         dgs2(1)=-dg1
         dgs2(2)=dg1
      else
         gs1(1)=djdc1
         gs1(2)=-djdc1
         gs2(1)=djdc2
         gs2(2)=-djdc2
         dgs1(1)=dg1
         dgs1(2)=-dg1
         dgs2(1)=dg2
         dgs2(2)=-dg2
      endif
c
c...Bye
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE DMOBL(nel,n1node,n2node,e21,dedv,mu21,lmup,elmat,ldmu,
     +                 dfdv,lsurf,nmobct)
      include 'p2conf.h'
c 
c     This routine computes the mobility between two nodes
c     considering any combination of:
c
c              carrier-carrier scattering dependent mobility
c              constant mobility 
c              field dependent mobility
c              (ionized) impurity concentration dependent mobility
c              lattice scattering dependent mobility
c
c     dfdn....is the derivative of mobility with respect to the electron 
c             concentration divided by the value of the mobility
c     dfdp....is the derivative of mobility with respect to the hole
c             concentration divided by the value of the mobility
c     dfdc....general variable for derivative of mobility with respect to
c             carrier concentration.  One or the other of the above values
c             is returned in this variable.
c     dfdv....is the derivative of mobility with respect to delta psi 
c             divided by the value of mobility
c
c     MRP         Stanford University              May, 1984
c     C.C. Abbas  (carrier-carrier scattering)     Sep, 1987
c     G.D. Anderson (Modularize calls,             Nov, 1989
c                    Add UT surface mobility model) 
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'setup.h'
      include     'impact.h'
      include     'trfldmob.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lmup,ldmu,lsurf
      integer elmat,n2,n1,nmobct
      real nmu,nmui
      double precision e21,dfdv,mu21,dedv
      double precision t1,t2,t3,t4,dvs
      integer nel,n1node,n2node
c 
c****************************************************************** 
c 
c                   start 
c
      n1=nop(n1node,nel)
      n2=nop(n2node,nel)
c...Try constant mobility first 
c...              fmup0=low field mobility.
c...              muco1 & muco3 are the mobility coefficients for electrons.
c...              muco2 & muco4 are the mobility coefficients for holes.
c... 
      dfdc=0.d0
      dfdv=0.d0
      if(lmup) then 
         mu21=fmup0
         nmu=muco2
         nmui=muco4
      else
         mu21=fmun0
         nmu=muco1
         nmui=muco3
      endif
c 
c-------------------------------------------------------------
c 
c                   concentration dep. mobility 
c        
      if(lconmb) then 
         if(lmup) then
            mu21=(mobp(n1)+mobp(n2))*0.5
         else
            mu21=(mobn(n1)+mobn(n2))*0.5
         endif
      endif
c
c-------------------------------------------------------------
c...jtw5: surface mobility calculation 5/9/87
c
      if(lsrfmb.and.lsurf) then
          call smobl(nel,n1node,n2node,mu21,lmup,ldmu)
      endif
c 
c-----------------------------------------------------------------------
c 
c                   carrier-carrier scattering dep. mobility 
c
      if(labbas) then 
         call ccsmob(n1, n2, lmup, mu21)
      endif
c        
c-----------------------------------------------------------------------
c 
c                   UT Transverse E-Field electron mobility model
c
      if(ltfldmb) then 
         call trfldmob(n1,n2,e21,dedv,lmup,ldmu,mu21,dfdv,nmu,nmui,
     +                 elmat)
c        
c-------------------------------------------------------------
c
c                Surface degredation factor
c
      else if(lsurf) then
         mu21=mu21*mudeg
      endif
c 
c-------------------------------------------------------------
c 
c                Field dependent mobility
c                (Already calculated if transverse e-field model is used)
c
      if(lfldmb.and.e21.gt.1.d0.and.(.not.ltfldmb)) then
         dvs=dble(vsat)
c
c...Si
c...remember, dfdv is the derivative of mu w/ respect to psi2
c...DIVIDED BY mu21 
c...use different code for special cases nmu=1,2 for efficiency
         if(elmat.eq.1) then
            if(nmu.eq.1.) then
               t1=dvs/mu21
               if(ldmu) then
                  t2=1.d0/(t1+e21)
                  mu21=dvs*t2
                  dfdv=-t2*dedv
               else
                  mu21=dvs/(t1+e21)
                  dfdv=0.d0
               endif
            else if(nmu.eq.2.) then
               t1=mu21/dvs
               t3=t1*t1*e21
               t4=t3*e21
               if(ldmu) then
                  t2=1.d0/(1.d0+t4)
                  mu21=mu21*dsqrt(t2)
                  dfdv=-t3*t2*dedv
               else
                  mu21=mu21*dsqrt(1.d0/(1.d0+t4))
                  dfdv=0.d0
               endif
            else
               t1=mu21/dvs
               t4=(t1*e21)**nmu
               if(ldmu) then
                  t2=1.d0/(1.d0+t4)
                  mu21=mu21*(t2**nmui)
                  dfdv=-(t4/e21)*t2*dedv
               else
                  mu21=mu21*(1.d0/(1.d0+t4))**nmui
                  dfdv=0.d0
               endif
            endif
c
c...GaAs (see Barnes, et. al. ED Sept. 1976)
c...remember, dfdv is the derivative of mu w/ respect to psi2
c...DIVIDED BY mu21
c
         else if(elmat.eq.2) then 

            t1=e21*dble(muco1)
            t2=t1*t1
            t3=t2*t1
            if(ldmu) then
               t4=1.d0/(1.d0+t2*t2)
               mu21=(mu21+dble(muco2)*t3)*t4
               dfdv=(dble(muco3)*t2/mu21-dble(muco4)*t3)*t4
               if(dfdv.lt.0.d0) then
                  nmobct=nmobct+1
                  dfdv=0.d0
               endif
               dfdv=dfdv*dedv

            else
               mu21=(mu21+dble(muco2)*t3)/(1.d0+t2*t2)
               dfdv=0.d0
            endif
c
c...Other (just use velocity saturation limiting)
         else
            if(mu21*e21.gt.dvs) mu21=dvs/e21
            dfdv=0.
         endif
      endif
c
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE RECOMB(inode,dnc1,dpc1,ui,dudv,dudn,dudp)
      include 'p2conf.h'
c
c     The following routine calculates the net recombination (ui) at
c     node inode as well as the derivatives with respect to psi, n
c     and p (dudv,dudn,dudp). 
c
c     Original:  Mark R. Pinto   Stanford University    Sept. 1983
c     Revision:  HRY(Rescaled by dcscl for low temp)  July. 1983
c     Revision:  HRY(Re-def of etrapn for low temp)     July. 1983
c     Revision:  G. Anderson (photogeneration)          June, 1989
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'sol.h'
      include     'setup.h'
      include     'photo.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      double precision ui,dudv,dudn,dudp,numui,denui,dnc1,dpc1
      double precision dnc2,dpc2,cncp,cni,tni,tpi,dnc1s,dpc1s
      double precision trapn,trapp,cmax,ssn,ssp,sfact,dexpu
      integer inode
      data cmax/1.d15/
c
c********************
c**  START RECOMB  **
c********************
c
c...Initialize
      ui=0.d0
      dudn=0.d0
      dudp=0.d0
      dudv=0.d0
c
      cni=dble(cnie(inode))
      trapn=dexpu(etrapn+lgcnie(inode))
      trapp=dexpu(etrapp+lgcnie(inode))
c
c...Get lifetime - either conc. dep or constant
      if(lconlt) then
         tni=taun(inode)
         tpi=taup(inode)
      else
         tni=taun0
         tpi=taup0
      endif
c
c...Interface recombination?
      ssn=snintf(inode)
      ssp=spintf(inode)
      if((ssn.gt.0.).or.(ssp.gt.0.)) then
         sfact=dqkt*dintf(inode)/essem(inode)
         tni=1.d0/(1.d0/tni+ssn*sfact)
         tpi=1.d0/(1.d0/tpi+ssp*sfact)
      endif
c
c...Do we need to scale (to prevent overflow)?
      if((dnc1.gt.cmax).or.(dpc1.gt.cmax)) goto 200
c
c--------------
c  Non-scaled
c--------------
c
      numui=dnc1*dpc1-cni*cni
c
c...SRH recombination
      if(.not.lsrh) goto 150
      denui=1.d0/(tni*(dpc1+trapp)+tpi*(dnc1+trapn))
      ui=numui*denui
      dnc2=-ui*tpi
      dpc2=-ui*tni
      if(l1hole) goto 145
      dudn=(dpc1+dnc2)*denui
      if(ncarr.lt.2) goto 150
145   dudp=(dnc1+dpc2)*denui
c
c...Auger recombination
150   if((.not.lauger).or.(numui.lt.0.d0)) goto 999
      dnc2=cnau*dnc1
      dpc2=cpau*dpc1
      cncp=dnc2+dpc2
      ui=ui+cncp*numui
      if(l1hole) goto 155
      dudn=dudn+dpc1*cncp+cnau*numui
      if(ncarr.lt.2) goto 156
155   dudp=dudp+dnc1*cncp+cpau*numui
156   goto 999
c
c----------
c  Scaled 
c----------
c
200   dnc1s=dnc1*dcscli
      dpc1s=dpc1*dcscli
      numui=dnc1s*dpc1s-(cni*dcscli)*(cni*dcscli)
c
c...SRH recombination
      if(.not.lsrh) goto 250
      denui=1.d0/(tni*(dpc1+trapp)+tpi*(dnc1+trapn))
      ui=(dmax1(-rcmax,dmin1(rcmax,numui*denui))*dcscl)*dcscl
      if(l1hole) goto 245
      dnc2=-ui*tpi
      dudn=(dpc1+dnc2)*denui
      if(ncarr.lt.2) goto 250
245   dpc2=-ui*tni
      dudp=(dnc1+dpc2)*denui
c
c...Auger recombination
250   if((.not.lauger).or.(numui.lt.0.d0)) goto 999
      dnc2=cnau*dnc1s
      dpc2=cpau*dpc1s
      cncp=dnc2+dpc2
      ui=ui+(dmax1(-rcmax2,dmin1(rcmax2,cncp*numui))*dcscl)*
     +                                               dcscl*dcscl
      if(l1hole) goto 255
      dudn=dudn+dmax1(-rcmax,dmin1(rcmax,dpc1s*cncp+cnau*numui))*
     +                                               dcscl*dcscl
      if(ncarr.lt.2) goto 999
255   dudp=dudp+dmax1(-rcmax,dmin1(rcmax,dnc1s*cncp+cpau*numui))*
     +                                               dcscl*dcscl
c
c...Photogeneration
999   CONTINUE
      if (lphgen.and.(cord(2,inode).ge.0.0)) then
        ui = ui - dble(flux*abscof*exp(-abscof*cord(2,inode)))
      endif
c...Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE BERNF(dpsi,gs,dgs,lplug)
      include 'p2conf.h'
c
c-----------------------------------------------------------------------
c
c     BERNF caculates the bernoulli fn. of the argument dpsi
c     and returns the result as gs.   The derivative with respect
c     to dpsi is also returned (dgs).
c
c     Original:  Mark R. Pinto   Stanford University    Sept. 1983
c
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c
      include     'blank.h'
      include     'setup.h'
c
      double precision dpsi,gs,dgs,tmp,tmp2,d12i,dexp
      logical lplug
      data d12i/8.33333333333333333d-2/
c
c*******************
c**  START BERNF  **
c*******************
c
c...If dpsi is very small, use series expansion (take to 2nd order
c...terms - there are no third order terms, so the series is 4th order
c...accurate)
      tmp=dabs(dpsi)
      if(tmp.gt.1.d-4) go to 10
      tmp=dpsi*d12i
      gs=1.d0+dpsi*(-.5d0+tmp)
      dgs=-.5d0+tmp+tmp
      return
c
c...Use exponential if no overflow
c...Dont waste time evaluating derivative term if not needed
10    if(tmp.gt.maxexp) goto 20
      tmp=dexp(dpsi)
      tmp2=1.d0/(tmp-1.d0)
      gs=dpsi*tmp2
      if(.not.lplug) dgs=tmp2-gs*(1.d0+tmp2)
      return
c
c...Possible overflow - use hyperbolic tangent formulation
20    tmp=.5d0/dtanh(0.5d0*dpsi)-.5d0
      gs=dpsi*tmp
      if(.not.lplug) dgs=(1.d0-dpsi-gs)*tmp
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SRVBC(inode,iel,elmat,esi1,idx2,idx3,lplug,lpcont,
     +                 djgs,dbdn,dbdv1,dbdv2,dbdv3)
      include 'p2conf.h'
c
c     Assemble SRV boundary condition.
c
c        inode....node index of vertex #1
c        iel......corresponding electrode index
c        elmat....element material type
c        esi1.....element area for vertex #1
c        idx2.....vertex #2 of element
c        idx3.....vertex #3 of element
c        lplug....flag for plug-in iteration
c        lpcont...flag for p-continuity
c
c        djgs.....recombination contribution 
c                 (current weighted by area)
c        dbdn.....derivative wrt conc at vertex #1
c        dbdv1....derivative wrt potl at vertex #1
c        dbdv2....derivative wrt potl at vertex #2
c        dbdv3....derivative wrt potl at vertex #3
c
c     Original : MRP   Stanford University   June, 1985
c     Revision : HRY   (links into stats.f)  July, 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical lplug,lpcont
      integer inode,iel,idx2,idx3,ib,elmat
      real esi1
      double precision dbdn,dbdv1,dbdv2,dbdv3,djgs
      double precision efld,tmp,dvc1,dvc2,efn,delvb,dbde
      double precision efdtri
      double precision POCCUP, NOCCUP
c
c*************
c**  START  **
c*************
c
c...Find boundary index for inode
      do 608 ib=1,nb
        if(nbc(ib).eq.inode) goto 609
608   continue
609   continue
c
c...Adjust barrier using electric field for element
      if(lbarl(iel)) then
         efld=efdtri(inode,idx2,idx3,lplug,dbdv1,dbdv2,dbdv3)
         tmp=dktq*depsc/epsmat(elmat)
         efn=dsqrt(tmp*efld)
         dvc1=barla(iel)
         dvc2=dqkt*barlb(iel)
         delvb=dvc1*efld+dvc2*efn
         if(.not.lplug.and.(efn.ne.0.d0)) then
            dbde=dvc1+dvc2*0.5d0*tmp/efn
            dbdv1=dbdv1*dbde
            dbdv2=dbdv2*dbde
            dbdv3=dbdv3*dbde
         endif
cc         write(*,*) 'inode,efld,delvb = ',inode,
cc     +               efld*dktq,delvb*dktq
      else
         delvb=0.d0
         dbdv1=0.d0
         dbdv2=0.d0
         dbdv3=0.d0
      endif
c
c...Calculate equivalent recombination (weight by area)
      if(lpcont) then
         dbdn=-dexp(delvb)*lmetal(ib)*vsp(iel)*esi1/essem(inode)
         djgs=-dbdn*(fp(inode)-POCCUP(lboltz,bias(iel),fv(inode),
     +                                     lgcnie(inode),nvband))
      else
         dbdn=-dexp(delvb)*lmetal(ib)*vsn(iel)*esi1/essem(inode)
         djgs=-dbdn*(fn(inode)-NOCCUP(lboltz,bias(iel),fv(inode),
     +                                     lgcnie(inode),ncband))
      endif
c
c...If barrier lowering, must supply derivatives wrt potl
      if(.not.lplug.and.(lbarl(iel))) then
         dbdv1=-djgs*dbdv1
         dbdv2=-djgs*dbdv2
         dbdv3=-djgs*dbdv3
      endif
c
c...Bye 
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION EFDTRI(i1,i2,i3,lplug,dedv1,dedv2,dedv3)
      include 'p2conf.h'
c
c     Calculate electric field for a triangle.
c
c        i1.......vertex #1 of element
c        i2.......vertex #2 of element
c        i3.......vertex #3 of element
c        lplug....flag for plug-in (no derivs)
c
c        dedv1....derivative wrt potl at vertex #1
c        dedv2....derivative wrt potl at vertex #2
c        dedv3....derivative wrt potl at vertex #3
c
c     Original : MRP   Stanford University   June, 1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      logical lplug
      integer i1,i2,i3
      double precision dedv1,dedv2,dedv3,v21,v31,tmp
      double precision a2,a3,b2,b3,k1,k2a,k2b,k3
      real x21,y21,x31,y31
c
c*************
c**  START  **
c*************
c
c...Get deltas - don't allow x21 to be zero since we are going to
c...divide by it later
      x21=cord(1,i2)-cord(1,i1)
      y21=cord(2,i2)-cord(2,i1)
      v21=fv(i2)-fv(i1)
      if(x21.eq.0.d0) then
         x31=x21
         y31=y21
         v31=v21
         x21=cord(1,i3)-cord(1,i1)
         y21=cord(2,i3)-cord(2,i1)
         v21=fv(i3)-fv(i1)
      else
         x31=cord(1,i3)-cord(1,i1)
         y31=cord(2,i3)-cord(2,i1)
         v31=fv(i3)-fv(i1)
      endif
c
c...Potential = (a2*v21+a3*v31)*X + (b2*v21+b3*v31)*Y
      tmp=y31*x21-y21*x31
      b2=-x31/tmp
      b3=x21/tmp
      a2=(1.d0-b2*y21)/x21
      a3=-b3*y21/x21
c
c...Now find magnitude of E-field.
c...Do it the easy way if we don't need derivatives
      if(lplug) then
         a2=a2*v21+a3*v31
         b2=b2*v21+b3*v31
         efdtri=dsqrt(a2*a2+b2*b2)
      else
         k1=(a2*a2+b2*b2)*v21
         k2b=a2*a3+b2*b3
         k2a=(k2b+k2b)*v21
         k2b=(k2b+k2b)*v31
         k3=(a3*a3+b3*b3)*v31
         efdtri=dsqrt((k1+k2b)*v21+k3*v31)
         if(efdtri.ne.0.d0) then
            dedv2=0.5d0*(k1+k1+k2b)/efdtri
            dedv3=0.5d0*(k3+k3+k2a)/efdtri
            dedv1=-(dedv2+dedv3)
         else
            dedv2=0.d0
            dedv3=0.d0
            dedv1=0.d0
         endif
      endif
c
      return
      end
