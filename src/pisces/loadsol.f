cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon Mar 11 14:51:59 PST 1991 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE LDSOL
      include 'p2conf.h'
c
c     This routine loads old solutions from data files.
c
c     Original : MRP            Stanford University       Nov, 1983
c     Modified : Michael Eldredge -- Stanford (may 88)
c         New xmktmp(), was Unix mktemp().
c     Modified : MJE -- Stanford (aug 89) New xmktmp() calling seq.
c
c     Copyright c 1983 the board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  common area
c
      include     'blank.h'
      include     'setup.h'
c------------------------------------------------------------------
c
c  type declarations
c
      logical lin1fl,lin2fl,lchk,lold,ldiffo,lasc
      character*20 in1fil,in2fil,dofil
c
c FUNCTIONS:
      logical iscval
      logical gtlval
c
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Old file/check parameter flags
      lold=gtlval(2)
      lchk=.not.gtlval(1)
c
c...Ascii?
      lasc=gtlval(4)
      lchk=.not.lasc.and.lchk
c
c...Clear get-file flags
      lin1fl=.false.
      lin2fl=.false.
c
c...Filename for sol1?
      if (.not.iscval(1)) goto 110
c
c...Yes, set flag and get filename
      lin1fl=.true.
      call gtcval(1, in1fil, LEN(in1fil))
      lflow=.false.
c
c...Filename for sol2?
110   if(.not.iscval(2)) goto 100
c
c...Yes, set flag and get filename
      lin2fl=.true.
      call gtcval(2, in2fil, LEN(in2fil))
c
c...Are we getting the difference between files 1 and 2?
      ldiff=gtlval(3)
      if(.not.ldiff) goto 100
      if(lin1fl.and.lin2fl) goto 111
      call erset(148,linum,0)
      goto 999
111   ldiffo=iscval(4)
      if(ldiffo) call gtcval(4, dofil, LEN(dofil))
      call fdiff(in1fil,in2fil,ldiffo,dofil,lold,lchk,lasc)
      lsol1=.false.
      lsol2=.false.
      goto 999
c
c...Get previous solution from file?
100   if(.not.(lin1fl.or.lin2fl)) goto 999
      call solrd(lin1fl,lin2fl,in1fil,in2fil,lold,lchk,lasc)
c
c...Done
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLRD(lin1fl,lin2fl,in1fil,in2fil,lold,lchk,lasc)
      include 'p2conf.h'
c
c     Reads previous solutions from files.
c
c     Original : MRP            Stanford University       Nov, 1983
c     Revised  : CSR                                      Jun 84
c
c     Copyright c 1981 the board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  common area
c
      include     'blank.h'
      include     'setup.h'
      include     'logunt.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c  type declarations
c
      logical lin1fl,lin2fl,lchk,lold,lasc
      integer lu,nelec2,nmat2,np2,ne2,i,ierr
      character*20 in1fil,in2fil,itime
      double precision dummy
      real ntemp,rtime
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c--------------
c  Read sol2?
c--------------
c
      if (.not.lin2fl) goto 100
c
c...Yes, read it (leave out material stuff)
      lu=lutmp
      if(lasc) then
         call fopcl(2,in2fil,20,lu,.false.,ierr)
      else
         call fopcl(1,in2fil,20,lu,.false.,ierr)
      endif
      if(errflg) return
      rewind(lu)
      if(lasc) then
         read(lu,556,end=991,err=992) itime
556      format(a20)
         read(lu,*,end=991,err=992) np2,ne2,nelec2,nmat2,rtime,
     +      ntemp,delt,delt0,ncarr0
         stime0=rtime
      else
         read(lu,end=991,err=992) itime
         read(lu,end=991,err=992) np2,ne2,nelec2,nmat2,rtime,
     +      ntemp,delt,delt0,ncarr0
         stime0=rtime
      endif
c
c...Error if inconsistent
      if((np2.ne.np).or.(nelec2.ne.nelect)) call erset(198,linum,0)
      if(ntemp.ne.temp) call erset(-199,linum,0)
      if(errflg) return
c
c...Read psi,n,p,qfs using old or new format
      if(lasc) then
         read(lu,*,end=991,err=992)
     +      (ofv(i),ofn(i),ofp(i),dummy,dummy,i=1,np2)
         read(lu,*,end=991,err=992)
     +      (obias(i),oamp(i),dummy,dummy,dummy,dummy,i=1,nelec2)
         read(lu,*,end=991,err=992) (dummy,dummy,i=1,nb)
      else
         read(lu,end=991,err=992)
     +      (ofv(i),ofn(i),ofp(i),dummy,dummy,i=1,np2)
         read(lu,end=991,err=992)
     +      (obias(i),oamp(i),dummy,dummy,dummy,dummy,i=1,nelec2)
         read(lu,end=991,err=992) (dummy,dummy,i=1,nb)
      endif
c
      if(lchk) call chkmat(lu,nelec2,nmat2,in2fil)
c
c...Scale factors
      do 3002 i=1,np2
      ofv(i)=ofv(i)*dqkt
      ofn(i)=ofn(i)*dcscli
3002  ofp(i)=ofp(i)*dcscli
c
c...Close the file and print date code - set flag
      call fopcl(0,in2fil,20,lu,.false.,ierr)
      write(luout,3000) in2fil,itime
3000  format(/' Solution 2 read from ',a20/' Date code = ',a20)
      lsol2=.true.
c
c---------------
c  Read sol1?
c---------------
c
100   if (.not.lin1fl) goto 200
c
c...Yes, read it (leave out material stuff)
      lu=lutmp
      if(lasc) then
         call fopcl(2,in1fil,20,lu,.false.,ierr)
      else
         call fopcl(1,in1fil,20,lu,.false.,ierr)
      endif
      if(errflg) return
      rewind(lu)
      if(lasc) then
         read(lu,556,end=991,err=992) itime
         read(lu,*,end=991,err=992) np2,ne2,nelec2,nmat2,rtime,
     +      ntemp,delt,delt0,ncarr0
         stime=rtime
      else
         read(lu,end=991,err=992) itime
         read(lu,end=991,err=992) np2,ne2,nelec2,nmat2,rtime,
     +      ntemp,delt,delt0,ncarr0
         stime=rtime
      endif
c
c...Error if inconsistent
      if((np2.ne.np).or.(nelec2.ne.nelect)) call erset(198,linum,0)
      if(ntemp.ne.temp) call erset(-199,linum,0)
      if(errflg) return
c
c...Read psi,n,p,qfs using old or new format
      if(lasc) then
         read(lu,*,end=991,err=992)
     +        (fv(i),fn(i),fp(i),qfn(i),qfp(i),i=1,np2)
         read(lu,*,end=991,err=992)
     +        (bias(i),amp(i),dflux(i),dflux0(i),
     +         vres(i),vres0(i),i=1,nelec2)
         read(lu,*,end=991,err=992) (dfxpt(i),dfxpt0(i),i=1,nb)
      else
         read(lu,end=991,err=992)
     +        (fv(i),fn(i),fp(i),qfn(i),qfp(i),i=1,np2)
         read(lu,end=991,err=992)
     +        (bias(i),amp(i),dflux(i),dflux0(i),
     +         vres(i),vres0(i),i=1,nelec2)
         read(lu,end=991,err=992) (dfxpt(i),dfxpt0(i),i=1,nb)
      endif
c
      if(lchk) call chkmat(lu,nelec2,nmat2,in1fil)
c
c...Scale factors
      do 3012 i=1,np2
      fv(i)=fv(i)*dqkt
      qfn(i)=qfn(i)*dqkt
      qfp(i)=qfp(i)*dqkt
      fn(i)=fn(i)*dcscli
      fp(i)=fp(i)*dcscli
3012  continue
c
c...Close the file and print date code - set flag
      call fopcl(0,in1fil,20,lu,.false.,ierr)
      write(luout,3011) in1fil,itime
3011  format(/' Solution 1 read from ',a20/' Date code = ',a20)
      lsol1=.true.
c
c...Done
200   return

c.......Here be dragons
  991 call erset(7,linum,0)
      return
  992 call erset(5,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLWR(fname,lwcurr,lasc)
      include 'p2conf.h'
c
c     Writes solutions to files
c
c     Original : C.H.Price    Stanford University       May, 1982
c     Revision : MRP          Stanford University       Nov, 1983
c     Revised  : CSR                                    Jun 84
c     Revised  : C.C.Abbas (carrier-carrier scattering) Sep, 1987
c     Revised  : A. Yabuta (impact ionization)          Oct, 1987
c     Revised  : G. Anderson (photogeneration, cyl cords)Aug, 1989
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
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
      include     'impact.h'
      include     'photo.h'
      include     'symme.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      integer TMPPAD(1401002)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c
c******************************************************************
c
c                   type declarations
c
c
      logical lwcurr,lasc
      integer ierr
      integer lu,i,j
      character*20 fname
      real tpu,tnu,afu,cnu,cpu,ncu,nvu,rtime
      double precision vpnt,vx,vy,jtci,jei,jhi,jdi,jti,ei,ui
c
c******************************************************************
c
c                   start
c
c
c...Write it
      lu=lutmp
      if(lasc) then
         call fopcl(12,fname,20,lu,.false.,ierr)
         rewind(lu)
         write(lu,556,err=991) idaytm
556      format(a20)
         rtime=stime
         write(lu,*,err=991) np,ne,nelect,nmat,rtime,temp,
     +                       delt,delt0,ncarr0
         write(lu,*,err=991) (fv(i)*dktq,fn(i)*dcscl,fp(i)*dcscl,
     +                      qfn(i)*dktq,qfp(i)*dktq,i=1,np)
         write(lu,*,err=991) (bias(i),amp(i),dflux(i),
     +                      dflux0(i),vres(i),vres0(i),i=1,nelect)
         write(lu,*,err=991) (dfxpt(i),dfxpt0(i),i=1,nb)
c
         write(lu,*,err=991) (vsn(i),vsp(i),workf(i),
     +                        cresis(i),ccapac(i),i=1,nelect)
         write(lu,*,err=991) nintrf,((ssdata(i,j),j=1,7),i=1,nintrf)
         write(lu,*,err=991) lconmb,lfldmb,lconlt,lsrh,lauger,
     +                  lbgn,lboltz,lincom,limpct,labbas,
C PHOTOGENERATION, CYLINDRICAL COORDS
     +                  lcyl, lphgen, abscof, flux*SNGL(dcscl)*ktq
         write(lu,*,err=991) (mattyp(i),epsmat(i),i=1,nmat)
      else
         call fopcl(11,fname,20,lu,.false.,ierr)
         rewind(lu)
         write(lu,err=991) idaytm
         rtime=stime
         write(lu,err=991) np,ne,nelect,nmat,rtime,temp,
     +                     delt,delt0,ncarr0
         write(lu,err=991) (fv(i)*dktq,fn(i)*dcscl,fp(i)*dcscl,
     +                      qfn(i)*dktq,qfp(i)*dktq,i=1,np)
         write(lu,err=991) (bias(i),amp(i),dflux(i),
     +                      dflux0(i),vres(i),vres0(i),i=1,nelect)
         write(lu,err=991) (dfxpt(i),dfxpt0(i),i=1,nb)
c
         write(lu,err=991) (vsn(i),vsp(i),workf(i),
     +                      cresis(i),ccapac(i),i=1,nelect)
         write(lu,err=991) nintrf,((ssdata(i,j),j=1,7),i=1,nintrf)
         write(lu,err=991) lconmb,lfldmb,lconlt,lsrh,lauger,
     +                  lbgn,lboltz,lincom,limpct,labbas,
C PHOTOGENERATION, CYLINDRICAL COORDS
     +                  lcyl, lphgen, abscof, flux*SNGL(dcscl)*ktq
         write(lu,err=991) (mattyp(i),epsmat(i),i=1,nmat)
      endif
c
c...Write semiconductor params (unscaled)
      tpu=taup0*qkt
      tnu=taun0*qkt
      afu=affin*ktq
      cnu=cnau*ktq/sngl(dcscl*dcscl)
      cpu=cpau*ktq/sngl(dcscl*dcscl)
      ncu=ncband*sngl(dcscl)
      nvu=nvband*sngl(dcscl)
      if(lasc) then
         write(lu,*,err=991) (semmat(i),i=1,5),tpu,tnu,
     +                     (semmat(i),i=8,11),afu,semmat(13),
     +                     cnu,cpu,(semmat(i),i=16,25),ncu,nvu,
     +                     (semmat(i),i=28,nspar)
      else
         write(lu,err=991) (semmat(i),i=1,5),tpu,tnu,
     +                     (semmat(i),i=8,11),afu,semmat(13),
     +                     cnu,cpu,(semmat(i),i=16,25),ncu,nvu,
     +                     (semmat(i),i=28,nspar)
      endif
c
c...Write currents,fields,recombination?
      if(.not.lwcurr) goto 200
      call nxtel(p2t,p2tc)
      call wcurr
      lflow=.true.
      do 150 i=1,np
      jtci=vpnt(i,1,vx,vy,.false.)
      if(l1hole) then
         jhi=vpnt(i,3,vx,vy,.false.)
         jei=0.d0
      else if(ncarr.eq.1) then
         jhi=0.d0
         jei=vpnt(i,2,vx,vy,.false.)
      else if(ncarr.eq.2) then
         jhi=vpnt(i,3,vx,vy,.false.)
         jei=vpnt(i,2,vx,vy,.false.)
      endif
      jdi=vpnt(i,4,vx,vy,.false.)
      jti=vpnt(i,5,vx,vy,.false.)
      ei=vpnt(i,6,vx,vy,.false.)
C...PHOTOGENERATION
      if(lsrh.or.lauger .or. lphgen) then
        ui=vpnt(i,7,vx,vy,.false.)
      else
        ui=0.d0
      endif
      if(lasc) then
         write(lu,*,err=991) jtci,jei,jhi,jdi,jti,ei,ui
      else
         write(lu,err=991) jtci,jei,jhi,jdi,jti,ei,ui
      endif
150   continue
      write(lu,err=991) (wsol(i),i=1,nepb)
c
c
c...Close the file
200   call  fopcl(0,fname,20,lu,.false.,ierr)
c
c...Write message
      write(luout,3100) fname
      if(luout.ne.lutty) write(lutty,3100) fname
3100  format(' Solution written to ',a20)
      call incnm(fname)
c
c...Done
      return

c......Here be dragons
991   call erset(9,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CHKMAT(lu,nelec2,nmat2,infil)
      include 'p2conf.h'
c
c     Check for consistency in materials and models between
c     input file and current run.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  common area
c
      include     'blank.h'
      include     'setup.h'
      include     'sol.h'
      include     'logunt.h'
      include     'impact.h'
      include     'photo.h'
      include     'symme.h'
c------------------------------------------------------------------
c
c  type declarations
c
      logical l2cm,l2fm,l2cl,l2srh,l2aug,l2bgn,l2bol,lwarn,l2in
      logical lschx,lresx
c carrier-carrier scattering, impact ionization
      logical l2abba,l2imp
c photogeneration, cylindrical coords
      logical l2pgen, labscf, lflux, l2cyl
      real tabs, tflux
c
      integer lu,nelec2,nmat2,i,tmat(MAXREG),nixx,j,k
      real tvsn(MAXCNT),tvsp(MAXCNT),twfn(MAXCNT),teps(MAXREG),
     +tsem(MAXPAR),tss(MAXINF,7)
      double precision tcr(MAXCNT),tcc(MAXCNT)
      character*20 infil
      real tpu,tnu,afu,cnu,cpu,etol
c      equivalence (tsem,ttmp),(tcr,ttmp),(tcc,ttmp(21))
c      equivalence (tvsp,ttmp(51)),(tvsn,ttmp(61)),(twfn,ttmp(41))
      data etol/1.e-4/
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize warning flag
      lwarn=.false.
c
c...Unscale some parameters
      tpu=taup0*qkt
      tnu=taun0*qkt
      afu=affin*ktq
      cnu=cnau*ktq/sngl(dcscl*dcscl)
      cpu=cpau*ktq/sngl(dcscl*dcscl)
c
c...Check contact model parameters
      read(lu) (tvsn(i),tvsp(i),twfn(i),tcr(i),tcc(i),i=1,nelec2)
      do 10 i=1,nelec2
         lschx=(tvsn(i).ne.vsn(i)).or.(tvsp(i).ne.vsp(i)).or.
     +         (twfn(i).ne.workf(i))
         lresx=(tcr(i).ne.cresis(i)).or.(tcc(i).ne.ccapac(i))
         if(lresx.or.lschx) then
            if(.not.lwarn) then
               write(luout,1000) linum,infil
               lwarn=.true.
            endif
            if(lschx) write(luout,1010) i
            if(lresx) write(luout,1011) i
1010        format(9x,'Contact #',i2,' has a different work fn. or',
     +             ' surface recomb. velocity')
1011        format(9x,'Contact #',i2,' has a different resistance ',
     +             'or capacitance attached')
         endif
10    continue
c
c...Check interface properties - remember, interfaces could be
c...ordered diffferently!
      read(lu) nixx,((tss(i,j),j=1,7),i=1,nixx)
      if(nixx.ne.nintrf) goto 318
      do 310 i=1,nixx

         do 314 k=1,nintrf
            do 315 j=1,7
               if(tss(i,j).ne.ssdata(k,j)) goto 314
315         continue
            goto 310
314      continue
         goto 318

310   continue
      goto 311

318   if(.not.lwarn) then
         write(luout,1000) linum,infil
         lwarn=.true.
      endif
      write(luout,3010)
3010  format(9x,'Different interface properties (srv,qf)')
c
311   continue
c
c...Flags
c...Photogeneration, cyl coords added 9/89
      read(lu) l2cm,l2fm,l2cl,l2srh,l2aug,l2bgn,l2bol,l2in,l2imp,
     &         l2abba, l2cyl, l2pgen, tabs, tflux
      l2cm=l2cm.neqv.lconmb
      l2fm=l2fm.neqv.lfldmb
      l2cl=l2cl.neqv.lconlt
      l2srh=l2srh.neqv.lsrh
      l2aug=l2aug.neqv.lauger
      l2bgn=l2bgn.neqv.lbgn
      l2bol=l2bol.neqv.lboltz
      l2in=l2in.neqv.lincom
      l2imp=l2imp.neqv.limpct
      l2abba=l2abba.neqv.labbas
C PHOTOGENERATION, cylindrical coords
      l2cyl=l2cyl.neqv.lcyl
      l2pgen=l2pgen.neqv.lphgen
      if(lphgen) then
        labscf = abscof.ne.tabs
        lflux = ABS(flux - tflux*SNGL(dcscli)*qkt)/flux .gt. etol
      end if
c
      if(l2cm.or.l2fm.or.l2cl.or.l2srh.or.l2aug.or.l2bgn.or.
     +  l2bol.or.l2in.or.l2imp.or.l2abba.or.
     +  l2cyl.or.l2pgen.or.labscf.or.lflux) then
         if(.not.lwarn) then
            write(luout,1000) linum,infil
            lwarn=.true.
         endif
         write(luout,1020)
1020     format(9x,'Inconsistencies in the following models :')
      endif
      if(l2cm)  write(luout,1050) 'Concentration dep. mobility'
      if(l2fm)  write(luout,1050) 'Field dep. mobility'
      if(l2abba) write(luout,1050) 'Carrier-carrier scattering dep.',
     &                             ' mobility'
      if(l2srh) write(luout,1050) 'SRH recombination'
      if(l2aug) write(luout,1050) 'Auger recombination'
      if(l2bgn) write(luout,1050) 'Band-gap narrowing'
      if(l2cl)  write(luout,1050) 'Concentration dep. lifetime'
      if(l2bol) write(luout,1050) 'Carrier statistics'
      if(l2imp)  write(luout,1050) 'Impact ionization'
      if(l2in)  write(luout,1050) 'Incomplete impurity ionization'
c PHOTOGENERATION, Cylindrical coords
      if(l2cyl)  write(luout,1050) 'Cylindrical coordinates'
      if(l2pgen)  write(luout,1050) 'Photogeneration (Off/On)'
      if(lphgen .and. .not. l2pgen .and. (labscf.or.lflux)) then
        write(luout, 1050) 'Photogeneration parameters:'
        if (labscf) write(luout, 1050) '  Absorption coefficient'
        if (lflux)  write(luout, 1050) '  Photon flux'
      end if
c
c...Materials/regions
      if(nmat2.eq.nmat) goto 100
      if(.not.lwarn) then
         write(luout,1000) linum,infil
         lwarn=.true.
      endif
      write(luout,1030)
1030  format(9x,'No. of regions is not equivalent')
      goto 200
c
100   read(lu) (tmat(i),teps(i),i=1,nmat2)
      do 20 i=1,nmat2
      if((tmat(i).ne.mattyp(i)).or.(teps(i).ne.epsmat(i))) then
         if(.not.lwarn) then
            write(luout,1000) linum,infil
            lwarn=.true.
         endif
         write(luout,1040) i
1040     format(9x,'Region #',i2,' has a different material type or',
     +          ' permittivity')
      endif
20    continue
c
c...Semiconductor parameters
c...Check - use relative check for scaled parameters
200   read(lu) (tsem(i),i=1,nspar)
c
      if(tsem(1).ne.stype) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Semiconductor type'
      endif
c
      if(tsem(2).ne.epsoq) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Permittivity'
      endif
c
      if((tsem(4).ne.fmun0).or.(tsem(13).ne.fmup0)) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Low-field mobilities'
      endif
c
      if(tsem(5).ne.vsat) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Saturation velocity'
      endif
c
      if( ( abs(tsem(6)-tpu)/tpu.gt.etol ).or.
     +    ( abs(tsem(7)-tnu)/tnu.gt.etol )) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Carrier lifetimes'
      endif
c
      if(tsem(8).ne.mudeg) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Mobility surface degredation factor'
      endif
c
      if(tsem(9).ne.eg300) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Energy gap at 300K'
      endif
c
      if((tsem(10).ne.egalph).or.(tsem(11).ne.egbeta)) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Energy gap temperature coefficients'
      endif
c
      if( abs(tsem(12)-afu)/afu.gt.etol ) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Electron affinity'
      endif
c
      if( ( abs(tsem(14)-cnu)/cnu.gt.etol ).or.
     +    ( abs(tsem(15)-cpu)/cpu.gt.etol )) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Auger coefficients'
      endif
c
      if((tsem(16).ne.arichn).or.(tsem(17).ne.arichp)) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Richardson coefficients'
      endif
c
      if(tsem(18).ne.nc300) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Nc at 300K'
      endif
c
      if(tsem(19).ne.nv300) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Nv at 300K'
      endif
c
      if(tsem(28).ne.nsrhn) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Nsrh(n)'
      endif
c
      if(tsem(29).ne.nsrhp) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Nsrh(p)'
      endif
c
      if(tsem(30).ne.etrap) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Trap level'
      endif
c
      if(tsem(32).ne.gcband) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Donor degeneracy Factor - gcb'
      endif
c
      if(tsem(33).ne.gvband) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Acceptor degeneracy Factor - gcb'
      endif
c
      if(tsem(34).ne.edband) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Donor activation energy - edb'
      endif
c
      if(tsem(35).ne.eaband) then
          if(.not.lwarn) then
             write(luout,1000) linum,infil
             lwarn=.true.
          endif
          write(luout,1050) 'Acceptor activation energy - eab'
      endif
c
1000  format(/' Warning in line ',i2,' - '/
     +        '    Inconsistencies were found between the solution',
     +        ' in ',a20/'    and the models/parameters specified',
     +        ' in this input file:'/)
1050  format(14x,a)
c
c...Whew!
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FDIFF(in1fil,in2fil,ldiffo,outfil,lold,lchk,lasc)
      include 'p2conf.h'
c
c     Subroutine to subtract two PISCES solutions.
c
c     Original : MRP   July 1984
c     Modified : CSR   July 1985 Now reads ascii files. !@#$%
c     Modified : C.C.Abbas  (carrier-carrier scattering)   Sep, 1987
c     Modified : A. Yabuta  (impact ionization)            Oct, 1987
c     Modified : G. Anderson (photogeneration)            Aug, 1989
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
      include     'logunt.h'
      include     'impact.h'
      include     'photo.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      include     'difftmp.h'
      integer TMPPAD(1323602)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c  type declarations
c
      logical ldiffo,lold,lchk,lasc
      integer i,j,lu1,lu2,lu3,ierr
      character*20 in1fil,in2fil,outfil,itime1,itime2
      integer np2,ne2,nelec2,nmat2,np1,ne1,nelec1,nmat1,ndum
      real ntemp1,ntemp2,stime1,stime2
      double precision dum1,dum2
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
      lu1=lutmp
      lu2=lusup
      lu3=lutmp2
      if(errflg) return
      if(.not.lasc) then
         call fopcl(1,in1fil,20,lu1,.false.,ierr)
         call fopcl(1,in2fil,20,lu2,.false.,ierr)
         read(lu1,end=991,err=992) itime1
         read(lu1,end=991,err=992) np1,ne1,nelec1,nmat1,stime1,ntemp1,
     +      dum1,dum2,ndum
         read(lu2,end=991,err=992) itime2
         read(lu2,end=991,err=992) np2,ne2,nelec2,nmat2,stime2,ntemp2,
     +      dum1,dum2,ndum
      else
         call fopcl(2,in1fil,20,lu1,.false.,ierr)
         call fopcl(2,in2fil,20,lu2,.false.,ierr)
         read(lu1,556,end=991,err=992) itime1
         read(lu1,*,end=991,err=992) np1,ne1,nelec1,nmat1,stime1,
     +      ntemp1,dum1,dum2,ndum
         read(lu2,556,end=991,err=992) itime2
         read(lu2,*,end=991,err=992) np2,ne2,nelec2,nmat2,stime2,
     +      ntemp2,dum1,dum2,ndum
556      format(a20)
      endif
c
c...Error if inconsistent
      if((np2.ne.np).or.(nelec2.ne.nelect).or.(np1.ne.np).or.
     +   (nelec1.ne.nelect)) call erset(198,linum,0)
      if((ntemp1.ne.temp).or.(ntemp2.ne.temp)) call erset(-199,linum,0)
      if(errflg) return
      if(ldiffo) then
         call fopcl(11,outfil,20,lu3,.false.,ierr)
         write(lu3) idaytm
         write(lu3) np,ne,nelect,nmat,0.,ntemp1,dum1,dum2,ndum
      endif
c
c...Read psi,n,p,qfs using old or new format
      if(.not.lasc) then
         read(lu1,end=991,err=992)
     +   (fv1(i),fn1(i),fp1(i),qfn1(i),qfp1(i),i=1,np)
         read(lu2,end=991,err=992)
     +   (fv2(i),fn2(i),fp2(i),qfn2(i),qfp2(i),i=1,np)
      else
         read(lu1,*,end=991,err=992)
     +   (fv1(i),fn1(i),fp1(i),qfn1(i),qfp1(i),i=1,np)
         read(lu2,*,end=991,err=992)
     +   (fv2(i),fn2(i),fp2(i),qfn2(i),qfp2(i),i=1,np)
      endif
c
      do 350 i=1,np
         fv(i)=fv1(i)-fv2(i)
         fn(i)=fn1(i)-fn2(i)
         fp(i)=fp1(i)-fp2(i)
         qfn(i)=qfn1(i)-qfn2(i)
         qfp(i)=qfp1(i)-qfp2(i)
350   continue
      if(ldiffo) write(lu3) (fv(i),fn(i),fp(i),qfn(i),qfp(i),i=1,np)
c
c...Read terminal conditions
      if (.not.lasc) then
         read(lu1,end=991,err=992) (bias1(i),amp1(i),dfl1(i),dfl01(i),
     +                              vres1(i),vres10(i),i=1,nelect)
         read(lu1,end=991,err=992) (dfx11(i),dfx10(i),i=1,nb)
         read(lu2,end=991,err=992) (bias2(i),amp2(i),dfl2(i),dfl02(i),
     +                              vres2(i),vres20(i),i=1,nelect)
         read(lu2,end=991,err=992) (dfx21(i),dfx20(i),i=1,nb)
      else
         read(lu1,*,end=991,err=992) (bias1(i),amp1(i),dfl1(i),dfl01(i),
     +                              vres1(i),vres10(i),i=1,nelect)
         read(lu1,*,end=991,err=992) (dfx11(i),dfx10(i),i=1,nb)
         read(lu2,*,end=991,err=992) (bias2(i),amp2(i),dfl2(i),dfl02(i),
     +                              vres2(i),vres20(i),i=1,nelect)
         read(lu2,*,end=991,err=992) (dfx21(i),dfx20(i),i=1,nb)
      endif
c
      do 360 i=1,nelect
         bias(i)=bias1(i)-bias2(i)
         amp(i)=amp1(i)-amp2(i)
         dflux(i)=dfl1(i)-dfl2(i)
         dflux0(i)=dfl01(i)-dfl02(i)
         vres(i)=vres1(i)-vres2(i)
         vres0(i)=vres10(i)-vres20(i)
360   continue
      do 361 i=1,nb
         dfxpt(i)=dfx11(i)-dfx21(i)
         dfxpt0(i)=dfx10(i)-dfx20(i)
361   continue

      if(ldiffo) then
         write(lu3) (bias(i),amp(i),dflux(i),dflux0(i),
     +               vres(i),vres0(i),i=1,nelect)
         write(lu3) (dfxpt(i),dfxpt0(i),i=1,nb)
      endif
c
c...Check models?
      if(lchk) then
         call chkmat(lu1,nelect,nmat,in1fil)
         call chkmat(lu2,nelect,nmat,in2fil)
      endif
      if(ldiffo) then
         write(lu3) (vsn(i),vsp(i),workf(i),
     +               cresis(i),ccapac(i),i=1,nelect)
         write(lu3) nintrf,((ssdata(i,j),j=1,7),i=1,nintrf)
         write(lu3) lconmb,lfldmb,lconlt,lsrh,lauger,lbgn,lboltz,
     +              lincom,limpct,labbas,
c PHOTOGENERATION
     +              lphgen, abscof, flux*SNGL(dcscl)*ktq
         write(lu3) (mattyp(i),epsmat(i),i=1,nmat)
         write(lu3) (semmat(i),i=1,nspar)
      endif
c
c...Currents, etc?
      if(lasc) goto 151
      do 150 i=1,np
      if(i.eq.1) then
         read(lu1,end=151,err=151) (jxi1(j),j=1,7)
         read(lu2,end=151,err=151) (jxi2(j),j=1,7)
      else
         read(lu1,end=991,err=992) (jxi1(j),j=1,7)
         read(lu2,end=991,err=992) (jxi2(j),j=1,7)
      endif
      do 157 j=1,7
157   jxi(i,j)=jxi1(j)-jxi2(j)
      if(ldiffo) write(lu3) (jxi(i,j),j=1,7)
150   continue

      ldcur=.true.
      read(lu1,end=991,err=992) (wsol(i),i=1,nepb)
      read(lu2,end=991,err=992) (wxi(i),i=1,nepb)
      do 168 i=1,nepb
         wsol(i)=wsol(i)-wxi(i)
168   continue
      lflow=.true.
      goto 159

151   ldcur=.false.
c
c...Close the file and print date code - set flag
159   call fopcl(0,in1fil,20,lu1,.false.,ierr)
      call fopcl(0,in2fil,20,lu2,.false.,ierr)
      write(luout,3000) in2fil,itime2
3000  format(/' Solution 2 read from ',a20/' Date code = ',a20)
      write(luout,3011) in1fil,itime1
3011  format(/' Solution 1 read from ',a20/' Date code = ',a20)
      if(ldiffo) then
         call fopcl(0,outfil,20,lu3,.false.,ierr)
         write(luout,3019) outfil
3019     format(/' Difference written to ',a20)
      endif
c
c...Scale factors
      do 3012 i=1,np
         fv(i)=fv(i)*dqkt
         qfn(i)=qfn(i)*dqkt
         qfp(i)=qfp(i)*dqkt
         fn(i)=fn(i)*dcscli
         fp(i)=fp(i)*dcscli
3012  continue
c
c...Done
      return
c
c.......Here be dragons
  991 call erset(7,linum,0)
      return
  992 call erset(5,linum,0)
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SOLCHK
      include 'p2conf.h'
c
c     Check one solution against another. (Assumed both are properly
c     ordered)
c
c     Original : MRP   July 1984
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
      include     'logunt.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      include     'difftmp.h'
      integer TMPPAD(1323602)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c  type declarations
c
      logical lsame
      character*60 itit1
      character*20 in1fil,itime1,mshfil
      integer np1,ne1,nelec1,nmat1,i,j,idum,npm,itot,newxy
      integer i1v,i1n,i1p,i0n,i0v,i0p,ierr
      integer   i2dum
      real ntemp1,stime1,rdum,xm,ym
      double precision dum,relv,relp,reln,rtemp,areln,arelp,arelv
      double precision relnn,relpp
c
c FUNCTIONS:
      logical gtlval
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Is mesh the same? If not, read new one.
      lsame=gtlval(1)
      if(lsame) then
         npm=np
      else
         call gtcval(1, mshfil, LEN(mshfil))
         call fopcl(1,mshfil,20,lutmp,.false.,ierr)
         if(errflg) return
         read(lutmp,end=991,err=992) itit1
         read(lutmp,end=991,err=992) idum,idum,npm,idum,idum,
     +                               idum,idum,idum
         read(lutmp,end=991,err=992) (x1i(i),y1i(i),rdum,rdum,i2dum,
     +                                i2dum,rdum,rdum,i2dum,i=1,npm)
         call fopcl(0,mshfil,20,lutmp,.false.,ierr)
      endif
c
c...Get sol file and read (error if incosistent)
      call gtcval(2, in1fil, LEN(in1fil))
      call fopcl(1,in1fil,20,lutmp,.false.,ierr)
      if(errflg) return
      read(lutmp,end=991,err=992) itime1
      read(lutmp,end=991,err=992) np1,ne1,nelec1,nmat1,stime1,ntemp1,
     +    dum,dum,idum
      if(np1.eq.npm) goto 5
      call erset(198,linum,0)
      return
5     read(lutmp,end=991,err=992)
     +      (fv1(i),fn1(i),fp1(i),qfn1(i),qfp1(i),i=1,np1)
      call fopcl(0,in1fil,20,lutmp,.false.,ierr)
c
c...Get avg/max error
      i0v=0
      i0n=0
      i0p=0
      relnn=0.d0
      relpp=0.d0
      arelv=0.d0
      areln=0.d0
      arelp=0.d0
      relv=0.d0
      reln=0.d0
      relp=0.d0
      itot=0
      do 3012 i=1,np1
c
c...Get node in current mesh corresponding to this point
      if(lsame) then
         j=i
         itot=itot+1
      else
         j=newxy(x1i(i),y1i(i))
         if(j.eq.0) goto 3012
         itot=itot+1
      endif
c
c...Psi
      rtemp=dabs(dktq*fv(j)-fv1(i))
      arelv=arelv+rtemp
      if(rtemp.gt.relv) then
         relv=rtemp
         i1v=i
         i0v=j
      endif
c
c...Electrons
      if(fn(i).gt.0.d0) then
         dum=dlog(dabs(fn(i)-fn1(j)*dcscli))-dlog(fn(i))
         if(dum.le.maxexp) then
            relnn=dmax1(relnn,dexp(dum))
         else
            relnn=maxdbl
         endif
      endif
      rtemp=dabs(dktq*qfn(j)-qfn1(i))
      areln=areln+rtemp
      if(rtemp.gt.reln) then
         reln=rtemp
         i1n=i
         i0n=j
      endif
c
c...Holes
      if(fp(i).gt.0.d0) then
         dum=dlog(dabs(fp(i)-fp1(j)*dcscli))-dlog(fp(i))
         if(dum.le.maxexp) then
            relpp=dmax1(relpp,dexp(dum))
         else
            relpp=maxdbl
         endif
      endif
      rtemp=dabs(dktq*qfp(j)-qfp1(i))
      arelp=arelp+rtemp
      if(rtemp.gt.relp) then
         relp=rtemp
         i1p=i
         i0p=j
      endif

      if(ldbug2) then
         write(6,*) ' '
         write(6,*) 'i,j,x,y : ',i,j,x1i(i),y1i(i),cord(1,j),cord(2,j)
         write(6,*) 'fv1,fv  : ',fv1(i),fv(j)*dktq
         write(6,*) 'fn1,fn  : ',fn1(i),fn(j)*dcscl
         write(6,*) 'fp1,fp  : ',fp1(i),fp(j)*dcscl
         write(6,*) ' '
      endif

3012  continue
c
c...Print it and leave
      write(luout,1001) mshfil,in1fil
1001  format(/' Solution comparison : '/
     +        '    Mesh read from ',a20/
     +        '    Solution read from ',a20/)

      if(.not.lsame) then
         write(luout,1000) np,np1,itot
1000     format( '    Current mesh has ',i5,' points'/
     +           '    Loaded mesh has ',i5,' points'/
     +           '    No. of common points is ',i5/)
      endif

      if(i0v.ne.0) then
         xm=cord(1,i0v)*1.e4
         ym=cord(2,i0v)*1.e4
      else
         xm=0.
         ym=0.
      endif
      write(luout,1005) relv,xm,ym
1005  format( '       Max. potential error   = ',1pe13.6,
     +        '   at (',0pf8.4,',',0pf8.4,')')

      if(i0n.ne.0) then
         xm=cord(1,i0n)*1.e4
         ym=cord(2,i0n)*1.e4
      else
         xm=0.
         ym=0.
      endif
      write(luout,1006) reln,xm,ym
1006  format( '       Max. electron qf error = ',1pe13.6,
     +        '   at (',0pf8.4,',',0pf8.4,')')

      if(i0p.ne.0) then
         xm=cord(1,i0p)*1.e4
         ym=cord(2,i0p)*1.e4
      else
         xm=0.
         ym=0.
      endif
      write(luout,1007) relp,xm,ym
1007  format( '       Max. hole qf error     = ',1pe13.6,
     +        '   at (',0pf8.4,',',0pf8.4,')')

      write(luout,1002) arelv/float(itot),areln/float(itot),
     +                  arelp/float(itot)
1002  format( '       Avg. potential error   = ',1pe13.6/
     +        '       Avg. electron qf error = ',1pe13.6/
     +        '       Avg. hole qf error     = ',1pe13.6/)

      write(luout,1022) relnn,relpp
1022  format( '       Max. rel. electron error = ',1pe13.6/
     +        '       Max. rel. hole error     = ',1pe13.6/)

      if(ldebug) then
         write(6,*) '      psi (current/loaded) : ',
     +              i0v,dktq*fv(i0v),'/',i1v,fv1(i1v)
         write(6,*) '      n   (current/loaded) : ',
     +              i0n,dcscl*fn(i0n),'/',i1n,fn1(i1n)
         write(6,*) '      p   (current/loaded) : ',
     +              i0p,dcscl*fp(i0p),'/',i1p,fp1(i1p)
      endif
      return

c
c...Here be dragons
  991 call erset(7,linum,0)
      return
  992 call erset(5,linum,0)
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION NEWXY(xi,yi)
      include 'p2conf.h'
c
c     Original : MRP
c
c     Copyright c 1984 The board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      real xi,yi,xj,yj,dxhi,dxlo,dyhi,dylo,fact
      integer ilo,ihi,imid
      data fact/1.e-6/
c------------------------------------------------------------------
c
c  Start
c
      newxy=0
      dxlo=xi-fact*abs(xi)
      dxhi=xi+fact*abs(xi)
      dylo=yi-fact*abs(yi)
      dyhi=yi+fact*abs(yi)
      if((dxhi.lt.cord(1,1)).or.(dxlo.gt.cord(1,np))) goto 99
c
c...Mesh coordinates are ordered by x - use binary search to
c...exact x
      ilo=0
      ihi=np+1
10    imid=ihi-ilo
      if(imid.le.1) goto 99
      imid=ilo+imid/2
      xj=cord(1,imid)
      if((xj.ge.dxlo).and.(xj.le.dxhi)) goto 50
      if(xj.lt.xi) goto 20
      ihi=imid
      goto 10
20    ilo=imid
      goto 10
c
c...Found an x - search for yi
50    xj=cord(1,imid)
      yj=cord(2,imid)
      if(xj.lt.dxlo) goto 55
      if(xj.gt.dxhi) goto 60
      if((yj.ge.dylo).and.(yj.le.dyhi)) goto 70
      if(yj.gt.yi) goto 60
55    ilo=imid
      goto 69
60    ihi=imid
69    imid=ihi-ilo
      if(imid.le.1) goto 99
      imid=ilo+imid/2
      goto 50
c
c...Found yi too!
70    newxy=imid
c
c...Bye
99    return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DUMPO(lsave,lsolu1)
      include 'p2conf.h'
c
c     Subroutine to dump and restore solutions.
c
c     MRP July 1984
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
      include     'setup.h'
      include     'names.h'
      include     'sol.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer i,ierr
      logical lsave,lsolu1
      character*40 namfil
c------------------------------------------------------------------
c
c  START
c
      if(.not.lsave) goto 100
c
c...Save
c.....Get a temp file name to use.
      call xmktmp(namfil, LEN(namfil), 'p2dmpo',6)
      call fopcl(31,namfil,LEN(namfil),lutmp2,.false.,ierr)
      if(lsolu1) then
         write(lutmp2) (fv(i),fn(i),fp(i),i=1,np)
         write(lutmp2) (bias(i),dflux(i),namp(i,1),namp(i,2),vres(i),
     +                  i=1,nelect)
         write(lutmp2) (dfxpt(i),i=1,nb)
      else
         write(lutmp2) (ofv(i),ofn(i),ofp(i),i=1,np)
         write(lutmp2) (obias(i),dflux0(i),vres0(i),i=1,nelect)
         write(lutmp2) (dfxpt0(i),i=1,nb)
      endif
      return
c
c...Restore
100   rewind(lutmp2)
      if(lsolu1) then
         read(lutmp2) (fv(i),fn(i),fp(i),i=1,np)
         read(lutmp2) (bias(i),dflux(i),namp(i,1),namp(i,2),vres(i),
     +                 i=1,nelect)
         read(lutmp2) (dfxpt(i),i=1,nb)
      else
         read(lutmp2) (ofv(i),ofn(i),ofp(i),i=1,np)
         read(lutmp2) (obias(i),dflux0(i),vres0(i),i=1,nelect)
         read(lutmp2) (dfxpt0(i),i=1,nb)
      endif
      call fopcl(0,namfil,LEN(namfil),lutmp2,.false.,ierr)
      return
      end
