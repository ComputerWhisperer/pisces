cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Sep 11 03:26:16 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
       SUBROUTINE SETMAT
      include 'p2conf.h'
c 
c     This segment performs all of the initial setup needed for 
c     solution process to begin. 
c 
c     Cards recognized= 
c                   material
c                   models  
c                   contact 
c     
c     Original : C.H.Price    Stanford University        May, 1982
c     Modified : MRP          Stanford University        Nov, 1983
c
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       
c-----------------------------------------------------------------------
c 
c                   common areas
c 
      include     'blank.h'
      include     'setup.h'
      include     'key.h'
c------------------------------------------------------------------
c
c FUNCTIONS:
      logical gtkey
c****************************************************************** 
c 
c                   start 
c
c...If we've scaled already, we dont belong here
      if(lscale) then
         call erset(2,linum,0)
         return
      endif
c 
c---------------------
c   Card check loop
c---------------------
c
c...Material card?
100   if(keyid.ne.kmater) goto 110 
      call matck
      if(errflg) return
      goto 101
c
c...Interface card?
110   if(keyid.ne.kintf) goto 120
      call intfck
      if(errflg) return
      goto 101
c
c...Contact card? 
120   if(keyid.ne.kconta) goto 200 
      call cntck
      if(errflg) return
c
c...Get next card, if eof terminate cleanly 
101   eofflg = .not.gtkey(keyid)
      errflg = eofflg
      if(eofflg) goto 200
      if(errflg) goto 9999 
      if(keyid.eq.kcomme) goto 101 
      goto 100
c 
c-------------------------------------------------------------
c 
c...Set next-card-already-read flag for PISCES controller 
200   lcrdrd=.true. 
c 
c...Done!!! 
9999  continue
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MINIT
      include 'p2conf.h'
c 
c     This routine sets initial values for all of the semiconductor 
c     or insulator material parameters in each region.
c 
c     Original : C.H.Price       Stanford University       May, 1982
c     Modified : MRP             Stanford University       Nov, 1983
c     Modified : HRY (added constatns for incomplete ion.
c                 & redefined etrapn & eliminated etrapp)  Jun. 1985
C     MODIFIED:  SHIN  (ADD MU FOR DATA, ARORA'S MODEL)        11/87
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
      include     'setup.h'
      include     'stat.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer num,i
      real simat(MAXPAR),gamat(MAXPAR),epsox,epsnit,epssap,epq
c 
c****************************************************************** 
c 
c                   data
c 
c                   semiconductor material data format: 
c                type       epsilon    ni(t)      mun0       vsat 
c                taup0      taun0      mudeg      egap(300)  eg alpha 
c                eg beta    affinity   mup0       cnau       cpau
c                arichn     arichp     nc300      nv300      vsurfn
c                vsurfp     muco1      muco2      muco3      muco4
c                ncband     nvband     nsrhn      nsrhp      etrap
c                null       gcb        gvb        edb        eab   
C...SHIN         MU1N       MU2N       MU1P       MU2P       null
c 
c
c...difference between maxpar and the actual number used (in the data
c....statement. Must be >= 1.
         integer SIPARD
      parameter (SIPARD = MAXPAR-40)
         integer GAPARD
      parameter (GAPARD = MAXPAR-35)
c
      data simat/1.        ,11.8      ,0.        ,1000.     ,0.        ,
     +           1.e-7     ,1.e-7     ,1.        ,1.08      ,4.73e-4   ,
     +           636.      ,4.17      ,500.      ,2.8e-31   ,9.9e-32   ,
     +           110.      ,30.       ,2.8e19    ,1.04e19   ,0.        ,
     +           0.        ,2.        ,1.        ,0.        ,0.        ,
     +           0.        ,0.        ,5.0e16    ,5.0e16    ,0.        ,
     +           0.        ,2.0d0     ,4.0d0     ,0.044d0   ,0.045d0   ,
     +           88.       ,1252.     ,54.3      ,407.      ,0.        ,
     +         SIPARD*0. /
c 
      data gamat/2.        ,10.9      ,0.        ,5000.     ,0.        ,
     +           1.e-7     ,1.e-7     ,1.        ,1.43      ,5.405e-4  ,
     +           204.      ,4.07      ,400.      ,2.8e-31   ,9.9e-32   ,
     +           6.2857    ,105.      ,4.7e17    ,7.0e18    ,0.        ,
     +           0.        ,4.0e3     ,0.        ,0.        ,0.        ,
     +           0.        ,0.        ,5.0e16    ,5.0e16    ,0.        ,
     +           0.        ,2.0d0     ,2.0d0     ,0.005d0   ,0.005d0   ,
     +         GAPARD*0. /
c 
      data epsox/3.9/,epsnit/7.5/,epssap/12./
c 
c****************************************************************** 
c 
c                   start 
c
c...Initialize qf and interface recom. vel.
      do 5 i=1,np
         qss(i)=0.
         snintf(i)=0.
         spintf(i)=0.
5     continue
c 
c...Permittivity always appears as kT/q*(e/q) (Get T dep. later)
      epq=eps0/qcharg 
      semmat(1)=0.
c
c------------------------
c  LOOP THROUGH REGIONS
c------------------------
c
      do 100 num=1,nmat
c
c...Silicon?
      if(mattyp(num).ne.1) goto 20 
      if((semmat(1).ne.simat(1)).and.(semmat(1).ne.0)) then
         call erset(144,linum,0)
         return
      endif
      epsmat(num)=simat(2)*epq
      do 15 i=1,nspar
      semmat(i)=simat(i)
15    continue
      epsoq=epsmat(num) 
      goto 100
c
c...Gallium arsenide?
20    if(mattyp(num).ne.2) goto 30 
      if((semmat(1).ne.gamat(1)).and.(semmat(1).ne.0)) then
         call erset(144,linum,0)
         return
      endif
      epsmat(num)=gamat(2)*epq
      do 25 i=1,nspar
      semmat(i)=gamat(i)
25    continue
      epsoq=epsmat(num) 
      goto 100
c
c...General semiconductor?
30    if(mattyp(num).ne.3) goto 40
      if((semmat(1).ne.3).and.(semmat(1).ne.0)) then
         call erset(144,linum,0)
         return
      endif
      epsmat(num)=0.
      do 35 i=1,nspar
      semmat(i)=0.
35    continue
      semmat(1)=3.
      goto 100
c
c...Oxide?
40    if(mattyp(num).ne.-1) goto 50
      epsmat(num)=epsox*epq
      goto 100
c
c...Nitride?
50    if(mattyp(num).ne.-2) goto 60
      epsmat(num)=epsnit*epq
      goto 100
c
c...Sapphire?
60    if(mattyp(num).ne.-3) goto 70
      epsmat(num)=epssap*epq
      goto 100
c
c...General insulator?
70    if(mattyp(num).ne.-4) goto 100
      epsmat(num)=0.
c
c...Done
100   continue
      lminit=.true.
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MATCK
      include 'p2conf.h'
c
c     This routine gets specified material parameters for a particular
c     region.
c
c     Original : C.H.Price       Stanford University       May, 1982
c     Modified : MRP             Stanford University       Nov, 1983
C     MODIFIED : SHIN  (ADD MU OPTION FOR ARORA'S MODEL)       11/87
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
      include     'setup.h'
      include     'stat.h'
c
c******************************************************************
c
c                   type declarations
c
      integer num,i
      real epq
c
c FUNCTIONS:
      logical isrval
      real    gtrval
c
c******************************************************************
c
c                   start
c
c...Permittivity always appears as kT/q*(e/q) (Get T dep. later)
      epq=eps0/qcharg
c
c...Material number
      num=gtrval(1)
      if(num.ge.1.and.num.le.nmat) goto 10
      call erset(64,linum,nmat)
      return
c
c...Get permitt. if spec.
10    if(isrval(4)) epsmat(num)=gtrval(4)*epq
c
c...Warn if semic. param. specified in an insulator
      if(mattyp(num).gt.0) goto 200
      do 150 i=2,11
      if(i.eq.4) goto 150
      if(.not.isrval(i)) goto 150
      call erset(-36,linum,0)
      goto 200
150   continue
c
c...Get any semic. parameters
200   if(isrval(2)) mudeg=gtrval(2)
      if(isrval(3)) eg300=gtrval(3)
      if(isrval(4)) epsoq=gtrval(4)*epq
      if(isrval(5)) taup0=gtrval(5)
      if(isrval(6)) taun0=gtrval(6)
      if(isrval(7)) fmun0=gtrval(7)
      if(isrval(8)) vsat=gtrval(8)
      if(isrval(9)) egalph=gtrval(9)
      if(isrval(10)) egbeta=gtrval(10)
      if(isrval(11)) affin=gtrval(11)
      if(isrval(12)) fmup0=gtrval(12)
      if(isrval(13)) cnau=gtrval(13)
      if(isrval(14)) cpau=gtrval(14)
      if(isrval(15)) nc300=gtrval(15)
      if(isrval(16)) nv300=gtrval(16)
      if(isrval(17)) arichn=gtrval(17)
      if(isrval(18)) arichp=gtrval(18)
      if(isrval(19)) nsrhn=gtrval(19)
      if(isrval(20)) etrap=gtrval(20)
      if(isrval(21)) nsrhp=gtrval(21)
      if(isrval(22)) gcband=gtrval(22)
      if(isrval(23)) gvband=gtrval(23)
      if(isrval(24)) edband=gtrval(24)
      if(isrval(25)) eaband=gtrval(25)
C...SHIN
      if(isrval(26)) MU1N=gtrval(26)
      if(isrval(27)) MU2N=gtrval(27)
      if(isrval(28)) MU1P=gtrval(28)
      if(isrval(29)) MU2P=gtrval(29)
           
c
c...Done
      lmatcd=.true.
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MODCK
      include 'p2conf.h'
c 
c     This routine gets physical model parameters (mobility,
c     recombination, carrier statistics and temperature).
c 
c     Original : C.H.Price       Stanford University       May, 1982
c     Modified : MRP             Stanford University       Nov, 1983
c     Modified : C.C.Abbas  (carrier-carrier scattering)   Sep, 1987
c     Modified : A. Yabuta  (impact ionization)            Oct, 1987
c     Modified : G. Anderson (photogeneration)            June, 1989
C                SHIN   U. Texas                                1/88
C                 ADD TRANS. E DEP. MOBILITY MODEL
C                   (REF: ED-30, P.1634, 1983)
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
      include     'setup.h'
      include     'stat.h'
      include     'impact.h'
      include     'photo.h'
      include     'trfldmob.h'
c
       real ntemp
       real top,bottom,left,right
c
c FUNCTIONS:
      logical isrval, islval
      real    gtrval
      logical gtlval
c****************************************************************** 
c 
c                   start 
c
c...Get flags 
      if(islval(1)) lsrh=gtlval(1)
      if(islval(2)) lauger=gtlval(2)
      if(islval(3)) lbgn=gtlval(3)
      if(islval(5)) lconmb=gtlval(5)
      if(islval(9)) labbas=gtlval(9)
      if(labbas) lconmb=.true.
      if(islval(7)) lfldmb=gtlval(7)
      if(islval(8)) lincom=gtlval(8)
      if(islval(4)) lconlt=gtlval(4)
      if(lconlt) lsrh=.true.
      if(islval(13)) lboltz=gtlval(13)
      if(islval(14)) lboltz=.not.gtlval(14)
      if(islval(15)) lsrfmb=gtlval(15)
      lsetpr=gtlval(12)
      if(islval(17)) limpct=gtlval(17)
      if(islval(18)) lphgen=gtlval(18)
      if(islval(19)) LTFLDMB=gtlval(19)
      if(islval(20)) LOTFL=gtlval(20)
      IF(LOTFL) LTFLDMB=.TRUE.
c
c
c...Mobility model parameters
      if(.not.isrval(1)) goto 310
      muco1=gtrval(1)
      if(muco1.le.0.) call erset(167,linum,0)
310   if(.not.isrval(2)) goto 315
      muco2=gtrval(2)
      if(muco2.le.0.) call erset(167,linum,0)
c
c   Concentration dependent mobility models...
c   "analytic" (lconm2) is 16, "arora" (lconm3) is 21.
c   "user1" (luser1) is 22.
c   "conmob" is assumed if "arora" or "analytic" is specified
c
315   if((.not.lconmb).and.
     +(islval(16).or.islval(21).or.islval(22))) then
        lconmb=.true.
      endif
c
      if(.not.lconmb) goto 320
      if(islval(16)) lconm2=gtlval(16)
      if(islval(21)) lconm3=gtlval(21)
      if(islval(22)) luser1=gtlval(22)
c
c   Check for conflicts; we can only have one of
c   "ccsmob", "analytic" and "arora."  It seems to make
c   the most sense to report an error if "ccsmob" is specified
c   with one of the others, default to "arora" if both
c   "arora" and "analytic" are specified and "ccsmob" and "user1"
c   are not, but default to "user1" if it is specified and "ccsmob" isn't.
c
      if (labbas.and.(lconm2.or.lconm3.or.luser1)) then
         call erset(310, linum, 0)
c
c   Clear the offending flags when we're in the interactive mode
c
         labbas = .false.
         lconmb = .false.
         lconm2 = .false.
         lconm3 = .false.
         luser1 = .false.
      endif
c
c   Carrier-carrier scattering not specified;
c   Check for conflicting concentration-dependent models.
c
      if(.not.labbas) then
c
c   User defined model not specified, but both analytic and arora
c   are.  Default to arora.
c
         if(.not.luser1.and.(lconm2.and.lconm3)) then
            call erset(-311, linum, 0)
            lconm2 = .false.
c
c   User defined model specified, and one of "arora" and "analytic"
c   are as well.  Default to user model.
c
         else if(luser1.and.(lconm2.or.lconm3)) then
            call erset(-312, linum, 0)
            lconm2 = .false.
            lconm3 = .false.
         endif
      endif
      if (luser1) call initum1
c...Set temperature - if here for a second time, can't change temp.
320   ntemp=temp
      if(isrval(7)) ntemp=gtrval(7)
      if((ntemp.gt.1000.).or.(ntemp.lt.10.)) call erset(35,linum,0)
      if(lmodcd.and.(ntemp.ne.temp)) call erset(53,linum,0)
      temp=ntemp
c
c...Photogeneration
      if (lphgen) THEN
          if(isrval(8)) THEN
            flux = gtrval(8)
          ELSE
            CALL ERSET(304, linum, 0)
          ENDIF
          if(isrval(9)) THEN
            abscof = gtrval(9)
          ELSE
            CALL ERSET(305, linum, 0)
          ENDIF
          if (flux.lt.0.0) then
            flux = -flux
            CALL ERSET(-306, linum, 0)
          ENDIF
          if (abscof.lt.0.0) then
            abscof = -abscof
            CALL ERSET(-307, linum, 0)
          ENDIF
      ENDIF
c
c...Transverse field mobility model
      IF(LTFLDMB) THEN
        call devlts(left,right,top,bottom)
c........get left,right,top, and bottom in microns
        left=left*1.0e4
        right=right*1.0e4
        top=top*1.0e4
        bottom=bottom*1.0e4
        ACCSF=GTRVAL(10)
        INVSF=GTRVAL(11)
        OXL=ANINT(GTRVAL(12)*1.0E4)/1.0E4
        OXR=ANINT(GTRVAL(13)*1.0E4)/1.0E4
        OXB=ANINT(GTRVAL(14)*1.0E4)/1.0E4
        IF(ACCSF.LE.0.OR.ACCSF.GT.1) CALL ERSET(308,LINUM,0)
        IF(INVSF.LE.0.OR.INVSF.GT.1) CALL ERSET(308,LINUM,0)
        IF(OXL.LT.left.OR.OXL.GE.OXR.or.oxl.gt.right)  then
          CALL ERSET(309,LINUM,0)
        endif
        IF(OXR.LT.left.OR.OXR.LE.OXL.or.oxr.gt.right) then
          CALL ERSET(309,LINUM,0)
        endif
        IF(OXB.LT.top.or.oxb.gt.bottom) CALL ERSET(309,LINUM,0)
      ENDIF
c
c...Done - set flag to indicate we've been here
      lmodcd=.true.
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CNTCK
      include 'p2conf.h'
c 
c     This routine gets the work function and Schottky
c     info off the contact card.  Also gets external R,C connected
c     to contact.
c 
c     Original : C.H.Price       Stanford University       May, 1982
c     Modified : MRP             Stanford University       Nov, 1983
c
c     Copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
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
      include     'stat.h'
      include     'symme.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c type declarations 
c 
      integer ilow,ihigh,i
      real walum,wmolyb,wtungs,wmosi2,wwsi2
      logical lneut
c
c FUNCTIONS:
      logical isrval
      real    gtrval
      logical gtlval
c 
c data
c 
      data walum/4.10/,wmolyb/4.53/,wtungs/4.63/,wmosi2/4.80/,
     +     wwsi2/4.80/
c 
c****************************************************************** 
c 
c                             start 
c 
      ilow=1
      ihigh=nelect
c
c...Do all electrodes?
      if(gtlval(5)) goto 10
c
c...No, just 1
      ilow=gtrval(1)
      ihigh=ilow
c
c...In range? 
      if(ilow.ge.1.and.ihigh.le.nelect) goto 10
      call erset(49,linum,ilow) 
      return
c
c...Loop through
10    do 200 i=ilow,ihigh
c
c----------------
c  Workfunction
c----------------
c
c...What contact material? (default to neutral)
c...      1 - Aluminum     
c...      2 - p poly      
c...      3 - n poly       
c...      4 - neutral
c...      6 - Moly         
c...      7 - Tungsten    
c...      8 - Moly disil.  
c...      9 - Tungsten disil.
c
      lneut=.not.(gtlval(1).or.gtlval(2).or.gtlval(3).or.
     +    gtlval(6).or.gtlval(7).or.gtlval(8).or.gtlval(9))
      if(gtlval(1)) workf(i)=walum
      if(gtlval(2)) workf(i)=affin+eg300
      if(gtlval(3)) workf(i)=affin
      if(gtlval(4)) workf(i)=0.
      if(gtlval(6)) workf(i)=wmolyb
      if(gtlval(7)) workf(i)=wtungs
      if(gtlval(8)) workf(i)=wmosi2
      if(gtlval(9)) workf(i)=wwsi2
c
c...Given?
      if(isrval(2)) then
         workf(i)=gtrval(2)
         lneut=.false.
      endif
c
c...Barrier lowering?
      lbarl(i)=gtlval(13)
      if(isrval(8)) barla(i)=gtrval(8)
cc      if(isrval(9)) barlb(i)=gtrval(9)
c
c----------------------------------
c  Surface recombination velocity
c----------------------------------
c
      vsn(i)=gtrval(3)
      vsp(i)=gtrval(4)
      schotk(i)=gtlval(10)
      if(schotk(i)) goto 14
c
c...Non-Schottky contact -
c...Warn if SRV set and bag it
      if((vsn(i).ne.-999.d0).or.(vsp(i).ne.-999.d0)) 
     +   call erset(-273,linum,0)
      vsn(i)=0.
      vsp(i)=0.
      goto 20
c
c...Schottky 
14    if((vsn(i).eq.-999.d0).and.(vsp(i).eq.-999.d0)) goto 15
      if(((vsn(i).ne.-999.d0).and.(vsn(i).lt.0.)).or.
     +   ((vsp(i).ne.-999.d0).and.(vsp(i).lt.0.))) then
         call erset(271,linum,0)
         return
      endif
c
c...If we already have default use it
15    if(lscale) then
         if(vsn(i).eq.-999.d0) vsn(i)=vsurfn
         if(vsp(i).eq.-999.d0) vsp(i)=vsurfp
      endif
c
c----------------
c  External R,C
c----------------
c
20    if((.not.isrval(5)).and.(.not.isrval(6))) goto 30
c
c...For now, complain if symbolic has already been done
      if(lsymdn) then
         call erset(249,linum,0)
         return
      endif
c
c...Get values (turn resistivity into conductivity) and set flags
      if(isrval(5)) cresis(i)=1.d0/gtrval(5)
      if(isrval(6)) ccapac(i)=gtrval(6)
      nlump=nlump+1
      nresis(i)=nlump
      lresis(i)=.true.
c
c...For now, do not allow srv and lumped element at same contact
      if(.not.schotk(i)) goto 200
      call erset(251,linum,0)
      return
c
c----------------------
c  Contact resistance
c----------------------
c
c...Do not allow contact resistance with srv or external
c...resistor (for now)
30    lcnres(i)=isrval(7)
      if(.not.lcnres(i)) goto 40
      if(lresis(i)) call erset(255,linum,0)
      if(schotk(i)) call erset(256,linum,0)
      if(lsymdn.and.(.not.lnewt)) call erset(252,linum,0)
c
c...Convert contact resistance to conductance and scale by 1 micron
c...unless cylindrical coordinates specified
c
      IF (LCYL) THEN
              cresis(i)=1.0/gtrval(7)
      ELSE
              cresis(i)=1.d-4/gtrval(7)
      ENDIF
c
c...Scale contact conductance by width
c
      if (lwidth) then
              cresis(i) = cresis(i)*width
      endif
      goto 200
c
c--------------
c  Current bc
c--------------
c
40    lcurbc(i)=gtlval(11)
      if(.not.lcurbc(i)) goto 50
      nlump=nlump+1
      nresis(i)=nlump
c
c...For now, complain if symbolic has already been done
      if(lsymdn) then
         call erset(249,linum,0)
         return
      endif
c
c--------------
c  Pseudo 1D base contact
c  A kludge to allow pseudo 1D bipolar simulations.
c  THIS KLUDGE HAS BEEN REMOVED! (Hry)
c--------------
c
C 50    l1dbas(i)=lval(12)  Why did pinto do this????  Hry.
 50    continue

c
c...Next electrode
200   continue
c
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MATER
      include 'p2conf.h'
c 
c     This routine checks that all material parameters
c     are completely specified and then computes
c     all temperature dependent constants.
c 
c     Original : C.H.Price       Stanford University       May, 1982
c     Modified : MRP             Stanford University       Nov, 1983
c     Modified : HRY  (incomplete Ioniation)               Jun. 1985
c     Modified : HRY  (low-temp.  based on lgcnie now)     Jun. 1985
c     Modified : C.C.Abbas  (carrier-carrier scattering)   Sep, 1987
c     Modified : G. Anderson (photogeneration)            June, 1989
C     MODIFIED : SHIN            UT                        4/88
C                 CHANGE THE EQUATION FOR TEMPERATURE DEPENDENT
C                 Vsat TO GET Vsat=9.23E6 AT 300 K
C                 (REF: J.APPL.PHYS., 54, P.1445, 1983)
C                 (above applies ONLY when TRFLDMOB mobdel is specified)
c
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
      include     'setup.h'
      include     'impact.h'
      include     'symme.h'
      include     'photo.h'
      include     'trfldmob.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      double precision  dtmp1,othird
      integer           elmat,i,ipt,j
      real              alpha,ccc,cdc,eac,ebc,etemp,expu,fnii,hac,hbc,
     &                  temp2,temp32,tp
c 
c***********************************************************************
c
c                    data section
c
      data     alpha,ccc,   cdc,  eac,    ebc,    hac,   hbc,
     &         othird
     &        /-2.20,2.0e17,8.28e8,4.61e17,1.52d15,1.0e17,6.25e14,
     &         0.3333333333333333/
c 
c***********************************************************************
c 
c****************************************************************** 
c 
c                   start 
c 
c...Every material has permitt. specified? 
      do 100 i=1,nmat 
      if(epsmat(i).eq.0.) call erset(39,linum,i) 
100   continue
c
c...Check all semiconductor param.
      if(eg300.eq.0.) call erset(41,linum,i) 
      if((fmun0.eq.0.).or.(fmup0.eq.0.)) call erset(42,linum,i)
      if((taup0.eq.0..or.taun0.eq.0.).and.lsrh) call erset(43,linum,i)
      if(errflg) return
c
C--------------------------------  
C  Globally  dependent quantities  
C--------------------------------  
c...Compute energy gap and intrinsic conc.
      egap=eg300+egalph*((9.e4/(300.+egbeta))-(temp*temp/(temp+egbeta)))
      tp=(temp/300.)**1.5
      ncband=nc300*tp
      nvband=nv300*tp
      dktq=dble(boltzk)*dble(temp)
      dqkt=1.d0/dktq
      ktq=sngl(dktq)
      qkt=sngl(dqkt)
c
c...Intrinsic conc. (and variations on that theme)
      logni0=dlog(dsqrt(dble(ncband))*dsqrt(dble(nvband)) ) -
     +              dble(egap)*0.5d0*dqkt
c
c..   Things are now geared off of logni0 to allow low temp.
c..   Calculating cnie is used for recombination.
c     
      if (logni0 .GT.  29.01D0) then
        dni = 3.970844d12
      else if (logni0 .LT. -29.01D0) then
        dni = 2.518355d-13
      else 
        dni = dexp(logni0)
      endif
c
      dnii=1.d0/dni
      dnisq=dni*dni
      dniisq=dnii*dnii
      dni3=dnisq*dni
      fni=sngl(dni)
      fnii=sngl(dnii)
c
c...New concentration scaling for low temperature (hry)
      dcscl = dsqrt(dble(ncband))*dsqrt(dble(nvband))
      dcscl = dsqrt(dcscl)
      
cCCC  dcscl = dni
      dcscli= 1.0D0/dcscl
c
c...Coefficients for Incomplete Ionization of Dopants. (hry)
      if (lincom) then
        gcstar=dlog(gcband*exp(edband/dktq))
        gvstar=dlog(gvband*exp(eaband/dktq))
      else
        gcstar=-lgmaxd
        gvstar=-lgmaxd
      endif
c
c...Recombination limits (to prevent overflow)
      rcmax=(maxdbl*dcscli)*dcscli
      rcmax2=(maxdbl/(dcscl))/(dcscl*dcscl)
c
c...Surface recombination velocity - set any contacts which were
c...specified as Schottky and the default srv were desired
      etemp=temp*temp/qcharg
      vsurfn=arichn*etemp/ncband
      vsurfp=arichp*etemp/nvband
      do 333 i=1,nelect
      if(vsn(i).lt.0.) vsn(i)=vsurfn
      if(vsp(i).lt.0.) vsp(i)=vsurfp
333   continue
c
c...Barrier lowering coeff
c...(must be divided by sqrt(e/q) somewhere else)
      do 335 i=1,nelect
      barlb(i)=dsqrt(0.25d0/3.1415927d0)
335   continue
c
c-----------
c  Scaling
c-----------
c
c...Set scaling flag
      lscale = .true.
c
c...Band stuff
      affin=affin*qkt
      egap=egap*qkt
      ncband=ncband*sngl(dcscli)
      nvband=nvband*sngl(dcscli)
      ngap=sngl(dlog(dble(ncband*dcscl))-logni0)
      pgap=egap-ngap
c
C--------------------------------  
C  Spatially dependent quantities  
C--------------------------------  
c...Doping/lifetime scaling 
      do 82 i=1,np
      ndconc(i)=ndconc(i)*sngl(dcscli)
      naconc(i)=naconc(i)*sngl(dcscli)
   82 r1(i)=r1(i)*sngl(dcscli)
      taun0=taun0*ktq
      taup0=taup0*ktq
c
c...Scale workfn,vsn,vsp
      do 83 i=1,nelect
      workf(i)=workf(i)*qkt
      vsn(i)=vsn(i)*qkt
      vsp(i)=vsp(i)*qkt
   83 continue
c
c...Scale cnau,cpau and get trap levels
      dtmp1=dqkt*dcscl*dcscl
      cnau=cnau*dtmp1
      cpau=cpau*dtmp1
      etrapn=dble(etrap*qkt)
      etrapp=-dble(etrap*qkt)
c
c...Photogeneration:  Scale photon flux so that when multiplied by the
c   absorption coefficient, the generation rate is properly scaled.
c
      flux = flux*SNGL(dcscli)*qkt
c
c...Temperature dependence of the lattice scattering dependent
c...mobility and of vsat
c
      if(stype.eq.1) then
c 
c-----------------------------------------------------------------------
c 
c                   lattice scattering dep. mobility &
c                   constants for the full mobility model for Si
c        
         if(labbas) then
            muln=1500.0*((temp/300.0)**alpha)
            dmucon=(1.025d0*1.43d0/(1.68d0*1.68d0))*muln*muln
            mulp= 450.0*((temp/300.0)**alpha)
            dmucop=(1.025d0*1.43d0/(1.68d0*1.68d0))*mulp*mulp
            temp2=temp*temp
            temp32=temp*sqrt(temp)
            cc=ccc*temp32/dcscl
            cd=cdc*temp2/(dcscl**(2.0d0*othird))
            ea=eac*temp32
            eb=ebc*temp2
            ha=hac*temp32
            hb=hbc*temp2
         endif
c        
c
c...Si coef's for field-dependency
c...TFLDMOB Model from UT uses different coefficients...
c
         if (vsat.le.0.)  then
            if (LTFLDMB) then
                vsat=2.1404e7/(1.+0.8*expu(temp/600.))
         else
                vsat=2.4e7/(1.+0.8*expu(temp/600.))
            endif
         endif
         muco3=1./muco1
         muco4=1./muco2
c
c...GaAs coef's for field-dependency
c...  muco1     : 1/E0 (see Barnes, et.al.)
c...  muco2,3,4 : no physical significance
c...                (each saves one multiply in d(mu)/dE)
      else if(stype.eq.2) then
         if(vsat.le.0.) vsat=1.13e7-1.2e4*temp
         muco1=1./muco1
         muco2=vsat*muco1
         muco3=3.*muco2*muco1
         muco4=4.*muco1
      endif
c
C--------------------------------------------------------
C    Set internal variables here. (Coupling Coefficients)              
C--------------------------------------------------------
c...Flux/current scale factors
      depsc=dktq*dcscli
      IF (LCYL) THEN
         decoef=-qcharg*dcscl
      ELSE
         decoef=-qcharg*1.d-4*dcscl
      ENDIF
      if (lwidth) then
         decoef=decoef*width
      endif
      djcoef=decoef*dktq
c
c...Scale resistors and capacitors
      do 884 i=1,nelect
      cresis(i)=cresis(i)/decoef
      ccapac(i)=ccapac(i)/decoef
884   continue
c
c...Scale ehed vector and eps
c...(Poisson coupling coefficients)
      do 84 i=1,nmat
   84 epsmat(i)=epsmat(i)*depsc

      do 885 i=1,ne 
         elmat=imat(i)
         etemp=epsmat(elmat)
         do 85 j=1,3
            ipt=nop(j,i)
            ehed(j,i)=ehed(j,i)*etemp
   85    continue
  885 continue
c
c  CONC DEP PARAMETERS
c
c...Get mobility, bgn, lifetime
      call condep
c
c...Interface parameters
      call setqss
c
c...Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SETPR
      include 'p2conf.h'
c 
c     This routine prints out the constants, variables 
c     and flags used in the setup.
c 
c     Original : C.H.Price      Stanford University       May, 1982
c     Revision : MRP            Stanford University       Nov, 1983
c     Revision : C.C.Abbas  (carrier-carrier scattering)  Sep, 1987
c     Revision : A. Yabuta  (impact ionization)           Oct, 1987
c     Revision : G. Anderson (photogeneration, cylindrical) Aug, 1989
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
      include     'setup.h'
      include     'logunt.h'
      include     'impact.h'
      include     'photo.h'
      include     'symme.h'
      include     'trfldmob.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      integer i
      real epstmp(MAXREG),epstm2 
      double precision tmp,tmp2
      character*15 cmeth,cgumm,cnewt,cbli,ccarr,chole,celec,cblnk
      data cnewt,cgumm,cbli/'Newton method  ','Gummel method  ',
     +                      'Block-iterative'/
      data chole,celec,cblnk/'Holes          ','Electrons      ',
     +                       '               '/
c 
c****************************************************************** 
c 
c                   start 
c 
      cmeth=cbli
      if(lnewt.or.(ncarr.eq.0)) cmeth=cnewt
      if(lgumm.and.(ncarr.ne.0)) cmeth=cgumm
      ccarr=cblnk
      if(ncarr.eq.1) then
        ccarr=celec
        if(l1hole) ccarr=chole
      endif
      write(luout,1000) cmeth,ncarr,ccarr
1000  format(1x,//a15,'    carriers =',i2,4x,a10)
      write(luout,1010) boltzk,qcharg,eps0,temp,ktq
1010  format(//' Constants :'/
     +       '   Boltzmanns k =',1pe12.5,/ 
     +       '   charge       =',1pe12.5,/ 
     +       '   permittivity =',1pe12.5,// 
     +       ' Temperature     =',0pf5.0,/ 
     +       ' Thermal voltage =',0pf8.6) 
c
c...epsmat actually contains eps0*epsr q, get epsr
      tmp=qcharg/(eps0*depsc)
      do 100 i=1,nmat 
      epstmp(i)=epsmat(i)*tmp
100   continue
      epstm2=epsoq*qcharg/eps0
      write(luout,1020) (i,mattyp(i),epstmp(i),i=1,nmat)
1020  format(//,' Material data :',/
     +       '  Num  Type  Rel Permit',/
     +       8(i5,i6,f9.2,/)) 
c 
      tmp=dktq/(dcscl*dcscl)
      tmp2=dqkt
      write(luout,1030) stype,epstm2,logni0/2.30258d0,fmun0,fmup0,vsat,
     +                  taup0*qkt,taun0*qkt,
     +                  (semmat(i),i=9,11),affin*dktq,
     +                  egap*dktq,cnau*tmp,cpau*tmp,arichn,arichp,
     +                  ncband*dcscl,nvband*dcscl
1030  format(//,' Semiconductor data :',/ 
     +       '   type        =',1pg12.5,/ 
     +       '   rel permit  =',1pg12.5,/ 
     +       '   log10(ni)   =',1pg12.5,/ 
     +       '   n-mobility  =',1pg12.5,/ 
     +       '   p-mobility  =',1pg12.5,/ 
     +       '   vsat        =',1pg12.5,/ 
     +       '   taup0       =',1pg12.5,/ 
     +       '   taun0       =',1pg12.5,/ 
     +       '   egap(300)   =',1pg12.5,/ 
     +       '   egalpha     =',1pg12.5,/ 
     +       '   egbeta      =',1pg12.5,/ 
     +       '   affinity    =',1pg12.5,/ 
     +       '   egap        =',1pg12.5,/
     +       '   cnau        =',1pg12.5,/
     +       '   cpau        =',1pg12.5,/
     +       '   An**        =',1pg12.5,/
     +       '   Ap**        =',1pg12.5,/
     +       '   Nc          =',1pg12.5,/
     +       '   Nv          =',1pg12.5)

c..  God! I don`t believe I have to do this! F77 under BSD4.2 appearantly
c..  can only handle 40 items in a format statement. God, the fortran legacy
c..  lives on.
      write(luout,1031) gcband,gvband,edband,eaband
1031  format(
     +       '   gcb         =',1pg12.5,/
     +       '   gvb         =',1pg12.5,/
     +       '   edb         =',1pg12.5,/
     +       '   eab         =',1pg12.5)
c 
c...Contacts
      write(luout,8002)
8002  format(//' Contacts :',/
     +       '  Num',3x,'Work fn.',5x,'Vsurfn',9x,'Vsurfp',7x,
     +       'Resistance',4x,'Capacitance')
      do 822 i=1,nelect
      if(lcnres(i)) then
         write(luout,8322) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       1.d0/(decoef*cresis(i)),decoef*ccapac(i)
      else if(cresis(i).ne.0.d0) then
         write(luout,8222) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       1.d0/(decoef*cresis(i)),decoef*ccapac(i)
      else
         write(luout,8222) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       0.d0,decoef*ccapac(i)
      endif
8322  format(i4,0pf10.3,1x,3(1pe15.6),'D',1pe14.6)
8222  format(i4,0pf10.3,1x,4(1pe15.6))
822   continue
c
      write(luout,1050) lsrh,lconlt,lauger,lbgn,lconmb,lfldmb,labbas,
     +                  lconm2,lconm3,LTFLDMB, 
     +                  limpct,lcyl,lphgen
1050  format(//' Model flags :'/
     +       '  SRH recombination   =',l2/,'  Conc. dep. lifetime =',l2/
     +       '  Auger recombination =',l2/,
     +       '  Band-gap narrowing  =',l2/,'  Conc. dep. mobility =',l2/
     +       '  Field dep. mobility =',l2/,'  CCS   dep. mobility =',l2/
     +       '  Analytic mobility   =',l2/,
     +       '  Arora mobility      =',l2/,'  Trans. E.  mobility =',l2/
     +       '  Impact Ionization   =',l2/,'  Cylindrical coords  =',l2/
     +       '  Photogeneration     =',l2///)

c  PHOTOGENERATION
      if (lphgen) then
      WRITE (luout, 1055) abscof, flux*SNGL(dcscl)*ktq
1055      FORMAT(' Photogeneration model parameters :'/
     +      '  Absorption coefficient = ', 1pg13.6, ' 1/cm'/
     +      '  Photon flux            = ', 1pg13.6, ' 1/(cm^2 sec)'//)
      end if

c
      write(luout,1065) mudeg
1065  format(' Mobility model parameters :'/
     +          '  Gsurf   = ',1pe13.6)
      if(lfldmb) then
         if(stype.eq.1) then
            write(luout,1066) muco1,muco2
1066        format('  B.elect = ',1pe13.6/
     +             '  B.hole  = ',1pe13.6///)
         else if(stype.eq.2) then
            write(luout,1067) muco1
1067        format('  E0      = ',1pe13.6///)
         endif
      else
         write(luout,1069) ' '
1069     format(a1///)
      endif
c
      if(lconlt) then
         write(luout,1068) nsrhn,nsrhp
1068     format(' Lifetime model parameters :'/
     +          '  Nsrhn   = ',1pe13.6/
     +          '  Nsrhp   = ',1pe13.6///)
      endif
c 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE CONDEP
      include 'p2conf.h'
c
c     The following subroutine calculates the low-field mobility
c     effective ni due to bgn at each grid point 
c     based on local concentration.  As of now, only mobilities of 
c     Si and GaAs have been implemented.
c
c     Original: MRP        Stanford University                Feb, 1984
c     Revision: HRY        (low temp. based on lgcnie now)    Jun. 1985
c     Revision: C.C.Abbas  (carrier-carrier scattering, IMOB) Sep, 1987
C     MODIFIED:  SHIN UT (SET MOBILITY=0 FOR INITIAL)   11/87
c 
c     Copyright c 1984 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Common
c
      include     'blank.h'
      include     'setup.h'
      include     'impact.h'
      include     'trfldmob.h'
      include     'emaco.h'
c------------------------------------------------------------------
c***********************************************************************
c 
c                   type declarations 
c 
      double precision  dexpu,dtmp,lgdcsc
      integer           i,ndmat
      real              usrmob1,amob2,amob,conc1,gmob,imob,rmob
c------------------------------------------------------------------
c
c*** Start ***
c
      lgdcsc = dlog(dcscl)
c..   
c...Loop through all nodes -
c...Remember, total conc has NOT been scaled
      do 100 i=1,np 
      ndmat=mattyp(itype(i))
C...SHIN      if(ndmat.le.0) goto 100
      if(ndmat.le.0) THEN
      MOBN(I)=0.0
      MOBP(I)=0.0
      goto 100
      ENDIF
      conc1=tconc(i)
c
c------------
c  Lifetime
c------------
c
c...Put conc. dep lifetime in arrays taun, taup
c...(we look later at whether to use it or not)
      taun(i)=taun0*nsrhn/(nsrhn+conc1)
      taup(i)=taup0*nsrhp/(nsrhp+conc1)
c
cMARK
c      tmp=(1.-expu(-1.5e4*cord(2,i))+.01)
c      taun(i)=taun(i)*tmp
c      taup(i)=taup(i)*tmp
c
c------------
c  Mobility
c------------
c
      if(ndmat.gt.2) goto 30
      if(ndmat.eq.2) goto 10
c
c...Si
      if(labbas) then
         mobn(i)=imob(.false.,conc1)
         mobp(i)=imob(.true.,conc1)
      else
         if(lconm2) then
            mobn(i)=amob(.false.,conc1,temp)
            mobp(i)=amob(.true.,conc1,temp)
         else if(lconm3) then
            mobn(i)=amob2(.false.,conc1,temp)
            mobp(i)=amob2(.true.,conc1,temp)
         else if(luser1) then
            mobn(i)=usrmob1(.false.,conc1,temp)
            mobp(i)=usrmob1(.true.,conc1,temp)
         else
            mobn(i)=rmob(.false.,conc1)
            mobp(i)=rmob(.true. ,conc1)
         endif
      endif
      goto 50
c
c...GaAs
10    mobn(i)=gmob(.false.,conc1)
      mobp(i)=gmob(.true. ,conc1)
      goto 50
c
c...We dont have it! - set to constant
30    mobn(i)=fmun0
      mobp(i)=fmup0
c
c----------------------
c  Band-gap narrowing
c----------------------
c
c...  Remember, net concentration (r1) has been scaled by dcscl.
c..   Everything should key off of lgcnie() if it can, because of low temp.
c
  50  lgcnie(i)=(logni0-lgdcsc)
      if(.not.lbgn.or.r1(i).eq.0.) goto 90
c
c...Use Slotboom relationship (modified for new scaling. hry)
      dtmp=dlog(dabs(dble(r1(i)))*dcscl/1.d17)
      dtmp=0.5d0*.009d0*dqkt*(dtmp+dsqrt(dtmp*dtmp+.5d0))
      lgcnie(i)=(dtmp+logni0-lgdcsc)
c
  90  if (lgcnie(i).GT.lgmaxd) then
         cnie(i) = sngl(maxdbl)
      else if (lgcnie(i).LT.lgmind) then
         cnie(i) = sngl(mindbl)
      else
         cnie(i) = sngl(dexpu(lgcnie(i)))
      endif
c
100   continue
c
c...Bye
      return
      end 
