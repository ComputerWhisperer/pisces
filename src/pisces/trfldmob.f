cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun May  6 20:41:07 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE TRFLDMOB(n1,n2,e21,dedv,lmup,ldmu,mu21,dfdv,nmu,nmui,
     +                    elmat)
      include 'p2conf.h'
c
C     SHIN           UT                        4/88
C             ADD TRANSVERSE E FIELD DEPENDENT MOBILITY MODEL
C               USING SCHWARZ AND RUSSEK EQ.
C              (REF: ED-36, P.1117, 1989)
C
C                    UT                        6/89
C             ADD TRANSVERSE E FIELD DEPENDENT MOBILITY MODEL
C               USING NEW EQ.
C             
c
c     G. Anderson  (separated code into this subroutine)   11/89
c
c     Copyright c 1989 The board of trustees of the Leland Stanford
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
c
c#include       "blank.h"
      include        'setup.h'
      include        'trfldmob.h'
      include        'emaco.h'
c
c******************************************************************
c
c                   parameter declarations
c
      integer n2,n1,elmat
      double precision e21,dedv,mu21,dfdv
      logical lmup,ldmu
c
c                   type declarations
c
      real nmu,nmui
      double precision t1,t2,t3,t4,dvs
c
      REAL TMU1,TMU2,ET,X,Y,ESAT,E01,E02
      REAL Z,P,DZ,DU,GAMMA,MUB,MUPH,MUSR,MUC,DMUPH,DMUSR
      DOUBLE PRECISION T11,T12,T13,T14,T21,T22,T23,T24,T
      DOUBLE PRECISION MU1,MU2,DMU1DE,DMU2DE
C
c
C...FOR HOLE
       if(lmup) then
C...FOR N1
          ET=0
C..IF THIS NODE IS DONE BEFORE (BY ANOTHER ELEMENT), USE STORED VALUE
          IF(TMOBP(1,N1).NE.0.0) THEN
            TMU1=TMOBP(1,N1)
          ELSE
              X=ANINT(CORD(1,N1)*1.0E8)/1.0E4
              Y=ANINT(CORD(2,N1)*1.0E8)/1.0E4
C
CCC  FOR SIMPLE ACCUM RF
            IF(R1(N1).LT.0.0) THEN
              IF(DINTF(N1).NE.0.0) THEN
                TMU1=MOBP(N1)*ACCSF
              ELSE
                TMU1=MOBP(N1)
              ENDIF
              TMOBP(1,N1)=TMU1
              GOTO 111
C
C  INVERSION LAYER
C..IF p<Nd, rf=1
            ELSE IF(FP(N1).LT.ABS(R1(N1))) THEN
              ET=0
CCC
C..RF=1 FOR NODES LOCATED MORE THAN 500A APART FROM OXIDE
            ELSE IF((OXL-X).GT.0.0.OR.(X-OXR).GT.0.0.OR.
     +              (Y-OXB).GT.0.05) THEN
              ET=0
C
C..RF=1 FOR NODES WITH E0>0.0 OR EY>0.0
            ELSE IF(E0(N1).GE.0.0.OR.TE(2,N1).GE.0.0) THEN
              ET=0
            ELSE
              ET=TE(2,N1)
            ENDIF
C...RF=1 FOR ET=0 EXCEPT THE INTERFACE NODE
            IF(ET.EQ.0.0) THEN
              TMU1=MOBP(N1)
              IF(R1(N1).GT.0.0.AND.DINTF(N1).NE.0.0.AND.
     +            TE(2,N1).LT.0.0) TMU1=TMU1*INVSF
            ELSE
              ET=ABS(ET)
C...ACCUMULATION ** ACTUALLY WE SKIP THIS ACCUM PART ***
              IF(R1(N1).LE.0.0) THEN
                ESAT=10**((2408.3/MOBP(N1)-ACCSF)/(348.4138/
     +                     MOBP(N1)))
                IF(ET.LE.(2*ESAT)) THEN
                  TMU1=MOBP(N1)*ACCSF
                ELSE
                  TMU1=2408.3-348.4138*ALOG10(ET/2)-348.4138
     +                   *0.434294482
C...LOGe=0.434294482
                ENDIF
              ELSE
C...INVERSION
                LTFM(N1)=.TRUE.
                E01=ABS(E0(N1))
                IF(INTFCHG.LE.0.0) THEN
                  WRITE(6,*) ' **** INCORRECT FIXED',
     +                         ' OXIDE CHARGE ****'
c                  WRITE(19,*) ' **** INCORRECT FIXED',
c     +                         ' OXIDE CHARGE ****'
                ENDIF
        T=TEMP/300.
C...MODIFIED SCHWARZ WITH Ni (OLD)
C                IF(LOTFL) THEN
C        Z=0.039*T*3./(ET+2*E01)+1.7E-5/((ET+2*E01)/3.)**(0.44)
C        P=0.09*T**1.5+1.5E-8/(FP(N1)*SNGL(DCSCL))**0.25/T*INTFCHG
C        TMU1=275.*Z/T**2.5/(Z+275.*2.34E-9*P/T**2)
C        DZ=-0.039*T*3/(ET+2*E01)**2-0.44*1.7E-5*3**(0.44)
C     +      /(ET+2*E01)**(1+0.44)
C        DU=(275./T**2.5*DZ*(Z+275.*2.34E-9/T**2*P)-275./T**2.5*Z
C     +      *DZ)/(Z+275.*2.34E-9/T**2*P)**2
C        TMOBP(2,N1)=(ET-E01)*DU
C                ENDIF
C....SIMPLE EQUATION FOR WATT AND PLUMMER'S DATA
                 IF(LOTFL) THEN
          TMU1=68.0*1.0E6**0.44*3.0**0.44/(ET+2*E01)**0.44
          TMOBP(2,N1)=(ET-E01)*(-0.44)*68.0*1.0E6**0.44*3.0**0.44
     +                /(ET+2*E01)**1.44
C...NEW SEMI-EMPIRICAL MODEL WE DERIVED
                ELSE
        Z=0.039*T*3./(ET+2.*E01)+1.51E-5/((ET+2.*E01)/3.)**(1./3.)
        P=0.334*T**1.5+3.14E-7/(FP(N1)*SNGL(DCSCL))**0.3/T*INTFCHG
        GAMMA=8.4E16/(FP(N1)*SNGL(DCSCL))*T**(-3.4)
        MUB=270.0/T**1.4
        MUPH=Z/(2.35E-9*P*T**0.5)
        MUSR=1E8/((ET+2.*E01)/3.)
        MUC=1.4E18*T**1.5/(ABS(R1(N1))*SNGL(DCSCL)*
     +    (ALOG(1+GAMMA)-GAMMA/(1+GAMMA)))
        TMU1=1.0/(1.0/MUB+1.0/MUPH+1.0/MUSR+1.0/MUC)
        DZ=-0.039*T*3/(ET+2.*E01)**2.0-1/3.*1.51E-5*3**(1./3.)
     +      /(ET+2.*E01)**(4./3.)
        DMUPH=DZ/(2.35E-9*P*T**0.5)
        DMUSR=-3.0*1E8/(ET+2.*E01)**2.0
        DU=TMU1**2*(DMUPH/MUPH**2.0+DMUSR/MUSR**2.0)
        TMOBP(2,N1)=(ET-E01)*DU
C        IF(N1.EQ.763) THEN
C         write(19,*) ' ET,E0,N,n,QF,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU1',
C     +  ', DZ,DMUPH,DMUSR,DU,TMOBP'
C         write(19,*)  ET,E01,ABS(R1(N1))*SNGL(DCSCL),FP(N1)*SNGL(DCSCL),
C     +   INTFCHG,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU1,
C     +   DZ,DMUPH,DMUSR,DU,TMOBP(2,N1)
C         ENDIF
                  ENDIF
C
              ENDIF
            ENDIF
            TMOBP(1,N1)=TMU1
          ENDIF
C..FOR N2
111       CONTINUE
          ET=0
C..IF THIS NODE IS DONE BEFORE (BY ANOTHER ELEMENT), USE STORED VALUE
          IF(TMOBP(1,N2).NE.0.0) THEN
            TMU2=TMOBP(1,N2)
          ELSE
              X=ANINT(CORD(1,N2)*1.0E8)/1.0E4
              Y=ANINT(CORD(2,N2)*1.0E8)/1.0E4
C
CCC  FOR SIMPLE ACCUM RF
            IF(R1(N2).LT.0.0) THEN
              IF(DINTF(N2).NE.0.0) THEN
                TMU2=MOBP(N2)*ACCSF
              ELSE
                TMU2=MOBP(N2)
              ENDIF
              TMOBP(1,N2)=TMU2
              MU21=(TMU1+TMU2+TMOBP(2,N1)+TMOBP(2,N2))*0.5
              GOTO 114
C
C  INVERSION
C  IF p<Nd, RF=1
            ELSE IF(FP(N2).LT.ABS(R1(N2))) THEN
              ET=0
CCC
C..RF=1 FOR NODES LOCATED MORE THAN 500A APART FROM OXIDE
            ELSE IF((OXL-X).GT.0.0.OR.(X-OXR).GT.0.0.OR.
     +              (Y-OXB).GT.0.05) THEN
              ET=0
C
C..RF=1 FOR NODES WITH E0>0.0 OR EY>0.0
            ELSE IF(E0(N2).GE.0.0.OR.TE(2,N2).GE.0.0) THEN
              ET=0
            ELSE
              ET=TE(2,N2)
            ENDIF
C...RF=1 FOR ET=0 EXCEPT THE INTERFACE NODE
            IF(ET.EQ.0.0) THEN
              TMU2=MOBP(N2)
              IF(R1(N2).GT.0.0.AND.DINTF(N2).NE.0.0.AND.
     +            TE(2,N2).LT.0.0) TMU2=TMU2*INVSF
            ELSE
              ET=ABS(ET)
C...ACCUMULATION ** ACTUALLY WE SKIP THIS ACCUM PART ***
              IF(R1(N1).LE.0.0) THEN
                ESAT=10**((2408.3/MOBN(N2)-ACCSF)/(348.4138/
     +                     MOBN(N2)))
                IF(ET.LE.(2*ESAT)) THEN
                  TMU2=MOBN(N2)*ACCSF
                ELSE
                  TMU2=2408.3-348.4138*ALOG10(ET/2)-348.4138
     +                   *0.434294482
C...LOGe=0.434294482
                ENDIF
              ELSE
C...INVERSION
                LTFM(N2)=.TRUE.                
                E02=ABS(E0(N2))
                IF(INTFCHG.LE.0.0) THEN
                  WRITE(6,*) ' **** INCORRECT FIXED',
     +                         ' OXIDE CHARGE ****'
c                  WRITE(19,*) ' **** INCORRECT FIXED',
c     +                         ' OXIDE CHARGE ****'
                ENDIF
        T=TEMP/300.
C...MODIFIED SCHWARZ WITH Ni (OLD)
C                IF(LOTFL) THEN
C        Z=0.039*T*3./(ET+2*E02)+1.7E-5/((ET+2*E02)/3.)**(0.44)
C        P=0.09*T**1.5+1.5E-8/(FP(N2)*SNGL(DCSCL))**0.25/T*INTFCHG
C        TMU2=275.*Z/T**2.5/(Z+275.*2.34E-9*P/T**2)
C        DZ=-0.039*T*3/(ET+2*E02)**2-0.44*1.7E-5*3**(0.44)
C     +      /(ET+2*E02)**(1+0.44)
C        DU=(275./T**2.5*DZ*(Z+275*2.34E-9/T**2*P)-275/T**2.5*Z
C     +      *DZ)/(Z+275*2.34E-9/T**2*P)**2
C        TMOBP(2,N2)=(ET-E02)*DU
C                ENDIF               
C....SIMPLE EQUATION FOR WATT AND PLUMMER'S DATA
                 IF(LOTFL) THEN
          TMU2=68.0*1.0E6**0.44*3.0**0.44/(ET+2*E02)**0.44
          TMOBP(2,N2)=(ET-E02)*(-0.44)*68.0*1.0E6**0.44*3.0**0.44
     +                /(ET+2*E02)**1.44
C...NEW SEMI-EMPIRICAL MODEL WE DERIVED
                ELSE
        Z=0.039*T*3./(ET+2.*E02)+1.51E-5/((ET+2.*E02)/3.)**(1./3.)
        P=0.334*T**1.5+3.14E-7/(FP(N2)*SNGL(DCSCL))**0.3/T*INTFCHG
        GAMMA=8.4E16/(FP(N2)*SNGL(DCSCL))*T**(-3.4)
        MUB=270.0/T**1.4
        MUPH=Z/(2.35E-9*P*T**0.5)
        MUSR=1E8/((ET+2.*E02)/3.)
        MUC=1.4E18*T**1.5/(ABS(R1(N2))*SNGL(DCSCL)*
     +    (ALOG(1+GAMMA)-GAMMA/(1+GAMMA)))
        TMU2=1.0/(1.0/MUB+1.0/MUPH+1.0/MUSR+1.0/MUC)
        DZ=-0.039*T*3/(ET+2.*E02)**2.0-1/3.*1.51E-5*3**(1./3.)
     +      /(ET+2.*E02)**(4./3.)
        DMUPH=DZ/(2.35E-9*P*T**0.5)
        DMUSR=-3.0*1E8/(ET+2.*E02)**2.0
        DU=TMU2**2*(DMUPH/MUPH**2.0+DMUSR/MUSR**2.0)
        TMOBP(2,N2)=(ET-E02)*DU
C        IF(N2.EQ.763) THEN
C         write(19,*) ' ET,E0,N,n,QF,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU2',
C     +  ', DZ,DMUPH,DMUSR,DU,TMOBP'
C         write(19,*)  ET,E02,ABS(R1(N2))*SNGL(DCSCL),FP(N2)*SNGL(DCSCL),
C     +   INTFCHG,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU2,
C     +   DZ,DMUPH,DMUSR,DU,TMOBP(2,N2)
C         ENDIF
                  ENDIF
C
C
              ENDIF
            ENDIF
            TMOBP(1,N2)=TMU2
          ENDIF
          MU21=(TMU1+TMU2+TMOBP(2,N1)+TMOBP(2,N2))*0.5
c
       else
c
C...FOR ELECTRON
c
C...FOR N1
      ET=0
C..IF THIS NODE IS DONE BEFORE (BY ANOTHER ELEMENT), USE STORED VALUE
      IF(TMOBN(1,N1).NE.0.0) THEN
        TMU1=TMOBN(1,N1)
      ELSE
        X=ANINT(CORD(1,N1)*1.0E8)/1.0E4
        Y=ANINT(CORD(2,N1)*1.0E8)/1.0E4
C
C..IF N IS LESS THAN Ni OR IF N IS LESS THAN 1.001*DOP FOR ACCUMULATION
C    REGION, RF=1
CCC            IF(R1(N1).GT.0.0.AND.FN(N1).LT.1.001*R1(N1)) THEN
CCC              ET=0
C
CCC  FOR SIMPLE ACCUM RF
        IF(R1(N1).GT.0.0) THEN
          IF(DINTF(N1).NE.0.0) THEN
            TMU1=MOBN(N1)*ACCSF
          ELSE
            TMU1=MOBN(N1)
          ENDIF
          TMOBN(1,N1)=TMU1
          GOTO 113
C
C  INVERSION LAYER
C..IF n<Na, rf=1
        ELSE IF(FN(N1).LT.ABS(R1(N1))) THEN
          ET=0
CCC
C..RF=1 FOR NODES LOCATED MORE THAN 300A APART FROM OXIDE
        ELSE IF((OXL-X).GT.0.0.OR.(X-OXR).GT.0.0.OR.
     +              (Y-OXB).GT.0.03) THEN
          ET=0
C
C..RF=1 FOR NODES WITH E0<0.0 OR EY<0.0
        ELSE IF(E0(N1).LE.0.0.OR.TE(2,N1).LE.0.0) THEN
          ET=0
        ELSE
          ET=TE(2,N1)
        ENDIF
C...RF=1 FOR ET=0 EXCEPT THE INTERFACE NODE
        IF(ET.EQ.0.0) THEN
          TMU1=MOBN(N1)
          IF(R1(N1).LT.0.0.AND.DINTF(N1).NE.0.0.AND.
     +            TE(2,N1).GT.0.0) TMU1=TMU1*INVSF
        ELSE
          ET=ABS(ET)
C...ACCUMULATION ** ACTUALLY WE SKIP THIS ACCUM PART ***
          IF(R1(N1).GE.0.0) THEN
            ESAT=10**((2408.3/MOBN(N1)-ACCSF)/(348.4138/
     +                     MOBN(N1)))
            IF(ET.LE.(2*ESAT)) THEN
              TMU1=MOBN(N1)*ACCSF
            ELSE
              TMU1=2408.3-348.4138*ALOG10(ET/2)-348.4138
     +                   *0.434294482
C...LOGe=0.434294482
            ENDIF
          ELSE
C...INVERSION
            LTFM(N1)=.TRUE.
            E01=E0(N1)
            IF(INTFCHG.LE.0.0) THEN
              WRITE(6,*) ' **** INCORRECT FIXED',
     +                         ' OXIDE CHARGE ****'
c              WRITE(19,*) ' **** INCORRECT FIXED',
c     +                         ' OXIDE CHARGE ****'
            ENDIF
C...SHIN    SCHWARZ EQUATION
C...        IF ET=E01, THIS EQ. BLOW-UP. AND SCHWARZ EQ. HAS MAXIMUM
C           WHEN E>E01. SO USE E=E01+3000 FOR MINIMUM
C                IF(ET.LE.E01+3000.) ET=E01+3000.
            T=TEMP/300.
C...SCHWARZ WITH Ni (OLD)
            IF(LOTFL) THEN
              Z=0.039*T*2./(ET+E01)+1.24E-5/((ET+E01)/2.)**(1./3.)
              P=0.09*T**1.5+1.5E-8/(FN(N1)*SNGL(DCSCL))**0.25/T*INTFCHG
              TMU1=1150.*Z/T**2.5/(Z+1150.*3.2E-9*P/T**2)
              DZ=-0.039*T*2/(ET+E01)**2-1/3.*1.24E-5*2**(1./3.)
     +      /(ET+E01)**(4./3.)
              DU=(1150./T**2.5*DZ*(Z+1150.*3.2E-9/T**2*P)-1150./T**2.5*Z
     +      *DZ)/(Z+1150.*3.2E-9/T**2*P)**2
              TMOBN(2,N1)=(ET-E01)*DU
C...NEW SEMI-EMPIRICAL MODEL WE DERIVED
            ELSE
              Z=0.0388*T*2./(ET+E01)+1.73E-5/((ET+E01)/2.)**(1./3.)
              P=0.09*T**1.75+4.53E-8/(FN(N1)*SNGL(DCSCL))**0.25
     +    /T*INTFCHG
              GAMMA=2.0E19/(FN(N1)*SNGL(DCSCL))*T**2.0
              MUB=1150.0/T**2.5
              MUPH=Z/(3.2E-9*P*T**0.5)
              MUSR=6E14/((ET+E01)/2.)**2.0
              MUC=1.1E21*T**1.5/(ABS(R1(N1))*SNGL(DCSCL)*
     +    (ALOG(1+GAMMA)-GAMMA/(1+GAMMA)))
              TMU1=1.0/(1.0/MUB+1.0/MUPH+1.0/MUSR+1.0/MUC)
              DZ=-0.0388*T*2/(ET+E01)**2.0-1/3.*1.73E-5*2**(1./3.)
     +      /(ET+E01)**(4./3.)
              DMUPH=DZ/(3.2E-9*P*T**0.5)
              DMUSR=-2.0*6E14*2.0**2/(ET+E01)**3.0
              DU=TMU1**2*(DMUPH/MUPH**2.0+DMUSR/MUSR**2.0)
              TMOBN(2,N1)=(ET-E01)*DU
            ENDIF
C
          ENDIF
        ENDIF
        TMOBN(1,N1)=TMU1
      ENDIF
C..FOR N2
113       CONTINUE
      ET=0
C..IF THIS NODE IS DONE BEFORE (BY ANOTHER ELEMENT), USE STORED VALUE
      IF(TMOBN(1,N2).NE.0.0) THEN
        TMU2=TMOBN(1,N2)
      ELSE
        X=ANINT(CORD(1,N2)*1.0E8)/1.0E4
        Y=ANINT(CORD(2,N2)*1.0E8)/1.0E4
C
CCC  FOR SIMPLE ACCUM RF
        IF(R1(N2).GT.0.0) THEN
          IF(DINTF(N2).NE.0.0) THEN
            TMU2=MOBN(N2)*ACCSF
          ELSE
            TMU2=MOBN(N2)
          ENDIF
          TMOBN(1,N2)=TMU2
          MU21=(TMU1+TMU2+TMOBN(2,N1)+TMOBN(2,N2))*0.5
          GOTO 114
C  
        ELSE IF(FN(N2).LT.ABS(R1(N2))) THEN
          ET=0
CCC
C..RF=1 FOR NODES LOCATED MORE THAN 300A APART FROM OXIDE
        ELSE IF((OXL-X).GT.0.0.OR.(X-OXR).GT.0.0.OR.
     +              (Y-OXB).GT.0.03) THEN
          ET=0
C
C..RF=1 FOR NODES WITH E0<0.0 OR EY<0.0
        ELSE IF(E0(N2).LE.0.0.OR.TE(2,N2).LE.0.0) THEN
          ET=0
        ELSE
          ET=TE(2,N2)
        ENDIF
C...RF=1 FOR ET=0 EXCEPT THE INTERFACE NODE
        IF(ET.EQ.0.0) THEN
          TMU2=MOBN(N2)
          IF(R1(N2).LT.0.0.AND.DINTF(N2).NE.0.0.AND.
     +            TE(2,N2).GT.0.0) TMU2=TMU2*INVSF
        ELSE
          ET=ABS(ET)
C...ACCUMULATION ** ACTUALLY WE SKIP THIS ACCUM PART ***
          IF(R1(N1).GE.0.0) THEN
            ESAT=10**((2408.3/MOBN(N2)-ACCSF)/(348.4138/
     +                     MOBN(N2)))
            IF(ET.LE.(2*ESAT)) THEN
              TMU2=MOBN(N2)*ACCSF
            ELSE
              TMU2=2408.3-348.4138*ALOG10(ET/2)-348.4138
     +                   *0.434294482
C...LOGe=0.434294482
            ENDIF
          ELSE
C...INVERSION
            LTFM(N2)=.TRUE.                
            E02=E0(N2)
            IF(INTFCHG.LE.0.0) THEN
              WRITE(6,*) ' **** INCORRECT FIXED',
     +                         ' OXIDE CHARGE ****'
c              WRITE(19,*) ' **** INCORRECT FIXED',
c     +                         ' OXIDE CHARGE ****'
            ENDIF
C...SHIN    SCHWARZ EQUATION
C...        IF ET=E02, THIS EQ. BLOW-UP. AND SCHWARZ EQ. HAS MAXIMUM
C           WHEN E>E02. SO USE E=E02+3000 FOR MINIMUM
C                IF(ET.LE.E02+3000.) ET=E02+3000.
            T=TEMP/300.
C...SCHWARZ WITH Ni (OLD)
            IF(LOTFL) THEN
              Z=0.039*T*2./(ET+E02)+1.24E-5/((ET+E02)/2.)**(1./3.)
              P=0.09*T**1.5+1.5E-8/(FN(N2)*SNGL(DCSCL))**0.25/T*INTFCHG
              TMU2=1150.*Z/T**2.5/(Z+1150.*3.2E-9*P/T**2)
              DZ=-0.039*T*2/(ET+E02)**2-1/3.*1.24E-5*2**(1./3.)
     +      /(ET+E02)**(4./3.)
              DU=(1150./T**2.5*DZ*(Z+1150.*3.2E-9/T**2*P)-1150./T**2.5*Z
     +      *DZ)/(Z+1150.*3.2E-9/T**2*P)**2
              TMOBN(2,N2)=(ET-E02)*DU
C...NEW SEMI-EMPIRICAL MODEL WE DERIVED
            ELSE
              Z=0.0388*T*2./(ET+E02)+1.73E-5/((ET+E02)/2.)**(1./3.)
              P=0.09*T**1.75+4.53E-8/(FN(N2)*SNGL(DCSCL))**0.25
     +    /T*INTFCHG
              GAMMA=2.0E19/(FN(N2)*SNGL(DCSCL))*T**2.0
              MUB=1150.0/T**2.5
              MUPH=Z/(3.2E-9*P*T**0.5)
              MUSR=6E14/((ET+E02)/2.)**2.0
              MUC=1.1E21*T**1.5/(ABS(R1(N2))*SNGL(DCSCL)*
     +    (ALOG(1+GAMMA)-GAMMA/(1+GAMMA)))
              TMU2=1.0/(1.0/MUB+1.0/MUPH+1.0/MUSR+1.0/MUC)
              DZ=-0.0388*T*2/(ET+E02)**2.0-1/3.*1.73E-5*2**(1./3.)
     +      /(ET+E02)**(4./3.)
              DMUPH=DZ/(3.2E-9*P*T**0.5)
              DMUSR=-2.0*6E14*2.0**2/(ET+E02)**3.0
              DU=TMU2**2*(DMUPH/MUPH**2.0+DMUSR/MUSR**2.0)
              TMOBN(2,N2)=(ET-E02)*DU
C        IF(N1.EQ.583) THEN
C         write(19,*) ' ET,E0,N,n,QF,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU1',
C     +  ', DZ,DMUPH,DMUSR,DU,TMOBN'
C         write(19,*)  ET,E01,ABS(R1(N1))*SNGL(DCSCL),FN(N1)*SNGL(DCSCL),
C     +   INTFCHG,T,P,Z,GAMMA,MUB,MUPH,MUSR,MUC,TMU1,
C     +   DZ,DMUPH,DMUSR,DU,TMOBN(2,N1)
C         ENDIF

            ENDIF
C
          ENDIF
        ENDIF
        TMOBN(1,N2)=TMU2
      ENDIF
      MU21=(TMU1+TMU2+TMOBN(2,N1)+TMOBN(2,N2))*0.5
      endif
c
114   CONTINUE
c
c                Field dependent mobility
c
cjhc-- vsat on each side
c
      if(lfldmb.and.e21.gt.1.d0) then
c
cjhc
        dvs=dble(vsat)
c
c...Si
c...remember, dfdv is the derivative of mu w/ respect to psi2
c...DIVIDED BY mu21
c...use different code for special cases nmu=1,2 for efficiency
C
C...SHIN
C  MU21 AND dfdv ARE DIFFERENT, IF THE MOBILITIES OF THE TWO NODES ARE
C  DERIVED USING THE SCHWARTZ'S EQUATION.
        IF(LTFM(N1).AND.LTFM(N2).AND.NMU.EQ.2) THEN
          T11=TMOBN(1,N1)/DVS
          T12=T11*T11*E21
          T13=T12*E21
          T14=1.D0+T13
          T21=TMOBN(1,N2)/DVS
          T22=T21*T21*E21
          T23=T22*E21
          T24=1.D0+T23
          MU1=TMOBN(1,N1)/DSQRT(T14)+TMOBN(2,N1)/DSQRT(T14)**3
          MU2=TMOBN(1,N2)/DSQRT(T24)+TMOBN(2,N2)/DSQRT(T24)**3
          MU21=(MU1+MU2)/2
          IF(LDMU) THEN
            DMU1DE=-1.*T12*TMOBN(1,N1)/DSQRT(T14)**3-3.*
     +             TMOBN(2,N1)*T12/DSQRT(T14)**5 
            DMU2DE=-1.*T22*TMOBN(1,N2)/DSQRT(T24)**3-3.*
     +             TMOBN(2,N2)*T22/DSQRT(T24)**5 
            DFDV=(DMU1DE+DMU2DE)/(2.*MU21)*DEDV
          ELSE
            DFDV=0.D0
          ENDIF
         ELSE IF(LTFM(N1).AND.LTFM(N2).AND.ELMAT.EQ.1.AND.NMU.EQ.1) THEN
c...set Vsat=1e7 for hole
            DVS=1.0D7
            T11=TMOBP(1,N1)/DVS
            T12=T11*E21
            T13=1.D0+T12
            T21=TMOBP(1,N2)/DVS
            T22=T21*E21
            T23=1.D0+T22
            MU1=TMOBP(1,N1)/T13+TMOBP(2,N1)/T13**2
            MU2=TMOBP(1,N2)/T23+TMOBP(2,N2)/T23**2
            MU21=(MU1+MU2)/2
            IF(LDMU) THEN
               DMU1DE=-1.*T11*TMOBP(1,N1)/T13**2-2.*
     +             TMOBP(2,N1)*T11/T13**3 
               DMU2DE=-1.*T21*TMOBP(1,N2)/T23**2-2.*
     +             TMOBP(2,N2)*T21/T23**3 
               DFDV=(DMU1DE+DMU2DE)/(2.*MU21)*DEDV
            ELSE
               DFDV=0.D0
            ENDIF
c
c use standard pisces expression here for want of a better way
c around this. 
c
        ELSE if(nmu.eq.1.) then
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
      endif
c
      return
      end
