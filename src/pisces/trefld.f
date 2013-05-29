cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue Apr 17 23:13:24 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE TREFLD
      include 'p2conf.h'
c
C...DATE CODE: 1/11/88
c
C   WRITTEN BY: H. SHIN
C   REVISION  : H. SHIN  8/8/89
C               ADD E0 AND PMOS
C
C   FUNCTION: CALCULATE E FIELD FOR TRANSVERSE FIELD DEP.
C             MOBILITY MODEL
C
c   G. Anderson  Cleanup for inclusion in Stanford release  11/89
c                rename TREFLD for TRansvers E FieLD...
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
C                  COMMON AREA
C
      include 'blank.h'
      include 'emaco.h'
      include 'setup.h'
      include 'trfldmob.h'
c
C...SHIN 
      LOGICAL LOXIDE
      INTEGER TYP(20)
      REAL    X1,Y1
C...
      logical lesol2
      integer inode,i,n,ni(5),idxnod,i1,i2,in
      integer elmat,nmati
      real xpt,ypt
Cc
C...SHIN
      DOUBLE PRECISION EXP,EYP,EXN,EYN,POTE,CCOEF
      REAL X1PT,X1X2,Y1PT,Y1Y2
      LOGICAL LXP,LXN,LYP,LYN,LXPINT,LXNINT,LYPINT,LYNINT
      INTEGER MP,MN,PP(20),II
      REAL X2,Y2,XX,YY,dxp,dxn,dyp,dyn
c
c                   type dec
c
      real oe0,x,y,ox
      integer nm
c
      data ni/1,2,3,1,2/
c
c******************************************************************
c    start
c
c...Get interface charge
      INTFCHG = ssdata(nintrf,5) 
c...scan through nodes
      do 100 inode=1,np 
      loxide=.false.
c...set oxide flag
      do 101 in=1,p2tc(inode)
        n=p2t(in,inode)
        nmati=imat(n)
        typ(in)=mattyp(nmati)
        if(in.ge.2.and.typ(in).ne.typ(in-1)) then
          loxide=.false.
          goto 103
        endif
        if(typ(in).lt.0) loxide=.true.
101   continue
103   continue
c...write Ex
C       write(6,*) 'node,x,y,oxide',inode,cord(1,inode),
C     +  cord(2,inode),loxide
C       write(luout,*) 'node,x,y,oxide',inode,cord(1,inode),
C     +  cord(2,inode),loxide
C   EXCLUDE THE NODES IN THE OXIDE
      if(loxide) goto 100
c
c*************
c**  START  **
c*************
Cc
C...THIS SECTION IS FOR ELECTRIC FIELD CALCULATED BY POTENTIAL DERIVATIVE
C
C        IF(FLAG.NE.6) GOTO 123
C...INITIALIZE
C  LXP: RIGHT-SIDE FIELD
C  LXN: LEFT-SIDE FIELD        
C  LYP: DOWN-SIDE FIELD
C  LYN: UP-SIDE FIELD
C  LXPINT: INTERFACE NODE WITH OXIDE AT LEFT
C  LXNINT: INTERFACE NODE WITH OXIDE AT RIGHT
C  LYPINT: INTERFACE NODE WITH OXIDE AT UPSIDE
C  LXNINT: INTERFACE NODE WITH OXIDE AT DOWNSIDE
      LESOL2=.FALSE.
      LXP=.FALSE.
      LXN=.FALSE.
      LYP=.FALSE.
      LYN=.FALSE.
      LXPINT=.FALSE.
      LXNINT=.FALSE.
      LYPINT=.FALSE.
      LYNINT=.FALSE.
      XPT=CORD(1,INODE)
      YPT=CORD(2,INODE)
      EXP=0.D0
      EYP=0.D0
      EXN=0.D0
      EYN=0.D0
      DXP=0.0
      DYP=0.0
      DXN=0.0
      DYN=0.0
      MP=1
      MN=P2TC(INODE)
C
C...SORT ELEMENT TO PUT OXIDE ELEMENTS  BACK
C
      DO 144 IN=1,P2TC(INODE)
        N=P2T(IN,INODE)
        NMATI=IMAT(N)
        ELMAT=MATTYP(NMATI)
        IF(ELMAT.LT.0) THEN
          PP(MN)=N
          MN=MN-1
        ELSE
          PP(MP)=N
          MP=MP+1
        ENDIF
144   CONTINUE
C
C...START
C
      DO 155 IN=1,P2TC(INODE)
      IF(LXP.AND.LXN.AND.LYP.AND.LYN) GOTO 900
      N=PP(IN)
      NMATI=IMAT(N)
      ELMAT=MATTYP(NMATI)
C...SET COEFICIENT OF ELECTRIC FIELD AT SILICON OR OXIDE
      IF(ELMAT.LT.0) THEN
         CCOEF=3.9/11.8
      ELSE
         CCOEF=1.D0
      ENDIF
C...FIND INDEX OF NODE
      IF(NOP(1,N).EQ.INODE) THEN
        IDXNOD=1
      ELSE IF(NOP(2,N).EQ.INODE) THEN
        IDXNOD=2
      ELSE
        IDXNOD=3
      ENDIF
C... GET CORDINATE OF OTHER NODES
      I=IDXNOD
      I1=NOP(NI(I+1),N)
      I2=NOP(NI(I+2),N)
      X1=CORD(1,I1)
      Y1=CORD(2,I1)
      X2=CORD(1,I2)
      Y2=CORD(2,I2)
C        write(19,*) 'i,i1,i2,x1,y1,v1,x2,y2,v2,xpt,ypt',
C     +    i,i1,i2,x1,y1,fv(i1),x2,y2,fv(i2),xpt,ypt
C...FIND THE INTERSECTION AND CALCULATE E
      IF(X1.EQ.XPT) THEN
        XX=X1
        YY=Y1
        II=I1
      ELSE  IF(X2.EQ.XPT) THEN
        XX=X2
        YY=Y2
        II=I2
      ENDIF
      IF(X1.EQ.XPT.OR.X2.EQ.XPT) THEN
        IF(YY.LT.YPT.AND..NOT.LYN) THEN
          IF(CCOEF.NE.1.D0) LYPINT=.TRUE.
          DYN=ABS(YPT-YY)
          LYN=.TRUE.
          IF(LESOL2) THEN
            EYN=DKTQ*(OFV(II)-OFV(INODE))/DYN*CCOEF
          ELSE
            EYN=DKTQ*(FV(II)-FV(INODE))/DYN*CCOEF
          ENDIF
        ELSE IF(YY.GT.YPT.AND..NOT.LYP) THEN
          IF(CCOEF.NE.1.D0) LYNINT=.TRUE.
          DYP=ABS(YY-YPT)
          LYP=.TRUE.
          IF(LESOL2) THEN
            EYP=DKTQ*(OFV(INODE)-OFV(II))/DYP*CCOEF
          ELSE
            EYP=DKTQ*(FV(INODE)-FV(II))/DYP*CCOEF
          ENDIF
        ENDIF
      ENDIF
      IF(Y1.EQ.YPT) THEN
        XX=X1
        YY=Y1
        II=I1
      ELSE  IF(Y2.EQ.YPT) THEN
        XX=X2
        YY=Y2
        II=I2
      ENDIF
      IF(Y1.EQ.YPT.OR.Y2.EQ.YPT) THEN
        IF(XX.LT.XPT.AND..NOT.LXN) THEN
          IF(CCOEF.NE.1.D0) LXPINT=.TRUE.
          DXN=ABS(XPT-XX)
          LXN=.TRUE.
          IF(LESOL2) THEN
            EXN=DKTQ*(OFV(II)-OFV(INODE))/DXN*CCOEF
          ELSE
            EXN=DKTQ*(FV(II)-FV(INODE))/DXN*CCOEF
          ENDIF
        ELSE IF(XX.GT.XPT.AND..NOT.LXP) THEN
          IF(CCOEF.NE.1.D0) LXNINT=.TRUE.
          DXP=ABS(XX-XPT)
          LXP=.TRUE.
          IF(LESOL2) THEN
            EXP=DKTQ*(OFV(INODE)-OFV(II))/DXP*CCOEF
          ELSE
            EXP=DKTQ*(FV(INODE)-FV(II))/DXP*CCOEF
          ENDIF
        ENDIF
      ENDIF
      IF(X1.GT.XPT.AND.X2.LT.XPT.OR.
     +         X1.LT.XPT.AND.X2.GT.XPT) THEN
      X1X2=ABS(X1-X2)
      X1PT=ABS(X1-XPT)
      YY=(Y2-Y1)/(X2-X1)*(XPT-X1)+Y1
      IF(YY.LT.YPT.AND..NOT.LYN) THEN
        IF(CCOEF.NE.1.D0) LYPINT=.TRUE.
        DYN=ABS(YPT-YY)
        LYN=.TRUE.
        IF(LESOL2) THEN
          POTE=OFV(I1)+X1PT/X1X2*
     +             (OFV(I2)-OFV(I1))
          EYN=DKTQ*(POTE-OFV(INODE))/DYN*CCOEF
        ELSE
          POTE=FV(I1)+X1PT/X1X2*
     +             (FV(I2)-FV(I1))
          EYN=DKTQ*(POTE-FV(INODE))/DYN*CCOEF
        ENDIF
      ELSE  IF(YY.GT.YPT.AND..NOT.LYP) THEN
        IF(CCOEF.NE.1.D0) LYNINT=.TRUE.
        DYP=ABS(YY-YPT)
        LYP=.TRUE.
        IF(LESOL2) THEN
          POTE=OFV(I1)+X1PT/X1X2*
     +             (OFV(I2)-OFV(I1))
            EYP=DKTQ*(OFV(INODE)-POTE)/DYP*CCOEF
          ELSE
            POTE=FV(I1)+X1PT/X1X2*
     +             (FV(I2)-FV(I1))
            EYP=DKTQ*(FV(INODE)-POTE)/DYP*CCOEF
          ENDIF
        ENDIF
      ENDIF
      IF(Y1.GT.YPT.AND.Y2.LT.YPT.OR.
     +         Y1.LT.YPT.AND.Y2.GT.YPT) THEN
          Y1Y2=ABS(Y1-Y2)
          Y1PT=ABS(Y1-YPT)
          XX=(X2-X1)/(Y2-Y1)*(YPT-Y1)+X1
          IF(XX.LT.XPT.AND..NOT.LXN) THEN
            IF(CCOEF.NE.1.D0) LXPINT=.TRUE.
            DXN=ABS(XPT-XX)
            LXN=.TRUE.
            IF(LESOL2) THEN
              POTE=OFV(I1)+Y1PT/Y1Y2*
     +             (OFV(I2)-OFV(I1))
              EXN=DKTQ*(POTE-OFV(INODE))/DXN*CCOEF
            ELSE
              POTE=FV(I1)+Y1PT/Y1Y2*
     +             (FV(I2)-FV(I1))
              EXN=DKTQ*(POTE-FV(INODE))/DXN*CCOEF
            ENDIF
          ELSE  IF(XX.GT.XPT.AND..NOT.LXP) THEN
            IF(CCOEF.NE.1.D0) LXNINT=.TRUE.
            DXP=ABS(XX-XPT)
            LXP=.TRUE.
            IF(LESOL2) THEN
              POTE=OFV(I1)+Y1PT/Y1Y2*
     +             (OFV(I2)-OFV(I1))
              EXP=DKTQ*(OFV(INODE)-POTE)/DXP*CCOEF
            ELSE
              POTE=FV(I1)+Y1PT/Y1Y2*
     +             (FV(I2)-FV(I1))
              EXP=DKTQ*(FV(INODE)-POTE)/DXP*CCOEF
            ENDIF
          ENDIF
        ENDIF
155     CONTINUE
C       EX=(DXP*EXP+DXN*EXN)/(DXP+DXN)
C       EY=(DYP*EYP+DYN*EYN)/(DYP+DYN)
C
C
C  FOR ALL THE NODES EXCEPT AT THE INTERFACE, USE THE AVERAGED FIELD.
C  FOR INTERFACE NODE, USE THE OXIDE FIELD WITH CONSIDERING 
C   THE DIELECTRIC COEFFICIENT DIFFRENCE AND EFFECT FROM INTERFACE
C   CHARGE
900     TE(1,INODE)=(DXN*EXP+DXP*EXN)/(DXP+DXN)
        TE(2,INODE)=(DYN*EYP+DYP*EYN)/(DYP+DYN)
        IF(LXPINT) THEN
          TE(1,INODE)=EXN+INTFCHG*1.6E-19/(11.8*8.854E-14)
        ELSE IF(LXNINT) THEN
          TE(1,INODE)=EXP-INTFCHG*1.6E-19/(11.8*8.854E-14)
        ELSE IF(LYPINT) THEN
          TE(2,INODE)=EYN+INTFCHG*1.6E-19/(11.8*8.854E-14)
        ELSE IF(LYNINT) THEN
          TE(2,INODE)=EYP-INTFCHG*1.6E-19/(11.8*8.854E-14)
        ENDIF
100      continue
C
C...E0 COMPUTATION
c...Set the initial condition
      OE0=0.0
      OX=-999.99
C
      I=0
901   CONTINUE
      I=I+1
      IF(I.GT.NP) GOTO 999
C
      X=ANINT(CORD(1,I)*1.0E8)/1.0E4
      Y=ANINT(CORD(2,I)*1.0E8)/1.0E4
C
C...NMOS: COMPUTE E0 FOR THE NODES WITHIN 300A APART FROM OXIDE AND N IS
C   LARGER THAN Ni
      IF((OXL-X).LE.0.0.AND.(X-OXR).LE.0.0.AND.(Y-OXB).GE.0.0
     + .AND.(Y-OXB).LE.0.03.AND.FN(I)*SNGL(DCSCL).GT.FNI
     + .AND.R1(I).LT.0.0.AND.TE(2,I).GT.0.0) THEN
C
C...USE A SAME E0 FOR NODES AT THE SAME X-COORDINATE
        IF(X.EQ.OX) THEN
           E0(I)=OE0
        ELSE
C
C...USE Es AS E0, IF Ni<N<Na at the surface
           IF(FN(I).LE.ABS(R1(I))) THEN
              E0(I)=TE(2,I)
              OE0=E0(I)
              OX=X
           ELSE
C
C...FIND THE NODE WHERE N<Na AND SET E0=Ey AT THIS NODE
              IN=I
              E0(IN)=OE0
200           IN=IN+1
              E0(IN)=OE0
              IF(R1(IN).GE.0.0) GOTO 113
              IF(FN(IN).GT.ABS(R1(IN))) GOTO 200
              DO 902 NM=I,IN
                 E0(NM)=TE(2,IN)
902           CONTINUE
              OX=ANINT(CORD(1,IN)*1.0E8)/1.0E4
              OE0=E0(IN)
           ENDIF
         ENDIF
      ENDIF
C
C...PMOS: COMPUTE E0 FOR THE NODES WITHIN 500A APART FROM OXIDE AND P IS
C   LARGER THAN Ni
      IF((OXL-X).LE.0.0.AND.(X-OXR).LE.0.0.AND.(Y-OXB).GE.0.0
     + .AND.(Y-OXB).LE.0.05.AND.FP(I)*SNGL(DCSCL).GT.FNI
     + .AND.R1(I).GT.0.0.AND.TE(2,I).LT.0.0) THEN
C
C...USE A SAME E0 FOR NODES AT THE SAME X-COORDINATE
        IF(X.EQ.OX) THEN
           E0(I)=OE0
        ELSE
C
C...USE Es AS E0, IF Ni<P<Nd at the surface
           IF(Fp(I).LE.ABS(R1(I))) THEN
              E0(I)=TE(2,I)
              OE0=E0(I)
              OX=X
           ELSE
C
C...FIND THE NODE WHERE p<Nd AND SET E0=Ey AT THIS NODE
              IN=I
              E0(IN)=OE0
300           IN=IN+1
              E0(IN)=OE0
              IF(R1(IN).LE.0.0) GOTO 113
              IF(Fp(IN).GT.ABS(R1(IN))) GOTO 300
              DO 912 NM=I,IN
              E0(NM)=TE(2,IN)
912           CONTINUE
              OX=ANINT(CORD(1,IN)*1.0E8)/1.0E4
              OE0=E0(IN)
           ENDIF
        ENDIF
      ENDIF
113   GOTO 901
C
999   CONTINUE
      return
      end
