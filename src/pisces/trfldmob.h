cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c
C...SHIN
      COMMON/SHINTFMB/ACCSF,INVSF,OXL,OXR,OXB,LTFLDMB,LOTFL,LCONM3
      LOGICAL LTFLDMB,LOTFL,LCONM3
      REAL ACCSF,INVSF,OXL,OXR,OXB
C
      COMMON/SHINTM/TMOBN,TE,E0,LTFM,TMOBP
      REAL TMOBN(2,MAXPT),TE(2,MAXPT),E0(MAXPT),TMOBP(2,MAXPT)
      LOGICAL LTFM(MAXPT)
C
      COMMON/SHININT/INTFCHG
      REAL INTFCHG
C...SHIN
      COMMON/SHINNX/P2T,P2TC
      integer p2t(MAXNB,MAXPT),p2tc(MAXPT)
