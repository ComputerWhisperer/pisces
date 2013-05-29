C NOTE:
C  1. This is the f77 version of the gplot include file
C     and is supposed to be the same but for Fortran includes.
C  2. All underscores from the C form have been changed to 'x',
C     thus: G_DRAW has become GxDRAW
C  3. The macro functions have not been included.  These functions
C     have been implemented as fortran subroutines and are
C     available in the library.
C  4. This file was generated from `gplot.h' (the C form) by a
C     program, and as such is not so `pretty'. Sorry.

        integer  GxCLR
      parameter (GxCLR=1)
        integer  GxDRAW
      parameter (GxDRAW=2)
        integer  GxMOVE
      parameter (GxMOVE=3)
        integer  GxSCAL
      parameter (GxSCAL=11)
        integer  GxTRAN
      parameter (GxTRAN=13)
        integer  GxROTA
      parameter (GxROTA=15)
        integer  GxANGL
      parameter (GxANGL=17)
        integer  GxCLPL
      parameter (GxCLPL=31)
        integer  GxCLPH
      parameter (GxCLPH=32)
        integer  GxLINE
      parameter (GxLINE=33)
        integer  GxPEN
      parameter (GxPEN=34)
        integer  GxUSR1
      parameter (GxUSR1=35)
        integer  GxUSR2
      parameter (GxUSR2=36)
        integer  GxDMOD
      parameter (GxDMOD=37)
        integer  GxAREA
      parameter (GxAREA=38)
        integer  GxFILS
      parameter (GxFILS=39)
        integer  GxRESE
      parameter (GxRESE=42)
        integer  GxSHIN
      parameter (GxSHIN=43)
        integer  GxPAUS
      parameter (GxPAUS=44)
        integer  GxMARK
      parameter (GxMARK=98)
        integer  GxPEND
      parameter (GxPEND=99)
        integer  GxNULL
      parameter (GxNULL=100)
        integer  GxASAV
      parameter (GxASAV=101)
        integer  GxPSIZ
      parameter (GxPSIZ=102)
        integer  GxCLOC
      parameter (GxCLOC=103)
        integer  GxDABL
      parameter (GxDABL=104)
        integer  GxREVC
      parameter (GxREVC=105)
        integer  GxPDEV
      parameter (GxPDEV=106)
        integer  GxPFIL
      parameter (GxPFIL=107)
        integer  GxATOG
      parameter (GxATOG=108)
        integer  GxGTOA
      parameter (GxGTOA=109)
        integer  GxXMAT
      parameter (GxXMAT=110)
        integer  GxONLO
      parameter (GxONLO=0)
        integer  GxONPH
      parameter (GxONPH=1)
        integer  GxDOBR
      parameter (GxDOBR=10)
        integer  GxNOBR
      parameter (GxNOBR=11)
        integer  GxBEG
      parameter (GxBEG=0)
        integer  GxON
      parameter (GxON=1)
        integer  GxOFF
      parameter (GxOFF=2)
        integer  GxSTOP
      parameter (GxSTOP=3)
        integer  GxSET
      parameter (GxSET=1)
        integer  GxGET
      parameter (GxGET=2)
        integer  GxMUL
      parameter (GxMUL=3)
        integer  GxCHEC
      parameter (GxCHEC=21)
        integer  GxMSET
      parameter (GxMSET=0)
        integer  GxMCLR
      parameter (GxMCLR=1)
        integer  GxMCMP
      parameter (GxMCMP=2)
