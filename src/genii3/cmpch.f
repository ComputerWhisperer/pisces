      LOGICAL FUNCTION CMPCH(ICHR1,ICHR2)
      character ichr1,ichr2
C .. date code: 20 aug 83
C ---------------------------------------------------------------------
C
C    Copyright 1980 by
C    The Board of Trustees of the Leland Stanford Junior University
C    All rights reserved.
C
C    This routine may not be used without the prior written consent
C    of the Board of Trustees of the Leland Stanford University.
C
C ---------------------------------------------------------------------
C
C    WRITTEN BY: Michael Eldredge
C    DATE: May 1981
C
C    MODIFICATIONS:
C      Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
C         Use of character variables.
C ON
C TOF
C
C    NAME: CMPCH
C
C UOFF
C UON
C
C    FUNCTION: Compares two characters ignoring case (i.e. a = A).
C
C    TYPE OF ROUTINE: LOGICAL FUNCTION
C
C    CALLING SEQUENCE:
C
C       L=CMPCH(ICHR1,ICHR2)
C
C    PARAMETERS:
C
C      (INPUT)
C
C        ICHR1  character     The first character to compare.
C        ICHR2  character     The second character to compare.
C
C      (OUTPUT)
C
C        CMPCH  FUNCTION      The result of the comparison, true if
C                             the two characters are the same, ignoring
C                             case.
C
C    ERROR CONDITIONS: None.
C
C    NOTES: None.
C
C    ROUTINES USED: None.
C
C UOFF
C    MACRO FILES:  GENIDF
C
C OFF
C --------------------------------------------------------------------
C    Functions
C --------------------------------------------------------------------
      character*1 mapch

c --------------------------------------------------------------------
c   Local variables
c --------------------------------------------------------------------
      character t1, t2

c --------------------------------------------------------------------
c    start of cmpch
c --------------------------------------------------------------------

c  Tue Nov  8 11:50:44 PST 1988 (dredge--stanford)
c   There seems to be a bug in the MIPS compiler that will not
c   let the character func() be in an expression.  We want to say:
c        cmpch = (mapch(ichr1).eq.mapch(ichr2))
c   So for now, to get this port working, we will just use the temp values.

      t1 = mapch(ichr1)
      t2 = mapch(ichr2)
      cmpch = (t1.eq.t2)

      return
      end
