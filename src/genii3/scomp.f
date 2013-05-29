      SUBROUTINE SCOMP(sstr,sstrln,cstr,cstrln,match,exact)
      logical match,exact
      character*(*) sstr,cstr
      integer cstrln,sstrln
c Thu Sep 14 01:41:04 PDT 1989 (dredge--stanford)
c---------------------------------------------------------------------
c
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c
c---------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen
c   DATE: Dec. 05, 1983
c
c   MODIFICATIONS:
c
c   NAME: SCOMP
c
c   FUNCTION: Compares two character strings ignoring case.  Returns
c             MATCH and EXACT = true if identical;
c             MATCH = true and EXACT = false if SSTR is a subset of CSTR.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL SCOMP(SSTR,SSTRLN,CSTR,CSTRLN,MATCH,EXACT)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       SSTR   character*n   The source string.
c       SSTRLN INTEGER       The length of SSTR.
c       CSTR   character*n   The comparison string.
c       CSTRLN INTEGER       The length of CSTR.
c
c     (OUTPUT)
c
c       MATCH  LOGICAL       Set true if SSTR is a subset of CSTR.
c       EXACT  LOGICAL       Set true if SSTR exactly matches CSTR.
c
c   ERROR CONDITIONS: None.
c
c   NOTES: None.
c
c   ROUTINES USED:
c       CMPCH
c
c   MACRO FILES:  GENIDF
c
c   ALGORITHM:
c
c--------------------------------------------------------------------
c   local variables
c--------------------------------------------------------------------
      logical cstend,sstend
      integer i
      character blnk
c--------------------------------------------------------------------
c Functions:
c--------------------------------------------------------------------
c..case ignorer
      logical cmpch

      data  blnk/' '/
c--------------------------------------------------------------------
c
c   start of scomp.
c
c--------------------------------------------------------------------
c.. search through the source string comparing it to the compare
c..  string until either the comparison fails or there are no more
c..   characters to compare in one of the strings.
      i=0
c.....REPEAT
100   i=i+1
      sstend = (i.gt.sstrln)
      if (.not.sstend) sstend = (sstr(i:i).eq.blnk)
      cstend = (i.gt.cstrln)
      if (.not.cstend) cstend = (cstr(i:i).eq.blnk)
c
      if (i.eq.1 .or. (.not.sstend .and. .not.cstend))
     +    match=(cmpch(sstr(i:i),cstr(i:i)))

      if (match .and. .not.sstend .and. .not.cstend) goto 100
C.... until (^match | sstend | cstend)
c
c.. if the strings matched to the end, then they are an exact match.
      exact = (match .and. sstend .and. cstend)

      return
      end
