      SUBROUTINE SKIPC(LPTR,LENG,LINE,EOL,LIST)
        logical eol
        integer lptr,leng
        character*(*) line
        character*(*) list
c Wed Sep 13 13:29:31 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1982 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen                 DATE: Sept. 08, 1982
c   MODS BY   : Michael Eldredge                  DATE: June      1983
c               UNIX conversion to Fortran-77
c               Use of character variables.
c   modified: Michael Eldredge (jul 87) Skip any chars in LIST.
c               Was skipb() for "Skip Blanks"
c
c   NAME: SKIPC
c   FUNCTION: From the location in LINE pointed to be LPTR, skips
c             until a character not in LIST is found.
c   TYPE OF ROUTINE: SUBROUTINE
c   CALLING SEQUENCE:
c      CALL SKIPC(LPTR,LENG,LINE,EOL,LIST)
c
c   PARAMETERS: (INPUT)
c    LPTR   INTEGER       Pointer to start of search string.
c    LENG    INTEGER       Maximum length of search string.
c    LINE   CHARACTER*N   Array conaining search string.
c    EOL    LOGICAL       End Of Line flag.
c    LIST   CHARACTER*N   List of chars over which to skip.
c   PARAMETERS: (OUTPUT)
c    LPTR   INTEGER       Pointer to the first character in LINE
c                         not matching any in list.
c    EOL    LOGICAL       True if the end of the search string was
c                         reached before a non-LIST character was
c                         found.
c
c   ERROR CONDITIONS: (None.)
c   ROUTINES USED: (None.)
c   INTRINSICS USED: index
c
c   NOTES: (None.)
c
c---------------------------------------------------------------------
c   start of skipc
c---------------------------------------------------------------------

c.. Skip all chars that match in LIST (index() non-zero :: found one).
100   if (eol .or. (INDEX(list, line(lptr:lptr)).eq.0)) goto 200
          eol=(lptr.gt.leng)
          lptr = lptr+1
          goto 100
c
200   return
      end
