      SUBROUTINE FINDC(LPTR,LENG,LINE,EOL,LIST)
        logical eol
        integer lptr,leng
        character*(*) line
        character*(*) list
c Wed Sep 13 09:25:27 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen                   DATE: Sept. 08, 1982
c   MODS BY   : Michael Eldredge                    DATE: June      1983
c               UNIX conversion to Fortran-77.
c               Use of character variables.
c   modified: Michael Eldredge (jul 87) Takes a list of possible characters
c       to find.  Was FINDB() for find-blank.
c
c   NAME: FINDC
c   FUNCTION: From the location in LINE pointed to be LPTR, skips over
c             characters until one of the characters in LIST is found.
c   TYPE OF ROUTINE: SUBROUTINE
c   CALLING SEQUENCE:
c      CALL FINDC(LPTR,LENG,LINE,EOL,LIST)
c
c   PARAMETERS: (INPUT)
c    LPTR   INTEGER       Pointer to start of search string.
c    LENG    INTEGER       Maximum length of search string.
c    LINE   character*n   Character buffer contain string to search.
c    EOL    LOGICAL       End Of Line flag.
c    LIST   character*N   List of characters for which to look.  Searching
c                         ends when any one of LIST chars is found.
c
c   PARAMETERS: (OUTPUT)
c    LPTR   INTEGER       Pointer to the first LIST character in the
c                         search string.
c    EOL    LOGICAL       True if the end of the search string was
c                         reached before a blank was found.
c
c   ERROR CONDITIONS: (None.)
c   ROUTINES USED: (None.)
c   INTRINSICS USED: index
c
c   NOTES: (None.)
c
c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i
c---------------------------------------------------------------------
c   start of findc
c---------------------------------------------------------------------

c.. find the char from LIST.
      i = INDEX(list, line(lptr:lptr) )

100   if (eol .or. INDEX(list, line(lptr:lptr)).ne.0) goto 200
          eol=(lptr.gt.leng)
          i = index(list, line(lptr:lptr))
          lptr = lptr + 1
          goto 100
c
200   return
      end
