      SUBROUTINE CENSTR(LU, STR, WID)
        integer       lu
        character*(*) str
        integer       wid
c Wed Sep 13 09:39:36 PDT 1989 (dredge--stanford)
c---------------------------------------------------------------------
c
c   Copyright 1988 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c
c---------------------------------------------------------------------
c
c   WRITTEN BY: Michael Eldredge
c   DATE: Jul 1988
c
c   MODIFICATIONS:
c
c   CENSTR: Output a string to LU centered between col 1 and WID
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL CENSTR(LU, STR, WID)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       LU     integer       Output to which to print the string
c       STR    character*(*) String to center and output
c       WID    integer       Width within which to center
c
c     (OUTPUT)    -none-
c
c   ERROR CONDITIONS: None.
c
c--------------------------------------------------------------------
c
c   local variables
c
c--------------------------------------------------------------------
      integer   i, j, k
      integer   ib
      character*50 blanks

      data blanks/'                                              '/

c--------------------------------------------------------------------
c
c   start of censtr
c
c--------------------------------------------------------------------

      i = 1
      j = LEN(str)

c find the beginning.
      do 10 i=1,j
         if (str(i:i).ne.' ') goto 11
10    continue

c find the end.
11    do 20 k=j,1,-1
         if (str(k:k).ne.' ') then
             j=k
             goto 21
         endif
20    continue
c
c..
21    ib = (wid-(j-i+1))/2

      write(lu, 101) blanks(1:ib), str(i:j)
101   format(1x,a,a)

      return
      end
