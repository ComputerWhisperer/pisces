      SUBROUTINE REDLN(LU,LINE)
      include 'genidf.inc'
      character*(*) line
      integer lu
c Thu Sep 14 00:36:03 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen
c   DATE: Oct. 18, 1979
c
c   MODIFICATIONS:
c
c   Michael Eldredge  Sept. 4, 1980
c     Convert to RATFOR
c   Michael Eldredge  June 30, 1981
c     Use FORTRAN-77 I/O
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c      Use of character variables.
c   Michael Eldredge (jul 87)  Convert to library package.
c
c   NAME: REDLN
c
c   FUNCTION: Reads a line of length LEN(line) from the specified input
c             file.  Error and EOF conditions are tested for.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL REDLN(LU,LINE)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       LU     INTEGER       The unit number of the file.
c
c     (OUTPUT)
c
c       LINE   character*(*)     Line read from the file at LU.
c
c   ERROR CONDITIONS:
c
c    038 : File system error detected on read from file.
c
c   NOTES:
c
c       Reads a record of length LEN(line) from the file associated with
c    the unit LU and returns it in the array LINE.  If an error is
c    detected on the read then the error is flaged and the file is
c    closed.  If an end-of-file is detected on the read then the file
c    is closed.
c
c   ROUTINES USED:
c
c       GERST
c
c   MACRO FILES:  GENIDF, COMMON
c
c   ALGORITHM:
c
c     1)  Read the next record from the file.
c     2)  If an error is detected on the read then call gerst to
c         flag the error, then close the file.
c     3)  If an EOF is detected then close the file.
c
c---------------------------------------------------------------------
c
c     common area
c
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c
c  local variables.
c
c--------------------------------------------------------------------
      integer ierr, ll
      character c1hblk
      data    c1hblk /' '/
c--------------------------------------------------------------------
c
c   start of redln.
c
c--------------------------------------------------------------------
c.. initialize the line to blanks.
      ll=LEN(line)
      call csetv(line,ll,c1hblk)

c.. read in the line.
      read (lu,10,iostat=ierr,err=98,end=99) line
10    format(a)
      return

c.. if there was a read error.
98    call gerst(038,linum,ierr,line,0)

c.. if an end of file or read error was detected.
99    eofflg=.true.
      return

      end
