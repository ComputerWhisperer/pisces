      subroutine clsfl(lu, closer, ierr)
        logical closer
        integer lu, ierr
c-----------------------------------------------------------------------
c   Copyright 1986 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen                        DATE: 13-Feb-82
c   MODIFICATIONS:
c   BY: Michael Eldredge                                 DATE:    Jul-83
c
c   NAME: clsfl
c   FUNCTION: Closes the specified file.
c
c   TYPE OF ROUTINE:  SUBROUTINE
c   CALLING SEQUENCE:
c    call clsfl(lu, closer, ierr)
c
c   PARAMETERS (INPUT):
c    lu            integer  The fortran unit number.
c
c   PARAMETERS (OUTPUT):
c    closer        logical  True if an error was detected on the close
c                           of the specified file.
c    ierr          integer  I/O status returned by the close call.
c
c   ERROR CONDITIONS: (None.)
c
c   ROUTINES USED: (None.)
c   INCLUDE FILES: genidf.h
c
c   NOTES: (None.)
c
c-----------------------------------------------------------------------
c   start of clsfl
c-----------------------------------------------------------------------
c.. Initialize the close error flag to false, (assume success).
      closer=.false.
      ierr=0
c
c.. Close the file.
      close(lu, err=11, iostat=ierr)
c
c.. Get on with it.
      return
c
c.. Error return.
11    closer=.true.
      return
      end
c
c =====================================================================
c
      subroutine clsrm(lu, closer, ierr)
        logical closer
        integer lu, ierr
c-----------------------------------------------------------------------
c   Copyright 1990 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   Original: Michael Eldredge -- Stanford (mar 90)
c
c   "clsrm": Like clsfl() but delete the file on close.
c
c   calling sequence:
c        call clsrm(lu, closer, ierr)
c
c   PARAMETERS (INPUT):
c    lu            integer  The fortran unit number.
c
c   PARAMETERS (OUTPUT):
c    closer        logical  True if an error was detected on the close
c                           of the specified file.
c    ierr          integer  I/O status returned by the close call.
c
c-----------------------------------------------------------------------
c   start of clsrm
c-----------------------------------------------------------------------
c.. Initialize the close error flag to false, (assume success).
      closer=.false.
      ierr=0
c
c.. Close the file.
      close(lu, err=11, iostat=ierr, status='delete')
c
c.. Get on with it.
      return
c
c.. Error return.
11    closer=.true.
      return
      end
