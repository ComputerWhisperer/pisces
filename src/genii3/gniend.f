       SUBROUTINE GNIEND
      include 'genidf.inc'
c Tue Mar  6 16:51:09 PST 1990 (dredge--stanford)
c---------------------------------------------------------------------
c   Copyright 1990 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c
c   "gniend": Clean up (close files) etc when genii is finished; or
c       if called by a user, force cleanup.
c
c    NOTE: this is called when GTVAL() has exhausted all of the
c       input or when a user calls it to force and end of input.
c    NOTE: This will delete the prs file.
c
c   calling sequence:
c
c       call gniend
c
c   Original: Michael Eldredge -- Stanford (feb 90)
c
c---------------------------------------------------------------------
c   common
c---------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables
c---------------------------------------------------------------------
       logical filerr
       integer ierr

c---------------------------------------------------------------------
c   start of gniend
c---------------------------------------------------------------------

c.. already closed?  Just return.
      if (eofflg) return

c.. set the end-of-file flag true.
      eofflg=.true.
c
c.. close the file.
      if (.not.opprs) return
      call clsrm(luprs,filerr,ierr)
      opprs=.false.
c
c.. if an error was detected on the close, then say so.
      if (filerr) then
         write(luttyo,21) ierr
         if (luout .ne.luttyo) write(luout,21) ierr
21       format(/' File system error #',I8,' detected on close of the',
     +        ' parsed input file.')
      endif

c... that was it.
      return
      end
