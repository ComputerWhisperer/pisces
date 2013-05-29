       LOGICAL FUNCTION fexist(fname)
      include 'genidf.inc'
       character*(*) fname
c Wed Sep 13 10:38:46 PDT 1989 (dredge--stanford)
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
c   DATE: May 1988
c
c   MODIFICATIONS:
c
c   NAME: CMPCH
c
c   FUNCTION: Check for file existence.
c
c   TYPE OF ROUTINE: LOGICAL FUNCTION
c
c   CALLING SEQUENCE:
c
c      <L>=FEXIST(FNAME)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       FNAME  character     Name of file on which to check.
c
c     (OUTPUT)
c
c       CMPCH  FUNCTION      True if file exists and false if not.
c
c   ERROR CONDITIONS: None.
c
c   NOTES: None.
c
c   ROUTINES USED: None.
c
c--------------------------------------------------------------------
c
c   local variables
c
c--------------------------------------------------------------------
      logical lflag
      character*(NAMRLN) name

c--------------------------------------------------------------------
c
c   start of fexist
c
c--------------------------------------------------------------------
      call xgeten(fname, name)
      if (name(1:1) .eq. ' ') name = fname
      inquire (file=name,exist=lflag)

      fexist = lflag
      return
      end
