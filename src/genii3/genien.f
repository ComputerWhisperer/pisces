      SUBROUTINE GENIEN
      include 'genidf.inc'
c Tue Mar  6 16:52:43 PST 1990 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1989 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c "genien": End/close up a genii preprocess run.
c
c NOTE: This is called after a batch mode run processing the input
c   file.  The prs file cannot go away since we assume, if no errors,
c   that the user will get into a loop calling gtval() to extract
c   the values.
c
c Original: Michael Eldredge -- Stanford (feb 89)
c
c-----------------------------------------------------------------------
c   common
c-----------------------------------------------------------------------
      include 'common.inc'
c-----------------------------------------------------------------------
c   local variables
c-----------------------------------------------------------------------
      logical  closer,opnerr
      integer  ierr
c  logical   cont
c  integer   i,np,iptr,indx,ityp,iloc,lpf,lpl
c  character*LINELN line
c  character space
c
c  logical   gtval
c
c  data space /' '/ ;
c-----------------------------------------------------------------------
c   start of geniin
c-----------------------------------------------------------------------


c.. close up shop and flag errors if any .
      if (opinp) then
c..... close the input file.
        call clsfl(luinp,closer,ierr)
        if (closer) call gerst(040,0,ierr,' ',0)
      endif
c
      if (opprs) then
c.. close the parsed file.
        call clsfl(luprs,closer,ierr)
        if (closer) call gerst(041,0,ierr,' ',0)
      endif

c.. if fatal errors (vs. warnings) were found, then make the output file
c... have a keyid indicating that things bombed here.  Then the program
c...  using GENII can check for this and not proceed. (mje jun83)
      if (errflg) then
          call opnfl(luprs,prsfil,opnerr,ierr,
     +             UNKNOWN,FORMED,NONEXCLUS,NOAPPEND)
          if (.not.opnerr) then
              keyid=FATALERRORS
c... write out the keyid
              call outpt
              call clsfl(luprs,closer,ierr)
          else
c... trying to open it again...
              call gerst(037,0,ierr,' ',0)
          endif
      endif

c incase gtval() is in the same program with us; reset some counters.
      linum = 0
      eofflg= .false.

c.. go output any error or warning messages.
      call gener

      return
      end
