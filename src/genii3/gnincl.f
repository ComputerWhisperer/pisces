      SUBROUTINE GNINCL(wch, fname, lerr)
      include 'genidf.inc'
      integer       wch
      character*(*) fname
      logical       lerr
c Thu Sep 14 00:26:28 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1989 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c "gnincl":
c
c Calling sequence:
c     call gnincl(wch, fname, lerr)
c
c Where:
c
c Original: Michael Eldredge -- Stanford (may 89)
c-----------------------------------------------------------------------
c   common
c-----------------------------------------------------------------------
      include 'common.inc'
c-----------------------------------------------------------------------
c   local variables
c-----------------------------------------------------------------------
      logical ljunk
      integer lu
      integer ierr,ll
c-----------------------------------------------------------------------
c

c.. Is it a "close current file" command?
      if (wch.eq.0) then
c...assume no more files on stack
          lerr=.false.
c...all done with stack
          if (nincls.le.0) return

          if (inpnam(1:1).ne.' ') call clsfl(luinp, ljunk, ierr)

          linum=nlins(nincls)
          luinp=luins(nincls)
          inpnam=incfls(nincls)
          iactiv=iacts(nincls)

          nincls = nincls - 1
c...not done, more to do..
          lerr=.true.
          return
      endif

c.. command is to Add a new file to the list.
c.... no more room -- stack full?
      if (nincls.ge.incmax) then
          ll=LEN(fname)
          call gerst(045,linum,0,fname,ll)
          lerr=.true.
          return
      endif

c we have a range of LUs from luicnl to luincl+incmax-1
      lu = luincl+nincls

      lerr=.false.
      if (fname(1:1).ne.' ') call opnfl(lu,fname,lerr,ierr,
     +               OLDFILE,FORMED,NONEXCLUS,NOAPPEND)

      if (lerr) then
          ll=LEN(fname)
          call gerst(036,linum,ierr,fname,ll)
          return
      endif

c.. push/save the new file information
      nincls = nincls + 1

      luins(nincls)  = luinp
      nlins(nincls)  = linum
      incfls(nincls) = inpnam
      iacts(nincls)  = iactiv

      inpnam = fname
      luinp  = lu
      linum  = 0
      iactiv = inpnam(1:1).eq.' '

      lerr=.false.
      return
      end
