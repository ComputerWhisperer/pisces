      subroutine opnfl(lu,cname,opnerr,ierr,status,form,exclus,appnd)
      include 'genidf.inc'
      logical   opnerr,form,exclus,appnd
      integer   lu,ierr,status
      character*(*) cname
c-----------------------------------------------------------------------
c   Copyright 1983 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c Thu Sep 14 00:20:47 PDT 1989 (dredge--stanford)
c
c opnfl: (subroutine) General file opener for suprem.  All the system
c        file opening dependancies should be in this routine. The
c        version here is very short, using array assignments to
c        set up the proper characteristics.  This version, also,
c        does not use the EXCLUSIVE/NON-EXCLUSIVE parameter since
c        this is a non-standard open parameter.
c        This also does APPEND/NO-APPEND opens.
c
c calling sequence:
c        call opnfl(lu,cname,opnerr,ierr,status,form,exclus,appnd)
c
c  where:
c    lu     - (int) lu number for assign to file once it's open.
c    cname  - (char*(*)) char buffer containing the name of the
c             file to open.
c    opnerr - (log) flag set TRUE if an error occurred on the attemped
c             open of the file.
c    ierr   - (int) I/O error number returned if an error occurred (opnerr
c             is TRUE).
c    status - (int) The status that the file should have on open.
c             = 1 --old.
c             = 2 --new.
c             = 3 --unknown.
c             = 4 --scratch.
c    form   - (log) FORMATTED/UNFORMATTED open flag.
c             = .true. --open the file formatted.
c             = .false. --open the file unformatted.
c    exclus - (log) EXCLUSIVE/NONEXCLUSIVE open flag.
c             = .true.  -- open in exclusive mode.
c             = .flase. -- open in non-exclusive mode.
c    appnd  - (log)   To open in append mode.
c             = .true.  -- open in append mode (leave file at EOF)
c             = .false. -- don't open in append (leave at record 0)
c
c notes:
c    1. Unix 4.1 opened files in append mode (at EOF), 4.2 now opens
c       them at the beginning.  Be sure you know what your system does
c       and modify this routine accordingly.
c
c written: Michael Eldredge (apr 83)
c mod # 1: Michael Eldredge (may 83)
c       Add APPEND/NO-APPEND parameter.
c mod # 2: Michael Eldredge (jul 83)
c       Scratch status added.
c mod # 3: Stephen Hansen  (feb 85)
c       Set no append default open for 4.2Unix
c mod # 4: Michael Eldredge (jul 87)
c       convert for libary package.
c
c--------------------------------------------------------------------
c   local variables
c--------------------------------------------------------------------
      character*(NAMRLN) fname
      logical isdev
      character x
c
      character*12 aform
      character*8  astat
      character*13 aexcl
c
      character*3  old
      character*3  new
      character*7  unknow
      character*7  scrat
      character*9  formtd
      character*11 unform
      character*9  exclsv
      character*12 nonexc
c
c..header............1234:6789:1234:.........to help count characters.
      data old    / 'old'          /
      data new    / 'new'          /
      data unknow / 'unknown'      /
      data scrat  / 'scratch'      /
      data formtd / 'formatted'    /
      data unform / 'unformatted'  /
      data exclsv / 'exclusive'    /
      data nonexc / 'nonexclusive' /
c--------------------------------------------------------------------
c   start of opnfl
c--------------------------------------------------------------------
c.. initialize the open error flag to false, (assume success).
      opnerr=.false.
c.. we can't tell, so...
      isdev =.false.
      ierr=0
c
      if(status .eq. 1) then
          astat=old
      else if(status .eq. 2) then
          astat=new
      else if(status .eq. 4) then
          astat=scrat
      else
          astat=unknow
      endif
c
      if(form) then
          aform=formtd
      else
          aform=unform
      endif
c
c..  note: this parameter is not used in U*ix version.
      if(exclus) then
          aexcl=exclsv
      else
          aexcl=nonexc
      endif
c
c.. clear file name
      fname = ' '
c.. see if the name is a logical DEFINE
      call xgeten(cname,fname)
      if (fname(1:1) .eq. ' ') fname = cname

C:SYSTEM=unix
      isdev=(fname(1:4) .eq. '/dev')
C.SYSTEM

      open(lu,file=fname,err=11,iostat=ierr,status=astat,form=aform)

c..it opened, now if the file is old, deal with the APPEND/NO-APPEND stuff.
c--vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv------
c..<<this code for machines that don't APPEND open by default>>
      if ((status.eq.1 .or. status.eq.3).and.appnd.and.
     +             .not.isdev) then
          if (form) then
310           read(lu,101,err=11,end=100,iostat=ierr)
101           format(1x)
              goto 310
          else
320           read(lu,err=11,end=100,iostat=ierr) x
              goto 320
          endif
      endif
100   continue
c--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----
c
c--vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv------
c..<<this code for machines that do APPEND opens by default>>
c..!!for the 4.1unix. If file is a device, don't try to rewind.
c if ((status.eq.1 .or. status.eq.3) .and.
c +  .not. appnd .and. .not.isdef) rewind(lu)
c--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----

      return
c
c...error found.....
11    opnerr=.true.
      return
      end
