      PROGRAM GENII3
      include 'genidf.inc'
c
c Thu Sep 14 00:38:21 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c genii3: (prog) grabs file names from the run string and
c        starts up GENII to do all the rest.  Success/Failure is returned
c        in the exit code.
c
c calling sequence:
c     genii text_key parsed_key parsed_input input [output]
c
c  where:
c     text_key   - key file source file.        default: "key.gni"
c     parsed_key - parsed form of the key file. default: "ukey.gni"
c     parsed_input- parsed form of input.       default: "prs.gni"
c     input      - the input file.              default: standard input.
c     output     - is the output file.          default: standard output.
c
c original: Michael Eldredge (may 83)
c Modified: Stephen E. Hansen  (may 85)
c       Return error status and check for close file errors.
c modified: Michael Eldredge (jul 87) Generalize beyoned Suprem's needs.
c       Pass key file names in as parameters.
c
c Notes:
c       + This should be the only file that needs to be modified
c         when this code is moved to a new system. (should be....)
c         See the sysf77lib funtions xargc(), xgtarg(), xexit()
c------------------------------------------------------------------
c   local variables
c------------------------------------------------------------------
      integer nargs, status, ierr
      integer key
      integer luout
      logical opout
      logical errflg
      character*(NAMRLN) namr, ukynam, keynam, inpnam, prsnam

c FUNCTIONS
      integer xargc
      logical genii
      logical gtval
c
c if no output file name give, just send stuff to the terminal.
      data luout / TTYOUT /
      data status /0/
c------------------------------------------------------------------
c   start of genii
c------------------------------------------------------------------
c.. find out how many args in the run string.
      nargs=xargc(ierr)
c
c..got too many args???
      if (nargs .gt. 5) then
          write(*,11) nargs
11        format(' *** Too many arguments to GENII (',i1,').')
          write(*, 33)
33        format(' Run as: genii3 keyfile ukeyfile',
     +           ' prsinput [input] [output]')
          status = 1
          goto 910
      endif

c... too few arguments?
      if (nargs .lt. 3) then
          write(*, 13) nargs
13        format(' *** Too few arguments to GENII (', i1,').')
c...Usage:
          write(*, 33)
          status = 1
          goto 910
      endif

c.. set up the files, run genii, close files and that's it.
      keynam = ' '
      ukynam = ' '
      prsnam = ' '
      inpnam = ' '

c..Get the 1.Key file, 2.unformatted key file, 3.temp file names
      call xgtarg(1,keynam)
      call xgtarg(2,ukynam)
      call xgtarg(3,prsnam)

      call gnilus(12,4,-1)

      if (nargs .ge. 4) then
c.. input file name
          call xgtarg(4,inpnam)
      else
          call genipm('Genii3: ', 8, 'genii3?     ', 12)
      endif

      opout = .false.
c... open the output file.
      if (nargs .eq. 5) then
c.. use the default output lu for the file.
          luout = DLUOUT
          call csetv(namr,NAMRLN,' ')
c.. get the output file name
          call xgtarg(2,namr)
          call opnfl(luout,namr,errflg,ierr,
     +               UNKNOWN,FORMED,NONEXCLUS,APPEND)
          if (errflg) then
              write(*,207) ierr
207           format(' Error #',i7,' found on output file open. ')
              status = 1
              goto 910
          endif
c.. remember that we need to close this.
          opout = .true.
      endif

c ====================================================================
c.. now start genii up.
      if (.not.genii(inpnam, prsnam, keynam, ukynam, luout))
     +    status = 2

c ====================================================================
c.. processing input is done in the calling program like:
250   if (.not.gtval(key)) goto 270
          write(*,43) key
43        format(8x,'Input key: ',i8)
          goto 250
270   continue

c ====================================================================
c clean up (close files, etc.)
      if (opout) then
          call clsfl(luout,errflg,ierr)
          if (errflg) then
              write(*,307) ierr
307           format(' Error #',i7,' found on output file close. ')
              status = 1
          endif
      endif

c
c.. Return the status to the system, 0 = ok, 1 = gag.
910   call xexit(status)
      end
