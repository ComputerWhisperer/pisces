       LOGICAL FUNCTION GTVAL(key)
      include 'genidf.inc'
        integer  key
c Tue Mar  6 16:55:25 PST 1990 (dredge--stanford)
c---------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen
c   DATE: 18 July 1979
c
c   MODIFICATIONS:
c
c   Stephen E. Hansen  Oct. 20, 1980
c     Convert to RATFOR and update.
c   Stephen E. Hansen  Feb. 12, 1982
c     Update to up OPNFL
c   Michael Eldredge (jul 83) UNIX conversion to FORTRAN-77.
c     Updated use of OPNFL
c   Michael Eldredge (jul 87) Convert to library package.
c       changed to logical function; returning error status
c   Modified: Michael Eldredge -- stanford (mar 90) Indicate that
c       we did or did not open the prs file.
c
c   NAME: GTVAL
c
c   FUNCTION: To fetch the next card type and its parameters from the
c             parsed input file.
c
c   TYPE OF ROUTINE: LOGICAL FUNCTION
c
c   CALLING SEQUENCE:
c
c     <logical> = GTVAL(key)
c
c   INPUT: (none).
c   OUTPUT:
c     integer key       - keyid of current card.
c     return            - True.  Got a new line.
c                         False. Error or EOF found.
c
c   ERROR CONDITIONS:
c
c     Reports file open errors on opens of the parsed input file.
c     Reports file read errors on reads from the parsed input file.
c
c   NOTES:
c
c   GTVAL reads a record from the file of parsed data output generated
c   by GENII.  Each record corresponds to a single card in the input
c   deck and consists of the cards KEYID number, the logical, numerical
c   and character parameter values associated with that card and three
c   logical arrays whose elements indicate whether or not the corre-
c   sponding input variable was actually specified in the input deck.
c   The line number of the current line is returned in LINUM.  If a
c   zero value in LINUM is passed into GTVAL, then the parsed input
c   file specified by FILNM is opened/rewound and the first record is
c   read.  If an end-of-file is detected, then GTVAL closes the file
c   and returns the logical variable EOFFLG with a true value.  If an
c   error is detected on an attempt to open or read from the file,
c   then GTVAL will write out the appropriate error message, set the
c   ERRFLG true, and return.
c
c   The record format of the parsed data file is described below.
c
c   The first value is the card's KEYID.
c   The next 20 values contain the card's logical parameters.
c   The next 20 values contain the card's numerical parameters.
c   The next 40 words contain the card's character parameter values,
c   or the character string, if the card does not use parameters.
c
c   ROUTINES USED: OPNFL, CLSFL
c
c   MACRO FILES:  DEFINE, COMMON
c
c   ALGORITHM:
c
c     1) If LINUM has a value of zero then open the parsed input file.
c        Write out an error message if an open error is detected.
c
c     2) Increment the line count in LINUM.
c
c     3) Read in a record from the parsed input file.  Write out an
c        error message if a read error is detected.
c
c---------------------------------------------------------------------
c   common
c
c---------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables
c---------------------------------------------------------------------
       logical filerr
       integer i, ierr
       logical lerr

c -- FUNCTIONS
       logical gnget1
c---------------------------------------------------------------------
c   start of GTVAL
c---------------------------------------------------------------------

c... assume error
       gtval = .false.
c
       if (eofflg) then
C..don't send out warning, just let the user know (maybe again)
C... that there is no more input.
C           write(luttyo,11) ierr
C           if (luout.ne.luttyo) write(luout,5) ierr
C5          format(/' Error: Genii:get value',
C     +             ' called after input exhausted.')
           return
       endif
	 
c... not a good key
       key   = -3

c test interactive stuff here.  Integrate code later....
       if (iactiv) then
100        gtval = gnget1(lerr)
c..ignore errors (they've been flagged, so let them try again)
           if (lerr) goto 100
           key = keyid
c          if (key.le.0) key = -1
           return
       endif

c.. Check for a zero value in LINUM.  If zero then open or rewind the
c.. file and reset the line count.
       if (linum .eq. 0) then
c.. open the file.
c.. status=old,form=formatted,no-append.
           call opnfl(luprs,prsfil,filerr,ierr,
     +                OLDFILE,FORMED,EXCLUSIVE,NOAPPEND)
c
c.. if an error was detected on the open.
           if (filerr) then
               write(luttyo,11) ierr
               if (luout.ne.luttyo) write(luout,11) ierr
11             format(/' File system error #',I6,
     +          ' detected on attempt to open the parsed input file.')
	       opprs = .false.
               gtval = .false.
               return
	   else
	       opprs = .true.
           endif
       endif
c
c---------------------------------------------------------------------
c   Get the next record from the file.
c---------------------------------------------------------------------
c.. if the linum is not a -1.
      if (linum.ne.-1) then
c.. Increment the line count.
          linum=linum+1
c
c.. Read in a line.
c.. unformatted output.
cread (luprs,iostat=ierr,end=20,err=30) keyid,lval,lspecd,rval,
c                                    rspecd,cval,cspecd,tval,tspecd
c
c.. formattted output.
          read(luprs, *, err=30, end=20, iostat=ierr) keyid
          read(luprs, *, err=30, iostat=ierr) (lspecd(i),i=1,LPC)
          read(luprs, *, err=30, iostat=ierr) (rspecd(i),i=1,RPC)
          read(luprs, *, err=30, iostat=ierr) (cspecd(i),i=1,CVPC)
          read(luprs, *, err=30, iostat=ierr) tspecd
          read(luprs, *, err=30, iostat=ierr) (lval(i),i=1,LPC)
          read(luprs, *, err=30, iostat=ierr) (rval(i),i=1,RPC)
          do 1020 i = 1, CVPC
              read(luprs, fmt=12, err=30, iostat=ierr) cval(i)
1020      continue
          read(luprs, fmt=12, err=30, iostat=ierr) tval
12        format(a)
c
c.. now for a successful return.
          key   = keyid
c... error found in genii() run ?
          gtval = (keyid.ne.-1)
c         gtval = .true.
          return
      endif
c
c---------------------------------------------------------------------
c   here if an eof was detected, or if one is to be forced.
c---------------------------------------------------------------------
c.. clean up.
20    call gniend
 
c... EOF (like an error but different)
      gtval = .false.
      return
c
c---------------------------------------------------------------------
c   error detected on read.
c---------------------------------------------------------------------
30    write (luttyo,31) linum,ierr
      if (luout.ne.luttyo) write (luout,31) linum,ierr
31    format(/' Error in line #',I4,
     +    /' File system error #',I6,' detected on a read from the',
     +     ' parsed input file.')
      gtval = .false.
      return
c
      end


c============================================================================
c
c Included here because some loaders will not search for and load from a
c   library - BLOCK DATA commons.  And GTVAL is needed by the user.  We
c   also dummy load this with GENII() in case it is in a different program
c   than GTVAL.  dredge jul 87.

       BLOCK DATA GENBK
      include 'genidf.inc'
c.. date code: 13 jul 87 (dredge--stanford)
c---------------------------------------------------------------------
c
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c
c---------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen
c   DATE: July  8, 1981
c
c   MODIFICATIONS:
c     Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c       Use of character variables.
c     Michael Eldredge (jul 87) Library-ize conversion. Name change.
cON
cTOF
c
c   NAME: GENIBK
c
cUOFF
cUON
c
c   FUNCTION: Defines and initializes the program common areas.
c
c   TYPE OF ROUTINE: BLOCK DATA
c
c   CALLING SEQUENCE:  None.
c
c   PARAMETERS: None.
c
c   ERROR CONDITIONS:  None.
c
c   NOTES: None.
c
c   ROUTINES USED: None.
c
cUOFF
c   MACRO FILES:  GENIDF, COMMON
cOFF
c---------------------------------------------------------------------
c
c   internal (private) common area
c
c---------------------------------------------------------------------
      include 'common.inc'
c----------------------------------------------------------------------
c
c     Data initialization of the common area.
c
c---------------------------------------------------------------------
c.. LU initialization -- this should be reset by the user.
       data luttyi/ TTYIN / , luttyo/ TTYOUT /
       data luinp / DLUINP / , luout / DLUOUT /
       data luprs / DLUPRS / , lukyf / DLUKYF / , lukyu / DLUKYU /
c
       data errflg/.false./ , wrnflg/.false./ , eofflg/.false./
       data keyid /-999/ , linum/0/
c
c.. filename initialization. These is always overwritten by the genii() call.
       data  PRSFIL/'rawin.gni'/

       data  iactiv/.false./

c.. number/depth of input files
       data nincls/0/
c.. always invalid!
       data inckey/-1/
c
       end
