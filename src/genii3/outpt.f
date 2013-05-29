      SUBROUTINE OUTPT
      include 'genidf.inc'
c Thu Sep 14 01:30:07 PDT 1989 (dredge--stanford)
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
c   DATE: June 28, 1979
c
c   MODIFICATIONS:
c
c   Michael ELdredge  Sept. 4, 1980
c     Convert to RATFOR
c   Michael Eldredge  June 30, 1981
c     Use FORTRAN-77 I/O
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c      Use of character variables.
c      Added TVAL for title and comment cards.
c   Michael Eldredge (jul 87)  Convert to library package.
c
c   NAME: OUTPT
c
c   FUNCTION: Outputs the parsed data from the input file to the
c             parsed input file.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL OUTPT
c
c   PARAMETERS:
c
c     None.
c
c   ERROR CONDITIONS:
c
c    039 : File system error detected on write to parsed input file.
c
c   NOTES:
c
c   Outputs the parsed data from the input file to the parsed data
c   file in the following format.
c
c     First word      = card key id number.
c     the next LPC words = the cards logical parameters.
c     the next LPC words = the logical specified flags.
c     the next RPC words = the cards real parameters.
c     the next RPC words = the real specified flags.
c     the next CPCRD chars = the cards character parameters
c     the next CVPC words = the character specified flags.
c     the next LINELN chars = the possible title/comment value.
c     the next word      = the logical flag indicating if above given.
c
c   This file is setup to be read by the GTVAL routine, called by
c   the main program using GENII.
c
c   ROUTINES USED:
c
c       GERST
c
c   MACRO FILES:  GENIDF, COMMON
c
c   ALGORITHM:
c
c     1)  Write out the data to the parsed data file
c
c     2)  If an error was detected, close the file and set the eofflg.
c
c---------------------------------------------------------------------
c     common area
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables
c--------------------------------------------------------------------
      integer i, ierr, l
c----------------------------------------------------------------------
c   start of outpt.
c----------------------------------------------------------------------
c.. for debugling.
c write(ludia,77) keyid,lval,lspecd,rval,rspecd,cval,cspecd,tval,tspecd
c 77 format(/' keyid =',i5,
c           /' lval  =',LPC l2,
c           /' lspecd=',LPC l2,
c           /' rval  ='/1p RPC g16.6,
c           /' rspecd=',RPC l3,
c           /' cval  ='/,a ,
c           /' cspecd=',CVPC l2,
c           /' tval  ='/,a ,
c           /' tspecd=',l2,
c           /)
c
c
c.. write it out.
c.. unformatted output.
cwrite (luprs,IOSTAT=ierr,ERR=99) keyid,lval,lspecd,rval,        _
c                                      rspecd,cval,cspecd,tval,tspecd
c
c.. formattted output.
      write(luprs, *, err=99, iostat=ierr) keyid
      write(luprs, *, err=99, iostat=ierr) (lspecd(i),i=1,LPC)
      write(luprs, *, err=99, iostat=ierr) (rspecd(i),i=1,RPC)
      write(luprs, *, err=99, iostat=ierr) (cspecd(i),i=1,CVPC)
      write(luprs, *, err=99, iostat=ierr) tspecd
      write(luprs, *, err=99, iostat=ierr) (lval(i),i=1,LPC)
      write(luprs, *, err=99, iostat=ierr) (rval(i),i=1,RPC)

      do 200 i = 1, CVPC
          do 220 l = CPCVL, 1, -1
              if (cval(i)(l:l).ne.' ') goto 221
220       continue
221       if (l.lt.1) l = 1
          write(luprs, fmt=11, err=99, iostat=ierr) cval(i)(1:l)
200   continue

      do 240 l = LINELN, 1, -1
          if (tval(l:l).ne.' ') goto 241
240   continue
241   if (l.lt.1) l = 1
      write(luprs, fmt=11, err=99, iostat=ierr) tval(1:l)
11    format(a)
      return
c
c
c.. we go here if there was an error during the write.
99    call gerst(039,linum,ierr,' ',0)
      return

      end
