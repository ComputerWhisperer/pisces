      SUBROUTINE GERST(NUM,LINE,IERR,ISTR,LENG)
      include 'genidf.inc'
      integer num,line,ierr,leng
      character*(ERMESLN) istr
c Thu Sep 14 01:23:22 PDT 1989 (dredge--stanford)
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
c   DATE: Aug. 28, 1979
c
c   MODIFICATIONS:
c
c   Stephen E. Hansen  May 12, 1980
c     Add line storage
c   Michael Eldredge  Sept. 4, 1980
c     Convert to RATFOR
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c     Use of character variables.
c   Michael Eldredge (jul 87)  Convert to simple library set.
c
c   NAME: GERST
c
c   FUNCTION: Used for logging error and warnings for GENII, also
c             to initialize error flags and storage.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL GERST(NUM,LINE,IERR,ISTR,LENG)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       NUM    INTEGER       The error or warning number, or flag to
c                            initialize.
c                             num < 0 : warning number
c                             num > 0 : error number
c                             num = 0 : initialize buffers and flags.
c       LINE   INTEGER       Line number in which error occurs, zero
c                            if not relevent.
c       IERR   INTEGER       Optional number to be written with the
c                            error or warning message.
c       ISTR   character*n   String to write out, usually a portion
c                            of the line where the error occurs.
c       LENG    INTEGER       The number of characters in ISTR.
c
c   ERROR CONDITIONS: None.
c
c   NOTES: None.
c
c   ROUTINES USED: None.
c
c   MACRO FILES:  GENIDF, COMMON
c
c---------------------------------------------------------------------
c     common area
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables.
c---------------------------------------------------------------------
      integer i
      character blnk
      data blnk/' '/
c--------------------------------------------------------------------
c
c   start of gerst.
c
c---------------------------------------------------------------------
c.. if num == 0 then initialize.
      if (num.eq.0) then
          erpntr=0
c.. no error, warnings or EOF yet.
          errflg=.false.
          wrnflg=.false.
          eofflg=.false.

c.. save all the information that we got.
      else
c.. if we've still got room to store the error.
          if (erpntr.lt.ERMAX) then
c..store error number, line and optional value
              erpntr=erpntr+1
              err(1,erpntr)=num
              err(2,erpntr)=line
              err(3,erpntr)=ierr
c.. careful since negative of leng is ok.
              i = iabs(leng)
c.. no more than @ERMESLN chars.
              i=min0(i,ERMESLN)
              if (i.gt.0) then
                  call cpack(istr,estrng(erpntr),i)
              else
                  call csetv(estrng(erpntr),ERMESLN,blnk)
              endif
              if (leng .lt. 0) i = -i
c.. store number of chars.
              eslen(erpntr)=i
          endif
          wrnflg=((num .lt. 0) .or. wrnflg)
          errflg=((num .gt. 0) .or. errflg)
      endif

      return
      end
