      REAL FUNCTION GETRL(IBUF,LENG)
      include 'genidf.inc'
      character*(*) ibuf
      integer leng
c Thu Sep 14 09:00:49 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: A. G. Gonzalez                         DATE:    Jul-76
c   MODIFICATIONS:
c   By: Stephen E. Hansen                              DATE:    Mar-78
c        Modify to ANSI standard FORTRAN-66.
c   By: Stephen E. Hansen                              DATE:    Apr-79
c        Modify for use with GENII.
c   By: Michael Eldredge                               DATE:    Aug-80
c        Convert to RATFOR
c   By: Stephen E. Hansen                              DATE: 02-Feb-82
c        Detect errors on incorrect placement of minus and plus signs.
c   By: Michael Eldredge                               DATE:    Jun-80
c        UNIX conversion to Fortran-77.  Use of character variables.
c   By: Michael Eldredge                               DATE:    jul-87
c       Convert to library package.
c   By: Michael Eldredge                               DATE:    mar-88
c       Fixed exponent plus sign bug.  1.0e+3 didn't work.  For that
c       matter neither did +1.0
c   By: Michael Eldredge                               DATE:    sep-89
c       Convert to ANSI Fortran-77 from ratfor
c
c   NAME: getrl
c   FUNCTION: Takes a character string representing a single precision
c             real or integer number and returns a real value.
c
c   TYPE OF ROUTINE: real function
c   CALLING SEQUENCE:
c      r=getrl(ibuf,leng)
c
c   PARAMETERS (INPUT):
c    ibuf*n        char     Array containing the character represen-
c                           tation of the number to be decoded.
c    leng           integer  The length of the character string in ibuf.
c
c   PARAMETERS (OUTPUT): (None.)
c
c   ERROR CONDITIONS:
c    005 : Invalid real number.
c
c   ROUTINES USED: gerst, cmpch
c   INCLUDE FILES: genidf.h, genicm.h
c
c   NOTES:
c       While LENG is supposed to contain the length of the character
c   string, the evaluation is terminated upon detection of either a
c   comma or a blank.  For real numbers containing an exponent, the
c   `E' indicating the start of the exponent field may be either upper
c   or lower case.
c
c---------------------------------------------------------------------
c     common area
c needed for LINUM in calls to GERST().
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   Local variables
c--------------------------------------------------------------------
c.. neg, neg exp,exp and dot seen?
      logical nsign, nexpfl, expfl, dotflg
      character ichar
      integer i,j,icntr,ecntr,iexpnt,numb
      character c1hpnt,c1hpls,c1hmns,c1hcma,c1hblk,c1hE
      character c1hint(10),c1h0,c1h9
      real    temp
c
      equivalence (c1h0,c1hint(1)) , (c1h9,c1hint(10))
c---------------------------------------------------------------------
c   Functions:
c--------------------------------------------------------------------
c.. char case ignorer.
      logical cmpch
c
c...these are all the characters that GETRL will need for the interpretation.
      data c1hpnt /'.'/ , c1hpls /'+'/ , c1hmns /'-'/
      data c1hcma /','/ , c1hblk /' '/ , c1hE   /'E'/
      data c1hint /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'/
c---------------------------------------------------------------------
c   start of getrl
c---------------------------------------------------------------------
c.. Initialize flags and the temp holder.
c... temp value holder and exponent holder
      temp=0.
      iexpnt=0
c... negative sign, exponent,neg exponent and dot (point) flags
      nsign =.false.
      expfl =.false.
      nexpfl=.false.
      dotflg=.false.
c
c.. Let's do it.  Start a the beginning and process the buffer.
c
c.. step through the buffer to the end unless a comma or a blank is
c.. detected.
      do 1000 i=1,leng
c... current character
          ichar=ibuf(i:i)
          if (ichar.eq.c1hcma .or. ichar.eq.c1hblk) goto 2000
c
c... digit?
          if (ichar.ge.c1h0 .and. ichar.le.c1h9) then
c.. Find which one.
              do 20 j=1,10
                  if (ichar.eq.c1hint(j)) goto 21
20            continue
c... get its value
21            numb=j-1
c
c..  If we're parsing the exponent field.
              if (expfl) then
c... digits in the exponent and update the exponent value
                  ecntr=ecntr+1
                  iexpnt=iexpnt*10+numb
c
c..  Or if we're parsing the digits to the right of the decimal.
              else if (dotflg) then
c... Number of digits to the right, update value
                  icntr=icntr+1
                  temp=temp+(numb/10.**icntr)
c
c..  Or we havn't seen a decimal point or an E yet.
              else
                  temp=temp*10.+numb
              endif
c
c.. If it's a decimal point
          else if (ichar.eq.c1hpnt) then
c.. If we've already seen a decimal point, flag an error.
              if (dotflg) then
                  call gerst(005,linum,0,ibuf,leng)
                  return
              endif
c
c.. If we're parsing the exponent field, flag an error.
              if (expfl) then
                  call gerst(005,linum,0,ibuf,leng)
                  return
              endif
c
c.. Number of digits to the right of the point.
c.. Remember that we've seen a decimal point.
              icntr=0
              dotflg=.true.
c
c.. Plus sign?
           else if (ichar.eq.c1hpls) then
c.. If the plus is not the first character in either the number
c.. or the exponent field, then flag an error.
c if (i.ne.1 .or.  (expfl .and. ecntr.ne.0))
              if (i.ne.1 .and. .not.(expfl .and. ecntr.eq.0)) then
                  call gerst(005,linum,0,ibuf,leng)
                  return
              endif
c
c... Minus sign.
          else if (ichar.eq.c1hmns) then
c... If we are parsing the exponent field.
              if (expfl) then
c.. If it's not the first character in the field.
                  if (ecntr.ne.0) then
                      call gerst(005,linum,0,ibuf,leng)
                      return
                  endif
c..... Negative exponent flag.
                  nexpfl=.true.
c.. must be a negative number
              else
c.. If it's not the first character in the string.
                  if (i.ne.1) then
                      call gerst(005,linum,0,ibuf,leng)
                      return
                  endif
                  nsign=.true.
              endif
c
c.. An Exponent indicator.
      else if (cmpch(ichar,c1hE)) then
c.. If there's more than one.
          if (expfl) then
              call gerst(005,linum,0,ibuf,leng)
              return
          endif
          expfl=.true.
          ecntr=0
c
c.. Error, I don't recognize the character.
      else
          call gerst(005,linum,0,ibuf,leng)
          return
      endif
c
c....bottom of the big loop over each character
1000  continue
c
c.. That's all the chars.  Let's finish things up.
2000  if (nexpfl) iexpnt=-iexpnt
      if (nsign)  temp=-temp
      getrl=temp*(10.**iexpnt)
c
      return
      end
