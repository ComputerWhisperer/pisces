      SUBROUTINE PARKY(FIELD1,FLD1LN,FIELD2,FIELD3,RFLD4,LFLD4,CFLD4,
     +      CFLD4L,LCARD)
      include 'genidf.inc'
      logical lfld4,lcard
      integer fld1ln,field2,field3,cfld4l
      character*(CPCVL) field1,cfld4
      real    rfld4
c Thu Sep 14 17:12:56 PDT 1989 (dredge--stanford)
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
c   DATE: Jan. 26, 1982
c
c   MODIFICATIONS:
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c      Use of character variables.
c   Michael Eldredge (jul 87) convert to be a library package.
c       Use SKIPC() .and. FINDC() for skipping and searching.
c
c   NAME: PARKY
c
c   FUNCTION: Reads an parses a line from the formated key file for
c             GENII initialization.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL PARKY(FIELD1,FLD1LN,FIELD2,FIELD3,RFLD4,LFLD4,CFLD4,
c                                                    CFLD4L,CARD)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       FLD1LN INTEGER        The length of the field1 array.
c       CFLD4L INTEGER        The length of the cfld4 array.
c
c     (OUTPUT)
c
c       FIELD1 character*n   The name of the card or parameter.
c       FIELD2 INTEGER       The card key id or the parameter type.
c       FIELD3 INTEGER       The parameter location or the optional
c                            card alias reference.
c       RFLD4  REAL          The default for real parameters.
c       LFLD4  LOGICAL       The default for logical parameters.
c       CFLD4  character*n   The default for character parameters.
c       CARD   LOGICAL       True if card, false if parameter.
c
c   ERROR CONDITIONS:
c
c    006 : Unexpected end-of-file detected during parse of formatted
c          key file.
c    044 : Default logical value was specified incorrectly in the key
c          file.
c
c   NOTES:
c
c       None.
c
c   ROUTINES USED:
c
c       GERST, REDLN, CMPCH, GETRL, SKIPC, FINDC
c
c   MACRO FILES:  GENIDF, COMMON
c
c--------------------------------------------------------------------
c   common
c--------------------------------------------------------------------
      include 'common.inc'
c--------------------------------------------------------------------
c   local variables
c--------------------------------------------------------------------
      logical commnt,eol,cmpch
      integer l,len,lpntr,p1
      character*(LINELN) line
      character chr
      character c1hstr,c1hblk,c1hT,c1hF,c1hL,c1hN,c1hC
c--------------------------------------------------------------------
c   Functions
c--------------------------------------------------------------------
      real    getrl

      data c1hstr /'*'/ , c1hblk /' '/
      data c1hT/'T'/ , c1hF/'F'/ , c1hL/'L'/ , c1hN/'N'/ , c1hC/'C'/
c--------------------------------------------------------------------
c
c   start of parky
c
c--------------------------------------------------------------------
c.. get the next line from the formatted key file.
c.. until we get a non-blank, non-comment line
c.. get the next line.
1000      call redln(lukyf,line)

c.. if there were problems or no line then return
          if (eofflg) return

c.. if we got a line, increment the count.
          linum=linum+1

c.. if we succesfully got a non-comment line, find its length.
          commnt=(line(1:1).eq.c1hstr)
          if (.not.commnt) then
              do 2200 len=LINELN,1,-1
                  if (line(len:len).ne.c1hblk) goto 2210
2200          continue
2210          commnt=(len.eq.0)
          endif
      if (commnt) goto 1000

c.. if we got a good line from the file start parsing it.

c.. initialize a few things.
c.. blank the name field.
      call csetv(field1,fld1ln,c1hblk)
c.. blank the char defalt.
      call csetv(cfld4 ,cfld4l,c1hblk)
      field2=0
      field3=0
c.. default default
      lfld4 =.false.
c.. default default
      rfld4 =0.
c.. set the end of line flag false.
      eol   =.false.
c.. assume initially that this is a parameter line.
      lcard  =.false.

c--------------------------------------------------------------------
c
c  get the first field.
c
c--------------------------------------------------------------------
c.. the first field is the name field, let's get the locations of
c.. the first and last characters in the field

c.. skip any leading spaces.
      lpntr=1
      call skipc(lpntr,len,line,eol,SPACE)
c.. first character.
      p1=lpntr

c.. skip until blank.
      call findc(lpntr,len,line,eol,SPACE)

c.. now move the first CPNAM characters into the field1 array.
      l=min0(fld1ln,lpntr-p1)
      field1(1:l) = line(p1:p1+l-1)

c--------------------------------------------------------------------
c
c   get the second field
c
c--------------------------------------------------------------------
c.. skip any blanks
      call skipc(lpntr,len,line,eol,SPACE)

c.. if an end of line was detected
      if (eol) then
c.. flag an error
          call gerst(006,linum,0,line,lpntr)
c.. and return
          return
      endif

c.. get the first character of the field.
      p1=lpntr
      chr=line(p1:p1)

c.. find the end of the field.
      call findc(lpntr,len,line,eol,SPACE)

c.. if it's an `l', `n', or a `c' then it is a parameter type.
c.. logical
      if (cmpch(chr,c1hL)) then
          field2=LOGTYPE
c.. numerical
      else if (cmpch(chr,c1hN)) then
          field2=NUMTYPE
c.. character
      else if (cmpch(chr,c1hC)) then
          field2=CHRTYPE

c.. if it's not one of those then it had better be a number.
      else
c.. parse the number.
c.. nearest integer.
          field2=nint( getrl(line(p1:p1),lpntr-p1) )

c.. if an error was detected in getrl then return.
          if (errflg)  return

c.. we're looking at a card line
          lcard=.true.
       endif

c--------------------------------------------------------------------
c
c   third field
c
c--------------------------------------------------------------------
c.. skip any blanks
      call skipc(lpntr,len,line,eol,SPACE)

c.. if an end of line was detected.
      if (eol) return

c.. remember the start of the field.
      p1=lpntr

c.. find the end of the field.
      call findc(lpntr,len,line,eol,SPACE)


c.. parse the number in the field
      field3=int( getrl(line(p1:p1),lpntr-p1) + 0.5 )

c--------------------------------------------------------------------
c
c   fourth field
c
c--------------------------------------------------------------------
c.. if this is a card line then there is no fourth field.
      if (.not.lcard) then
c.. find the start of the field
          call skipc(lpntr,len,line,eol,SPACE)

c.. if an end of line was detected then return.
          if (eol) return

c.. remember the start of the field.
          p1=lpntr

c.. find the end of the field
          call findc(lpntr,len,line,eol,SPACE)

c.. now get the default depending on the parameter type.

c.. numerical type.
          if (field2 .eq. NUMTYPE) then
              rfld4=getrl(line(p1:p1),lpntr)

c.. logical type.
          else if (field2 .eq. LOGTYPE) then
              if (cmpch(line(p1:p1),c1hT)) then
                  lfld4=.true.
              else if (cmpch(line(p1:p1),c1hF)) then
                  lfld4=.false.
              else
c.. flag an error, we couldn't understand the value.
                  call gerst(16,linum,0,line(p1:p1),lpntr)
              endif

c.. character type.
          else if (field2 .eq. CHRTYPE) then
c.. length to store.
              l=min0(cfld4l,lpntr-p1)
              cfld4(1:l) = line(p1:p1+l-1)
          endif
      endif

c--------------------------------------------------------------------
c
c   that's it boss.
c
c--------------------------------------------------------------------
      return
c.. of parky.
      end
