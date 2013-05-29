      SUBROUTINE PARSE(LPARMS,LPARTYP,LPARLOC,LNPARMS,LINE,LPF,LPL)
      include 'genidf.inc'
      character*(CPNAM) lparms(LNPARMS)
      integer lnparms,lpartyp(LNPARMS),lparloc(LNPARMS)
      character*(LINELN) line
      integer lpf,lpl
c Tue Apr  3 23:16:41 PDT 1990 (dredge--stanford)
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
c   DATE: Aug. 09, 1977
c
c   MODIFICATIONS:
c
c   Stephen E. Hansen  Nov. 16, 1977
c     Restructure
c   Stephen E. Hansen  Apr. 13, 1979
c     Adapt to GENII
c   Michael Eldredge  Sept. 02, 1980
c     Convert to RATFOR
c   Stephen E. Hansen  Jan. 31, 1982
c     Restructure
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c      Use of character variables.
c   Stephen E. Hansen  Dec. 12, 1983
c      Use SCOMP to distinguish between exact and subset matches.
c   Michael Eldredge (jul 87)  Convert to library package. Add SKIPC/FINDC
c       calls to replace the many explicit loops.
c
c   NAME: PARSE
c
c   FUNCTION: Parses the parameters on a GENII input line.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL PARSE(PARMS,PARTYP,PARLOC,NPARMS,LINE,LPF,LPL)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       PARMS  character*CPNAM (NPARS) List of valid parameter names.
c       PARTYP INTEGER(NPARMS) Array containing the type of value
c                            associated with the parameters
c                            (1=numerical, 2=logical, 3=character).
c       PARLOC INTEGER(NPARMS) Array containing the index of the
c                            location in the real, logical, or character
c                            array that receives the parameter value.
c       NPARMS INTEGER       The number of possible parameters
c                            associated with the current card type.
c       LINE   character*LINELN  Array containing the line to parse.
c       LPF    INTEGER       Pointer to first non-blank character of
c                            parameter list.
c       LPL    INTEGER       Pointer to last non-blank character of
c                            parameter list.
c
c
c   ERROR CONDITIONS:
c
c       003 : Invalid parameter specification.
c       004 : Parameter name is ambiguous.
c       007 : No value following equal sign
c       014 : No parameter after the NOT indicator.
c       017 : Parameter already specified.
c       018 : Logical parameter cannot be followed by an equal sign.
c       019 : The parameter is not of type logical.
c
c   NOTES:
c
c       PARSE is passed a parameter list for the current line and a
c   description of the valid parameter names and their types and
c   locations.  It parses the parameter list according to this
c   information and stores the parameter values in the appropriate
c   parameter data arrays.
c
c   ROUTINES USED:
c
c       GERST, SCOMP, GETRL, CPACK, SKIPC, FINDC
c
c   MACRO FILES:  GENIDF, COMMON
c
c--------------------------------------------------------------------
c
c     common area
c
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables.
c---------------------------------------------------------------------
      logical eol,exact,match,notflg,perror
      integer i,indx,iloc,ityp,len,nmatch,ptr1,ptr2
      character chr,c1hnot,c1hblk,c1hcma,c1hequ
      character c1qu1, c1qu2, c1quo
c---------------------------------------------------------------------
c   Functions
c---------------------------------------------------------------------
      real    getrl

      data    c1hblk /' '/ , c1hcma /','/ , c1hequ /'='/
c.. the .NOT. char for logicals
      data    c1hnot /'^'/
c.. single and double quotes
      data    c1qu1/''''/, c1qu2/'"'/
c---------------------------------------------------------------------
c
c   start of parse.
c
c---------------------------------------------------------------------

c.. while there is something left in the line.
1000  eol=(lpf .gt. lpl)
      if (eol) goto 2000
c.. valid parameter not found flag.
          perror=.false.
c.. NOT indicator flag.
          notflg=.false.
c.. pointer to start of string being processed.
          ptr1=lpf
c.. pointer to parameter name, type, and location.
          indx=0
c.. parameter type.
          ityp=0
c.. parameter value destination index.
          iloc=0

c.. look at the next character, if it's a NOT character then
          chr=line(ptr1:ptr1)
          if (chr .eq. c1hnot) then
c.. set the not flag true.
              notflg=.true.
c.. move the pointer past the not character.
              ptr1=ptr1+1
c.. if that's all there is
              if (ptr1 .gt. lpl) then
c.. set the end-of-line flag true.
                  eol=.true.
c.. otherwise
              else
c.. find the begining of the parameter.
                  call skipc(ptr1,lpl, line, eol, SPACE)
              endif

c.. if a end-of-line was detected.
              if (eol) then
c.. then there is no parameter after the NOT indicator.
                  call gerst(014,linum,0,line(lpf:lpf),lpl-lpf+1)
                  perror=.true.
              endif
          endif

c.. find the end of the parameter name.
          ptr2=ptr1
          if (.not.eol) then
              call findc(ptr2, lpl, line, eol, EWORD)

c.. how long is the parameter name.
              len=ptr2-ptr1
              if (len .gt. CPNAM) len=CPNAM

              exact=.false.
              match=.false.
              nmatch=0
              indx=1

c.. search the list of valid parameters.
              do 3010 i=1,lnparms
                  if (exact) goto 3011
c.. look for a match.
                  call scomp(lparms(i),CPNAM,line(ptr1:ptr1),
     +                                        len,match,exact)
c
c.. if the parameter matches one in the list.
                  if (match) then
c.. if this is an exact match or if it is not an alias
c.. of a previous match.
                      if (exact .or. nmatch .lt. 1 .or.
     +                    lparloc(i) .ne. iloc .or.
     +                    lpartyp(i) .ne. ityp) then
c.. increment the match count.
c.. and remember where you found it.
c.. remember its type
c.. and where its value goes.
                          nmatch=nmatch+1
                          indx=i
                          ityp=lpartyp(i)
                          iloc=lparloc(i)
                      endif
                  endif
3010          continue
3011          continue

c Mon Sep 11 12:34:32 PDT 1989 (dredge--stanford)
c why is 'len' changed to len+=1?  Error messsages then have "bad
c word 'foople='" but should be "bad word 'foople'".
c       len=ptr2-lpf+1

c.. if no match was found.
              if (nmatch .eq. 0) then
                  call gerst(003,linum,0,line(lpf:lpf),len)
                  perror=.true.

c.. or if no exact match was found and more than one non-exact
c.. match was found.
              else if (.not.exact .and. nmatch .gt. 1) then
                  call gerst(004,linum,0,line(lpf:lpf),len)
                  perror=.true.

c.. or we've got a live one.
              else
c.. if it is not a logical parameter and it was preceeded by
c.. a not flag then flag an error.
                  if (ityp .ne. LOGTYPE .and. notflg)
     +                call gerst(014,linum,0,line(lpf:lpf),len)
              endif
          endif

c.. find the next character not a blank or a comma after the
c.. parameter name.
          call skipc(ptr2, lpl, line, eol, EPARM)
          chr = line(ptr2:ptr2)

c.. if there is no value equated to the parameter name.
          if (eol .or. chr .ne. c1hequ) then
c.. and we know what parameter it is.
              if (.not.perror) then
c.. if it's not a logical then we needed a value.
                  if (ityp .ne. LOGTYPE) then
                      call gerst(019,linum,0,line(lpf:lpf),len)

c.. if it is a logical
                  else
c.. and we've seen it before on this line, flag an error.
                      if (lspecd(iloc)) then
                          call gerst(017,linum,0,line(lpf:lpf),len)
c.. if we haven't seen it before.
                      else
c.. set it's value.
                          lval(iloc)=.not.notflg
c.. and that it was specified.
                          lspecd(iloc)=.true.
                      endif
                  endif
              endif

c.. if an equal sign was found.
          else if (chr .eq. c1hequ) then
c.. and we know which parameter it is.
              if (.not.perror) then
c.. if this ia a logical parameter then it can't be equated
c.. to a value.
                  if (ityp .eq. LOGTYPE)
     +                call gerst(-018,linum,0,line(lpf:lpf),len)
              endif

c.. find the next non-blank character, the start of the
c.. parameter value.
              ptr1=ptr2+1
              call skipc(ptr1,lpl, line, eol, SPACE)

c.. if we're at the end of the line then we're missing a
c.. parameter after the equal sign, so if this is not a
c.. logical parameter.
              if (eol .and. ityp .ne. LOGTYPE) then
                  call gerst(007,linum,0,line(lpf:lpf),lpl-lpf+1)

c.. find the last character of the value string.
              else
		  c1quo = line(ptr1:ptr1)
c..quoted string?
		  if (c1quo.eq.c1qu1 .or. c1quo.eq.c1qu2) then
		      ptr1=ptr1+1
		      ptr2=ptr1
		      call findc(ptr2, lpl, line, eol, c1quo)
                      len=ptr2-ptr1
c..normal parameter (not quoted)
		  else
                      ptr2=ptr1
                      call findc(ptr2, lpl, line, eol, EPARM)
                      len=ptr2-ptr1
		  endif

c.. if we know what paremter this is.
                  if (.not.perror) then
c.. if this is a numerical parameter
                      if (ityp .eq. NUMTYPE) then
c.. if we have seen this one before.
                          if (rspecd(iloc)) then
                              call gerst(017,linum,0,
     +                                line(lpf:lpf),ptr2-lpf+1)
c.. we haven't
                          else
c.. get the value.
                              rval(iloc)=getrl(line(ptr1:ptr1),len)
c.. and indicate that it was specified.
                              rspecd(iloc)=.true.
                          endif

c.. or if this is a character type parameter.
                      else if (ityp .eq. CHRTYPE) then
c.. if we have seen this one before.
                          if (cspecd(iloc)) then
                              call gerst(017,linum,0,
     +                             line(lpf:lpf),ptr2-lpf+1)

c.. we haven't
                          else
c.. indicate that it was specified.
                              cspecd(iloc)=.true.

c.. and then get the value.
                              call csetv(cval(iloc),CPCVL,c1hblk)
                              call cpack(line(ptr1:ptr1),
     +                                        cval(iloc),len)
                          endif

c.. or if this is the obsolete logical format.
c                     ^^^^^^^^
c..if the check above is only a warning, then we can
c  get to here, else we never will.
                      else if (ityp .eq. LOGTYPE) then
c.. if we have seen this one before.
                          if (lspecd(iloc)) then
                              call gerst(017,linum,0,
     +                                  line(lpf:lpf),ptr2-lpf+1)

c.. we haven't
                          else
c.. indicate that it was specified.
                              lspecd(iloc)=.true.

c.. and then get the value.
                              chr = line(ptr1:ptr1)
                              if (chr.eq.'t' .or. chr.eq.'T' .or.
     +                            chr.eq.'y' .or. chr.eq.'Y' .or.
     +                            chr.eq.'o' .or. chr.eq.'O') then
                                  lval(iloc)=.true.
                              else if (chr.eq.'n' .or. chr.eq.'N' .or.
     +                                 chr.eq.'f' .or. chr.eq.'F')then
                                  lval(iloc)=.false.
                              else
                                  call gerst(044,linum,0,
     +                                       line(ptr1:ptr1),len)
                              endif
                          endif
                      endif
                  endif
              endif
c.. skip any characters not a blank or a comma.
              call findc(ptr2, lpl, line, eol, EPARM)

c.. skip any blanks or commas
              call skipc(ptr2, lpl, line, eol, EPARM)
          endif

c.. update the pointer to the first character of the next parameter
c.. and unless an end-of-line was detected, go get it.
          lpf=ptr2
          goto 1000

c.. end of line, we're done for now.
2000  continue

      return
      end
