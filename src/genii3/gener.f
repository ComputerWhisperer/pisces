      SUBROUTINE GENER
      include 'genidf.inc'
c Wed Nov  8 22:39:59 PST 1989 (dredge--stanford)
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
c   Michael Eldredge  Sept. 4, 1980
c     Convert to RATFOR
c   Stephen E. Hansen Jan. 26, 1982
c     Restructure flow of routine.
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c   Michael Eldredge (jul 87)  Common rearrange.
c   Michael Eldredge (aug 89)  Back to f77 (no ratfor)
c   Michael Eldredge (nov 89)  Different format for interactive.
c
c   NAME: GENER
c
c   FUNCTION: Outputs all errors or warnings currently logged in the
c             err array.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL GENER
c
c   PARAMETERS: None.
c
c   ERROR CONDITIONS:
c       Error specified not listed.
c
c   NOTES: None.
c
c   ROUTINES USED: None.
c
c   MACRO FILES:  GENIDF, COMMON
c
c---------------------------------------------------------------------
c
c     Genii private common area
c
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c
c   local variables.
c
c--------------------------------------------------------------------
      integer indx,line,ierr,len,num
c--------------------------------------------------------------------
c
c   start of gener.
c
c----------------------------------------------------------------------
c.. as long there are errors to be processed.
      do 100 indx=1,erpntr
          if (err(1,indx).eq.0) goto 200

c.. the error number and line number of the error.
          num=err(1,indx)
          line=err(2,indx)
c.. optional number associated with the error.
          ierr=err(3,indx)
c.. number of chars in string.
          len =eslen(indx)

c.. first we write to luout and then to lutty if different.
          call gnwrer(luout,num,line,ierr,estrng(indx),len,iactiv)
          if (luout.ne.luttyo)
     +       call gnwrer(luttyo,num,line,ierr,estrng(indx),len,iactiv)
100   continue

c.. gool-ooly! we're done.
200   call gerst(0,0,0,' ',0)
      return
      end
c ===================================================================
c "GNWRER": Genii Write Error message
c
      SUBROUTINE GNWRER(lu,num,line,ierr,mess,messln,simpl)
      integer lu,num,line,ierr
      character*(*) mess
      integer messln
      logical simpl
c -------------------------------------------------------------------
c  Local variables
c -------------------------------------------------------------------
      integer n

c -------------------------------------------------------------------
c  Start:
c -------------------------------------------------------------------
      n = abs(num)

c.. write out the error/warning number, if it is associated
c.. with a particular input line then write out the line
c.. number also,otherwise write out only the error number.
      if (.not.simpl) write(lu,1009)
1009  format(1x)
      if (line.le.0 .or. simpl) then
          if (num.gt.0) then
              write(lu,1011) n
1011          format(' ** ERROR #',i3,' **')
          else
              write(lu,1012) n
1012          format(' ** warning #',i3,' **')
          endif
      else
          if (num.gt.0) then
              write(lu,1013) n,line
1013          format(' ** ERROR #',i3,', in line #',i3,' **')
          else
              write(lu,1014) n,line
1014          format(' ** warning #',i3,', in line #',i3,' **')
          endif
      endif


c.. now write out the error/warning message itself.
      if (n.eq.1) then
        write(lu, 1)
1       format(' * Invalid card type specification *')

      else if (n.eq.2) then
        write(lu, 2)
2       format(' * Card type is ambiguous *')

      else if (n.eq.3) then
        write(lu, 3)
3       format(' * Invalid parameter specification *')

      else if (n.eq.4) then
        write(lu, 4)
4       format(' * Parameter name is ambiguous *')

      else if (n.eq.5) then
        write(lu, 5)
5       format(' * Invalid real or integer specification *')

      else if (n.eq.6) then
        write(lu, 6)
6       format(' * Unexpected end-of-line detected during parse of',
     +      ' formatted key file. *')

      else if (n.eq.7) then
        write(lu, 7)
7       format(' * No value following equal sign. *')

      else if (n.eq.8) then
        write(lu, 8)
8       format(' * A parameter specification follows a card alias. *')

      else if (n.eq.9) then
        write(lu, 9) ierr
9       format(' * An invalid status code of ',i6,' was specified on a',
     +      ' call to the GENOF routine *')

      else if (n.eq.10) then
        write(lu,10) ierr
10      format(' * The number of card types in the specification ',
     +      ' file exceeds ',i3,' *')

      else if (n.eq.11) then
        write(lu,11) ierr
11      format(' * The number of total parameters in the specification',
     +      ' file exceeds ',i3,' *')

      else if (n.eq.12) then
        write(lu,12) ierr
12      format(' * During input initialization, card id#',i4,
     +      ' referenced a parameter',
     +     /'   list not previously defined *')

      else if (n.eq.13) then
        write(lu,13)
13      format(' * Unexpected END-OF-FILE detected in formatted',
     +      ' specification file *')

      else if (n.eq.14) then
        write(lu,14)
14      format(' * No parameter after the NOT indicator *')

      else if (n.eq.15) then
        write(lu,15)
15      format(' * First non-comment line in keyfile not a card name *')

      else if (n.eq.16) then
        write(lu,16)
16      format(' * Default logical value was specified incorrectly',
     +      ' in the key file. *')

      else if (n.eq.17) then
        write(lu,17)
17      format(' * Parameter already specified on this card *')

      else if (n.eq.18) then
        write(lu,18) mess(1:messln), mess(1:messln)
18      format(' * Old format. ',
     +      'Logical should not be followed by an equal sign.'
     +      ,/,'   Try: "',a,'" for True; "^',a,'" for False *')

      else if (n.eq.19) then
        write(lu,19)
19      format(' * Parameter is not of type logical *')

      else if (n.eq.30) then
        write(lu,30) ierr
30      format(' * File system error #',i6,
     +      ' detected on attempt to open',
     +      ' the'/'   unformatted specification file *')

      else if (n.eq.31) then
        write(lu,31)
31      format(' * Unexpected end-of-file on read from unformatted key',
     +      ' file *')

      else if (n.eq.32) then
        write(lu,32) ierr
32      format(' * File system error #',i6,' detected on read of',
     +      ' unformatted'/'   specification file *')

      else if (n.eq.33) then
        write(lu,33) ierr
33      format(' * File system error #',i6,
     +      ' detected on attempt to open',
     +      ' the formatted'/'   specification file *')

      else if (n.eq.34) then
        write(lu,34) ierr
34      format(' * File system error #',i6,' detected on attempt to',
     +      ' create the unformatted card',
     +      /'   specification file *')

      else if (n.eq.35) then
        write(lu,35) ierr
35      format (' * File system error #',i6,' detected on attempt to',
     +      ' write to the unformatted',
     +      /'   card specification file *')

      else if (n.eq.36) then
        write(lu,36) ierr
36      format (' * File system error #',i6,' detected on attempt to',
     +      ' open the input file *')

      else if (n.eq.37) then
        write(lu,37) ierr
37      format (' * File system error #',i6,' detected on attempt to',
     +      ' the output file *')

      else if (n.eq.38) then
        write(lu,38) ierr
38      format (' * File system error #',i6,
     +      ' detected on read from file *')

      else if (n.eq.39) then
        write(lu,39) ierr
39      format (' * File system error #',i6,' detected on write to',
     +      ' the parsed output file. *')

      else if (n.eq.40) then
        write(lu,40) ierr
40      format (' * File system error #',i6,' detected on close of the',
     +      ' input file.')

      else if (n.eq.41) then
        write(lu,41) ierr
41      format (' * File system error #',i6,' detected on close of the',
     +      ' parsed output file. *')

      else if (n.eq.42) then
        write(lu,42) ierr
42      format (' * File system error #',i6,' detected on close of the',
     +      ' unformatted key file. *')

      else if (n.eq.43) then
        write(lu,43) ierr
43      format (' * File system error #',i6,' detected on close of the',
     +      ' formatted key file. *')

      else if (n.eq.44) then
        write(lu,44)
44      format(' * Unknown or Bad LOGICAL value given *')

      else if (n.eq.45) then
        write(lu,45)
45      format(' * Too many "include" files. Stack overflow. *')


c.. unidentified error.
      else
        write(lu,1021) num
1021    format(' * GENII error code number ',i7,' not found *')

c...END of the switch on error number!
      endif



c.. phew!!!!
c.. now if there was a input line associated with this error,
c.. then write part of it out.
      if (messln.gt.0) then
        write(lu,1019) mess(1:messln)
1019    format(' ==> ',a)
      endif

      return
      end
