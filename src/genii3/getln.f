      SUBROUTINE GETLN(PRM,PRMSZ,LINE,LPF,LPL,PEEK,
     +     ISCONT,EXCONT,NEWKEY,DIDGET)
      include 'genidf.inc'
      character*(*) prm
      integer       prmsz
      character*(LINELN) line
      integer lpf,lpl
      logical peek,iscont,excont
      integer newkey
      logical didget
c Mon Nov 20 22:58:05 PST 1989 (dredge--stanford)
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
c   DATE: Nov. 09, 1977
c
c   MODIFICATIONS:
c
c   Stephen E. Hansen  April 12, 1979
c     Modify for GENII
c   Stephen E. Hansen  July 14, 1979
c     Eliminate look ahead
c   Michael Eldredge  Sept. 02, 1980
c     Convert to RATFOR
c   Michael Eldredge  April 01, 1981
c     Return NEWKEY to calling routine
c   Stephen E. Hansen  Jan. 31, 1982
c     Restructure
c   Michael Eldredge (jun 80)  UNIX conversion to Fortran-77.
c     Use of character variables.
c   Stephen E. Hansen  Dec. 12, 1983
c     Use SCOMP to distinguish exact matches from subset matches.
c   Michael Eldredge (jul 87)  Convert to library package.
c       Convert some skip/find loops to use funcs.
c   Michael Eldredge (sep 89)  Convert from Ratfor to ANSI F77
c   MJE (nov 89) Fixed peek-from-and-include-file bug.
c
c   NAME: GETLN
c
c   FUNCTION: Gets the next line from the input file and determines
c             the card id number.
c
c   TYPE OF ROUTINE: SUBROUTINE
c
c   CALLING SEQUENCE:
c
c      CALL GETLN(LINE,LPF,LPL,ISCONT,EXCONT,KEYIDS,NKEYS,
c                  KEYS,KEYPTR,NEWKEY,DIDGET)
c
c   PARAMETERS:
c
c     (INPUT)
c
c       KEYIDS INTEGER(CARD)             Array with card id numbers.
c       NKEYS  INTEGER                   Number of cards in KEYS.
c       KEYS   character*CPNAM(CARD)     Array containing card names.
c
c     (OUTPUT)
c
c       LINE   character*LINELN   Input data line.
c       LPF    INTEGER       Pointer to the first non-blank character
c                            in LINE.
c       LPL    INTEGER       Pointer to the last non-blank character
c       ISCONT LOGICAL       True if line returned is a continuation
c                            of the previous line
c       EXCONT LOGICAL       True if line ended with continue mark,
c                            ie: EXpect CONTinuation.
c       KEYPTR INTEGER       index of the cards id number in the
c                            KEYIDS arrays.
c       NEWKEY INTEGER       id number of the card returned in LINE
c       DIDGET LOGICAL       was something gotten? (don't push if F)
c
c   ERROR CONDITIONS:
c
c    001 : Invalid card name.
c    002 : Ambiguous card name.
c    040 : Invalid continuation card.
c
c   NOTES: None.
c
c   ROUTINES USED:
c
c       GERST, REDLN, SCOMP
c
c   MACRO FILES:  GENIDF, COMMON
c--------------------------------------------------------------------
c     common area
c----------------------------------------------------------------------
      include 'common.inc'
c---------------------------------------------------------------------
c   local variables.
c---------------------------------------------------------------------
      logical badlst,eol,exact,match
      logical wascnt,waspsh
      integer nptr,leng,nmat,i
      character*(NAMRLN) fnam
      logical incflg
      character c1hspc, c1hcma, c1hpls
c end of line continuation character
      character c1hcnt

      data c1hspc/' '/, c1hcma/','/, c1hpls/'+'/, c1hcnt/'%'/
c--------------------------------------------------------------------
c   start of getln.
c---------------------------------------------------------------------
c.. set the end-of-line flag false.
c.. Go till we get a good line.
1000      eol=.false.
          wascnt=iscont
          waspsh=(lpf.ge.1)
          excont=.false.
c... usually we will have grabbed a line; only really important when
c.... we are peeking.
	  didget=.true.

c
c.. we don't know the keyid of the next line yet.
          newkey=0

c
c.. if badlst is true then it indicates that the last line read had an
c.. error in it.  Since we have just entered getln, the way things
c.. work insure that the last line was a good one.
          badlst=.false.

c
c.. not a continuation line yet.
          iscont=.false.

c--------------------------------------------------------------------
c
c   get a non-blank line from the input deck.
c
c--------------------------------------------------------------------
c.. until we get a non blank line.
1100          if (.not.waspsh) then
c.. get a line
		  if (iactiv) then
		      call rdline(prm,prmsz,line,LEN(line),i)
		      if (i.lt.0) eofflg = .true.
		  else
                      call redln(luinp,line)
		  endif
                  if (eofflg) then
c..check to see if there are open files on the stack
                      call gnincl(0, fnam, incflg)
c..no more in the stack
                      if (incflg) then
                          eofflg=.false.
			  if (.not.peek)  goto 1100
c.. peek fails if EOF from an include
			  didget=.false.
			  return
                          endif
                  endif
              endif

c.. if we're done or there was an error, let's just go.
              if (eofflg) return

c.. find out how long the line is.
              lpl=LINELN
1120          if (lpl.ge.1 .and.
     +                   INDEX(SPACE,line(lpl:lpl)).gt.0) then
                  lpl=lpl-1
                  goto 1120
              endif

c a continue mark at the end?
              if (line(lpl:lpl).eq.c1hcnt) then
                  excont=.true.
                  lpl = lpl-1
1130              if (lpl.ge.1 .and.
     +                    INDEX(SPACE,line(lpl:lpl)).gt.0) then
                      lpl=lpl-1
                      goto 1130
                  endif
              endif

c.. if it was a blank line write out a blank line
              if (lpl .eq. 0) then
                  if (.not.iactiv) then
                      write (luout,11)
11                    format(2X)
                  else if (wascnt) then
                      return
                  endif
              endif
C..go back an continue search for non blank line
              if (lpl.le.0) goto 1100

c--------------------------------------------------------------------
c
c   if the line is a continuation of a previous one.
c
c--------------------------------------------------------------------
c.. find the first non-blank character in the line.
          lpf = 1
          call skipc(lpf, lpl, line, eol, SPACE)

c if not a continue statement, return. else continue with it...
          if (peek .and. (line(lpf:lpf).ne.c1hpls)) then
              iscont = .false.
              return
          endif

c.. if this is a continuation of the previous line.
          if (line(lpf:lpf) .eq. c1hpls .or. wascnt) then
c.. write out the line without a line number.
              if (.not.iactiv) write (luout,21) line(1:lpl)
21            format(4X,'... ', a)

c.. if the first line is a continuation line then something is
c.. wrong.
c.. a continue first line???
              if (linum.eq.0) then
c.. since it is the first line.
                  linum=linum+1
                  call gerst(040,linum,0,line(lpf:lpf),lpl-lpf+1)
                  badlst=.true.

c.. looks good so far.
              else
c.. continuation line.
                  iscont=.true.
c.. same keyid number as before.
                  newkey=keyid
c.. pass up the continuation character
                  if (.not.wascnt) lpf=lpf+1
c.. find the next non-blank char.
                  call skipc(lpf, lpl, line, eol, SPACE)
                  badlst=.false.
              endif

c--------------------------------------------------------------------
c
c   found a new line.
c
c--------------------------------------------------------------------
c.. no continue character so must be a new card.
          else
c.. increment the card count
              linum=linum+1

c.. get the number of characters in the card name.
              nptr=lpf
              call findc(lpf, lpl, line, eol, EWORD)
c             eol=(lpf.gt.lpl)

c.. get that length.
              leng=lpf-nptr
              if (leng.gt.CPNAM) leng=CPNAM


c.. get the card type.

c.. true if exact match.
              exact=.false.
c.. true if subset match.
              match=.false.
c.. number of card matches.
              nmat=0
c.. to keep stupid compilers happy.
              keyptr=1

c.. search for a match in the keys array.
              do 2400 i=1,nkeys
                  if (exact) goto 2401
c.. look for a match.
                  call scomp(keys(i),CPNAM,line(nptr:nptr),leng,
     +                  match,exact)
c
c.. if a match was found.
                  if (match) then
c.. if it is an exact match, the first match, or not an
c.. alias of a previous non-exact match.
                      if (exact .or. nmat .lt. 1 .or.
     +                           keyids(i).ne.keyids(keyptr)) then
c.. increment the match count; and remember where you found it.
                          nmat=nmat+1
                          keyptr=i
                      endif
                  endif
2400         continue
2401         continue

c.. if no match was found.
              if (nmat .eq. 0) then
c.. invalid card name.
                  call gerst(001,linum,0,line(nptr:nptr),leng)
                  badlst=.true.

c.. or if no exact match was found and more than one non-exact
c.. match was found.
              else if (.not.exact .and. nmat .gt. 1) then
c.. ambiguous card name.
                  call gerst(002,linum,0,line(nptr:nptr),leng)
                  badlst=.true.

c.. got a live one.
              else
c.. get that magical keyid
                  newkey=keyids(keyptr)
c.. find the next character not a blank or a comma.
                  call skipc(lpf, lpl, line, eol, EPARM)
                  badlst=.false.
              endif

c.. write out the line and line number.
              if (newkey .ne. inckey) then
                  if (.not.iactiv) write(luout,31)linum,line(1:lpl)
31                format(1x,i3,'... ', a)
              endif
          endif


c INCLUDE FILES!
          if (newkey .eq. inckey) then
              fnam = line(lpf:lpl)
              call gnincl(1, fnam, incflg)
              if (incflg) then
		 call gerst(040,linum,0,fnam,lpl-lpf+1)
		 fnam=' '
		 if (iactiv) goto 5000
	      endif

c time to leave the loop....
          else if (.not.badlst .or. iactiv) then
              goto 5000
          endif

c in case we are going around again....
          iscont = .false.
          lpf    = 0
c
c..Loop back up in the big REPEAT
          goto 1000


5000  return
c.. of getln.
      end
