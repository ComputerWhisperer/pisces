cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c XDATE, XTIME - return current date and current time strings

c =======================================================================
c "XDATE": Return current date string.
c
c  Usage:
c     call xdate(str)
c  Notes:
c     + Format is somewhat arbitrary; however, a good one is:
c          "23 Sep 88"   "04 sep 88"   " 4 Sep 88"  "04-Sep-88"
c       or 9 (or less) characters long.
c     + In clock() the time and date strings are cat'ed together
c       like: str = date // '  ' // time
c       This resultant string MUST be 20 or less characters or
c       saved solution files will not work (lodsol will always
c       read and write 20 characters)
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------
       SUBROUTINE XDATE(STR)
       character*(*)  STR
c -- LOCAL variables
       character*30 dat
c -----------------------------------------------------------------------
c...get the Unix ctime() string
c.....  1234 6789,1234 6789,1234 6789,
c..... "Mon May 23 18:34:22 1988"
       call fdate(dat)
c
c...reformat to our liking.
       str = ' '
       str(1: 2) = dat(9:10)
       str(3: 3) = '-'
       str(4: 6) = dat( 5: 7)
       str(7: 7) = '-'
       str(8: 9) = dat(23:24)
	
       return
       end

c =======================================================================
c "XTIME": Return current time of day string.
c
c  Usage:
c     call xtime(str)
c  Notes:
c     + Format is somewhat arbitrary; however, a good one is:
c	   23:04:56     23:04:00    11:04:56pm
c       or 8 to 10 characters long (but no more than 10 - see note
c       for XDATE.
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------
       SUBROUTINE XTIME(STR)
       character*(*)  STR
c -- LOCAL variables
       character*30 tim
c -----------------------------------------------------------------------
c...get the Unix ctime() string
       call fdate(tim)
c
c...reformat to our liking.
       str = tim(12:19)
       return
       end
