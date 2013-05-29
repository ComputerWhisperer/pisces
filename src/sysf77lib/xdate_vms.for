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
c  Original: K. Kosai, 31 May 1989
c
c -----------------------------------------------------------------------
      SUBROUTINE XDATE(str)
      CHARACTER*(*)  str
c
c   ..Returns 9-byte string in form dd-mmm-yy where
c       dd  = 2-digit date
c       mmm = 3-letter month specification
c       yy  = last two digits of the year
c
      CALL DATE(str)
c
      RETURN
      END

c =======================================================================
c "XTIME": Return current time of day string.
c
c  Usage:
c     call xtime(str)
c  Notes:
c     + Format is somewhat arbitrary; however, a good one is:
c        23:04:56     23:04:00    11:04:56pm
c       or 8 to 10 characters long (but no more than 10 - see note
c       for XDATE.
c
c  Original: K. Kosai, 31 May 1989
c
c -----------------------------------------------------------------------
      SUBROUTINE XTIME(str)
      CHARACTER*(*)  str
c
c   ..Returns time as 8-byte ASCII string of form hh:mm:ss (24-hour
c     clock).
c
      CALL TIME(str)
c
      RETURN
      END
c -----------------------------------------------------------------------
c
c      PROGRAM VMS_DATE
c      CHARACTER*20 cdate, ctime
cc
c      CALL XDATE(cdate)
c      CALL XTIME(ctime)
c      cdate(10:10) = ' '
c      cdate(11:20) = ctime(1:10)
c      WRITE (6, '(X, ''Date and Time = '', A)') cdate
cc
c      STOP
c      END
