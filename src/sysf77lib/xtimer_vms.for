c =======================================================================
c "XTIMER" - Return current cpu time (in seconds) usage.
c
c  Usage:
c     subroutine xtimer(tim)
c       real tim   - seconds of cpu used.
c
c  Original: K. Kosai, 31 May 1989
c
c -----------------------------------------------------------------------
      SUBROUTINE XTIMER(tim)
      REAL  tim
c -- LOCAL variables
      INCLUDE  '($JPIDEF)'
      INTEGER  itime, ibuffer(4)
c
c       set up request for cpu time into integer*4
      ibuffer(1) = JPI$_CPUTIM*'10000'X + 4
c       define temporary location
      ibuffer(2) = %loc(itime)
c       get cpu time and convert to seconds
      CALL SYS$GETJPI (,,,ibuffer,,,)
      tim = REAL(itime)/100.
c
      RETURN
      END
cc -----------------------------------------------------------------------
cc
c      PROGRAM TIME
cc
cc      Compile this with no optimization!
cc
c      REAL secnds, begtim, elaptm
c      CHARACTER*20 cdatim
c      INTEGER i, j
c      REAL*8 dummy
cc
c      CALL XDATE(cdatim(1:9))
c      cdatim(10:10) = ' '
c      CALL XTIME(cdatim(11:20))
c      WRITE (6, '(X, ''Date and Time = '', A)') cdatim
c      CALL xtimer(begtim)
c      WRITE (6, '(X, ''CPU Time = '', F8.3)') begtim
c      DO j = 1, 5
c        DO i = 1, 10000
c          dummy = DASIN(DSIN(DBLE(i+j)))
c        END DO
c        CALL XTIME(cdatim(11:20))
c        WRITE (6, '(X, ''Date and Time = '', A)') cdatim
c        CALL xtimer(secnds)
c        WRITE (6, '(X, ''CPU Time = '', F8.3, '' Elapsed CPU Time ='',
c     1    F8.3)') secnds, secnds-begtim
c      END DO
cc
c      STOP
c      END
