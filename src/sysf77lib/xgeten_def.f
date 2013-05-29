cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c =======================================================================
c "XGETEN": Get environment variables from a file.
c
c  This is intended to be used if there is no way to get environmental
c  defines (Unix 'setenv', or VMS 'define/logical', etc..)
c
c  Usage:
c     call xgetenv(key, val)
c     if (val(1:1).ne.' ') then
c         -- we match a value in the environment --
c     else
c         -- no match --
c     endif
c
c  Environment file format:
c    <0+ spaces><word><1+spaces><up to end of line>
c    FOO   foo_value
c    BAR bar_value
c    BLIP_IS_LONGER    "blip's value......"
c       OTHERS   '",!@#$
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c  Notes:
c     + The "environment" is defined below to be the file 'env'.
c     + Change the TMPLU define below to a number not used by the
c       program.
c     + Should make spaces AND TABS delimit words.
c
c -----------------------------------------------------------------------
       SUBROUTINE XGETEN(KEY, VAL)
       character*(*)  key, val
c -----------------------------------------------------------------------
       integer MAXLIN
       parameter (MAXLIN=256)
c
c...choose an lu number that will not be used by the program
       integer TMPLU
       parameter (TMPLU=34)
c
c -- LOCAL variables
       character*(MAXLIN) line

       integer i, k1, kn, v1
       integer ke
       character*40 env
       integer      lu

c -- DATA
       data env /'env.txt'/
       data lu  /TMPLU/
   
c -----------------------------------------------------------------------
c    START
c -----------------------------------------------------------------------

c....not found or error is invalid return.
       val = ' '

c....delimit the key
       ke = LEN(key)      
2      if (ke.eq.0) goto 99
       if (key(ke:ke).ne.' ') goto 5
       ke = ke - 1
       goto 2
c...Open The Environment.
5      open(lu, file=env, status='old', form='formatted', err=99)

10     read(lu, '(a)', err=90, end=90) line
       i = 1
c...find start of key
20     if (i.gt.MAXLIN) goto 10
       if (line(i:i).ne.' ') goto 30
       i = i + 1
       goto 20
c...find end of key
30     k1 = i
32     if (i.gt.MAXLIN) goto 10
       if (line(i:i).eq.' ') goto 40
       i = i + 1
       goto 32
c...find start of value.
40     kn = i-1
42     if (i.gt.MAXLIN) goto 10
       if (line(i:i).ne.' ') goto 50
       i = i + 1
       goto 42
c...See if we match
50     v1 = i

       if (line(k1:kn).eq.key(1:ke)) then
	   val = line(v1:MAXLIN)
	   goto 90
       endif
c...try again....
       goto 10
c...done..................
90     close(lu)
99     return
       end
CDc
CDc.. test main
CD       program foople
CD       character*40 xkey, xval
CD
CD10     write(6,11)
CD11     format(' key: ', $ )
CD       read(5,'(a)') xkey
CDc
CD       call xgeten(xkey, xval)
CD
CD       if (xval(1:1).ne.' ') then
CD         write(6, 21) xval
CD21       format('  FOUND: ', a)
CD        else
CD         write(6,23)
CD23       format('  not found.')
CD       endif
CD
CD       goto 10
CD       end
