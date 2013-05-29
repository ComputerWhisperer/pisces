cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c XTIMER - return cpu time usage.

c =======================================================================
c "XTIMER" - Return current (user) cpu time (in seconds) usage.
c
c  Usage:
c     subroutine xdate(tim)
c       real tim		- user seconds of cpu used.
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------
       SUBROUTINE XTIMER(tim)
       real  tim
c -- LOCAL variables
       real  uands(2)
       real  retn
c -- FUNCTIONS
       real  etime
c -----------------------------------------------------------------------
c...get the Unix times() routine
c.........etime() is elapsed since beginning
c.........dtime() is elapsed since last call to dtime()
       retn = etime(uands)

c.........UandS times 1-user time; 2-system time.
       tim = uands(1)
       return
       end
