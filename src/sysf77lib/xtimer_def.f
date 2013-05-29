cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c XTIMER - return cpu time usage.  FAKE ROUTINE.  THIS RETURN A
c     MEANINGLESS NUMBER (BIGGER EACH TIME IT IS CALLED).

c =======================================================================
c "XTIMER" - Return current cpu time (in seconds) usage.
c
c  Usage:
c     subroutine xdate(tim)
c       real tim		- seconds of cpu used.
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------
       SUBROUTINE XTIMER(tim)
       real  tim
c -- LOCAL variables
       real  fake
       data  fake/0.0/

       fake = fake + 1.0
       tim  = fake
       return
       end
