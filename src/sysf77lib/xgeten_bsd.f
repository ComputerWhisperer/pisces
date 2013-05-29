cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c =======================================================================
c "XGETEN": Get environment variables from the UNIX environment.
c
c  If you can't getenv() then just use xgeten_def.f
c
c  Usage:
c     call xgetenv(key, val)
c     if (val(1:1).ne.' ') then
c         -- we match a value in the environment --
c     else
c         -- no match --
c     endif
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------
       SUBROUTINE XGETEN(KEY, VAL)
       character*(*)  key, val
c -----------------------------------------------------------------------
       call getenv(key, val)
       return
       end
