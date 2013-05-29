cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1989 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Feb 24 10:20:19 PST 1989 (dredge--stanford)
c =======================================================================
c "XEXIT.F": Return an exit status to the operating system.
c
c Routines:
c
c SUBROUTINE       XEXIT(stat)
c
c Where:
c     stat  - integer:  0 for everthing ok; 1 for abnormal finish.
c
c Notes:
c     + If your system wants the exit status different, do the mappings
c       here.  For example, VMS likes odd numbers for OK and even for
c       error.  So you could map our 0 -> 1 and our 1 -> 0 for VMS.
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------


c =======================================================================
       SUBROUTINE XEXIT(stat)
       integer stat
c
       call exit(stat)
c
c.. NOTREACHED: we never actually get here
       return
       end
