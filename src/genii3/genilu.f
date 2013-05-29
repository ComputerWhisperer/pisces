c
c Wed Sep 13 10:59:03 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1987 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c "genilu":  (Optional) Set in non-default LUs for genii.
c
c Function: to give the Genii software a differnt set of numbers to
c       use for LU numbers.
c
c original: Michael Eldredge (jul 87)
c

      SUBROUTINE GNILUS(lu1st, lucnt, inclky)
      include 'genidf.inc'
       integer lu1st, lucnt
       integer inclky
c
c
      include 'common.inc'

c...check for some problems.
      if (lucnt.lt.4) then
          write(luttyo,91) lucnt
91        format(/' FATAL GENII ERROR. Too few LUs given.',i1)
          stop 4
      endif
      if (lucnt.gt.4+MXINCL) then
          write(luttyo,93) lucnt-4,MXINCL
93        format(/' FATAL GENII ERROR. Too many "include" LUs given ',
     +            i5,', max ',i2)
          stop 5
       endif

c...assign the lu numbers to use
      luinp = lu1st+0
      luprs = lu1st+1
      lukyf = lu1st+2
      lukyu = lu1st+3

c...if they want INCLUDE statements...
      if (lucnt.gt.4) then
          luincl=lu1st+4
          incmax=lucnt-4
          inckey=inclky
          if (inckey.le.0) inckey = -1
      endif

      return
      end

c -------- OLD, USE NO MORE! ----------
      SUBROUTINE GENILU(iinp, iprs, ikyf, ikyu)
      include 'genidf.inc'
       integer iinp, iprs, ikyf, ikyu
c
c
      include 'common.inc'

       luinp = iinp
       luprs = iprs
       lukyf = ikyf
       lukyu = ikyu

       return
       end
