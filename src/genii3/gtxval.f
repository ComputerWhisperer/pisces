c Wed Sep 13 14:53:58 PDT 1989 (dredge--stanford)
c---------------------------------------------------------------------
c
c   Copyright 1987 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c
c---------------------------------------------------------------------
c
c "gtxval.r" : return values from common (so user doesn't need to
c       include common, etc.. (stand alone library).
c
c Original: Michael Eldredge (Stanford) 13 Jul 87
c Modified:
c
c Notes: These are a set of routines to access common for the user.
c       The normal way of getting values was:
c               if (rspecd(3)) x = rval(3)
c       but this required the user to have access to common, etc...
c       Now it can be done equivalently as:
c               if (isrval(3)) x = gtrval(3)
c
c -------------------------------------------------------------------


c -------------------------------------------------------------------
c Check "was specified" values from common.
c -------------------------------------------------------------------

c "isrval": test the rspecd value.
      LOGICAL FUNCTION ISRVAL(n)
      include 'genidf.inc'
      integer  n
c --------------
      include 'common.inc'
c --------------

      isrval = .false.
      if (n.le.0 .or. n.gt.RPC) return

      isrval = rspecd(n)
      return
      end


c "islval": test the lspecd value.
      LOGICAL FUNCTION ISLVAL(n)
      include 'genidf.inc'
      integer  n
c --------------
      include 'common.inc'
c --------------

      islval = .false.
      if (n.le.0 .or. n.gt.LPC) return

      islval = lspecd(n)
      return
      end


c "iscval": test the cspecd value.
      logical function iscval(n)
      include 'genidf.inc'
      integer  n
c --------------
      include 'common.inc'
c --------------

      iscval = .false.
      if (n.le.0 .or. n.gt.CVPC) return

      iscval = cspecd(n)
      return
      end


c "istval": test the tspecd value.
      logical function istval(dummy)
      include 'genidf.inc'
      integer  dummy
c --------------
      include 'common.inc'
c --------------

      istval = tspecd
      return
      end

c -------------------------------------------------------------------
c GET values from common.
c -------------------------------------------------------------------

c "gtrval": get the real value.
      real function gtrval(n)
      include 'genidf.inc'
      integer  n
c --------------
      include 'common.inc'
c --------------

      gtrval = 0.0
      if (n.le.0 .or. n.gt.RPC) return

      gtrval = rval(n)
      return
      end


c "gtlval": get the logical value.
      logical function gtlval(n)
      include 'genidf.inc'
      integer  n
c --------------
      include 'common.inc'
c --------------

      gtlval = .false.
      if (n.le.0 .or. n.gt.LPC) return

      gtlval = lval(n)
      return
      end


c "gtcval": get the character value.
      subroutine gtcval(n, buf, maxlen)
      include 'genidf.inc'
      integer  n
      character*(*) buf
      integer  maxlen

c --------------
      include 'common.inc'

      integer i
c --------------

c.. clear buffer
      buf(1:maxlen) = ' '
      if (n.le.0 .or. n.gt.CVPC) return

      i = min(maxlen, CPCVL)
c.. copy in
      buf(1:i) = cval(n)
      return
      end


c "gttval": get the title value.
      subroutine gttval(buf, maxlen)
      include 'genidf.inc'
      character*(*) buf
      integer  maxlen
c --------------
      include 'common.inc'

      integer i
c --------------

c.. clear buffer
      buf(1:maxlen) = ' '
      i = min(maxlen, LINELN)
      buf(1:i) = tval
      return
      end

