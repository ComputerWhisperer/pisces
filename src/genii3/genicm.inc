c--------------------------------------------------------------------
c Thu Jul 27 14:12:43 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c    User Accessible Common for GENII.
c
c    original: Michael Eldredge & Stephen Hansen (jun 83)
c    modified: Michael Eldredge (jul 87) Modified for "subr"ed version.
c    Modified: Michael Eldredge (jul 89) 2 common areas.  Some
c      machines do not like character and numeric data in the same
c      common.  So we have one for each.
c
c--------------------------------------------------------------------
c.. Parsed input line variables.

         logical   lspecd(LPC),rspecd(RPC),cspecd(CVPC),tspecd
         logical   lval(LPC)
         real      rval(RPC)
         character*(CPCVL) cval(CVPC)
         character*(LINELN) tval

       common /genicm/ lval,lspecd,rval,rspecd
       common /genicm/ cspecd,tspecd
       common /genCcm/ cval,tval
c---------------------------------------------------------------------
