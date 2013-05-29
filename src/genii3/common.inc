c--------------------------------------------------------------------
c Thu Jul 27 14:08:56 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c    Internal (non-user accessible) Common for GENII.
c
c    Original: Michael Eldredge & Stephen Hansen (jun 83)
c    Modified: Michael Eldredge (jul 87) Modified for "subr"ed version.
c    Modified: Michael Eldredge (jul 89) 2 common areas.  Some
c	machines do not like character and numeric data in the same
c	common.  So we have one for each.
c
c--------------------------------------------------------------------
c.. logical unit definition.
         integer luttyi,luttyo,luinp,luout
         integer lukyf,lukyu,luprs
c..  did we ever open them?
         logical opinp, opprs
       common /gniicm/ luttyi,luttyo,luinp,luout
       common /gniicm/ lukyf,lukyu,luprs
       common /gniicm/ opinp, opprs
c---------------------------------------------------------------------
c.. input file stacks
c..base LU for includes, max that we have LUs for
         integer luincl, incmax
         integer nincls, inckey
         integer luins(MXINCL)
         integer nlins(MXINCL)
         character*(NAMRLN) incfls(MXINCL) 
         logical iacts(MXINCL)
       common /gniicm/  luincl, incmax
       common /gniicm/  nincls, inckey
       common /gniicm/  luins
       common /gniicm/  nlins
       common /gniCcm/  incfls
       common /gniicm/  iacts
c---------------------------------------------------------------------
c.. Parser information
c...... keyid names.
         character*(CPNAM) keys(CARD)
c.... key numb,parloc,pars
         integer   keyids(CARD),parptr(CARD),nparms(CARD)
         integer   nkeys,keyptr

c... param names.
         character*(CPNAM) parms(PARM)
c... parms location in xVAL, type
         integer   parloc(PARM),partyp(PARM)

c... default values for logical...
         logical   ldfalt(PARM)
c...  real and....
         real      rdfalt(PARM)
c...   and character parameters.
         character*(CPCVL) cdfalt(PARM)
       common /gniCcm/ keys
       common /gniicm/ keyids, parptr, nparms
       common /gniicm/ nkeys,  keyptr
       common /gniCcm/ parms
       common /gniicm/ parloc, partyp
       common /gniicm/ ldfalt, rdfalt
       common /gniCcm/ cdfalt

c---------------------------------------------------------------------
c.. error flags and input variables.
         logical   errflg,wrnflg,eofflg

         integer	 keyid
         integer   linum,erpntr,err(3,ERMAX)
         integer   eslen(ERMAX)
         character*(ERMESLN) estrng(ERMAX)
       common /gniicm/ keyid
       common /gniicm/ errflg,wrnflg,linum
       common /gniicm/ eofflg,erpntr,err,eslen
       common /gniCcm/ estrng
c---------------------------------------------------------------------
c.. prompts and interactive/batch flag
         character*(NAMRLN) prsfil
         character*(NAMRLN) inpnam
         character*(PRMTSZ) prmpt, prmpt2
         integer          prmln, prm2ln
         logical          iactiv
       common /gniCcm/ prsfil, inpnam
       common /gniCcm/ prmpt, prmpt2
       common /gniicm/ prmln, prm2ln, iactiv
c---------------------------------------------------------------------
c
c   User accessible common area for GENII.
c
c---------------------------------------------------------------------
      include 'genicm.inc'
c---------------------------------------------------------------------
