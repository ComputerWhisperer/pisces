      LOGICAL FUNCTION GENII(filinp, filprs, filkey, filuky, outlu)
      include 'genidf.inc'
      character*(*) filinp, filprs, filkey, filuky
      integer       outlu
c Tue Sep 12 14:55:38 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c   WRITTEN BY: Stephen E. Hansen                        DATE: 18-Jan-80
c   MODIFICATIONS:
c   BY:  Michael Eldredge                                DATE: 04-Sep-80
c   BY:  Michael Eldredge                                DATE: 30-Jun-81
c   BY:  Stephen E. Hansen                               DATE: 13-Feb-82
c   BY:  Michael Eldredge                                DATE: 25-May-83
c   BY:  Michael Eldredge                                DATE: 08-Jul-87
c       Rearrange common.  Subr-ize the library.
c       Logical function w/ file name parameters.
c
c   NAME: genii
c   FUNCTION: Main routine of GENII (GENeral Input Interpreter) which
c             takes as input a sequence of lines, parses them and stores
c             the values obtained in a fixed format file.
c
c   TYPE OF ROUTINE:  LOGICAL FUNCTION
c   CALLING SEQUENCE:
c    <logical> = genii(filinp, filprs, filkey, filuky, outlu)
c
c   PARAMETERS (INPUT):
c       character*NAMRLN filinp - Input file source name.
c       character*NAMRLN filprs - Parsed Input file destination name.
c       character*NAMRLN filkey - Textual key file name.
c       character*NAMRLN filuky - Parsed (unformatted) key file name.
c       integer                 - open file for messages.
c
c   PARAMETERS (OUTPUT):
c       <return>        - True. No errors.
c                       - False. Error(s) found in parsing.
c
c   NOTES:
c       + Some loaders will not search-and-find a common block. We
c         need the common block for both the Genii parsing code AND
c         the gtval() routine (which may be in differnt programs).
c         So, the BLOCK DATA for all the genii code is included with
c         the GTVAL.R file.  Thus, for this code to get it, we
c         reference GTVAL(), but never call it!  Get it?
c
c-----------------------------------------------------------------------
c   common
c-----------------------------------------------------------------------
      include 'common.inc'
c-----------------------------------------------------------------------
c   local variables
c-----------------------------------------------------------------------
       logical lerr
c-----------------------------------------------------------------------
c   Functions:
c-----------------------------------------------------------------------
       logical gnget1

c-----------------------------------------------------------------------
c   start of genii
c-----------------------------------------------------------------------

c No errors, yet....
      genii = .true.

      call initl(filinp, filprs, filkey, filuky, outlu)
      if (errflg) then
         genii=.false.
         goto 500
      endif

c if interactive, just return and gtval() will do the rest.
      if (iactiv) goto 600

c not interactive (normal, batch mode)
100   if (.not.gnget1(lerr)) goto 500
      call outpt
      goto 100

c.. finish up (close files, output error messages, etc.)
500   continue
c.. get the state of errflg before genien() is called!!
      genii=.not.errflg
      call genien

600   return
c.. of genii.
      end
