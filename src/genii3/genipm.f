      SUBROUTINE GENIPM(prim, primsz, scnd, scndsz)
      include 'genidf.inc'
      character*(*) prim
      integer       primsz
      character*(*) scnd
      integer       scndsz
c Thu Sep 14 00:25:32 PDT 1989 (dredge--stanford)
c-----------------------------------------------------------------------
c   Copyright 1989 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c
c "genipm": Set the prompt string and declare interactive mode.
c
c Calling sequence:
c     call genipm(prim, primsz, scnd, scndsz)
c
c Where:
c     prim   - character*(*): Primary prompt string.
c     primsz - integer: Prompt with first PRMSZ characters.
c     scnd   - character*(*): Secondary prompt string. For continued
c              lines.
c     scndsz - integer: Prompt with first PRMSZ characters.
c
c Original: Michael Eldredge -- Stanford (feb 89)
c-----------------------------------------------------------------------
c   common
c-----------------------------------------------------------------------
      include 'common.inc'
c-----------------------------------------------------------------------
c   local variables
c-----------------------------------------------------------------------
c
      prmpt  = ' '
      prmpt2 = ' '
      iactiv = .false.

      if (primsz.gt.0 .and. prim(1:1).ne.' ') then
          iactiv=.true.
          prmpt = prim(1:primsz)
          prmln = primsz
          prmpt2= scnd(1:scndsz)
          prm2ln= scndsz
      endif

      return
      end
