c Fri Feb  2 16:10:11 PST 1990 (dredge--stanford)
C-----------------------------------------------------------------------
C   Copyright 1990 by
C   The Board of Trustees of the Leland Stanford Junior University
C   All rights reserved.
C
C   This routine may not be used without the prior written consent
C   of the Board of Trustees of the Leland Stanford University.
C-----------------------------------------------------------------------
c
      SUBROUTINE RDLINE(prompt,plen,buf,blen,ncin)
      character*(*) prompt
      integer       plen
      character*(*) buf
      integer       blen
      integer       ncin
c ------------------------------------------
      integer       i
c
c ------------------------------------------
      write(*,11) prompt(1:plen)
11    format(a, $ )

      buf=' '
      read(*,12,err=90,end=90) buf
12    format(a)

      ncin = 0
      do 20 i=blen,1,-1
	if (buf(i:i).ne.' ') then
	   ncin=i
	   goto 22
	endif
20    continue
22    continue
      return


90    ncin=-1
      return
      end

