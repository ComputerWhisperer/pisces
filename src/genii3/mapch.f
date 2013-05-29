c-----------------------------------------------------------------------
c   Copyright 1980 by
c   The Board of Trustees of the Leland Stanford Junior University
c   All rights reserved.
c
c   This routine may not be used without the prior written consent
c   of the Board of Trustees of the Leland Stanford University.
c-----------------------------------------------------------------------
c CHARACTER*1 FUNCTION MAPCH(CH)
c
c Wed Sep 13 13:51:15 PDT 1989 (dredge--stanford)
c
c   MAPCH: (character FUNCTION) If a character is lower case, MAPCH will
c      map the character to its upcase equivalent.
c
c   CALLING SEQUENCE:
c
c      <C>=MAPCH(CH)
c
c   WHERE:
c
c      mapch - (character) The case folded character.  The only characters
c         touched are lower chase characters, all others are passed on
c         as untouched.
c      ch - (character) The char to compare.  CH must be a single
c         character.
c
c   ORIGINAL: Michael Eldredge (JUL 81)
c
c   MODIFICATIONS:
c   Michael Eldredge (jul 83) UNIX conversion to FORTRAN-77.
c      Use of character variables.
c   Michael Eldredge (jul 87) Convert to library package.
c       New (5th) version.  Included are 3 versions and timings.  I
c       thought that vers#5 would be the fastest, but vers#4 is by
c       20 to 50 times faster.  I've left the others in incase vers#4
c       doesn't port.  Version#3 will always work (al beit slower).
c
c3:c ======================================================================
c3:c Version#3
c3:c Sun:    104.7 real       101.0 user         0.3 sys
c3:c Convex: 452.9 real       439.5 user         2.6 sys
c3:c ----------------------------------------------------------------------
c3:     CHARACTER*1 FUNCTION MAPCH(CH)
c3:      character ch
c3:c
c3:      integer i
c3:      character little(26),big(26)
c3:
c3:      data little/'a','b','c','d','e','f','g','h','i','j','k','l','m',
c3:             'n','o','p','q','r','s','t','u','v','w','x','y','z'/
c3:      data big   /'A','B','C','D','E','F','G','H','I','J','K','L','M',
c3:                  'N','O','P','Q','R','S','T','U','V','W','X','Y','Z'/
c3:c
c3:
c3:      mapch1=ch
c3:      do 100 i=1,26
c3:        if(ch .eq. little(i)) then
c3:           mapch1=big(i)
c3:           goto 200
c3:         endif
c3:100   continue
c3:
c3:200   continue
c3:      return
c3:      end
c
c ======================================================================
c Version#4
c Sun:     19.2 real        18.6 user         0.0 sys
c Convex:   9.9 real         9.3 user         0.1 sys
c ----------------------------------------------------------------------
        CHARACTER*1 FUNCTION MAPCH(CH)
        character ch
c
        integer ich, ia, ibiga
c
        mapch = ch
        ia = ICHAR('a')
        ibiga = ICHAR('A')
        ich= ICHAR(ch)

        if (ich.ge.ia .and. ich.le.ICHAR('z') )
     +          mapch = CHAR( ich - ia + ibiga )

        return
        end
c
c5:c ======================================================================
c5:c Version#5
c5:c Sun:    331.2 real       322.3 user         0.8 sys
c5:c Convex: 169.8 real       168.6 user         0.4 sys
c5:c ----------------------------------------------------------------------
c5:     CHARACTER*1 FUNCTION MAPCH(ch)
c5:     character ch
c5:c
c5:     integer i
c5:     character*26 little, big
c5:c
c5:        data little/'abcdefghijklmnopqrstuvwxyz'/
c5:        data big   /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c5:c
c5:     mapch = ch
c5:     i = INDEX(little, ch)
c5:     if (i.gt.0) mapch = big(i:i)
c5:c
c5:     return
c5:     end
