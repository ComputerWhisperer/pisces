cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  $Date: 90/03/10 17:51:53 $  ($Author: pisces $)  $Revision: 8940.2 $
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      LOGICAL FUNCTION GTKEY(key)
      include 'p2conf.h'
c
c     Like GTVAL() but keeps linum (really command-number)
c         up-to-date.
c
c     Original : Michael Eldredge -- Stanford (Oct 87)
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      integer key
c----------------------------------------------------------------------
c.........common area
      include     'blank.h'
c
c----------------------------------------------------------------------
c
c FUNCTIONS:
      logical gtval

c******************** Start ********************************************
c
      gtkey = gtval(key)
      linum = linum + 1
      return
      end
