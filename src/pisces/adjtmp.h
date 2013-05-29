cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 15:06:31 PST 1990 (dredge--stanford)
c*comdeck adjtmp
c 
c                   temporary adjacency information
c
c       p2t(1..MAXNBR,ip) = all the triangle neighours of point ip
c       p2tc(ip) = number of  "       "       at ip
c
c       Maxnbr is set in subroutine NXTEL 
c       (see also ip2t,ip2tc in soltmpn.h)
c
      common /tmpco/ p2t, p2tc
c 
      integer p2t(MAXNB,MAXPT), p2tc(MAXPT)
c 
