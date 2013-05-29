cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 13:55:39 PST 1990 (dredge--stanford)
c*comdeck rgrid
c
c........Common for regrid modes, sizes, steps.
      common /rgrid/ lregio,fdel,logar,labs,lchang,fmode,rgbox,
     +               llimit,ioff,lxg

      integer lregio(MAXCNT),fmode,llimit,ioff
      real fdel,rgbox(4),lxg
      logical labs,lchang,logar
c
      common /rgridc/ ttname
      character*20 ttname
c
