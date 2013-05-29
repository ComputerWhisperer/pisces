cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon Mar  6 12:51:22 PST 1989 (dredge--stanford)
c*comdeck plot
c-------------------------------------------------------------
c 
c     common for plotting routines
c 
      common /pltco/ xpmin,xpmax,xpdel,xpoff,xpwid,ypmin,ypmax,
     +       ypdel,ypoff,ypwid,lntmax,devxmn,devxmx,devymn,devymx,
     +       rndoff,idepln,juncln,ibndln,igrdln, 
     +       ielcln,initpl,ipendn,
     +       iarchg,iparea,gpsize,
     +       amin,amax,fmin,fmax,lxcomp,lycomp,
     +       lpause,xpwid0,ypwid0,n1max,
     +       linsid,lrx,lry, ncolor,palett,lmargn
      common /pltco/ uxpwid, uypwid, uxoff, uyoff
c 
      logical lpause,linsid,lmargn,lxcomp,lycomp
      logical initpl
      integer lntmax,idepln,juncln,ibndln,n1max,
     +        igrdln,ielcln,ipendn,
     +        iarchg,iparea,gpsize,
     +        ncolor,palett(20)
      real xpmin,xpmax,xpdel,xpoff,xpwid,ypmin,ypmax,ypdel,ypoff,ypwid, 
     +     devxmn,devxmx,devymn,devymx,rndoff,xpwid0,ypwid0,
     +     lrx,lry,amin,amax,fmin,fmax
c.user specified values...
      real uxpwid, uypwid, uxoff, uyoff
c
c...axis sizes (to be calculated by sizplt())
      real hnumb, htic, htitle
      common /pltco/ hnumb, htic, htitle
c
      common /pltcoc/ nplout
      character*20 nplout
c 
      common /pltcoc/ header, hdvers
      character*20    header, hdvers
