cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 13:42:01 PST 1990 (dredge--stanford)
c*comdeck mshtmp
c
c ...  temporary grid info for rectangular grids
c
c      mshmap(i,j)	= 0 if node has been eliminated
c               	= index of node i,j in the cord array.
c
c      nrcd		= number of region cards (<=100)
c      rmap(5,ir)       = region structure for region card ir:
c                         (1,ir) = region number
c                         (2,ir) = low x index
c                         (3,ir) = high x index
c                         (4,ir) = low y index
c                         (5,ir) = high y index
c
c     start, done       = temporary stuff.
c
      common /tmpco/ mshmap,start,done,nrcd,rmap
      common /tmpco/ ni,nj,xi,yi,rx,ry,xcord,ycord
      integer mshmap(MAXREC,MAXREC),nrcd,rmap(5,MAXREC)
      integer ni(MAXREC),nj(MAXREC)
      logical start(MAXREC,MAXREC),done(MAXREC,MAXREC)
      real xi(MAXREC),yi(MAXREC),rx(MAXREC),ry(MAXREC)
      real xcord(MAXREC),ycord(MAXREC)
c
c
