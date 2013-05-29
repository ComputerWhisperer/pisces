cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:02:06 PST 1990 (dredge--stanford)
c*comdeck smhtmp
c
c                   temporary common area for smoothing algorithmsS
c                   todo and done are used for nodes AND triangles so
c                   the dimension is the larger.
       common/tmpco/todo,done,lbndry
c
       logical todo(MAXTRI),done(MAXTRI),lbndry(MAXPT)
