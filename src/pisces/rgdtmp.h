cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 13:52:56 PST 1990 (dredge--stanford)
c*comdeck rgdtmp
c
c		common for grid refinement
c		level  is the current level of each triangle
c               fath(ie)  is the father of triangle ie (green negative)
c               son(ie)   is the first son of ie (green negative)
c               imat2(ie) is the material numbers of the full triangle tree,
c                         and needs twice as much storage as imat.
c
c
	common /tmpco/tnop,level,fath,son,imat2

	integer tnop(3,MAXTRI2),level(MAXTRI2),fath(MAXTRI2),
     +          son(MAXTRI2),imat2(MAXTRI2)
