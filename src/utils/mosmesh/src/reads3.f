c/*----------------------------------------------------------------------
c**  Copyright 1989 by
c**  The Board of Trustees of the Leland Stanford Junior University
c**  All rights reserved.
c**
c**  This routine may not be used without the prior written consent of
c**  the Board of Trustees of the Leland Stanford University.
c**----------------------------------------------------------------------
c**/

c/*	reads3.f		Version 1.2		*/
c/*	Last Modification:	1/22/90 13:20:19		*/



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE READS3(supfil,imptyp, dpeak, djunc, dstd,
     +   dsub, type, depth)
c
c	supfil - suprem3 filename
c
c	imptyp - impurity type to scan for (-> mapped to integer in
c			export file )
c	             5 - Boron  -> 1   
c		     6 - Phosphorus -> 2
c		     7 - Arsenic -> 3
c		     8 - Antimony -> 4
c
c
c	dpeak - peak doping concentration
c
c	djunc - junction depth
c
c	dstd - standard deviation
c
c	dsub - substrate doping
c
c       type - substrate doping type (0 for N, 1 for P)
c
c       depth - thickness of substrate
c
      integer imptyp
      integer nlayrs, nimps, nnodes
      integer matt(10), top(10), impt(10)
      integer idxsub, idximp
      integer flushi
      integer sibgn
      integer type
      integer i
      integer j
      real oxth
      real peak
      real junc
      real sub
      real thick(10)
      real subpos(10)
      real flushr
      real pkloc
      real rdepth
      real y(500), consup(500)
      character*20 supfil
      character*20 flushc
      double precision dpeak
      double precision djunc
      double precision dstd
      double precision dsub
      double precision dpkloc
      double precision dsibgn
      double precision depth

c-----------------------------------------------------------------------

c...Array bounds for SUPREM-III (if changed, remember to change 
c...array bounds in common/com.doptmp for consup and ysup)
c     data maxmat/10/,maximp/4/,maxlay/10/,maxgrd/500/
c
c*************
c**  START  **
c*************
c
c     nnodes = maxgrd
      peak = 0.0
      pkloc = 0.0
c------------------
c  NEW SUPREM-III
c------------------
c
c...Export format
c
c...Open/rewind file
      open( unit=1, status='old', file=supfil, form='unformatted' )
c
c...Read no. layers, no. impurities and no. points (record 1)
      read(1,err=999,end=999) nlayrs,nimps,nnodes
c
c...Read structure and find substrate layer index  (record 2)
c...(layer code - 1=Si,2=SiO2,3=poly,4=Si3N4,5=Al)
      read(1,err=999,end=999) (matt(i),thick(i),top(i),i=1,nlayrs)

c...find index of node which represents the silicon surface
      do 501 idxsub=1,nlayrs
      if(matt(idxsub).eq.1) goto 502
 501  continue
c...silicon not found, return
      goto 530
 502  sibgn = top(idxsub)
      rdepth = thick(idxsub)

c...Read layer names (record 3)
      read(1,err=999,end=999) (flushc,i=1,nlayrs)

c...Read crystalline orientation and grain size (record 4)
      read(1,err=999,end=999) (flushi,flushr,i=1,nlayrs)

c...determine oxide thickness
      do 503 idxsub=1,nlayrs
      if(matt(idxsub).eq.2) goto 504
503   continue
504   oxth=thick(idxsub)

c...Read impurity code array and find impurity index (record 5)
      read(1,err=999,end=999) (impt(i),i=1,nimps)
      do 505 idximp=1,nimps
      if(impt(idximp).eq.(imptyp-4)) goto 506
505   continue
c...  impurity not found, return
      goto 530
c
c...More Hansen fecies
c...Read Impurity Name (record 6)
506   read(1,err=999,end=999) (flushc,i=1,nimps)

c...read integrated dopant and the interior concentration of the grains
c...(record 7)
      read(1,err=999,end=999) ((flushr,flushr,j=1,nimps),i=1,nlayrs)
c
c...Read in dx array (record 8)
      read(1,err=999,end=999) (flushr,i=1,nnodes)

c...Read in distance array (record 9)
      read(1,err=999,end=999) (y(i),i=1,nnodes)

c...Read in chemical and active concentrations (record 10)
      do 510 j=1,nimps
	  if (j .ne. idximp) then
              read(1,err=999,end=999) (flushr,i=1,nnodes)
              read(1,err=999,end=999) (flushr,i=1,nnodes)
              subpos(j) = abs(flushr)
	  else
              read(1,err=999,end=999) (flushr,i=1,nnodes)
              read(1,err=999,end=999) (consup(i),i=1,nnodes)
              subpos(j) = abs(consup(nnodes))
          endif
510   continue

c...determine substrate doping from subpos matrix
      sub = 0
      do 513 i=1,nimps
	  if (subpos(i) .gt. sub) then
	      sub = subpos(i)
	      type = impt(i)
          endif
513   continue

c...now convert type to either N or P
c...  only type 1 (B) is P. the rest are N
      if (type .ne. 1) then
	  type = 0
      endif
	  

c...with information in y(i) and consup(i), determine info

      do 515 i=sibgn,nnodes
      if (abs(consup(i)) .gt. peak) then
          peak = abs(consup(i))
          pkloc = y(i)
      endif
      if (abs(consup(i)) .lt. sub) then
         junc = y(i) - y(sibgn)
         goto 520
      endif
515   continue

c...since the calling C program expects double precision convert here
520   continue

      dpkloc = pkloc
      dsibgn = y(sibgn)
      dpeak = peak
      djunc = junc
      dsub = sub
      depth = rdepth
      
      dstd = (djunc - (dpkloc - dsibgn)) / dsqrt(dlog(dpeak/dsub))



c
c...Close file and proceed
      close( unit = 1 )
c
530   continue
999   continue
c
      end
