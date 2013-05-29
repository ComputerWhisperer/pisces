cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Mar  8 14:14:06 PST 1990 (dredge--stanford)
c*comdeck symtmp3
c
c                   temporary common to set up
c                   minimum degree ordering
c
      common/tmpco/cia,cja,head,next,last,tv,tl
      integer cia(MAXEQN),head(MAXEQN),next(MAXEQN),
     +        last(MAXEQN),tv(MAXIY),tl(MAXIY)
      INTeger cja(MAXCJA)

c
c...Note - max. L,U is 400k/2=200k (put this bound in iyale)
c
comment on storage :
c     Minimum degree call has form
c     md( n, ia, ja, max, v, l, head, last, next, mark, flag )
c            --  --       -max-  -n-   -n-   -n-   -n-
c
c     n 	is the #equations
c     ia,ja 	is the row ordered sparsity pattern
c     v,l 	are work arrays of length max; 
c     max 	= n+2k where k is the number of non-zeroes in u (estimated)
c     head, mark= working arrays of length n
c     last,next = the inverse and direct orderings
c
c     For the call we use local variables :
c     cia,cja for the (uncompressed column-ordered) sparsity pattern, 
c     tl,tv for the (huge) working arrays, 
c     head,last,next for the corresponding parameters
c     tv again for mark (allowed by md)
c
