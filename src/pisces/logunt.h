cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  $Date: 90/08/26 13:24:52 $  ($Author: pisces $)  $Revision: 9009.3 $
c*comdeck logunt
c-------------------------------------------------------------
c 
c     common for logical unit numbers 
c 
      common /lucom/ lutty,luout,luinf,ludia,luplt,lutmp, 
     +               lusup,lucpu,lugtmp,lutmp2,
     +               lulog,lulog2,luxtra
c 
      integer lutty,luout,luinf,ludia,luplt,lutmp
      integer lusup,lucpu,lugtmp,lutmp2,lulog,luxtra,lulog2
c 
