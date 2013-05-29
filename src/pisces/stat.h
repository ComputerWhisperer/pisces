cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  $Date: 90/08/26 13:28:52 $  ($Author: pisces $)  $Revision: 9009.3 $
c*comdeck stat
c-------------------------------------------------------------
c 
c     common for program status flags 
c 
      common /stcom/ lmshcd,lregcd,lelecd,lxmscd,lymscd,ldopcd,lmatcd,
     &       lmshdn,lsymdn,lmthdn,lrect,lmodcd,lbpt1,lbicep,
     +       lminit
c 
      logical lmshcd,lregcd,lelecd,lxmscd,lymscd,ldopcd,lmshdn,lsymdn,
     +        lmthdn,lrect,lmodcd,lbpt1,lbicep,lmatcd,lminit
c 
