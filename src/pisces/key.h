cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Thu Aug  9 15:37:16 PDT 1990 (dredge--stanford)
c*comdeck key
c-------------------------------------------------------------
c 
c     common for keyids and segment name
c 
      common /kycom/ ktitle,kcomme,kend,kmesh,kxmesh,kymesh, 
     +       kregio,kdopin,kelect,kprint,ksymbo,kmodel,kmeth,
     +       kmater,ksolve,kconta,kplot1,kplot2,kcntur,keline,
     +       kspred,kintf,kelim,kload,krgrid,kvectp,
     +       kxtrac,kcheck,klogj,koptn,kwindo,kmcmsh,kmcmat,kmcsol,
     +       kmcout,kcompo
c 
      integer ktitle,kcomme,kend,kmesh,kxmesh,kymesh, 
     +        kregio,kdopin,kelect,kprint,ksymbo,kmeth,kmodel, 
     +        kmater,ksolve,kconta,kplot1,kplot2,kcntur,keline, 
     +        kspred,kintf,kelim,kload,krgrid,kvectp,
     +        kxtrac,kcheck,klogj,koptn,kwindo,kmcmsh,kmcmat,kmcsol,
     +        kmcout,kcompo
c
      logical        lcrdrd
      integer        keyid
      common /kycom/ lcrdrd, keyid
c
