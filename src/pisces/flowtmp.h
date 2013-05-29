cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon Mar 11 14:51:59 PST 1991 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c*comdeck flowtmp
c
c NOTE: some constants may have to be calculated by hand!  There
c   are just too many used in the equivalence statements to come
c   up with dozens of silly names if your compilers can handle it.
c   All declarations are precomputed constants; it is just within
c   the equiv statements where work may have to be done.

c
c Common for computing current potentials/flow lines
c
      common /tmpco/ a,luspec,cia,cja,cil,cjl,ciu,cju
      common /tmpco/ civa,civl,civu,iwpc,iwpri,wnxtel
      double precision a(MAXADJ),x(MAXEQN),di(MAXEQN)
      double precision luspec(MAXLU2+MAXEQ2)
      double precision l(MAXLU),u(MAXLU)
      integer cia(MAXEQN+1),cil(MAXEQN+1),ciu(MAXEQN+1),wnxtel(3,MAXTRI)
      integer iwelpt(MAXPT),civa(MAXEQN),civl(MAXEQN+1),civu(MAXEQN+1)
      integer cja(MAXADJ),cjl(MAXADJ),cju(MAXADJ)
      INTeger iwpri(MAXEQN),iwpc(MAXEQN)
      logical wzero(MAXPT)
c
      equivalence (l,luspec),(u,luspec(MAXLU+1))
      equivalence (di,luspec(MAXLU2+1)),(x,luspec(MAXLU2+MAXEQN+1))
      equivalence (iwelpt,luspec),(wzero,luspec(MAXPT1))
c
c...Symbolic/min. degree
      integer tv(MAXLU2),tl(MAXLU2), head(MAXEQN)
      integer kp(MAXEQN),ixt(MAXEQ2),ixb(MAXEQN)
c
      equivalence (tv,luspec),(tl,luspec(MAXLU+1))
      equivalence (head,luspec(MAXLU2+1))
      equivalence (kp,a),(ixt,a(MAXEQN+1)),(ixb,a(MAXEQ2+1))
c
c...ICCG
      double precision aw(2*MAXAG),resid(MAXEQN),p(MAXEQN)
      double precision ap(MAXEQN),qinvr(MAXEQN),aqinvr(MAXEQN)
      integer ilsh(MAXEQN)
c
      equivalence (aw    ,luspec(     1)),(resid ,luspec(2*MAXAG+1))
      equivalence (p     ,luspec(2*MAXAG+MAXEQN+1))
      equivalence (ap    ,luspec(2*MAXAG+2*MAXEQN+1))
      equivalence (qinvr ,luspec(2*MAXAG+3*MAXEQN+1))
      equivalence (aqinvr,luspec(2*MAXAG+4*MAXEQN+1))
      equivalence (ilsh  ,luspec(2*MAXAG+5*MAXEQN+1))

c
