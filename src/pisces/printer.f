cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 13:19:21 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PRINTR
      include 'p2conf.h'
c 
c     All requested print information in PISCES is printed here.
c
c     Original : CHP
c     Revised  : MRP  July 1984
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      include     'blank.h'
c------------------------------------------------------------------
c
c 
      logical lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwbia1,lwbia2, 
     +        lwmate,lcoord ,lwamp2,lwque1,lwque2
      integer ixmin,ixmax,iymin,iymax 
      real cxmin,cxmax,cymin,cymax
c 
c****************************************************************** 
c 
c 
      call prnck(lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwamp2,
     +           lwque1,lwque2,lwbia1,lwbia2,lwmate,
     +           ixmin,ixmax,iymin,iymax,lcoord,
     +           cxmin,cxmax,cymin,cymax) 
      if(errflg) return
c                   do it 
      call mprnt(lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwamp2,
     +           lwque1,lwque2,lwbia1,lwbia2,lwmate,
     +           ixmin,ixmax,iymin,iymax,lcoord,
     +           cxmin,cxmax,cymin,cymax)
c                   return
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE MPRNT(lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwamp2,
     +                 lwque1,lwque2,lwbia1,lwbia2,lwmate,
     +                 ixmin,ixmax,iymin,iymax,
     +                 lcoord,cxmin,cxmax,cymin,cymax) 
      include 'p2conf.h'
c 
c     this routine prints all node information, element nodes 
c     or element geometry information depending on the 3 flags. 
c 
c                   11/02/79     C.H.Price
c                   2/84         MRP
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      include     'blank.h'
      include     'logunt.h'
      include     'setup.h'
      include     'sol.h'
      include     'stat.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'plttmp.h'
      include     'difftmp.h'
      integer TMPPAD(1323602)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      logical lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwbia1,lwbia2, 
     +        lwmate,lbc,lcoord,lwndo,lwindo,lwamp2,lwque1,lwque2
      integer numbc,ixmin,ixmax,iymin,iymax,num(3),node(3),nodes(3,3),
     +        materi(3),i,j,k,l,nod
      real epstmp(10),epstm2,cxmin,cxmax,cymin,cymax,mun,mup,sarea,iarea
      real tn,tp
      double precision tmp,tmp2,vpnt,vx,vy,out1,out2,out3,out4,out5
c 
c****************************************************************** 
c 
c                   start
c 
      if(lwamp1.or.lwamp2.or.lwque1.or.lwque2) call nxtel(p2t,p2tc)
c
c...Controller section
100   if(lpoint) goto 1
      if(lelmnt) goto 2
      if(lgeom ) goto 3
      if(lwbia1) goto 4
      if(lwamp1) goto 5
      if(lwamp2) goto 5
      if(lwsol1) goto 6
      if(lwbia2) goto 4
      if(lwsol2) goto 6
      if(lwque1) goto 65
      if(lwque2) goto 65
      if(lwmate) goto 8
c                   all done, return
      write(luout,'(//)')
      return
c****************************************************************** 
c 
c                   point print 
c 
1     write(luout,1000) 
1000  format(//'1',25x,'Node information :',//, 
     +       ' Node   x coord    y coord     Doping      Qf   ',
     +       '       Material  Electrode',/, 
     +       '  (#)    (um)       (um)       (cm-3)      (cm-2)',
     +       '        (#)        (#)',
     +       /)
      numbc=1 
      lbc=.false. 
c
c...Scan points 
      do 50 i=1,np
c
c...Electrode node? 
      lbc=lm(i).ne.0
c
c...In print window?
      if(lwndo(i,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax,cymin, 
     +    cymax)) goto 11 
      goto 50 
c
c...If electrode, print with electrode no.
11    if(lbc) goto 10 
      if(lscale) then
         write(luout,1002) i,cord(1,i)*1.e4,cord(2,i)*1.e4,
     +         r1(i)*sngl(dcscl),qss(i)*sngl(dcscl),
     +                             itype(i),-ecode(eptr(i))
      else
         write(luout,1002) i,cord(1,i)*1.e4,cord(2,i)*1.e4,
     +         r1(i),qss(i),itype(i),-ecode(eptr(i))
      endif
cC1001  format(i5,1p,2e11.3,e12.3,e12.3,0p,i8)
      goto 50 
c
10    if(lscale) then
         write(luout,1002) i,cord(1,i)*1.e4,cord(2,i)*1.e4,
     +         r1(i)*sngl(dcscl),qss(i)*sngl(dcscl),itype(i),lm(i)
      else
         write(luout,1002) i,cord(1,i)*1e+4,cord(2,i)*1e+4,
     +                     r1(i),qss(i),itype(i),lm(i)
      endif
1002  format(i5,1p,2e11.3,e12.3,e12.3,0p,i8,i11) 
c
50    continue
      lpoint=.false.
      goto 100
c****************************************************************** 
c 
c                   element print 
c 
2     write(luout,2000) 
2000  format(//'1',25x,'Element information',//,
     +       3(' Elem# ----Nodes---- Mat#')/) 
      j=0 
      do 200 i=1,ne 
c 
c                   check window
      lwindo=.false.
      do 240 k=1,3
      node(k)=nop(k,i)
      if(lwndo(node(k),ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax, 
     +     cymin,cymax)) lwindo=.true.
240   continue
      if(lwindo) goto 245
      if(i.eq.ne) goto 255 
      goto 200
c 
c                   put info into print out array 
245   j=j+1 
      num(j)=i
      do 250 k=1,3
      nodes(k,j)=node(k)
250   continue
      materi(j)=imat(i) 
c 
c                   print buffer full?
255   if((j.ne.3.and.i.ne.ne).or.j.eq.0) goto 200
c 
c                   do it!
      write(luout,2001) (num(k),(nodes(l,k),l=1,3),materi(k),k=1,j) 
2001  format(3(5i5))
c 
c                   reset  j
      j=0 
c 
c                   next element
200   continue
c                   done with element print 
      lelmnt=.false.
      goto 100
c****************************************************************** 
c 
c                   geometry print
c 
3     write(luout,3000) 
3000  format(//'1',25x,'Geometry information',//,
     +      ' elem  i',12x,'eh/ed',8x,'jh/jd',13x,'node',7x,'area'/)
c 
c                   scan elements 
      do 70 i=1,ne
c                   inside window?
      do 68 j=1,3 
      nod=nop(j,i)
      if(lwndo(nod,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax,cymin, 
     +    cymax)) goto 69 
68    continue
c                   no, skip print
      goto 70 
c 
c                   yes, print
69    if(lscale) then
         tmp=epsmat(imat(i))
         write(luout,3001) i,
     +      (j,ehed(j,i)/tmp,jhjd(j,i),nop(j,i),es(j,i),j=1,3) 
      else
         write(luout,3001) i,
     +      (j,ehed(j,i),jhjd(j,i),nop(j,i),es(j,i),j=1,3) 
      endif
3001  format(i5,3(i3,1pe19.4,1pe13.4,i14,1pe15.4/,5x))
c 
c                   next element
70    continue
c                   done with geometry print
      lgeom=.false. 
      goto 100
c****************************************************************** 
c 
c                   bias print
c 
4     if(.not.lwbia1) goto 45
      if(ldiff) goto 48
      if(.not.lsolst) goto 40
c
c...Actual solution biases
      write(luout,4000) 
4000  format(//'1',30x,'*** Solution ***',//,20x,'Bias',/)
      goto 46
c
c...Setup 1 biases
40    write(luout,4004) 
4004  format(//'1',15x,'*** Previous Solution 1 ***',//,
     +       20x,'New bias',/) 
      goto 46
c
c...Difference
48    write(luout,4008)
4008  format(//'1',15x,'*** Difference of Solutions 1 and 2 ***',//,
     +         20x,'Bias Difference',/)
c
46    write(luout,4001) (dktq*bias(i),i=1,nelect)
4001  format(1p4e17.8)
      lwbia1=.false.
      write(luout,5001) 
5001  format(/,20x,'Terminal voltages'/) 
      write(luout,4001) (dktq*vres(i),i=1,nelect) 
      write(luout,5000) 
5000  format(/,20x,'Terminal currents'/) 
      write(luout,4001) (amp(i),i=1,nelect) 
      write(luout,5011) 
5011  format(/,20x,'Terminal fluxes'/) 
      write(luout,4001) (dflux(i),i=1,nelect) 
      goto 100
c
c...Setup 2 biases
45    write(luout,4007) 
4007  format(//'1',15x,'*** Previous Solution 2 ***',//,20x,'Bias',/) 
      write(luout,4001) (dktq*obias(i),i=1,nelect) 
      lwbia2=.false.
      write(luout,5001) 
      write(luout,4001) (dktq*vres0(i),i=1,nelect) 
      write(luout,5000) 
      write(luout,4001) (oamp(i),i=1,nelect) 
      write(luout,5011) 
      write(luout,4001) (dflux0(i),i=1,nelect) 
      goto 100
c****************************************************************** 
c 
c                   current 
c 
5     if(lwamp1) then
         write(luout,6051) 
6051     format(//' Node',7x,'|Jn|',7x,'|Jp|',6x,'|Jcond|',4x,
     +          '|Jdisp|',3x,'|Jtotal|')
      else
         write(luout,6053) 
6053     format(//' Node',7x,'|Jn|',7x,'|Jp|',6x,'|Jcond|')
      endif
c
c...Loop through nodes
      do 560 i=1,np
c 
c...Inside window?
      if(.not.lwndo(i,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax, 
     +    cymin,cymax)) goto 560 
c
c...Solution 1? 
      if(.not.lwamp1) goto 558
      if(ldiff) then
         out1=jxin(i)
         out2=jxip(i)
         out3=jxic(i)
         out4=jxid(i)
         out5=jxit(i)
      else
         out1=vpnt(i,2,vx,vy,.false.)
         out2=vpnt(i,3,vx,vy,.false.)
         out3=vpnt(i,1,vx,vy,.false.)
         out4=vpnt(i,4,vx,vy,.false.)
         out5=vpnt(i,5,vx,vy,.false.)
      endif
      write(luout,6052) i,out1,out2,out3,out4,out5
6052  format(i5,3x,1p6e11.4) 
      goto 560 
c 
c...Solution 2
558   out1=vpnt(i,2,vx,vy,.false.)
      out2=vpnt(i,3,vx,vy,.false.)
      out3=vpnt(i,1,vx,vy,.false.)
      write(luout,6052) i,out1,out2,out3
c 
c...Next node 
560   continue
c
c...Done
      if(lwamp1) then
         lwamp1=.false.
      else
         lwamp2=.false.
      endif
      goto 100
c****************************************************************** 
c 
c                   solution
c 
6     write(luout,6001) 
6001  format(//' Node',8x,'v',11x,'n',11x,'p',10x,'qfn',
     +         9x,'qfp')
c 
c...Scan nodes
      do 60 i=1,np
c 
c...Inside window?
      if(.not.lwndo(i,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax, 
     +    cymin,cymax)) goto 60 
c
c...Solution 1? 
      if(.not.lwsol1) goto 58
      out1=fv(i)*dktq
      out2=fn(i)*dcscl
      out3=fp(i)*dcscl
      out4=qfn(i)*dktq
      out5=qfp(i)*dktq
      goto 59 
c 
c...Solution 2
58    out1=ofv(i)*dktq
      out2=ofn(i)*dcscl
      out3=ofp(i)*dcscl
      out4=out1-dktq*dlog(ofn(i))
      out5=out1+dktq*dlog(ofp(i))
c 
c...Print
59    write(luout,6002) i,out1,out2,out3,out4,out5
6002  format(i5,3x,0pf10.5,1p5e12.4)
c 
c...Next node 
60    continue
c 
c...Done,  sol1?
      if(.not.lwsol1) goto 61
      lwsol1=.false.
      goto 100
c 
c...Done, sol2?
61    lwsol2=.false.
      goto 100
c****************************************************************** 
c 
c                   QUE
c 
65    write(luout,6501) 
6501  format(//' Node',9x,'Q',11x,'QA',10x,'U',10x,'|E|')
c 
c...Scan nodes
      do 650 i=1,np
c 
c...Inside window?
      if(.not.lwndo(i,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax, 
     +    cymin,cymax)) goto 650 
      call nodea(i,sarea,iarea)
c
c...Solution 1? 
      if(.not.lwque1) goto 658
      if(ldiff) then
         out1=(fp(i)-fn(i)-ofp(i)+ofn(i))*dcscl*qcharg
         out2=out1*sarea
         out3=uxi(i)
         out4=exi(i)
      else
         tmp=(fp(i)-fn(i)+r1(i))*dcscl*qcharg
         tmp2=0.d0
         if(iarea.gt.0.d0) tmp2=qss(i)*dintf(i)*dcscl*qcharg/iarea
         out1=tmp+tmp2
         out2=sarea*tmp+iarea*tmp2
         out3=vpnt(i,7,vx,vy,.false.)
         out4=vpnt(i,6,vx,vy,.false.)
      endif
      goto 659 
c 
c...Solution 2
658   tmp=(ofp(i)-ofn(i)+r1(i))*dcscl*qcharg
      tmp2=0.d0
      if(iarea.gt.0.d0) tmp2=qss(i)*dintf(i)*dcscl*qcharg/iarea
      out1=tmp+tmp2
      out2=sarea*tmp+iarea*tmp2
      out3=vpnt(i,7,vx,vy,.true.)
      out4=vpnt(i,6,vx,vy,.true.)
c 
c...Print
659   write(luout,6502) i,out1,out2,out3,out4
6502  format(i5,3x,1p4e12.4)
c 
c...Next node 
650   continue
c 
c...Done
      if(lwque1) then
         lwque1=.false.
      else
         lwque2=.false.
      endif
      goto 100
c****************************************************************** 
c 
c                   material
c
c...If we havent had a model card (and havent scaled), error!
c...Otherwise scale/initialize here (if we havent already)
8     if((.not.lscale).and.(.not.lmodcd)) call erset(266,linum,0)
      if(errflg) return
      if(.not.lminit) call minit
      if(errflg) return
      if(.not.lscale) call mater
      if(errflg) return
c 
c...epsmat actually contains eps0*epsr q, get epsr
      do 80 i=1,nmat
      epstmp(i)=epsmat(i)*qcharg/(eps0*depsc)
80    continue
      epstm2=epsoq*qcharg/eps0
c 
      write(luout,8000) (i,mattyp(i),epstmp(i),i=1,nmat)
8000  format(//,' Material data',/
     +       '  num  type  rel permit',/
     +       8(i5,i6,f9.2,/)) 
c 
      tmp=dktq
      tmp2=dktq/(dcscl*dcscl)
      write(luout,8001) stype,epstm2,logni0/2.30258d0,fmun0,fmup0,vsat,
     +   taun0*qkt,taup0*qkt,(semmat(i),i=9,11),affin*tmp,egap*tmp,
     +   cnau*tmp2,cpau*tmp2,arichn,arichp,ncband*dcscl,
     +   nvband*dcscl
8001  format(//,' Semiconductor data',/ 
     +       '   type        =',1pg12.5,/ 
     +       '   rel permit  =',1pg12.5,/ 
     +       '   log10(ni)   =',1pg12.5,/ 
     +       '   n-mobility  =',1pg12.2,/ 
     +       '   p-mobility  =',1pg12.2,/ 
     +       '   vsat        =',1pg12.2,/ 
     +       '   taup0       =',1pg12.5,/ 
     +       '   taun0       =',1pg12.5,/ 
     +       '   egap(300)   =',1pg12.5,/ 
     +       '   egalpha     =',1pg12.5,/ 
     +       '   egbeta      =',1pg12.5,/ 
     +       '   affinity    =',1pg12.5,/ 
     +       '   egap        =',1pg12.5,/
     +       '   cnau        =',1pg12.5,/
     +       '   cpau        =',1pg12.5,/
     +       '   An**        =',1pg12.5,/
     +       '   Ap**        =',1pg12.5,/
     +       '   Nc          =',1pg12.5,/
     +       '   Nv          =',1pg12.5)
      write(luout,7001) gcband,gvband,edband,eaband
7001  format(
     +       '   gcb         =',1pg12.5,/
     +       '   gvb         =',1pg12.5,/
     +       '   edb         =',1pg12.5,/
     +       '   eab         =',1pg12.5)
c 
c...Contacts
      write(luout,8002)
8002  format(//' Contacts :',/
     +       '  Num',3x,'Work fn.',5x,'Vsurfn',9x,'Vsurfp',7x,
     +       'Resistance',4x,'Capacitance')
      do 822 i=1,nelect
      if(lcnres(i)) then
         write(luout,8322) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       1.d0/(decoef*cresis(i)),decoef*ccapac(i)
      else if(cresis(i).ne.0.d0) then
         write(luout,8222) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       1.d0/(decoef*cresis(i)),decoef*ccapac(i)
      else
         write(luout,8222) i,ktq*workf(i),ktq*vsn(i),ktq*vsp(i),
     +                       0.d0,decoef*ccapac(i)
      endif
8322  format(i4,0pf10.3,1x,3(1pe15.6),'D',1pe14.6)
8222  format(i4,0pf10.3,1x,4(1pe15.6))
c
      write(luout,1065) mudeg
1065  format(' Mobility model parameters :'/
     +          '  Gsurf   = ',1pe13.6)
      if(lfldmb) then
         if(stype.eq.1) then
            write(luout,1066) muco1,muco2
1066        format('  B.elect = ',1pe13.6/
     +             '  B.hole  = ',1pe13.6///)
         else if(stype.eq.2) then
            write(luout,1067) muco1
1067        format('  E0      = ',1pe13.6///)
         endif
      else
         write(luout,1069) ' '
1069     format(a1///)
      endif
c
      if(lconlt) then
         write(luout,1068) nsrhn,nsrhp
1068     format(' Lifetime model parameters :'/
     +          '  Nsrhn   = ',1pe13.6/
     +          '  Nsrhp   = ',1pe13.6///)
      endif
c 
cdbg      ltmp=0.
cdbg      do 823 j=1,nb
cdbg      if(lm(nbc(j)).eq.i) ltmp=ltmp+lmetal(j)
cdbg823   continue
cdbg      write(*,*) 'Length = ',ltmp*1.e4
c
822   continue
c
c...If concentration dep. mobility, print it
      write(luout,8003)
8003  format(//'  Node',5x,'Mobility(n)',5x,'Mobility(p)',5x,
     +                     'Lifetime(n)',5x,'Lifetime(p)'/
     +       12x,'cm**2/V-s',7x,'cm**2/V-s',10x,'sec',13x,'sec')
      do 801 i=1,np
      if(.not.lwndo(i,ixmin,ixmax,iymin,iymax,lcoord,cxmin,cxmax, 
     +    cymin,cymax)) goto 801
      if(lconmb) then
         mun=mobn(i)
         mup=mobp(i)
      else
         mun=fmun0
         mup=fmup0
      endif
      if(lconlt) then
         tn=taun(i)*qkt
         tp=taup(i)*qkt
      else
         tn=taun0*qkt
         tp=taup0*qkt
      endif
      tmp=0.0d0
      if (essem(i).gt.0.) tmp=dintf(i)/essem(i)
      tn=1.d0/(1.d0/tn+snintf(i)*tmp)
      tp=1.d0/(1.d0/tp+spintf(i)*tmp)
      write(luout,8004) i,mun,mup,tn,tp
8004  format(i5,0pf15.2,0pf16.2,1x,2(1pe16.3))
801   continue
c
c...Done
      lwmate=.false.
      goto 100
c
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      LOGICAL FUNCTION LWNDO(node,ixmin,ixmax,iymin,iymax,lcoord,cxmin, 
     +                 cxmax,cymin,cymax) 
      include 'p2conf.h'
c 
c     date code: 800909 
c 
c                   this function returns .true. if the node is inside
c                   the specified window
c 
c                   "i" prefix denotes grid indices 
c                   "c" prefix denotes coordinates
c 
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lcoord
      integer ixmin,ixmax,iymin,iymax,ix,iy,node
      real cxmin,cxmax,cymin,cymax,xcord,ycord
c 
c****************************************************************** 
c 
c                   start 
c 
c                   assume true 
      lwndo=.true.
c                   see if a coordinate is specified
      if (.not.lcoord) goto 100 
c 
c                   yes get node coord and compare. 
      xcord=cord(1,node)
      ycord=cord(2,node)
      if (xcord.lt.cxmin.or.xcord.gt.cxmax.or.
     +    ycord.lt.cymin.or.ycord.gt.cymax) goto 200
      goto 9999 
c 
c                   check index limits
100   ix=(node-1)/ny+1
      iy=node-(ix-1)*ny 
      if (ix.ge.ixmin.and.ix.le.ixmax.and.
     +    iy.ge.iymin.and.iy.le.iymax) goto 9999
c 
c                   not inside window 
200   lwndo=.false. 
c 
c                   done
9999  return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PRNCK(lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwamp2,
     +                 lwque1,lwque2,lwbia1,lwbia2,lwmate,
     +                 ixmin,ixmax,iymin,iymax,
     +                 lcoord,cxmin,cxmax,cymin,cymax) 
      include 'p2conf.h'
c 
c                   print card check routine
c 
c     Original : CHP
c     Revised  : MRP  July 1984
c
c     copyright c 1981 the board of trustees of the Leland Stanford 
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'stat.h'
      include     'setup.h'
      include     'plot.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lpoint,lelmnt,lgeom,lwsol1,lwsol2,lwamp1,lwbia1,lwbia2, 
     +        lwmate,lwset1,lwas1,lcoord,lwamp2,lwque1,lwque2,lque1
      integer ixmin,ixmax,iymin,iymax 
      real cxmin,cxmax,cymin,cymax
c
c FUNCTIONS:
      logical isrval, islval
      real    gtrval
      logical gtlval
c****************************************************************** 
c 
c                   start 
c
c...Initialize
      lwsol1=.false.
      lwsol2=.false.
      lwamp1=.false.
      lwamp2=.false.
      lwbia1=.false.
      lwbia2=.false.
      lwque1=.false.
      lwque2=.false.
c
c                   coordinate limits specified?
      lcoord=.false.
      if(lrect.and.(.not.isrval(5).and..not.isrval(6).and.
     +    .not.isrval(7).and..not.isrval(8))) goto 5 
      lcoord=.true. 
      call devlts(cxmin,cxmax,cymin,cymax)
      if (isrval(5)) cxmin=gtrval(5)*1e-4
      if (isrval(6)) cxmax=gtrval(6)*1e-4
      if (isrval(7)) cymin=gtrval(7)*1e-4
      if (isrval(8)) cymax=gtrval(8)*1e-4
c 
c                   get line number limits
5     ixmin=1 
      ixmax=nx
      iymin=1 
      iymax=ny
      if (isrval(1)) ixmin=gtrval(1) 
      if (isrval(2)) ixmax=gtrval(2) 
      if (isrval(3)) iymin=gtrval(3) 
      if (isrval(4)) iymax=gtrval(4) 
c 
c                   get parameters
c 
      lpoint=gtlval(1)
      lelmnt=gtlval(2)
      lgeom =gtlval(3)
      lwsol1=gtlval(4)
      lwset1=gtlval(5)
      lwsol2=gtlval(6)
      lwamp1=gtlval(7)
      lwas1=gtlval(8)
      lwamp2=gtlval(10)
      lwque1=gtlval(11)
      lque1=gtlval(12)
      lwque2=gtlval(13)
      lwmate=gtlval(9)
c                   mesh data present?
      if (.not.(lpoint.or.lelmnt.or.lgeom).or.(lmshdn)) goto 20 
      lpoint=.false.
      lelmnt=.false.
      lgeom =.false.
      call erset(-25,linum,0) 
c
c                   solution present? 
20    if (.not.(lwque1.or.lwsol1.or.lwamp1).or.lsol1.or.ldiff) goto 30
      lwque1=.false.
      lwsol1=.false.
      lwamp1=.false.
      call erset(-66,linum,0) 
c
c                   setup 1 present?
30    if (.not.(lque1.or.lwset1.or.lwas1).or.lsol1) goto 40 
      lwset1=.false.
      lwas1=.false.
      lque1=.false.
      call erset(-68,linum,0) 
c
c                   setup 2 present?
40    if (.not.(lwque2.or.lwsol2.or.lwamp2).or.lsol2) goto 50
      lwque2=.false.
      lwsol2=.false.
      lwamp2=.false.
      call erset(-67,linum,0) 
c
c...Ok for difference?
50    if(.not.(lwque1.or.lwamp1).or.(.not.ldiff).or.ldcur) goto 60
      lwamp1=.false.
      lwque1=.false.
      call erset(-165,linum,0)
c
c...Get control flags
60    lwamp1=lwamp1.or.lwas1
      lwsol1=lwsol1.or.lwset1
      lwque1=lwque1.or.lque1
      lwbia1=lwamp1.or.lwsol1.or.lwque1
      lwbia2=lwamp2.or.lwsol2.or.lwque2
c
c...X or Y component of vectors
      lxcomp=.false.
      lycomp=.false.
      if(islval(29)) lxcomp=gtlval(29)
      if(islval(30)) lycomp=gtlval(30)
      if(lxcomp.and.lycomp) then
        lxcomp=.false.
        lycomp=.false.
      else if(ldiff.and.(lxcomp.or.lycomp)) then
         lxcomp=.false.
         lycomp=.false.
         call erset(-40,linum,0)
      endif
c
c                   done
      return
      end 
