cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon Jul  9 13:02:56 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      REAL FUNCTION RMOB(hflag,conc)
      include 'p2conf.h'
      logical hflag 
      real conc 
c 
c     DATE CODE: JUL. 23, 1980
c-----------------------------------------------------------------------
c 
c     RMOB : CALCULATES HOLE AND ELECTRON MOBILITY AS A FUNCTION OF 
c            IMPURITY CONCENTRATION 
c 
c            HFLAG - HOLE FLAG (T/F = HOLE/ELECTRON)
c            CONC - CONCENTRATION (CM-3)
c 
c     Copyright C 1981 The Board of Trustees of the Leland Stanford 
c                      Junior University. All Rights Reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
c-----------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------- 
      integer im1,ndata,idec1,idecad,i,irange
      real    concn(36),emob(36),hmob(36),clog,clog1,clog2,ratio
      real    rmob1,rmob2,flog2,range
      data    ndata/36/,idec1/14/ 
      data    flog2/.30102992/
c 
c------------------------------------------------------------------------ 
c     mobility data are for 300 degrees k 
c 
c     electrons 
c     ========= 
c 
c 
c     impurity concentration (cm-3) :            reference :
c 
c     1e14 - 1e15                                extrapolation
c     2e15 - 6e19                                irvin
c     8e19 - 4e20                                michel and guerin (as) 
c     6e20 - 1e21                                extrapolation
c 
c 
c 
c     holes 
c     ===== 
c 
c 
c     impurity concentration (cm-3) :            reference :
c 
c 
c     1e14 - 8e14                                extrapolation
c     1e15 - 8e17                                joy
c     1e18 - 1e21                                michel 
c 
c-----------------------------------------------------------------------
c 
c     electron mobility data
c 
c                  1        2        4        6        8
      data emob/1350.0,  1345.0,  1335.0,  1320.0,  1310.0, 
     +          1300.0,  1248.0,  1200.0,  1156.0,  1115.0, 
     +          1076.0,   960.0,   845.0,   760.0,   720.0, 
     +           675.0,   524.0,   385.0,   321.0,   279.0, 
     +           252.0,   182.5,   140.6,   113.6,    99.5, 
     +            90.5,    86.9,    83.4,    78.8,    71.6, 
     +            67.8,    52.0,    35.5,    23.6,    19.0, 
     +            17.8/ 
c 
c     hole mobility data
c 
      data hmob/ 495.0,   495.0,   495.0,   495.0,   495.0, 
     +           491.1,   487.3,   480.1,   473.3,   466.9, 
     +           460.9,   434.8,   396.5,   369.2,   348.3, 
     +           331.5,   279.0,   229.8,   203.8,   186.9, 
     +           178.0,   130.0,    90.0,    74.5,    66.6, 
     +            61.0,    55.0,    53.7,    52.9,    52.4, 
     +            52.0,    50.8,    49.6,    48.9,    48.4, 
     +            48.0/ 
c 
c     corresponding impurity concentrations 
c 
      data concn/  1e14,    2e14,    4e14,    6e14,    8e14,
     +             1e15,    2e15,    4e15,    6e15,    8e15,
     +             1e16,    2e16,    4e16,    6e16,    8e16,
     +             1e17,    2e17,    4e17,    6e17,    8e17,
     +             1e18,    2e18,    4e18,    6e18,    8e18,
     +             1e19,    2e19,    4e19,    6e19,    8e19,
     +             1e20,    2e20,    4e20,    6e20,    8e20,
     +             1e21/
c 
c---------------------------------------------------------------------
c 
c     start of rmob 
c 
c     note: the interval finding routine requires that the data 
c     must start on a decade boundary (eg. 1e13, 1e14, etc.) and
c     that the 1, 2, 4, 6, 8 spacing must be maintained. when 
c     adding data, set: 
c                   ndata=total number of data entries
c                   idec =decade number of first entry
c 
c---------------------------------------------------------------------- 
c.....extrapolate for very low or very high concentrations
      if(conc.gt.concn(1)) go to 5
      rmob=emob(1)
      if(hflag) rmob=hmob(1)
      go to 1000
5     if(conc.lt.concn(ndata)) go to 8
      rmob=emob(ndata)
      if(hflag) rmob=hmob(ndata)
      go to 1000
c 
c.....find the interval containing conc 
8     clog=alog10(conc)
      idecad=int(clog)
      range=(clog-float(idecad))/flog2
      irange=int(range)+1 
      if (range.gt.2.5849628) irange=irange+1 
      im1=(idecad-idec1)*5+irange 
      i=im1+1 
c 
      clog1=alog10(concn(im1)) 
      clog2=alog10(concn(i)) 
c 
c.....linear interpolation
      if (hflag) go to 40 
      rmob1=emob(im1) 
      rmob2=emob(i) 
      go to 50
40    rmob1=hmob(im1) 
      rmob2=hmob(i) 
50    ratio=(rmob2-rmob1)/(clog2-clog1) 
      rmob=rmob1 + (clog-clog1)*ratio 
c 
1000  return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION GMOB(hflag,conc)
      include 'p2conf.h'
      logical hflag
      real conc 
c 
c     date code: jul. 23, 1980
c-----------------------------------------------------------------------
c 
c     gmob : calculates hole and electron mobility as a function of 
c            impurity concentration 
c 
c            conc - concentration (cm-3)
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
c-----------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------- 
      integer im1,ndata,idec1,idecad,i,irange
      real    concn(26),emob(26),hmob(26),clog,clog1,clog2,ratio
      real    rmob1,rmob2,flog2,range
      data    ndata/26/,idec1/14/ 
      data    flog2/.30102992/
c 
c------------------------------------------------------------------------ 
c      mobility data are for 300 degrees k (we need better data here!!)
c-----------------------------------------------------------------------
c 
c     electron mobility data
c 
c                  1        2        4        6        8
      data emob/8000.0,  7718.0,  7445.0,  7290.0,  7182.0, 
     +          7300.0,  6847.0,  6422.0,  6185.0,  6023.0, 
     +          5900.0,  5474.0,  5079.0,  4861.0,  4712.0, 
     +          4600.0,  3874.0,  3263.0,  2950.0,  2747.0, 
     +          2600.0,  2060.0,  1632.0,  1424.0,  1293.0, 
     +          1200.0/
c 
c     hole mobility data
c 
      data hmob/ 390.0,   380.0,   375.0,   360.0,   350.0, 
     +           340.0,   335.0,   320.0,   315.0,   305.0, 
     +           302.0,   300.0,   285.0,   270.0,   245.0, 
     +           240.0,   210.0,   205.0,   200.0,   186.9, 
     +           170.0,   130.0,    90.0,    74.5,    66.6, 
     +            61.0/
c 
c     corresponding impurity concentrations 
c 
      data concn/  1e14,    2e14,    4e14,    6e14,    8e14,
     +             1e15,    2e15,    4e15,    6e15,    8e15,
     +             1e16,    2e16,    4e16,    6e16,    8e16,
     +             1e17,    2e17,    4e17,    6e17,    8e17,
     +             1e18,    2e18,    4e18,    6e18,    8e18,
     +             1e19/
c 
c---------------------------------------------------------------------
c 
c     start of gmob 
c 
c     note: the interval finding routine requires that the data 
c     must start on a decade boundary (eg. 1e13, 1e14, etc.) and
c     that the 1, 2, 4, 6, 8 spacing must be maintained. when 
c     adding data, set: 
c                   ndata=total number of data entries
c                   idec =decade number of first entry
c 
c---------------------------------------------------------------------- 
c.....extrapolate for very low or very high concentrations
      if(conc.gt.concn(1)) go to 5
      gmob=emob(1)
      if(hflag) gmob=hmob(1)
      go to 1000
5     if(conc.lt.concn(ndata)) go to 8
      gmob=emob(ndata)
      if(hflag) gmob=hmob(ndata)
      go to 1000
c 
c.....find the interval containing conc 
8     clog=alog10(conc)
      idecad=int(clog)
      range=(clog-float(idecad))/flog2
      irange=int(range)+1 
      if (range.gt.2.5849628) irange=irange+1 
      im1=(idecad-idec1)*5+irange 
      i=im1+1 
c 
      clog1=alog10(concn(im1)) 
      clog2=alog10(concn(i)) 
c 
c.....linear interpolation
      if (hflag) go to 40 
      rmob1=emob(im1) 
      rmob2=emob(i) 
      go to 50
40    rmob1=hmob(im1) 
      rmob2=hmob(i) 
50    ratio=(rmob2-rmob1)/(clog2-clog1) 
      gmob=rmob1 + (clog-clog1)*ratio 
c 
1000  return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION AMOB(hflag,conc1,temper)
      include 'p2conf.h'
c
c     Analytic function to calculate low-field mobility based on
c     total impurity concentration and temperature.
c
c              hflag......true if holes
c              conc1......total concentration
c              temper.....temperature
c
c     For Si see Selberherr, Microelectron. Reliab., vol.24, no.2,
c     pp. 225-257, 1984.
c
c     Original : MRP  Aug 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      logical hflag
      real conc1,temper,tmp
c
      real uminn,uminp,bn,bp,cn,cp,non,nop,an,ap,gn,gp
      data an,bn,cn,gn,non,uminn/ 7.12e8, -3.8, 0.73, -2.3,
     +                            1.072e17, 55.24 /
      data ap,bp,cp,gp,nop,uminp/ 1.35e8, -3.7, 0.70, -2.2,
     +                            1.606e17, 49.7 /
c
      if(hflag) then
         tmp=1./(1.+((temper/300.)**bp)*((conc1/nop)**cp))
         amob=ap*(temper**gp)*tmp+uminp*(1.-tmp)
      else
         tmp=1./(1.+((temper/300.)**bn)*((conc1/non)**cn))
         amob=an*(temper**gn)*tmp+uminn*(1.-tmp)
      endif
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      REAL FUNCTION IMOB(hflag,conc1)
      include 'p2conf.h'
c
c     Analytic function to calculate the low-field mobility due to
c     ionized impurity scattering and based on total impurity
c     concentration and temperature.
c
c              hflag......true if holes
c              conc1......total concentration
c
c     See C.C. Abbas, "Transient One-Dimensional Numerical Analysis
c     of Bipolar Power Semiconductor Devices", Ph.D. Thesis, No. 7614,
c     Swiss Federal Institute of Technology, Zuerich, Switzerland, 1984.
c     &
c     See J.-M. Dorkel and Ph. Leturq, "Carrier Mobilities in Silicon
c     Semiempirically Related to Temperature, Doping and Injection Level",
c     Solid-State Electronics, vol. 24, pp. 821-825, 1981.
c
c     Original : C. Christiaan Abbas     Sep 87
c
c     Copyright c 1987 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'impact.h'
c
c***********************************************************************
c
c                   type declarations 
c
      logical  hflag
      real     cl,conc1,gb
c
c * Start *
c
      cl=conc1
      if(cl.gt.1.0e19) cl=1.0e19
      if(hflag) then
c Hole mobility
         gb=log(1.0+hb/cl)-hb/(cl+hb)
         imob=ha/(cl*gb)
      else
c Electron mobility
         gb=log(1.0+eb/cl)-eb/(cl+eb)
         imob=ea/(cl*gb)
      endif
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Mon Nov 20 15:07:41 PST 1989 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE CCSMOB(n1,n2,lmup,mu21)
      include 'p2conf.h'
c 
c     Calculate hole and electron mobility using
c     carrier-carrier scattering dependent mobility
c     (ionized) impurity concentration dependent mobility
c     lattice scattering dependent mobility
c
c     See C.C. Abbas, "Transient One-Dimensional Numerical Analysis
c     of Bipolar Power Semiconductor Devices", Ph.D. Thesis, No. 7614,
c     Swiss Federal Institute of Technology, Zuerich, Switzerland, 1984.
c     &
c     See J.-M. Dorkel and Ph. Leturq, "Carrier Mobilities in Silicon
c     Semiempirically Related to Temperature, Doping and Injection Level",
c     Solid-State Electronics, vol. 24, pp. 821-825, 1981.
c
c     dfdc....is the derivative of mobility with respect to the carrier
c             concentration divided by the value of the mobility
c
c     G. Anderson                                  Nov, 1989
c      (based on work by C.C. Abbas)
c     C.C. Abbas  (carrier-carrier scattering)     Sep, 1987
c 
c     Copyright c 1989 The board of trustees of the Leland Stanford 
c                      Junior University.  All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'impact.h'
      include     'emaco.h'
c------------------------------------------------------------------
c 
c                   parameter declarations 
c 
      double precision  mu21
      integer           n1,n2
      logical           lmup
c 
c                   type declarations 
c 
      double precision  aux1,aux2,ceh,cel,cho,dmuco,
     &                  ex,gcc,mucc,othird
      real              mul
c 
c-----------------------------------------------------------------------
c 
c                   data section
c 
      data othird/0.3333333333333333/
c 
c-----------------------------------------------------------------------
c 
      dfdc=0.d0
      if(lmup) then 
c For carrier-carrier scattering for holes
         dmuco=dmucop
c Lattice dependent scattering for holes
         mul=mulp
      else
c For carrier-carrier scattering for electrons
         dmuco=dmucon
c Lattice dependent scattering for electrons
         mul=muln
      endif
c 
c-----------------------------------------------------------------------
c 
c             carrier-carrier scattering dep. mobility term
c
      cel=dsqrt(fn(n1)*fn(n2))
      cho=dsqrt(fp(n1)*fp(n2))
      ceh=cel*cho
      if(ceh.lt.1.0d-37) then
         ceh=1.0d-37
c         write(6,*) 'ceh < 1.0d-37'
      endif
      aux1=ceh**othird
      gcc=dlog(1.0d0+cd/aux1)
      mucc=cc/(dsqrt(ceh)*gcc)
c 
c-----------------------------------------------------------------------
c 
c             lattice scattering term,
c             (ionized) impurity scattering term and
c             total mobility 
c        
      ex=dsqrt(6.0d0*mul*(mu21+mucc)/(mu21*mucc))
      aux2=1.0d0+(ex/1.68d0)**1.43d0
      mu21=mul*(1.025d0/aux2-0.025d0)
      if(ceh.gt.1.0d-37) then
c Partial derivative of the ccs dependent mob w.r.t. a carrier
         dfdc=(cd/(gcc*(aux1+cd))-1.5d0)/mucc
c Partial derivative of the total mob w.r.t. a carrier
         dfdc=dmuco*((1.68d0/ex)**0.57d0)/(aux2*aux2)*dfdc
      endif
c 
c...remember, dfdc is the derivative of mu w/ respect to a carrier
c...DIVIDED BY mu21. The factor of two results from the partial derivative
c...of cel or cho w.r.t. fn(n1), etc.
      dfdc=dfdc/(2.0d0*mu21)
c
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      REAL FUNCTION AMOB2(hflag,conc1,temper)
      include 'p2conf.h'
c
C...SHIN   11/87
c-----new mobility model, Ref.: Arora, Hauser, Roulston,
c                               IEEE ED-29,pp 292-295
c
c-----NOTE:  This implementation corrects the typo in the above reference!
c
c     Analytic function to calculate low-field mobility based on
c     total impurity concentration and temperature.
c
c              hflag......true if holes
c              conc1......total concentration
c              temper.....temperature
c
c     This is the bulk model you get when you specify "arora"
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c-------------------------------------------------------------
c
c     common for setup of initial guesses
c
C...SHIN
      include    'setup.h'
      logical hflag
      real conc1,temper,tmp,denom,ualpha
c
      real bn,bp,cn,cp,an,ap,gn,gp,HN,HP
      data an,bn,cn,gn,HN/ -0.57, -2.33, 1.26e17, 2.4, -0.146/
      data ap,bp,cp,gp,HP/ -0.57, -2.23, 2.35e17, 2.4, -0.146/
c
      tmp=temper/300.
      if(hflag) then
         ualpha=0.88*TMP**HP
         denom=1.+abs((conc1/(CP*TMP**GP)))**ualpha
         amob2=mu1p*(tmp**ap) + mu2p*(tmp**bp)/denom
      else
         ualpha=0.88*TMP**HN
         denom= 1.+abs((conc1/(CN*TMP**GN)))**ualpha
         amob2=mu1n*(tmp**an) + mu2n*(tmp**bn)/denom
      endif
      return
      end
