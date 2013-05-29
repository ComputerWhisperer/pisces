cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Wed Mar 28 17:49:18 PST 1990 (pisces--stanford)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DIRACI(arg, Vpot, dVpot) 
      include 'p2conf.h'
c
c     Calculate the iverse to the fermi-dirac integral. The joyce Dixon 
c     appr. is used over much of the interval with an asympotic analyti
c     cal formula used at the upper end.
c
c   
c     Original: H.R.Yeager       Stanford University     July 1985
c
c
c     CALLS TO  : 
c     CALLED BY : DIRAC
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      include     'blank.h'
c------------------------------------------------------------------

      double precision arg,Vpot,dVpot
      double precision jda,jdb,jdc,jdd,  pi,tmp,offset, dexpu
      
      data jda, jdb, jdc, jdd, offset, pi
     1 /3.5355339059327379d-001, -4.9500897298752622d-003,
     2  1.4838577128872821d-004, -4.4256301190009895d-006,
     3 -1.370382755377d-002,      3.14159d0/
c------------------------------------------------------------------


c..   diraci is valid for both holes and electrons.
c..    condition       arg              Vpot
c..    electrons       n/Nc        (Vcb - PHIn)/Vt + offset
c..    holes           p/Nv        (PHIp - Vvb)/Vt + offset
   
      if (arg.LT.mindbl)  then
         dVpot =  1.0d0/mindbl
         Vpot  =  lgmind
         return 
      endif
      
      if (arg .GT. 8.463d0) then
c..     use asympotic expansion to F1/2inv
        tmp   = 3.0d0*dsqrt(pi)*(arg)/4.0d0
        Vpot  = dsqrt( dexpu(dlog(tmp)*4.0d0/3.0d0) - pi*pi/6.0d0)
        dVpot = 0.5d0/(Vpot)
        dVpot = (dVpot)*(dexpu(dlog(tmp)/3.0d0))*
     1                                        (4.0d0/3.0d0)*(tmp/(arg))
        Vpot  = Vpot + offset
c..     Offset is used to join the two curves at arg=8.463. THe value
c..     of 8.463 was choose because the slopes of the two curves are 
c..     equal there. a fortunate occurance.
      else  
c..     use standard joyce dixon.
        Vpot  = dlog(arg) + arg*(jda+arg*(jdb+arg*(jdc+arg*jdd)))
        dVpot = 1.0/(arg) + 
     1          (jda+arg*(2.0d0*jdb+arg*(3.0d0*jdc+arg*4.0d0*jdd)))
      endif 
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE DIRAC(arg, Cconc, dCconc) 
      include 'p2conf.h'
c
c     Calculate the value of the fermi-dirac integral. The Subroutine
c     DIRACI is used to do this along with a standard Newton iteration.
c     This may seem odd, but it is useful in keeping the program modular
c     since only on routine need be changed.
c   
c     Original: H.R.Yeager       Stanford University     July 1985
c
c
c     CALLS TO  : DIRACI
c     CALLED BY : ALL OVER
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      
      include     'blank.h'
c------------------------------------------------------------------

      double precision arg,Cconc,dCconc
      double precision xx,delxx,derr,tmp, offset,pi, dexpu
      integer ii
      double precision m0(4),m1(4),m2(4)
      
      data m0,m1,m2
     1   / -8.000118589197264d+00, -4.006432391040088d+00,
     2     -2.676870174717364d-01,  1.873953425972189d+00,
     3      9.997851195235986d-01,  9.888110325119358d-01,
     4      7.709242036436873d-01,  3.509595176361950d-01,
     5     -8.180569226662326d-05, -4.205279213793461d-03,
     6     -5.902400452806975d-02, -3.349225474910988d-02  /
      data offset, pi
     1   / -1.370382755377d-002,    3.14159d0              /
c------------------------------------------------------------------
c
      if (arg.LT.lgmind) then
        Cconc=0.0d0
        dCconc=0.0d0
        return
      endif 
c
      if (arg.LT.-30.0d0)  then
        tmp=dexpu(arg)
        dCconc=tmp
        Cconc=tmp
        return
      endif
c
c..   No easy way out. Generate initial guess. Use look up table.
c..   Rational polynomials al la Cody et. al. (Mathematics of Computation
c..   1967) do not prove to be faster. DIRAC is not a bottle neck, but
c..   DIRACI is.
c
      if (arg.LT.-10.0d0) then
         xx=dexpu(arg)
      else if (arg.GT.5.0d0) then
         tmp=arg-offset
         xx=4.0d0/(3.0d0*dsqrt(pi))
     1                          *dexpu(0.75d0*dlog(tmp*tmp+pi*pi/6.0d0))
      else
        if (arg.LT.-2.0d0) then
           if (arg.LT.-6.0d0) then
             tmp=arg+8.0d0
             ii=1
           else
             tmp=arg+4.0d0
             ii=2
           endif
        else
           if (arg.LT.2.0d0) then
             tmp=arg
             ii=3
           else
             tmp=arg-4.0d0
             ii=4
           endif
        endif
         xx=dexpu(m0(ii)+tmp*(m1(ii)+tmp*m2(ii)))
      endif

c..   finally, the newton loop.
 100  call diraci(xx,delxx,derr)
      delxx=(delxx-arg)/derr
      xx=xx-delxx
      if (dabs(delxx/xx).GT.1.0d-12) goto 100
c
      dCconc=1.0d0/derr
      Cconc=xx
      return
      end
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FERMIS(arg,Lgstar,Fs,dFs)
      include 'p2conf.h'
c
c     Calculate the value of a single energy level fermi statistic.
c   
c     Original: H.R.Yeager       Stanford University     July 1985
c
c
c     CALLS TO  : 
c     CALLED BY : POTRHS (method.f)
c     
c     Fs = 1.0/(1.0 + Gdstar*exp(arg))
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      include     'blank.h'
c------------------------------------------------------------------
c
      double precision arg,Lgstar,Fs,dFs,  tmp,dexpu
c------------------------------------------------------------------
c
      tmp=arg+Lgstar 
      if (tmp.LT.lgmind) then
        Fs = 1.0d0
        dFs= 0.0d0
        return
      endif
      if (tmp.GT.lgmaxd) then
        Fs = 0.0d0
        dFs= 0.0d0
        return
      endif
      tmp =   dexpu(tmp)
      Fs  =   1.0d0/(1.0d0+tmp)
      dFs = -Fs*tmp/(1.0d0+tmp)
c
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION GAMMA(arg)
      include 'p2conf.h'
c
c     Calculate the value gamma.
c   
c     Original: H.R.Yeager       Stanford University     July 1985
c
c
c     CALLS TO  : DIRACI
c     CALLED BY : POTRHS (method.f)
c     
c     calculate gamma = (F1/2(-))/exp(-) = arg/(exp(F1/2inv(arg))
c     where arg is either n/Nc or p/Nv
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      double precision arg, tmp, dtmp, dexpu
c------------------------------------------------------------------

      call diraci(arg,tmp,dtmp)
      if (tmp .GT. 80.0d0) then
         tmp = 5.54d34
      else if (tmp .LT. -80.0d0) then
         tmp = 1.805e-35
      else 
         tmp = dexpu(tmp)
      endif

      tmp = arg/tmp
c..   this is only used to calculate an initial guess in POTRHS, so we 
c..   can limit this on a zero excursion without worring about continuity.
      if (tmp .LT. 0.01d0) tmp = 0.01d0
      GAMMA = tmp
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE LNGAMMA(arg, LnGam, dLnGam)
      include 'p2conf.h'
c
c     Calculate the value of ln(gamma)
c   
c     Original: H.R.Yeager       Stanford University     July 1985
c
c
c     CALLS TO  : DIRACI
c     CALLED BY : ASSMBJ (solasmb1.f)
c     
c     calculate log( (F1/2)/exp ) = log( (arg)/exp(diraci(arg)) )
c     where arg is either n/Nc or p/Nv
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      double precision arg,LnGam,dLnGam,  tmp,dtmp
c------------------------------------------------------------------
  
      if (arg .LT. 1.0d-18) then
         LnGam  = 0.0d0
         dLnGam = 0.0d0
      else
         call diraci(arg,tmp,dtmp)
         LnGam  = dlog(arg) - tmp
         dLnGam = 1.0d0/(arg) - dtmp
      endif
  
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION POCCUP(lboltz, qfpp, psi, lgnie, nvb)
      include 'p2conf.h'
c
c     calculate hole concentration. Trivial but modular.
c
c     Original: H.R.Yeager       Stanford University     July 1985
c
c     CALLS TO  : DIRAC
c     CALLED BY : 
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      logical lboltz 
      double precision qfpp,psi, argp,tmp,dtmp,dexpu,lgnie
      real             nvb
c------------------------------------------------------------------

      if (lboltz) then
c          POCCUP=lgnie*dexpu(qfpp-psi)
           tmp   =(qfpp-psi)
           POCCUP=dexpu(tmp+lgnie)
         else
c          argp = qfpp - psi + dlog(dble(lgnie/nvb))
           argp = qfpp - psi
           tmp  =              (lgnie-dlog(dble(nvb)))
           argp = argp + tmp
           call dirac(argp, tmp, dtmp)
           POCCUP = dble(nvb)*tmp
         endif
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION NOCCUP(lboltz, qfnn, psi, lgnie, ncb)
      include 'p2conf.h'
c
c     calculate electron concentration. Trivial but modular.
c
c     Original: H.R.Yeager       Stanford University     July 1985
c
c     CALLS TO  : DIRAC
c     CALLED BY : 
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      logical lboltz 
      double precision qfnn,psi, argn,tmp,dtmp,dexpu,lgnie
      real             ncb
c------------------------------------------------------------------

      if (lboltz) then
c          NOCCUP=lgnie*dexpu(psi-qfnn)
           tmp   =(psi-qfnn)
           NOCCUP=dexpu(tmp+lgnie)
         else
c          argn = psi - qfnn + dlog(dble(lgnie/ncb))
           argn = psi - qfnn
           tmp  =              (lgnie-dlog(dble(ncb)))
           argn = argn + tmp
           call dirac(argn, tmp, dtmp)
           NOCCUP = dble(ncb)*tmp
         endif
      return
      end
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION QFPP(lboltz, fpp, psi, lgnie, nvb,Nmaj)
      include 'p2conf.h'
c
c     calculate hole quasi-fermi level from hole concentration. 
c     Trivial but modular.
c
c     Original: H.R.Yeager       Stanford University     July 1985
c
c     CALLS TO  : DIRAC
c     CALLED BY : 
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      include     'blank.h'
c------------------------------------------------------------------
      
      logical lboltz 
      double precision fpp,psi,  argp,tmp,dtmp, tmp2, tmp3,lgnie, Nmaj
      real             nvb
c------------------------------------------------------------------
      
c..   If we cannot resolve the hole concentration, assume that pn = ni*ni
c..   this does make some amount of sense.
        
      if (dabs(fpp).LT.mindbl*10.0d0) then
        if (dabs(Nmaj).GT.dabs(fpp)) then 
           tmp2 =       (lgnie - dlog(Nmaj))
           QFPP = psi + tmp2
        else
c..        assume p=n=ni, both p and n are fall below the machine precision.
           QFPP = psi 
        endif
        return
      endif
      
      
      if (lboltz) then
        tmp2 =       (dlog(dabs(fpp)+mindbl)-lgnie)
        QFPP = psi + tmp2
      else
        argp = fpp/dble(nvb)
        call diraci(argp, tmp, dtmp)

        tmp2 =           - (lgnie-dlog(dble(nvb)))
        tmp3 = tmp + psi 
        QFPP = tmp2 + tmp3
      endif
      return
      end

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION QFNN(lboltz, fnn, psi, lgnie, ncb,Pmaj)
      include 'p2conf.h'
c
c     calculate electron concentration. Trivial but modular.
c
c     Original: H.R.Yeager       Stanford University     July 1985
c
c     CALLS TO  : DIRAC
c     CALLED BY : 
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      
      include     'blank.h'
c------------------------------------------------------------------

      logical lboltz 
      double precision fnn,psi, argn,tmp,dtmp, tmp2,tmp3,lgnie,Pmaj
      real             ncb
c------------------------------------------------------------------

c..   If we cannot resolve the hole concentration, assume that pn = ni*ni
c..   this does make some amount of sense.
      
      if (dabs(fnn).LT.(mindbl)*10.0d0) then
        if (dabs(Pmaj).GT.dabs(fnn)) then 
           tmp2=     - (lgnie-dlog(Pmaj))
           QFNN= psi + tmp2
        else
c..        assume n = p = ni, both n and p are out of range.
           QFNN= psi
        endif
        return 
      endif
     
      if (lboltz) then
c          QFNN= psi -  dlog(fnn/lgnie)
           tmp2=     - (dlog(dabs(fnn)+mindbl)-lgnie)
           QFNN= psi + tmp2
         else
           argn = fnn/dble(ncb)
           call diraci(argn,tmp,dtmp)
c          QFNN = psi - tmp + dlog(dble(lgnie/ncb))
           tmp2 = psi - tmp 
           tmp3 =           + (lgnie-dlog(dble(ncb)))
           QFNN = tmp2 + tmp3
         endif
      return
      end
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      DOUBLE PRECISION FUNCTION DEXPU(arg)
      include 'p2conf.h'
c
c     Prevent underflow on large negative arguments to exponential.
c
c     Original  MRP     
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      double precision arg, maxexp
c------------------------------------------------------------------
      maxexp = 80.0d0
c
      if(arg.gt.-maxexp) then
         dexpu=dexp(arg)
      else
         dexpu=0.0d0
      endif
      return
      end
