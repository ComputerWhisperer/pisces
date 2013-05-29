cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Wed Mar 28 17:16:59 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ICP1(neq,esiz,ia,il,ja,a,aw,l,di,x)
      include 'p2conf.h'
c
c       Calculate incomplete cholesky factorisation of "a" matrix.
c
c     Restrictions : 
c      a must be Stieljes! 
c      pivots are taken from the main diagonal.
c
c     Input :
c       neq         = number of equations
c       esiz        = size of a
c       a           = numeric values 
c
c     Output :
c       l           = numeric values of l, stored in regular a form
c       di          = inverse pivot values.
c
c     Temporary :
c      (ia,ja)      = storage for expanded sparsity pattern
c      il           = storage for partial l sparsity
c      aw           = working copy of a
c
c
c     Original :        CSR Jan 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c...............local types
      integer neq
      integer ia(1),il(1)
      INTeger ja(1)
      double precision a(1),aw(1),l(1),x(1),di(1)
      integer ci,rj,ilo,esiz,clo,chi,i,alo,ahi,llo,lhi,ill,iaa,
     +        rx,ri,iex,cji,rxj,jex,itry
      double precision eps,xd,alpha,delta(8),lrx
      data eps/1e-8/,delta/0.0d0,1d-6,5d-6,1d-5,1d-4,1d-3,1d-2,1d-1/
c
c******************** Start ********************************************
c
c...............Return point if factorisation fails
      itry=0
 1111 continue
      itry = itry+1
      alpha = 1.d0 + delta(itry)


c...............Initialize :
c                   il = start of jth column of l, one after the pivot
c               3) copy a into aw
c...............

      do 12 i=1,esiz
         l(i)=0.d0
   12    continue

      do 20 ci=1,neq
         clo=ia(ci)
         chi=ia(ci+1)-1
         do 21 rx=clo,chi
            ri=ja(rx)
            if (ri.eq.ci) then
               il(ci)=rx+1
               goto 20
            endif
   21    continue
   20 continue
      
      do 30 ci=1,neq
       clo=ia(ci)
       chi=ia(ci+1)-1
       do 30 rx=clo,chi
             ri=ja(rx)
             if (ri.ne.ci) then
          aw(rx)=a(rx)
             else if (itry.eq.1) then
          aw(rx)=a(rx)
             else
          aw(rx)=a(rx)*alpha
             endif
   30 continue

c...............Do the incomplete cholesky column by column.
c     call timer(1,6,' ',elap)
      do 1000 ci=1,neq

c...............pointers into columns 
         alo=ia(ci)
         ahi=ia(ci+1)-1
         llo=il(ci)
         lhi=ahi

         ill=llo-1
         iaa=alo-1
c
c...............get the pivot ; warning here
         xd = aw(llo-1)
         if (xd.lt.eps) then
             call erset(-245,0,ci)
           itry = itry+1
           if (itry.lt.8) goto 1111
        call erset(246,0,8)
        return
         endif
         xd = 1.0d0/xd
         di(ci)=xd

c...............walk down the column
         do 500 rx=llo,lhi
            ri = ja(rx)
            lrx = aw(rx)*xd
            l(rx) = lrx

            ilo=il(ri)-1
            iex=ilo
            cji=ja(ilo)

            do 550 rxj=rx,lhi
               rj = ja(rxj)
               if (rj.eq.cji) then
                  aw(iex) = aw(iex) - lrx*aw(rxj)
                  iex=iex+1
                  cji=ja(iex)
               else
                  aw(ilo) = aw(ilo) - lrx*aw(rxj)
                  jex=il(rj)-1
                  aw(jex) = aw(jex) - lrx*aw(rxj)
               endif
  550       continue

  500    continue

 1000 continue

     
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MXV(neq,ia,ja,a,v,res)
      include 'p2conf.h'
c
c       Multiply sparse matrix a by vector v giving res
c     Input :
c       neq     = number of equations
c       ia      = expanded column table
c       ja      = expanded row array
c       v       = vector to multiply
c     Output :
c       res     = result
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c...............local types
      integer neq
      integer ia(1)
      integer ja(1)
      double precision a(1)
      double precision v(1),res(1),vci
      integer ri,ci,aix,clo,chi,rx
c
c******************** Start ********************************************
c
      do 100 ri=1,neq
  100    res(ri)=0.d0

      aix=0
      do 200 ci=1,neq
         clo=ia(ci)
         chi=ia(ci+1)-1
         vci=v(ci)
         do 200 rx=clo,chi
            aix=aix+1
            ri=ja(rx)
  200       res(ri)=res(ri)+vci*a(aix)

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE INVLLT(neq,ia,il,ja,l,r,x,di)
      include 'p2conf.h'
c
c     Solve l*d*lt x = r
c   Input :
c       neq     = number of equations
c       ia,ja   = sparsity pattern of a
c       il      = start of l columns
c       l       = incomplete factorisation
c       r       = right hand side
c       di      = inverse pivot values
c   Output:
c       x       = solution
c
c   Original : CSR Jan 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......local types
      integer neq
      integer ia(1)
      integer ci,ri,il(1)
      integer ja(1)
      double precision l(1)
      double precision r(1),x(1),di(1),aux
      integer aix,clo,chi,rx,cx
c
c******************** Start ********************************************
c
      do 100 ci=1,neq
  100    x(ci)=r(ci)

c......Forward elimination; scan columns

      do 500 ci=1,neq
         aux=x(ci)
         clo=il(ci)
         chi=ia(ci+1)-1
         aix=clo-1
c...............Subtract  aux*remainder of column from x
         do 500 rx=clo,chi
            ri=ja(rx)
            aix=aix+1
            x(ri)=x(ri)-l(aix)*aux
  500 continue


c.............Back substitution 
c.................r := d**-1 r
      do 750 ci=1,neq
         x(ci) = x(ci) * di(ci)
  750 continue

c.................r := lt**-1 r
c.................The neat trick here is that the column ordered
c.................l is also the row ordered lt.
      do 1000 ri=neq,1,-1
         clo=il(ri)
         chi=ia(ri+1)-1
         aix=clo-1

         do 1000 cx=clo,chi
            ci=ja(cx)
            aix=aix+1
            x(ri)=x(ri)-l(aix)*x(ci)
 1000 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ICP2(neq2,ia,il,ja,a,l,di,rhs,
     +           r,p,ap,qinvr,aqinvr,ascl,mxloop,litex)
      include 'p2conf.h'
c
c       Conjugate gradient iteration.
c       Preconditioning is matrix generated in icp1.
c       Reference : H.C.Elman "Preconditioned conjugate-gradient 
c       methods for nonsymmetric systems of linear equations"
c       Yale University Math. Dept. Research Report 203, April 1981.
c
c     Input :
c       neq2     = number of equations
c       ia      = column table of expanded matrix
c       il      = column table of l
c       ja      = row indices for a/l
c       a       = the dreaded "a" matrix
c       l       = lower cholesky approximate factor
c       di      = inverse pivot values
c       rhs     = right hand side, scaled down by ascl
c       ascl    = original max of rhs
c       mxloop  = max cg loops
c    Output :
c       rhs     = solution vector
c       litex   = flag to indicate max iterations exceeded
c    Temporary :
c       r       = residual
c       p       = current direction vector
c       ap      = matrix a * p 
c       qinvr   = q**-1 residual (where q is the conditioning matrix)
c       aqinvr  = a * qinvr
c
c     Original : CSR Jan 84       
c     Modified : MRP Apr 85     (Max iterations)
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c......control common
      include     'sol.h'
      include     'blank.h'
      include     'logunt.h'
      include     'setup.h'
c------------------------------------------------------------------

c......local types
      integer neq2
      integer ia(1),il(1)
      integer ja(1)
      double precision a(1),l(1),ascl
      double precision rhs(1),di(1)
      double precision r(1),p(1),alpha,beta
      double precision ap(1),qinvr(1),aqinvr(1)
      double precision normap,rap,norm2,numer,ri,api,outer2,nratio
      real   tol2,tol0,norm0,t1
      integer i,itc,mxloop
      logical litex

c******************** Start ********************************************
c
c......Initialize 'old' vectors
c       1. Initial guess (zero for a newton method)
c       2. Initial residual (r) and norm of r.
c       3. Initial direction vector(p)
c       4. Initial a*p
c      call timer(1,6,' ',elap)

      norm2 = 0.d0
      do 100 i=1,neq2
         ri=rhs(i)
         norm2 = norm2 + ri*ri
         r(i)=ri
         p(i)=ri
         rhs(i)=0.d0
  100 continue
      norm2=sqrt(norm2)
      
      call mxv(neq2,ia,ja,a,p,ap)

c.......Calculate tolerance.
c       Must be careful to calculate ratio using outer, 
c.......not inner norms.

      outer2 = ascl*norm2
      if (res00.eq.0.d0 .or. res00.lt.outer2) then
        tol2 = lu1cri * norm2
      else
        nratio = outer2/res00
        tol2 = dmin1(lu1cri, lu2cri * nratio) * norm2
      endif

c........Save new outer norm
      res00 = outer2

      if (ldebug) write(lutty,1401) 0,dlog10(outer2)


      tol0 = 0.1*ptolg/(-decoef)/ascl

      

c*************************************************
c                                                *
c           conjugate gradient loop              *
c                                                *
c*************************************************
      itc=0
 1000 continue
      
c
c...........Compute alpha factor
c
      normap=0.0d0
      rap=0.0d0
      do 1200 i=1,neq2
         api=ap(i)
         normap = normap + api*api
         rap = rap + api*r(i)
 1200 continue
         

cMRP
      if(normap.eq.0.d0) goto 5000
cMRP
      alpha = rap / normap
c
c............Update solution and residual vectors
c
      do 1300 i=1,neq2
         rhs(i) = rhs(i) + alpha*p(i)
         r(i) = r(i) - alpha*ap(i)
 1300 continue

c
c............Check out residual
c
      norm2=0.0d0
      norm0=0.0d0
      do 1400 i=1,neq2
       t1=abs(r(i))
       if (t1.gt.norm0) norm0=t1
 1400    norm2 = norm2 + t1*t1
      norm2 = dsqrt(norm2)
      
      if (ldebug) write(lutty,1401) itc,dlog10(norm2*ascl)

      litex=itc+1.eq.mxloop
      if (norm2.lt.tol2 .or. norm0.lt.tol0 .or. litex) goto 5000

c
c............Set up next iteration and loop
c
      call invllt(neq2,ia,il,ja,l,r,qinvr,di)
      call mxv(neq2,ia,ja,a,qinvr,aqinvr)

      numer = 0.0d0
      do 1500 i=1,neq2
 1500    numer = numer + aqinvr(i)*ap(i)

      beta = - numer / normap

      do 1600 i=1,neq2
            p(i) =  qinvr(i) + beta*p(i)
            ap(i)= aqinvr(i) + beta*ap(i)
 1600    continue                

      itc=itc+1
      goto 1000

c*************************************************
c                                                *
c             Loop exit                          *
c                                                *
c*************************************************

 5000 continue

      ninner = itc+1

      return

 1401 format('log(error) at iteration',i3,' is ',1pe12.5) 
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ASCALE(ldir,rhs,asfac,neq)
      include 'p2conf.h'
c
c Scale the right hand side to avoid x**2 problems.
c ldir=.true.  means scale
c ldir=.false. means unscale
c
c Input :
c       rhs     - right hand side
c       neq     - no. of equations
c
c Output :
c       rhs     - scaled (largest element=1)
c       asfac   - largest element before scaling
c
c Original : CSR March 84
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      double precision rhs(1),maxr,dr,asfac
      integer i,neq
      logical ldir
c
c********************* Start *******************************************
c
c......Scale
      if (.not.ldir) goto 500
      maxr=0.0d0
      do 100 i=1,neq
         dr=dabs(rhs(i))
       if (dr.gt.maxr) maxr=dr
  100 continue

      asfac=maxr

      maxr=1.0d0/maxr
      do 200  i=1,neq
  200    rhs(i)=rhs(i)*maxr

      return

c......Unscale
  500 continue

      do 550 i=1,neq
       rhs(i)=rhs(i)*asfac
  550 continue
      
      return
      end

