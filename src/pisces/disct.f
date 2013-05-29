cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:24:14 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE DISCT(nvar)
      include 'p2conf.h'
c 
c     orders equations using method based on george's nested
c     dissections as modifed by i. s. duff, a.m. erisman &
c     j  k. reid, siam j. num. anal., vol.13, no. 5, pp. 686-695, 
c     oct. 1976., (but with m*n instead of n*n matrix). 
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'logunt.h'
      include     'emaco.h'
c****************************************************************** 
c 
c                   type declarations 
c 
      integer ipm(100),ipn(100),istore(100),ictm(19),ictn(19),ic(19), 
     +        nvar,ityp,n,ln,m,lmm,lmax,i,j,k,l,it,itb,ll,nbcond,ivar,
     +        nn,ihi,ilo,iref,nb2,nb1
c 
c****************************************************************** 
c 
c                   compute pi vectors
c 
      n=nx
      call pi(n,ipn,ln,ictn)
      m=ny
      call pi(m,ipm,lmm,ictm)
c          find number of points ic(i) in each set i. 
      lmax=max0(ln,lmm)
      do 50 i=1,lmax
   50 ic(i)=0 
      do 100 i=1,lmm 
      do 100 j=1,ln 
      k=max0(i,j) 
  100 ic(k)=ic(k)+ictm(i)*ictn(j) 
c          correct ic(i) for boundary points
      do 200 l=1,nb 
      k=nbc(l)
      i=mod(k-1,m)+1
      j=(k-1)/m+1 
      ityp=max0(ipm(i),ipn(j)) 
  200 ic(ityp)=ic(ityp)-1 
c          order according to sets omitting boundary points 
c          first set up starting values for sets
      it=ic(1)
      ic(1)=2*nb
      do 300 i=2,lmax 
      itb=it
      it=ic(i)
  300 ic(i)=itb+ic(i-1) 
      ll=1
      nbcond=2*nb*nvar
      write(luout,1000) 
 1000 format('1dissection pattern:'//)
      do 400 j=1,n
      do 395 i=1,m
      ityp=max0(ipm(i),ipn(j)) 
      ivar=(j-1)*m+i
      if (ivar.ne.nbc(ll)) go to 360
      ll=ll+1 
c          istore(1) is used for temporary storage
      istore(i)=0 
      go to 390 
  360 do 380 nn=1,nvar
  380 ipc(ic(ityp)*nvar+nn)=(ivar-1)*nvar+nn 
      istore(i)=ityp 
      ic(ityp)=ic(ityp)+1 
  390 continue
  395 continue
      write(luout,1010) (istore(i),i=1,m) 
 1010 format(' ',65i2)
  400 continue
      ihi=m*n+nb
      ilo=2*nb+1
      do 500 i=ilo,ihi
      do 495 nn=1,nvar
      iref=(i-1)*nvar+nn
  495 ipri(ipc(iref))=iref
  500 continue
c 
      nb2=nb*2
      nb1=nb+1
      write(luout,1100) nb1,nb2 
 1100 format('1variables ',i4,' - ',i4,' are the currents.' 
     &/' order of elimination of nodes is as follows.'/ 
     &' each is a set of 3 in the simultaneous solution.'//)
      ll=1
      do 600 j=1,n
      do 590 i=1,m
      iref=(j-1)*m+i
      ipm(i)=ipri(iref*nvar)/nvar 
      if (iref.ne.nbc(ll)) go to 590
      ll=ll+1 
      ipm(i)=ipm(i)-nb
  590 continue
  600 write(luout,1110) (ipm(i),i=1,m)
 1110 format(' ',33i4)
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE PI(n,ipn,l,ict)
c 
c          computes pi vector in ipn, value of l such that
c          2**l.ge.(n-1), and ict(i) gives number of times i appears
c          im ipn.  note: the n here corresponds to n+1 in the paper. 
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   type declarations 
c 
      logical is
c 
      integer ipn(100),ict(100),n,l ,n1,k,i,m,ilo,ihi,isum
      real ctr
c 
c****************************************************************** 
c 
c                   find l
      n1=n-1
      k=1 
      l=0 
    5 k=k*2 
      l=l+1 
      if (k.lt.n1) go to 5
c          initialize 
      ipn(1)=1
      ipn(n)=1
      do 10 i=2,n1
   10 ipn(i)=0
c          scan for zeros 
      m=l 
      ctr=float(n1)*.5
   12 is=.false.
      ict(m)=0
      i=1 
   13 i=i+1 
      if (i.gt.n) go to 40
      if (is) go to 20
      if (ipn(i).ne.0) go to 13 
      is=.true. 
      ilo=i 
      go to 13
   20 if (ipn(i).eq.0) go to 13 
      is=.false.
      ihi=i-1 
      isum=ilo+ihi
      k=isum/2
      if (k*2.eq.isum) go to 30 
      if (float(k).lt.ctr) k=k+1
   30 ipn(k)=m
      ict(m)=ict(m)+1 
      go to 13
   40 m=m-1 
      if (m.ge.1) go to 12
      ict(1)=ict(1)+2 
c 
      return
      end 
