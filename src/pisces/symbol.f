cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SYMGEN
      include 'p2conf.h'
c
c     Symbolic main program for triangular mesh f.e. analysis
c     this uses vsort, vmsp, zempty, zwrit2, unblk, fini,
c     zemptz, vsrt4 from VEGES.  VEGES is desribed in:
c     D. A. Calahan et al. "VEctorized GEneral Sparsity algorithms
c     with backing store," sel report no. 96, Systems Engineering
c     Laboratory, the University of Michigan, Ann Arbor, Jan. 1977.
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP            Stanford University        Nov, 1983
c     Addition : CSR (Minimum degree routine)              Mar, 1984
c     Revision : MRP (No bc rows/cols)                     Mar, 1984
c
c     Copyright c 1981 The board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'stat.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'symtmp1.h'
      integer TMPPAD(1187006)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   type declarations
c
      logical lmindg,ldisct,linfil,loutfl,lprint,lrdwrt,lstrip
      integer ipart(2),ixp(2),ict,nvar,nnode,adimx,ludimx
      character*20 infil,outfil
c
c                   data
c
      data nvar/1/,nnode/3/
c
c******************************************************************
c
c...If no mesh, bag it!
      if(lmshdn) goto 5
      call erset(258,linum,0)
      return
c
c...Check symbolic factorization card
5     call symck(infil,outfil,lmindg,ldisct,lprint,loutfl,linfil,
     +           lstrip)
      if(errflg) return
c
c...Check if file input mode
      if(.not.linfil) goto 100
      lrdwrt=.true.
      call srdwr(infil,lrdwrt)
      if(errflg) return
      goto 200
c-------------------------------------------------------------
c
c                   must do factorization
c                   determine pivots
c
100   nvar=nmult
      neq0=np*nvar
      neq=neq0+nlump
      neqp1=neq+1
      call pivot(nvar)
c
c...Set up element and bound. node entries to unblk
      call smblc(ict)
c
c...Set up for unblock
      ipart(1)=1
      ipart(2)=neqp1
      ixp(1)=1
      ixp(2)=ict+1
c
c...Set up pointers to "a"-matrix elements
      call unblk(nvar,nnode,ipart,ixp,lstrip)
c
c
c..........Re-order pivots if requested.
c          For rectangular grids, use nested dissection.
c..........For more general grids, use minimum degree algorithm
c..........(already done for full Newton)
 
      if(ldisct) call disct(nvar)
      if(lmindg) call mindg
c
c...Set limits on A,L,U (method specific)
      if(lgumm) then
         ludimx=ludimg
         adimx=adimg
      else 
         ludimx=ludim
         adimx=adim
      endif
c
c...Do symbolic factorization of a
      call vmsp(neq,ipc,ipri,ja,jl,ju,ia,il,iu,iva,ivl,ivu,
     +          ludimx,iudim,ildim,lsiz,usiz,ilsiz,iusiz,kp,ixt,ixb)
c
c...If lumped-element contacts, get addresses of pivots to save
c...time later on
      call lumpa
c
c-------------------------------------------------------------
c
c...Check array sizes against dimensions
200   if(iasiz.gt.iadim.or.ilsiz.gt.ildim.or.iusiz.gt.iudim.or.
     +       asiz.gt.adimx.or.lsiz.gt.ludimx.or.usiz.gt.ludimx.or.
     +       mapsiz.gt.mapdim) call erset(29,linum,0)
      if(errflg) lprint=.true.
c
c...Print if requested (or forced)
      if(lprint) call sprnt(linfil,adimx,ludimx,lmindg)
c
c...Check if file output mode (or errors)
      if(.not.loutfl.or.errflg) goto 999
      lrdwrt=.false.
      call srdwr(outfil,lrdwrt)
c
c...All done
999   lsymdn=.true.
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE PIVOT(nvar)
      include 'p2conf.h'
c
c     Sets up column pivot vector in ipc and inverse row pivot vector
c     in ipri.  Original matrix order u, ib.  This sets row order
c     ub,ib,ui.
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP            Stanford University        Nov, 1983
c     Revision : MRP    (no bc rows/cols)                  Mar, 1984
c     Revision : MRP    (full Newton min. degree)          Mar, 1984
c     Revision : MRP    (lumped elements)                  Dec, 1984
c
c     Copyright c 1981 The board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  common areas
c
      include     'blank.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c  type declarations
c
      integer ick,i,j,ii,nvar
c------------------------------------------------------------------
c
c...Calculate pivot column no.'s (ipc) and inverse of pivot
c...rows (inverse ipr = ipri)  
      ick=0
      ii=0
      do 100 i=1,np
      do 90 j=1,nvar
      ii=ii+1
      ick=ick+1
c      ipr(ick)=ii
      ipri(ii)=ick
      ipc(ick)=ii
90    continue
100   continue
c
c...Extra equations for lumped element bc's
      do 200 i=1,nlump
      ii=ii+1
      ick=ick+1
c      ipr(ick)=ii
      ipri(ii)=ick
      ipc(ick)=ii
200   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SMBLC(ict)
      include 'p2conf.h'
c
c     Sets up array na(i) for unblk.
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP            Stanford University        Nov, 1983
c
c     Copyright c 1981 The board of trustees of the Leland Stanford
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University.
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  common areas
c
      include     'blank.h'
      include     'setup.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'symtmp1.h'
      integer TMPPAD(1187006)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c  type declarations
c
      logical lnxtdn
      integer ict,ielec,k,j,ipt,ib,irow,iel,iadjt,ipt2
c
c******************************************************************
c
c                   start
c
      ict=0
      lnxtdn=.false.
c
c---------------
c  Regular eqs
c---------------
c
c...Insert finite elements in na
      do 35 iel=1,ne
      do 33 k=1,3
      ict=ict+1
   33 na(ict)=nop(k,iel)
   35 continue
c
c-----------------------------
c  Lumped element boundaries
c-----------------------------
c
c...Must set up equations for applied voltage at each boundary
c...with a lumped element connection
      do 100 ib=1,nelect
      if(.not.(lresis(ib).or.lcurbc(ib))) goto 100
      irow=nresis(ib)+neq0
      nresis(ib)=irow
c
c...If we havent gotten adjacency info, get it now
      if(.not.lnxtdn) then
         lnxtdn=.true.
         call nxtel(p2t,p2tc)
      endif
c
c...Diagonal
      ict=ict+1
      na(ict)=-irow
      ict=ict+1
      na(ict)=-irow
c
c...Search boundary nodes for nodes belonging to this contact
      do 110 j=1,nb
      ipt=nbc(j)
      ielec=lm(ipt)
      if(ielec.ne.ib) goto 110
c
c...Set up boundary node
      call symnod(ict,irow,ipt)
      ict=ict+1
      na(ict)=-irow
      ict=ict+1
      na(ict)=-(ipt*nmult-nmult+1)
c
c...Set up all adjacent node terms
      do 120 iadjt=1,p2tc(ipt)
      iel=p2t(iadjt,ipt)
      do 120 k=1,3
      ipt2=nop(k,iel)
      if(ipt2.ne.ipt) call symnod(ict,irow,ipt2)
120   continue
c
c...Try next boundary node
110   continue
c
c...Next lumped-element contact
100   continue
c
c-------------
c  Finish up
c-------------
c
c...Additional na to avoid undefined value in unblk
      nasiz=ict+1
      na(nasiz)=0
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SYMNOD(ict,irow,ipt)
      include 'p2conf.h'
c
c     Set up a matrix positions for a node ipt in row irow.
c
c     Original : MRP   Dec. 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'symtmp1.h'
      integer TMPPAD(1187006)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer ict,irow,ipt,icol,i
c------------------------------------------------------------------
c
      icol=nmult*ipt-nmult
      do 130 i=1,nmult
         icol=icol+1
         ict=ict+1
         na(ict)=-icol
         ict=ict+1
         na(ict)=-irow
130   continue
c
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FINI(ie,ipp,nvar,nnode,kt,lstrip)
      include 'p2conf.h'
c
c   Subroutine to generate weighted vectors and lengths given finite 
c   element node numbers.
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common areas
c
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'symtmp1.h'
      integer TMPPAD(1187006)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   type declarations
c
      logical lstrip,l90(3)
      integer ipp,nvar,nnode,kt,nsave,j,n1,m,n1save,nxsave,l,n2,lsave
      integer idx1,idx2,ie,ie2,ni(5),iside
      data ni/1,2,3,1,2/
c
c******************************************************************
c
c                   start
c



      if(.not.lstrip) goto 10
      do 8 iside=1,nnode
      l90(iside)=ehed(iside,ie).eq.0.
      if(.not.l90(iside)) goto 8
      idx1=nop(ni(iside+1),ie)
      idx2=nop(ni(iside+2),ie)
      ie2=nextel(iside,ie)
      if(ie2.le.0) goto 8
      l=1
7     m=nop(l,ie2)
      if((m.ne.idx1).and.(m.ne.idx2)) goto 9
      l=l+1
      if(l.lt.nnode) goto 7
9     l90(iside)=ehed(l,ie2).eq.0.
8     continue
10    continue




      kt= kt-1
      nsave= kt
      do 5 j= 1,nnode
         n1= na(ipp+j-1)-1
         do 5 m= 1,nvar
            n1save= n1*nvar+m
            nxsave= -1
            do 4 l= 1,nnode

c
c...Check if coupling should be stripped
               if((.not.lstrip).or.(j.eq.l)) goto 15
               if(l90(6-(l+j))) goto 4
15             continue

               n2= na(ipp+l-1)-1
               nx= n2*nvar
               if(kt.eq.nsave) goto 2
               if(nx.eq.nxsave) goto 3
    2          kt= kt+1
               fip(kt)= n1save*neqp1+nx+1
               lsave= 0
    3          nxsave= nx+nvar
               lsave= lsave+nvar
               len(kt)= lsave
    4          continue
    5       continue
      kt= kt+1
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SRDWR(namfil,lrdwrt) 
      include 'p2conf.h'
c 
c     Reads or writes symbolic files.
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP            Stanford University        Nov, 1983
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common area 
c 
      include     'blank.h'
      include     'logunt.h'
      include     'symb.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lrdwrt,lnewtx
      integer lu,ncarrx,nmultx,npx,i,ierr
      character*20 namfil
      character*20 itime
c 
c****************************************************************** 
c 
c                   read or write? (true=read)
c 
      if(lrdwrt) goto 500
c-------------------------------------------------------------
c
c  Write
c
      lu=lutmp
      call fopcl(11,namfil,20,lu,.false.,ierr) 
      if(errflg) return
      rewind(lu)
      write(lu) neq,neqp1,ncarr,nmult,lnewt
      write(lu) iasiz,ilsiz,iusiz,asiz,lsiz,usiz,nasiz 
      write(lu) (ia(i),i=1,iasiz)
      write(lu) (ja(i),jl(i),ju(i),iva(i),ivl(i),ivu(i),i=1,neqp1)
      write(lu) (il(i),i=1,ilsiz)
      write(lu) (iu(i),i=1,iusiz)
      write(lu) (ipc(i),ipri(i),i=1,neq)
      write(lu) idaytm 
c
c...Done. close the file! 
      call fopcl(0,namfil,20,lu,.false.,ierr) 
c
c...Write out the symbolic file date code 
      write(luout,3100) namfil
3100  format(/'Symbolic factorization written to ',a20)
c
c...Bye!!!
      return
c-------------------------------------------------------------
c 
c  Read
c
500   lu=lutmp
      call fopcl(1,namfil,20,lu,.false.,ierr)
      if(errflg) return
      rewind(lu)
      read(lu) neq,neqp1,ncarrx,nmultx,lnewtx
      read(lu) iasiz,ilsiz,iusiz,asiz,lsiz,usiz,nasiz
      npx=neq/nmult
c
c...Check for consistency
      if((ncarrx.ne.ncarr).or.(nmultx.ne.nmult).or.(npx.ne.np)) then
         call erset(290,linum,0)
         return
      endif
      read(lu) (ia(i),i=1,iasiz)
      read(lu) (ja(i),jl(i),ju(i),iva(i),ivl(i),ivu(i),i=1,neqp1)
      read(lu) (il(i),i=1,ilsiz)
      read(lu) (iu(i),i=1,iusiz)
      read(lu) (ipc(i),ipri(i),i=1,neq)
      read(lu) itime 
c
c...Done. close the file! 
      call fopcl(0,namfil,20,lu,.false.,ierr)
c
c...Write out the symbolic file date code 
      write(luout,3000) itime 
3000  format(/'Symbolic factorization date code = ',a20)
c
c...Bye!!!
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SPRNT(linfil,adimx,ludimx,lmindg)
      include 'p2conf.h'
c 
c     This routine prints the symbolic factorization results to the 
c     output device. 
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP            Stanford University        Nov, 1983
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'symb.h'
      include     'logunt.h'
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical linfil,lmindg
      integer neqdim,neqp1d,neq1o2,nq1o2d,adimx,ludimx,neqt,neqtp1
c 
c****************************************************************** 
c 
c                   start 
c 
      neqdim=npdim+nbdim
      neqp1d=neqdim+1 
      neqt=neq
      neqtp1=neqt+1
      neq1o2=neqtp1/2
      nq1o2d=neqp1d/2 
c 
c...Print header
      write(luout,1000) 
1000  format(//'Symbolic factorization results'//) 
c 
c...Print node, element, and electrode limits 
      write(luout,1001) np,npdim,ne,nedim,nb,nbdim
1001  format(' # of nodes          =',i5,',  maximum=',i5,
     +       ',  parameter=MAXPT',/
     +       ' # of elements       =',i5,',  maximum=',i5,
     +       ',  parameter=MAXTRI',/
     +       ' # of electrode nodes=',i5,',  maximum=',i5,
     +       ',  parameter=MAXCON',///)
c 
c...Print out mesh info array sizes and dimensions
      write(luout,1002) np,npdim,ne,nedim,nb,nbdim
1002  format(' Arrays                     size     dimensions',
     +       '      parameter',//
     +       ' cord,r1,itype,mob,tconc,',/
     +       ' fp,fn,fv,ofn,ofv        ',i6,6x,i6,12x,'MAXPT'/ 
     +       ' nop,ehed,jhjd,es,imat   ',i6,6x,i6,12x,'MAXTRI'/ 
     +       ' nbc,ietype              ',i6,6x,i6,12x,'MAXCON')
c 
c...Print out symbolic array sizes and dimensions 
      write(luout,1003) neqt,ipcdim,neqtp1,ipcdim+1,
     +      neq1o2,ixbdim,neqt,3*npdim,iasiz,iadim,ilsiz,ildim,
     +      iusiz,iudim,nasiz,nadim,mapsiz,mapdim
1003  format(' ipc,ipri,ixt,           ',i6,6x,i6,12x,'MAXEQN'/ 
     +       ' ja,jl,ju,iva,ivl,ivu,jp1',i6,6x,i6,12x,'MAXEQN+1'/ 
     +       ' ixb,kp                  ',i6,6x,i6,12x,'MAXEQN'/ 
     +       ' rhs,di,x                ',i6,6x,i6,12x,'MAXEQN'/ 
     +       ' ia                      ',i6,6x,i6,12x,'MAXIA'/ 
     +       ' il                      ',i6,6x,i6,12x,'MAXILU'/ 
     +       ' iu                      ',i6,6x,i6,12x,'MAXILU'/ 
     +       ' na                      ',i6,6x,i6,12x,'MAXIA'/
     +       ' len,fip,jp              ',i6,6x,i6,12x,'MAXMAP')
c 
c...Min. degree?
      if(lmindg) write(luout,1033) iysiz,iydim
1033  format(' tv,tl                   ',i6,6x,i6,12x,'MAXADJ+1')
c 
c...Print out numeric array sizes and dimensions
      write(luout,1004) asiz,adimx,lsiz,ludimx,usiz,ludimx
1004  format(' a                      ',i7,5x,i7,12x,'MAXADJ/MAXAG'/ 
     +       ' l                      ',i7,5x,i7,12x,'MAXLU/MAXLUG'/
     +       ' u                      ',i7,5x,i7,12x,'MAXLU/MAXLUG')
c 
c...Done
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE UNBLK(nvar,nnode,ibreak,ixp,lstrip)
      include 'p2conf.h'
c 
c 
c   Subroutine to perform block preprocessing, generating a 
c   vectorized column ordered symbolic matrix from randomly 
c   entered finite elements, matrix blocks, column vectors, 
c   and scalars.
c 
c   input:
c    n         - number of columns in the matrix
c    na(*)     - for finite element, list of element node numbers.
c              - for block, zero followed by block type (1-full,
c                2-diagonal, 3-tridiagonal) and i,j block extremes. 
c              - for column vector, negative column index, starting 
c                and ending row indices.
c              - for scalar, neg col and row index. 
c              - tail of na contains boundary condition variable
c                numbers which can be used for a row ordered
c                matrix to zero this row except for a 1 on the
c                diagonal.
c    ixp(nbrks+1) 
c              - partition pointer into na
c    nbcqq     - number of boundary condition variables specified 
c                at tail of na (for different boundary condition
c                scheme or normal column ordered matrix set nbc=0)
c    nbrks     - number of partitions in a matrix during numeric
c                formulation.  this breaking is totally independent 
c                of any breaking in the matrix factorization. 
c    ibreak(nbrks+1)
c              - starting columns for column breaks 
c    imunit    - logical i/o unit on which to write map 
c    fip(kt)    - scratch vector  
c    jp()      - scratch vector, dimension kt for vector map, 
c                for scalar map must be large enough to hold scalar 
c                map of largest partition.
c    jp1(n+1)  - scratch vector 
c    len(kt)   - scratch vector 
c    iva(n+1)  - scratch vector (not really, we use this as pointer into a) 
c     where *= ixp(nbrks+1)-1+nbc.
c 
c   output: 
c    kt        - number of map vectors
c    asiz      - dimension required for one a buffer
c    ja(n+1)   - column pointers into symbolic row descriptors in ia
c    ia(*)     - if negative, iabs is row index of single element.
c              - if positive, starting row for a vector and the 
c                next position of ia is ending row for this vector. 
c     where *= 2*number of vectors + number of scalars in the matrix. 
c 
c   Note: 
c 
c   In the non-partitioned version of the matrix solver, the a matrix 
c   is assumed to be in memory all at the same time.  Thus to 
c   partition the a matrix formulation is unnecessary. nbrks should 
c   be set to 1, ibreak(1) to 1, and ibreak(2) to n+1.
c 
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'adjtmp.h'
      include     'symtmp1.h'
      integer TMPPAD(1187006)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lstrip
      integer nvar,nnode,ibreak(2),ixp(2),nbcqq,nbrks,
     +  n,np1,kt,i,ipp,dnp1,ippnd,itx,ity,ilow,ihigh,
     +  jlow,jhigh,j,itop,ibot,k,isave,iax,ivax,ibcx,ii,ij,ivaxs,
     +  idiag,ipx,ipxnd,ilown,ihighn,tmp27,tmp28,ie
c 
c****************************************************************** 
c 
c                   data
c 
      data nbcqq/0/,nbrks/1/
c 
c****************************************************************** 
c 
c                   nvar= number of variables per node and
c                   nnode= number of nodes per finite element 
c 
      n=neq 
      np1= n+1
      dnp1=n+1
      kt= 1 
      ie=0
c 
c convert elements into weighted column vectors for sorting 
c 
c ip gets weighted vectors
c len gets vector lengths 
c 
      i=1 
      ipp= ixp(i) 
      ixp(i)= kt
      ippnd= ixp(i+1)-1 
100   if(ipp.gt.ippnd) goto 150
      itx= na(ipp)
      if(itx) 110,120,130
c 
c scalar or col vector specified
c 
110   ipp= ipp+1
      ity= na(ipp)
      if(ity.gt.0) goto 111
c 
c scalar
c 
      fip(kt)= -itx*dnp1-ity  
      len(kt)= 1
      goto 112
c 
c vector
c 
111   fip(kt)= -itx*dnp1+ity  
      ipp= ipp+1
      len(kt)= na(ipp)+itx+1
112   kt= kt+1
      if(kt.gt.mapdim) goto 998
         goto 140 
c 
c block specified 
c 
120   ipp= ipp+1
      itx= na(ipp)
      ipp= ipp+1
      ilow= na(ipp) 
      ipp= ipp+1
      jlow= na(ipp) 
      ipp= ipp+1
      ihigh= na(ipp) 
      ipp= ipp+1
      jhigh= na(ipp) 
      if(itx-2) 121,123,125 
c 
c full block
c 
121   do 122 j= jlow,jhigh 
         fip(kt)= j*dnp1+ilow 
         len(kt)= ihigh-ilow+1 
122      kt= kt+1 
      if(kt.gt.mapdim) goto 998
      goto 140
c 
c diagonal
c 
123   do 124 j= jlow,jhigh 
         itop= ilow+j-jlow
         fip(kt)= j*dnp1+itop 
         len(kt)= 1 
124      kt= kt+1 
      if(kt.gt.mapdim) goto 998
      goto 140
c 
c tridiagonal 
c 
125   do 126 j= jlow,jhigh 
         itop= ilow+j-jlow
         ibot= itop 
         if(j.ne.jlow) itop= itop-1
         if(j.ne.jhigh) ibot= ibot+1
         fip(kt)= j*dnp1+itop 
         len(kt)= ibot-itop+1 
126      kt= kt+1 
      if(kt.gt.mapdim) goto 998
      goto 140
c 
c finite element specified
c 
130   ie=ie+1
      call fini(ie,ipp,nvar,nnode,kt,lstrip)
         ipp= ipp+nnode-1 
140      ipp= ipp+1 
         goto 100 
150   ixp(nbrks+1)= kt
      kt= kt-1
      if(kt.gt.mapdim) goto 998
c 
c sort, "tagging" with identity vector for permutation
c 
      do 15 k= 1,kt 
15      jp(k)= k
      call vsrt4(kt,jp,fip)
c 
c set jp1 to column pointer into ip, ip to row index of vectors 
c 
      isave= 0
      do 20 k=1,kt
         i=fip(k)/dnp1
         fip(k)=mod(fip(k),dnp1)
         if(i.ne.isave) then
            isave=i 
            jp1(isave)=k
         endif
20    continue
      jp1(np1)=kt+1 
c 
c ip is starting row index for vectors
c jp is permutation into column ordering
c len is unpermuted vector lengths
c jp1 is column pointer into ip 
c 
c get ia, ja
c 
c ip set to point into single buffer
c 
      iax= 1
      ivax= 1 
  
      asiz = 0
      ibcx= 1 
      ippnd= ipp+nbcqq-1
      ii= ibreak(1) 
      ij= ibreak(2)-1 
      ivaxs= ivax 
c 
c  thru each column 
c 
      do 35 j= ii,ij
         ja(j)= iax 
         iva(j)= ivax 
         idiag= 0 
         ipx= jp1(j)
         ipxnd= jp1(j+1)-1
c                                 stop 27020 - bad ordering 
         if(ipx.gt.ipxnd) then 
            call erset(28,linum,27020)
            return
         endif
c                                 set up a current vector 
         ilow= fip(ipx) 
       tmp27 = jp(ipx)
         ihigh= ilow+len(tmp27)-1 
         fip(ipx)= ivax-ivaxs+1 
         ipx= ipx+1 
c                                 set up a "n"ew vector 
21          if(ipx.gt.ipxnd) goto 28 
            ilown= fip(ipx) 
          tmp28 = jp(ipx)
            ihighn= ilown+len(tmp28)-1
            if(ilown.gt.ihigh+1) goto 22 
c                                          vectors overlap
            fip(ipx)= ivax-ivaxs+1+ilown-ilow 
            ipx= ipx+1
            ihigh= max0(ihigh,ihighn) 
            goto 21 
c                               no overlap - store current element in ia
22          if(ilow.le.j.and.ihigh.ge.j) idiag=ivax-iva(j)+j-ilow+1 
            if(ilow.eq.ihigh) goto 23
c                                          vector 
         ia(iax)= ilow
         iax= iax+1 
         ia(iax)= ihigh 
         iax= iax+1 
         ivax= ivax+ihigh-ilow+1
         goto 24
c                                          scalar 
23       ia(iax)= -ilow 
            iax= iax+1
            ivax= ivax+1
c                                    new vector becomes current vector
24       ilow= ilown
         ihigh= ihighn
         fip(ipx)= ivax-ivaxs+1 
         ipx= ipx+1 
         goto 21
c                                   store last element this column
28       if(ilow.le.j.and.ihigh.ge.j) idiag=ivax-iva(j)+j-ilow+1 
         if(ilow.eq.ihigh) goto 29 
c                                                 vector
         ia(iax)= ilow
         iax= iax+1 
         ia(iax)= ihigh 
         iax= iax+1 
         ivax= ivax+ihigh-ilow+1
         goto 30
c                                                 scalar
29       ia(iax)= -ilow 
         iax= iax+1 
         ivax= ivax+1 
30       if(ipp.gt.ippnd.or.j.ne.na(ipp)) goto 35
c                       save diagonal position for later row zeroing
         na(ibcx)= na(ipp)
         ipp= ipp+1 
         ibcx= ibcx+1 
c              stop 27021 - row to be zeroed has no diagonal position 
         if(idiag.eq.0) then 
            call erset(28,linum,27021)
            return
         endif
         na(ibcx)= idiag
         ibcx= ibcx+1 
35       continue 
c                                   maximum dimension for a buffers 
      asiz= max0(asiz,ivax-iva(ii)) 
      ja(np1)= iax
      iasiz=iax 
      iva(np1)= ivax
c
c...Bye
998   mapsiz=kt
      return
      end 
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
      SUBROUTINE VSRT4(kt,jp,fip)
      include 'p2conf.h'
c
c                      Partition sorting algorithm
c           Reference : Collected algorithms of the ACM - 63,64,65
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
c**********************************************************************
c
c              type declarations
c
      integer isep,kt,fip(1),jp(1)
      integer ihigh(32),ilow(32),nsegs,il1,ih,isepx,ixh,ixl,it
c
c
c****************************************************************** 
c
c     initialize
      nsegs= 1
      il1= 1
      ih= kt
c     if no elements in this segment do nothing 
10    if(il1.ge.ih) goto 70
c     choose isep (separation entry): 
c     make fip(il1) <= fip((il1+ih)/2) <= fip(ih) by interchange
20    isepx= (ih+il1)/2 
      ixl=il1 
      ixh=ih
      call order(fip(il1),fip(isepx),fip(ih), 
     +            jp(il1),jp(isepx),jp(ih)) 
      isep= fip(isepx)
30    ixh=ixh-1 
      ixl=ixl+1 
31    if(fip(ixh).le.isep) goto 32
         ixh=ixh-1
      goto 31
32    if(fip(ixl).ge.isep) goto 33
         ixl=ixl+1
      goto 32
33    if(ixl.le.ixh) then 
         call swapd(fip(ixh),fip(ixl))
         call swapi(jp(ixh),jp(ixl))
         goto 30
      endif 
      if(ixh+ixl.le.ih+il1) then
         ilow(nsegs)=ixl
         ihigh(nsegs)=ih
         ih=ixh 
      else
         ilow(nsegs)=il1
         ihigh(nsegs)=ixh 
         il1=ixl
      endif 
      nsegs=nsegs+1 
80    if(ih-il1.ge.11) goto 20
      if(il1.eq.1) goto 10
90    if(il1.eq.ih) goto 70 
91    if(fip(il1).gt.fip(il1+1)) goto 92
         il1=il1+1
         if(il1.eq.ih) goto 70
      goto 91
92    isep=fip(il1+1) 
      ixl=il1 
      it=jp(il1+1)
100   fip(ixl+1)=fip(ixl) 
      jp(ixl+1)=jp(ixl) 
      ixl=ixl-1 
      if(isep.lt.fip(ixl)) goto 100 
      fip(ixl+1)=isep 
      jp(ixl+1)=it
      il1=il1+1 
      goto 90 
70    nsegs=nsegs-1 
      if(nsegs.eq.0) return 
      il1=ilow(nsegs) 
      ih=ihigh(nsegs) 
      goto 80 
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE SWAPI(ia,ib) 
      include 'p2conf.h'
c
      integer ia,ib,itemp 
c
      itemp=ib
      ib=ia 
      ia=itemp
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE SWAPD(ia,ib) 
      include 'p2conf.h'
c
      integer*4 ia,ib,itemp 
c------------------------------------------------------------------
c
      itemp=ib
      ib=ia 
      ia=itemp
      return
      end 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ORDER(a,b,c,pa,pb,pc)
      include 'p2conf.h'
c
      integer pa,pb,pc
      integer a,b,c 
c------------------------------------------------------------------
c
      if(c.lt.a) then 
         call swapd(a,c)
         call swapi(pa,pc)
      endif 
      if(b.lt.a) then 
         call swapd(a,b)
         call swapi(pa,pb)
      endif 
      if(c.lt.b) then 
         call swapd(b,c)
         call swapi(pb,pc)
      endif 
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE VMSP(ineq,ipc,ipri,ja,jl,ju,ia,il,iu,iva,ivl,ivu,
     +                maxlun,maxus,maxls,lsiz,usiz,ilsiz,iusiz,
     +                kp,ixt,ixb)
      include 'p2conf.h'
c 
c*******************************************************************
c*                                                                 *
c*           v m s p - sparse lu factorization (symbolic)          *
c*                                                                 *
c*******************************************************************
c*                                                                 *
c*    This subroutine performs the symbolic lu factorization       *
c*           of a general non-singular sparse matrix.              *
c*                                                                 *
c*         ***** arbitrary pivoting order version *****            *
c*                                                                 *
c*******************************************************************
c 
c  n     - number of rows/columns in the matrix.
c  ja(j) - start of row position descriptors in ia for col j
c  ia    - if negative, iabs is row index of a single element.
c        - if positive, starting row for a vector.
c          next element of ia is ending row for this vector.
c 
c********** 
c the following vectors are results from this routine 
c********** 
c 
c  iva   - index of first column j a matrix value.
c  ju(j) - start of row position descriptors in iu for col j
c  jl(j) - start of row position descriptors in il for col j
c  iu    - if negative, iabs is row index of single element.
c        - if positive, starting row for a vector, and the
c          next element of iu is ending row for this vector.
c  il    - if negative, iabs is row index of single element.
c        - if positive, starting row for a vector, and the
c          next element of il is ending row for this vector.
c  ivu   - index of first column j u matrix value.
c  ivl   - index of first column j l matrix value.
c  ixt   - scratch vector for top x indicies of length n. 
c          also used to permute the input row indicies
c  ixb   - scratch vector for bottom x indicies of len (n+1)/2. 
c  kp    - scratch permutation vector of length (n+1)/2.
c  maxus - maximum dimension for iu array.
c  maxls - maximum dimension for il array.
c  maxlun - maximum dimension for l,u array. 
c  lsiz   - actual size of l
c  usiz   - actual size of u
c  ilsiz  - actual size of il
c  iusiz  - actual size of il
c 
c  ipc   - column pivot permutation vector
c  ipri  - inverse row pivot permutation vector 
c 
c  kp,ixt,ixb - work space
c 
c*******************************************************************
c 
c   stop 27100 - bad n parameter to vmsp. 
c   stop 27101 - singular matrix because column j is zero.
c   stop 27102 - singular matrix because of pivoting. 
c   stop 27103 - missing pivot element in column j (nothing on diagonal)
c   stop 27104 - insufficient space to continue.
c   stop 27105 - row index bad on input.  top end of vector has 
c                higher index than bottom or input not spaced with
c                at least one free row between input elements.
c   stop 27106 - input error - column ends in the middle of a vector. 
c   stop 27107 - internal error on return from vsort. 
c   stop 27108 - internal error on return from vsort. 
c   stop 27110 - too many symbolic positions - u
c   stop 27111 - too many symbolic positions - l
c   stop 27112 - too many numeric positions - u 
c   stop 27113 - too many numeric positions - l 
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
c blank just for linum....
c
c****************************************************************** 
c 
c                   equivalences
c 
      integer ineq,maxus,maxls,maxlun,n,iuxs,ilxs,iuxn,ilxn,i,j,
     +        jaxn,ipcx,jat,jab,in,lowlim,ixtop,ixbot,length,leng,
     +        indx,jtop,ixcol,ixlx,jbot,ixps,ixltop,ixlbot,
     +        ixpb,ixpc,iltop,ilbot,ixpc2,itemp,ipss,isymu,isyml,
     +        inuml,inumu,ixlpss,ixhpss,np1,lsiz,usiz,ilsiz,iusiz,
     +        ja(1),jl(1),ju(1),iva(1),ivl(1),ivu(1),
     +        kp(1),ixt(1),ixb(1)
      INTeger   ia(1),il(1),iu(1),ipc(1),ipri(1)
c 
c****************************************************************** 
c 
c                   start 
c 
      n=ineq 
      if(n.ge.32766 .or. n.le.0) then
         call erset(27,linum,27100) 
         return
      endif 
c*******************************************************************
c*                                                                 *
c*    following loop calculates the symbolic positions of          *
c*    elements in the jth column of matrices l and u.              *
c*                                                                 *
c*******************************************************************
c 
c iuxs is a pointer to the next unused position in iu.
c ilxs is a pointer to the next unused position in il.
c iuxn is a pointer to the next unused position in u. 
c ilxn is a pointer to the next unused position in l. 
c 
c u and l are numeric vectors. iuxn and ilxn are used to set the
c vectors ivu and ivl which are used in vmnp and in vmbp to 
c locate values when indexing u or l nonsequentially. 
c 
c 
      iuxs= 1 
      ilxs= 1 
      iuxn= 1 
      ilxn= 1 
      do 170 j= 1,n 
         ju(j)= iuxs
         jl(j)= ilxs
         ivu(j)= iuxn 
         ivl(j)= ilxn 
         jaxn= 0
         ipcx= ipc(j) 
         jat= ja(ipcx)
         jab= ja(ipcx+1)
         if(jat.ge.jab) then 
            call erset(27,linum,27101)
            return
         endif
c 
c********** 
c copy row indicies for elements in column j from ia into ixt 
c converting to scalar row indicies (all positive)
c********** 
c 
         in= 0
         lowlim= 0
20          ixtop= ia(jat)
            jat= jat+1
            if(ixtop.gt.0) goto 21
c 
c scalar in ia
c 
            ixtop= -ixtop 
            if(ixtop.lt.lowlim.or.ixtop.gt.n) then 
               call erset(27,linum,27105) 
               return
            endif 
            in= in+1
            ixt(in)= ipri(ixtop)
            jaxn= jaxn+1
            lowlim= ixtop+1 
            goto 23 
c 
c vector in ia
c 
 21         if(jat.eq.jab) then
               call erset(27,linum,27106) 
               return 
            endif 
            ixbot= ia(jat)
            jat= jat+1
            if(ixtop.gt.ixbot.or.ixtop.lt.lowlim.or.ixbot.gt.n) then 
                call erset(27,linum,27105)
                return
            endif 
           length= ixbot-ixtop+1 
           do 22 i= ixtop,ixbot
              in= in+1 
22            ixt(in)= ipri(i) 
           lowlim= ixbot+1 
           jaxn= jaxn+leng 
23         if(jat.lt.jab) goto 20 
c 
c sort permuted row indicies
c 
         call vsort(in,kp,ixt,ixb) 
c 
c vectorize into ixt, ixb 
c 
         indx= 0
         jtop= 0
         i= 1 
 30      ixcol= ixt(i)
         if(ixcol.le.jtop.or.ixcol.gt.n) then
            call erset(27,linum,27107)
            return
         endif
         jtop= ixcol
         ixlx= i+1
 31      if(ixlx.gt.in) goto 33
         jbot= ixt(ixlx)
         if(jbot.ne.jtop+1) goto 32
         jtop= jbot 
         ixlx= ixlx+1 
         goto 31
32       if(jbot.gt.n.or.jbot.le.jtop) then
            call erset(27,linum,27111)
            return
         endif
33       indx= indx+1 
         kp(indx)= indx+1 
         ixt(indx)= ixcol 
         ixb(indx)= jtop
         i= ixlx
         if(i.le.in) goto 30 
         in= indx+1 
         kp(indx)= in 
         indx= in 
         ixt(indx)= 32760 
         ixb(indx)= 32760 
         kp(indx)= -32760 
c 
c*******************************************************************
c 
c main inner loop for calculations on positions in column j.
c symbolic eliminations gets fill structure of column j of u and l. 
c 
c to operate on column j we get an element of the j column (it may
c be a vector or scalar (vector with top and bottom equal)) 
c and save its top and bottom row ends in ixltop and ixlbot.
c we obtain the positions in column x by stepping along the vectors 
c one position at a time (just once for scalars.) 
c 
c for each position in x above the diagonal we calculate
c the fill associated with a scalar multiply of that position 
c in x (row m) by the l vector part of column m, overlaying the 
c symbolic vector product on the x vector (column j of u/l) by
c updating the structure information about the current column in
c ixt and ixb.
c 
c*******************************************************************
         ixps= 1
c 
c get bounds on vector in x which we are at 
c 
 40      ixltop= ixt(ixps)
         ixlbot= ixb(ixps)
c 
c branch depending whether the current start position in ix 
c is above, at, or below (error) the diagonal 
c 
 50      if(ixltop-j) 53,100,190 
c*******************************************************************
c 
c current start position in x is above diagonal- compute fill.
c 
c if column ixltop of the symbolic matrix l contains no elements
c then we do nothing since there will be no symbolic fill.
c 
c if there are symbolic positions in column ixltop of matrix l: 
c 
c      set ixpc to point to the current element in the column 
c       which is intially ixps, the start position, and set ixpb
c       to point to the previous position in the column, initially
c       there is none 
c 
c      get the next element of the symbolic column ixltop of matrix 
c       l (we know there is one because we check before the start 
c       of the loop and at the end of the loop before looping again)
c 
CC 51      continue 
c 
c      update the structure of the current column as represented
c      by ixtop and ixbot:
c 
c       step along the vectors in x until we find a vector which
c 
c       1. overlaps the element of l we are at
c            we then update the low and high ends of the
c            current x vector to represent this overlay,
c            including in the composite any vectors in x
c            which are overlapped by the new x element. 
c 
CC 52   continue
c 
c    or 2. starts more than one position after the end of the 
c          end of the l vector. 
c            we then insert the element from l before this
c            element of x.  this has a special case since the 
c            end of column marker is a single row at row 32767
c            and thus copying any positions in l to the end of the
c            "true" line in x consists of inserting them before 
c            this fake position.
c 
c*******************************************************************
 53      continue 
c get column in l corresponding to current start position 
         jtop= jl(ixltop) 
         jbot= jl(ixltop+1)-1 
         if(jtop.gt.jbot) goto 94
c set backup and current pointers in x to initial values
         ixpb= 0
         ixpc= ixps 
c get another element from l
 60      iltop= il(jtop)
         if(iltop.lt.0) goto 61
         jtop= jtop+1 
         ilbot= il(jtop)
         goto 62
 61      ilbot= -iltop
         iltop= ilbot 
 62      jtop= jtop+1 
         leng= ilbot-iltop+1
c step down the x vector till we overlap or pass l vector 
 70      ixtop= ixt(ixpc) 
         ixbot= ixb(ixpc) 
         if(ixbot+1.ge.iltop) goto 80
         ixpb= ixpc 
         ixpc= kp(ixpb) 
         goto 70
 80      if(ilbot.ge.ixtop-1) goto 90
c *** insert l vector in the x list above current 
         in= in+1 
         ixt(in)= iltop 
         ixb(in)= ilbot 
         kp(in)= ixpc 
         kp(ixpb)= in 
         ixpb= in 
         goto 93
c *** vectors overlap so update x vectors if necessary
 90      if(ixtop.le.iltop) goto 91
         ixtop= iltop 
         ixt(ixpc)= ixtop 
 91      if(ilbot.le.ixbot) goto 93
         ixbot= ilbot 
         ixb(ixpc)= ixbot 
c if x current overlaps x succeeding, combine all such overlaps 
         ixpc2= kp(ixpc)
 92      if(ixpc2.eq.indx .or. ixt(ixpc2)-1.gt.ixbot) goto 93
         itemp= ixb(ixpc2)
         ixbot= max0(itemp,ixbot) 
         ixb(ixpc)= ixbot 
         ixpc2= kp(ixpc2) 
         kp(ixpc)= ixpc2
         goto 92
c continue with next element of l if not end of column in l 
 93      if(jtop.le.jbot) goto 60
c bump start indicator in x 
 94      leng= ivl(ixltop+1)-ivl(ixltop)
         ixlbot= ixb(ixps)
         if(ixltop.eq.ixlbot) goto 95
         ixltop= ixltop+1 
         goto 50
 95      ixps= kp(ixps) 
         if(ixps.ne.indx) goto 40
c********** 
c diagonal reached - check if any array maximums exceeded 
c********** 
 100     ixps= 1
         ipss= 1
         isymu= 0 
         inumu= 0 
         isyml= 0 
         inuml= 0 
 101        ixlpss= ixt(ipss) 
            ixhpss= ixb(ipss) 
            if(ixlpss-j) 102,103,104 
 102        isymu= isymu+1
            if(ixlpss.ne.j-1 .and. ixlpss.ne.ixhpss) isymu= isymu+1
            inumu= inumu+min0(ixhpss,j-1)-ixlpss+1
 103        if(ixhpss.le.j) goto 105 
            ixlpss= j+1 
 104        isyml= isyml+1
            if(ixlpss.ne.ixhpss) isyml= isyml+1
            inuml= inuml+ixhpss-ixlpss+1
 105        ipss= kp(ipss)
            if(ipss.ne.indx) goto 101
         if(iuxs+isymu.gt.maxus) then
            call erset(27,linum,27110)
         endif
         if(ilxs+isyml.gt.maxls) then
            call erset(27,linum,27111)
            return
         endif
         if(iuxn+inumu.gt.maxlun) then
            call erset(27,linum,27112)
            return
         endif
         if(ilxn+inuml.gt.maxlun) then
            call erset(27,linum,27113)
            return
         endif
         goto 120 
c*******************************************************************
c copy ix to iu and il
c*******************************************************************
 110     continue 
c********** 
c do u part 
c********** 
         ixps= kp(ixps) 
c check for end of column 
         if(ixps.eq.indx) goto 163 
 120     ixtop= ixt(ixps) 
         ixbot= ixb(ixps) 
         if(ixtop-j) 130,140,161 
c current vector starts before the diagonal.
c if it runs across the diagonal go split it. 
 130     if(ixbot.gt.j) goto 150 
c diagonal belongs to noone 
         if(ixbot.eq.j) ixbot= j-1 
         if(ixtop.eq.ixbot) goto 131 
c store a vector in u 
         iu(iuxs)= ixtop
         iuxs= iuxs+1 
         iu(iuxs)= ixbot
         leng= ixbot-ixtop+1
         iuxs= iuxs+1 
         iuxn= iuxn+leng
         goto 110 
c store a scalar in u 
 131     continue
         iu(iuxs)= -ixtop 
         iuxs= iuxs+1 
         iuxn= iuxn+1 
         goto 110 
c current vector starts at the diagonal 
c check to see if it splits between u and l 
 140     if(ixbot.le.j) goto 110 
         goto 152 
c current vector runs across the diagonal 
c  up to (not including) the diagonal goes into u.
c  from one after the diagonal to the end goes into l.
 150     if(ixtop.eq.j) goto 152 
c put something in u
         if(ixtop.ne.j-1) goto 151 
         iu(iuxs)= -j+1 
         iuxs= iuxs+1 
         iuxn= iuxn+1 
         goto 152 
 151     iu(iuxs)= ixtop
         iuxs= iuxs+1 
         iu(iuxs)= j-1
         iuxs= iuxs+1 
         leng=j-ixtop 
         iuxn= iuxn+leng
c set jxtop so code below will do l part of a broken column.
 152     ixtop= j+1 
         goto 161 
c********** 
c do l part 
c********** 
 160     continue 
c continue along row in x if anything is left 
         ixps= kp(ixps) 
         if(ixps.eq.indx) goto 163 
         ixtop= ixt(ixps) 
         ixbot= ixb(ixps) 
 161     if(ixtop.eq.ixbot) goto 162 
c part in l is a vector 
         leng= ixbot-ixtop+1
         il(ilxs)= ixtop
         ilxs= ilxs+1 
         il(ilxs)= ixbot
         ilxs= ilxs+1 
         ilxn= ilxn+leng
         goto 160 
c part in l is a scalar 
 162     continue
         il(ilxs)= -ixtop 
         ilxs= ilxs+1 
         ilxn= ilxn+1 
         goto 160 
 163     continue 
c163     iva(ipcx)= jaxn
 170     continue 
c ********************* end of main loop ********************** 
      np1= n+1
      ivu(np1)= iuxn
      ivl(np1)= ilxn
      ju(np1)= iuxs 
      jl(np1)= ilxs 
      usiz=iuxn 
      lsiz=ilxn 
      iusiz=iuxs
      ilsiz=ilxs
      jaxn= 1 
      do 180 j= 1,n 
      leng= ivl(j+1)-ivl(j) 
      leng= ivu(j+1)-ivu(j) 
c     itemp= iva(j) 
c     iva(j)= jaxn
c     jaxn= jaxn+itemp
 180  continue
c     iva(np1)= jaxn
      return
 190  call erset(27,linum,27102)
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE VSORT(in,kp,ixt,ixb)
      include 'p2conf.h'
c 
c                      Partition sorting algorithm
c      Reference : Collected algorithms of the ACM - 63,64,65 
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      integer ihigh(32),ilow(32),nsegs,il1,ih,isep,isepx,ixl,ixh,itt,in
      integer kp(1),ixt(1),ixb(1)
c 
c****************************************************************** 
c 
c                   initialize
c 
      nsegs= 1
      il1= 1
      ih= in
c if no elements in this segment do nothing 
 10   if(il1.ge.ih) goto 70
c choose isep (separation entry): 
c  make ixt(il1) <= ixt((il1+ih)/2) <= ixt(ih) by interchange 
c  set isep= ixt((il1+ih)/2)
 20   isepx= (ih+il1)/2 
      isep= ixt(isepx)
c ixl is lower segment index (current)
      ixl= il1
c make ixt(il1) <= ixt(isepx) 
      if(ixt(il1).le.isep) goto 30 
         ixt(isepx)= ixt(il1) 
         ixt(il1)= isep 
         isep= ixt(isepx) 
c ixh is highest segment index (current)
 30   ixh= ih 
c make ixt(ih) >= ixt(isepx)
      if(ixt(ih).ge.isep) goto 50
         ixt(isepx)= ixt(ih)
         ixt(ih)= isep
         isep= ixt(isepx) 
c make ixt(il1) <= ixt(isepx) 
      if(ixt(il1).le.isep) goto 50 
         ixt(isepx)= ixt(il1) 
         ixt(il1)= isep 
         isep= ixt(isepx) 
         goto 50
c exchange low part entry which is greater than separator with high 
c part entry which is less than or equal to the separator value.
 40   itt= ixt(ixh) 
      ixt(ixh)= ixt(ixl)
      ixt(ixl)= itt 
c move down upper segment as far as we can
 50   ixh= ixh-1
      if(ixt(ixh).gt.isep) goto 50 
c move up lower segment as far as we can
 51   ixl= ixl+1
      if(ixt(ixl).lt.isep) goto 51 
c nothing to do if both segments have at most one entry in common 
      if(ixl.le.ixh) goto 40 
c if both segments overlap then they are separated
c in this case continue with shorter segment, storing the longer
      if(ixh-il1.le.ih-ixl) goto 60
c lower segment longer, contin with upper after saving lower
      ilow(nsegs)= il1
      ihigh(nsegs)= ixh 
      il1= ixl
      nsegs= nsegs+1
      goto 80 
c upper segment longer, contin with lower after saving upper
 60   ilow(nsegs)= ixl
      ihigh(nsegs)= ih
      ih= ixh 
      nsegs= nsegs+1
      goto 80 
c get another segment for processing if there are any more
 70   nsegs= nsegs-1
      if(nsegs.eq.0) return
      il1= ilow(nsegs)
      ih= ihigh(nsegs)
c continue to segment as long as length is greater than 11
 80   if(ih-il1.ge.11) goto 20 
      if(il1.eq.1) goto 10 
      goto 91 
c sort elements within segment by interchange of adjacent pairs 
 90   il1= il1+1
 91   if(il1.eq.ih) goto 70
      isep= ixt(il1+1)
      if(ixt(il1).le.isep) goto 90 
      ixl= il1
 100  ixt(ixl+1)= ixt(ixl)
      ixl= ixl-1
      if(isep.lt.ixt(ixl)) goto 100
      ixt(ixl+1)= isep
      goto 90 
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE SYMCK(infil,outfil,lmindg,ldisct,lprint,loutfl,linfil,
     +                 lstrip)
      include 'p2conf.h'
c 
c     Symbolic factorization card check routine 
c
c     Original : C.H.Price      Stanford University        May, 1982
c     Revision : MRP,CSR        Stanford University        Mar, 1984
c 
c     Copyright c 1981 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   common areas
c 
      include     'blank.h'
      include     'stat.h'
      include     'symb.h'
c
c****************************************************************** 
c 
c                   type declarations 
c 
      logical lmindg,ldisct,lprint,loutfl,linfil,lstrip
      character*20 infil,outfil
c
c FUNCTIONS:
      logical iscval
      real    gtrval
      logical gtlval
c 
c****************************************************************** 
c 
c...Initialize
      lsymdn=.false.
      lgumm=.false.
      lnewt=.false.
      lpoiss=.false.
      nmult=1
c
c...Check file input mode and get file name if specified
      linfil=.false.
      if (.not.iscval(1)) goto 2
      linfil=.true. 
      call gtcval(1, infil, LEN(infil))
c
c...Check file output mode and get file name if specified
2     loutfl=.false.
      if (.not.iscval(2)) goto 3
      loutfl=.true. 
      call gtcval(2, outfil, LEN(outfil))
c
c...Set print flag
3     lprint=gtlval(2)
      lstrip=gtlval(9)
c
c-------------------
c  Solution method
c-------------------
c
c...No. of carriers
      ncarr=gtrval(2)
      if(ncarr.eq.2) goto 20
      if(ncarr.eq.1) goto 50
      if(ncarr.ne.0) goto 11
      lpoiss=.true.
      lgumm=.true.
      goto 60
11    call erset(295,linum,0)
      goto 99
c
c...If only one carrier, which one? (default=electrons)
50    if(.not.(gtlval(3).and.gtlval(4))) goto 30
      call erset(292,linum,0)
      goto 99
30    l1hole=gtlval(4)
c
c...Gummel or Newton?
c...(error if more than one or none)
20    lgumm=gtlval(5)
      lnewt=gtlval(6)
      if(lgumm.and.lnewt) call erset(294,linum,0)
      if(.not.(lgumm.or.lnewt)) call erset(282,linum,0)
      if(lnewt) then
         nmult=ncarr+1
      endif
c
c
c...Get dissection (minimum degree) flag 
60    ldisct=gtlval(8)
      lmindg=gtlval(1)
      if(ldisct.and.lmindg) call erset(30,linum,0)
      if(ldisct.and.(.not.lrect)) call erset(-129,linum,0)
c
c...Check number of nodes depending on solution mode.
      if(lgumm .and. np.gt.mpgum) call erset(180,linum,mpgum)
      if(lnewt .and. ncarr.eq.1 .and. np.gt.mp1fn)
     +                             call erset(183,linum,mp1fn)
      if(lnewt .and. ncarr.eq.2 .and. np.gt.mp2fn)
     +                             call erset(184,linum,mp2fn)
c
c...Cannot use external elements with anything but full newton
      if(lgumm.and.(nlump.gt.0)) 
     +   call erset(252,linum,0)
c
c...Done
   99 return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE MINDG
      include 'p2conf.h'
c
c                 Driver for YSMP minimum degreee algorithm.
c
c-----------------------------------------------------------------------
c     Original :        CSR Nov 83
c
c     Copyright c 1983 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
      include     'blank.h'
      include     'symb.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'symtmp3.h'
      integer TMPPAD(1263005)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
c                   local types
      integer flag,csiz,i
c     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c................Expand Calahans's compressed column-ordered arrays
c                into column-ordered YSMP form.
c                (Change of nomenclature : cia is the pointer table,
c................ cja is the long list of entries. Conor's convention.)
c
      call ijexp(neq,ja,ia,cia,cja,csiz)
      if(errflg) return

c................ Since the matrix is symmetric, we can skip
c                 the column- to row-wise transformation

c
c...................Call the YSMP routine
c
      flag=0
      call md(neq,cia,cja,iysiz,iydim,tv,tl,head,last,next,tv,flag)
c
c...................May not have had enough storage
c
      if(flag.ne.0) then
          call erset(-279,0,flag)
          return
      endif
c
c...................Set up the pivot rows and columns according
c                   to the recommended reordering.
c
      do 3000 i=1,neq
          ipri(i)=next(i)
          ipc(i)=last(i)
 3000 continue

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IJEXP(neq,ia,ja,cia,cja,csiz)
      include 'p2conf.h'
c
c       Expand (ia,ja) from compressed to regular storage format.
c
c       neq             = Number of equations     (Input)
c       (ia,ja)         = Compressed sparsity map (Input)
c       (cia,cja)       = Expanded sparsity map   (Output)
c       csiz            = Size of cja             (Output)
c
c     Original :        CSR Dec 83
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c...............local types
c...seems like Conor's naming is switched (i's and j's) (mje mar 90)
      INTeger ja(1)
      integer ia(1)
      integer cja(1)
      integer cia(1),neq
      integer csiz,ci,jlo,jhi,jix,it,it1,icount,iii
c
c********************* Start *******************************************
c
      csiz=0
      cia(1)=1
      do 1000 ci=1,neq
          jlo=ia(ci)
          jhi=ia(ci+1)-1
c...................From bottom to top of column, in steps of 1 or 2
          jix=jlo
  500     if(jix.gt.jhi) goto 950

          it=ja(jix)
          if(it.lt.0) then
              csiz=csiz+1
              cja(csiz)=-it
              jix=jix+1
          else
              it1=ja(jix+1)
              icount=it1-it+1
              do 600 iii=1,icount
  600             cja(csiz+iii)=it+iii-1
              csiz=csiz+icount
              jix=jix+2
          endif
          goto 500
  950     continue

          cia(ci+1)=csiz+1
 1000 continue
 
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE IJRC(ineq,cia,cja,ria,rja,cjatag,cjaw8,cjaxxx,csiz)
      include 'p2conf.h'
c
c...............Convert matrix from column-ordered(cia,cja) format
c               to row-ordered (ria,rja).
c...............Only for uncompressed storage.
c
c     ineq       = number of rows/columns
c     csiz      = size of cja
c     cia       = column table
c     cja       = row indices
c     ria,rja   = row table, column indices
c     ciatag    = temporary for tagging each element of cja
c     cjaw8     = temporary for weighting each element of cja
c     cjaxxx    = temporary for holding column indices of cja
c
c     Original :        CSR Dec 83
c
c     Modified :        CSR Jan 84  Faster algorithm :  D.A.Calahan 
c                                   "Computer Aided Network Design"
c                                   McGraw Hill 1972. p.277
c                                   Now n log n, not n**2
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c...............common area
      include     'blank.h'
c blank just for linum.

c...............local types
      INTeger   rja(1)
      integer   cja(1)
      integer cia(1),ria(1),cjatag(1),cjaw8(1),cjaxxx(1)
      integer ri,ysiz,ci,ilo,ihi,iix,ineq,ineqp1,csiz,ii
c
c******************** Start ********************************************
      ineqp1=ineq+1

c......Initialise row length table
      ria(1)=1
      do 1100 ri=2,ineqp1
1100     ria(ri)=0

      ysiz=0

c......Scan all entries of the column ordered form;
c      weight each with its position in matrix, remember its column 
c......number, and increment the row's entry in the row length table.
      do 1500 ci=1,ineq
          ilo=cia(ci)
          ihi=cia(ci+1)-1
          do 1500 iix=ilo,ihi
             
             ysiz=ysiz+1
             ri=cja(ysiz)
             cjaxxx(ysiz)=ci
             ria(ri+1)=ria(ri+1)+1
             cjaw8(ysiz)=ineqp1*ri+ci
 1500  continue

c.......Finish row length table
       do 1600 ri=1,ineq
 1600     ria(ri+1)=ria(ri+1)+ria(ri)

c.......Set tags, then index and sort rja
       do 1750 ii=1,ysiz
 1750     cjatag(ii)=ii

       call vsrt4(ysiz,cjatag,cjaw8)

       do 1800 ii=1,ysiz
 1800     rja(ii)=cjaxxx(cjatag(ii))
       
c...............Look before we leap
      if(ysiz.ne.csiz) then
          call erset(-132,linum,0)
          return
      endif

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE ROWORD
      include 'p2conf.h'
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                   common area
c
      include     'blank.h'
      include     'emaco.h'
c....the magic TMP common memory overlays ....
      include     'symtmp4.h'
      integer TMPPAD(1220007)
      common/tmpco/ TMPPAD
c....NOTE that the above padding MUST be here for the overlays
c------------------------------------------------------------------
c
      integer csiz
c     
c
c...Expand Calahan's compressed column-ordered arrays
c...(Change of nomenclature : cia is the pointer table,
c...cja is the long list of entries. Conor's convention.)
c
      call ijexp(neq,ja,ia,cia,cja,csiz)
      if(errflg) return
c
c...Row ordering - put result in iar,jar (back to Calahan's naming
c...convention)
      call ijrc(neq,cia,cja,jar,iar,cja1,cja2,cja3,csiz)
c
c...Done
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE LUMPA
      include 'p2conf.h'
c
c     Get addresses in a for lumped-element contacts and
c     current bc.  Need diagonal for both as well as an
c     extra potential element (any) from same row for 
c     current bc.
c
c     Original : MRP  Dec 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'blank.h'
      include     'setup.h'
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer i,nlbc,mapa
c
c...For now, can only do with full Newton
      do 232 i=1,nelect
         if(lresis(i).or.lcurbc(i)) then
            if(.not.lnewt) goto 240
            nlbc=nresis(i)
            ialump(i)=mapa(nlbc,nlbc)
         endif
232   continue
      goto 999
c
240   call erset(252,linum,0)
999   return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      INTEGER FUNCTION MAPA(irow,icol)
      include 'p2conf.h'
c
c     Function that returns location of the matrix (a) element 
c     specified by row/col pointers irow,icol.  a is assumed to
c     be column ordered.  The value -1 is returned if the entry
c     is not found.
c
c     Original : MRP          Stanford University         Mar, 1984
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      include     'emaco.h'
c------------------------------------------------------------------
c
      integer irow,icol,inextc,idxa,idxia,ilow,ihigh,i
c------------------------------------------------------------------
c
c*************
c**  START  **
c*************
c
c...Initialize 
      idxia=ja(icol)
      idxa=iva(icol)
      inextc=ja(icol+1)
c
c...Loop 
10    ilow=ia(idxia)
c
c...Vector or simple element?
      if(ilow.lt.0) then
       i=-ilow
       if(i.eq.irow) goto 99
       idxa=idxa+1
c
c...Vector
      else
       idxia=idxia+1
       ihigh=ia(idxia)
         if(irow.lt.ilow) goto 98
       if(irow.le.ihigh) then
          idxa=idxa+irow-ilow
          goto 99
       endif
       idxa=idxa+ihigh-ilow+1
      endif
c 
      idxia=idxia+1
      if(idxia.lt.inextc) goto 10
c
c...Done
98    mapa=-999
      return
99    mapa=idxa
      return
      end
