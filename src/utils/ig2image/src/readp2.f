c	readp2.f		Version 1.7		
c	Last Modification:	3/31/90 09:37:09	
c
c  Copyright 1989 by
c  The Board of Trustees of the Leland Stanford Junior University
c  All rights reserved.
c
c  This routine may not be used without the prior written consent of
c  the Board of Trustees of the Leland Stanford University.
c
c



c     
c     Read pisces mesh and solution files
c     and write the corresponding suprem4 structure.
C     
      SUBROUTINE READP2(meshf, solf, outf, key, mfiles, minx,
     +  maxx, miny, maxy, gmin, gmax, tlog, tabs, traps)

c
c     meshf - pisces file for mesh
c     solf  - pisces file for solution
c     outf  - output file in suprem4 format
c     key - solution variable
c     mfiles - multiple pisces files
c     minx - minimum x value
c     maxx - maximum x value
c     miny - minimum y value
c     maxy - maximum y value
c     gmin - minimum value of solution variable
c     gmax - maximum value of solution variable
c     tlog - take log
c     tabs - takes absolute value
c     traps - are there deep level traps in the solution file
c
      character*80 meshf
      character*80 solf
      character*80 outf
      integer key
      integer mfiles
      real minx
      real maxx
      real miny
      real maxy
      real gmin
      real gmax
      integer tlog
      integer tabs
      integer traps
     
      integer*2 itype(3000), nop(3,6000), nbc(300), imat(6000)
      integer*2 lm(3000), ietype(300), eptr(3000), nextel(3,6000)
      integer ecode(20), mattyp(10) 
      integer nx, ny, np, ne, nb, nelect, nepb, nmat, ndreg
      integer lu, i, j, k, np2, ne2, nelec2, nmat2, im
      integer ip, tt, m, nn, ie, ncarr0
      integer flag(3000,8), nm(3000), nq(3000)
      integer s4mat(8), nintrf
      integer ijunk(10)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     integer nspar
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      logical rdsol
      logical outpt
      logical ljunk
      logical curr
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     logical lconmb, lfldmb, lconlt, lsrh, lauger
c     logical lbgn, lboltz, lion, lincom
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      real rtime, ntemp, delt, delt0, stime
      real cord(2,3000), r1(3000), ehed(3,6000), jhjd(3,6000)
      real es(3,6000), mobn(3000), mobp(3000), lmetal(300)
      real dopsgn(10), essem(3000), estot(3000), dintf(3000)
      real ssdata(10,7)
      real rjunk(15)
c  rnti1 is for deep level traps
      real rnti1(3000)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     real vsn(10), vsp(10), workf(10) 
c     real epsmat(10)
c     real semmat(40)
c     real tpu
c     real tnu
c     real cnu
c     real cpu
c     real ncu
c     real nvu
c     real rtmp
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      double precision fp(3000), fn(3000), fv(3000), qfn(3000)
      double precision qfp(3000), jei(3000), jhi(3000), jti(3000)
      double precision dbnum
      double precision djunk(10)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     double precision bias(10), amp(10), dflux(10), dflux0(10)
c     double precision vres(10), vres0(10)
c     double precision dfxpt(10), dfxpt0(10)
c     double precision cresis(10), ccapac(10)
c     double precision jtci0, jdi0, ei0, ui0
c     double precision dtmp
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      character*20 itime
      character*60 itit1

      data curr/.false./
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     data nspar/40/
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c     
c...Read the file names
    
 
      rdsol=.false. 
      if (solf.ne.' ') rdsol=.true.

      outpt=.false.
      if (outf.ne.' ') outpt=.true.
      
c     
c...Read the mesh information      
      lu=1
      open (unit=lu,file=meshf,err=2000,form='unformatted',status='old')
      
      read(lu,err=992,end=993) itit1
      read(lu,err=992,end=993) nx,ny,np,ne,nb,nelect,nmat,nepb
      
      if (np.gt.3000 .or. ne.gt.6000 .or. nb.gt.300) then
         write(6,*) 'Weird bounds in binary file, I give up:', np,ne,nb
         stop 1
      endif
      
      read(lu,end=993,err=992) 
     +     ((cord(j,i),j=1,2),r1(i),dintf(i),itype(i),eptr(i),
     +     mobn(i),mobp(i),lm(i),essem(i),estot(i),i=1,np)
      read(lu,end=993,err=992) 
     +     ((nop(i,j),i=1,3),(ehed(i,j),i=1,3),
     +     (jhjd(i,j),i=1,3),(es(i,j),i=1,3),
     +     (nextel(i,j),i=1,3),imat(j),j=1,ne)
      read(lu,end=993,err=992) (nbc(i),ietype(i),lmetal(i),i=1,nb)
      read(lu,end=993,err=992) ndreg,(dopsgn(i),ecode(i),i=1,ndreg)
      read(lu,end=993,err=992) (mattyp(i),i=1,nmat)
      read(lu,end=993,err=992) itime
      close(lu)
      
c...Pisces internal coordinates are cm
      do 98 ip=1,np
         do 98 j=1,2
            cord(j,ip) = cord(j,ip)*1e4
            if (key .ne. 0)  then
              if ((j .eq. 1) .and. (cord(j,ip) .lt. minx)) then
                minx = cord(j,ip)
              endif
              if ((j .eq. 1) .and. (cord(j,ip) .gt. maxx)) then
                maxx = cord(j,ip)
              endif
              if ((j .eq. 2) .and. (cord(j,ip) .lt. miny)) then
                miny = cord(j,ip)
              endif
              if ((j .eq. 2) .and. (cord(j,ip) .gt. maxy)) then
                maxy = cord(j,ip)
              endif
            endif
 98      continue

c...Pisces negative boundary codes are not meaningful
      do 99 ie=1,ne
         do 99 j=1,3
            if (nextel(j,ie).lt.0) nextel(j,ie) = -1024
 99      continue

      
c...Read the solution info      
      if (rdsol) then
         open (unit=lu,file=solf,err=3000,form='unformatted',
     +      status='old')
         read(lu,end=1991,err=1992) itime
         read(lu,end=1991,err=1992) np2,ne2,nelec2,nmat2,rtime,
     +        ntemp,delt,delt0,ncarr0
         stime=rtime
         read(lu,end=1991,err=1992) 
     +        (fv(i),fn(i),fp(i),qfn(i),qfp(i),i=1,np2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        read(lu,err=1992) (bias(i),amp(i),dflux(i),
c    +                      dflux0(i),vres(i),vres0(i),i=1,nelect)
c        read(lu,err=1992) (dfxpt(i),dfxpt0(i),i=1,nb)
c        if (traps.eq.1) then
c            read(lu,err=1992) (rnti1(i),i=1,np2)
c        endif
c        read(lu,err=1992) (vsn(i),vsp(i),workf(i),
c    +                      cresis(i),ccapac(i),i=1,nelect)
c        read(lu,err=1992) nintrf,((ssdata(i,j),j=1,7),i=1,nintrf)
c        read(lu,err=1992) lconmb,lfldmb,lconlt,lsrh,lauger,
c    +                  lbgn,lboltz,lion,lincom
c        read(lu,err=1992) (mattyp(i),epsmat(i),i=1,nmat)
c        read(lu,err=1992) (semmat(i),i=1,5),tpu,tnu,
c    +                     (semmat(i),i=8,11),afu,semmat(13),
c    +                     cnu,cpu,(semmat(i),i=16,25),ncu,nvu,
c    +                     (semmat(i),i=28,nspar)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         read(lu,end=1991,err=1992)
     +        (djunk(i),djunk(i),djunk(i),djunk(i),djunk(i),djunk(i),
     +         i=1,nelect)
         read(lu,end=1991,err=1992)
     +        (djunk(i),djunk(i),i=1,nb)
         if (traps.eq.1) then
             read(lu,end=1991,err=1992) (rnti1(i),i=1,np2)
         endif
         read(lu,end=1991,err=1992)
     +        (rjunk(i),rjunk(i),rjunk(i),djunk(i),
     +        djunk(i),i=1,nelect)
         read(lu,end=1991,err=1992)
     +        nintrf
     +        ,((ssdata(i,j),j=1,7),i=1,nintrf)
         read(lu,end=1991,err=1992)
     +        ljunk,ljunk,ljunk,ljunk,ljunk,ljunk,ljunk,
     +        ljunk,ljunk
         read(lu,end=1991,err=1992)
     +        (ijunk(i),rjunk(i),i=1,nmat)
         read(lu,end=1991,err=1992)
     +        (rjunk(i),i=1,5),rjunk(1),rjunk(1),
     +        (rjunk(i),i=1,4),rjunk(1),rjunk(1),
     +        rjunk(1),rjunk(1),(rjunk(i),i=1,10),rjunk(1),
     +        rjunk(1), (rjunk(i),i=1,13)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       check to see if we have any data for currents.
c         
           read(lu,end=90,err=1992) djunk(1),jei(1),jhi(1),
     +        djunk(1), jti(1), djunk(1),djunk(1)
         curr=.true.

           do 100 i=2,np
c          read(lu,end=1991,err=1992) jtci0,jei0,jhi0,jdi0,jti0,
c    +         ei0,ui0
               read(lu,end=1991,err=1992) djunk(1),jei(i),jhi(i),
     +            djunk(1), jti(i), djunk(1),djunk(1)
 100       continue

 90      continue

c...Flush other stuff to get to currents
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         close(lu)
      endif

c...Write out the points and triangles.
     
      if (outpt) then
        open (unit=lu,file=outf,err=4000)
      else
        lu=6
      endif
      write(lu,4040)
 4040 format('v pisces converted file')
      
      do 150 ip=1,np
         write(lu,4050) ip, cord(1,ip), cord(2,ip)
 4050    format('c   ', i10, 2f12.5, '  0.0')
 150  continue
      do 200 ie=1,ne
         write(lu,4060) ie, imat(ie), nop(1,ie), nop(2,ie), 
     +        nop(3,ie),
     +        nextel(1,ie), nextel(2,ie), nextel(3,ie)
 4060    format('t   ', 8i8)
 200  continue
      
c...Regions, converting pisces material codes for suprem4      
c... for now just asssume any semiconductor material (Si, GaAs, etc)
c... gets mapped to Silicon
      do 300 i=1,nmat
         if ((mattyp(i).eq.1).or.(mattyp(i).eq.2)
     +          .or.(mattyp(i).eq.3)) then
            s4mat(i) = 3
         else if (mattyp(i).eq.-1) then
            s4mat(i) = 1
         else if (mattyp(i).eq.-2) then
            s4mat(i) = 2
         endif
         write(lu,4070) i, s4mat(i)
 4070    format('r  ',2i10)
 300  continue
      
      if (rdsol) then
c...Have to arrange dummy extra nodes.
         do 400 ip=1,np
            do 400 j=1,8
               flag(i,j)=0
 400        continue
            
            do 500 ie=1,ne
               m=s4mat(imat(ie))
               flag( nop(1,ie), m) = 1
               flag( nop(2,ie), m) = 1
               flag( nop(3,ie), m) = 1
 500        continue
            
            nn = 0
            do 600 ip=1,np
               do 601 m=1,8
                  if(flag(ip, m).ne.0) then
                     nn = nn+1
                     nm( nn ) = m
                     nq( nn ) = ip
                  endif
 601           continue
 600        continue
         
c    for suprem4 14=psi, 6=electron, 7=hole, 2=arsenic(net)   
c              23=electron current 24=hole current
c              25=total current (conduction + displacement)
c              26=deep level traps

            if ((curr).and.(traps.eq.0)) then
                write(lu,4075)
            else if ((curr).and.(traps.eq.1)) then
		write(lu,4085)
	    else if ((.not.curr).and.(traps.eq.0)) then
                write(lu,4080) 
	    else
		write(lu,4090)
            endif
 4075       format('s 7 14 6 7 2 23 24 25')
 4080       format('s 4 14 6 7 2')
 4085       format('s 8 14 6 7 2 23 24 25 26')
 4090       format('s 5 14 6 7 2 26')
            do 700 tt=1,nn
               ip = nq(tt)
               im = nm(tt)
               if ((curr).and.(traps.eq.0)) then
                   write(lu,701) ip-1, im, fv(ip), fn(ip), fp(ip), 
     +                 r1(ip), jei(ip), jhi(ip), jti(ip)
               else if ((curr).and.(traps.eq.1)) then
                   write(lu,703) ip-1, im, fv(ip), fn(ip), fp(ip), 
     +                 r1(ip), jei(ip), jhi(ip), jti(ip), rnti1(ip)
               else if ((.not.curr).and.(traps.eq.0)) then
                   write(lu,702) ip-1, im, fv(ip), fn(ip), fp(ip), 
     +                 r1(ip)
               else
                   write(lu,704) ip-1, im, fv(ip), fn(ip), fp(ip), 
     +                 r1(ip), rnti1(ip)
               endif

c  find check if we update gmin, gmax
               if (im .eq. 3) then
                   if (key .eq. 1) then
                       dbnum = fv(ip)
                   else if (key .eq. 2) then
                       dbnum = fn(ip)
                   else if (key .eq. 3) then
                       dbnum = fp(ip)
                   else if (key .eq. 4) then
                       dbnum = r1(ip)
                   else if (key .eq. 5) then
                       dbnum = jei(ip)
                   else if (key .eq. 6) then
                       dbnum = jhi(ip)
                   else if (key .eq. 7) then
                       dbnum = jti(ip)
		   else if (key .eq. 8) then
		       dbnum = rnti1(ip)
                   endif
                   if ((tabs .eq. 1) .and. (dbnum .lt. 0)) dbnum=-dbnum
                   if (tlog .eq. 1) then
                       if (dabs(dbnum) .le. 1.0) then
                           dbnum = 0.0
                       else if (dbnum .lt. -1.0) then
                           dbnum = -dlog10(-dbnum)
                       else
                           dbnum = dlog10(dbnum)
                       endif
                   endif
                   if (dbnum .lt. gmin) gmin = dbnum
                   if (dbnum .gt. gmax) gmax = dbnum
               endif
 700        continue
 701        format('n ',2(i5,' '),7(1pe12.5,' '))
 702        format('n ',2(i5,' '),4(1pe12.5,' '))
 703        format('n ',2(i5,' '),8(1pe12.5,' '))
 704        format('n ',2(i5,' '),5(1pe12.5,' '))
         
         if (outpt) close(lu)  
         endif
       
c    for multiple pisces files, scan through to get gmin and gmax 
         if (mfiles .eq. 1) then
           do 850 j=1,1000
             call incnm(solf)
             open (unit=lu,file=solf,err=900,form='unformatted',
     +          status='old')
             read(lu,end=1991,err=1992) itime
             read(lu,end=1991,err=1992) np2,ne2,nelec2,nmat2,rtime,
     +            ntemp,delt,delt0,ncarr0
             stime=rtime
             read(lu,end=1991,err=1992) 
     +            (fv(i),fn(i),fp(i),qfn(i),qfp(i),i=1,np2)

             read(lu,end=1991,err=1992)
     +        (djunk(i),djunk(i),djunk(i),djunk(i),djunk(i),djunk(i),
     +         i=1,nelect)
             read(lu,end=1991,err=1992)
     +        (djunk(i),djunk(i),i=1,nb)
             if (traps.eq.1) then
                 read(lu,err=1992) (rnti1(i),i=1,np2)
             endif
             read(lu,end=1991,err=1992)
     +        (rjunk(i),rjunk(i),rjunk(i),djunk(i),
     +        djunk(i),i=1,nelect)
             read(lu,end=1991,err=1992)
     +        nintrf
     +        ,((ssdata(i,k),k=1,7),i=1,nintrf)
             read(lu,end=1991,err=1992)
     +        ljunk,ljunk,ljunk,ljunk,ljunk,ljunk,ljunk,
     +        ljunk,ljunk
             read(lu,end=1991,err=1992)
     +        (ijunk(i),rjunk(i),i=1,nmat)
             read(lu,end=1991,err=1992)
     +        (rjunk(i),i=1,5),rjunk(1),rjunk(1),
     +        (rjunk(i),i=1,4),rjunk(1),rjunk(1),
     +        rjunk(1),rjunk(1),(rjunk(i),i=1,10),rjunk(1),
     +        rjunk(1), (rjunk(i),i=1,13)

              if (curr) then
               do 820 i=1,np
c              read(lu,end=1991,err=1992) jtci0,jei0,jhi0,jdi0,jti0,
c    +           ei0,ui0
                 read(lu,end=1991,err=1992) djunk(1),jei(i),jhi(i),
     +             djunk(1), jti(i), djunk(1),djunk(1)
 820           continue
             endif
             close(lu)
             do 800 tt=1,nn
               ip = nq(tt)
               im = nm(tt)
c  find check if we update gmin, gmax
               if (im .eq. 3)  then
                   if (key .eq. 1) then
                       dbnum = fv(ip)
                   else if (key .eq. 2) then
                       dbnum = fn(ip)
                   else if (key .eq. 3) then
                       dbnum = fp(ip)
                   else if (key .eq. 4) then
                       dbnum = r1(ip)
                   else if (key .eq. 5) then
                       dbnum = jei(ip)
                   else if (key .eq. 6) then
                       dbnum = jhi(ip)
                   else if (key .eq. 7) then
                       dbnum = jti(ip)
		   else if (key .eq. 8) then
		       dbnum = rnti1(ip)
                   endif
                   if ((tabs .eq. 1) .and. (dbnum .lt. 0)) dbnum=-dbnum
                   if (tlog .eq. 1) then
                       if (dabs(dbnum) .le. 1.0) then
                           dbnum = 0.0
                       else if (dbnum .lt. -1.0) then
                           dbnum = -dlog10(-dbnum)
                       else
                           dbnum = dlog10(dbnum)
                       endif
                   endif
                   if (dbnum .lt. gmin) gmin = dbnum
                   if (dbnum .gt. gmax) gmax = dbnum
               endif
 800        continue
 850       continue
         endif 
c...Coda
 900     continue
         return
 2000    write(6,*) 'Mesh file not found', meshf
 4000    write(6,*) 'Error on opening .piscestos4 for write'
 992     write(6,*) 'I/O error reading mesh file'
         stop 1
 993     write(6,*) 'Mesh file is too short'
         stop 1
c
 3000    write(6,*) 'Solution file not found', solf
 1991    write(6,*) 'I/O error reading solution file'
         stop 1
 1992    write(6,*) 'Solution file is too short'
         stop 1
         end


      SUBROUTINE INCNM(name)
      character*80 name 
c---------------------------------------------------------------------
c 
c     incnm : Increments an ascii string of up to 80 characters by 1 
c             character, starting with the last character and possibly
c             extending the incrementation to characters on the left
c             of the last character.
c             The characters are incremented using the sequence 0-9,a-z.
c             when a character is incremented beyond "z" the character
c             is set to "0" and the next character to the left is 
c             incremented.
c 
c     copyright c 1984 The Board of Trustees of Leland Stanford
c                      Junior University. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of Stanford University. 
c 
c 
c---------------------------------------------------------------------
c 
c     local variables 
c 
c---------------------------------------------------------------------
      integer i
      logical flag
c 
c---------------------------------------------------------------------
c 
c     start of incnm
c 
c---------------------------------------------------------------------
c.....find the last non-blank character in the name
      do 100 i=1,80,1
        if (name(i:i).eq.char(0)) goto 200
100   continue
c.....check for special characters in last digit and handle their  
c.....incrementing.
200   flag = .true.
      i = i - 1
10    if (.not.flag) goto 20
      flag = .false.
      if (name(i:i).eq.'z') then
        name(i:i) = '0'
        flag = .true.
        else if (name(i:i).eq.'Z') then
             name(i:i) = 'a'
        else if (name(i:i).eq.'9') then
             name(i:i) = 'A'
        else 
          name(i:i) = char(ichar(name(i:i))+1)
      endif
      i = i - 1
      if (i.eq.0) flag =.false.
      goto 10
20      continue
      return
      end 
