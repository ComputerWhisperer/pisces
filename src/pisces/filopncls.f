cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Tue May 16 16:43:02 PDT 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE FOPCL(iop,ffname,nch,lu,lignor,ierr)
c
c     fopcl = file open and close 
c
c     Input
c     =====   iop     0=close
c                     -1=close and delete
c                     1=open binary for read
c                     2=open ascii  for read
c                     11=open binary for write
c                     12=open ascii  for write
c                     21=open binary for write - append
c                     22=open ascii  for write - append
c                     31=open binary for write - scratch
c                     32=open ascii  for write - scratch
c             ffname  file name
c             nch     number of characters in ffname
c             lu      logical unit
c             lignor  ignore error (but return code)
c             ierr    error number
c
c     Original:
c     Modified: Michael Eldredge -- Stanford (oct 87)
c         convert to use new GENII
c     Modified: Michael Eldredge -- Stanford (may 88)
c         add the XGETEN() option.  We have supplied a couple different
c         versions of xgeten.f; try one of them.  If all else fails
c         then use the xgeten_def.f version which reads the environment
c         from files.  Or you could just write xgeten() like:
c
c             subroutine xgeten(key, val)
c             character*(*) key, val
c             val = ' '
c             return
c             end
c
c         Then, the default action will happen.
c     Modified: MJE -- Stanford (may 89) added the close&delete op
c
c     Copyright c 1984 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      integer iop,lu,ierr,nch
      character*20 cform,cstatu
      character*40 namfil
      character*(*) ffname
      character tchar
      logical lread ,lform,lexist,iopen,lapp,lscrat,lignor
c
c******************** Start ********************************************
c
c                   Decode iop.
c
      iopen = iop.gt.0
      lread  = (iop.lt.10)
      lapp   = (iop.gt.20)
      lscrat = (iop.gt.30)
      lform  = (mod(iop,10).ne.1)
      cform='unformatted'
      if(lform) cform='formatted'
c 
c...see if there is a DEFINE by the file name, use that instead.
      call xgeten(ffname, namfil)
      if (namfil(1:1) .eq. ' ') namfil = ffname
CC..on older unix systems you had to NULL terminate the string
CC      if(nch.lt.40) namfil(nch+1:40)=CHAR(0)
c 
      if(.not.iopen) then
       if (iop.eq.0) then
          close(unit=lu)
       else if (iop.eq.-1) then
          close(unit=lu, status='delete')
       endif
       return

      else
        call filinq(namfil,40,lexist,ierr)
        if(ierr.gt.0) goto 2000
        cstatu='new'
        if(lexist) cstatu='old'
        if(lscrat) cstatu='scratch'

c...........Read : check that it exists.
          if(lread ) then
c...........If the file is not found, then set ierr to a I/O error
c...........  number that corrresponds to [FILENOTFOUND] on your system.
            if(.not.lexist) then
              ierr=2
              goto 2000
            endif

            open (unit=lu,file=namfil,iostat=ierr,err=2000,
     +                     form=cform,status='old')

c............Write : 
       else
            open (unit=lu, file=namfil, iostat=ierr, err=2000,
     +              form=cform, status=cstatu)
       endif
      endif

c......Set the file pointer to a known state.
      rewind(lu)

c......Append? Need to find the end of the file.
c......Read bytes until an end-of-file.
      if(lapp) then
  500    continue
            if(lform) then
             read(lu,'(a1)',end=501,err=2000,iostat=ierr) tchar
          else
             read(lu,end=501,err=2000,iostat=ierr) tchar
          endif
          goto 500
  501    continue
      endif

      return
c
c...Sorry
 2000 if(.not.lignor) then
         write(6,2001) namfil,ierr 
         call erset(289,-1,0)
 2001    format(/' Error on open of ',a20,': #',i3) 
      endif
      return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE FILINQ(ffname,nch,lexist,ierr)
c
c     Does file fname exist? - return .true. if it does.
c
c     Original: Mark R. Pinto   Stanford University  May,1985
c
c     Copyright c 1985 The board of trustees of the Leland Stanford 
c                      Junior University. All rights reserved.
c     This subroutine may not be used outside of the PISCES computer
c     program without the prior written consent of Stanford University. 
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      character*(*) ffname
      character*40  namfil
      integer ierr,nch
      logical lexist
c
c...see if there is a DEFINE by the file name, use that instead.
      call xgeten(ffname, namfil)
      if (namfil(1:1) .eq. ' ') namfil = ffname
CC..on older unix systems you had to NULL terminate the string
CC      if(nch.lt.40) namfil(nch+1:40)=CHAR(0)
c
      inquire(file=namfil,exist=lexist,err=99,iostat=ierr)
99    return
      end
