cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Mar  9 14:23:56 PST 1990 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
      SUBROUTINE XCLOCK(key,itime,elaptm)
c 
c            this routine reads the rte clock and then:
c    key =
c     1   puts time into itime
c     2   computes the elapsed time elaptm=time - elaptm
c         and puts time into itime
c     3   converts julian date into mo/day and puts 
c         mo, day, hr, min into itime 
c
c     Notes:
c     + The itime string must be less than or equal to 20 characters.
c     This is because lodsol will read/write the time stamp into/from
c     a 20 character buffer.  If we change it, all old save files
c     will break.
c 
c     copyright c 1981 the board of trustees of the leland stanford 
c                      junior university. all rights reserved.
c     this subroutine may not be used outside of the pisces computer
c     program without the prior written consent of stanford university. 
c 
c 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 
c                   type dec
c 
c -- PARAMETERS
      integer key
      character*(*) itime
      real elaptm 
c
c -- LOCAL
      real temp
      character*20 itemp
c 
c****************************************************************** 

c 
c     operation that is neccessary?
      goto (100,200,300),key
c       
c        Get the time of day: "23:04:56"
100   call xtime(itime) 
      goto 400
c
c        Compute the elapsed time since the last call.
200   call xtimer(temp)
      elaptm = temp - elaptm
c        Put temp into itime 
      write (itime,500) elaptm
500   format (f20.2)
      goto 400
c
c        Get the julian date and time and put into itime: "Sep 04, 1988"
300   call xdate(itime)
      call xtime(itemp)
      itime(10:10) = ' '
      itime(11:20) = itemp(1:10)
400   return
      end 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      SUBROUTINE TIMER(i,lu,str,elap)
      include 'p2conf.h'
c
c......Convenience routine to print the time since it was last called
      include     'blank.h'
c
      integer i,lu
      character*(*) str
      character*20 itim
      real elap
c********************* Start *******************************************
c
c.......If cpu profile is turned off, bag it.
      if  (.not.lcpu) return

      if (i.eq.1) then
        elap = 0.
        call xclock(2,itim,elap)
      else
        call xclock(2,itim,elap)
        write(lu,10) str,elap
      endif
10    format(a35,f12.2)
      return
      end
