cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Fri Aug 25 09:47:37 PDT 1989 (dredge--stanford)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c =======================================================================
c "XTEST": Test all of the X____ routines outside of the BIG programs.
c	This simply calls each of the system dependent cover functions
c	"x-routines" and prints the results.
c
c  There are 2 ways to run the test program.  The first method
c  is that if there exists a script (named "runtst*") that works
c  for your system, use it.
c           In unix use the shell script:
c               runtst.sh
c           In vms use the com script:
c               runtst_vms.com
c
c  The scripts all perform 3 functions that you must do by hand if
c  there is no appropriate script.
c
c        1. set the enviroment variable MATCH to something.
c           In unix CSH:
c               setenv MATCH something
c           In unix SH or KSH:
c               MATCH=something ; export MATCH
c           In VMS:
c               define/logical MATCH something
c        2. Run the program with four parameters as:
c                xtest aaaa bbb cc d
c           actually, any variables can be given, but the test
c           will look for and comment on the above.
c        3  When the test program completes, examine the return
c           status.  It will return and OK status (0 for Unix,
c           1 for VMS, etc....) regardless of the results of the
c           other tests (since this is a test also).
c           In unix CSH:
c                echo $status
c           In unix SH or KSH:
c                echo $?
c
c -----------------------------------------------------------------------
       PROGRAM XTEST
c -----------------------------------------------------------------------
c
c -- CONSTANTS
       integer LUTMP
       parameter (LUTMP=12)
c
c -- LOCAL variables
       integer   i,j, junk, ierr
       integer   nargs
       character*20 argstr
       character*10 datstr
       character*12 timstr
       character*12 keystr
       character*40 valstr
       character*3  result
       character*40 tmpstr
       integer      tmplu
       integer      trys
       real         xval
       logical      fail
       real         x
       integer      nerrs
c
c -- FUNCTIONS
       integer xargc

c -----------------------------------------------------------------------

      tmplu = lutmp
      nerrs = 0


c.. XARGC
1     format(/1x,'************ ',a,' ************')
      write(*,1) 'XGTARG XARGC'

      fail=.false.
      nargs = xargc(junk)
      write(*, 101) nargs
101   format(1x,'The program was called with ', i2,' arguments:')
      if (nargs.le.0) goto 140
      do 120 i = 1,nargs
	 call xgtarg(i, argstr)
	 write(*, 111) i, argstr
111      format(1x,'    ',i3,': ',a)
c........STANDARD TESTS ....
      if (i.eq.1 .and. argstr.ne.'aaaa') fail=.true.
      if (i.eq.2 .and. argstr.ne.'bbb')  fail=.true.
      if (i.eq.3 .and. argstr.ne.'cc')   fail=.true.
      if (i.eq.4 .and. argstr.ne.'d')    fail=.true.
120   continue
140   continue
      if (fail .or. nargs.ne.4) then
      write(*,1001)
1001  format(' FAILED TEST: expected the 4 arguments: aaaa bbb cc d')
      nerrs = nerrs + 1
      endif

      call xgtarg(nargs+5, argstr)
      write(*, 151)
151   format(1x,'Get an invalid argument, should be blank:')
      if (argstr(1:1).eq.' ') then
	 result = 'OK '
      else
	 result = 'BAD'
	 nerrs = nerrs + 1
      endif
      write(*, 153) argstr, result
153   format(1x,4x,'We got: ',a,/1x,8x,a)


c.. XDATE
      write(*,1) 'XDATE'

      write(*, 301) '"01 Jan 88"'
301   format(1x,
     + 'Date string should look something like (9 chars max): ',a)
      call xdate(datstr)
      write(*, 305) datstr
305   format(1x,4x,'[123456789 123456789 chars...]',/,1x,4x,' ',a)
      if (datstr(10:10).ne.' ') then
	 write(*, 1002)
1002     format(' FAILED TEST: greater than 9 chars in date string')
	 nerrs = nerrs + 1
      endif


c.. XTIME
      write(*,1) 'XTIME'

      write(*, 311) '"23:04:56"'
311   format(1x,
     + 'Time string should look something like (10 chars max): ',a)
      call xtime(timstr)
      write(*, 315) timstr
315   format(1x,4x,'[123456789 123456789 chars...]',/,1x,4x,' ',a)
      if (timstr(11:11).ne.' ') then
	 write(*, 1003)
1003     format(' FAILED TEST: greater than 10 chars in time string')
	 nerrs = nerrs + 1
      endif


c.. XTIMER
      write(*,1) 'XTIMER'

      write(*, 321)
321   format(1x, 'Test the CPU seconds timer.  Call it a few times.')
      i = 0
      call xtimer(xval)
      write(*, 323) i, xval
323   format(1x,4x,'xtimer(',i2,') = ', f12.4)
      x = xval

      trys = 5
      do 330 i = 1, trys
c....waste some time...
         do 324 j = 1,5000
	    xval = sqrt(FLOAT(2)*3.14/6.0/FLOAT(j)) * sin(FLOAT(j)/3.14)
            if (xval.gt.0) xval = (FLOAT(j*44)/.623)
324      continue

         call xtimer(xval)
         write(*, 323) i,xval
	 if (xval.le.x) then
	   write(*, 1007)
1007       format(' FAILED TEST: cpu time did not increase')
	   nerrs = nerrs + 1
         endif
	 x = xval
330   continue



c.. XGETEN
      write(*,1) 'XGETEN'

      write(*, 501)
501   format(1x,'Expecting MATCH to be defined (if not define it).')
      write(*, 502)
502   format( 1x,'Expecting OOPS to NOT be defined!'/)

      keystr = 'MATCH'
      call xgeten(keystr, valstr)
      if (valstr(1:1).eq.' ') then
	 result = 'BAD'
	 nerrs = nerrs + 1
      else
	 result = 'OK '
      endif
      write(*, 511) valstr, result
511   format(1x,4x,'xgeten(keystr, valstr): valstr = ',a,/1x,8x,a)

      call xgeten('MATCH', valstr)
      if (valstr(1:1).eq.' ') then
	 result = 'BAD'
	 nerrs = nerrs + 1
      else
	 result = 'OK '
      endif
      write(*, 513) valstr, result
513   format(1x,4x,'xgeten("MATCH",valstr): valstr = ',a,/1x,8x,a)


      keystr = 'OOPS'
      call xgeten(keystr, valstr)
      if (valstr(1:1).eq.' ') then
	 result = 'OK '
      else
	 result = 'BAD'
	 nerrs = nerrs + 1
      endif
      write(*, 515) valstr, result
515   format(1x,4x,'xgeten(keystr, valstr): valstr = ',a,/1x,8x,a)

      call xgeten('OOPS', valstr)
      if (valstr(1:1).eq.' ') then
	 result = 'OK '
      else
	 result = 'BAD'
	 nerrs = nerrs + 1
      endif
      write(*, 517) valstr, result
517   format(1x,4x,'xgeten("OOPS", valstr): valstr = ',a,/1x,8x,a)


c.. XMKTMP
      write(*,1) 'XMKTMP'

      trys = 3
      write(*, 801) trys
801   format(1x,
     + 'Now try to make some temp file names.  Try ',i2,' times.')
 
      do 890 i = 1, trys
        call xmktmp(tmpstr, LEN(tmpstr), 'xtst', 4)
        write(*, 805) i, tmpstr
805     format(1x,'Try #',i2,'; Got the name: ',a)
        open(tmplu, file=tmpstr,
     +    form='formatted',status='new',err=819,iostat=ierr)
        write(tmplu, 811) tmpstr
811     format(1x,4x,'File name: ',a)
        write(*, 813)
813     format(1x,4x,'OPEN (new named did not conflict with old).')
        close(tmplu)
	goto 890

c.....error on open
819     write(*, 821) ierr
821     format(1x,4x,'FALIED TEST: error on open #',i7,'.')
        write(*, 1004)
1004    format(' ... maybe the new name is the same as the old?')
	nerrs = nerrs + 1
C       goto 890

890   continue


c.. ---done (except for XEXIT)---
      write(*, 1009)
1009  format(//1x,'================================================')
      write(*, 901) nerrs
901   format(//1x,'    ====> All internal tests finished, ',
     +   i4,' errors <====')
      write(*, 1005)
1005  format(/ 1x,'    (Do not forget to check the exit status)')


c.. XEXIT (use will have to check when we finish)
c.....0::ok and 1::error (xexit will map to system ok and err)
      call xexit(0)

      end
