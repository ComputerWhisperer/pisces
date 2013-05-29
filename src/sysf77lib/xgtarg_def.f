cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c =======================================================================
c "XGTARG.F": Get the command line arguments when there is no other
c	way.  THIS IS VERY NON-ELEGANT; just prompt the user.
c
c Routines:
c
c INTEGER FUNCTION XARGC(idummy)		-- call FIRST!
c SUBROUTINE       XGTARG(nth, buf)
c
c There are three cases for getting the command line args.
c 1. There is a system supplied call to get the n'th argument, just
c	call that routine.
c 2. There is a way to get the entire (unparsed) command line.
c	Look at xgtarg_vms.f -- copy it and changed the get command
c	line call.
c 3. There is NO way to get the command line.  Use xgtarg_def.f -- this
c	will simply prompt the user for the arguments interactively.
c
c  Original: Michael Eldredge -- Stanford University (may 88)
c
c -----------------------------------------------------------------------


c =======================================================================
       INTEGER FUNCTION XARGC(idummy)
       integer idummy
c
c ---- Common ----
	integer MAXARG
	parameter (MAXARG=8)
	integer argc
	character*(80) argv(MAXARG)
	common /xargcz/ argc, argv

c ---- Local ----
	integer i
	character*(80) buf

	argc = 0
	do 20 i = 1, MAXARG
	  write(*, 11) i
11	  format(' Enter argument ',i2,'("." when done): ', $ )
	  read(*, 13) buf
13        format(a)
c
	  if (buf(1:2).eq.'. ') goto 30
	  argv(i) = buf
	  argc = argc + 1
20      continue
c
30	xargc = argc
	return
	end

c =======================================================================
	SUBROUTINE XGTARG(NTH, BUF)
	integer		nth
	character*(*)	buf
c
c ---- Common ----
	integer MAXARG
	parameter (MAXARG=8)
	integer argc
	character*(80) argv(MAXARG)
	common /xargcz/ argc, argv

	if (nth.gt.0 .and. nth.le.argc) then
	   buf = argv(nth)
	else
	   buf = ' '
	endif
	return
	end
