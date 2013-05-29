cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1988 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c =======================================================================
c "XGTARG.F": Get the command line arguments from UNIX.
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
	integer iargc
c
	xargc = iargc()
	return
	end

c =======================================================================
	SUBROUTINE XGTARG(NTH, BUF)
	integer		nth
	character*(*)	buf
$ALIAS getarg = 'Ftn_getarg' (%ref, %ref, %ref)
c
	call getarg(nth, buf, LEN(buf))
	return
	end
