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


C ----------------------------------------------------------------------------
	INTEGER FUNCTION XARGC(idummy)
	integer idummy
c
	integer		argc
	character*1024	argb
	common/argsco/ argc, argb
c
	integer	i
	character*256 temp
c
c   ..KK, 19 July 1989
      INTEGER LIB$GET_FOREIGN

C...get command line from VMS.....
	i = LIB$GET_FOREIGN(argb)

c... clean up
	do 100 i = 1, LEN(argb)
	  if (argb(i:i) .eq. '	') argb(i:i) = ' '
100	continue

c...count by getting until no more....
 	do 200 i = 1, 10000
c     ..KK, 7/19/89.  getarg()-->xgtarg()
	  call xgtarg(i, temp)
	  if (temp(1:1) .eq. ' ') goto 201
200	continue

201	continue
	argc = i - 1
	xargc = argc
	return
	end

C ----------------------------------------------------------------------------
	SUBROUTINE XGTARG(NTH, BUF)
	integer		nth
	character*(*)	buf
c
	integer		argc
	character*1024	argb
	common/argsco/ argc, argb
c
	integer		ib, ie, i
c
	ib = 1
	buf = ' '
	if (nth.le.0) return
c
c...Look for the Nth argument....
	do 100 i = 1, nth
	  call nxtarg(argb, ib, ie)
	  if (ib .le. 0 .or. i.eq.nth) goto 101
	  ib = ie + 1
100	continue

101	continue
	if (ib .gt. 0) buf = argb(ib:ie)
	return
	end

C ----------------------------------------------------------------------------
	subroutine nxtarg(buf, ib, ie)
	character*(*)	buf
	integer		ib, ie
c
	if (ib.le.0 .or. ib.gt.LEN(buf)) then
	  ib = -1
	  return
	endif
c
100	if (ib.le.LEN(buf) .and. buf(ib:ib).eq.' ') then
	  ib = ib + 1
	  goto 100
	endif
c
	if (ib.gt.LEN(buf)) then
	  ib = -1
	  return
	endif
c
	ie = ib
200	if (ie.le.LEN(buf) .and. buf(ie:ie).ne.' ') then
	  ie = ie + 1
	  goto 200
	endif

	return
	end
