cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Wed Feb 22 11:04:53 PST 1989 (dredge--stanford)
c
      PROGRAM PISC2
c
c-----------------------------------------------------------------------
c
c  The following is the main procedure calling routine in the PISCES
c  modeling program.  The main effort in porting this routine should
c  be in getting the command line arguments.  If there is no way to
c  get the command line, one could simply prompt for an input file
c  name.....
c
c
c          ****** PISCES-IIB *******
c
c  Original : C.H.Price     Stanford University          May, 1982
c  Revision : MRP           Stanford University          Nov, 1983
c  Revision : DYC           Monte Carlo                  Jun, 1986
c  Modified : Michael Eldredge -- Stanford               Oct, 1987
c       Added new GENII.  Re Org.  Split pisci into pisc2, pisces
c  Modified : MJE -- Stanford                            Sep, 1988
c       Get arguments with the general xgtarg() and xargc()
c       routines (these can be writen for system depends).
c  Modified : MJE -- Stanford                            Feb, 1989
c       Exit through the routine xexit() -- a system independent
c       way of returning an exit status.  Call the new genii
c       genipm() (GENIi ProMpt) to go into interactive,non-batch
c       genii mode.
c
c  Copyright c 1981 The board of trustees of the Leland Stanford 
c                   Junior University. All rights reserved.
c  This subroutine may not be used outside of the PISCES computer
c  program without the prior written consent of Stanford University. 
c
c-----------------------------------------------------------------------
c 
c                   common areas
c
c             NO COMMON IN MAIN ROUTINE
c------------------------------------------------------------------
c 
c -- CONSTANTS
c.......LUODEF is if there is an output file; else lutto will
c........be used.
      integer LUODEF
      parameter (LUODEF=3)
c
c -- DEFINTIONS
      integer numarg,ierr,i
      character*40 outfil
      character*60 keyfil, ukyfil
      character*40 infile
      integer      luout,lutto
      logical haderr
c..primary and secondary prompts.
      character*8  p2prm1
      character*12 p2prm2
c
c -- FUNCTIONS
      integer  xargc
c
c -- DATA
      data luout/3/
      data lutto/6/
c..................123456789 12345
      data p2prm1/'PISCES: '/
      data p2prm2/'PISCES?     '/
      data keyfil/'pisc.key'/
      data ukyfil/'PISC2UKY'/
c...to xgeten(): set ukyfil to "PISC2UKY", else: to "<path>pisc.uky"

c 
c****************************************************************** 
c 
c...Trap floating point exceptions!
cc      call trpfpe(1,1d38)
c
c...In case there is no explicit output file.
      luout = lutto 

c...Get command line parameters (SYSTEM-dependent)
      numarg = xargc(i)
      if(numarg.lt.1) then
         infile=' '
c...setup for interactive mode; give genii some prompts to use.
         call genipm(p2prm1,LEN(p2prm1), p2prm2,LEN(p2prm2) )
      else
         call xgtarg(1, infile)
      endif
c
c...Check to see if an output file needs to be opened 
      if (numarg.gt.1) then 
        call xgtarg(2, outfil)
        luout = LUODEF 
        call fopcl(12,outfil,20,luout,.false.,ierr)
        if (ierr.ne.0) goto 9998
      endif 

c ==============================================================
c...And now we're off...
      call pisces(infile, luout, keyfil, ukyfil, haderr)
      if (luout.ne.lutto) call fopcl(0,outfil,20,luout,.true.,ierr)
      if (haderr) goto 9999
c
c ==============================================================
c...All finished. Good exit code.
      call xexit(0)
c
c...Error
c
9998  write(luout,1002) ierr
1002  format('Error number ',i5,' on the open of the output file')
c
9999  write(luout,1001)
1001  format(/'********* PISCES Aborted **********'/)
c
c......Bad Exit code.
      call xexit(1)
c
      end
