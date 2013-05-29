cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     Copyright c 1990 The Board of Trustees of the Leland Stanford
c     Junior University. All rights reserved.  This routine may not
c     be used without the prior written consent of Stanford University.
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c Sun Sep  2 21:54:00 PDT 1990 (anderson--stanford)
c
c Configuration Parameters for the PISCES-II program.
c
c
c GLOBAL (possibly) system dependent parameters
c
c
c BASIC SIZES USED THROUGHOUT PISCES
c
         integer MAXPT,  MAXTRI, MAXTRI2
	 integer MAXEQN, MAXADJ, MAXLU, MAXCON
         integer MAXPT1, MAXPT2, MAXCNT, MAXNB, MAXPAR
	 integer MAXLU2, MAXEQ2
	 integer MAXIA,  MAXILU
	 integer MAXNEPB, MAXINF, MAXREG
c
c.. number of points
      parameter (MAXPT = 3000)
c
c.. maximum number of contact nodes
      parameter (MAXCON = 300)
c.. maximum number of contacts
      parameter (MAXCNT = 10)
c.. maximum number of triangles a point can belong to
      parameter (MAXNB = 20)
c.. maximum number material parameters
      parameter (MAXPAR = 50)
c
c.. maximum number of interfaces
      parameter (MAXINF = 10)
c.. maximum number of regions
      parameter (MAXREG =  8)
c.. number of points + 1
      parameter (MAXPT1 = MAXPT+1)
c.. number of points * 2 +1
      parameter (MAXPT2 = 2*MAXPT+1)
c
c.. number of triangles and 2* number of triangles
      parameter (MAXTRI  = 2*MAXPT)
      parameter (MAXTRI2 = 2*MAXTRI)
c
c.. number of elements plus boundary elemts
      parameter (MAXNEPB = MAXPT+MAXTRI)
c
c
c.. number of equations 3 * maxpt
      parameter (MAXEQN = 3*MAXPT)
c.. 2*number of equations
      parameter (MAXEQ2 = 2*MAXEQN)
c
c
c.. maximum size of the a matrix
      parameter (MAXADJ = 30*MAXPT+10000)
c.. maximum size of the lu matrix
      parameter (MAXLU = MAXPT*MAXPT/40)
c.. 2*size of lu maxtrix
      parameter (MAXLU2 = 2*MAXLU)
c
c
c.. SYMBOLIC CONSTANTS
c
c.. maximum for ia
      parameter (MAXIA = 20*MAXPT)
c.. maximum for il and iu
      parameter (MAXILU = MAXLU/4)
c.. maximum number of contact nodes
c
c
c
c GUMMEL RELATED PARAMETERS
c
c.. max size of A for gummel and max size for L and U for Gummel
	 integer MAXAG,MAXLUG
      parameter (MAXAG = 6*MAXPT)
      parameter (MAXLUG= 4*MAXAG)
c
c MORE SPECIALIZED VALUES
c
c
c.. maxium elements for temporary grid info for rectangular grids
c... used in mshtmp.h
	 integer MAXREC
      parameter (MAXREC = 400)
c
c.. max values for reading 1d doping profiles (used in doptmp.h)
	 integer MAXSUP
      parameter (MAXSUP = 500)
c
c.. max values for reading 2d doping profiles (used in doptmp.h)
	 integer MAX2DOP
      parameter (MAX2DOP = 25000)
c
c.. max plot buffer size
	 integer MAXPLT
      parameter (MAXPLT = 1000)
c
c.. max map size (in symtmp1.h)
	 integer MAXMAP
      parameter (MAXMAP = 30*MAXPT)
c
c..     symtmp3.h:
c.. work arrays tv, tl
	 integer MAXIY
      parameter (MAXIY = MAXADJ + 1)
c
c.. sizes for cja and related arrays in symtmp3.h, symtmp4.h
	 integer MAXCJA
      parameter (MAXCJA = 90000)
c
c
c
c GENII will need a KeyId for include files and a max include
c   depth
         integer INCKEY,NINCLS
      parameter (INCKEY=1000)
      parameter (NINCLS=4)
