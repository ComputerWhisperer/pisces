.EQ
delim @@
.EN
.bC SYMBOLIC
.NH  0
Syntax
.sp 2
.R
.in +4
SYmbolic <solution method> <carriers> <options>
.in -4
.sp
.NH 
Description
.IP 
The symbolic card performs a symbolic factorization in preparation
for the LU decompositions in the solution phase of \*(PI.  Because
each of the available numerical solution techniques used by \*(PI
may result in entirely different linear systems, the method used
and the number of carriers to be simulated must be specified at this
time.  The symbolic factorization may be optionally read from or 
written to a file; if an input file is specified, the symbolic 
factorization information in that file must be consistent with 
the method specified on the present card. 
.NH
Parameters
.sp 2
.RS
.IP "<solution method>"
.sp 2
One of:
.in +4
.TS
l l l l.
Newton	\\=	<logical>	
.br
Gummel	\\=	<logical>	
.TE
.in -4
The solution method must always be specified, except for the
Poisson only case (carriers = 0). The different methods and
their applications are discussed in Chapter 3 of [1] and this
report.
.sp 2
.IP <carriers>
.br
.in +4
.TS
l l l l.
Carriers	\\=	<integer>	(default is 1)
Electrons	\\=	<logical>	(default is true)
Holes	\\=	<logical>	(default is false)
.TE
.in -4
.sp
The CARRIERS parameter specifies the number of carriers to be
simulated.  If only one carrier is to be simulated,  the specific
carrier can be specified by including either HOLES or ELECTRONS.
.sp
.KS
.IP "<options>"
.sp
.in +4
.TS
l l l l.
Min.degree	\\=	<logical>	(default is true)
Strip	\\=	<logical>	(default is true)
Infile	\\=	<character string>	
Outfile	\\=	<character string>	
Print	\\=	<logical>	(default is false)
.TE
.in -4
MIN.DEGREE uses a minimum degree ordering of the pivots for 
decomposition in order to reduce the size of the generated L and U
and hence, to reduce the amount of cpu spent in solving linear
systems.  This parameter is definitely recommended.
STRIP specifies that redundancy (zero couplings) be removed from
the symbolic map, and is naturally on.
The INFILE and OUTFILE parameters specify input/output files names
for the symbolic factorization.  The file names may contain up
to 20 characters. Note that these binary files can be quite large,
so it may be advisable not to use this feature. (In some computing
environments it is also faster to compute the symbolic information than
to read it from disk).
The PRINT parameter indicates that information
about the memory allocated for the run should be printed to the
\*(PI standard output file.
.sp 2
.KE
.RE
.NH
Examples
.IP
.sp 2
The following specifies a symbolic factorization for a simulation
with only holes and using the Gummel method (the
symbolic factorization is saved in a file called SYMB.OUT):
.sp
.in +4
.ss 24
SYMBOLIC  GUMMEL CARR=1 HOLES OUTF=SYMB.OUT
.in -4
.sp 2
In the next example, a previously generated symbolic factorization
is read in from a file called SYMB.IN.  The method used is the full
Newton method, and both carriers are included in the simulation.
Additionally, the sizes and dimensions of all arrays associated
with the sparse matrix solution are printed:
.sp
.in +4
.ss 24
.nf
SYMBOLIC  NEWTON CARR=2 INF=SYMB.IN PRINT
.fi
.eC
