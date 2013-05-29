.\" Wed Sep 13 15:28:10 PDT 1989 (anderson--stanford)
.EQ
delim @@
.EN
.bC LOAD
.NH  0
Syntax
.sp 2
.R
.in +4
Load  <solution files>
.in -4
.sp
.NH 
Description
.IP 
The LOAD card loads previous solutions from files for plotting or
as initial guesses to other bias points.
.NH
Parameters
.sp 2
.RS
.KS
.IP "<solution files>"
.sp 1
.in +4
.TS
l l l l.
INFile (or IN1file)	\\=	<filename>
IN2file	\\=	<filename>
Outdiff	\\=	<filename>
Differ	\\=	<logical>	(default is false)
Ascii	\\=	<logical>	(default is false)
No.check	\\=	<logical>	(default is false)
.TE
.in -4
The INFILE (or IN1FILE) and IN2FILE parameters specify input 
files names for solution data and may be up to 20 characters in length. 
INFILE (or IN1FILE) and IN2FILE represent a present and previous
solution respectively.
If only one solution is to be loaded (for plotting or as a single
initial guess using the PREVIOUS option on the SOLVE card) then
INFILE should be used.  If two input files are needed to perform
an extrapolation for an initial guess (i.e., the PROJECT option
on the SOLVE card), IN1FILE and IN2FILE should be used.  The solution
in IN2FILE is the first to be lost when new solutions are obtained.
The difference between two solutions (IN1FILE-IN2FILE) can be 
analyzed by reading in both with the mode DIFFER set. 
The difference is stored;
this solution may not be used as an initial guess, or for any
purpose other than plotting or extracting data. The difference
solution may also be stored in another file using the
parameter OUTDIFF.
ASCII specifies that any files read or written by this card
should be ascii rather than binary.  NO.CHECK prevents
\*(PI from checking material parameter differences between 
the loaded files and the values set in the current \*(PI
input file.
.+B 8940
Checking is never done for ascii solution files.
.-B 8940
.RE
.sp
.KE
.NH
Examples
.IP
.sp 2
The following specifies that a single solution file called 
SOL.IN should be loaded.
.sp
.in +4
.ss 24
LOAD  INF=SOL.IN
.in -4
.sp 2
In the next example, two solutions are loaded.  The present
solution is to SOL1.IN and the previous solution is SOL2.IN.
We intend to use SOL1.IN and SOL2.IN to project an initial guess
for a third bias point.
.sp
.in +4
.ss 24
.nf
LOAD IN1F=SOL1.IN IN2F=SOL2.IN
.fi
.sp 2
.in -4
Finally, two solutions are loaded, and the difference calculated
and stored in a third file.
.sp
.in +4
.ss 24
.nf
LOAD IN1F=SOL1.IN IN2F=SOL2.IN DIFF OUTD=SOL1-2
.eC
