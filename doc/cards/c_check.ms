.EQ
delim @@
.EN
.bC CHECK
.NH  0
Syntax
.sp 2
.R
.in +4
CHeck <file specification>
.in -4
.sp 2
.NH 
Description
.IP
The CHECK card compares a specified solution against the current 
solution, returning the maximum and average
difference in electrostatic and quasi-fermi potentials.
The check card is particularly useful for comparing solutions that
have been obtained on different generations of regrids.
.NH 
Parameters
.RS
.IP "<file specification>"
.TS
l l l l.
Infile	\\=	<filename>
Mesh	\\=	<filename>
Samemesh	\\=	<logical>	(default is false)
.TE
.LP
INFILE specifies the name of the solution file to compare.  MESH is
the name of the file containing the mesh for that solution.
SAMEMESH indicates the the solution in INFILE used the same mesh
as the current solution.
.sp
.RE
.NH
Examples
.IP
Compare solution ``sol1'' obtained using mesh ``mesh1'' against the 
current solution.
.sp 2
.in +4
.ss 24
.nf
CHECK  INFILE=sol1 MESH=mesh1
.fi
.eC
