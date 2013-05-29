.EQ
delim @@
gsize 11
.EN
.bC PRINT
.NH  0
Syntax
.sp 2
.R
.in +4
PRint <location> <quantity> <flags>
.in -4
.sp
.NH 
Description
.IP 
The PRINT card prints specific quantities at points within a defined
area of the device.
.NH
Parameters
.sp 2
.RS
.IP "<location>"
.sp 1
.in +4
.TS
l l l.
X.MIn	\\=	<real>
X.MAx	\\=	<real>
Y.MIn	\\=	<real>
Y.MAx	\\=	<real>
.TE
.in -4
or
.in +4
.TS
l l l.
IX.Low	\\=	<integer>
IX.High	\\=	<integer>
IY.Low	\\=	<integer>
IY.High	\\=	<integer>
.TE
.in -4
The above parameters define area in which the points of interest
lie.  X.MIN, X.MAX, Y.MIN and Y.MAX specify an area in physical
coordinates (in \(*mm).  IX.LOW, IX.HIGH, IY.LOW and IY.HIGH
specify the area by the bounding indices (valid only for rectangular
meshes).  The default area is the entire device.
.sp +2
.KS
.IP "<quantity>"
.sp 
.in +4
.TS
l l l.
POints	\\=	<logical>
Elements	\\=	<logical>
Geometry	\\=	<logical>
Solution	\\=	<logical>
P.SOL1	\\=	<logical>
P.SOL2	\\=	<logical>
Current	\\=	<logical>
P.CURR1	\\=	<logical>
P.CURR2	\\=	<logical>
Que	\\=	<logical>
P.QUE1	\\=	<logical>
P.QUE2	\\=	<logical>
Material	\\=	<logical>
.TE
.in -4
The above parameters specify the quantities to be plotted. 
Any or all may be specified, and each defaults to false.
POINTS prints node information (coordinates, doping,
etc.).  ELEMENTS prints information on the triangular elements
(number, nodes, material).  GEOMETRY prints geometrical information
on the triangles.  SOLUTION prints the \fIpresent\fR solution 
( @ psi , n , p @ and quasi-fermi potentials),
while P.SOL1 and P.SOL2 print the \fIprevious\fR two solutions.  CURRENT
prints currents (electron, hole, conduction, displacement and total)
at each node for the present solution; P.CURR1 and P.CURR2 print 
currents for previous solutions.  QUE prints space charge,
recombination and electric field for the present solution;
P.QUE1 and P.QUE print the same quantities for the previous 
two solutions.  MATERIAL prints
material information (permittivity, band-gap, etc.), including
the value of the concentration dependent mobility and lifetime
(if specified) at each point.
.sp 2
.KE
.sp +2
.KS
.IP "<flags>"
.sp 
.in +4
.TS
l l l l.
X.Component	\\=	<logical>	(default is false)
Y.Component	\\=	<logical>	(default is false)
.TE
.in -4
X.COMPONENT and Y.COMPONENT specify how any of the 
various vector quantities (currents, fields) should be printed.
The default is the magnitude of the vector as a whole.
X.COMPONENT specifies that the magnitude of the x-component
of all vectors be printed, while Y.COMPONENT specifies
the y-component.  Only one of these (or neither) can
be specified on a single card.
.sp 2
.KE
.RE
.KS
.NH
Examples
.IP
.sp 2
The following prints the physical coordinates, doping and 
region/electrode information for points
along the 10th x grid line, from the 1st to the 20th
y grid lines.
.sp
.in +4
.ss 24
PRINT  POINTS IX.LO=10 IX.HI=10 IY.LO=1 IY.HI=20
.in -4
.sp 2
In the next example, solution information is printed for 
.br
0 < x < 1\(*mm and 0 < y < 2\(*mm.
.sp
.in +4
.ss 24
.nf
PRINT  SOLUTION X.MIN=0 X.MAX=1 Y.MIN=0 Y.MAX=2
.KE
.fi
.eC
