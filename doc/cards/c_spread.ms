.EQ
delim @@
.EN
.bC SPREAD
.NH  0
Syntax
.sp 2
.R
.in +4
SPread <direction> <region> <specifics>
.in -4
.sp
.NH 
Description
.IP 
The SPREAD card provides a way to distort rectangular grids in
the vertical direction to follow
surface and junction contours.  SPREAD is very useful in 
reducing the amount of grid for some specific problems, 
most notably MOSFET's.  The SPREAD card is somewhat complicated;
it is suggested to follow the supplied examples very carefully
(particularly the MOSFET example in Chapter 5 in [1]).
.NH
Parameters
.sp 2
.RS
.IP "<direction>"
.sp 2
One of:
.sp
.in +4
.TS
l l l l.
LEft	\\=	<logical>	(default is false)
Right	\\=	<logical>	(default is false)
.TE
.in -4
LEFT and RIGHT specify that the left and right-hand sides of the grid
respectively be distorted. 
.sp +2
.IP "<region>"
.sp 
.in +4
.TS
l l l.
Width    	\\=	<real>
Upper    	\\=	<integer>
LOwer    	\\=	<integer>
.TE
.in -4
WIDTH specifies the width from the left or right edge (depending on
the LEFT and RIGHT parameters) of the distorted area.  The actual
x-coordinate specified by WIDTH (min[x] + WIDTH for LEFT,
max[x] - WIDTH for RIGHT) will lie in the middle of the transition
region between the distorted and undistorted grid regions.  UPPER and 
LOWER specify the upper and lower y-grid lines between which the
distortion will take place.  
.sp 2
.KS
.IP "<specifics>"
.sp 2
One of:
.in +4
.TS
l l l.
Y.Lower  	\\=	<real>
Thickness	\\=	<real>
.TE 
.in -4
And:
.in +4
.TS
l l l l.
Vol.ratio	\\=	<real>	(default is 0.44)
Encroach	\\=	<real>	(default is 1.0)
GRAding	\\=	<real>	(default is 1.0)
GR1	\\=	<real>	(default is 1.0)
GR2	\\=	<real>	(default is 1.0)
Middle	\\=	<real>	
Y.Middle	\\=	<real>	
.TE
.in -4
The Y.LOWER and THICKNESS parameters define the distorted 
grid region; only
one should be supplied.  Y.LOWER is the physical location
in the distorted region at which the line specified by LOWER
will be moved.  The line specified by UPPER is not moved.
THICKNESS is the thickness of
the distorted region; THICKNESS will usually move the positions of
both the UPPER and LOWER grid lines (unless VOL.RATIO is set to
0 or 1).  VOL.RATIO specifies the ratio of the 
downward displacement of the lower grid line to the net increase
in thickness.  The default is 0.44 so that oxide-silicon interfaces
are correct.  VOL.RATIO is ignored if Y.LOWER is specified.
ENCROACH is a factor which defines the abruptness of the transition
between distorted and non-distorted grid.  The transition region
becomes more abrupt with smaller ENROACH factors (the minimum is 0.1).  
An important note:
depending on the characteristics of the undistorted grid, very
bad triangles (long, thin and obtuse) may result if ENCROACH
is set too low.  GRADING specifies
a grid ratio (identical to the RATIO parameter on the X.MESH and
Y.MESH cards) to produce a non-uniform grid in the distorted region.
As alternative to a single grading parameter, GR1 and GR2 can be
specified along with the y grid line MIDDLE and location Y.MIDDLE 
so that GR1 is used as the
grading in the spread region from UPPER to MIDDLE and GR2 is
the grading from MIDDLE to LOWER.
.sp 2
.KE
.RE
.KS
.NH
Examples
.sp 2
.IP
The following spreads what was previously a uniform 400 Angstroms of
oxide to 1000 Angstroms on the left side of the device.  This will 
result in a net increase in thickness of 600 Angstroms of oxide.
Because the default VOL.RATIO is used, 0.44*(600) = 264 Angstroms
of the net increase will lie below the original 400 Angstroms and
0.56*(600) = 336 Angstroms of the net increase will lie above the 
original 400 Angstroms.  The width of the spread region is 0.5 \(*mm
and the oxide taper is quite gradual because of the high encroachment
factor.  The grid is left uniform in the spread region.
.sp
.in +4
.nf
$ *** Mesh definition ***
.br
.ss 24
.nf
MESH    NX=30 NY=20 RECT 
.br
.ss 24
.nf
X.M     N=1  L=0
.br
.ss 24
.nf
X.M     N=30 L=2
.br
.ss 24
.nf
Y.M     N=1  L=-.04
.br
.ss 24
.nf
Y.M     N=5  L=0
.br
.ss 24
.nf
Y.M     N=20 L=1 R=1.4
.sp
.nf
$ *** Thin oxide ***
.br
.ss 24
.nf
REGION  X.L=1 X.H=30 Y.L=1 Y.H=5
.sp
.nf
$ *** Silicon substrate ***
.br
.ss 24
.nf
REGION  X.L=1 X.H=30 Y.L=5 Y.H=20
.sp
.nf
$ *** Spread ***
.ss 24
.nf
SPREAD  LEFT WIDTH=0.5 UP=1 LO=5 THICK=0.1 ENC=1.3
.in -4
.KE
.KS
.sp 2
In the next example, the right side of the grid is distorted in order
to follow a junction contour.  Assume that the initial grid is defined
as above.  Y.LOWER is used so that there is no 
increase in the size of the device, just grid redistribution.  With
Y.LOWER set to the junction, the ENCROACH parameter should be chosen 
such that the lower grid line (LOWER=10) follows the junction as closely
as possible.  Note that the grid is graded so that the grid lines are
spaced closer together as they approach the junction.  Because the
point specified by WIDTH on the spread card lies in the middle of the
transition region, it should be chosen to be slightly larger than 
the width of the doping ``box'' (WIDTH < X.LEFT - X.RIGHT = 0.5 \(*mm).
.sp
.in +4
.nf
$ *** Doping ***
.ss 24
.nf
DOPING  UNIFORM N.TYPE CONC=1E15
.ss 24
.nf
DOPING  GAUSS P.TYPE X.LEFT=1.5 X.RIGHT=2
.ss 24
.nf
+       PEAK=0 CONC=1e19 RATIO=.75 JUNC=0.3
.sp
.nf
$ *** Spread ***
.ss 24
.nf
SPREAD  RIGHT WIDTH=0.7 UP=5 LO=10 Y.LO=0.3 
.ss 24
.nf
+       ENC=1.2 GRAD=0.7
.KE
.fi
.eC
