.\" Tue Sep 20 11:00:36 PDT 1988 (dredge--Stanford)
.EQ
delim @@
.EN
.bC PLOT.2D
.NH  0
Syntax
.sp 2
.R
.in +4
PLOT.2d <area definition> <plotted quantity> <control>
.in -4
.sp
.NH 
Description
.IP 
The PLOT.2D card plots quantities in a specified two-dimensional 
area of the device.  A PLOT.2D card is required before performing
a contour plot (see CONTOUR card) in order to obtain the plot
boundaries.
.NH
Parameters
.sp 2
.RS
.IP "<area definition>"
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
The above parameters define the rectangular area of the device to
be plotted.  The default area is a rectangle around the
entire device.
.sp +2
.IP "<plotted quantity>"
.sp
.in +4
.TS
l l l l.
Grid or Mesh	\\=	<logical>	(default is false)
Crosses	\\=	<logical>	(default is false)
Boundary	\\=	<logical>	(default is false)
Depl.edg	\\=	<logical>	(default is false)
Junction	\\=	<logical>	(default is false)
.TE
.in -4
The GRID option plots the grid, including lines delineating elements.  
CROSSES plots crosses at the locations of grid points.  
BOUNDARY indicates that the boundaries
around the device and between regions are to be plotted.  
DEPL.EDG indicates that
depletion edges are to be plotted (note: depletion edges can only be
plotted after a solution is present).  The JUNCTION option specifies
that the junctions from the doping profiles are to be plotted.
.sp 2
.KS
.IP "<control>"
.sp
.in +4
.TS
l l l l.
NO.TIc	\\=	<logical>	(default is false)
NO.TOp	\\=	<logical>	(default is false)
NO.Fill	\\=	<logical>	(default is false)
NO.Clear	\\=	<logical>	(default is false)
LAbels	\\=	<logical>	(default is false)
Flip.x	\\=	<logical>	(default is false)
Pause	\\=	<logical>	(default is false)
L.Elect	\\=	<integer>	
L.Deple	\\=	<integer>	
L.Junct	\\=	<integer>	
L.Bound	\\=	<integer>	
L.Grid	\\=	<integer>	
.+B
Outfile	\\=	<character>	(default from OPTION card)
.-B
.TE
.in -4
NO.TIC indicates
that tic marks are not to be included around the plotted area.
NO.TOP indicates that tic marks are not to be put on the top
of the plotted region.  The NO.FILL option will force \*(PI to
draw the device area plotted to scale; if this option is not
specified, the plot will fill the screen, and the triangles
will appear distorted.  NO.CLEAR specifies
that the screen is not to be cleared before plotting.  
LABELS makes room for color contour labels on the right side of
the plot device.
FLIP.X flips the plot about the y-axis; i.e., it negates all
x coordinates so that the plot is mirrored.
The PAUSE option causes \*(PI to stop at the end of the plot so that a
hardcopy may be made before continuing.  Execution can be resumed
by hitting a carriage return.  
L.ELECT, L.DEPLE, L.JUNCT, L.BOUND and L.GRID set line types for
electrodes, depletion edges, junctions, region boundaries and
grid, respectively.
.+B
The default plot device is generally the user's
terminal but may be reset with the OPTION card.
If OUTFILE is specified, the graphics output will be directed
to that file.  For further discussion, see the OPTION card.
.-B
.\" OUTFILE generates a binary plotfile
.\" which can be read by the dplot system program (only at Stanford).
.sp 2
.KE
.RE
.KS
.NH
Examples
.IP
.sp 2
The following plots the entire grid to scale with tic marks:
.sp
.in +4
.ss 24
PLOT.2D  GRID NO.FILL
.in -4
.sp 2
In the next example, the device and region boundaries, junctions and
depletion edges are plotted in the rectangular area bounded by
@ 0 < x < 5 mu @ and @ 0 < y < 10 mu @.  The plot is allowed 
to fill the screen and tic marks are not included along 
the top of the plot.
.sp
.in +4
.ss 24
.nf
PLOT.2D  X.MIN=0 X.MAX=5 Y.MIN=0 Y.MAX=10 
.br
.ss 24 
.nf
+        JUNCT BOUND DEPL NO.TOP 
.KE
.fi
.eC
