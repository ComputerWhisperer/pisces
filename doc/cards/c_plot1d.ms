.EQ
delim @@
.EN
.bC PLOT.1D
.NH  0
Syntax
.sp 2
.R
.in +4
PLOT.1d  <line segment definition> <plotted quantity> <control>
.in -4
.sp
.NH 
Description
.IP
The PLOT.1D card plots a specific quantity along a line segment 
through the device (mode A), or plots an I-V curve of data (mode B).
.NH
Parameters
.sp 2
.RS
.IP "<line segment definition>"
.sp 1
.in +4
.TS
l l l.
X.Start or A.X	\\=	<real>
Y.Start or A.Y	\\=	<real>
X.End or B.X	\\=	<real>
Y.End or B.Y	\\=	<real>
.TE
.in -4
The above parameters define the Cartesian coordinates of the
start (A.X,A.Y) and the end (B.X,B.Y) 
of the line segment along which the specified quantity is to
be plotted.  The data is plotted as a function of distance
from the start (A). 
The line segment may not be defaulted. It is required in mode A.
.sp +2
.IP "<plotted quantity>"
.sp 2
One of:
.TS
l l l l.
POTential	\\=	<logical>	Mid-gap potential
QFN	\\=	<logical>	Electron quasi-fermi level
QFP	\\=	<logical>	Hole quasi-fermi level
Doping	\\=	<logical>	Doping
ELectrons	\\=	<logical>	Electron concentration
Holes	\\=	<logical>	Hole concentration
NET.CHarge	\\=	<logical>	Net charge concentration
NET.CArrier	\\=	<logical>	Net carrier concentration
J.Conduc	\\=	<logical>	Conduction current
J.Electr	\\=	<logical>	Electron current
J.Hole	\\=	<logical>	Hole current
J.Displa	\\=	<logical>	Displacement current
J.Total	\\=	<logical>	Total current
E.Field	\\=	<logical>	Electric field
Recomb	\\=	<logical>	Net recombination
BAND.Val	\\=	<logical>	Valence band potential
BAND.Con	\\=	<logical>	Conduction band potential
.TE
or :
.TS
l l l l.
X.Axis	\\=	<character>
Y.Axis	\\=	<character>
INFile	\\=	<character>
.TE
The above parameters specify the quantity to be plotted.  There
is no default. In mode A, one of the solution variables is
plotted versus distance into the device. For vector quantities,
the magnitude is plotted. 
In mode B, terminal characteristics can be plotted against
each other by choosing the value to be plotted on each axis
(XAXIS=,YAXIS=).
Quantities available for plotting include applied biases
(XAXIS/YAXIS=VA1, VA2, ..., VA9, VA0), actual contact bias 
which may differ from applied bias in the case of lumped
element boundary conditions (V1, V2, etc.), terminal current (I1, I2, etc.),
AC capacitances (C11, C12, C21, etc.), 
AC conductance (G11, G12, G21, etc.)
and AC admittance (Y11, Y12, Y21, etc.).
Additionally, any of the voltages or currents can be plotted
versus time for transient simulations, and any AC quantity can be
plotted versus frequency.
The values plotted are the I-V or AC data of the present run, provided 
a log is being kept (see the LOG card). Alternatively,
a different log file can be loaded with INFILE.
.sp 2
.IP "<control>"
.sp
.in +4
.TS
l l l l.
LOgarithm or Y.Log	\\=	<logical>	(default is false)
X.Log	\\=	<logical>	(default is false)
ABsolute	\\=	<logical>	(default is false)
NO.Clear	\\=	<logical>	(default is false)
NO.Axis	\\=	<logical>	(default is false)
Unchanged	\\=	<logical>	(default is false)
INTegral	\\=	<logical>	(default is false)
NEGative	\\=	<logical>	(default is false)
NO.Order	\\=	<logical>	(default is false)
POInts	\\=	<logical>	(default is false)
PAuse	\\=	<logical>	(default is false)
LIne.type	\\=	<integer>	(default is 1)
MIn.value	\\=	<real>	
MAx.value	\\=	<real>	
X.Max	\\=	<real>	
X.Component	\\=	<logical>	(default is false)
Y.Component	\\=	<logical>	(default is false)
Spline	\\=	<logical>	(default is false)
NSpline	\\=	<logical>	(default is 100)
.+B
OUTFile	\\=	<character>	(default from OPTION card)
.-B
AScii	\\=	<logical>	(default is false)
.TE
.in -4
ABSOLUTE specifies that the absolute value of the variable be taken.
For rapidly varying quantities, the LOGARITHM (Y.LOG, X.LOG)
is often more revealing.
Since many of the quantities may become negative, \*(PI actually uses
\"DID NOT WORK.so .logf
.EQ
    log (x) = ~roman "sign" (x) ~times~ log sub 10 ( 1 + | x | )
.EN
to avoid overflow. To get the true logarithm of a quantity, specify ABSOLUTE
and LOGARITHM - the absolute is taken first and there is no danger
of negative arguments.
NO.CLEAR indicates that
the screen is not to be cleared before the current plot so that
several curves can be plotted on the same axis.  NO.AXIS indicates
that the axes for the graph are not to be plotted.  UNCHANGED
is a synonym for NO.AXIS and NO.CLEAR, but additionally it
forces the use of the previous axis bounds so that a number of
curves can easily be put on the same axis.
INTEGRAL plots the integral of the specified ordinate.
NEGATIVE negates the ordinate values.
\*(PI by default will order the plot coordinates by abscissa
value; this ordering will result in unusual plots for
IV curves with negative resistance, for example.  The
NO.ORDER parameter forces \*(PI to plot the data points
as they naturally occur.
POINTS marks the data points on the plotted curve.  The PAUSE
option causes \*(PI to stop at the end of the plot so that a
hardcopy may be made before continuing.  Execution can be resumed
by hitting a carriage return.  LINE.TYPE specifies the line type
for the plotted curve.  MIN.VALUE and MAX.VALUE specify minimum
and maximum values for the ordinate of the graph; their defaults are
found automatically from the data to be plotted.  X.MAX allows
a maximum value for the abscissa to be specified (default is just
the maximum abscissa value in the data to be plotted).
X.COMPONENT and Y.COMPONENT force the x and y components respectively of any
vector quantities to be plotted as opposed to the default total
magnitude.
The SPLINE option
indicates that spline-smoothing should be performed on the data using
NSPLINE interpolated points (maximum is 500).  
.+B
The default plot device
is generally the user's terminal but may be reset
with the OPTION card.
If OUTFILE is specified, the graphics output will be directed
to that file.  For further discussion, see the OPTION card.
.-B
.\"ig
.\"OUTFILE generates an output file containing
.\"the data points plotted.  ASCII specifies that this output file have
.\"an ascii format (the default binary format is for use with the
.\"Stanford dplot system).
.\"..
.RE
.sp 2
.KS
.NH
Examples
.IP
.sp 2
The following plots a graph of potential along a straight line from
(0.0,0.0) to (5.0,0.0):
.sp
.in +4
.ss 24
PLOT.1D  POTEN A.X=0 A.Y=0 B.X=5 B.Y=0
.in -4
.sp 2
In the next example, the log of the electron concentration is plotted
from (1.0,-0.5) to (1.0,8.0) with bounds on the plotted 
electron concentration of 1.0e10 and 1.0e20.  
A spline interpolation is performed
with 300 interpolated points. The non-spline-interpolated points are
marked.
.sp
.in +4
.ss 24
.nf
PLOT.1D  ELECT LOG A.X=1 A.Y=-.5 B.X=1 B.Y=8
.br
.ss 24
.nf
+        MIN=10 MAX=20 SPLINE NSPL=300 POINTS
.sp
.fi
.in -4
In the following example, the current in contact 1 is plotted as a 
function of contact 2 voltage, then the curve is compared with
a previous run.
.sp
.in +4
.ss 24
.nf
PLOT.1D  X.AXIS=V2 Y.AXIS=I1 
PLOT.1D  X.AXIS=V2 Y.AXIS=I1 INF=logf0 UNCH
.sp
.fi
.in -4
The following plots the actual contact voltage on a contact
versus the applied voltage.
.sp
.in +4
.ss 24
.nf
PLOT.1D  X.AXIS=V3 Y.AXIS=VA3  OUTFILE=save.plot
.sp
.fi
.in -4
Finally, the following shows a plot of two capacitance components
versus the log of frequency. A different line type is chosen for
the second component.
.sp
.in +4
.ss 24
.nf
PLOT.1D  X.AXIS=FREQ Y.AXIS=C21 X.LOG
PLOT.1D  X.AXIS=FREQ Y.AXIS=C31 X.LOG UNCH LINE=4
.KE
.eC
