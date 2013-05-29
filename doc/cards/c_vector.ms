.EQ
delim @@
.EN
.bC VECTOR
.NH  0
Syntax
.sp 2
.R
.in +4
Vector <plotted quantity> <control>
.in -4
.sp
.NH 
Description
.IP
The VECTOR card plots vector quantities over an area of the device
defined by the previous PLOT.2D card.
.sp
.NH
Parameters
.sp 2
.RS
.IP "<plotted quantity>"
.sp 2
.in +4
One of:
.TS
l l l l.
J.Conduc	\\=	<logical>	Conduction current
J.Electr	\\=	<logical>	Electron current
J.Hole	\\=	<logical>	Hole current
J.Displa	\\=	<logical>	Displacement current
J.Total	\\=	<logical>	Total current
E.field	\\=	<logical>	Electric field
.TE
.in -4
The above parameters specify the quantity to be plotted.  There
is no default. 
.sp 2
.KS
.IP "<control>"
.sp
.in +4
.TS
l l l l.
LOgarithm	\\=	<logical>	(default is false)
MInimum	\\=	<real>	(default is 0)
MAximum	\\=	<real>	(default is 0)
Scale	\\=	<real>	(default is 1)
Clipfact	\\=	<real>	(default is 0.1)
LIne.type	\\=	<integer>	(default is 1)
.TE
.in -4
LOGARITHM specifies logarithmically-scaled magnitudes.  
By default, all vectors in a linear plot 
are scaled by the maximum magnitude of
the quantity of interest over the grid, while
if LOGARITHM is set, the default scaling uses
the minimum (non-zero) magnitude.
The minimum and maximum values, both of which are printed
during execution of a plot, can be arbitrarily set
using the MINIMUM and MAXIMUM parameters, so that
it is possible to plot two bias conditions
or devices with the same scaling.
SCALE a constant scale factor to be multiply all magnitudes by.  
CLIPFACT is a threshold below which vectors are not plotted.
LINE.TYPE specifies the vector line type for plotting.
.sp 2
.KE
.RE
.KS
.NH
Examples
.IP
Plot electron and hole currents over a device :
.sp 
.ss 24
.nf
.na
.in +4
PLOT.2D BOUN NO.FILL
VECTOR J.ELEC LINE=2
VECTOR J.HOLE LINE=3
.in -4
.fi 
.P2
.KE
.eC
