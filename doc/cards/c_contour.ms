.\" Tue Sep 20 11:52:35 PDT 1988 (dredge -- Stanford)
.EQ 
delim @@
.EN
.bC CONTOUR
.NH 0
Syntax
.R
.sp 2
.in +4
CONTOur <plotted quantity> <range definition> <control>
.in -4
.sp 2
.NH 
Description
.PP 
The CONTOUR card plots contours on a plotted two-dimensional area of
the device, as specified by the most recent PLOT.2D card.
.NH
Parameters
.sp 2
.RS
.KS
.IP "<plotted quantity>"
.sp 2
One of:
.TS
l l l l.
POtential	\\=	<logical>	Mid-gap potential
QFN	\\=	<logical>	Electron quasi-fermi level
QFP	\\=	<logical>	Hole quasi-fermi level
Valenc.b	\\=	<logical>	Valence band potential
CONduc.b	\\=	<logical>	Conduction band potential
DOping	\\=	<logical>	Doping
ELectrons	\\=	<logical>	Electron concentration
Holes	\\=	<logical>	Hole concentration
NET.CHarge	\\=	<logical>	Net charge concentration
NET.CArrier	\\=	<logical>	Net carrier concentration
J.Conduc	\\=	<logical>	Conduction current
J.Electr	\\=	<logical>	Electron current
J.Hole	\\=	<logical>	Hole current
J.Displa	\\=	<logical>	Displacement current
J.Total	\\=	<logical>	Total current
E.field	\\=	<logical>	Electric field
Recomb	\\=	<logical>	Net recombination
Flowlines	\\=	<logical>	Current flow lines
.+B
ALPHAN	\\=	<logical>	Ionization rate for electrons (/cm)
ALPHAP	\\=	<logical>	Ionization rate for holes (/cm)
IMPACT	\\=	<logical>	Generated carrier density due to impact ionization (/cm2)
.-B
.TE
.KE
.RE
The above parameters specify the quantity to be plotted.  For
vector quantities the magnitude is plotted. Model
dependent parameters (current and recombination) are calculated
with the models currently defined, \fInot\fR with the models
that were defined when the solution was computed. This allows
the display of, for instance, Auger and Shockley-Read-Hall
components of recombination separately. 
For consistent values of current, the models used in the
solution should be specified.
The quantity to be plotted has no default.
.sp 2
.KS
.IP "<range definition>"
.sp
.in +4
.TS
l l l.
MIn.value	\\=	<real>
MAx.value	\\=	<real>
DEl.value	\\=	<real>
NContours	\\=	<integer>
.TE
.in -4
MIN.VALUE and MAX.VALUE specify the minimum and maximum contours to
be plotted.  
MIN.VALUE and MAX.VALUE default to the minimum and maximum values
of the quantity to be plotted over the device (these are printed 
during execution).
DEL.VALUE specifies the interval between contours.
Alternatively, NCONTOURS specifies the number of contours to be plotted.
If the plot is logarithmic, the minimum and
maximum should be given as the logarithmic bounds.
.sp 2
.KE
.KS
.IP "<control>"
.sp
.in +4
.TS
l l l l.
LIne.type	\\=	<integer>	(default is 1)
ABsolute	\\=	<logical>	(default is false)
LOgarithm	\\=	<logical>	(default is false)
NEGative	\\=	<logical>	(default is false)
X.compon	\\=	<logical>	(default is false)
Y.compon	\\=	<logical>	(default is false)
PAuse	\\=	<logical>	(default is false)
COLor	\\=	<logical>	(default is false)
C1.color	\\=	<integer>	(default is 6)
C2.color	\\=	<integer>	(default is 7)
C3.color	\\=	<integer>	(default is 8)
C4.color	\\=	<integer>	(default is 9)
C5.color	\\=	<integer>	(default is 10)
C6.color	\\=	<integer>	(default is 11)
C7.color	\\=	<integer>	(default is 12)
C8.color	\\=	<integer>	(default is 13)
C9.color	\\=	<integer>	(default is 14)
C0.color	\\=	<integer>	(default is 15)
.TE
.in -4
LINE.TYPE defines the plot line type.
ABSOLUTE specifies that the absolute value of the variable be taken.
For rapidly varying quantities, the LOGARITHM is often more revealing.
Since many of the quantities may become negative, \*(PI actually uses
\"DID NOT WORK.so .logf
.EQ
    log (x) = ~roman "sign" (x) ~times~ log sub 10 ( 1 + | x | )
.EN
to avoid overflow. To get the true logarithm of a quantity, specify ABSOLUTE
and LOGARITHM - the absolute is taken first and there is no danger
of negative arguments.
NEGATIVE negates the values of thevariables plotted.
X.COMPON and Y.COMPON take the x and y components of a vector quantity,
respectively.  The PAUSE
option causes \*(PI to stop at the end of the plot so that a
hardcopy may be made before continuing.  Execution can be resumed
by hitting a carriage return. 
COLOR specifies that color fill, as opposed to simple lines, should
be used to delineate contours.  C1.COLOR, C2.COLOR, etc. specify
the color types for the contours.
.sp 2
.KE
.RE
.KS
.NH
Examples
.sp 2
.IP
The following plots the contours of potential from -1 volts to
3 volts in steps of .25 volts:
.P1
CONTOUR  POTEN MIN=-1 MAX=3 DEL=.25
.P2
.sp
In the next example, the log of the doping concentration is plotted
from 1.0e10 to 1.0e20 in steps of 10.  By specifying ABSOLUTE, both
the n-type and p-type contours are shown.
.P1
CONTOUR  DOPING MIN=10 MAX=20 DEL=1 LOG ABS
.P2
.sp
In the following, current flow lines are plotted.  The number of 
flow lines is 11 so that 10% of the current flows between
adjacent lines.
.P1
CONTOUR  FLOW NCONT=11
.P2
.KE
.eC
