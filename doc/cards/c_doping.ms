.\" Sat Sep  8 15:17:15 PDT 1990 (chin--stanford)
.EQ
delim $$
gsize 11
.EN
.bC DOPING
.NH 0
Syntax  
.P1  
DOping <profile type> <location> <region> <profile specification> <save>
.P3
.NH 
Description
.IP     \"Paragraph of description
The DOPING card dopes selected regions of the device. 
.NH 
Parameters
.sp 2
.RS     \"Start new level of indentation
.IP "<profile type>"
.br
One of the following types must be selected.
.DS
.nf
Erfc
Gaussian
Uniform
SUprem3
.\"# NEw.suprem3
.+B 8822
OLD.Suprem3
.-B 8822
AScii
.+B 9009
S4geom
SImpldop
.-B 9009
.fi
.DE
.+B 8940
ERFC, GAUSSIAN and UNIFORM are used to analytically describe
.-B 8940
profile shapes.
.+B 8822
SUPREM3 reads data files produced by the more
recent release of \*(SU, which supports
the ``\fIexport\fR'' output file format.  The default
is to read binary formatted export files.  If the ASCII parameter
is also specified, then ASCII (text) formatted \*(SU export
files will be expected.
OLD.SUPREM3 is used to get profile information
from the original version of the \*(SU process modeling
program.  This reads the binary structure file.
ASCII without the SUPREM3 parameter allows for input of
simple ascii data
.-B 8822
files containing concentration versus depth information.
The format of the ASCII input file is a depth in $mu$m followed by
a concentration in $cm sup {-3}$ \- one pair per line.
.+B 9009
By convention, positive concentrations refer to donors (n-type),
while negative concentration values refer to acceptors (p-type).
S4GEOM allows doping from a SUPREM-IV file (using the 
struct pisces=foo command in SUPREM-IV) to be interpolated
onto an existing PISCES-II mesh.
SIMPLDOP takes doping from a SIMPL-2 file (rectangular grid
of doping values) to be interpolated onto an existing
PISCES-II mesh.
.-B 9009
.sp 1
.KS
.IP <location>
.sp 1
.TS
box;
c s s |c s s.
Parameter	DEFAULT
\^	_
.T&
c s s |c |c s.
.+B 8940
\^	(Uniform)	(Erfc/Gaussian/Suprem/Ascii)
.-B 8940
\^	\^	_
.T&
c s s |c |c |c.
\^	\^	(x-direction)	(y-direction)
_
.T&
l l l |c |c |c.
X.Left	\\=	<real>	$ - inf $	$ italic "SP" $	$ - inf $
X.Right	\\=	<real>	$ ~inf $	X.LEFT	$ ~inf $
.T&
l l l |c |c |c.
Y.Top	\\=	<real>	$ - inf $	$ - inf $	Y.BOT
Y.Bottom	\\=	<real>	$ ~inf$	$~inf$	$italic "SP"$
.TE
.nr PS 11
.ps 11
The box given by the X and Y bounds locates the profile
within a device, and defines an area (or line)
where the profile is constant (mask edges).
Outside this area, it falls off along the principal
axis according to the profile type, and along the lateral
axis according to the lateral specifications. The default
bounds of the box are chosen depending on the type and principal
.+B 8940
direction of the profile. In the Erfc/Gaussian/Suprem case, the
.-B 8940
bounds default to a line perpendicular to the principal axis and
located at the peak/start of the profile, respectively. This is
denoted by the entry SP in the above table.
.KE
.sp 2
.IP <region>
.TS
l l l.
REgion	\\=	<integer>
.TE
Region number where doping is to be added (optional).  Multiple
regions may be included by concatenating their region numbers
into a single integer.  If no region is specified, the entire 
semiconductor portion of the device is used.
.sp 2
.+B 9009
\"********************S4Geom********************
.ti -4
If <profile type>=S4GEOM 
the following profile specification is relevant:
.sp
.IP "<Input filename>"
.TS
l l l.
Infile	\\=	<filename>
.TE
The doping is obtained from Infile and interpolated onto
an existing PISCES-IIB mesh.
\"********************SImpldop********************
.ti -4
If <profile type>=SIMPLDOP 
the following profile specification is relevant:
.sp
.IP "<Input filename>"
.TS
l l l.
Infile	\\=	<filename>
.TE
The doping is obtained from Infile and interpolated onto
an existing PISCES-IIB mesh.
.eC
.-B 9009
\"********************Suprem3********************
.ti -4
If <profile type>=SUPREM3 or OLD.SUPREM3, 
the following profile specifications are relevant:
.sp
.IP "<Input filename>"
.TS
l l l.
Infile	\\=	<filename>
.TE
.IP "<dopant>:  One of"
.TS
l l l.
Boron	\\=	<logical>
PHosphorus	\\=	<logical>
ARsenic	\\=	<logical>
ANtimony	\\=	<logical>
.TE
The selected dopant profile will be extracted from
the \*(SU save file.
.sp 
.IP "<Two-dimensional spread>"
.TS
c s s c
l l l l.
Parameter	Default
DIrection	\\=	x or y	y
STart	\\=	<real>	0
.+B 8940
RAtio.Lateral	\\=	<real>	0.8 
.-B 8940
.TE
This group of parameters specifies where to locate the one-dimensional
profile in the 2-dimensional device and how to extend it to the 
second dimension.
DIRECTION is the axis along which the profile is directed. START
is the depth in the specified direction where the profile should 
start, and should normally be at the surface. The lateral profile
is assumed to have the same form as the principal profile, but
shrunk/expanded by the factor RATIO.LATERAL.  The defaults for
the location box are set up as a line, parallel to the surface,
and located at START.
.sp 2
\"********************Ascii********************
.ti -4
If <profile type>=ASCII
the following profile specifications are relevant:
.sp
.IP "<Input filename>"
.TS
l l l.
Infile	\\=	<filename>
.TE
.sp
The ascii concentrations in INFILE are read and added to
the impurity profiles.
.sp 
.IP "<Two-dimensional spread>"
.sp
As above - see description for SUPREM3 and OLD.SUPREM3.
.eC
.sp 2
.KS \"********************Gaussian********************
.ti -4
If <profile type>= GAUSSIAN, the following profile specifications are 
relevant:
.sp
.IP <profile>
.br
.TS
l s s s s s.

.T&
 l l l l l l.
 	COncentration	\\=	<real>	(no default)	$ ( roman "cm" sup -3 )$
 	JUnction	\\=	<real>	(no default)	$ ( mu roman "m" ) $
 	SLice.lat	\\=	<real>	(see below)	$ ( mu roman "m" ) $
.T&
l s s s s s.

or

.T&
 l l l l l l.
 	DOSe	\\=	<real>	(no default)	$ ( roman "cm" sup -2 ) $
 	CHaracteristic	\\=	<real>	(no default)	$ ( mu roman "m" ) $
.T&
l s s s s s.

or

.T&
 l l l l l l.
 	COncentration	\\=	<real>	(no default)	$ ( roman "cm" sup -3 )$
 	CHaracteristic	\\=	<real>	(no default)	$ ( mu roman "m" ) $
.T&
l s s s s s.

and one of :

.T&
l l l l l l.
 	N.type/DONor	\\=	<logical>	(no default)	 
 	P.type/ACceptor	\\=	<logical>	(no default)	 
.T&
l s s s s s.

and any combination of :

.T&
l l l l l l.
 	RAtio.Lateral	\\=	<real>	(default: 0.8)	 
 	Erfc.Lateral	\\=	<logical>	(default: false)	 
 	Lat.char	\\=	<real>	(see below)	 
 	PEak	\\=	<real>	(default: 0)	$ ( mu roman "m" )$
 	DIrection	\\=	<character>	(default: y)	 
.TE
.+B 8940
\"********************Erfc********************
.ti -4
If <profile type>= ERFC, the following profile specifications are 
relevant:
.sp
.IP <profile>
.br
.TS
l s s s s s.

.T&
 l l l l l l.
 	COncentration	\\=	<real>	(no default)	$ ( roman "cm" sup -3 )$
 	JUnction	\\=	<real>	(no default)	$ ( mu roman "m" ) $
 	J.Conc	\\=	<real>	(CO/100)	$ ( roman "cm" sup -3 )$
 	SLice.lat	\\=	<real>	(see below)	$ ( mu roman "m" ) $
.T&
l s s s s s.

or

.T&
 l l l l l l.
 	COncentration	\\=	<real>	(no default)	$ ( roman "cm" sup -3 )$
 	CHaracteristic	\\=	<real>	(no default)	$ ( mu roman "m" ) $
.T&
l s s s s s.

and one of :

.T&
l l l l l l.
 	N.type/DONor	\\=	<logical>	(no default)	 
 	P.type/ACceptor	\\=	<logical>	(no default)	 
.T&
l s s s s s.

and any combination of :

.T&
l l l l l l.
 	RAtio.Lateral	\\=	<real>	(default: 0.8)	 
 	Erfc.Lateral	\\=	<logical>	(default: false)	 
 	Lat.char	\\=	<real>	(see below)	 
 	PEak	\\=	<real>	(default: 0)	$ ( mu roman "m" )$
 	DIrection	\\=	<character>	(default: y)	 
.TE
.-B 8940
These parameters govern the profile outside the constant box.
DIRECTION defines the principal axis of the profile.
CONCENTRATION is the peak concentration, DOSE the total dose.
.+B 8940
J.CONC is the concentration at the junction,
.-B 8940
JUNCTION is the location of the junction and must be in silicon,
outside the constant box; 
CHARACTERISTIC is the principal characteristic length.
The peak concentration and principal characteristic length 
are computed from the given combination of the
first four parameters. 
When JUNCTION is used, \*(PI
computes the characteristic length by examining  the doping at a 
point half way between the ends of the
constant box and at the given depth; if some other lateral position is
desired for the computation, use the parameter SLICE.LATERAL=<real>. 
The lateral impurity profile may
be an error function  instead of gaussian, and its characteristic length is 
either the product of RATIO.LATERAL and 
the principal characteristic length (default) or can be specified 
using LAT.CHAR.
PEAK specifies the position of the peak. The defaults for the
constant box are set up as a line, parallel to the surface and located
at PEAK.
.KE
.sp 2
.KS \"********************UNIFORM********************
.RE
.ti -4
If <type>=UNIFORM the following parameters are relevant:
.sp 
.RS
.IP <concentration>
.TS
l l l.
COncentration	\\=	<real>
N.type/DONor	\\=	<logical>
P.type/ACceptor	\\=	<logical>
.TE
Concentration is the value of the uniform doping level.
It should be given in units of atoms/$ roman "cm" sup 3  $ and be positive.
The polarity is given by the logical parameters. 
Doping is introduced in the intersection of the box 
and the region selected.  The default box is set up to include the
entire region.
.KE
.RE
.sp 2
<save>
.in +4
.TS
l l l.
Outfile	\\=	<filename>
.TE
.in -4
The save option allows the user to save a machine-readable copy of
all the DOPING cards in a file. The first DOPING card should have
the OUTFILE parameter, so that the doping information on it and
all subsequent DOPING cards are saved in that file.
The file can be reread after regrid to calculate the doping on the
new grid.

.KS
.NH 
Examples
.IP     \"Take it from here
.KS
A one-dimensional diode with substrate doping 
$10 sup 16 ~ roman "cm" sup -3$
and Gaussian profile.
.P1
DOP   UNIF CONC=1E16 P.TYPE
DOP   GAUSS CONC=1E20 JUNC=0.85 N.TYPE PEAK=0
.P2
.KE
.KS
An n-channel MOSFET with Gaussian source and drain.
Because the default X.RIGHT is $ + inf $, for the source we
must limit the constant part to X.RIGHT=4, and conversely for
the drain. Thus the profile has a constant part along the surface,
falls off as an error function towards the gate, and as a gaussian
in the direction of the bulk. In both cases, the vertical junction is at 
$ 1.3 \(*m"m" $.
.KE
.KS
.P1
DOP  UNIF CONC=1E16 P.TYPE 
DOP  GAUSS CONC=9E19 N.TYPE
+    X.RIGHT=4 JUNC=1.3 R.LAT=0.6 ERFC.LAT 
DOP  GAUSS CONC=9E19 N.TYPE
+    X.LEFT=12 JUNC=1.3 R.LAT=0.6 ERFC.LAT
.P2
.KE
.KS
Read a Suprem bipolar profile and add it to a uniform substrate
concentration.  Add doping only to those points lying in region 1.
.P1     
COM  *** SUBSTRATE ***
DOP  REGION=1 UNIF CONC=1E16 N.TYPE
COM  *** BASE ***
DOP  REGION=1 ASCII SUPREM   BORON R.LAT=0.7 INF=plt3.out1
+    START=0
COM  *** EMITTER ***
DOP  REGION=1 ASCII SUPREM   PHOS  R.LAT=0.8 INF=plt3.out1
+    X.LEFT=12.0 X.RIGHT=13.0 START=0
.P2  
.KE
.KS
Simulate a triple diffused bipolar by using a mixture 
of analytic and SUPREM-III
profiles.  Use an erfc for the emitter, a SUPREM-III profile
for the base, a gaussian for the collector,
and add it to a uniform substrate
concentration.  Add doping only to those points lying in region 1.
.P1     
COM *** SUBSTRATE ***
DOP REGION=1 UNIFORM CONC=9.999463e+14 p.type
COM *** EMITTER ***
DOP REGION=1 ERFC N.TYPE CON=1e20 CHAR=0.1 
+  X.LEF=-1 X.RIG=0 R.LAT=0.8 
COM *** BASE ***
DOP REGION=1 SUPREM3 INFILE=base.exp BORON
+	X.LEF=-4 X.RIG=0 R.LAT=0.8
COM *** COLLECTOR ***
DOP REGION=1 GAUSS PHOS CON=1e17 CHAR=0.8
+	X.LEF=-7 X.RIG=0 R.LAT=0.8
.P2  
.KE
.P1
.P2
.eC
.KE
