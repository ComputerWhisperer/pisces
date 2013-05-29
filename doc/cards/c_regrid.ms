.EQ
delim $$
.EN
.bC REGRID
.NH 0
Syntax
.P1     
REGRid  <location> <variable> <control> <files>
.P2
.NH 
Description
.IP     
The REGRID card allows refinement of a crude mesh. Any triangle
across which the chosen variable changes by more than a
specified tolerance, or in which the chosen variable
exceeds a given value, is refined.
.NH 
Parameters
.sp 2
.RS     \"Start new level of indentation
.IP <location> 
.sp 
One of :
.TS
l l l l.
X.MIn	\\=	<real>
X.MAx	\\=	<real>
Y.MIn	\\=	<real>
Y.MAx	\\=	<real>
REGion	\\=	<integer>	default:all
IGnore	\\=	<integer>	default:none
.TE
The bounds X.MIN-Y.MAX are used to limit the refinement; only 
triangles which have nodes which fall inside the box are considered
for refinement. The REGION parameter has a similar use; only regions
specified are refined according to the user criterion. (Others may be refined
as a side effect, to maintain well-shaped triangles). 
The default is to refine all regions for
potential and electric field regrids, and all semiconductor regions
for regrids which depend on the other variables. The parameter
IGNORE is similar to REGION, but opposite in effect. Ignored regions
are not regridded either according to the user criterion or according
to the ``obtuse criterion'' (see below); nor are they smoothed after regrid.
The default is not to ignore any region.
.sp 2
.IP <variable> 
.sp 
One of :
.TS
l l l l l.
Potential	\\=	<logical>	Mid-gap potential	(Volts)
EL.field	\\=	<logical>	Electric-field	(Volts/cm)
QFN	\\=	<logical>	Electron quasi-fermi level	(Volts)
QFP	\\=	<logical>	Hole quasi-fermi level	(Volts)
DOPIng	\\=	<logical>	Net doping	$( roman "cm" sup -3 )$
ELEctron	\\=	<logical>	Electron concentration	$( roman "cm" sup -3 )$
Hole	\\=	<logical>	Hole concentration	$( roman "cm" sup -3 )$
NET.CHrg	\\=	<logical>	Net charge	$( roman "cm" sup -3 )$
NET.CArr	\\=	<logical>	Net carrier concentration	$( roman "cm" sup -3 )$
MIn.carr	\\=	<logical>	Minority carrier conc.	$( roman "cm" sup -3 )$
.TE
This parameter selects the discriminatory variable.
.sp 2
.IP <control>
.TS
l l l l.
STep or RAtio	\\=	<real>	no default
CHange	\\=	<logical>	default : true
ABsolute	\\	<logical	default: false
LOGarithm	\\=	<logical>	default: false
MAx.level	\\=	<integer>	default: dynamic
SMooth.k	\\=	<integer>	default: 0
LOCaldop	\\=	<logical>	default: false
COs.ang	\\=	<real>	default:2.0
.TE
CHANGE determines whether to use the magnitude or the difference of
a variable in a triangle as the criterion of refinement. It is
normally set to true (difference).
STEP is the numerical criterion for refining a triangle. RATIO is a synonym.
If the variable ranges
across many orders of magnitude, it is advisable to examine its 
logarithmic variation, using the LOG flag. In this case STEP will
be interpreted as the step in the logarithm.  ABSOLUTE specifies
that the absolute value of the quantity is to be used.
Since many of the quantities may become negative, \*(PI actually uses
\".so .logf DID NOT WORK
.EQ
    log (x) = ~roman "sign" (x) ~times~ log sub 10 ( 1 + | x | )
.EN
to avoid overflow. To get the true logarithm of a quantity, specify ABSOLUTE
and LOGARITHM - the absolute is taken first and there is no danger
of negative arguments.
LOCALDOP is used with minority carrier regrids, and specifies that
if the minority carrier concentration exceeds the local doping,
the grid is to be refined.
MAX.LEVEL is the maximum level of any triangle relative to the original
mesh. It defaults to one more than the maximum level of the
grid, but can be set to a smaller value to limit refinement.
Values less than or equal to zero are interpreted relative to the
current maximum level.
SMOOTH.K has the same meaning as on the mesh card.
COS.ANGLE defines the ``obtuse criterion'' to
limit the creation of obtuse angles in the mesh. 
If regrid would create a triangle with an
angle whose cos is less than -COS.ANGLE, nodes are added so that
this does not occur. 
The test can be turned off locally by
using the ignore parameter; it can be turned off everywhere by 
using a value of COS.ANG greater than 1. The default is to turn
it off everywhere.
.sp 2
.KS
.IP  <files>
.TS
l l l l.
OUTFile	\\=	<filename>
OUT.green	\\=	<filename>
IN.green	\\=	<filename>
DOPFile	\\=	<filename>
AScii	\\=	<logical>	(default is false)
.TE
OUTFILE is the binary output mesh file, and is necessary if the
mesh is to be used for subsequent runs. A history of the
triangle tree is always generated to assist further regriding steps. 
Its name can be specified by OUT.GREEN, and its default is
generated from OUTFILE by concatenating the letters ``tt''
to the end.  Additionally, a triangle tree for the previous mesh
(if a tree exists) is used for this regrid.  By default,
\*(PI will look for a file with the same name as the
current mesh plus ``tt'' at the end as above.  Alternatively,
IN.GREEN can be used to implement a different file name.
DOPFILE is the name of a file (up to 20 characters) which 
contains the doping for the device
(see DOPING card).  Specifying DOPFILE avoids interpolating doping
values at any newly created grid points (the default), by using the 
initial doping specification to redope the structure.
ASCII specifies that all mesh files and triangle trees
(not DOPFILE) for this card should be done in ascii
rather than the default - binary.
.RE
.KE
.KS
.NH 
Examples
.IP 
Starting with an initial grid, we refine twice, requesting that
all triangles with large doping steps be refined:
.P1
REGRID  LOG DOPING STEP=6 OUTF=grid1 DOPF=dopxx1
REGRID  LOG DOPING STEP=6 OUTF=grid2 DOPF=dopxx1
.P2
A similar effect could be obtained with just one regrid statement:
.P1
REGRID  LOG DOPING STEP=6 OUTF=grid2 DOPF=dopxx1 
+       MAX.LEVEL=2
.P2
In both cases two levels of refinement are done. The first choice
is preferable however, because new doping information is introduced
at each level of refinement. This gives a better criterion for refinement,
and fewer triangles.
.KE
.KS
Now we perform an initial solution and refine triangles which exhibit
large potential steps:
.P1
SOLVE  INIT OUT=grid2.si
REGRID POTENTIAL STEP=0.2 OUTF=grid3
.P2
.KE
.eC
