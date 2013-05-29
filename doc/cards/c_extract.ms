.EQ
delim @@
gsize 11
.EN
.bC EXTRACT
.NH 0
Syntax  
.P1  
EXtract <variable> <bounds> <file i/o>
.P3
.NH
Description
.IP 
The EXTRACT card extracts selected electrical data from the solution.
.NH
Parameters
.sp 2
.RS
.IP "<variable>"
.sp 1
.TS
l l l l.
NET.CHar	\\=	<logical>	Integrated net charge
NET.CArr	\\=	<logical>	Integrated carrier concentration
Electron	\\=	<logical>	Integrated electron concentration
Hole	\\=	<logical>	Integrated hole concentration
Metal.Ch	\\=	<logical>	Integrated charge on a contact
N.Resist	\\=	<logical>	n-Resistance of a cross section
P.Resist	\\=	<logical>	p-Resistance of a cross section
N.Current	\\=	<logical>	n-current through an electrode
P.Current	\\=	<logical>	p-current through an electrode
.TE
The net carrier, charge, electron or hole concentrations can
be integrated over a section of a device. The charge on
a part of an electrode can be calculated, as can the current
through that part. This is useful for capacitance studies,
in conjunction with the difference mode of the load card.
The resistance of a cross sectional structure, for instance
a diffused line, can be calculated.
.sp 1
.IP "<bounds>"
.sp 1
.TS
l l l.
X.MIn	\\=	<real>
X.MAx	\\=	<real>
Y.MIn	\\=	<real>
Y.MAx	\\=	<real>
Contact	\\=	<integer>
Regions	\\=	<integer>
.TE
Only nodes falling within the rectangle X.MIN-Y.MAX are included in
the integrations. The default bounds include the entire device.
For electrode quantities (current and metal charge)
a CONTACT must be chosen; only nodes falling within the bounds and
belonging to the contact are included in the integration.
REGIONS can be optionally specified, forcing integration 
only on nodes within the specified bounds that are also part of a 
particular set of regions.
.sp 2
.IP "<file i/o>"
.sp 1
.TS
l l l.
Outfile	\\=	<filename>
.TE
An optional ascii OUTPUT file can be specified to which
the result and bias infor will be written.
.RE
.sp
.NH
Examples
.IP
.sp 2
The following extracts the resistance of a p-type line diffused
into a lightly doped n substrate. Since the p-conductivity of
the substrate is negligible, the bounds of the integration can
include the whole device.
.sp
.in +4
.ss 24
EXTRACT  P.RESIST
.in -4
.sp 2
In the next example, the charge on the lower surface of a gate
electrode is integrated. There is 0.05 \(*mm of gate oxide
on the surface, which is at y=0.
.sp
.in +4
.ss 24
.nf
EXTRACT  METAL.CH CONT=1 X.MIN=-2.0 X.MAX=2.0 
+        Y.MAX=-0.0499 Y.MIN=-0.0501

.eC
