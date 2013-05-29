.EQ
delim $$
.EN
.bC INTERFACE
.NH 0
Syntax   
.P1     
Interface  <parameters>  <location>  
.P2
.NH 
Description
.IP     \"Paragraph of description
The INTERFACE card allows the specification of interface
parameters (recombination velocities and fixed charges)
at semiconductor-insulator boundaries.
.NH 
Parameters
.sp 2
.RS     \"Start new level of indentation
.IP <parameters>
.TS     \"Table of what parameter looks like
l l l l.
S.N	\\=	<real>	Electron surface recombination velocity (cm/sec)
S.P	\\=	<real>	Hole surface recombination velocity (cm/sec)
Qf	\\=	<real>	Fixed charge density ($ roman "cm" sup -2 $).
.TE     \"followed by text description
See chapter 2 of this report for a description of interface
surface recombination velocities.  
.sp 2
.IP <location>
.TS
l l l.
X.Min	\\=	<real>
X.Max	\\=	<real>
Y.Min	\\=	<real>
Y.Max	\\=	<real>
.TE
X.MIN, X.MAX, Y.MIN and Y.MAX define a
bounding box, measured in $ mu roman "m" $. 
Any oxide/semiconductor interfaces found within this region are charged.
A non-planar surface may be defined by using a box which contains the
whole device, provided there is only one interface in the device.
.RE
.NH 
Examples
.IP     \"Take it from here
Define an interface with both fixed charge and recombination velocities.
.P1     \" Before each set of input cards
INTERFACE  X.MIN=-4 X.MAX=4 Y.MIN=-0.5 Y.MAX=4
.br
+          QF=1E10 S.N=1E4 S.P=1E4
.P2     \" To return to normal font etc. after input card images.

.eC
