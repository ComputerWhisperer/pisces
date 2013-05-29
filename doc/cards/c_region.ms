.EQ
delim $$
.EN
.bC REGION
.NH  0
Syntax
.sp 2
.R
.in +4
REGIon <number> <position> <material>
.in -4
.sp
.NH 
Description
.IP
The region card defines the location of materials in a 
rectangular mesh. Every triangle must be defined to be some material.
.NH
Parameters
.sp 2
.RS
.IP <number>
.TS
l l l.
NUmber	\\=	<integer>
.TE
This parameter selects the region in question. There is 
a maximum of eight regions in a \*(PI structure. 
.sp 2
.IP <position>
.br
.TS
l l l.
IX.Low	\\=	<integer>
IX.High	\\=	<integer>
IY.Low	\\=	<integer>
IY.High	\\=	<integer>
.TE
.br
These parameters are the indices of a box 
in the rectangular mesh. 
.sp 2
.KS
.IP <material>
.br
.nf
.in +4
One of:
.sp
SILicon
Gaas
SEmiconductor
Oxide or SIO2
NItride or SI3n4
SApphire
INsulator
.in -4
.fi
.KE
.RE
.sp
.NH
Examples
.IP
.sp
The following defines a silicon region extending from
nodes 1 to 25 in the x direction and nodes 4 to 20 in the 
y direction :
.sp
.in +4
.ss 24
REGION NUM=1 IX.LO=1 IX.HI=25 IY.LO=4 IY.HI=20 SIL
.in -4
.sp
Note that region 
cards are cumulative in effect : 
.sp
.in +4
.ss 24
.nf
REGION NUM=1 IX.LO=4  IX.HI=5  IY.LO=1 IY.HI=20 OXIDE
.br
.ss 24
REGION NUM=1 IX.LO=36 IX.HI=37 IY.LO=1 IY.HI=40 OXIDE
.fi
.in -4
.sp
defines one region comprised of two separate strips.
.eC
