.EQ
delim @@
.EN
.bC ELECTRODE
.sp 2
.NH 0
Syntax  
.P1
ELEctrode <number> <position> 
.P2
.NH
Description
.IP     \"Paragraph of description
The ELECTRODE card specifies the location of electrodes in
a rectangular mesh.
.NH
Parameters
.sp 2
.RS	\"Start new level of indentation
.IP <number>
.TS  	\"Table of what parameter looks like
l l l.
Number	\\=	<integer>
.TE   	\"followed by text description
There may be up to ten electrodes, numbered 1,2,3,...,9,0.
They may be assigned in any order, but if there are N electrodes,
none can have an electrode number above N.
.sp 2
.IP <location>
.TS
l l l.
IX.Low	\\=	<integer>
IX.High	\\=	<integer>
IY.Low	\\=	<integer>
IY.High	\\=	<integer>
.TE
Nodes having x and y indices between IX.LOW and IX.HIGH and 
between IY.LOW and IY.HIGH respectively are designated electrode
nodes.
.RE
.NH
Examples
.sp 2
.IP	\"Take it from here
Define a typical back-side contact.
.sp
.ss 24
ELEC   N=1 IX.LOW=1 IX.HIGH=40 IY.LOW=17 IY.HIGH=17
.eC
