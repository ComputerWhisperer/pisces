.EQ
delim @@
.EN
.bC ELIMINATE
.NH 0
Syntax   
.P1     
ELIMINATE <range> <direction>
.P2
.NH 
Description
.IP     \"Paragraph of description
The ELIMINATE card terminates mesh points along lines 
in a rectangular grid.
.NH 
Parameters
.sp 2
.RS     \"Start new level of indentation
.IP <direction>
.TS     \"Table of what parameter looks like
l l l.
X.direction	\\=	<logical>
Y.direction	\\=	<logical>
.TE     \"followed by text description
These parameters determine whether to eliminate points along
vertical or horizontal lines. One must be chosen.
.sp 2
.IP <range>
.TS
l l l.
IX.Low	\\=	<integer>
IX.High	\\=	<integer>
IY.Low	\\=	<integer>
IY.High	\\=	<integer>
.TE
Points along every second line within the chosen range is removed. 
Successive eliminations of the same range remove points along
every fourth, eighth line,
and so on. For horizontal elimination, the vertical bounds should
be decreased by one at each re-elimination of the same region,
and conversely for vertical eliminations.
.RE
.KS
.NH 
Examples
.IP     \"Take it from here
Points along vertical lines between 10 and 20 are removed.
.P1     \" Before each set of input cards
ELIM    Y.DIR IY.LO=10 IY.HI=20 IX.LO=1 IX.HI=8
ELIM    Y.DIR IY.LO=10 IY.HI=20 IX.LO=1 IX.HI=7
.P3     \" To return to normal font etc. after input card images.
.KE
.eC
