.EQ
delim @@
.EN
.bC X.MESH Y.MESH
.sp 2
.NH 0
Syntax
.P1
X.Mesh  <node>  <location>  <ratio>
Y.Mesh  <node>  <location>  <ratio>
.P2
.NH  
Description
.IP     
The X.MESH and Y.MESH cards specify the location of lines of nodes
in a rectangular mesh. 
.NH
Parameters
.sp 2
.RS	\"Start new level of indentation
.IP <node>
.TS  	\"Table of what parameter looks like
l l l.
Node	\\=	<integer>
.TE   	\"followed by text description
This is the number of the line in the mesh. There can be at most
120 lines in either direction. Lines are assigned consecutively,
beginning with the first and ending with the last.
.sp 2
.IP <location>
.TS
l l l.
Location	\\=	<real>
.TE
This is where to locate the line. The location is interpreted in microns.
.sp 2
.IP <ratio>
.TS
l l l.
Ratio	\\=	<real>
.TE
This gives the ratio to use when interpolating lines between the given
lines. The spacing grows/shrinks by ratio in each subinterval, and
the ratio should usually lie between 0.667 and 1.5.
.RE
.KS
.NH
Examples
.IP	
Space grid lines closely around a 
junction in a 1-d diode with the junction at 0.85 microns:
.sp 2
.in +4
.ss 24
.nf
Y.MESH N=1 LOC=0.0 
Y.MESH N=20 LOC=0.85 RATIO=0.75
Y.MESH N=40 LOC=2 RATIO=1.333
.fi
.KE
.eC
