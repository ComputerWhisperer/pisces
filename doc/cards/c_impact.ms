.\" Tue Sep 20 14:48:41 PDT 1988 (dredge -- Stanford)
.EQ
delim @@
.EN
.+B
.bC IMPACT
.NH  0
Syntax
.sp 2
.R
.in +4
IMpact <parameters>
.in -4
.sp 2
.NH 
Description
.IP
The IMPACT card specifies the use of the impact ionization model.
For many devices, the impact ionization model for continuity
equations allows the accurate prediction of avalanche breakdown.
Baraff's model ([6]) has been approximated
with compact formulae by Crowell and Sze ([7]).
The current models are for Si only.
See also, the IMPACT parameter to the MODEL card and the impact
parameters to the CONTOUR card.
.PP
.NH 
Parameters
.RS
.TS
l l l l.
CRowell	\\=	<logical>	(default is false)
MOnte	\\=	<logical>	(default is false)
LAMDAE	\\=	<real>	(default is 6.2e-7)
LAMDAH	\\=	<real>	(default is 3.8e-7)
.TE
.LP
CROWELL specifies the use of Crowell and Sze formulae.
if MONTE is given then the alpha values are extracted by
Monte Carlo simulation.
If CROWELL is not specified, Grant's model [4] is used.
The basic implementation idea follows Laux [5], but using
Scharfetter-Gummel current discretization formula without
a weighting scheme.
LAMBDAE and LAMDAH specify the mean free path in @cm@ for
electrons and holes respectively.
.PP
Also the Newton method with 2-carrier must be specified on the
METHOD card since impact ionization is a 2-carrier process.
.RE
.NH
Examples
.IP
Use the Crowell and Sze formulae with the default mean free
paths.
.sp 2
.in +4
.ss 24
.nf
IMPACT  CROWELL LAMDAE=6.2e-7  LAMDAH=3.8e-7
.fi
.eC
.-B
