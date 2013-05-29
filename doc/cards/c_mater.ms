.EQ 
delim $$
define XX " times 10 sup "
.EN
.bC MATERIAL
.NH 0
Syntax  
.sp 2
.R
.in +4
MAterial  <region> <material definitions>
.in -4
.sp 2
.NH     
Description
.IP     \"Paragraph of description
The material card associates physical parameters with 
the materials in 
the mesh. Many of the parameters are default for standard materials.
Any equation numbers referred to below correspond to [1].
.NH
Parameters
.sp 2
.RS	\"Start new level of indentation
.IP "<region>"
.sp
.TS
l l l l l.
NUmber	\\=	<integer>
	or
Region	\\=	<integer>
.TE
.sp 2
NUMBER (or REGION) specifies the region number to which these parameters
apply.  Only one set of semiconductor parameters is allowed.  Therefore,
if the region specified is a semiconductor region, all 
other semiconductor regions (if there are any) will be changed as well.
.sp 2
.IP "<material definitions>"
.in 0
.sp
.TS
center;
l l l l l.
EG300	\\=	<real>	: Energy gap at 300K (eq. 2.16)	(eV)
EGAlpha	\\=	<real>	: Alpha (eq. 2.16)
EGBeta	\\=	<real>	: Beta (eq. 2.16)
AFfinity	\\=	<real>	: Electron affinity	(eV)
Permittivity	\\=	<real>	: Dielectric permittivity	(F/cm)
Vsaturation	\\=	<real>	: Saturation velocity (eq. 2.34, 2.35)	$ ( roman "cm" / roman "s") $
MUN	\\=	<real>	: Low-field electron mobility	$( roman "cm" sup 2 / roman "s" )$
MUP	\\=	<real>	: Low-field hole mobility	$( roman "cm" sup 2 / roman "s" )$
G.surface	\\=	<real>	: surface mobility reduction (eq. 2.33)	
TAUP0	\\=	<real>	: SRH Electron lifetime (eq. 2.6, 2.8)	(s)
TAUN0	\\=	<real>	: SRH Hole lifetime (eq. 2.6, 2.9)	(s)
 	 
 	 	...continued...
.TE
.bp
.TS
l l l l l.
NSRHN	\\=	<real>	: SRH conc. parameter - electrons (eq. 2.8)	$ ( roman "cm" sup {-3} )$
NSRHP	\\=	<real>	: SRH conc. parameter - holes (eq. 2.9)	$ ( roman "cm" sup {-3} )$
ETrap	\\=	<real>	: Trap level = $ E sub t ~ - ~ E sub i $ (eq. 2.6)
AUGN	\\=	<real>	: Auger coefficient ($ c sub n$) (eq. 2.7)	$ ( roman "cm" sup 6 / roman "s" )$
AUGP	\\=	<real>	: Auger coefficient ($ c sub p$) (eq. 2.7)	$ ( roman "cm" sup 6 / roman "s" )$
NC300	\\=	<real>	: Conduction band density at 300K (eq. 2.17)	$ ( roman "cm" sup {-3} )$
NV300	\\=	<real>	: Valence band density at 300K (eq. 2.18)	$ ( roman "cm" sup {-3} )$
ARICHN	\\=	<real>	: Richardson constant for electrons (eq. 2.45)
ARICHP	\\=	<real>	: Richardson constant for holes (eq. 2.46)
GCb	\\=	<real>	: Conduction-band degeneracy factor (eq. 2.33a,IIB-sm)
GVb	\\=	<real>	: Valence-band degeneracy factor (eq. 2.33b,IIB-sm)
EDb	\\=	<real>	: Donor energy level (eq. 2.34a,IIB-sm)
EAb	\\=	<real>	: Acceptor energy level (eq. 2.34b,IIB-sm)
.TE
.in
.KS
.sp
Defaults:
.TS
center;
c s s s.
\fISemiconductors\fR
.sp 0.5
.T&
c c c c.
Constant	Silicon	Gallium Arsenide	Arbitrary
.T&
l c c c.
Energy gap (300K)	1.08	1.43	0.0
Alpha	$ 4.73 XX -4 $	$ 5.405 XX -4 $	0.0
Beta	636.	204	0.0
Electron affinity	4.17	4.07	0.0
Permittivity	11.8	10.9	0.0
Saturation velocity	(eq. 2.36)	(eq. 2.37)	0.0
Electron mobility	1000	5000	0.0
Hole mobility	500	400	0.0
Surface mobility reduction	$1.0$	1.0	0.0
SRH Electron lifetime	$1.0 XX -7$	$1.0 XX -7$	0.0
SRH Hole lifetime	$1.0 XX -7$	$1.0 XX -7$	0.0
Auger coefficient (n)	$2.8 XX -31$	$2.8 XX -31$	0.0
Auger coefficient (p)	$9.9 XX -32$	$9.9 XX -32$	0.0
Cond band density (300K)	$2.8 XX 19$	$4.7 XX 17$	0.0
Val band density (300K)	$1.04 XX 19$	$7.0 XX 18$	0.0
Eff Richardson const (n)	$110$	$6.2857$	0.0
Eff Richardson const (p)	$30$	$105$	0.0
SRH conc. parameter (n)	$5.0 XX 16$	$5.0 XX 16$	0.0
SRH conc. parameter (p)	$5.0 XX 16$	$5.0 XX 16$	0.0
Trap level	$0.0$	$0.0$	0.0
Cond band degen factor	$2.0$	$2.0$	0.0
Val band degen factor	$4.0$	$2.0$	0.0
Donor energy level (eV)	$0.044$	$0.005$	0.0
Acceptor energy level (eV)	$0.045$	$0.005$	0.0
.TE
.KE
.KS
.TS
center;
c s s s s
c c c c c
l n n n n.
\fIInsulators\fR
.sp 0.5
Constant	Silicon dioxide	Silicon nitride	Sapphire	Arbitrary
Permittivity	3.9	7.5	12.0	0.0
.TE
.KE
.KS
.RE
.NH
Examples
.IP
.sp 2
The following defines SRH lifetimes and concentration-independent
low-field mobilities for all the semiconductor regions within the device
(all the other parameters are assumed to be their default, consistent
with the semiconductor type chosen):
.sp
.in +4
.ss 24
MATERIAL  TAUN0=5.0e-6 TAUP0=5.0e-6 MUN=3000 MUP=500
.KE
.fi
.eC
