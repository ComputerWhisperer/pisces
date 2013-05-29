.EQ
delim $$
.EN
.bC CONTACT
.NH 0
Syntax
.P1 
CONTAct  <number> <workfunction> <special conditions>
.P2
.NH
Description
.IP     
The CONTACT card defines the physical parameters of an electrode.
If no contact card is supplied for an electrode, it is assumed
to be charge-neutral (ohmic).  Lumped elements are also
specified here.
.NH
Parameters
.sp 2
.RS     \"Start new level of indentation
.IP <number>
.sp
.TS     \"Table of what parameter looks like
l l l.
NUmber	\\=	<integer>
 	or	 
ALL	\\=	<logical>
.TE   	\"followed by text description
The number must be that of a previously defined electrode.
Using ALL instead of <number> defines the same properties for
all electrodes.
.sp 2
.IP "<workfunction>"
.sp 2
One of :
.TS
center;
c c
l c.
\fIMaterial\fR	\fIWork function used\fR
NEutral	(calculated from doping)
ALUminum	4.17
P.polysilicon	$4.17 + Egap$
N.polysilicon	4.17
MOLybdenum	4.53
TUNgsten	4.63
MO.disilicide	4.80
TU.disilicide	4.80
.TE
or
.TS
l l l.
Workfunction	\\=	<real>
.TE
The work function is set to the above values for the standard materials,
or to the given value. The value is interpreted in volts.
NEUTRAL (the default) stands for charge-neutral (ohmic).
.sp 2
.IP "<special conditions>"
.sp 2
.TS
l l l l.
Surf.rec	\\=	<logical>	(default false)
VSURFN	\\=	<real>	Surface recombination rate for electrons
VSURFP	\\=	<real>	Surface recombination rate for holes
BArrierl	\\=	<logical>	(default false)
ALPha	\\=	<real>	(default is 0)
	or	
CUrrent	\\=	<real>	(default is false)
	or	
Resistance	\\=	<real>	(default is 0)
	or	
CApacitance	\\=	<real>	(default is 0)
	or	
COn.resist	\\=	<real>	(default is 0)
.TE
If none of the above are specified, this contact is a normal
ohmic (Dirichlet) boundary condition.
If SURF.REC is specified, finite surface recombination velocities
are used at the respective contact.  
VSURFN and VSURFP have the units cm/sec;
their defaults are calculated using equations 2.45 and 2.46 [1].
BARRIERL is a flag to turn-on the barrier lowering mechanism,
while ALPHA is the linear, dipole barrier lowering 
coefficient (see chapter 2 of this report) and has the units of cm.
CURRENT denotes a current boundary condition.
RESISTANCE specifies a lumped resistance value ($OMEGA - mu$m)
and CAPACITANCE a lumped capacitance value ($OMEGA / mu$m)
to be attached to the contact.
CON.RESIST is a distributed contact resistance in $OMEGA - roman{cm} sup 2$.
See chapter 2 of this report for details on current, lumped element and
distributed-resistance boundary conditions.
.RE
.sp 2
.NH
Examples
.IP     \"Take it from here
Define all electrodes except number 2 to be neutral, and number 2
is aluminum.  Besides a workfunction, electrode number 2 also
includes finite surface recombination velocities and barrier lowering.
Note that the definition on the second card overrides that of the first.
.P1
CONTACT ALL NEUTRAL
CONTACT NUM=2 ALUM SURF BARR
.P2
.sp 2
.IP 
Attach a lumped resistor to contact number 2 with a value of
$10 sup 5 OMEGA - mu $m.  Include distibuted contact resistance
($10 sup {-6} OMEGA - roman {cm} sup 2 ~ approx $ Aluminum) on contact\ 4.
.P1
CONTACT NUM=2 RESIS=1E5
CONTACT NUM=4 CON.RES=1E-6
.P2
.eC
