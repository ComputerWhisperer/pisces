.\" Mon Nov 26 22:27:19 PST 1990 (anderson--Stanford)
.EQ
delim @@
.EN
.bC MODELS
.NH  0
Syntax
.sp 2
.R
.in +4
MOdels  <model flags> <numerical parameters>
.in -4
.sp
.NH 
Description
.IP
The model card sets the temperature for the simulation and 
specifies model flags to indicate the inclusion of various 
physical mechanisms and models.  
.NH
Parameters
.sp 2
.RS
.KS
.IP "<model flags>"
.br
.TS
l l l l.
Srh	\\=	<logical>	(default is false)
CONSrh	\\=	<logical>	(default is false)
AUger	\\=	<logical>	(default is false)
BGn	\\=	<logical>	(default is false)
CONMob	\\=	<logical>	(default is false)
ANalytic	\\=	<logical>	(default is false)
ARORA	\\=	<logical>	(default is false)
FLdmob	\\=	<logical>	(default is false)
.+B
SURFmob	\\=	<logical>	(default is false)
TFLDmob	\\=	<logical>	(default is false)
OLDtfld	\\=	<logical>	(default is false)
CCSmob	\\=	<logical>	(default is false)
USER1	\\=	<logical>	(default is false)
IMPAct	\\=	<logical>	(default is false)
.-B
BOltzmann	\\=	<logical>	(default is true)
FErmidirac	\\=	<logical>	(default is false)
Incomplete	\\=	<logical>	(default is false)
.+B 8940
Photogen	\\=	<logical>	(default is false)
.-B 8940
Print	\\=	<logical>	(default is false)
.TE
.KE
.sp
CONSRH/SRH and AUGER specify Shockley-Read-Hall (eq. 2.6 [1]) and 
Auger recombination (eq. 2.7 [1]) respectively.  SRH uses fixed lifetimes
and CONSRH uses concentration-dependent lifetimes.
(eq. 2.8, 2.9 [1]).
BGN is band-gap narrowing (eq. 2.20 [1]).  CONMOB is 
concentration dependent mobility from tables for 300K
(currently only silicon and gallium arsenide have
been implemented).  ANALYTIC is an analytical concentration
dependent mobility model for silicon only (see chapter 1, section 3 of
[1]) which will include temperature dependence.
.+B 9009
ARORA specifies an alternative concentration dependent mobility model 
for silicon [8].
.-B 9009
FLDMOB specifies a lateral field-dependent model (eq. 2.34, 2.35 [1]).  
.+B
SURFMOB invokes the effective field
based surface mobility model. ([2] [3])  The current implementation
accounts only for phonon scattering at room temperature,
restricting it to effective fields of less than 0.5 MV/cm.
CCSMOB invokes the mobility model of Dorkel and Leturcq[9], which includes
carrier-carrier scattering effects, temperature dependence and concentration
dependence.
TFLDMOB invokes the transverse-field mobility model whose derivation is
described in [10].  OLDTFLD specifies the use of the Schwarz-Russek 
formulation [12], while TFLDMOB invokes an extended version of the
Schwarz-Russek formulation for both holes and electrons [10][11].
USER1 specifies the user-customizable concentration-dependent mobilty model.
Only one of ANALYTIC, ARORA, CCSMOB or USER1 may be specified.
IMPACT invokes the empirical impact ionization
model. ([4] [5])  A more rigorous impact ionization
model can be specified with IMPACT command.
.-B
BOLTZMANN and FERMIDIRAC indicate the carrier statistics to be used
(eq. 2.13, 2.14 and 2.10, 2.11 [1]),
while INCOMPLETE indicates that incomplete-ionization of impurities 
should be accounted for (eq. 2.31, 2.32 [1]).
.+B 8940
PHOTOGEN specifies that photogeneration is to be used; FLUX and ABS.COEF
must also be specified to use this model.
.-B 8940
PRINT prints the status of all models and a variety of
coefficients and constants.
.sp 2
.KS
.IP "<Numerical parameters>"
.sp
.TS
l l l l.
Temperature	\\=	<real>	(default is 300K)
B.Electrons	\\=	<real>	(default is 2)
B.Holes	\\=	<real>	(default is 1)
E0	\\=	<real>	(default is @ 4 times 10 sup 3 @V/cm)
.+B 9009
Flux	\\=	<real>	(default is 0.0 @roman cm sup -2@)
Abs.coef	\\=	<real>	(default is 0.0 @roman cm sup -1@)
Acc.sf	\\=	<real>	(default is 0.87)
Inv.sf	\\=	<real>	(default is 0.75)
Ox.left	\\=	<real>	
Ox.right	\\=	<real>	
Ox.bottom	\\=	<real>	
.-B 9009
.TE
.KE
TEMPERATURE should be specified in Kelvin units.  B.ELECTRONS and
B.HOLES are parameters used in the field-dependent mobility
expression for silicon (eq. 2.34 [1]), while E0 is a parameter used in
the field-dependent mobility model for gallium arsenide (eq. 2.35 [1]).
.+B 8940
FLUX is the incident photon flux at the y=0 surface
in @ photons/{cm sup 2}@, and
ABS.COEF is the optical absorption coefficient 
in @cm sup -1@.
.-B 8940
.+B 9009
ACC.SF is the low-field surface reduction factor for accumulation layers,
used in conjunction with the transverse-field mobility model TFLDMOB.
INV.SF is the inversion layer low-field surface reduction factor 
for the transverse-field mobility model.
OX.LEFT, OX.RIGHT, and OX.BOTTOM define the location of the gate region
for the transverse-field mobility model.
.-B 9009
.RE
.sp 2
.KS
.NH
Examples
.IP
.sp 2
The following example selects concentration dependent mobility
and SRH recombination.  Fermi-dirac statistics are used, and
the simulation is specified to be performed at 290K.
.sp
.in +4
.ss 24
MODELS  CONMOB SRH FERMI TEMP=290
.KE
.fi
.eC
