.EQ
delim $$
tdefine ppd % "\o'\(ru\s-3\(or\s+3'" %
ndefine ppd % | %
tdefine || % \(or\(or %
tdefine angstrom % "\fR\zA\v'-.3m'\h'.2m'\(de\v'.3m'\fP\h'.2m'" %
ndefine angstrom % A to o %
.EN
.nr VS 24
.LP
.TL
Improved Local-Field Electron and Hole Mobilities for MOS Inversion Layers
in PISCES-IIB Release 9009
.sp
.LP
Any local-field-dependent mobility model for electrons and holes in
the inversion
layer should satisfy the equation below in order to replicate the experimentally
observed effective (or average) mobility, $mu sub {eff}$, when the local-field
mobility $mu$ is
integrated across the inversion layer to calculate the effective
mobility [1].
.sp
.EQ
mu ( E sub {||}~,~E sub {ppd}~)~=~mu sub eff left ( E sub {||}~,~{E sub {ppd}~+~
E sub o } over 2 right )~+~ ( E sub ppd~-~E sub o )
{{d mu sub eff} over {d E sub {ppd}}}
.EN
.LP
where
.IP
$E sub ||$: longitudinal electric field parallel to the Si/SiO2 interface.
.IP
$E sub ppd$: transverse electric field.
.IP
$E sub o$: transverse electric field at the boundary between the inversion
layer and the depletion layer.
.LP
New transverse-field-dependent mobility models for electrons and holes
have been derived by applying the above equation to the new physically-based
semi-empirical equations for electron and hole effective mobility [2,3]. These
effective mobility models account explicitly for surface
roughness scattering and
screened Coulomb scattering in addition to phonon scattering, and they exhibit
excellent agreement with considerable experimentally measured mobility data.
Therefore, the new local mobility models [2,3], $mu$,
developed with the approach described in Reference [1] replicate the
experimentally measured $mu sub{eff}$ when
integrated across the inversion layer. As a result, the
simulated I-V characteristics
using these new mobility models are expected to match the
measured data very well.
.LP
The new models require some restrictions in the grid structure. First, it is
recommended to have at least 10 nodal points in the vertical
direction for the first
200$angstrom$ of the surface region in the silicon at Si/SiO2
interface. This mesh structure is dictated by the necessity for
accurately computing the transverse field
within the inversion layer which is not possible with a coarse grid. Second, the
nodes within the inversion layer should be aligned vertically.
It is because, for the nodes on each vertical line, a search is
performed beginning with the surface
node to find the node where the carrier concentration drops below the substrate
doping level. This point is the edge of the inversion layer.
.LP 
The following parameters have been added to the \fBMODEL\fP card:
.IP
\fBTFLDMB\fP: Specifies the use of the new mobility model extracted from the
experimentally measured effective mobility including surface
roughness and Coulomb scattering [2,3]. This is the recommended
model.
.IP
\fBOLDTFL\fP: Specifies that a mobility model extracted from Schwarz and
Russek's equation [4] is used (This is an early version of the new
modeling approach as discussed in [1]). 
.IP
\fBACC.SF\fP: The low-field surface reduction factor for accumulation layers.
It is recommended that 0.87 be used for the electron case [5].
.IP
\fBINV.SF\fP: The low-field surface reduction factor for the inversion layer.
This reduction factor is applied to the Si/SiO2 interface nodes when
the inversion is weak (the carrier concentration is less than the doping
level at the interface).
.IP
\fBOX.LEFT\fP: The x coordinate of the left edge of gate electrode.
.IP
\fBOX.RIGHT\fP: The x coordinate of the right edge of gate electrode.
.IP
\fBOX.BOTTOM\fP: The y coordinate of the Si/SiO2 interface at the channel region.
.LP
.sp
\fBReferences:\fP
.IP
[1] H. Shin et al., \fIIEEE Trans. Elect. Dev.\fP, ED-36, 1117 (1989).
.IP
[2] H. Shin et al., accepted for publication in Solid-State Electronics, 1990.
.IP
[3] V.M. Agostinelli et al., NUPAD III Tech. Dig.,  p. 39, 1990.
.IP
[4] S.A. Schwarz et al., \fIIEEE Trans. Elect. Dev.\fP, ED-30, 1634 (1983).
.IP
[5] S.C. Sun et al., \fIIEEE Trans. Elect. Dev.\fP, ED-27, 1497 (1980).
