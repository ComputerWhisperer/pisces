.EQ
delim @@
define XX " times^ 10 sup "
.EN
.bC METHOD
.NH  0
Syntax
.sp
.R
.ce 
METhod <general parameters> <method-dependent parameters>
.NH 
Description
.IP 
The METHOD card sets parameters associated with the particular
solution algorithm chosen on the SYMBOLIC card.  There can be more
than one METHOD card in a single simulation, so that parameters
can be altered.  The default values of the
parameters are used on the first occurrence of the METHOD card;
subsequent METHOD cards only alter those coefficients specified.
.NH
Parameters
.sp 2
.RS
.IP "<general parameters>"
.sp 1
.TS
c s s c
l l l l.
\fIParameter	Default\fR
ITlimit	\\=	<integer>	15
.br
Xnorm	\\=	<logical>	true
.br
Rhsnorm	\\=	<logical>	false
.br
P.toler	\\=	<real>	Depends on choice of norm
.br
C.toler	\\=	<real>	Depends on choice of norm
.br
LImit	\\=	<logical>	false
.br
PRint	\\=	<logical>	false
.br
Fix.qf	\\=	<logical>	false
.br
TRap	\\=	<logical>	false
.br
ATrap	\\=	<real>	0.5
.TE
.IP
The above parameters are used to determine the convergence
of the solution methods.
ITLIMIT is the maximum number of allowed outer loops (i.e., 
Newton loops or Gummel continuity iterations).  P.TOL and 
C.TOL are the termination criteria for the Poisson and continuity
equations, respectively. If XNORM is chosen as the error norm,
the Poisson updates are measured in units of @ kT / q @, and carrier updates
are measured relative to the local carrier concentration. In this case
the default value for both P.TOL and C.TOL is @ 1 XX -5 @.
If RHSNORM is selected, the Poisson 
error is measured in C/\(*mm and the continuity error in
A/\(*mm. P.TOL then defaults to
@ 1 XX -26 C/\(*mm @, and C.TOL to @ 5 XX -18 A/\(*mm @.
.IP
LIMIT indicates that the convergence criterion should be ignored,
and iterations are to proceed until ITLIMIT is reached.
PRINT prints the terminal fluxes/currents after each continuity
iteration; if this parameter is not set, the terminal fluxes/currents
are only printed after the solution converges.  
FIX.QF fixes the quasi-Fermi potential of each non-solved for
carrier to a single value, instead of picking a value based
on local bias (see section 5 of chapter 2 [1] and the ``p.bias''
and ``n.bias'' parameters on the SOLVE card).
TRAP specifies
that if a solution process starts to diverge, the electrode bias 
steps taken from
the initial guess are reduced by the multiplicative factor ATRAP.
.sp 2
.IP "<method-dependent parameters>"
.sp 2
For the Gummel method:
.in +4
.sp
The following parameters are for damping the Poisson updates.
.in +4
.TS
l l l l.
DVlimit 	\\=	<real>   	(default is 0.1)
	or
DAMPEd	\\=	<logical>	(default is false)
DElta	\\=	<real>	(default is 0.5)
DAMPLoop	\\=	<integer>	(default is 10)
DFactor	\\=	<real>	(default is 10.0)
.TE
.in -4
The DVLIMIT parameter limits the maximum @ psi @ update for a single loop.
The DAMPED parameter indicates the use of a more
sophisticated damping scheme proposed by Bank and Rose 
(this is the recommended option, particularly for large bias steps).
The remaining damping parameters are only interpreted if the DAMPED
parameter is specified.  DELTA is the threshold for determining the
damping factor for @ DELTA psi @ and must be between 0 and 1.  
DAMPLOOP is the
maximum number of damping loops allowed to find a suitable damping
coefficient.  DFACTOR is a factor which serves to increase the
initial damping coefficient for the next Newton loop.
.sp
The following parameters select acceleration methods for the
Gummel iteration.
.in +4
.TS
l l l l.
SInglepoisson	\\=	<logical>	(default is false)
ICcg	\\=	<logical>	(default is false)
LU1cri\	\\=	<real>	(default is @3 XX -3) @
LU2cri\	\\=	<real>	(default is @3 XX -2) @
Maxinner	\\=	<integer>	(default is 25)
ACCEleration	\\=	<logical>	(default is false)
ACCSTArt	\\=	<real>	(default is 0.3)
ACCSTOp	\\=	<real>	(default is 0.6)
ACCSTEp	\\=	<real>	(default is 0.04)
.TE
.in -4
The first two parameters specify how the Poisson equation iterations
are to be performed.  
The SINGLEPOISSON option indicates that only a single Poisson iteration
is to be performed per Gummel loop as opposed to the default where
the continuity equation is only treated after Poisson has fully converged.
The ICCG parameter chooses whether or not to use iteration to
solve the multi-Poisson loops. It should be set whenever doing
multi-Poisson. The next two parameters specify how much work
is done per Poisson loop (cf. Section 3.3 [1]).
The inner norm is required to decrease by
at least LU1CRI before returning, or to reach a factor of 
LU2CRI below
the projected Newton error, whichever is the smaller.
(If the inner norm is allowed to exceed the projected Newton error, 
quadratic convergence is lost).
MAXINNER sets the maximum number of ICCG iterations.
The remaining parameters deal with
an acceleration method for attaining faster overall convergence in
the single-Poisson mode.
The ACCELERATION option specifies that acceleration 
is to be used.  ACCSTART is the 
starting value of the acceleration parameter, ACCSTOP is the final
(limiting) value of the acceleration parameter and ACCSTEP is the
step to be added to the value of the acceleration parameter
after each iteration [1].
.in -4
.sp 2
For the direct Newton method:
.in +4
.in +4
.TS
center;
l l l l.
AUtonr	\\=	<logical>	(default is false)
NRcriterion	\\=	<real>	(default is 0.1)
2ndorder	\\=	<logical>	(default is true)
TAuto	\\=	<logical>	
TOl.time	\\=	<real>	(default is @ 5 XX -3 @)
L2norm	\\=	<logical>	(default is true)
Dtmin	\\=	<real>	(default is @ 1 XX -25 @)
Extrapolate	\\=	<logical>	(default is false)
.TE
.in -4
The first two of the above parameters are for implementing 
an automated Newton-Richardson
procedure which attempts to reduce the number of LU decompositions per
bias point.  The AUTONR option indicates that this algorithm is to
be used.  NRCRITERION is the ratio by which the norm from the previous
Newton loop must go down in order to be able to use the same Jacobian 
(i.e., LU decomposition) for the current Newton loop. This is strongly
recommended for full Newton iteration.
.sp
The remaining parameters are for transient simulations.
2NDORDER specifies that the second-order discretization
of Bank, et. al (see chapter 2 of this report) be used as
opposed to the first-order backward difference of \s-2PISCES-IIA\s+2.
TAUTO forces \s-2PISCES-IIB\s+2 to select time-steps
automatically from the local truncation error estimates.
Note that automatic time-stepping is the default for the
second-order discretization but is not allowed for the
first-order scheme.
TOL.TIME is the maximum allowed local truncation error.
L2NORM specifies that the error norms be L2 as opposed
to infinity norms for calculating the time-steps.
DT.MIN is the minimum time-step allowed in seconds, and
EXTRAPOLATE uses a second-order extrapolation to compute
initial guesses for successive time-steps.
.in -4
.RE
.in 0
.sp 2
.NH
Examples
.IP
.sp 2
The following specifies that for a simulation using the Gummel
method (as previously specified by an appropriate symbolic card),
that damping is to be employed and the Poisson error tolerance
should be @ 1 XX -30 @ coul/\(*mm.  Note that because XNORM
defaults to true, XNORM must be turned off to use the rhs norm
as a convergence criterion.  If XNORM=FALSE had not been specified,
the rhs norm and the update norm would have both been printed, but
only the update norm would have been used to determine convergence.
.sp
.in +4
.ss 24
METHOD  DAMPED P.TOL=1.e-30 RHSNORM XNORM=FALSE
.in -4
.sp 2
The next example illustrates the trap feature, which
can be quite useful for capturing knees of IV curves for
devices such as SCRs.
The first SOLVE card solves for the initial, zero bias case.
On the second SOLVE card, we attempt to solve for V2=3 volts
V3=5 volts.  If such a large bias change caused the solution
algorithms to diverge for this bias point, the bias steps would
be multiplied by ATRAP(0.5); i.e., an intermediate point
(V2=1.5 volts, V3=2.5 volts) would be attempted before trying to
obtain V2=3 volts and V3=5 volts again.  If the intermediate point can
not be solved for either, \*(PI will continue to reduce the
bias step (the next would be V2=0.75 volts and V3=1.25 volts)
up to 4 times.  Note also that the intermediate solutions
will be saved in output files in a manner similar to voltage stepping
using the VSTEP parameter on the SOLVE card;
i.e., if two intermediate steps to V2/V3=3/5 volts were required,
they would be stored in ``outa'' and ``outb'' while V2/V3=3/5 volts
would be stored in ``outc''.
.sp
.ss 24
.in +4
METHOD  TRAP ATRAP=0.5
.br
SOLVE   INIT
.br
SOLVE   V2=3 V3=5 OUTFILE=outa
.in -4
.sp 2
Finally, an example of transient simulation.  By default,
the second-order discretization is used, but the required
LTE, @ 1 XX -3 @, is smaller than the default.
Newton-Richardson is also used.  Note that because
the Jacobian is exact for the second part (BDF-2) of
the composite time-step, there should be very few
factorizations for the BDF-2 interval when AUTONR is specified
(see chapter 2 of this report).
.sp
.in +4
.ss 24
METHOD  TOL.TIME=1E-3 AUTONR
.in -4
.fi
.eC
