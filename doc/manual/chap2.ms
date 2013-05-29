.EQ
gsize 10
delim $$
.EN
.so macros
.sp 5
.nr H1 2
.tl '' Chapter 2 ''
.sp 2
.tl ''Major Enhancements''
.sp 4
.NH 2
Transient Device Simulation
.sp 1
.PP
PISCES-II assumes that the electrical behavior of semiconductor devices 
is governed by Poisson's equation
.EQ I (2.1)
epsilon del sup 2 psi ~ = ~ - q left ( p - n + 
N sub D sup + - N sub A sup - right ) - rho sub F
.EN
and the continuity equations for electrons and holes
.EQ I (2.2)
{partial n} over {partial t} ~ = ~ 1 over q del cdot J sub n - U sub n
~ = ~ F sub n left ( psi , n , p right )
.EN
.EQ I (2.3)
{partial p} over {partial t} ~ = ~ - 1 over q del cdot J sub p - U sub p
~ = ~ F sub p left ( psi , n , p right )
.EN
In this section, we are concerned only with the time-dependent
discretization of (2.1)-(2.3).  See [1] and section 2.4 of
this report for a description of the auxiliary physical considerations
and [1-3] for the details of the spatial discretization
for these coupled partial differential equations.
.PP
Because of the extremely stiff nature of (2.1)-(2.3), strong
stability requirements are placed on any proposed transient
integration scheme.  
Additionally, it is most convenient to
use one-step integration methods so that only the solution
at the most recent time-step is required.
Most device simulation codes (including PISCES-IIA)
have therefore made use of a simple first-order (implicit) backward
difference formula [4,5], i.e. (2.2) and (2.3) are discretized as
.EQ I (2.4)
{ n sub {k} - n sub {k-1} } over { DELTA t sub k }
~ = ~ F sub n left ( psi sub {k}, n sub {k}, p sub {k} right )
~ = ~ F sub n left ( k right )
.EN
.EQ I (2.5)
{ p sub {k} - p sub {k-1} } over { DELTA t sub k }
~ = ~ F sub p left ( psi sub {k}, n sub {k}, p sub {k} right )
~ = ~ F sub p left ( k right )
.EN
where $ DELTA t sub k ~ = ~ t sub k - t sub {k-1} $ and $ psi sub k $
denotes the potential at time $t sub k$, etc..
This scheme (BDF1) is a one-step method and is
known to be both A- and L-stable [4,6] but
suffers from a large local truncation error (LTE) which is 
proportional to the size of the time-steps taken, i.e.
the error at each time-step $k$ is $O ( DELTA t sub k ) $ .
.PP
Because it is a one-step, second-order ($O ( DELTA t sub {k} sup {2} ) $), 
A-stable (implicit) 
integration scheme, the trapezoidal rule (TR) [4]
is a very attractive alternative to BDF1 for a number of problems
involving the solution of differential equations.
Applying TR to (2.2) and (2.3) we obtain
.EQ I (2.6)
{ n sub {k} - n sub {k-1} } over { DELTA t sub k }
~ = ~ 1 over 2 left [ 
F sub n left ( k right ) ~ + ~ F sub n left ( k-1 right )
right ]
.EN
.EQ I (2.7)
{ p sub {k} - p sub {k-1} } over { DELTA t sub k }
~ = ~ 1 over 2 left [ 
F sub p left ( k right ) ~ + ~ F sub p left ( k-1 right )
right ]
.EN
Unfortunately, apart from being slightly more difficult
to implement, TR is not in general L-stable so that numerical 
``ringing'' may be observed unless very small time-steps (much
smaller than dictated by LTE considerations) are chosen.  
In fact, this behavior is indeed the case with the semiconductor
device equations, as noted first in [5], due to the extreme
stiffness of the problem as mentioned above.
.PP
An alternative second-order method might be a second-order 
backward difference formula (BDF2) [4], i.e.
.EQ I (2.8)
{ { 2 - gamma } over { 1 - gamma } n sub {k} 
-  1 over { gamma ( 1 - gamma ) } n sub {k-1}
+ { 1 - gamma } over { gamma } n sub {k-2} } over { t sub k - t sub {k-2} }
~ = ~ F sub n left ( k right )
.EN
.EQ I (2.9)
{ { 2 - gamma } over { 1 - gamma } p sub {k} 
-  1 over { gamma ( 1 - gamma ) } p sub {k-1}
+ { 1 - gamma } over { gamma } p sub {k-2} } over { t sub k - t sub {k-2} }
~ = ~ F sub p left ( k right )
.EN
where 
.EQ I (2.10)
gamma ~ = ~ { t sub {k-1} - t sub {k-2} } over { t sub k - t sub {k-2} }
.EN
Like BDF1, BDF2 is both A- and L-stable, but BDF2 requires
two previous solutions at times $k-1$ and $k-2$ (i.e. it is not
a one step method) so that restarting is generally more
difficult.  Additionally, although both are second order methods,
BDF2 has a higher LTE than TR due to the associated proportionality
factor.
.PP
Recently, Bank, et. al. [7] proposed a composite TR-BDF2 method
which is one-step, second-order and both A- and L-stable.
The basic algorithm is to divide each time interval $DELTA t sub {k}$
into two parts - $gamma DELTA t sub {k}$ and 
$ ( 1 - gamma ) DELTA t sub {k}$;
TR (2.6, 2.7) is then used in the first sub-interval,
and BDF2 (2.8, 2.9) is used in the second.
Additionally, if $gamma$ is chosen properly it can be shown that
the Jacobian need not be re-factored for the TR step, 
substantially reducing the cpu time per time-step.
.PP
In PISCES-IIB, the TR-BDF2 composite integration
scheme of Bank, et. al. has been implemented.  
The method has been found to be strictly second-order and
stable for every device and bias condition tested.
In order to simplify the simulation procedure, an automatic 
time-step selection algorithm based on the LTE of the TR-BDF2 method 
has been included.
Specifically, at each time point $t sub k$, the LTE can 
be estimated by the divided-difference formula [7]
.EQ I 
LTE ~ mark = ~ C ( DELTA t sub k ) sup 3 
{d sup 3 F sub {n,p}} over { d t sup 3 } 
.EN
.EQ (2.11)
lineup approx ~ 2 C ( DELTA t sub k ) left [
gamma sup {-1} F sub {n,p} left ( k right )
- gamma sup {-1} ( 1 - gamma ) sup {-1} 
  F sub {n,p} left ( k-1 right )
+ ( 1 - gamma ) sup {-1} 
  F sub {n,p} left ( k-2 right ) right ]
.EN
where C is a pre-defined constant.
The new time-step $DELTA t sub {k+1}$ is then selected by
.EQ I (2.12)
DELTA t sub {k+1} ~ = ~ DELTA t sub {k} 
left [ tol over { || tau sub i || } right ] sup {1 / ^ 3}
.EN
where $tau sub i$ is the relative LTE for each node $i$ and $tol$ is
a specified LTE tolerance.
Special care is taken not to let the time step grow by more than
a certain factor (usually 2).  
On some occasions (usually
with a very large specified LTE tolerance), PISCES will decide to go 
back to the solution for previous time and re-solve with a smaller time-step.
.PP
In PISCES-IIB,
all the user must specify is an initial time-step as well as an
input bias condition (voltage/current, ramp/step).
All the numerical parameters have defaults and most are accessible.
Time-steps are stored for each solution so that a transient 
simulation can be continued from a saved solution file
without specifying a new initial time-step.
Note that the intermediate solution of the composite
step is not printed or saved.
See the descriptions of the METHOD and SOLVE cards in the appendix and
the example in chapter 5 for more details.
.PP
One final comment on general transient device simulation :
if a voltage step (or ramp, which is just small incremental steps)
is applied directly to a semiconductor contact (i.e., not through
a resistor - see next section), the resultant terminal currents
will include an impulse response term (delta function) due to the
infinitely fast change in voltage and the intrinsic capacitance
of the device, which depends on the size of the simulation
space and dielectric constants.  
It may be advisable to try and resolve this 
component if a voltage input is being applied directly.
.sp 1
.NH 2
New Boundary Conditions
.sp 1
.PP
PISCES-IIA included the basically four types of boundary 
conditions : ohmic contacts, Schottky contacts,
insulator contacts and Neumann (reflective) boundaries 
(see section 2.4 of [1]).
Continued use of the program for a number of specific 
applications has led to the implementation in PISCES-IIB of
additional (supplemental) boundary conditions.
First, in order to save grid space, it would be quite beneficial
to include lumped resistances and/or capacitances between applied
biases and semiconductor device contacts.  
Second, for devices such as SCR's where current is a
multi-valued function of applied voltage, a current
boundary condition would be quite useful.  
Finally, to account for the finite resistivity of contacts
to the semiconductor, a true distributed contact resistance
might be desirable.  
These three auxiliary boundary conditions are described in
detail below.
.sp 1
.NH 3
Lumped elements
.sp 1
.PP
The need for lumped elements was basically motivated by two
desires: to cut down on the number of grid points used to 
discretize some device structures (i.e., to save cpu time)
and to handle transient simulation in a more realistic
manner (see comments in section 1 of this chapter).
A typical example of a case where a lumped resistance
is useful is shown in Figure 1.  The p-tub (base) contact
in this CMOS cross-section might be tens or hundreds of
microns away from the active area of the embedded vertical
npn bipolar transistor, which may only be $10-20 mu $m on
a side.  If the whole structure were to be simulated, a
tremendous number of grid points, probably more than half,
would be wasted in order to account for a purely resistive
region of the device.  Because cpu time is generally
a super-linear function of the number of grid points for
all numerical methods of interest [2], including such 
regions explicitly can be quite expensive.
Another example might be the 
substrate of a MOSFET, where we most certainly would not want to
include grid points all the way to the back side of a wafer!
In both of these cases, a simple lumped resistance
might be substituted. Of course similar arguments hold true for
capacitance.
.PP
Figure 2 shows a schematic of how these boundary conditions
might be implemented  (momentarily ignore the current source -
it will be treated later).  An extra unknown, the voltage on
the semiconductor contact ($phi$), must be included and
is defined by a Kirchoff equation:
.EQ I (2.13)
{ V sub {app} - phi } over R + C { d ( V sub {app} - phi ) } over {dt}
- sum from i=1 to {N sub b} (J sub n + J sub p + J sub {disp} ) sub i 
~ = ~ 0
.EN
where $N sub b$ is the number of boundary grid points associated
with the electrode of interest.  Note that this auxiliary
equation, due to the currents inside the summation, 
has dependencies on the values
of potential and carrier concentrations (represented as $x$ in
the figure) at the nodes on the electrode as well as all 
nodes directly adjacent to the electrode.
Note also that the temporal term associated with
the capacitor must be discretized in a manner consistent with
the device equations (see section 1 above).
It is important to remember that in contrast to the distributed
resistance to be described below, a lumped element contact
will have a single voltage (or potential - adjusted for possible
doping non-uniformities) associated with the entire
electrode.
.PP
Within PISCES-IIB, lumped resistances and capacitances are specified
on the CONTACT card (see appendix).  Note that because an auxiliary
equation must be added to the system, the symbolic matrix factorization
(SYMBOL card) must follow all CONTACT cards in an input deck.
The values of the resistances should be in units of $OMEGA - mu$m while
capacitance should have the units F/$mu$m.  Remember that
capacitance increases with device width (into the z plane) while
conversely, resistance decreases.
Except for the case of extremely large resistances, where the
arrangement becomes similar to a pure current source (see below),
no convergence degradation has been observed for a lumped element
boundary in comparison to a simple ohmic contact.  Further, as 
mentioned in section 1 above, transient simulation becomes
easier and is more well-defined.
.PP
One final comment: one should always make use of the simulator
as much as possible to help calculate any resistance (or capacitance)
components that might be included as lumped elements.  For instance in
the case of Figure 1, one could simulate just the p-tub with
ohmic contacts at either end.  From the plot of
terminal current (in A/$mu$m) versus voltage, the resistance
can be directly extracted from the slope.  Be very careful
however to account for any three dimensional effects (e.g.,
current spreading) before using such a resistance value in further
simulations.
.sp 1
.NH 3
Current boundary conditions
.sp 1
.PP
One of the first applications of PISCES-II was in the analysis of
CMOS latch-up triggering [8].  Shown in Figure 3 are 
simulated (by PISCES-II) ``low'' current
IV characteristics of two different embedded SCR structures
found in a typical VLSI CMOS process as used in [8].  
Note that as mentioned previously, the terminal
current is a multi-valued function of the applied voltage.
This condition implies that for some voltage boundary conditions,
a numerical procedure may end up - depending on the initial
guess - with a solution in either of
two (actually three - the third, higher current state is not shown)
distinct and stable states.  Furthermore, the condition
of primary interest is the point at which $dV/dI ~ = 0 $, known
as the trigger point, which is exceedingly difficult to obtain
with a simple voltage boundary condition.  Additionally, it
is nearly impossible to compute any solutions in the negative
resistance regime with voltage inputs.
.PP
A possible alternative would be to define a current boundary
condition, since voltage can be thought of as a single-valued
function of the terminal current (just turn Figure 3 on its side).
Such a boundary condition has been implemented within PISCES-IIB,
again as an auxiliary equation with an additional unknown
boundary potential.  Like the lumped R/C case, a Kirchoff equation
is written at the electrode :
.EQ I (2.14)
J sub {source}
- sum from i=1 to {N sub b} (J sub n + J sub p + J sub {disp} ) sub i 
~ = ~ 0
.EN
Note that unlike the lumped R/C case,
$J sub {source}$ is constant and has no dependence on
the boundary potential $phi$ (the $phi$ dependence is buried
in the summation).  Because of this weaker dependence on $phi$,
the convergence of the non-linear Newton iteration is affected,
but not always for the worse.  Table I shows the number of
Newton loops needed for various bias conditions using
both voltage and current boundary conditions
(with ``previous'' solution used as initial guess).
.sp 1
.ce
Table I - Voltage vs. Current BC
.sp 1
.TS 
center allbox;
c c c c
n c n n.
$V sub F$	$I$ (Amps/$mu$m)	Voltage BC	Current BC
0	$0.00 roman{x} 10 sup 0$	8	-
0.1v	$8.02 roman{x} 10 sup -15$	5	6
0.2v	$9.50 roman{x} 10 sup -14$	5	7
0.3v	$1.96 roman{x} 10 sup -12$	5	8
0.4v	$6.98 roman{x} 10 sup -11$	5	9
0.5v	$2.92 roman{x} 10 sup -9$	5	9
0.6v	$8.07 roman{x} 10 sup -8$	6	8
0.7v	$9.11 roman{x} 10 sup -7$	7	6
0.8v	$6.73 roman{x} 10 sup -6$	6	5
0.9v	$3.45 roman{x} 10 sup -5$	6	5
1.0v	$7.48 roman{x} 10 sup -5$	5	5
1.1v	$1.01 roman{x} 10 sup -4$	5	4
1.2v	$1.23 roman{x} 10 sup -4$	5	4
1.3v	$1.43 roman{x} 10 sup -4$	4	4
.TE
.sp 1
Note that for regions where a small $dI/dV$ is small,
the voltage boundary condition is definitely preferable.
However, as evidenced by its slight advantage as the diode turns on,
the current boundary condition may be preferable for
operating regimes where $dI/dV$ is large.  One note here:
it is not uncommon for the negative resistance regime of
an SCR to have a slope $dI/dV$ very close to 0.
Such behavior should be considered when using a current source
to trace out an entire IV curve;
i.e., it might be preferable to switch back to a voltage source
after passing the trigger point.
.sp 1
.NH 3
Distributed contact resistance
.sp 1
.PP
As mentioned above, for a contact with a lumped element
or for a simple voltage boundary condition,
a single potential is associated with the entire electrode.
However, because contact materials have finite resistivities,
the electrostatic potential is not truly uniform along the
metal-semiconductor interface.  To account for this effect,
a distributed contact resistance can be associated with any
electrode in a PISCES-IIB simulation.
.PP
The implementation of distributed contact resistance
is as follows :
PISCES-IIB internally places a resistance $R sub i$ 
at each node $i$ associated with the contact of interest.
The value of each $R sub i$ is computed from the
contact resistance, $ rho sub c $ (in $OMEGA - roman {cm} sup 2$) as
.EQ I (2.15)
R sub i ~ = ~ rho sub c d sub i
.EN
where $d sub i$ is the length of the contact associated with node $i$.
An auxiliary equation is then added for each electrode node, unlike
the lumped element case where a single equation is added for the
entire electrode.  So for every node $i$ that is part of the contact,
.EQ I (2.16)
{ V sub {app} - left ( psi sub i +- {kT} over q log N over {n sub i} right ) }
over { R sub i } - (J sub n + J sub p + J sub {disp} ) sub i ~ = ~ 0
.EN
Note however that the extra equations added to
the system are strictly local in nature.
Only the current at node $i$ is included in (2.16)
as opposed to the summation over the contact
in (2.13), so that there is no direct coupling between
non-adjacent electrode nodes and neighbors.
As such it is actually somewhat simpler to implement numerically than
the lumped element or current boundary conditions.
.PP
Figure 4 shows a typical example as simulated by PISCES-IIB.
The device under consideration 
(Figure 4b) is a uniformly doped slab ($4 mu$m by $0.2 mu$m)
of n-type silicon with a sheet resistance of $25 OMEGA / $sq
(note that the figures are not drawn to scale).  Figure 4b,c
shows the current vectors for 0 and $10 sup {-6} OMEGA - roman {cm} sup 2$
respectively with V=0.2v.  Note the way the current crowds into the
nearest corner of the electrode on the right in Figure 4a, whereas
in Figure 4b, the current is evenly distributed along the contact.
As evidenced by the magnitude of
the arrows, the current with 0 contact resistance is much larger.
Figure 4c shows the IV characteristics for both structures.
The slope of the line corresponding to the device with contact
resistance is equivalent to a total resistance of 
$approx ~ 116 OMEGA - mu$m
which agrees exactly with the analytical solution for this case.
.sp 1
.NH 2
Small-Signal Analysis
.sp 1
.PP
PISCES-IIA included two basic types of electrical analysis:
DC steady state and transient analysis.  In PISCES-IIB, in
addition to enhancing the transient analysis (see section 1
of this chapter), AC small-signal analysis has been added.
Specifically, starting from a DC bias condition, an input
of given amplitude and frequency can be applied to a device
structure from which
sinusoidal terminal currents and voltages are calculated.
Then using the relationship
.EQ I (2.17)
Y tilde sub {ij} ~ = ~ G sub {ij} + j omega C sub {ij} 
~ = ~ { I tilde sub i } over { V tilde sub j } 
~ ~ ~ ~ , ~ ~ ~ ~ V tilde sub k ~ = ~ 0 ~ ~ k != j
.EN
the frequency dependent admittance matrix, and hence
capacitance, can be calculated.  Note also that by
varying the frequency and examining the various
device admittances, $f sub T$ can be directly determined.
.PP
PISCES-IIA has been successfully applied to the calculation of
bias dependent gate capacitances for small geometry
MOSFETs [9].  By solving for a bias point 
($V sub G$, $V sub D$, $V sub S$, $V sub B$) 
and then solving again for
($V sub G$, $V sub D + DELTA V$, $V sub S$, $V sub B$),
one can obtain very accurate estimates of capacitance by then 
integrating the charge on the gate electrode at both
bias points, i.e.
.EQ I (2.18)
C sub {ij} ~ = ~ C sub {GD} ~ = ~ {d Q sub G} over {d V sub D} 
~ approx ~ {DELTA Q sub G} over {DELTA V}
.EN
Of course, this procedure can be repeated to obtain
$C sub {GS}$ and $C sub {GB}$ by applying the potential
increment to the source and bulk respectively.
Unfortunately, as pointed out by Laux [10], this
technique of charge partitioning is only rigorously correct for
cases where the current into electrode $i$ (to calculate $C sub {ij}$)
is strictly displacement current, i.e. insulator contacts like MOS gates.
Therefore, capacitances such as $C sub {DG}$
and $C sub {SG}$ cannot be accurately estimated by this method.
Additionally, the capacitances obtained via this method are
strictly quasi-static (low frequency) values.
.PP
Using sinusoidal steady-state analysis, both of the drawbacks
associated with the charge partitioning approach are eliminated.
In PISCES-IIB, the approach of Laux [10] has been followed.
An ac sinusoidal voltage bias is applied to an electrode $i$
such that 
.EQ I (2.19)
V sub i ~ = ~ V sub i0 + V tilde sub i e sup {j omega t}
.EN
where $V sub i0$ is the existing dc bias, $V tilde sub i$ is
the magnitude of the ac sinusoidal bias and $V sub i $ is 
the actual bias (sum) to be simulated.
Rearranging the basic partial differential equations (2.1)-(2.3),
one obtains
.EQ I (2.20)
F sub psi left ( psi , n , p right ) ~ = ~ 
epsilon del sup 2 psi + q left ( p - n + 
N sub D sup + - N sub A sup - right ) + rho sub F ~ = ~ 0
.EN
.EQ I (2.21)
F sub n left ( psi , n , p right ) ~ = ~
1 over q del cdot J sub n - U sub n
~ = ~ {partial n} over {partial t} 
.EN
.EQ I (2.22)
F sub p left ( psi , n , p right ) ~ = ~
- 1 over q del cdot J sub p - U sub p
~ = ~ {partial p} over {partial t}
.EN
The ac solution to (2.20)-(2.22) can be written as
.EQ I (2.23)
psi sub i ~ = ~ psi sub {i0} ~ + ~ psi tilde sub i e sup {j omega t}
.EN
.EQ I (2.24)
n sub i ~ = ~ n sub {i0} ~ + ~ n tilde sub i e sup {j omega t}
.EN
.EQ I (2.25)
p sub i ~ = ~ p sub {i0} ~ + ~ p tilde sub i e sup {j omega t}
.EN
where $psi sub {i0}$, $n sub {i0}$ and $p sub {i0}$ are the dc
potential and carrier concentrations at node $i$ while 
$psi tilde sub i$, $n tilde sub i$ and $p tilde sub i$ are
the respective ac values, which in general are complex.
By substituting (2.23)-(2.25) back into (2.20)-(2.22) and expanding
as a Taylor series to first-order only (the small-signal approximation),
we obtain non-linear equations of the form
.EQ I (2.26)
F left ( psi , n , p  right ) ~ = ~ 
F left ( psi sub 0 , n sub 0 , p sub 0 right )
+ {d F} over {d psi} ( psi tilde e sup {j omega t} )
+ {d F} over {d n} ( n tilde e sup {j omega t} )
+ {d F} over {d p} ( p tilde e sup {j omega t} )
.EN
for each of the three PDE's.
If we have already have computed a valid
dc solution at the desired dc bias, then 
$ F  left ( psi sub 0 , n sub 0 , p sub 0 right ) ~ = ~ 0$ so
the following linear system is obtained:
.EQ I (2.27)
left [ matrix {
 ccol { 
   { {d F sub psi} over {d psi} } above
   { {d F sub n  } over {d psi} } above
   { {d F sub p  } over {d psi} }
      }
 ccol { 
   { {d F sub psi} over {d n} } above
   { {d F sub n  } over {d n} + { D sub 1 } } above
   { {d F sub p  } over {d n} }
      }
 ccol { 
   { {d F sub psi} over {d p} } above
   { {d F sub n  } over {d p} } above
   { {d F sub p  } over {d p} + { D sub 1 } }
      }
       } right ]
left [ pile { {psi tilde} above {n tilde} above {p tilde} } right ]
~ = ~ b sub 1
.EN
where each of the $dF/d psi$, $dF/dn$ and $dF/dp$ are n-by-n
matrices which form the dc Jacobian,
$D sub 1$ is an n-by-n matrix with $- j omega$ on the diagonal
and 0 off-diagonal (resulting from the expansion of the
time-dependent portion of each continuity equation) and
$b sub 1$ is a vector of length 3n which contains the ac input
voltage boundary conditions.
.PP
Splitting the system (2.27) into real and imaginary parts, one
obtains
.EQ I (2.28)
left [ matrix { ccol { J above {D sub 2} } 
                ccol { {-D sub 2} above J } } right ]
left [ pile { {X sub R} above {X sub I} } right ]
~ = ~ b sub 2
.EN
where $J$ is the 3n-by-3n dc Jacobian, $D sub 2$ is a 3n-by-3n diagonal
matrix (related to $D sub 1$ above),
$b sub 2$ is a permuted version of $b sub 1$ above and $X sub R$ and 
$X sub I$ refer to the real and imaginary ac solution vectors,
each 3n in length.
The system (2.28) is twice the size of the dc system.
However, because we have already factored $J$
if the full Newton method was used (required by PISCES for this
analysis), an iterative method like SOR can be used on (2.28)
to obtain the ac solution.  Convergence is generally very good
for low to moderate frequencies; however as the frequency
reaches rather high values ($ approx f sub T / 10$), 
more iterations are necessary.
For frequencies above $f sub T / 10$, a relaxation parameter with a 
value less than 1 may have to be used.
For exceedingly high frequencies, SOR will not converge at all.
See Laux [10] for more details.
.PP
PISCES-IIB performs ac analysis as a post-processing
step after a dc solution.
Specific details are given in the documentation
in the appendix regarding the SOLVE card, and an example
of ac analysis is included in the example in chapter 5.  
.sp 1
.NH 2
Enhanced Physics
.sp 1
.PP
PISCES-IIA has a very comprehensive set of models
to accurately describe the actual physical mechanisms associated
with semiconductor device behavior.  However, as mentioned
in chapter 2 of the first technical report on PISCES-II [1],
neither Fermi-Dirac statistics nor incomplete ionization
of impurities were included in version II-A.  They have been
included in PISCES-IIB and are described below.
Additionally, surface recombination at insulator-semiconductor
interfaces and barrier lowering at Schottky contacts
have also now been included.
.PP
We fully realize that there are a number of other important
physical effects for which we have not explicitly included
models.  Therefore, included elsewhere in this document is
a brief discussion on how one might implement new models into
the code.
.sp 1
.NH 3
Fermi-Dirac Statistics 
.sp 1
.PP
The formulation of Fermi-Dirac statistics follows that developed by Yu [11]
where the carrier concentrations are given by equation (2.29):
.sp
.EQ I (2.29a)
n ~~=~~ N sub C gamma sub n e sup {eta sub n}
.EN
.sp
.EQ I (2.29b)
p ~~=~~ N sub V gamma sub p e sup {eta sub p}
.EN
.sp
where $eta sub n$, $eta sub p$, $gamma sub n$ and $gamma sub p$ are
defined in terms of the quasi-fermi levels ( $ E sub Fn $ & $ E sub Fp$ )
and band energies ( $ E sub C $ & $ E sub V$ ):
.sp
.EQ I (2.30a)
eta sub n ~~=~~ {E sub Fn ~-~ E sub C} over {k T}
.EN
.EQ I (2.30b)
eta sub p ~~=~~ {E sub V ~-~ E sub Fp} over {k T}
.EN
.EQ I (2.30c)
gamma sub n ~~=~~ {F sub {1/2} ( eta sub n )} over {exp ( eta sub n )}
.EN
.EQ I (2.30d)
gamma sub p ~~=~~ {F sub {1/2} ( eta sub p )} over {exp ( eta sub p )}
.EN
.sp
In effect, equations (2.29) and (2.30) introduce a form one 
( $ exp ( eta ) / exp ( eta ) $ ) into the Fermi-Dirac statistic.
The identification of the exponential term in (2.29) facilitates
the standard Scharfetter-Gummel discretization of the continuity 
equations. The actual code implementation requires both the
Fermi-Dirac function $ F sub {1/2} ( cdot ) $  and its inverse
$ {F sub {1/2}} sup {-1} ( cdot ) $. The inverse function is calculated
from the Joyce-Dixon approximation [12] given by equation (2.31):
.sp
.EQ I (2.31)
{F sub {1/2}} sup {-1} ( X sub S ) ~=~
eta sub S ~=~ ln ( X sub S ) ~+~ 
a X sub S ~+~ b ( X sub S ) sup 2 ~+~ c ( X sub S ) sup 3 ~+~ 
d ( X sub S ) sup 4
.EN
.sp
where $eta sub S$ represents either $eta sub n$ or $eta sub p$ and
$ X sub S$ denotes either $ n / N sub C $ or $ p / N sub V $. 
The constants 
$a$, $b$, $c$, and $d$ can be found in either [12] or [11].
For values of $ X sub S  ~>~ 8.0 $, the asymptotic expansion for the
Fermi-dirac inverse is used:
.sp
.EQ I (2.32)
eta sub S ~=~ 
left (
left (
{3 sqrt {pi} } over {4} X sub S 
right ) sup {4 / 3}
~-~ {pi sup 2} over {6} 
right ) sup {1 / 2}
.EN
.sp
In either case, the Fermi-Dirac function is calculated from its inverse
through a straight forward inversion process.
.PP
Fermi-Dirac statistics can be selected by setting the parameter 
`FERMIDIRAC = true'
on the MODELS card. It is strongly recommended that incomplete ionization
of donors also be included for accurate simulations (see next section).
.sp 1
.NH 3
Incomplete Ionization
.sp 1
.PP
PISCES-IIB now associates an incomplete ionization to p-type and n-type
doping: 
.sp
.EQ C (2.33a)
{N sub D} sup "+" ~~=~~ {N sub D} over 
{1 ~+~ g sub D exp left ( {E sub Fn - E sub D} over {k T} right ) }
.EN
.EQ C (2.33b)
{N sub A} sup "-" ~~=~~ {N sub A} over 
{1 ~+~ g sub A exp left ( {E sub A - E sub Fp} over {k T} right ) }
.EN
.sp
where $ N sub D $ and $ N sub A $ are net compensated n-type and p-type
doping respectively 
(ie. if $ N sub total ~==~ ( N sub "D,total" ~-~ N sub "A,total" )  ~>~ 0$, 
then 
$ N sub D ~:=~ | N sub total | $ and $ N sub A ~:=~ 0$ else 
$ N sub D ~:=~ 0$ and 
$ N sub A ~:=~ | N sub total | $ ).
.PP
Incomplete Ionization can be selected by setting the parameter 
`INCOMPLETE = true' on the MODELS card. The degeneracy factors
$ g sub D $ and $ g sub A $ are located on the MATERIAL card 
and specified as `GCB' and `GVB' respectively. The donor ionization
energies are also located on the MATERIAL card and are referenced
to the corresponding energy band:
.sp
.EQ (2.34a)
"`EDB'" ~~=~~ E sub C ~-~ E sub D
.EN
.EQ (2.34b)
"`EAB'" ~~=~~ E sub A ~-~ E sub V
.EN
.sp
`EDB' and `EAB' are, in general, positive quantities.
.sp 1
.NH 3
Low Temperature Simulations
.sp 1
.PP
In conjunction with the Fermi-Dirac statistics and donor freeze-out,
the PISCES-IIB program has been restructured to allow low temperature
simulations. In general, simulations can be done down to $ 50 \(de K $
without loss of quadratic convergence.  Below this temperature, the
carrier and ionization statistics develop sharp transitions which
induces severe damping, hence loss of quadratic convergence, in the 
non-linear newton step. Since several iterations are required below 50\(deK,
the `ITLIMIT' parameter on the METHODS card should be increased.
.PP
Due to the limited exponent range on some machines, trouble in calculating 
the quasi-fermi level of minority carriers may be encountered. As the
temperature decrease, the intrinsic carrier concentration $ n sub i $
also decreases (for example $ n sub i ~ approx ~ 10 sup {-10} $ at 
100\(deK for silicon ). In the quasi-neutral regions, the minority carrier 
concentration can easily underflow. PISCES-II has always handled such
situations by setting those concentrations to zero. This, however, does
not allow an accurate post-calculation of the minority carrier quasi-fermi
levels. To compensate the quasi-fermi level calculations, the majority
carrier concentration and the relation $ n p ~=~ {n sub i} sup 2 $ are
used to deduce the minority carrier concentrations should they underflow. 
Despite these efforts,
spurious `glitches' are occasionally observed at low temperatures 
in minority quasi-fermi levels.  
The current calculations, however, are not affected by these glitches as the
semiconductor equations are solved with the $psi$, $n$ and $p$ variable
set.
.PP
Currently, PISCES-IIB sets the underflow limit at $10 sup -38$. 
If your machine has a larger exponent range, this limit can be change
by redefining the `mindbl' and `lgmind' variables in the file pisci.f
( Note that `lgmind' is the base 2 logarithm of `mindbl' ; both must be 
consistent).
.sp 1
.NH 3
Surface Recombination
.sp 1
.PP
PISCES-IIA included two basic recombination mechanisms:
Shockley-Read-Hall recombination and Auger recombination.
Additionally, a special boundary condition for carrier
concentrations was set up to include non-infinite
surface recombination velocities at some electrodes.
It has been found useful to also include an additional
recombination component at specific insulator-semiconductor 
interfaces.  This recombination mechanism can be described
by a surface recombination velocity as described in [13].
.PP
In PISCES-IIB, surface recombination velocities for holes
and electrons can be specified for any interface using the
INTERFACE card (see appendix).  
Note that the INTERFACE card also includes fixed charge densities,
replacing the old QF card in PISCES-IIA.
For each node on the interface so specified, an effective SRH
lifetime for each carrier, $tau sub {n,p} sup {eff}$, is computed 
based on the given recombination velocity, $ s sub {n,p} $, i.e.,
.EQ I (2.35)
1 over { tau sub {n,p} sup {eff} (i) }
~ = ~ {A sub i} over {s sub {n,p} d sub i}
+ 1 over { tau sub {n,p} sup 0 (i)}
.EN
where $ tau sub {n,p} sup 0 (i)$ is the regular SRH lifetime at
node $i$ (possibly concentration dependent) and
$A sub i$ is the semiconductor area and $ d sub i $ is the
length of the interface associated with node $i$.
Different interfaces can be defined separately with arbitrary
recombination velocities at each.
.sp 1
.NH 3
Barrier Lowering
.sp 1
.PP
As mentioned above, PISCES-IIA included a Schottky boundary
condition defined by a work function of the electrode metal
and an optional surface recombination velocity.
Referring to equations (2.42)-(2.44) in [1], 
.EQ I (2.36)
psi sub S ~ = ~ psi sub S0 ~ = ~ 
^ chi ~ + ~ E sub g over {2q} ~ + ~ {kT} over {2q}
ln { N sub C over N sub V } ~ - ~ phi sub m ~ + ~ V sub { applied }
.EN
.EQ I (2.37)
J sub {sn} ~ = ~ ^ q v sub {sn} left ( ^ n sub S - n sub {eq} right )
.EN
.EQ I (2.38)
J sub {sp} ~ = ~ ^ q v sub {sp} left ( ^ p sub S - p sub {eq} right )
.EN
where $psi sub S ~ = ~ psi sub S0$ is the potential at the
contact, $ chi $ is the electron affinity of the semiconductor,
$ phi sub m $ is the work function of the metal, $J sub sn$ and 
$J sub sp$ are the electron and hole contact currents, $n sub S$
and $p sub S$ are the actual hole and electron concentrations
at the contact, $n sub eq$ and $p sub eq$ are the equilibrium
electron and hole concentrations (infinite recombination
velocity) and $v sub sn$ and $v sub sp$ are the electron and
hole surface recombination velocities, respectively.
.PP
In PISCES-IIB, the Schottky model has been enhanced to account
for field-dependent barrier-lowering mechanisms, arising from
image-forces and possible static dipole layers at the 
metal-semiconductor interface [14].  Defining the barrier height
as
.EQ I (2.39)
phi sub bn ~ = ~ phi sub m - chi
.EN
.EQ I (2.40)
phi sub bp ~ = ~ ^ chi ~ + ~ E sub g over {2q} - phi sub m
.EN
then the amount by which these barriers are lowered is expressed as
.EQ I (2.41)
DELTA phi sub b ~ = ~ left [ q over { 4 pi epsilon sub s } right ] 
sup 1/2 E sup 1/2 ~ + ~ alpha E
.EN
where $ E $ is the magnitude of the electric field at the interface.
Note that the term with the square root dependence on electric field
corresponds to the image force, while the linear term corresponds
to the dipole effect.  The coefficient $alpha$ is user accessible
(see the description of the CONTACT card); see [14] for typical
values of $alpha$.
.PP
Barrier lowering had been previously implemented in an experimental 
copy of PISCES [15].  It has been re-incorporated in PISCES-IIB
in a slightly different manner.  
The basic procedure is to solve Poisson equation normally
with the boundary condition 
$psi sub S ~ = ~ psi sub S0$ as in (2.36).  However, using the 
electric field consistent with the solved potentials, effective
surface potentials are computed as
.EQ I (2.42)
psi sub S sub eff ~ = ~ psi sub S0 +- DELTA phi sub b
.EN
where the $+$ is for electrons and the $-$ for holes, respectively.
The continuity equations are then solved using (2.37) and (2.38)
as boundary conditions, but with $n sub S$ and $n sub eq$ replaced
with $n sub S sub eff$ and $n sub eq sub eff$ which are computed
using $psi sub S sub eff$.  The physical interpretation of this
approach is that the Poisson equation is solved consistently
with the charge, but that the electrons and holes see a combined
Poisson and image-force potential.  Note that the full barrier lowering
term has been applied directly at the surface; in reality this peak
occurs a slight distance within the semiconductor.
.PP
In PISCES-IIB, surface recombination is implemented on a triangle
by triangle basis.  That is, using the surface recombination
velocity and geometrical data, a recombination component is calculated
for each triangle that an element of interest in connected to.
Using the electric field for each triangle, an adjusted recombination
term can be computed if barrier lowering is to be incorporated.
This is in contrast to [15] where a single field value
for the electrode node was used to compute a total recombination
value.  Also unlike [15], barrier lowering can be used with any of
the basic numerical solution procedures, i.e. Gummel or Newton.
.sp 1
.NH 2
Other Modifications
.sp 1
.PP
A number of other enhancements of a slightly more minor nature
have been made.  Apart from those which are relatively transparent
(such as modifications to speed up certain parts of the code, etc.),
most are briefly mentioned below.  See the appropriate card
description in the Appendix for a more details.
.sp 1
.NH 3
Ascii file i/o
.sp 1
.PP
In almost all cases where PISCES-IIA used to use a binary
input or output file (MESH, SOLVE, LOAD, REGRID cards), those
files can now be specified as ascii by the inclusion of
a logical parameter.  This feature is useful in that it then
allows transfer of data files between different computer
types.
.sp 1
.NH 3
Mesh and doping
.sp 1
.PP
Several new features have been added to the MESH, REGRID and DOPING 
cards.  First on the MESH card, the user now has the option of directing
the diagonals in a rectangular mesh.  The choices are to have
all the diagonals face the same way or to have symmetry about
the center of the grid as did PISCES-IIA (see Figure 5.9 in [1]).
On the REGRID card, the electric field can now be selected as
a refinement criterion.
On the DOPING card, the latest version of SUPREM-III, supporting
the ``export'' data file format, can be used as input to PISCES.
The original version can still be used as well.  Finally,
PISCES-IIB also allows profiles to be implemented via
ascii (concentration versus depth) data files.  This feature is
useful, for example, if one has experimental profile information.
.sp 1
.NH 3
Plots
.sp 1
.PP
A substantial number of enhancements were performed on the PLOT.1D card.
A number of new quantities can now be plotted (valence and conduction
band potentials, applied and actual biases, x and y components of
any vectors as well as a number of AC quantities).  A few new control
parameters have been added (`x.log', `unchanged', `x.max', `no.order'
and `negative'), and an option to integrate the plotted function
is now available (for example, if one integrates a plot of current
versus time, the result is charge versus time).  Finally, a data
point can be skipped in a log file by replacing the blank in
the column 1 of the first associated line with a `#'.
.PP
The PLOT.2D/CONTOUR cards have some new features as well.  First,
color contour fill has been implemented for the Tektronix 4107
terminal.  Instead of simple contour lines, areas of the device
are color coded and filled, depending on their values.  Second,
as with the PLOT.1D card, one can now plot x and y components of
vectors separately and can plot valence and conduction band 
potentials.  A `negative' option has also been installed.
Third, contours can now be specified by giving a number of
contour lines as opposed to an increment.  Additionally,
the minimum and maximum contour values now default to the minimum 
and maximum values of the quantity to be plotted.
Finally, current flowlines can now be plotted using the algorithm in
[16].  An example is shown in Figure 5.  Here there are 11 flowlines
(specified by setting the number of contours to 11) with 2 invisible
because they lie exactly on the outer boundary of the device.
Basically the interpretation of Figure 5 is that 10% of the current
flows between each line.
.sp 1
.NH 3
Still others
.sp 1
.PP
Very briefly, a few more miscellaneous enhancements.  The CHECK
feature has been substantially enhanced in its output. The PRINT
feature (as with PLOT.1D and CONTOUR) can print x and y components
of vectors individually.  The VECTOR card has an improved syntax
and scaling system.  The METHOD card allows for the specification
of a maximum number of loops for ICCG (this was a fatal error in
PISCES-IIA).  Finally, the SOLVE card now allows transient 
ramps as input in a very painless way.  
.sp 1
.NH 2
References
.IP [1]
M. R. Pinto, C. S. Rafferty and R. W. Dutton, 
``PISCES-II - Poisson and Continuity Equation Solver,''
Stanford Electronics Laboratory Technical Report, Stanford
University, September 1984.
.IP [2]
C. S. Rafferty, M. R. Pinto and R. W. Dutton, ``Iterative
methods in semiconductor device simulation,'' 
\fIIEEE Transactions on Electron Devices\fR, October, 1985.
.IP [3]
C. H. Price, ``Two Dimensional Numerical Simulation of Semiconductor
Devices,'' Ph.D. Dissertation, Stanford University, May, 1982.
.IP [4]
C. W. Gear, \fINumerical Initial Value Problems in Ordinary
Differential Equations\fR, Prentice-Hall, 1971.
.IP [5]
A. De Mari, ``An accurate numerical one-dimensional solution of
the \fIp-n\fR junction under arbitrary transient conditions,''
\fISolid-State Electronics\fR, vol. 11, 1968, pp. 1021-1053.
.IP [6]
J. Lambert, \fIComputational Methods in Ordinary Differential
Equations\fR, Wiley, 1973.
.IP [7]
R. Bank, W. M. Coughran, W. Fichtner, E. H. Grosse, D. J. Rose
and R. K. Smith, ``Transient simulation of silicon devices and
circuits,''
\fIIEEE Transactions on Electron Devices\fR, October, 1985.
.IP [8]
M. R. Pinto and R. W. Dutton, ``Accurate trigger condition analysis for
CMOS latch-up,'' 
\fIIEEE Electron Device Letters\fR, vol. EDL-6, February, 1985.
.IP [9]
H. Iwai, M. R. Pinto, C. S. Rafferty, J. E. Oristian and R. W. Dutton,
``Velocity saturation effect on short channel MOS transistor
capacitance,'' 
\fIIEEE Electron Device Letters\fR, vol. EDL-6, March, 1985.
.IP [10]
S. E. Laux, ``Techniques for small-signal analysis of semiconductor
devices,'' \fIIEEE Transactions on Electron Devices\fR, October, 1985.
.IP [11]
Zhiping Yu and R. W. Dutton, \fISEDAN III - A Generalized Electronic
Material Device Analysis Program\fR,
Stanford Electronics Laboratory Technical Report, Stanford
University, July 1985.
.IP [12]
W. B. Joyce and R. W. Dixon, ``Analytic Approximation for the Fermi Energy
of an ideal Fermi Gas,'' \fIAppl. Phys. Lett.\fR, vol. 31, pp. 354-356, 1977.
.IP [13]
A. S. Grove, \fIPhysics and Technology of Semiconductor Devices\fR,
Wiley, 1967.
.IP [14]
S. M. Sze, \fIPhysics of Semiconductor Devices\fR, Wiley, 1981.
.IP [15]
E. Sangiorgi, C. S. Rafferty, M. R. Pinto and R. W. Dutton,
``Non-planar Schottky device analysis and applications,''
Proc. International Conf. on Simulation of Semiconductor
Devices and Processes, Swansea, U.K., July, 1984.
.IP [16]
E. Palm and F. Van De Wiele, ``Current lines and accurate contact
current evaluation in 2D numerical simulation of semiconductor
devices,'' \fIIEEE Transactions on Electron Devices\fR, October 1985.
