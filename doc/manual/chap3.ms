.EQ
gsize 10
delim $$
.EN
.so macros
.sp 5
.nr H1 3
.tl '' Chapter 3 ''
.sp 2
.tl ''General Discussion''
.sp 4
.NH 2
Numerical Considerations
.sp 1
.PP
In this section, some additional numerical considerations
will be discussed.  Refer to [1,2] for background material
and additional details.
.sp 1
.NH 3
Computational methods
.sp 1
.PP
PISCES-IIA served as both a general two-dimensional device
simulation code and as a vehicle to explore various alternative
computational methods [2].  PISCES-IIA had
four basic numerical solution techniques as described in [1]: 
the Gummel method, 
block-Newton method, knot-iterative method and full Newton
method.  From the experiences gathered since the initial
release of the program on
numerous computers and applications, the full Newton
and Gummel methods should now be used exclusively.
Hence, only these two methods have been included in PISCES-IIB.
Also, only the $( psi , n , p )$ variable set has been implemented
in PISCES-IIB.
.PP
As will be shown below, the Gummel and full-Newton methods have
fairly distinct regions of preference.
In general, the Gummel method is
preferred for zero/reverse bias and low-level injection
simulations.  The full Newton is preferable for high-level
injection as well as MOS saturation conditions (see [1]);
it is required for transient and ac small-signal analysis.
As an brief illustration, Figure 1a shows a cross-section of a submicron
bipolar transistor [3], suitable for comparing the computational methods
in PISCES-IIA.  Because of the aggressive scaling of lateral dimensions,
this device shows strong two-dimensional effects, particularly with
regard to collector resistance and current crowding.
Figure 1b shows a close-up of the intrinsic region of the device,
with a base-emitter bias of 0.9V.  Contours of hole current density 
are plotted at intervals of $2x10 sup 3 roman {Amps/cm} sup 2$.
The base current is dominated by injection through the corners of the
emitter, degrading the gain.  Such effects can be reduced by accurate
control of the emitter and extrinsic base profiles.
.PP
This bipolar transistor was simulated on a grid of approximately
1300 points, stepping the base-emitter voltage in 100mV increments
initially and 25mV increments later upon achieving very high forward bias.
Identical simulations were performed using Gummel's method, the block
method, the knot method and the full Newton (sparse direct LU) including
Newton-Richardson (see below).  Figures 2a and 2b illustrate 
the performance of each method.  In low-level injection, the block-Newton
method performs very well compared to the full Newton; however,
as is typically the case, Gummel's method is still better in this bias
range.  As the built-in voltage of the base-emitter junction is approached
($ approx $ 0.9V), Both the Gummel and block methods become rapidly more
expensive, and the block method eventually is a bit faster than Gummel.
In contrast, the full Newton is much more insensitive to bias condition.
Although slightly more expensive for low-level injection, the full
Newton definitely becomes the method of choice for biases above
$ approx $ 0.8V where it becomes 4-5 times cheaper than the
Gummel or block methods.  Finally, the knot-iterative method is generally
stable, but consistently more expensive than the direct method.  
However, it is still felt that this method may hold some promise for 
very large (3D?) problems with a code optimized for a 
vector/parallel machine.
.sp 1
.NH 3
Norms
.sp 1
.PP
Another important numerical consideration is the convergence criterion
used to define the end of the non-linear iteration procedure.
PISCES-II has a number of different convergence criteria,
as well as error measures for the non-linear iterations.
The default error measure is called the ``xnorm'' which measures
the size of the updates (absolute change in potential scaled by
kT/q and relative changes in carrier concentrations) on each iteration.
The default convergence criterion is that the xnorm for each variable
must be less than $10 sup -5$.  This requirement is VERY conservative,
and by no means should it be taken as an absolute!  Another warning
regarding this error norm: it is not always guaranteed to decrease
from iteration to iteration, although the system may be converging.
.PP
Possibly a more interesting norm (and one that is guaranteed to go down
from one iteration to the next) is the norm of the right-hand side of the 
non-linear Newton system (``rhsnorm'').
The rhsnorm is basically a measure of the maximum amount by which the
net flux or current is out of balance at any single grid point in 
the mesh.  In PISCES, the rhsnorm has been scaled so that it has the
same units (coul/$ mu $m or Amps/$ mu $m) as the terminal 
fluxes and currents.
It is particularly useful to examine the continuity rhsnorms for very low
current bias conditions, as they will give some information on
the resolution obtained; if the rhsnorms are on the same order
as any terminal currents of interest, those currents may not be reliable.
For such cases, it may be advantageous to force one or two more iterations
to see if the norm can be driven down further.  Although the rhsnorm
is not generally used as a strict criterion for convergence (the xnorm
tends to be more reliable in this regard), 
it is generally advisable to print it 
particularly if convergence problems are suspected.
Note that if both ``xnorm'' and ``rhsnorm'' are set on the METHOD card,
both will be printed, but the xnorm will be used exclusively to determine 
convergence.
.sp 1
.NH 3
Initial guesses
.sp 1
.PP
Another area of importance with respect to the convergence of the
non-linear iteration is the quality of the initial guess used to
start the procedure.  A good initial guess may cut down the number
of iterations by factors of two or more.
As described in [1], PISCES-II has basically
four different kinds of initial guesses\s-2\u\(dg\d\s+2 :
.FS
\(dg
Note that as described in [1], there
is one other type of initial guess that is generated after a re-grid
of a mesh upon which a solution had existed in order to estimate
values for newly created nodes.
.FE
`initial', `previous',
`project' and `local', any one of which can be selected as an option
on the SOLVE card.  \fIInitial\fR is used to start a new device; this 
guess is obtained by assuming charge neutrality throughout the
device.  Generally it is used to obtain a solution 
with zero applied bias.  \fIPrevious\fR takes the solution at the
previously solved bias point and adjusts the boundary conditions
(if necessary) only.  Every internal $psi , n , p$ is left as it
was for the previous point.  \fIProject\fR extrapolates internal
$psi , n , p$ from two previous bias points.  It therefore requires
two previous solutions, which additionally must have equivalent
bias steps taken on any electrodes that have had there voltages changed
(the bias step may be different from one solution to the next however).
Finally, the \fIlocal\fR guess uses the majority-carrier quasi-Fermi
potential (as determined by the applied biases), to set $ psi , n , p $.
.PP
Given that \fIinitial\fR will only be used for the first bias point,
there remains a choice of \fIprevious\fR, \fIproject\fR or 
\fIlocal\fR for any other bias points.  \fILocal\fR tends to work
best for strictly reverse-biased devices and is especially 
efficient when trying to jump by very large voltage increments.
\fIProject\fR is usually better than \fIprevious\fR where applicable
and therefore is the default in situations where either \fIproject\fR
or \fIprevious\fR can be used.
\fIPrevious\fR is the conservative choice.  It is required for
time-dependent simulation and current sources.
.PP
There are situations that arise where the user can force PISCES
to do better than it would have automatically.  A case in point
is a bipolar transistor where one is stepping $V sub BE$ with
a constant $V sub CE$.  As shown in Table I, \fIprevious\fR
outperforms \fIproject\fR in high-level injection.  The performance
of a current boundary condition on the base is also shown.
.sp 1
.KS
.ce
Table I - Newton loops for a bipolar transistor
.sp 1
.TS 
center allbox;
c c c c c c
n c c c c c.
$V sub BE$	$I sub B$	$I sub C$	\fIPrevious\fR	\fIProject\fR	Current
0.1v	$-3.11 roman{x} 10 sup -14$	$8.50 roman{x} 10 sup -14$	7	-	7
0.2v	$-2.58 roman{x} 10 sup -14$	$1.04 roman{x} 10 sup -13$	7	4	9
0.3v	$1.85 roman{x} 10 sup -14$	$3.36 roman{x} 10 sup -12$	7	4	12
0.4v	$6.71 roman{x} 10 sup -13$	$1.53 roman{x} 10 sup -10$	7	5	>20
0.5v	$2.11 roman{x} 10 sup -11$	$6.98 roman{x} 10 sup -9$	7	5	>20
0.6v	$8.96 roman{x} 10 sup -10$	$3.10 roman{x} 10 sup -7$	8	4	>20
0.7v	$3.67 roman{x} 10 sup -8$	$9.51 roman{x} 10 sup -6$	8	5	>20
0.75v	$2.09 roman{x} 10 sup -7$	$3.35 roman{x} 10 sup -5$	7	5	7
0.8v	$9.93 roman{x} 10 sup -7$	$8.18 roman{x} 10 sup -5$	7	6	8
0.825v	$2.10 roman{x} 10 sup -6$	$1.15 roman{x} 10 sup -4$	6	7	7
0.85v	$5.12 roman{x} 10 sup -6$	$1.60 roman{x} 10 sup -4$	8	9	8
0.875v	$1.34 roman{x} 10 sup -5$	$2.20 roman{x} 10 sup -4$	9	>20	9
0.9v	$2.88 roman{x} 10 sup -5$	$2.92 roman{x} 10 sup -4$	9	>20	9
.TE
.KE
.sp 1
The situation actually improves for each case 
as the device moves further into high-level injection; for 
instance, in going from $ V sub BE ~ = ~ 1.2V $ to
$V sub BE ~ = ~ 1.3V $, \fIprevious\fR only takes 6 Newton
loops while \fIproject\fR takes 10.
Remember that \fIproject\fR is the default automatically selected by
PISCES for each bias point after $V sub BE ~ = ~ 0.1V$.
Another common situation where a previous guess is better than project
is in stepping the drain of a MOSFET with gate held constant. For
that reason, it is often more economical to compute MOSFET characteristics
by stepping the gate with constant drain, using project.
.PP
Another alternative to improving initial guesses is of course
to simply take smaller bias steps.  Adjacent solutions then become
much more similar, and the number of Newton loops \fIper bias
point\fR is reduced when using either \fIproject\fR or \fIprevious\fR.  
However, since more bias points need to be solved
for, the total computation time may increase.
A procedure exists within PISCES to reduce the size of the
bias step taken if non-convergence is observed (see the
parameters ``trap'' and ``atrap'' on the METHOD card).
This feature has been found of particular value when finding
knees of I-V curves with voltage boundary conditions.
.sp 1
.NH 2
Increasing Bounds
.sp 1
.PP
As mentioned above, the Gummel and full Newton methods have been 
found preferable for nearly all bias conditions and are therefore
to be used exclusively in PISCES-IIB.  
The Gummel method is not very memory intensive; currently it 
can handle up to 3000 grid points.  The full Newton method
is VERY memory intensive.  PISCES-IIB as configured
prohibits the use of more than 1600 points with the full Newton
and 2 carriers (2000 points for the full Newton and one carrier).
These bounds are somewhat arbitrary.  They were chosen so that
the entire program would be about 7-MByte in order to fit
in non-virtual 8-Mbyte machines.
.PP
Given that most users do have access to machines/operating systems
with virtual memory, there has been a desire expressed to increase
the maximum number of grid points that can be treated.  In this
section, a few comments and guidelines will be presented that
may help in this regard.  One word of caution: applications
will tend to expand to fill the number of grid points allowed,
regardless of whether the points are necessary or not (a corollary
to one of Murphy's laws, so we are told).  Especially now that
options such as lumped elements are available, every possible
objection should be raised to the use of multi-thousand node
simulation runs if you share a computer resource amongst 
several PISCES users.  It may be advisable to actually
support different sized versions of the code to cut down on the
amount of memory requested at one time.
.PP
The majority of the memory required by PISCES is involved
with the sparse LU solver.  PISCES uses the sparse solver
VEGES [4] along with a minimum degree ordering from YSMP[5].
An explanation of the functions of the various arrays
involved in these codes is beyond the scope of this
report; see [4,5,6] for details.  However, it should be
possible to increase the bounds of these arrays without a detailed
knowledge of their purpose.
.PP
PISCES has a common block called `emaco' which contains most of
the large arrays used during execution.  This common block has a static
(permanent) part in the include file 'com.emaco'.  During
various phases of the program, other temporary storage is
added to the end of this static part by an include of another
common file following `com.emaco'. These additional files
usually have the letters `tmp' in their name; 
for instance, during a solution
using the full Newton method, the temporary storage is in
`com.soltmpn' while during a plot, `com.plttmp' is used.
.PP
The declared dimensions within PISCES are stored in a static
common block called `symco' in the include file com.symb.
For instance, `mp2fn' is the maximum number of points for 
Newton with 2 carriers, `mp1fn' is for Newton with 1 carrier and 
`mpgum' is for Gummel.  These variables and others (which are
generally self-explanatory) are set in the block data subprogram.
.PP
All arrays within PISCES that are dimensioned to 3000 are
dependent on the maximum number of points.  There are also
a number of arrays that have a dimen
sion of 6000, referring
to the maximum number of triangles, which is always
less than two times the number of grid points.  
There are two other similar dependencies which are not as obvious.
In `com.rgdtmp', the arrays dimensioned to 12000 refer to 
twice the maximum number of triangles, while in 'com.soltmpn',
the arrays dimensioned to 9800 refer to three times the maximum
number of points allowed with the full Newton and 2 carriers
(mp2fn which defaults to 1600).
.PP
The sparse matrix arrays are quite a bit harder to set dimensions for.  
In addition to numeric arrays for the 
matrix to be factored, A, and its upper and lower triangular
factorization, L and U, PISCES requires a set of pointer arrays
for each of these (IA, JA, IVA, IL, JL, IVL, IU, JU, IVU).
These pointer arrays are produced, along with the pivoting
order (IPC and IPRI), during the symbolic factorization
and are kept in `com.emaco'.
The numeric A, L and U are only used during
the solution phase of PISCES and are kept in temporary storage, 
as are all the various arrays associated with performing the symbolic
factorization.
.PP
The pointer arrays JA, JL, JU and IVA, IVL, IVU are the
relatively straight forward to dimension; 
they are set to the maximum number of equations to
be solved simultaneously which is max(3*mp2fn,2*mp1fn,mpgum)+1.
Therefore, JA, etc. are now set to 3*1600+1=4801.
.PP
The size of IA, IL, IU, A, L and U are somewhat harder to
determine.  In fact,
they are quite structure dependent.  The best advice for setting
these quantities is to run a set
of examples with varying number of points using the `print'
option on the SYMBOLIC card which will report on the actual
size of each array.  Table II shows the result of such an experiment
for a square device (rectangular grid). 
Note that NP is the number of points and NEQ is the number of
PDEs solved simultaneously; i.e., NEQ=1 refers to 0 carrier
or Gummel method simulations and NEQ=2 or 3 refers
to 1 or 2 carrier Newton simulations.
Similarly, Table III shows the results of an identical 
experiment using a ``thin''
device, i.e. one that is rectangular but only 5 nodes high.
.KF
.sp 1
.ce
Table II - Array sizes for square device (n-by-n)
.sp 1
.TS 
center allbox;
c c c c c c c c
n n n n n n n n.
NP=$n sup 2$	NEQ	IA	IL	IU	A	L	U
=
25	1	91	63	57	105	78	78
100	1	381	389	394	460	558	558
400	1	1561	1989	2087	1920	3392	3392
900	1	3541	5024	5362	4380	9712	9712
1600	1	6321	10006	9878	7840	19766	19766

25	2	261	191	165	420	334	334
100	2	1121	1214	1256	1840	2325	2325
400	2	4641	5955	6283	7680	13965	13965
900	2	10561	15760	15646	17520	39685	39685
1600	2	18881	30097	28873	31360	80797	80797

25	3	391	328	265	945	769	769
100	3	1691	2002	1978	4140	5305	5305
400	3	6961	9748	9847	17280	31720	31720
900	3	15841	26506	24448	39420	89965	89965
1600	3	28321	51223	45232	70560	183046	183046
.TE
.sp 1
.KE
.KF
.sp 1
.ce
Table III - Array sizes for thin device (n-by-5)
.sp 1
.TS 
center allbox;
c c c c c c c c
n n n n n n n n.
NP=$5n$	NEQ	IA	IL	IU	A	L	U
=
25	1	91	63	57	105	78	78
100	1	391	320	282	450	412	412
400	1	1591	1325	1152	1830	1732	1732
900	1	3591	3000	2602	4130	3932	3932
1600	1	6391	5345	4632	7350	7012	7012

25	2	261	191	165	420	334	334
100	2	1161	971	871	1800	1745	1745
400	2	4761	4155	3631	7320	7325	7325
900	2	10761	9451	8231	16520	16625	16625
1600	2	19161	16875	14671	29400	29645	29645

25	3	391	328	265	945	769	769
100	3	1741	1660	1387	4050	4000	4000
400	3	7141	7147	5797	16470	16780	16780
900	3	16141	16300	13147	37170	38080	38080
1600	3	28741	29107	23437	66150	67900	67900
.TE
.sp 1
.KE
There are several interesting features to note in Tables II and III.
First, there is about a factor of 2 difference in storage for A
in going from 1 to 2 equations (0 carriers to 1 carrier, full Newton)
and a factor of 3 difference in going from 1 to 3 equations (0 carriers
to 2 carriers, full Newton).
The size of A seems to be proportional to the number of equations 
being solved for simultaneously.  
Second, in Table II the sizes
of L and U, which are identical, seem to be proportional to the
square of the number of equations; hence increases of 4 and 9 
times are observed for 1 and 2 carrier full Newton
simulations respectively over 0 carrier simulations.  
Third, note that because of fill-in and the vector pointer scheme
used in the sparse solver, the sizes of L and U become
much larger than their associated pointers IL and IU.  This
size differential becomes even more significant if data types
are considered since L and U must be real*8, while IL and IU
can be integer*2.
Finally, the distinct difference between the thin and square devices
is noted in the sizes of L and U.  Although the A dependence is
roughly identical, the sizes of L and U are much smaller for the
thin device; in fact, the sizes of L and U are almost equal to A.
This result is not surprising due to the extremely low amount of
fill-in that can be achieved for such structures using a minimum
degree pivot ordering.  The unfortunate consequence of such a result
is that different device geometries will have different memory
requirements.  Such considerations must be addressed in choosing
array bounds.  Note further that pure triangular grids (see chapter 4),
as opposed to the rectangular grids above, will also yield slightly
different bounds for a given number of nodes.
.PP
There are a number of other (temporary) arrays during symbolic
that are located in com.symtmp1, com.symtmp3 and com.symtmp4.
The actual sizes of most of these are also printed out using the
``print'' option, although they were not shown above.  Additionally,
there may be some arrays with mysterious bounds in some common blocks.
Some of the more obvious bounds are : 
the maximum number of regions (8), the maximum
number of electrodes (10), the maximum plot buffer length (1000),
and the maximum number of electrode nodes (300).
Any other dimensions probably need never be changed, and hopefully, 
if any are ever exceeded, PISCES should respond 
gracefully with a suitable diagnostic message.
.PP
One final comment should be made on the sparse matrix package.
The LU factorization (subroutine VMNPD) is the most costly (in
cpu) section of the program.  As such, it might be worth
trying to optimize if one has access to a vector machine.
However, this optimization process is far from being trivial.
In order to gain substantial benefit, one must really understand
the sparse matrix pointer structure and operations.  
Refer to [7] for a possible improvement and a discussion of
some of the issues involved.
.sp 1
.NH 2
Model Implementation
.sp 1
.PP
Another area where some user customization has been desired is
in the implementation of new physical models.  Hence, this
section includes a brief discussion of the general issues
and approach to such an endeavor.  
.PP
Referring to Figure 3, we write the box discretization [8] for a node 
in the triangular grid:
.EQ I (3.1)
sum from t left ( - epsilon {h sub {3} } over {d sub {3} } ( psi sub {2} -
psi sub {1} ) - epsilon {h sub {2} } over {d sub {2} } ( psi sub {3} -
psi sub {1} ) - rho sub 1 A sub {1} right ) 
~ = ~ sum from t F sub { psi sub 1 } (t) ~ = ~ 0
.EN
.EQ I (3.2)
sum from t left ( J sub {n sub {12}} h sub 3 + J sub {n sub {13}} h sub 2
- q U sub 1 A sub 1 right )
~ = ~ sum from t F sub { n sub 1 } (t) ~ = ~ 0
.EN
.EQ I (3.3)
sum from t left ( J sub {p sub {12}} h sub 3 + J sub {p sub {13}} h sub 2
+ q U sub 1 A sub 1 right )
~ = ~ sum from t F sub { p sub 1 } (t) ~ = ~ 0
.EN
The sum in (3.1)-(3.3) is over all the triangles that the node labeled
`1' is a part of.
The indices `1',`2' and `3' in (3.1)-(3.3) should more properly have a 
superscript $t$, with `$1 sup t$' always being the node of interest
and `$2 sup t$' and `$3 sup t$' the nodes adjacent to `$1 sup t$' 
for each triangle $t$ (see figure 3b).
$ J sub {n,p sub 12} $ and $ J sub {n,p sub 13} $ above refer
to the current densities between nodes 1 and 2 and between nodes
1 and 3 of a triangle $t$ and are computed using the Scharfetter-Gummel
discretization [9]:
.EQ I (3.4)
J sub {n sub 12} ~ = ~ { q mu sub {n sub 12} } over { d sub 3 } left [
( n sub 2 - n sub 1 ) bold {B} left ( psi sub 2 - psi sub 1 right ) -
n sub 1 ( psi sub 2 - psi sub 1 ) right ]
~ = ~ { mu sub { n sub 12 } } G sub { n sub 12 }
.EN
.EQ I (3.5)
J sub {p sub 12} ~ = ~ { q mu sub {p sub 12} } over { d sub 3 } left [
( p sub 1 - p sub 2 ) bold {B} left ( psi sub 2 - psi sub 1 right ) -
p sub 2 ( psi sub 2 - psi sub 1 ) right ]
~ = ~ { mu sub { p sub 12 } } G sub { p sub 12 }
.EN
where $ bold {B} $ is the Bernoulli function.
.PP
The equation assembly procedure within PISCES is performed on a triangle
by triangle basis.  That is, a submatrix and right-hand side
are set up for each triangle,
containing the equations relating the nodes of that single triangle.
Afterward, this submatrix and right-hand side are
added to the global system.  The submatrix (estifm) has the 
following form:
.KS
.EQ I (3.6)
roman { estifm } ~ = ~ left [ matrix {
ccol {
     { partial F sub 1 } over { partial x sub 1 } above
     { partial F sub 2 } over { partial x sub 1 } above
     { partial F sub 3 } over { partial x sub 1 } }
ccol {
     { partial F sub 1 } over { partial x sub 2 } above
     { partial F sub 2 } over { partial x sub 2 } above
     { partial F sub 3 } over { partial x sub 2 } }
ccol {
     { partial F sub 1 } over { partial x sub 3 } above
     { partial F sub 2 } over { partial x sub 3 } above
     { partial F sub 3 } over { partial x sub 3 } } } right ]
.EN
.KE
where, for the full Newton and 2 carriers,
.KS
.EQ I (3.7)
{ partial F sub i } over { partial x sub j } ~ = ~ left [ matrix {
ccol {
     { partial F sub { psi sub i } } over { partial psi sub j } above
     { partial F sub { n   sub i } } over { partial psi sub j } above
     { partial F sub { p   sub i } } over { partial psi sub j } }
ccol {
     { partial F sub { psi sub i } } over { partial n   sub j } above
     { partial F sub { n   sub i } } over { partial n   sub j } above
     { partial F sub { p   sub i } } over { partial n   sub j } }
ccol {
     { partial F sub { psi sub i } } over { partial p   sub j } above
     { partial F sub { n   sub i } } over { partial p   sub j } above
     { partial F sub { p   sub i } } over { partial p   sub j } } }
right ] ~ ~ ~ i ~ = ~ 1,2,3 ~ ~ , ~ ~ j ~ = ~ 1,2,3
.EN
.KE
The corresponding right-hand side for the triangle, wrhs, is
.EQ I (3.8)
roman {wrhs} ~ = ~  - left [ pile {
{ F sub 1 } above { F sub 2 } above { F sub 3 } } right ]
.EN
where, again for the full Newton method and two carriers,
.EQ I (3.9)
F sub i ~ = ~ left [ pile {
{F sub {psi sub i} } above {F sub {n sub i} } above {F sub {p sub i} } }
right ] ~ ~ ~ , ~ ~ ~ i ~ = ~ 1,2,3
.EN
Therefore, estifm is a 9-by-9 matrix and wrhs has a length of 9
for the full Newton method and two carriers.
Note that for the full Newton and only one carrier, 
$partial F sub i / partial x sub j $ would only be 2-by-2 and
$ F sub i $ would be of length 2, so estifm would be 6-by-6 and
wrhs would be of length 6.  For 0 carriers and Gummel, estifm is
a simple 3-by-3 matrix, and wrhs is a vector of length 3.  
.PP
In order to assemble the equations for a single
triangle $t$, all the elements of estifm and wrhs must be
computed.  By using (3.8) and (3.9) along with (3.1)-(3.5),
wrhs can be constructed directly.  However, estifm requires
careful calculation of derivatives.  Failure to include
proper derivative terms, either by neglect or error, may
lead to slow convergence (or divergence).
Regardless of the exactness of estifm, if the
iterations do converge, the solution will be correct if
wrhs is correct.
.PP
As an example, $ partial F sub { n sub 1 } / partial n sub 2 $
will be formulated.  This entry corresponds to estifm(2,5) for the
full Newton with two carriers, estifm(2,4) for the full Newton with
one carrier and estifm(1,2) for the Gummel method.  
The corresponding right-hand side entry 
(wrhs(2) for Newton with one or two carriers and wrhs(1) for Gummel) is
.EQ I (3.10)
- F sub { n sub 1 } ~ = ~
- ( h sub 3 J sub { n sub 12 } ~ + ~ h sub 2 J sub { n sub 13 } ~ + ~ 
q A sub 1 U sub 1 )
.EN
Therefore,
.EQ I (3.11)
{ partial F sub { n sub 1 } } over { partial n sub 2 } ~ = ~
h sub 3 { partial J sub { n sub 12 } } over { partial n sub 2 } ~ + ~
h sub 2 { partial J sub { n sub 13 } } over { partial n sub 2 } ~ + ~
q A sub 1 { partial U sub 1 } over { partial n sub 2 }
.EN
Substituting in (3.4),(3.5) and 
noting that $ partial J sub { n sub 13 } / partial n sub 2 ~ = ~ 0 $,
.EQ I (3.12)
{ partial F sub { n sub 1 } } over { partial n sub 2 } ~ = ~
h sub 3 left [
   mu sub { n sub 12 } { partial G sub { n sub 12 } } 
   over { partial n sub 2 } ~ + ~
   G sub { n sub 12 } { partial mu sub { n sub 12 } } 
   over { partial n sub 2 } right ]
~ + ~ q A sub 1 { partial U sub 1 } over { partial n sub 2 }
.EN
.PP
There are three derivatives in (3.12) to compute.  The first,
$ partial G sub { n sub 12 } / partial n sub 2 $ can be computed
directly from (3.4):
.EQ I (3.13)
{ partial G sub { n sub 12 } } over { partial n sub 2 } ~ = ~
q over d sub 3 bold { B } left ( psi sub 2 - psi sub 1 right )
.EN
Recombination (SRH or Auger) is generally calculated at a node directly
from the carrier concentrations at that node, as is the space
charge, $rho sub 1$ in (3.1).  In this case, 
$U sub 1 ~ = ~ f ( n sub 1 , p sub 1 ) $, therefore
.EQ I (3.14)
{ partial U sub 1 } over { partial n sub 2 } ~ = ~ 0
.EN
Note however that if impact-ionization, generally is written as a 
function of the hole and electron currents $J sub {n sub 12}$ and
$J sub { p sub 12 } $ [10], was to be added, the generation term produced
would have a dependence on $ n sub 2 $ from the diffusion
component of $ J sub {n sub 12} $.
.PP
Finally in (3.12), $ partial mu sub { n sub 12 } / partial n sub 2 $
must be evaluated.  Within PISCES, mobility presently can have
several dependencies (see chapter 2 of [1]).  
Some dependencies may be static; for instance,
mobility may be a function of impurity concentration which does
not change with the solution.  Additionally, mobility may have
a dependence of electric field and hence potential, introducing a 
non-zero $ partial mu sub { n sub 12 } / partial psi sub 1 $ and
$ partial mu sub { n sub 12 } / partial psi sub 2 $.
Suppose it was desirable to include some carrier-carrier
scattering mechanism within PISCES (there currently is no
such model within PISCES-IIB).  Just for illustration,
assume the mobility should then be written as
.EQ I (3.15)
mu sub {n sub 12} ~ = ~ 
mu sub {n sub 12} sup 0 
f ( n sub 1 , n sub 2 , p sub 1 , p sub 2 ) 
~ = ~ mu sub { n sub 12 } sup 0
{ left [ 1 + C ( n sub 1 + n sub 2 + p sub 1 + p sub 2 ) right ] } 
sup {-1/2}
.EN
where $ mu sub { n sub 12 } sup 0 $ may include both static and 
field dependencies and $C$ is a constant.  Then,
.EQ I (3.16)
{ partial mu sub { n sub 12 } } / { n sub 2 } ~ = ~ 
- C over 2 mu sub { n sub 12 } sup 0 
{ left [ 1 + C ( n sub 1 + n sub 2 + p sub 1 + p sub 2 ) right ] } 
sup {-3/2}
.EN
Therefore, for this mobility model, we obtain
.EQ I (3.17)
{ partial F sub { n sub 1 } } over { partial n sub 2 } ~ = ~
h sub 3 left [
   { q mu sub { n sub 12 } } over d sub 3
   bold { B } ( psi sub 2 - psi sub 1 ) ~ - ~
   { C G sub { n sub 12 } } over 2
   mu sub { n sub 12 } sup 0 
   { left [ 1 + C ( n sub 1 + n sub 2 + p sub 1 + p sub 2 ) right ] } 
   sup {-3/2}
right ]
.EN
where $ G sub { n sub 12 } $ is computed from (3.4) and 
$ mu sub { n sub 12 } $ is computed from (3.15).
Any other of the elements of estifm (and wrhs) are assembled in
a similar manner.
.PP
One final point to consider is that throughout the PISCES code,
variables and equations are scaled.  This scaling takes place
in a subroutine called MATER.  Basically, any potentials are
scaled by $kT/q$ while concentrations (including doping) are
scaled by $ n sub i $. Poisson's equation, (3.1),
is scaled by a variable called ``decoef'', while the
continuity equations, (3.2) and (3.3) are scaled by a variable
called ``djcoef''.  See MATER for the precise values for
these coefficients and any other scaling information.
.sp 1
.NH 2
Vector Calculations
.sp 1
.PP
It is sometimes useful to examine internal fields or currents
from a PISCES solution.  Unfortunately these vector
quantities, unlike the real solution $( psi , n , p)$, are not
accurately defined at grid points but instead are computed
between nodes along the edges of triangles.
(see (3.4) and (3.5)).
This presents a problem whenever internal currents or fields are to be
printed or plotted, in that there is no obvious way to assign
a nodal value of current or electric field.  Further, in order
to interpolate contours, it is required to obtain nodal values for 
currents and fields.
.PP
However, with some mathematical manipulation, reasonable nodal 
averages can be obtained for these vector quantities.  In the 
following, the electric field will be used as an example, but
any of the various current components (or total current) can
be treated in a similar fashion.
.PP
Consider the expression
.EQ I (3.18)
del cdot ( lambda E vec ) ~ = ~ lambda del cdot E vec ~ + ~ 
E vec cdot del lambda
.EN
where $ E vec ~ = ~ - del psi $ is the electric field and $ lambda $ is
some scalar quantity.  Noting that from Poisson's equation (2.1)
.EQ I (3.19)
del cdot E vec ~ = ~ rho over epsilon
.EN
(3.18) can be simplified to 
.EQ I (3.20)
del cdot ( lambda E vec ) ~ = ~ lambda rho over epsilon ~ + ~
E sub x { partial lambda } over { partial x } ~ + ~
E sub y { partial lambda } over { partial y } 
.EN
where $ E sub x$ and $E sub y$ are the x and y components of the
electric field.
Now if one arbitrarily chooses $lambda ~ = ~ x$ and re-arranges (3.20),
then
.EQ I (3.21)
E sub x ~ = ~ del cdot ( x E vec ) ~ - ~ x rho over epsilon
.EN
Using Gauss's law just as with the Poisson
equation itself [8], (3.21) can be used to obtain a reasonable 
estimate of $E sub x$ at each node.  Similarly, with
$lambda ~ = ~ y$, $E sub y$ and hence $E vec$ can be computed.
Note that identical arguments hold true for currents as well, with
the added benefit that when calculating total node current, 
$del cdot J vec sub TOTAL ~ = ~ 0$ so that the first term on the
right-hand side of (3.18) is zero.
.PP
It must be stressed that the final results are still just estimates;
if (3.21) is fully integrated and expanded, one finds that the
result is a weighted average of x and y components by triangle area.  
For more exact applications, a self-consistent electric field value 
is available for each triangle (constant over the triangle)
computed from the three potentials at each triangle 
vertex.  This is in fact how barrier-lowering computations are 
performed - see section 4.3 of chapter 2.
.sp 1
.NH 2
Errata
.sp 1
.PP
Since this report is to be a supplement and not a replacement
for our earlier document, the errors found to date
in [1] are listed below:
.sp 1
.IP \(bu 4
The ``ascii'' parameter on the PLOT.1D card is a logical
flag and not a character string; i.e., if an output file
is requested using the ``outfile=<ch string>'' parameter,
it is binary by default and ascii only if the ``ascii=<logical>'
is set to true.
.IP \(bu 4
The example in OPTIONS card section is incorrect.  It should
read:
.in +10
OPTIONS   TEK4107  X.S=6 ...
.in -10
not
.in +10
OPTIONS   DEV=TEK4107  X.S=6 ...
.in -10
.IP \(bu 4
An example in EXTRACT card section is incorrect.  It should
read:
.in +10
EXTRACT   METAL.CH CONT=1 ...
.in -10
not
.in +10
EXTRACT   ME.CHARGE CONT=1 ...
.in -10
.IP \(bu 4
No fixed charge density was supplied for the QF card example.
.IP \(bu 4
The defaults for ``y.top'' and ``y.bot'' are listed incorrectly
in the section on the DOPING card for y-direction profiles.
The correct defaults are ``SP'' for the ``y.bot''
parameter and ``y.bot'' for the ``y.top'' parameter.
.IP \(bu 4
The ``diff'' option on the LOAD card produces the difference
between the solution in file 1 and the solution in file 2, i.e.
(solution 1) - (solution 2).  The example regarding the ``diff''
option should therefore have read ``OUTD=SOL1-2'' not ``OUTD=SOL2-1''.
.IP \(bu 4
Equation (2.36) is incorrect (the exponentiation operator
was left out of the denominator).  It should read
.EQ I
v sub {sat} (T) ~ = ~ ^ { 2.4 times 10 sup 7 } over 
{ 1 ~ + ~ 0.8 exp left ( T over 600 right ) }
.EN
.sp 1
.PP
The following features were undocumented:
.sp 1
.IP \(bu 4
When using the Newton-Richardson option for the full Newton
method (``autonr'' on the METHOD card), PISCES will put an
asterisk (``*'') in front of any Newton iteration
where the Jacobian was re-factored.
.IP \(bu 4
The analytic concentration-dependent, low-field mobility
model for Si (including temperature effects - see section 3
of chapter 2) was not included in the documentation of the
MODELS card in [1].  It is included in this report.
.IP \(bu 4
The ``print'' option for printing model coefficients, flags
and physical constants was not included in the documentation of the
MODELS card in [1].  It is included in this report.
.IP \(bu 4
No mention was made of the ``tt'' files (``tt'' is an abbreviation
for triangle-tree) created during each regrid.  These files contain 
a history of the procedure by which an existing mesh was created.
Retaining the mesh history helps prevent the occurence of unduly
large or small angles during the regrid procedure.  It is possible,
but inadvisable to regrid a mesh without a history file.  A history
file is generated for every file that is a regrid of some other
mesh; hence, an initial mesh will not have a ``tt'' file.
.IP \(bu 4
When printing information on grid points (the ``points'' option
on the PRINT card), PISCES sometimes prints some confusing numbers
under the electrode column.  If the number outputted is positive,
it corresponds to the electrode number to which that node is a part.
If a negative number is printed, the absolute value of this number
(if $<=$ the total number of electrodes, nelect) corresponds to the 
electrode to which this node is connected via semiconductor of
equivalent doping type.  If a negative number is printed whose
absolute value is greater than nelect, then this node is connected
via equivalent doping to more than one electrode.  The absolute
value then corresponds to a code in base nelect+1 to indicate
each contact association.  This association of internal nodes to contacts
is necessary for setting quasi-fermi levels properly for 0 and 1
carrier solutions.  Additionally, if the number in the electrode column
is zero, then this node is part of a doping region which is not
contacted by an electrode (or is an insulator node).
.IP \(bu 4
Device-independent plot files can be saved instead of writing to
the user's screen by specifying a binary ``outfile'' on the PLOT.2D or
PLOT.1D cards.  These files are useful for off-line or hardcopy plots.
Defined here is the file format for programmers who need to write
off-line plot drivers.
.sp 1
The plot files contain a sequence of fixed-length binary records,
each containing two real numbers (the x,y coordinates) and an
integer (the action code).  The actions are:
.sp 1
.KS
.TS
center;
c l.
Code	Action to be performed
=
-i	Change linetype to i (0<i<8) (x,y are ignored)
0	Initialize the plotting system (x,y ignored)
1	Clear screen / new page (x,y ignored)
2	Draw line to x,y
3	Move to x,y
13	Offset subsequent positions by x,y
15	Rotate subsequent positions around x,y
17	Rotate subsequent positions by x (y ignored)
19	Post buffers (x,y ignored)
20	Obsolete - ignore
40,41	Switch to/from graphics mode (terminals only)
99	Finish plotting
.TE
.KE
.sp 1
Most of the actions are graphics primitives.  The exceptions are
the translation and rotation actions which affect the interpretation
of all subsequent x,y coordinates.  These transformation actions are
intended to be cumulative in effect.  Note that a rotation is
defined by two adjacent calls, one to define the location, the
second to define the angle.
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
E. Crabb\*'e, Stanford University, private communication.
.IP [4]
D. A. Calahan and P. G. Buning, ``Vectorized General Sparsity
Algorithms with Backing Store,'' SEL Report 96, Systems
Engineering Laboratory, University of Michigan, Ann Arbor,
MI, 1977.
.IP [5]
S. C. Eisenstat, M. C. Gursky, M. H. Scultz, A. H. Sherman,
Dept. Computer Science, Yale University, Technical Report
112, 1977.
.IP [6]
A. George and J. W. H. Liu, \fIComputer Solutions of Large
Sparse Positive Definite Systems\fR, Prentice-Hall, 1981.
.IP [7]
R. Bank, W. M. Coughran, W. Fichtner, E. H. Grosse, D. J. Rose
and R. K. Smith, ``Transient simulation of silicon devices and
circuits,''
\fIIEEE Transactions on Electron Devices\fR, October, 1985.
.IP [8]
C. H. Price, ``Two-dimensional Numerical Simulation of Semiconductor
Devices,'' Ph.D. dissertation, Stanford University, May, 1982.
.IP [9]
D. Scharfetter and H. K. Gummel, ``Large-signal analysis of a 
silicon Read diode oscillator,'' \fIIEEE Transactions on Electron
Device\fR, vol. ED-16, pp. 64-77, 1969.
.IP [10]
C. R. Crowell and S. M. Sze, ``Temperature dependence of avalanche
multiplication in semiconductors,'' \fIApplied Physics Letters\fR,
vol. 9, no. 6, pp. 242-244, 15 September, 1966.
