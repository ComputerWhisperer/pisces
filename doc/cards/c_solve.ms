.EQ
delim @@
.EN
.bC SOLVE
.NH  0
Syntax
.sp 2
.R
.in +4
SOlve <estimate> <dc bias> <transient> <ac> <files>
.in -4
.sp
.NH 
Description
.IP
The SOLVE card instructs \*(PI to perform
a solution for one or more specified bias points.  
.NH
Parameters
.sp 2
.RS
.IP "<estimate>"
.sp 1
.in +4
.TS
l l l.
INitial	\\=	<logical>
PREvious	\\=	<logical>
PROject	\\=	<logical>
LOcal	\\=	<logical>
.TE
.in -4
The above parameters are used to specify how the initial guess for
the solution is to be obtained.  The first bias point for a given
structure must have the INITIAL parameter specified.  From then
\*(PI will either use the previous solution (PREVIOUS), or if
there are two previous solutions present and equivalent bias steps are
taken on any electrodes that are changed (see chapter 3 in this
report and the example
below), an extrapolation (PROJECT) from the preceding two 
solutions will be used to get an improved initial guess.  After
the initial bias point, \*(PI will automatically use extrapolation
wherever possible if no estimate parameter is supplied. A different
type of previous guess is available by using LOCAL values of
the quasi-fermi levels.
.sp 2
.KS
.IP "<bias conditions>"
.sp
.in +4
.TS
l l l l.
V1	\\=	<real>	
I1	\\=	<real>	
V2	\\=	<real>	
I2	\\=	<real>	
	.		
	.		
	.		
V9	\\=	<real>	
I9	\\=	<real>	
V0	\\=	<real>	
I0	\\=	<real>	
VStep	\\=	<real>	(default is 0.0)
IStep	\\=	<real>	(default is 0.0)
NSteps	\\=	<integer>	(default is 0)
Electrode	\\=	<integer>	
N.bias	\\=	<real>	
P.bias	\\=	<real>	
.TE
.in -4
The parameters V1, V2,...,V9, V0 represent the bias voltages 
(for non-current boundaries only) and
I1, I2,...,I9, I0 represent the terminal currents in units
of Amps/@mu@m (for current boundary contacts only - see CONTACT card)
applied at contacts 1, 2,...,9, 0.
The defaults for these parameters are normally the potentials 
(currents) from the previous bias point.
However, if the INITIAL flags is specified on the solve card, then all
contact potentials that are not specified on the curent contact card
will be reset to 0.0v. This reset holds even if a contact potential has
been set on a previous solve card.
VSTEP (ISTEP) is a voltage (current) increment to be added to 
one or more 
electrodes, as specified by the integer assigned to ELECTRODE.
If more than one electrode is to be stepped, ELECTRODE should then
be an n-digit integer, where each of the n-digits is a separate
electrode number (and if there are 10 electrodes, don't put
electrode 0 first in the sequence!).  NSTEPS is the number of
bias increments (steps) to be taken; i.e., if VSTEP (ISTEP) is specified,
the specified electrode is incremented NSTEPS times.
Finally, N.BIAS and P.BIAS specify fixed quasi-Fermi potentials
for carriers (electrons and holes, respectively) that are not
being solved for.  If N.BIAS or P.BIAS are not specified, then
\*(PI either will choose local quasi-Fermi potentials based on bias and
doping (see chapter 2 of [1]) or if ``fix.qf'' is set on the
METHOD card, will set the quasi-Fermi levels where applicable
to values which produce the least amount of free carriers
(maximum bias for electrons and minimum bias for holes).
.sp 2
.KE
.KS
.IP "<transient>"
.in +4
.TS
l l l l.
Dt or TSTEp	\\=	<real>	(default is 0)
TSTOp or TFinal	\\=	<real>	
Ramptime	\\=	<real>	(default is 0)
ENdramp	\\=	<real>	(default is 0)
.TE
.in -4
DT (or TSTEP) is the time-step to be taken.  For automatic
time-step runs (see the METHOD card), DT (TSTEP) is used to
select the first time step only.  TSTOP (or TFINAL) specifies
the end of the time interval to be simulated so that
if the simulation begins at @ t ~ = ~ t sub 0 @, it will end
at @ t  ~ = ~ @TSTOP (TFINAL).
Alternatively, NSTEPS can be used to signal the end of 
the run; i.e., the final time would be 
@t ~ = ~ t sub 0 + roman {NSTEPS} * roman {TSTEP} @.
RAMPTIME and ENDRAMP apply any bias changes as linear
ramps.  RAMPTIME specifies a ramp interval in seconds; i.e.,
the ramp will begin at @ t ~ = ~ t sub 0 @ and end at 
@ t ~ = ~ t sub 0 + roman { RAMPTIME } @.  Alternatively, ENDRAMP
specifies the exact end of the ramp in running time; i.e. the
ramp will start at @ t ~ = ~ t sub 0 @ and end at 
@ t ~ = ~ roman {ENDRAMP} @.
.sp 2
.KE
.KS
.IP "<ac>"
.in +4
.TS
l l l l.
AC.analysis	\\=	<logical>	(default is false)
FRequency	\\=	<real>	
FStep	\\=	<real>	(default is 0)
MUlt.freq	\\=	<logical>	(default is false)
NFsteps	\\=	<integer>	(default is 0)
VSs	\\=	<real>	(default is 0.1*kT/q)
TErminal	\\=	<integer>	(default is all)
S.omega	\\=	<real>	(default is 1.0)
MAx.inner	\\=	<integer>	(default is 25)
TOlerance	\\=	<real>	(default is @ 1 x 10 sup -5 @)
.TE
.in -4
AC.ANALYSIS is a flag to indicate that AC sinusoidal
small-signal analysis (see chapter 2 of this report)
be performed after the DC condition is solved for.  
Note that the full Newton method (2 carriers)
must be used for this analysis.  
FREQUENCY is the frequency (in Hz) at which this analysis is
to be performed.  The analysis can be repeated at a number of
different frequencies (without resolving the DC condition) by
selecting an FSTEP.  FSTEP is a frequency increment which is
added to the previous frequency by default, or it may
be multiplied by setting MULT.FREQ.  The number of increments
is given by NFSTEPS.
VSS is the magnitude of the applied small-signal bias 
(@ V tilde sub i @ in equation (2.19)).  TERMINAL is
the contact to which this AC bias will be applied.
More than one contact number may be specified (via concatenation),
but each will be solved separately.  Each contact that is specified
yields a column of the admittance matrix as defined by
equation (2.17).  The analysis makes use SOR to solve a
linear system.  S.OMEGA is the relaxation parameter for SOR
(it is NOT a frequency), MAX.INNER is the maximum number of
SOR iterations and TOLERANCE is the SOR convergence criterion.
Specification of the ``g.debug'' flag on the OPTIONS
card will provide some detailed information on the AC solution
procedure.
.sp 2
.KE
.KS
.IP "<files> (optional)"
.in +4
.TS
l l l l.
Outfile	\\=	<filename>	
Currents	\\=	<logical>	(default is false)
AScii	\\=	<logical>	(default is false)
.TE
.in -4
The OUTFILE parameter specifies the name of the binary output file
for the solution of this bias point.  The file names may contain up
to 20 characters.  If an electrode is stepped so that more
than one solution is generated by this card, the last non-blank 
character of the supplied file name
will have its ascii code incremented by one for each bias point in 
succession, resulting in a unique file per bias point.
If CURRENTS is specified, the electron, hole, and displacement
currents, and the electric field, will be computed and stored with
the solution.  IF ASCII is specified, OUTFILE will be ascii as
opposed to binary.
.sp 2
.KE
.RE
.KS
.NH
Examples
.IP
.sp 2
The following performs an initial bias point, saving the
solution to the data file OUT0:
.sp
.in +4
.ss 24
SOLVE  INIT OUTF=OUT0
.in -4
.sp 2
In the next example, bias stepping is illustrated.  The two solve
cards produce the following bias conditions:
.in +4
.TS
c c c c
n n n n.
Bias point #	V1	V2	V3
1	0.0	0.5	-0.5
2	1.0	0.5	0.0
3	2.0	0.5	0.0
4	3.0	0.5	0.0
5	4.0	0.5	0.0
6	5.0	0.5	0.0
.TE
.in -4
The solutions for these bias points will be saved
to the files OUT1, OUTA, OUTB, OUTC, OUTD and OUTE.  
Note that the initial guess for the first bias point is obtained
directly from the preceding solution because the PREVIOUS option 
was specified.  The initial guesses for bias points 2 and 3
will also be obtained as if PREVIOUS had been specified since
two electrodes (numbers 1 and 3) had their biases changed on
bias point 2.  However, for bias points 4, 5 and 6, \*(PI will
use a projection to obtain an initial guess since starting with
bias point 4, both of its preceding solutions (bias points 2 and
3) only had the same electrode bias (number 1) altered.
.sp
.in +4
.ss 24
.nf
SOLVE  PREV V1=0 V2=.5 V3=-.5 OUTF=OUT1
.br
.ss 24
.nf
SOLVE  V1=1 V2=.5 V3=0 VSTEP=1 NSTEPS=4 
+      ELECT=1 OUTF=OUTA
.in -4
.KE
.KS
.sp 2
Here is a case where two electrodes are stepped (2 and 3).  The bias
points solved for will be (0,0,1), (0,.5,1.5), (0,1,2) and (0,2,3).  
\*(PI will use the PROJECT option to predict an initial guess for 
the third and fourth bias points since the bias voltages on both 
electrodes 2 and 3 have been altered by the same amount between each
point.
.sp
.in +4
.ss 24
.nf
SOLVE  	V1=0 V2=0 V3=1 VSTEP=.5 NSTEPS=2 
+      	ELECT=23
SOLVE   V2=2 V3=3
.in -4
.KE
.KS 
.sp 2
If no new voltages are specified and a VSTEP is included,
the first bias point solved for is the preceding one incremented
appropriately by VSTEP.  This is illustrated by repeating the 
above example as a three card sequence:
.sp
.in +4
.ss 24
.nf
SOLVE  V1=0 V2=0 V3=1
SOLVE  VSTEP=.5 NSTEPS=2 ELECT=23
SOLVE  VSTEP=1  NSTEPS=1 ELECT=23
.in -4
.KE
.KS
.sp 2
The following sequence is an example of a time-dependent solution.
The METHOD card specifies the second-order discretization and
automatic time-step selection option, along with Newton-Richardson.
The first SOLVE card then computes the solution for a device 
with 1 volt on V1 and 0 on V2 in steady-state.  
The second SOLVE card specifies
that V1 is to be ramped to 2 volts over a period of 10ns and
is left on until 25 ns.
Each solution is written to a file; the name of the file is 
incremented in a manner similar to that described above for
a dc simulation (UP1, UP2, etc.).
Note that an initial time step had to be specified on this card.
The third SOLVE card ramps V1 down from 2 volts to -1 volts in
20 ns (end of ramp is at t = 45ns).  
The device is then solved at this bias for another 55 ns (out to 100 ns).
Note that again each solution is saved in a separate file (DOWN1,
DOWN2, etc.) and that no initial time-step was required since
one had been estimated from the last transient solution
for the previous SOLVE card.
Finally, the fourth SOLVE card performs the steady-state solution 
at V1=-1 and V2=0.
.sp
.in +4
.ss 24
.nf
METHOD 2ND TAUTO AUTONR
SOLVE  V1=1 V2=0
SOLVE  V1=2 TSTART=1E-12 TSTOP=25E-9 RAMPTIME=10E-9 
+      OUTF=UP1
SOLVE  V1=-1 TSTOP=100E-9 RAMPTIME=20E-9 OUTF=DOWN1
SOLVE  V1=-1 V2=0
.in -4
.KE
.KS
.sp 2
Finally, an AC example is presented.
Assume the device to be simulated has 3 electrodes.
Starting from solved DC conditions at V1 = 0, 0.5, 1.0, 1.5 and
2.0 volts, 10 mV AC signals of frequency
1 MHz, 10 MHz, 100 MHz, 1 GHz, 10 GHz and 100GHz
are applied to each electrode in the device.
Note that the number of AC solutions to be performed is 5*6*3=90.
.sp
.in +4
.ss 24
.nf
SOLVE  V1=0 V2=0 V3=0 VSTEP=0.5 NSTEPS=4 ELECT=1
+      AC FREQ=1E6 FSTEP=10 MULT.F NFSTEP=5 VSS=0.01
.in -4
.KE
.eC
