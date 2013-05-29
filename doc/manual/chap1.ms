.EQ
gsize 10
delim $$
.EN
.so macros
.sp 5
.nr H1 1
.tl '' Chapter 1 ''
.sp 2
.tl ''Introduction''
.sp 4
.PP
The following is a description of the PISCES-IIB semiconductor
device modeling program.  This document is intended to be
a supplement to the report on the earlier version of this
program, PISCES-IIA [1].
.PP
PISCES-IIB contains a number of enhancements over version II-A.
The major additions include a second-order time discretization
and automatic time-step selection algorithm, lumped resistive
and capacitive boundary conditions, current boundary conditions,
distributed contact resistance, small-signal analysis,
Fermi-Dirac statistics (including incomplete ionization) and
a new grid generation program (written in C).
Many other enhancements and improvements have been made and 
are either explicitly described in this document or are
simply included in the revised description of the input language.
.PP
This report begins in chapter 2 with a brief description of a
number of the new features as mentioned above.  
In chapter 3, some
observations will be made with regard to general simulation issues.  
Chapter 4 is a guide to the new grid generation system,
while chapter 5 contains simple, reproducible
examples incorporating some of the new analytical capability.
Finally the appendix contains a complete description of the 
revised input language.
.sp 1
.SH
References
.IP [1]
Mark R. Pinto, Conor S. Rafferty and Robert W. Dutton, 
``PISCES-II - Poisson and Continuity Equation Solver,''
Stanford Electronics Laboratory Technical Report, Stanford
University, September 1984.
