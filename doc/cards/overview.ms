.in 4
.EQ
delim $$
gsize 11
.EN
.nr PS 11
.nr VS 13
.ps 11
.vs 13
.SH
Overview
.NH
Card format
.PP
PISCES-II takes its input from a user specified disk file.
The input is read by GENII, the same input processor that 
is used in SUPREM and other Stanford programs.
Each line is a particular statement, identified by the first
word on the card. The remaining parts of the line are the
parameters of that statement. 
The words on a line are separated by blanks or tabs. If more than
one line of input is necessary for a particular statement,
it may be continued on subsequent lines by placing a plus
sign (+) as the first non-blank character on the continuation
lines.
Parameter names do not need to be typed in full; only 
enough characters to ensure unique identification is
necessary. 
.PP
Parameters may be one of three types : numerical, logical
or character. Numerical parameters are assigned values
by following the name of the parameter by an equal sign (=)
and the value. Character parameters are assigned values
by following the name of the parameter by an equal sign (=)
and a sequence of characters. The first blank or tab
delimits the string.
.+B
The presence of a logical value indicates TRUE while
a logical value preceded by a caret (^) indicates FALSE.
.-B
.PP
In the card descriptions which follow, the
letters required to identify a parameter 
are printed in upper case, and the
remainder of the word in lower case. A phrase enclosed
in angle brackets <...> represents a parameter list
to be explained in further detail.
.+B
Bars in the right-hand margin denote changes in this version
of the manual.
.-B
.sp 
.NH
Card sequence
.PP
The order of occurrence of cards is significant in some cases.
Be aware of the following dependencies:
.ds PL "\(->
.IP \*(PL
The MESH card must precede all other cards, except TITLE, COMMENT
and OPTIONS.
.IP \*(PL
When defining a rectangular mesh, the order of specification is
.RS
.IP
MESH
.IP
X.MESH \fI(all)\fR  
.IP
Y.MESH \fI(all)\fR
.IP 
ELIMINATE
.IP
SPREAD
.IP
REGION
.IP
ELECTRODE
.RE
ELIMINATE and SPREAD cards are optional but if they occur they must
be in that order.
.IP \*(PL
DOPING cards must follow directly after the mesh definition
.IP \*(PL
Before a solution, a symbolic factorization is necessary. Unless
solving for the equilibrium condition, a previous solution must also
be loaded to provide an initial guess.
.IP \*(PL
Any CONTACT cards must precede the SYMBOLIC card.
.IP \*(PL
Physical parameters \fBmay not\fR be changed using the MATERIAL, CONTACT
or MODEL cards after the first SOLVE or LOAD card is encountered.
The MATERIAL and CONTACT cards precede the MODEL card.
.IP \*(PL
A PLOT.2D must precede a contour plot, to establish the plot bounds.
.IP \*(PL
PLOT.2D, PLOT.1D, REGRID or EXTRACT cards which access solution quantities
($ psi , n , p , phi sub n , phi sub p$, currents, recombination)
must be preceded somewhere in the input deck
by a LOAD or SOLVE card to provide those quantities.
.sp 
.NH
References (used below)
.IP [1]
Mark R. Pinto, Conor S. Rafferty and Robert W. Dutton, 
``PISCES-II - Poisson and Continuity Equation Solver,''
Stanford Electronics Laboratory Technical Report, Stanford
University, September 1984.
.br
.+B
.IP [2]
J. T. Watt, "Improved Surface Mobility Models in PISCES," presented
at Computer-Aided Design of IC Fabrication Processes, Stanford 
University, August 6, 1987.
.IP [3]
J. T. Watt and J. D. Plummer, "Universal Mobility-Field Curves for
Electrons and Holes in MOS Inversion Layers," presented at 1987
Symposium on VLSI Technology, Karuizawa, Japan.
.IP [4]
W. N. Grant, "Electron and Hole Ionization Rates in Epitaxial Silicon at 
High Electric Fields," \fISolid State Electron.\fP, vol. 16,
pp. 1189-1203, 1973.
.IP [5]
S. E. Laux, "A General Control-volume Formulation for Modeling Impact
Ionization in Semiconductor Transport," \fIIEEE Trans. Computer-Aided Design\fP,
vol. CAD-4, pp. 520-526, Oct., 1985.
.IP [6]
G. A. Baraff, "Distribution Functions and Ionization Rates for Hot
Electrons in Semiconductors," \fIPhysical Review\fP, vol. 128,
pp. 2507-2517, 1962.
.IP [7]
C. R. Crowell, and S. M. Sze, 
"Temperature Dependence of Avalanche Multiplication  
in Semiconductors," \fIApplied Physics Letters\fP, 9, pp. 242-244, 1966.
.IP [8]
N. Arora, J. Hauser and D. Roulston, "Electron and Hole 
Mobilities in Silicon as a Function of Concentration and Temperature,"
\fIIEEE Trans. Electron Devices\fP, vol. ED-29, pp. 292-295, 1982.
.IP [9]
J. M. Dorkel and P. Leturcq, "Carrier Mobilities in Silicon Semi-Empirically
Related to Temperature, Doping and Injection Level," \fI Solid State
Electron.\fP, vol. 24, pp. 821-825, 1981.
.IP [10]
H. Shin, A. Tasch, C. Maziar and S. Banerjee, "A New Approach 
to Verify and Derive a Transverse-Field-Dependent Mobility Model 
for Electrons in MOS Inversion Layers," \fIIEEE Trans. Electron Devices\fP,
vol. ED-36, pp. 1117-1123, 1989.
.IP [11]
V.M. Agostinelli, H. Shin, A. Tasch, "A Comprehensive Model of
Inversion Layer Hole Mobility for Simulation of Submicron Mosfets,"
NUPAD III Technical Digest, pp. 39-40, 1990.
.IP [12]
S. Schwarz and S. Russek, "Semi-Empirical Equations for Electron Velocity in
Silicon:  Part II--MOS Inversion Layer," \fI IEEE Trans. Electron Devices\fP,
vol. ED-30, pp. 1634-1639, 1983.
.sp
.-B
.FS \(dg
Note: All equations referenced in this appendix can be found in the
PISCES-IIA manual [1] unless otherwise stated 
(eg. an equation suffixed with ``IIB-sm'' can be found in the PISCES-IIB
supplementary manual).
.FE
.bp
