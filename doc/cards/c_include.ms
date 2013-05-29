.\" Tue Dec  4 23:15:50 PST 1990 (anderson--stanford)
.EQ
delim $$
gsize 11
.EN
.bC INCLUDE
.NH  0
Syntax
.sp 2
.R
.in +4
INClude <filename>
.sp
.in +11
or
.in -11
.sp
SOURCE <filename>
.in -4
.sp 2
.NH 
Description
.IP 
The INCLUDE statement provides a shorthand way to include information 
from other files in the \*(PI input file.  The statements in the
INCLUDEd file will be inserted into the \*(PI input file in place of the
INCLUDE statement when the input file is processed.  The statements in the
INCLUDEd file must use correct \*(PI input syntax, and they must be in
correct order with respect to the other statements in the \*(PI 
input file when the INCLUDEd file is expanded by the input parser.
This is most useful for libraries of material and model parameters.
.sp
.NH
Examples
.LP
.sp 2
.in +4
.ss 24
.nf
INCLUDE MAT.init
.fi
.eC
