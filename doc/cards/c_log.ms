.EQ
delim @@
.EN
.bC LOG
.NH  0
Syntax
.sp
.R
.in +4
LOG <file specification>
.in -4
.sp 2
.NH 
Description
.IP
The LOG card allows the I-V and/or AC characteristics of a run to be 
logged to disk.
Any I-V or AC data subsequent to the card is saved. If a log file is 
already open, it is closed and a new file opened.
.NH 
Parameters
.RS
.IP "<file specification>"
.TS
l l l.
Outfile or Ivfile	\\=	<filename>
Acfile	\\=	<filename>
.TE
.LP
OUTFILE or IVFILE specify the log file for I-V information.
ACFILE specifies the file for AC data.
.sp
.RE
.NH
Examples
.IP
Save the I-V data in a file called IV1 and AC data in AC1.
.sp 2
.in +4
.ss 24
.nf
LOG OUTF=IV1 ACFILE=AC1
.fi
.eC
