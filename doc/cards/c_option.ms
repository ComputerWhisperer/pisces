.\" Tue Sep 20 10:55:41 PDT 1988 (dredge--Stanford)
.EQ
delim @@
gsize 11
.EN
.bC OPTIONS
.NH 0
Syntax  
.P1  
Options  <run control> <plot control>
.P3
.NH
Description
.IP 
The OPTIONS card sets options for an entire run.
.NH
Parameters
.sp 2
.RS
.IP "<run control>"
.sp 1
.in +4
.TS
l l l l.
G.debug or Debug	\\=	<logical>	(default is false)
N.debug	\\=	<logical>	(default is false)
CPUStat	\\=	<logical>	(default is false)
CPUFile	\\=	<character>	(default is pisces.cpu)
.TE
.in -4
G.DEBUG (or DEBUG) and N.DEBUG print debugging information 
to the \*(PI standard output.  G.DEBUG (or DEBUG) 
prints general information, 
N.DEBUG more specifically numerical parameters. CPUSTAT
is a flag to indicate that a cpu profile of the solution process
is to be printed to the file specified by CPUFILE.
.sp 2
.IP "<Plot control>"
.in +4
.TS
l l l l.
.+B
PLOTDevice	\\=	<character>	(default login terminal)
PLOTFile	\\=	<character>	(default device specific)
.-B
.\" HP2648	\\=	<logical>	(default true)
.\" HP2623	\\=	<logical>	(default false)
.\" Tek4107	\\=	<logical>	(default false)
X.Screen	\\=	<real>	(default devices size)
Y.Screen	\\=	<real>	(default devices size)
X.Offset	\\=	<real>	(default  0 inches)
Y.Offset	\\=	<real>	(default  0 inches)
.TE
.in -4
.+B
The first parameter specifies the output plot device.  If no device
is given, a default (usually the user's graphics terminal) will
be used.  Most versions of \*(PI use the PLOTCAP graphics package from
Stanford.  Please refer to the PLOTCAP documents for further details.
The full set of supported devices is contained in the PLOTCAP
data base. Possibilities include:
.RS
.TS
tab(~);
l l l l.
hp2648~hp2623~tek4107~vt240
laserwriter~ditroff~latex~xwindows
hp9873~printronix~sunview~save
.TE
.RE
Plots will be scaled to the size of the specific device.
Also note that on
.-B
.\"ig
.\"The first three parameters may be used to change the plotting
.\"device. In releases II-A and II-B, the Hewlett-Packard HP2648 and HP2623
.\"graphics terminals, and the Tektronix 4107 color graphics terminal,
.\"are supported. On
.\"..
color graphics terminals, the different line types
are implemented as different colors; on the black and white monitors
they are implemented as dot and line patterns.
X.SCREEN is the physical width of the screen
and Y.SCREEN is the height. They are set automatically depending
on the plot device, but can be altered for special effects (split
screen plots, for instance). 
The offset from the bottom left corner of the screen may be set 
using X.OFFSET and Y.OFFSET.
.IP
.+B
The output file is generally defined by the plot device. For example,
a graphics terminal will use the terminal as the output \fIfile\fR.
Printers may have the output file be a spooler.  The graphics
output file can be explicitly set by the the PLOTFILE command.
All graphics output will then be routed to the given file.
Note that the contents of the file will be in a format specific
to the given device.
.-B
.RE
.KS
.NH
Examples
.IP
The following sets up a plot for a Tektronix terminal, using a small
centered window. Cpu information is also logged to the default file.
.+B
.P1
OPTIONS   PLOTDEV=tek4107 X.S=6 Y.S=5 X.Off=1 Y.OFF=0.5  
+         CPUSTAT
.P2
.IP
Here we set the plot device to the LaserWriter and save the output
in a file called \fBplot.ps\fR.
.P1
OPTIONS   PLOTDEV=lw  PLOTFILE=plot.ps
.P2
.-B
.KE
.eC
