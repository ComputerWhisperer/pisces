\"...The names of various programs.
.ds sk \fBskel\fR
.ds Sk \fBSkel\fR
.ds tr \fBtri\fR
.ds Tr \fBTri\fR
.ds IG \s-2IGGI\s+2
.ds PI \s-2PISCES-II\s+2
.ds P0 \s-2PISCES\s+2
.ds P1 \s-2PISCES-I\s+2
.ds SU \s-2SUPREM-III\s+2
.ds S4 \s-2SUPREM-IV\s+2
.ds gp \fBgplot2\fR
.ds Gp \fBGplot2\fR
.ds dp \fBdplot\fR
.ds Dp \fBDplot\fR
.EQ
gsize 10
delim $$
.EN
.so macros
.sp 5
.nr H1 4
.tl '' Chapter 4 ''
.sp 2
.tl ''Grid Generator''
.sp 4
.NH 2
Introduction
.PP
This release of \*(PI introduces a new grid generator.
It is written in the C language, and is much more portable
than the Pascal program provided in the first release (\*(IG).
The triangulation algorithm, due to R.E. Bank[1],
is new and automatically smooths the grading from coarse to fine areas of
the grid. This relieves the user of much of the work in generating
a grid.
The grid generation process has been split into two
phases, editing and triangulation. There are separate programs,
named \*(sk and \*(tr for each phase. \*(Sk is an interactive editor
where the user defines the skeleton of a structure. \*(Tr is the
triangulation phase, where the skeleton is filled with triangles.
The programs are treated separately in the documentation that follows.
The chapter is divided into three sections: a lengthy description
of \*(sk, then a Unix-style manual page for each program.
.NH 2
Grid Editing with Skel
.NH 3
Getting started
.PP
This section is an introduction to \*(sk. The discussion will make
more sense if you are running \*(sk on a graphics terminal while
you are reading this. Sections 1 through 6 discuss the main features
of the editor, and section 7 discusses the connection between 
\*(sk, \*(tr, and \*(PI.
Finally Section 8 offers some suggestions on using \*(sk.
.NH 4
Specifying terminal type
.PP
Before you start \*(sk you must tell it what terminal 
you are using. This is done (in Unix) by setting the environment variable
TERM. At present only the following combinations are supported:
.TS
center;
c
l l l l.
Output device	Input device	Code
_
HP2623	hp2623 cursor	TERM=hp2623	
HP2648	hp2648 cursor	TERM=hp2648
.T&
c s s s.
(coming soon)
.T&
l l l l.
9000 console	HP9111 tablet	TERM=hp9020
TEK4107	tek mouse	TERM=tek4107
TEK4107	tek tablet	TERM=tekt
.TE
Your terminal type is normally defined when you log in. 
.PP
It is important that you define the right terminal before starting
\*(sk, because the graphics initialization sequence for one terminal will
generally cause another to lock up, display strobe lights, or self-destruct.
In particular, you need to be specific about which of a manufacturer's 
models you are using, because the screens are different sizes for (say) a
HP2648 and a HP2623.
.NH 4
The Display
.PP
Assuming the initialization went correctly,
both the alpha and graphics terminal screens should clear, a large box with axes
should appear on the right of the screen, and several small boxes with
text should appear on the left. The right box is the \fImesh\fR window.
Initially it is empty, and has a range of roughly 0-1 in 
both x and y directions.
The exact numbers depend on the dimensions of the screen; the bounds are 
adjusted so that an inch of screen represents the same length regardless
of its direction. 
.PP
The left boxes constitute the command menu, and the text labels are the commands
that are available. All the commands have operands; when a command
is chosen, a second menu is displayed to the right of the first, and
its boxes display the possible operands for that command.
Above the command menu is an asterisk, indicating that \*(sk is waiting
for input from that menu. 
When input is expected from the operand menu, the asterisk will move to 
that menu, and when input is expected in the mesh window, 
the asterisk will be replaced by a longer text over the mesh.
By observing which of the windows is labeled, you can always tell what
state the program is in. The moving label is referred to as the run light.
.PP
One other area of the display is used, and is initially blank. This is the
top line of the display, used for text input/output and printing warning
messages. When several messages arrive in this buffer in succession, the
program will make sure you have read each message before displaying the
next by prompting with the message ``--More--''. The next message can be
displayed by typing a carriage return.
.NH 4
Input
.PP
Most interaction with \*(sk is by moving the graphics cursor to some
location on the screen and indicating that you have finished moving it.
Using cursor keys, the cursor is moved by holding down the appropriate 
arrow keys, and the location is taken when any alphanumeric key is struck.
With the tablet, the pen is moved above the surface of the tablet; the cursor
should follow its motion. The location is taken when the pen is pressed to
the surface. Either method is referred to as pointing the cursor.
If possible you should try to find a tablet; moving the cursor keys
can get tedious.
.PP
To choose a menu item, point the cursor at a menu box.
When using keyboard cursor keys, if the character struck
is the first letter of any of the commands, that command
is chosen regardless of the cursor position. This helps cut down cursor motion.
(It only works when menu input is expected.)
The graphics cursor is also used for indicating objects and positions in the
mesh window. When a position is required, you can always give numeric
data by typing `n' to the graphics prompt.
There is also a display option (section 4.2.4.2)
which turns on a regularly spaced background grid and rounds input data
to the nearest grid point.
.PP
Some commands require text input, which cannot be represented with
the graphics cursor. In these cases a prompt appears in the top line buffer,
and you type input in the same way you would in talking to any other program.
.NH 4
Quitting
.PP
You can exit \*(sk by choosing the `quit' box on the main level menu, and
choosing `yes' when it asks to make sure. If you choose `no' you return
to the main level without affecting anything.
One thing to remember is that most \*(sk states are repetitive.
For example, if you give a create command, the 
possible objects you can create will be listed in the operands menu,
and you will remain in the create state until you quit it (by choosing
the quit box in that menu). 
Until you quit the create state, items chosen from the main menu are ignored.
Furthermore, once you have chosen to create, say, nodes,
the program will continually wait for node locations in the mesh window
until you move the cursor outside it. 
Thus in the most general case, you may have to pop up three levels, from
the mesh window to the second level menu, from the second menu to the
first, and from the first to the outside world. 
The run light is useful for telling how deep you are.
.NH 3
Objects
.PP
A skeleton grid is a collection of three types of object, 
nodes, edges and regions. 
Nodes may belong to edges, and edges may belong to regions, so that
the hierarchy of objects is 
.EQ
region~~~->~~~edge~~~->~~~node
.EN
.sp
There are commands to create, destroy and manipulate these objects.
A fourth object, triangle, is also recognized, but no commands apply to it. 
The purpose of the program is to create only the outline of a structure; 
you are not expected to manipulate triangles by hand!
In terms of the hierarchy, triangles have links to all three primary types.
We examine each of the objects briefly in this section.
.NH 4
Nodes
.PP
A node is the most primitive object. It has three numbers associated
with it, its $x$ and $y$ coordinates, and its spacing value, to be discussed
later (Section 7). 
Nodes can be created at will, and can be destroyed if they
are not on any existing edge.
Commands which expect you to choose a node do not require an exact position.
The cursor needs only to be closer to the desired node than to any other.
.NH 4
Edges
.PP
Edges consist of a pair of nodes and a boundary condition code.
The boundary condition code has no significance for \*(sk, and is
passed on through the triangulation phase to be interpreted by whatever
simulator reads the grid. \*(PI interprets this code as an electrode
number, \*(S4 uses it to choose the boundary condition.  
The default code is zero.
Edges are created and destroyed automatically by commands which alter regions.
Edges are displayed as dotted lines, with different line types to
distinguish zero and non-zero electrode codes.
Commands which expect you to choose an edge do not require an exact position.
You need only be closer to the desired edge than to any other.
(The perpendicular distance to the edge is used for the comparison.)
.NH 4
Regions
.PP
A region is the most complex structure. It consists of a material number,
which is only of significance to the simulators, and a list of edges.
The list must have at least three elements (otherwise, there would be no 
way to display or pick that region), and is always closed.
Since a region does not belong to any higher object, it can be 
destroyed at will. 
Regions are not explicitly displayed at present, since the edges which
define the region are visible.
To choose a region, point the graphics cursor inside the region.
A point is defined to lie ``in'' the region 
if a line drawn
to infinity crosses an odd number of edges, and ``outside'' that
region if it crosses an even number of edges. 
.NH 3
Operators
.PP
In the following, the notation word1-word2 denotes a 
command whose name on the first menu is word1 and on the second
menu is word2. To invoke any command, point at word1 on the
main menu and then word2 on the sub-menu that appears.
The main operators available are
.NH 4
Create
.PP
You may create nodes or regions.
To create a node, point at the desired location. It is illegal
to create a node on top of another. Nodes created on top of an edge
do \fInot\fR automatically become part of that edge.
The create node command is repetitive, that is,
nodes continue to be created until the cursor is moved outside
the mesh window.
To create a region, pick a series of counterclockwise nodes. 
The boundary is updated to reflect the addition of each node.
If less than two nodes are chosen, the command is
aborted without creating any region. It is important to choose
the nodes counterclockwise, otherwise the join and split commands
will get confused (this problem should go away).
.NH 4
Destroy
.PP
You may destroy nodes or regions. Point the cursor at the node or inside
the appropriate region.
The destroy-node command is repetitive (be careful!).
.NH 4
Move
.PP
You may move a node by picking the node and pointing at its new location. 
You may move an edge or region by picking the edge or
region, then indicating a displacement vector by pointing
at a start position and an end position. The edge or region is moved
by the difference between the end and start position.
You may move the origin by pointing at a node; that node then 
becomes the origin, and all positions are adjusted.
There is also a move-block command which is useful for shifting whole
areas of a grid, for instance in generating several MOSFET grids which
differ only in their channel lengths. The idea is that you define a
section of grid to be stretched. Everything lying outside the section
on one side is shifted according to a specified displacement vector,
and everything on the other side remains fixed. Nodes lying inside the
section are moved by an appropriate fraction. The syntax for doing
this is a little convoluted: the section to stretch is defined as
the area between two parallel lines, which are perpendicular to
and situated at either end of a given line segment. 
The segment is specified by giving its start and end ($x$ and $y$ in Fig.1).
The displacement vector is then defined by giving a third point ($z$), 
usually but not necessarily collinear with the first two. 
Figure 1 might make this more clear.
.KF
.PS
ds = 0.05
define dmia |
[
    move left ds
    line up ds right ds then down ds right ds then down ds left ds then up ds left ds
    move right ds
] |
bs = 2

A: box height 0.5*bs width bs
dmia at A.ne
dmia at A.nw
dmia at A.se
dmia at A.sw
B: box height 0.1*bs width 0.333*bs with .bot at last box.top
dmia at B.ne
dmia at B.nw
dmia at B.se
dmia at B.sw

AA: box height 0.5*bs width 1.2*bs with .nw at A.sw + (0, -0.5*bs)
dmia at AA.ne
dmia at AA.nw
dmia at AA.se
dmia at AA.sw
BB: box height 0.1*bs width 0.533*bs with .bot at last box.top
dmia at BB.ne
dmia at BB.nw
dmia at BB.se
dmia at BB.sw

box invis height 0.25*bs width 0.75*bs "x,y define the" "segment" with .nw at A.ne
box invis height 0.25*bs width 0.75*bs "z-y is the disp-" "lacement vector" with .sw at A.se
box invis "Before" with .e  at A.w
box invis "After" with .e at AA.w

line dashed from A.top + (0,0.25*bs) to AA.sw + (0.5*bs, -0.1*bs)
line dashed from B.se  + (0,0.25*bs) to B.se  - (0, 0.75*bs)
line dashed from BB.se + (0, 0.25*bs) to BB.se - (0, 0.6*bs)
circlerad=0.05
circle invis "x" with .e at A.center
line -> to A.center + (0.1667*bs, 0)
circle invis "y" with .w at last line.end
circle invis "z" with .center at A.center + (0.367*bs, 0)
.PE
.sp
.ce 
Figure 1 - Using Block Move
.sp
.KE
.NH 4
Alter
.PP
The alter command is used to change parameters that are
not represented on the display. For nodes, you can modify
the spacing value. For edges, you can change the electrode code. 
For regions, you can change the material number. 
Input for these commands is typed on the top line.
For convenience, all numerical data associated with an object
is displayed first, so this can be used to find the exact
coordinates of a node.
.NH 4
Split
.PP
You can split an edge or a region. To split an edge, pick
the edge, indicate which end to start adding at,
then pick a series of new nodes. For each node added,
the edge is split in two. The nodes must already exist.
(Figure 2).
.KF
.PS
ds = 0.05
define dmia |
[
    move left ds
    line up ds right ds then down ds right ds then down ds left ds then up ds left ds
    move right ds
] |

bs = 2
boxht = 0.1 ; boxwid = 2*boxht
define hoho |
[
box height 0.5*bs width bs
dmia at last box.ne
dmia at last box.nw
dmia at last box.se
dmia at last box.sw
Y:dmia at last box.sw + (0.333*bs, 0.3*bs); 
Z:dmia at last box.sw + (0.333*bs, 0.2*bs); 
X:dmia at last box.ne - (0.333*bs, 0)
XX:dmia at last box.se - (0.333*bs, 0)
box invis "y" with .e at Y.w
box invis "z" with .e at Z.w
box invis "x" with .s at X.n
] |
A: hoho
line from A.X to A.XX
box invis "E" with .w at A.X - (0, 0.25*bs)
box width 2 invis "Splitting edge" "E starting at x" with .w at A.e
B: hoho with .top at A.bot - (0, 0.1*bs)
line from B.X to B.Y
line from B.Y to B.XX
box invis width 2 "After adding" "node y" with .w at B.e
C: hoho with .top at B.bot - (0, 0.1*bs)
line from C.X to C.Y
line from C.Y to C.Z
line from C.Z to C.XX
box invis width 2 "After adding" "node z" with .w at C.e
.PE
.sp
.ce 
Figure 2 - Splitting an Edge
.sp
.KE
To split a region, pick the region and two non-adjacent nodes belonging
to that region. The region will be split along the line joining those
nodes.
.NH 4
Join
.PP
You can join edges or regions. To join edges,
pick two adjacent edges. They will be condensed into one, and
the node they previously shared will be free for removal.
(Figure 3). Join-edge then waits for another edge to join,
and is terminated by moving the cursor outside the graphics
window.
To join regions, pick two adjacent regions. At present, the regions
must be separated by just one edge, as they would be after a split-region
command. To join two regions which have an extended interface, first use
join-edge to telescope the interface down to one edge.
.KF
.PS
ds = 0.05
define dmia |
[
    move left ds
    line up ds right ds then down ds right ds then down ds left ds then up ds left ds
    move right ds
] |
bs = 2

define hoho |
[
box height 0.5*bs width bs
dmia at last box.ne
dmia at last box.nw
dmia at last box.se
dmia at last box.sw
Y:dmia at last box.sw + (0.333*bs, 0.3*bs); 
Z:dmia at last box.sw + (0.333*bs, 0.2*bs); 
X:dmia at last box.ne - (0.333*bs, 0)
XX:dmia at last box.se - (0.333*bs, 0)
] |
A: hoho
line from A.X to A.Y ; line to A.Z ; line to A.XX
circle invis "x" with .se at 1/2 way between A.X and A.Y
circle invis "y" with .e at 1/2 way between A.Y and A.Z
circle invis "z" with .ne at 1/2 way between A.Z and A.XX
box width 2 invis "Joining edge" "x and y" with .w at A.e
B: hoho with .top at A.bot - (0, 0.1*bs)
line from B.X to B.Z
line from B.Z to B.XX
box width 2 invis "Joining z to" "the new edge" with .w at B.e
C: hoho with .top at B.bot - (0, 0.1*bs)
line from C.X to C.XX
.PE
.sp
.ce 
Figure 3 - Joining edges
.sp
.KE
.NH 3
Other commands
.NH 4
Window commands
.PP
The window on the mesh may be manipulated in several ways, primarily zooming
and panning. The commands are listed below.
.NH 5
Fresh
.PP
The window is simply refreshed. Useful when your screen is messed by an
interrupt, or if some command hasn't made all the consequent changes to
the display. (This problem should go away).
.NH 5
In
.PP
Zoom in on a part of the grid, specified by pointing 
at the lower left and upper right of an imaginary box. Window-in and
window-out use a stack of windows; window-in creates a new window each
time it is invoked, and the previous window is pushed on a stack.
.NH 5
Out
.PP
Pops the stack and redisplays the mesh at the previous size. If you
are already at the root of the stack, window-out prompts for the coordinates
of a new window in the top line buffer. This is the only way to expand
a window to be bigger than the current mesh limits.
.NH 5
Pan
.PP
The window is moved, at the present scale, over the mesh. Window-pan 
prompts for the start and end of a displacement vector. The window is
moved by that vector.
.NH 5
Reset
.PP
The window stack is cleared, and the mesh displayed in a window just
large enough to accommodate the entire mesh.
.NH 4
Display options
.PP
There are a number of options controlling what is displayed on the screen.
Some of these are merely to speed up the display on slow terminals
(which means 4800 baud in this context), others are useful.
All the options are toggles.
.IP Nodes 12
Display nodes. Initially on. May be turned off to get rid of those diamonds.
.IP Edges 12
Display edges. Initially on. 
.IP Regions 12
Display region. Initially off. (Not supported at present).
.IP Triangles 12
Display triangles. Initially off. 
.IP Obtuse 12
Color in obtuse triangles. Initially off. 
.IP Stretch 12
Stretch the mesh to fill the screen. Proportionality is lost. Initially off.
.IP Grid 12
Set up a background grid to assist in locating the cursor accurately.
Initially off. The background grid is usually necessary for precision
work. With this option enabled, input to any command that requires positions 
(create node, move-anything, window-in) is rounded to the nearest point
on the background grid.
Option-grid prompts for the $x$ and $y$ grid spacing. If the spacing is so
small that the display would be covered with thousands of grid points,
the spacing used in the display is rounded up accordingly and you are
warned. The spacing used in rounding input data is the specified one,
not the one used in the display.
.IP Axis 12
Display axes. Initially on. Can be turned off to conserve baud.
.PP
There is one more display option, given on the command line when the program
is invoked. The \fB-s\fR option (``short'') abbreviates all text displayed
on the screen to one character and makes menu display much faster. 
Initially you will want to see the full texts, though. 
.NH 4
File commands
.PP
At present there are three file commands in \*(sk: file-read, 
file-write, and file-dplot. File-read creates a text file 
describing the grid, which can be read back by file-write.
Several files can be read in the same run, but you should
be careful not to read grids that overlap. File-dplot
dumps a file that can be read with the \*(dp system
program. The purpose of this option is to allow you to create
simple stick pictures using the graphics facilities of \*(sk, and
make hard copy using \*(dp.
.NH 3
Miscellaneous
.NH 4
Error handling/Terminating commands
.PP
If you give too little, or invalid, input to a command, \*(sk ignores
the command, and prints either a warning message or the message ``Aborted''
in the top line buffer. Any half-completed command can be aborted by simply
moving the cursor outside the mesh window and choosing another command.
Errors in repetitive commands terminate the repetition.
.NH 4
Differences from \*(IG
.PP
For users familiar with the earlier program \*(IG, the following
list of differences may help in the transition to \*(sk.
First, there are new commands to manipulate regions. In particular,
you can create a new region. (In \*(IG, it was only possible to split
and join regions). This has the advantage of being much more flexible, 
and the disadvantage of introducing the possibility to create
overlapping regions.
Secondly, the triangulation and drawing programs have been separated,
to allow you to run the triangulation in the background. The triangulation
algorithm is smarter than that used in \*(IG, and usually does not require
any user intervention (no more ``region densities''). As a result, the
triangle type is better described as tolerated rather than supported in \*(sk.
It is only included to allow you to use the interactive zoom and pan
commands to inspect a finished mesh, and to fine-tune triangles by moving
nodes around. Another change is the menu handling, where all input is
in graphics mode to allow users with tablets to dispense with the keyboard
(except for file names).
.NH 3
Interface to \*(Tr.
.PP
The output of \*(sk is a list of nodes, edges, and regions. 
\*(PI requires a triangular grid which covers 
the entire structure.
The triangulation program \*(tr performs the mapping from one to the other.
\*(Tr imposes a particular discipline on its
input, in order to generate valid triangulations. It expects a collection
of regions that are simple polygons, without any holes or self-crossings.
The regions are defined in terms of edges which in turn are defined in terms
of nodes, and there are no unconnected edges or nodes. Edges are
handled automatically by \*(sk, but the user must check whether there are
stray nodes before passing the input to \*(tr.
.PP
\*(Tr begins by computing a local grid spacing (the ``\fIh\fR'' value)
for each node, by looking at how closely other nodes approach that node.
If a value was specified with \*(sk's alter-node command, that value overrides
the computed value.
Small values force finer grid. 
Both user-specified and automatic spacing values are smoothed to 
avoid large discrepancies between neigboring nodes.
The edges are then subdivided in such a way that the
spacing at each end of the edge is the value of $h$ for the node at
that end. The node spacing inside an edge varies geometrically 
between the two end values. 
After the edge division phase, the triangulation proceeds by recursively
breaking each region into smaller regions, adding new nodes as necessary, until
eventually the subregions degenerate into single triangles. 
.PP
Finally there is an optimization phase, where various manipulations are
applied to the grid to make the angles as close to 60\(de as possible.
Depending on the size of the grid and how much optimization is done,
this can be quite slow. It was for this reason that the triangulation
was separated from the input phase, so that the optimization can be
run in the background. See the \*(tr manual page for the options
available.
The triangulated grid is then written to a file that can be read by
\*(PI, or reread with \*(sk. \*(Sk does not allow any database
operations on a triangulated grid; the only valid operators are
the move operators, which may be used to fine-tune the angles.
.NH 3
\*(Sk usage and idioms
.PP
The typical sequence in using \*(sk is shown in Figure 4.
.KF
.PS
u = 0.6
box invis height 0.5 width 1.0
L1:box width 2*u height 2*u "Text editor" "c 1 $x sub 1 y sub 1$ 0" "c 2 $x sub 2 y sub 2$ 0" "..."
line -> from L1.s down 0.5*u
L2:box width 1*u height 0.5*u "% skel"

R1: box width 1*u height 0.5*u "% skel" with .nw at L1.nw + (4*u, 0*u)
line -> from R1.s down 0.5*u
R2: box width 3*u height 0.5*u "window-out (to set bounds)" 
line -> from R2.s down 0.5*u
R3: box width 2*u height 1*u "create-node" "create-node" "..."

C1: box width 4*u height 0.5*u "create-regions (join the dots)" with .top at L1.top + (2*u, -4*u)
line -> from L2.s to C1.n
line -> from R3.s to C1.n
line -> from C1.s down 0.5*u
C2: box width 4*u height 0.5*u "alter-regions (define material numbers)"
line -> from C2.s down 0.5*u
C3: box width 4*u height 0.5*u "alter-edges (define electrodes)"
line -> from C3.s down 0.5*u
C4: box width 2*u height 0.5 "file-write"
line -> from C4.s down 0.5*u
C5: box width 2*u height 0.5*u "% tri"
.PE
.sp
.ce
Figure 4 - Skel Flow Chart
.sp
.KE
.PP
Coordinates are entered using either a text editor or directly with
the \*(sk create-node command. Then the nodes are linked into regions,
and the regions are assigned material numbers. After electrodes are
defined, the mesh can be saved and triangulated with \*(tr.
The material numbers have the same meaning as before.
.PP
There are a few idioms that may help you in using \*(sk.
In no particular order, they are...
.IP \(bu
Most people using \*(sk have an accurate picture of the desired structure,
with known coordinates for each point.
In this situation, it makes more sense to use a text editor to put
the coordinates into a file in \s-2ASCII\s+2 grid format (Appendix I). 
Then read the text file and use \*(sk to set up the edges and regions.
.IP \(bu
If you need to create a region where some of the nodes are so close
together that they can't be distinguished on the display, it need not be
done in one fell swoop.
You can create the region using just the well-spaced nodes, then window
in on the smaller features and use split-edge to add the remaining
nodes.
.IP \(bu
The edge-spacing algorithm uses the spacing at either end to determine
the number of subdivisions. Thus a long edge separating two detailed
features will be inappropriately subdivided. By adding one node at
the center, the grid spacing will increase from one end to the center,
and decrease back down to the other end.
.IP \(bu
When working with \s-2MOSFET\s+2s, you may find that the default grid spacing
around the thin oxide is too conservative, causing too many nodes
to be generated. The number of nodes can be reduced (at the cost of
accuracy) by specifying a coarse spacing value for the nodes above and
below the oxide. 
.IP \(bu
To make two copies of an object (for whatever reason), do file-write, then
move-block to shift the entire structure somewhere else, and file-read to
read in the disk copy (which will be placed in the original position).
.NH 3
Appendix - Grid format.
.PP
The programs dealing with grid all share a common format. Text files are
used for a variety of reasons, primarily because they can be sent over simple 
links from CAD stations to simulation machines, and they can be inspected 
or created directly by users. The format is a list of nodes, edges, regions
and triangles in that order. Both \*(sk and \*(tr expect regions to
be closed and counterclockwise, with no free edges. \*(Tr also expects
no free nodes.
Each object is on a separate line, identified
by the first character on that line. 
.LP
A node is represented as
.sp
.ti +4
\fBc \fIn x y h\fR
.sp
where \fBc\fR (`coordinate') is the identifying character, \fIn\fR is the
number of the node, referenced by subsequent edges, and \fIx, y\fR and \fIh\fR 
are the coordinates and the spacing value of that node. To get the default
spacing, give $h~=~-1$.
.LP
An edge is represented as
.sp
.ti +4
\fBe \fIn i j b\fR
.sp
where \fBe\fR is the identifying character, \fIn\fR is the number
of that edge, referenced by subsequent regions, \fIi, j\fR are the nodes
on that edge and \fIb\fR is the boundary code of that edge.
.LP
A region is represented as
.sp
.ti +4
\fBr \fIn m\fR
.sp
where \fBr\fR is the identifying character, \fIn\fR is the number of the
region, \fIm\fR is the material code of that region. This initial card
is then followed by a list of the region's edges in the form
.sp
.ti +4
\fBb \fIn\fR
.sp
where $n$ is the number of the edge.
.LP
A triangle is represented as
.sp
.ti +4
\fBt \fIn r i1 i2 i3 e1 e2 e3
.sp
where \fBt\fR is the identifying character, \fIn\fR is the number of the
triangle, \fIi1 i2 i3\fR are the nodes of the triangle, and \fIe1 e2 e3\fR
are the boundary codes of the triangle sides.
.SH
References
.IP [1]
R.E. Bank, W.Fichtner
\fIPLTMG User's Guide\fR, Bell Laboratories Technical Memorandum 82-52111-9
