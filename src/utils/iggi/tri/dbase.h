/* $Header: /users/suprem/ig2/tri/RCS/dbase.h,v 1.5 85/10/30 23:25:37 conor Exp $*/
/* Data base for tri - triangle generator. */

/***************************************************
 *                                                 *
 *            Constants.                           *
 *                                                 *
 ***************************************************/

#define MAXNODE         3000
#define MAXTRI          6000
#define MAXEDGE		6000
#define MAXREG   	  20

#define BEFORE             1
#define AFTER              0
#define NO_ELEC		-32767	/* "Impossible" electrode code */
#define OFFSET_ELEC     -1024	/* Distinguish BC from triangles by offset*/

/***************************************************
 *                                                 *
 *            Macros                               *
 *                                                 *
 ***************************************************/
#define nF(E)    (E->iscc ? edge[E->edge]->n[1] : edge[E->edge]->n[0])
#define nB(E)    (E->iscc ? edge[E->edge]->n[0] : edge[E->edge]->n[1])
#define WALK(START,F,B) for (B=0, F=START;  F!=B; F=F->next, B=START)
#define ined(N,E) ((N)==edge[E]->n[0] || (N)==edge[E]->n[1])
#define ELEC(N)  (node[N]->elec != NO_ELEC)

/***************************************************
 *                                                 *
 *            Types.                               *
 *                                                 *
 ***************************************************/

struct  Scord   {
		double x,y;     /* basic coordinate structure. */
		};

#ifndef SLINK
struct Slink    {
		int i;			/* objects being linked. */
		struct Slink *next;	
		};
#endif


struct  Snode 	{ 
		double x,y;	/* coordinates */
		double h;	/* local spacing */
		int elec;	/* is on electrode - a simple kludge for opt */
		struct Slink *p2p;	/* list of neighbor nodes */
		struct Slink *p2t;	/* list of neighbor triangles */
		};

struct  Stri  	{ 
		int reg; 	/* Region this triangle belongs to. */
		int n[3];	/* nodes of this triangle. */
		int e[3];	/* neighbor/edge codes */
		};

struct  Sedge 	{ 
		int elec; 	/* Electrode number of this edge. */
		int n[2];	/* Nodes of this edge */
		int r[2];       /* Regions containing this edge.  */
		int orig;	/* Original edge, if subdivided.  */
		};

struct  LLedge 	{ 
		int edge;		/* Index of edge */
		int iscc;               /* Is the edge c-c in the region*/
		struct LLedge *next;	/* Next edge in the linked list */
		struct LLedge *prev;	/* Last " */
		double ang;		/* Angle between this & prev edge. */
		struct LLedge *gt;	/* Edge with greater angle. */
		struct LLedge *lt;	/* Edge with lesser  angle. */
		};

struct  Sreg  	{ 
		int len;		/* length of region. */
		int mat;		/* Material index of this region. */
		struct LLedge *bnd; 	/* Pointer to linked list for region. */
		struct LLedge *maxa;	/* Pointer to edge of max angle. */
		struct LLedge *mina;	/* Pointer to edge of min angle. */
		};


/***************************************************
 *                                                 *
 *            Variables.                           *
 *                                                 *
 ***************************************************/


/****
   Use the following trick to declare the structure
   everywhere, but allocate storage only once.
 ****/

#ifndef STATIC_ALLOCATION_TIME
#define EXT extern
#else
#define EXT
#endif

EXT struct Snode * node[MAXNODE];	/* Allocate storage for dbase. */
EXT struct Stri  *  tri[MAXTRI];
EXT struct Sreg  *  reg[MAXREG];
EXT struct Sedge * edge[MAXEDGE];

EXT int nnode,ntri,nreg,nedge;  	/* Working maxima */

EXT int summary,debug1,debug2;

EXT double mr;				/* Maximum spacing ratio. */
EXT double mgeom;			/* Minimum triangle geometry. */

/***************************************************
 *                                                 *
 *            Functions.                           *
 *                                                 *
 ***************************************************/
char * ad_edge (); 
char * add_ang ();
char * cr_edge ();
char * cr_node ();
char * cr_reg ();
char * cr_tri ();
struct LLedge * eindex ();
