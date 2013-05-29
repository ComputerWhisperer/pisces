/* $Header: /users/suprem/ig2/skel/RCS/dbase.h,v 1.3 85/10/18 13:39:49 conor Exp $*/

/*---------------------------------------------------------------------
 *
 * dbase.h - data base for skel program.
 *           Uses rings instead of arrays for everything to allow easy
 *           insertion and deletion of objects.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Feb85
 *---------------------------------------------------------------------*/


/***************************************************
 *                                                 *
 *            Constants.                           *
 *                                                 *
 ***************************************************/

#define BEFORE             1
#define AFTER              0
#define OFFSET_ELEC	-1024	/* Offset to distinguish BC's from neighbors */

/***************************************************
 *                                                 *
 *            Types.                               *
 *                                                 *
 ***************************************************/

typedef 
struct  Snode 	{ 
		double x,y;		/* Coordinates */
		double h;		/* Local spacing */
		int link;		/* Number of edges linked to node */
		int iocode;		/* External name */
		struct LLedge *ptmp;	/* For temporary lists */
		struct Snode *next;	
		struct Snode *prev;	/* Doubly linked list. */
		}
	node;

typedef
struct  Sedge 	{ 
		int elec; 		/* Electrode number of this edge. */
		node *n[2];		/* Nodes of this edge */
		int link;		/* Number of region links */
		int iocode;		/* External name */
		struct Sedge *next;
		struct Sedge *prev;	/* Doubly linked list. */
		}
	edge;

typedef
struct  LLedge 	{ 
		edge   *edge;		/* Edge ;-) */
		struct LLedge *next;	/* Next edge in the linked list */
		struct LLedge *prev;	/* Last " */
		}
	lledge;

typedef
struct  Sreg  	{ 
		int mat;		/* Material index of this region. */
		int len;		/* # of edges (convenient redundancy) */
		int iocode;		/* External name */
		lledge *bnd; 		/* Pointer to linked list for region. */
		struct Sreg *next;
		struct Sreg *prev;	/* Doubly linked list */
		}
	region;

typedef
struct  Stri  	{ 
		region *r;		/* Region it belongs to */
		node *n[3];		/* Nodes of this triangle. */
		int   e[3];		/* Neighbors/boundary codes*/
		int iocode;		/* External name */
		struct Stri *next;
		struct Stri *prev;	/* Doubly linked list. */
		}
	triangle;


/***************************************************
 *                                                 *
 *            Macros.                              *
 *                                                 *
 ***************************************************/
#define WALK(START, F, B) for (B=0, F=START;  F != B;  F=F->next, B=START)
#define ined(N,E) ((N)==(E)->n[0] || (N)==(E)->n[1])
#define inedge(N,E) (N==E->edge->n[0] || N==E->edge->n[1])
#define inhibit() (root.tri != 0)


/***************************************************
 *                                                 *
 *            Variables.                           *
 *                                                 *
 ***************************************************/

#ifdef STATIC_ALLOCATION_TIME
#define EXT 
#else
#define EXT extern
#endif

EXT struct 	{
		node *node;
		edge *edge;
		triangle *tri;
		region *reg;
		} 
    root;


EXT int debug1, debug2;
EXT int check;			/* checking on */

/***************************************************
 *                                                 *
 *            Functions.                           *
 *                                                 *
 ***************************************************/
char *cr_node(), *cr_edge(), *cr_reg(), *cr_tri(), *ds_node(), *ds_edge(),
     *ds_reg(), *link_edge(), *unlink_edge(), *join_reg(), *split_reg();
int shared(), iscc();
lledge *e_in_r ();
