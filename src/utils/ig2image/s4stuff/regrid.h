/*
 * regrid.h 3.1   9/14/87 10:41:48
 * Storage for grid refinement.
 * During tree-structured regrid, a new copy of the whole 
 * point-node-tritree structure
 * is built, and when finished the old one is free'd and global arrays
 * pointed at the new one.
 *
 * Also has stuff for moving grid update
 */
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

EXTERN pt_typ *o_pt[MAXPNT];	EXTERN int o_np;
EXTERN nd_typ *o_nd[MAXPNT];	EXTERN int o_nn;
EXTERN tri_typ *o_tri[MAXTRI];	EXTERN int o_ne;

#if 0
EXTERN int lregio[MAXMAT], 	/* Code for each region TRUE=>refine */
	   logar, 		/* Look at log(variable)? */
	   labs, 		/* Look at abs(variable)? */
	   fmode, 		/* Variable code */
	   llimit, 		/* Maximum level */
	   ioff;		/* Mod-4 offset of root grid. */
#endif

EXTERN double 
	   fdel,		/* Step size */
	   rgbox[4];		/* Box to limit refinement */
	      
EXTERN float
           zmark[MAXTRI];       /* Z marks the spot to refine */


/* Describe a polygon by a linked list */
struct polyel {
    int nd;		/* Node number */
    int nb;		/* Neighbor element on clockwise side of that node */
    int nx;		/* Which neighbor of that element needs to be fixed */
    struct polyel *prev, *next;
};

#define KILL_ME     (-32767)	/* Unlikely value for triangle region or
				   node material or point flag */
#define dead_tri(N) (tri[N]->regnum == KILL_ME)
#define fing_tri(N) (tri[N]->regnum =  KILL_ME)
#define dead_nd(N)  (nd [N]->mater  == KILL_ME)
#define fing_nd(N)  (nd [N]->mater  =  KILL_ME)
#define dead_pt(N)  (pt [N]->flags  == KILL_ME)
#define fing_pt(N)  (pt [N]->flags  =  KILL_ME)

#define twhich(T,N) ((tri[T]->nb[0] == (N) ? 0 : \
		     (tri[T]->nb[1] == (N) ? 1 : \
		     (tri[T]->nb[2] == (N) ? 2 : NO_TRIANGLE))))
#define nwhich(T,N) ((tri[T]->nd[0] == (N) ? 0 : \
		     (tri[T]->nd[1] == (N) ? 1 : \
		     (tri[T]->nd[2] == (N) ? 2 : NO_TRIANGLE))))

