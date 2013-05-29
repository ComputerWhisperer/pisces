/*************************************************************************
 *									 *
 *   Original : MEL         Stanford University        Sept, 1984	 *
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   geom.h                Version 3.1     */
/*   Last Modification : 9/14/87  10:41:38 */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/************************************************************************
 *									*
 *	This file contains the structure definitions of the three 	*
 *  fundamental units used to represent data in SUPREM IV.  These are	*
 *  points, nodes, and triangles.  Definitions:				*
 *									*
 *  Point:  A physical location in the mesh.  Each grid point 		*
 *	represents a single location at which the partial differential	*
 *	equations are solved.  Each physical point will have one or 	*
 *	more nodes associated with it, which represent the data at that	*
 *	point in each of the materials which may meet at it.		*
 *									*
 *  Nodes:  Each node represents the solution in a particular material	*
 *	at a physical point.  A given node may represent the Silicon	*
 *	values at a point with coordintes (0,0), an entirely different	*
 *	node will represent the Silicon Dioxide values at point (0,0).	*
 *									*
 *  Triangles:  Each triangle is in a single material and has a set of	*
 *	three physical points associated with its vertices.		*
 *									*
 ************************************************************************/

/*special bits for point identification*/
#define SURFACE 0x0001

/*the code used by triangle to indicate a neumann edge*/
#define BC_OFFSET -1024

/*the code to represent non-existant sons or fathers in the triangle tree*/
#define NO_TRIANGLE -32767

/*define a structure of the physical point information*/
struct pt_str {
    float cord[2];		/*the coordinate value of the point*/
    float cordo[2];		/*the initial coordinates of the point*/
    int flags;			/*set of bits to indicate special conditions*/
    int nn;			/*the number of nodes at this point*/
    int nd[MAXMAT];		/*indices into the node structure*/
};
typedef struct pt_str pt_typ;

/*define a structure for the logical nodes*/
struct nd_str {
    double sol[MAXIMP];		/*the solution values for each stream*/
    int mater;			/*the material index for this node*/
    double step;		/*the step the node was added at*/
    double time;		/*the time of the step it was added*/
    int ne;			/*number of triangles which contain the point*/
    int *tri;			/*the triangles that contain the point*/
    int pt;			/*the index of the physical point*/
};
typedef struct nd_str nd_typ;

/*define the structure to contain the element information*/
struct tri_str {
    int nd[3];		/*the index to the nodes in the triangle*/
    int nb[3];		/*the neighbor triangles*/
    int nd0[3];		/*the nodes before flipping (grrr)*/
    int nb0[3];		/*the neighbors before flipping*/
    float ehed[3];	/*the coupling coefficients*/
    float d[3];		/*the length of the sides - boundary conditions*/
    float earea[3];	/*the area of the triangle for each node*/
    float sig[3];	/*the per-triangle stresses*/
    int regnum;		/*the region number of the triangle*/
    int fath;		/*the father of this triangle*/
    int son;		/*the first son of this triangle*/
    int level;		/*the height in the tree (redundant but fun)*/
};
typedef struct tri_str tri_typ;

#define exists(X) ((X) != NO_TRIANGLE)
#define root(X) ((X)->fath == NO_TRIANGLE)
#define green(X) ((X)->fath < 0 && (X)->fath != NO_TRIANGLE)
#define reglr(X) (!(green(X)))
#define leaf(X) ((X)->son == NO_TRIANGLE)
#define EC(X) if (err = X) return(err);

EXTERN struct pt_str  *pt[MAXPNT];
EXTERN struct nd_str *nd[MAXPNT];
EXTERN struct tri_str *tri[MAXTRI];

extern float dist();
extern float area_tri();

/*should be in the geom stuff*/
EXTERN int ne;		/*number of elements*/
EXTERN int np;		/*number of points*/
EXTERN int nn;		/*the number of nodes*/

/*the step number*/
EXTERN int process_step;
