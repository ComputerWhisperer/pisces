/*----------------------------------------------------------------------
 *
 * check.c - The expensive checking routines which can be turned off.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Mar 84
 *---------------------------------------------------------------------*/

#include "general.h"
#include "dbase.h"
#include "skelp.h"	/* For the window bounds */


/*-----------------PROX-------------------------------------------------
 * Check a position for proximity to other things.
 *----------------------------------------------------------------------*/
char * prox (x, y)
    double x,y;
{
    node *n,*dum; 			/* Walkers */
    double tol,dis(),edis();		/* Error tolerance */

    if (!check) return (0);
    if (!root.node) return (0);
    if (!wmesh) return ("Mesh window is not yet defined!");

  /*...Get bounds from mesh window. 1e-6 is reasonable because 1um/1A = 1e4 */
    tol = 1e-6 * 
	  dmax (wmesh->wxmax - wmesh->wxmin, wmesh->wymax - wmesh->wymin);

  /*...Check against close nodes*/
    WALK (root.node, n, dum) 
	if (dis (n->x, n->y, x, y) < tol) 
	    return("Too close to another node");

  /*...Nothing happened.*/
    return(0);
}

/*-----------------BADEDGE----------------------------------------------
 * Check if an edge is already present (user must be retarded or blind or both)
 *----------------------------------------------------------------------*/
char *badedge (i, j)
    node *i,*j;
{
    edge *e, *edum;

    WALK (root.edge, e, edum) 
	/* Add logical values : K&R p.41 " relational expressions like i>j 
	 * ... are defined to have value 1 if true, and 0 if false."
	 */
	if ( (e->n[0] == i) + (e->n[1] == i) + (e->n[0] == j) + (e->n[1] == j)
	     > 1)
	    return ("That edge is already in the database");
    
    return(0);
}

/*-----------------L_IN_R-----------------------------------------------
 * Verification that a link is really part of a region.
 * This error checking is getting out of hand.
 *----------------------------------------------------------------------*/
int l_in_r (r, l)
    region *r;
    lledge *l;
{
    lledge *f, *b;

    if (!check || (!r->bnd && !l)) return(1);

    WALK (r->bnd, f, b)
	if (f==l) return(1);
    return(0);
}
