/*------------------------------------------------------------------------
 *
 * u2.c - Slightly higher level user commands for working with regions.
 *	  The intent is that the user can avoid knowing about edges
 *	  at all.
 *
 *     Copyright c 1985 The board of trustees of the Leland Stanford
 *                      Junior University. All rights reserved.
 *     This subroutine may not be used outside of the IGGI-2 computer
 *     program without the prior written consent of Stanford University.
 *
 * Original : CSR Apr 1985
 *
 *-----------------------------------------------------------------------*/

#include "general.h"
#include "dbase.h"
#include "udb.h"		/* level-2 data base */
#include "skelp.h" 		/* Window sizes */
#include "griph.h"		/* Pick functions */
#include "alpha.h"		/* Reading stuff from the top line */


/*-----------------UCR2_REG---------------------------------------------
 * Create a region by nodes. 
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ucr_reg (void)
#else

char *ucr_reg()
#endif
{
    node *n0, *n1, *n2, *nn;		 /* First 3 nodes, generic new node. */
    region *r;				 /* Region created. */
    edge *e0, *e1, *e2, *esp, *en0, *en1;/* First 3 edges, split and new edges*/
    char *err;

    if (err = cr_reg (&r)) return(err);

    /*
     * Get the first three nodes, make a three-edge region,
     * then go split the third edge. 
     */
    if (!(n0 = npick("Node")) || !(n1 = npick("Node"))) 
	{ tds_reg (r); return("Aborted");}
    if ((err = pcr_edge (n0, n1, &e0)) || 
	(err = link_edge (r, e0, r->bnd, BEFORE)))
	{ tds_reg (r); return (err);}
    draw_edge (e0, -1);
    if (!(n2 = npick("Node")))
	{ tds_reg (r); return("Aborted");}
    if ((err = pcr_edge (n1, n2, &e1)) || 
	(err = link_edge (r, e1, r->bnd, BEFORE)) ||
	(err = pcr_edge (n2, n0, &e2)) ||
	(err = link_edge (r, e2, r->bnd, BEFORE)))
	{tds_reg (r); return(err);}
    draw_edge (e1, -1);
    draw_edge (e2, -1);
    
    esp = e2;
    while (nn = npick("Node")) {
	if (err = split_edge (esp, nn, &en0, &en1)) 
	    {uerr(err); break;}
	esp = ined (n0, en1) ? en1 : en0;	/* whichever has n0 */
	}

    return(0);
}



/*-----------------USP_EDGE---------------------------------------------
 * Add new nodes to an edge.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
usp_edge (void)
#else

char *usp_edge()
#endif
{
    edge *ework, *ef, *eb;	/* Edge before and after being broken. */
    node *stg, *fng, *n;	/* Starting, finishing, and working node */
    char *err;

    /*
     * Get the edge, and the direction of adding. 
     */
    if (!(ework = epick("Edge to split"))) return("Aborted");

    if (!(stg = npick("Starting at which node"))) return("Aborted");

    if      (stg == ework->n[0]) fng = ework->n[1];
    else if (stg == ework->n[1]) fng = ework->n[0];
    else return ("Node is not on the edge being split");
    
    /*  
     * Now collect nodes, assumed to be already created.
     * Eventually allow creating nodes on the fly. 
     */
    while (n = npick ("New node")) {
	if (err = split_edge (ework, n, &ef, &eb))
	    return (err);
	ework = (ined (fng, ef)) ? ef : eb;	/* Whichever has fng */
    }

    return(0);
}
/*-----------------UJOIN_EDGE-------------------------------------------
 * Join two edges, freeing the node between them.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ujoin_edge (void)
#else

char *ujoin_edge()
#endif
{
    edge *ej[2], *enew;
    char *err;

    if (!(ej[0] = epick("1st edge")))
	return ("Aborted");

    while (ej[1] = epick ("Edge")) {
	if (err = join_edge (ej, &enew)) 
	    return(err);
	ej[0] = enew;
    }

    return(0);
}

/*-----------------USP_REG----------------------------------------------
 * Nodewise region split routine.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
usp_reg (void)
#else

char *usp_reg ()
#endif
{
    node *head, *tail;
    edge *enew;
    lledge *etail, *ehead;
    region *r, *R;
    char *err;

    if (!(r = rpick("Region"))) 
	return("Aborted");

    if (!(tail = npick("Tail"))) 
	return("Aborted");
    if (!(etail = efoll (r, tail)))
	return("That node is not in the region specified");
    if (!(head = npick("Head")))
	return("Aborted");
    if (!(ehead = efoll (r, head)))
	return("That node is not in the region specified");

    if (err = pcr_edge (head, tail, &enew))
	return(err);

    if (err = split_reg (r, ehead, etail, &R)) {
	ds_edge (enew);
	return(err);
	}

    if ((err = link_edge (R, enew, R->bnd, BEFORE)) ||
	(err = link_edge (r, enew, r->bnd, BEFORE)))
	panic("data base");

    draw_edge (enew, -1);
    
    return(0);
}

/*-----------------UJOIN_REG--------------------------------------------
 * Join two regions nodewise.
 * This version insists that they share just one common edge.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
ujoin_reg (void)
#else

char *ujoin_reg ()
#endif
{
    lledge *l, *L, *ldum, *lshare, *Lshare, *lnext, *Lnext;
    region *r, *R;
    edge *eshare;
    char *err;
    int found;

    if (!(r = rpick("1st region")) || !(R = rpick("2nd region")))
	return ("Aborted");
    if (r == R)
	return ("Cannot join a region to itself.");
    

    /*...Eliminate the common edge. */

    found = 0;
    WALK (r->bnd, l, ldum) 
	if (L = e_in_r (R, l->edge))
	    { lshare = l; Lshare = L; found++; }

    if (found == 0) return ("Those regions do not share an edge");
    if (found > 1) return ("You must condense the interface first");

    eshare = lshare->edge;	
    lnext = lshare->next;	
    Lnext = Lshare->next;
    if (err = unlink_edge (r, lshare)) return (err);
    if (err = unlink_edge (R, Lshare)) panic (err);
    draw_edge (eshare, 0);
    if (err = ds_edge (eshare)) panic (err);


    /*...Sew 'em up. */

    if (err = join_reg (r, R, lnext, Lnext))
	panic (err);	/* too late to recover. */

    return(0);
}

/*-----------------UDS_REG----------------------------------------------
 * Destroy a region.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

char *
uds_reg (void)
#else

char *uds_reg()
#endif
{
    region *r;

    if (!(r = rpick("Region"))) return ("Aborted");
    return (tds_reg (r));
}

