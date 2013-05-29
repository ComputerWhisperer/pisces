/*----------------------------------------------------------------------
 *
 * udb 	- second layer of data base routines to simplify user interface.
 *        The following assertions are maintained:
 *	- no free edges.
 *	- in each region, each linked edge shares an edge with the next
 *	  and previous linked edge, so the region as a whole is closed.
 *	- as a consequence, each node is linked 0 or >=2 times.
 *	- (someday: no duplicate nodes in a region)
 *	- (someday: > 2 nodes in any region)
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Apr 1985
 *---------------------------------------------------------------------*/
#include "general.h"
#include "dbase.h"
#include "udb.h"
#include <stdio.h>

/*-----------------ISCC-------------------------------------------------
 * Is an edge counterclockwise in a region?
 *----------------------------------------------------------------------*/
int iscc (r, e)
	region *r;
	lledge *e;
{
    if (!r || !e || r->len < 2) panic ("bad call");

    if (r->len == 2) return(1);

    if (inedge (e->edge->n[0], e->prev) &&
	inedge (e->edge->n[1], e->next)) return(TRUE);
    else 
    if (inedge (e->edge->n[1], e->prev) &&
       inedge (e->edge->n[0], e->next)) return(FALSE);
    else 
        panic ("broken region");
    return(MAYBE); /* for lint */
}

/*-----------------EFOLL------------------------------------------------
 * Find the edge of a region which follows a given node.
 * (the edge which has a least counter-clockwise node == n)
 * This depends on closed regions.
 *----------------------------------------------------------------------*/
lledge *efoll (r, n)
    region *r;
    node *n;
{
    lledge *l, *ldum;
    int cc;

    WALK (r->bnd, l, ldum) {
	cc = iscc (r, l);
	if (l->edge->n[cc?0:1] == n) return(l);
    }
    return(0);
}

/*-----------------PCR_EDGE---------------------------------------------
 * Possibly Create Edge.
 * Create an edge, if it is not already there.
 *----------------------------------------------------------------------*/
char *pcr_edge (i, j, en)
    node *i, *j;
    edge **en;
{
    edge *e, *edum;
    if (!i || !j || !en) panic ("bad parameters to pcr_edge");
    if (i==j) return ("ends of an edge must be different");

    WALK (root.edge, e, edum)
	if ((e->n[0]==i) + (e->n[1]==i) + (e->n[0]==j) + (e->n[1]==j) > 1)
	    {*en = e; return(0);}
    
    /*/ If we fall out of the loop, it wasn't there. /*/
    return (cr_edge (i, j, en));
}

/*-----------------TDS_REG----------------------------------------------
 * Totally Destroy Region.
 * Unlinks, then zaps the edges of a region.
 *----------------------------------------------------------------------*/
char * tds_reg (r)
	region *r;
{
    lledge *l;
    edge *esave;
    char *err;

    /*...Just keep unlinking the root edge. */

    for (l=r->bnd; l != 0; l=r->bnd) {
	esave = l->edge;
	if (err = unlink_edge (r, l)) panic(err);
	if (esave->link <= 0) {
	    draw_edge (esave, 0);
	    if (err = ds_edge (esave)) panic(err);
	}
    }
    if (err = ds_reg (r)) panic (err);
    return(0);
}


/*-----------------SPLIT_EDGE-------------------------------------------
 * Add a new node to an edge.
 *----------------------------------------------------------------------*/
char *split_edge (e, n, eb, ef)
    edge *e, **eb, **ef;
    node *n;
{
    char *err;
    region *r, *rdum, *rsave[2];
    lledge *l,        *lsave[2];
    int cc, ir, nr;
    if (!e || !n) panic("null pointer");

    if (err = pcr_edge (e->n[0], n,       eb)) return(err);
    if (err = pcr_edge (n,       e->n[1], ef)) { ds_edge (*eb); return(err); }
    (*eb)->elec = (*ef)->elec = e->elec;

    /*
     * Update regions pointing at e. The hard part is the link order.
     * Do a dry run to to check first.
     * Panic on err because the loop may already have hit another region.
     */
    nr = 0;
    WALK (root.reg, r, rdum) 
	if (l = e_in_r (r, e)) {
	    rsave[nr] = r; lsave[nr] = l; nr++;
	    if (efoll (r, n)) {
		ds_edge (*ef); ds_edge (*eb); 
		return ("That would link a node twice into one region");
	    }
	}
	
    for (ir = 0; ir < nr; ir++) {
	r = rsave[ir];	l = lsave[ir];
	cc = iscc (r, l);
	if (err = link_edge (r, *eb, l, cc ? BEFORE : AFTER)) panic (err);
	if (err = link_edge (r, *ef, l, cc ? BEFORE : AFTER)) panic (err);
	if (err = unlink_edge (r, l)) panic (err);
    }

    /*...Having removed all links to it, destroy e. */
    draw_edge (e, 0);
    draw_edge (*eb, -1);
    draw_edge (*ef, -1);
    if (err = ds_edge (e)) panic (err);	

    return(0);
}
    
		
	    
/*-----------------JOIN_EDGE--------------------------------------------
 * Join two consecutive edges, with appropriate region modifications.
 *----------------------------------------------------------------------*/
char *join_edge (ej, enew)
    edge *ej[2], **enew;
{
    int i, ir;
    char *err;
    region *r, *rdum, *revisit[2];
    lledge *l[2];

    i = shared (ej[0], ej[1]);
    if (i < 16 || i > 32) 
	return ("cannot merge edges which are disjoint or the same");

    /*...Create new edge with unshared nodes */
    if (err = pcr_edge (ej[0]->n[!(i&1)], ej[1]->n[!(i&2)], enew))
	return (err);
    (*enew)->elec = (ej[0]->elec == ej[1]->elec)? ej[0]->elec : 0;
    
    /*
     * Link new edge and unlink old ones to/from any region involved.
     * Panic on err because the loop may already have hit another region.
     * Add a little check to avoid degenerate regions.
     */
    ir = 0;
    WALK (root.reg, r, rdum) 
	if ((l[0] = e_in_r (r, ej[0])) && (l[1] = e_in_r (r, ej[1]))) {
	    if (err = link_edge (r, *enew, l[0], BEFORE)) panic(err);
	    if ((err = unlink_edge (r, l[0])) ||
		(err = unlink_edge (r, l[1]))) panic (err);
	    if (r->len < 3) {
		uerr("That caused a degenerate region which has been removed");
		revisit[ir++] = r;
		}
	}
    
    /*...See if the edges need to be removed. */
    if (ej[0]->link <= 0) 
	{ draw_edge (ej[0], 0); if (err = ds_edge (ej[0])) panic (err); }
    if (ej[1]->link <= 0) 
	{ draw_edge (ej[1], 0); if (err = ds_edge (ej[1])) panic (err); }
    draw_edge (*enew, -1);

    /*...and fix any dead regions. do it last to avoid complications */
    for (i=0; i < ir; i++)
	if (err = tds_reg (revisit[i])) panic (err);

    return(0);
}


/*-----------------PANIC------------------------------------------------
 * This is what comes of trying to be user friendly.
 *----------------------------------------------------------------------*/
panic (s)
    char *s;
{
    char ebuf[80], *err, *wumesh();

    uerr ("Saving mesh...");
    if (err = wumesh ("panic.save"))
        {sprintf (ebuf, "Error in saving: %s", err); uerr (ebuf);}
    sprintf (ebuf, "Congratulations, you have found a %s bug.", s);
    uerr (ebuf);
    uerr ("Please contact the person in charge of maintaining this program");
    allover();
}

