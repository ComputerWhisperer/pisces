static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/dbase.c,v 1.3 85/06/07 00:40:36 conor Exp $";
/*************************************************************************
 *                                                                       *
 *     dbase.c - data base routines for triangle generator and optimizer.*
 *               This version uses arrays since material is not being    *
 *               removed, just added.                                    *
 *               It is cavalier about errors - if they occur, the data   *
 *               base gets corrupted and the program must exit.          *
 *                                                                       *
 *     Copyright c 1985 The board of trustees of the Leland Stanford     *
 *                      Junior University. All rights reserved.          *
 *     This subroutine may not be used outside of the IGGI-2 computer    *
 *     program without the prior written consent of Stanford University. *
 *                                                                       *
 * Original : CSR Nov84                                                  *
 *                                                                       *
 *************************************************************************/

#include "general.h"
#include "dbase.h"
#include <stdio.h>
#include <stdlib.h>

/*-----------------CR_NODE----------------------------------------------
 * Add a new node. 
 * If number of node to be created is 0, puts it at the end.
 *----------------------------------------------------------------------*/
char * cr_node (in, x, y, h)
    int *in;		/* Index */
    double x,y,h;	/* Coordinates and local spacing. */

{
    static char ebuf[80];
    if (*in <= 0) *in = ++nnode; 	else nnode = imax(*in,nnode);

    if (nnode >= MAXNODE) return("Node table overflow");

    if (node[*in] != 0) 
        {sprintf(ebuf,"Node %d is already assigned",*in);return(ebuf);}
    node[*in] = (struct Snode *) malloc (sizeof (struct Snode));
    if (node[*in]==0) return("Out of memory");

    node[*in]->x = x;
    node[*in]->y = y;
    node[*in]->h = (h <= 0) ? -MAXFLOAT : h;
    node[*in]->elec = 0;

    return(0);
}

/*-----------------CR_EDGE----------------------------------------------
 *Create a new edge.
 *----------------------------------------------------------------------*/
char * cr_edge (ie, e, a, b)
    int *ie;	/* Edge index */
    int e;	/* Electrode code */
    int a,b;	/* Nodes belonging to it. */
{
    static char err[80];

    if (*ie <= 0) 
	*ie = ++nedge;
    else 
	nedge = imax(nedge,*ie);

    if (nedge >= MAXEDGE) 
        {sprintf(err,"Edge table overflow (%d)",nedge); return(err);}

    if (edge[*ie] != 0) 
        {sprintf(err,"Edge %d is already assigned",*ie); return(err);}
    edge[*ie] = (struct Sedge *) malloc (sizeof (struct Sedge));
    if (edge[*ie] == 0) return("Out of memory");
    if(a<1 || a>=MAXNODE || b<1 || b>=MAXNODE) return("Bad node index");
    if(a==b) return("Edge has the same node at both ends");

    edge[*ie]->elec = e;
    edge[*ie]->n[0] = a;
    edge[*ie]->n[1] = b;
    edge[*ie]->r[0] = edge[*ie]->r[1] = edge[*ie]->orig = 0;
    node[a]->elec = e;
    node[b]->elec = e;

    return(0);
}
/*-----------------CR_REG-----------------------------------------------
 *Create a new region.
 *----------------------------------------------------------------------*/
char * cr_reg (ir, mat)
    int *ir;	/* Region index */
    int mat;	/* Material number of region. */
{
    static char ebuf[80];
    if (*ir <= 0) 
	*ir = ++nreg;
    else 
	nreg = imax(nreg,*ir);
       
    if (nreg >= MAXREG) return("Region table overflow");

    if (reg[*ir] != 0) 
        {sprintf(ebuf,"Region %d is already assigned", *ir); return(ebuf);}
    reg[*ir] = (struct Sreg *) malloc (sizeof (struct Sreg));
    if (reg[*ir] == 0) return("Out of memory");

    reg[*ir]->len = 0;
    reg[*ir]->mat = mat;
    reg[*ir]->maxa = reg[*ir]->mina = reg[*ir]->bnd = 0;

    return(0);
}

/*-----------------CR_TRI-----------------------------------------------
 * Create a new triangle.
 *----------------------------------------------------------------------*/
char * cr_tri (it, rn, i, j, k, e0, e1, e2)
    int *it;		/* Triangle index */
    int rn;		/* Region it belongs to. */
    int i,j,k;		/* Nodes of triangle. */
    int e0, e1, e2;	/* Neighbors/BC's of triangle */
{
    static char ebuf[80];
    if (*it <= 0)
	*it = ++ntri;
    else
	ntri = imax(*it,ntri);

    if (ntri >= MAXTRI) return("Triangle table overflow");

    if (tri[*it] != 0) 
        {sprintf(ebuf,"Triangle %d is already assigned",*it);return(ebuf);}
    tri[*it] = (struct Stri *) malloc (sizeof (struct Stri));
    if (tri[*it] == 0) return("Out of memory");

    tri[*it]->reg = rn;
    tri[*it]->n[0] = i;
    tri[*it]->n[1] = j;
    tri[*it]->n[2] = k;
    tri[*it]->e[0] = e0;
    tri[*it]->e[1] = e1;
    tri[*it]->e[2] = e2;

    return(0);
}

/*-----------------AD_EDGE----------------------------------------------
 * Add edge to region around edge lep.
 *----------------------------------------------------------------------*/
char * ad_edge (ir, ie, lep, iscc, pos)
   int ir;		/* Region to add to. */
   int ie;		/* Edge to add. */
   int iscc; 		/* Does  this edge conform with region order? */
   struct LLedge *lep;	/* Edge to enter around */
   int pos;		/* Whether before or after lep. */
{
   struct LLedge *new,*first,*last;

  /*...Check against bad call. */
   if (ir < 1 || ir >= MAXREG) return ("Bad region index to ad_edge");
   if (ie < 1 | ie >= MAXEDGE) return ("Bad edge index to ad_edge");
   if (!reg[ir] || !edge[ie]) return("Bad args to ad_edge");
   if (reg[ir]->bnd && !lep) return("No position given to ad_edge");

  /*...Create new link */
   new = (struct LLedge *) malloc (sizeof(struct LLedge)); 
   					if (!new) return("Out of memory");
   new->edge = ie;
   new->iscc = iscc;
   new->ang  = -999;
   new->gt = new->lt = 0;


   /*...Include in linked list. */
    if (reg[ir]->bnd == 0) {     	/* Initialize a region. */
       reg[ir]->bnd = new->next = new->prev = new;
       } 
    else {				/* Insert into existing region */
	first = (pos==BEFORE)? lep : lep->next;
	last =  first->prev;

	last->next = new;	new->prev  = last;
	new->next  = first;	first->prev = new;
       }
    reg[ir]->len++;
    
    /* Tell edge itself that it is being pointed at from this region. */
    if (edge[ie]->r[0] == 0) 
	edge[ie]->r[0] = ir;
    else if (edge[ie]->r[0] == ir)
	return("Duplicate edge in region");
    else if (edge[ie]->r[1] == 0)
	edge[ie]->r[1] = ir;
    else if (edge[ie]->r[1] == ir)
	return("Duplicate edge in region");
    else 
	return("Edge in >2 regions");

    return(0);
}

/*-----------------EINDEX-----------------------------------------------
 * Find pointer from a region to an edge - simple linear search.
 * Returns 0    (bad region or couldn't find it)
 *         pointer (usually)
 *----------------------------------------------------------------------*/
struct LLedge * eindex (ir, ie)
    int ir;	/* Region to search */
    int ie;	/* Edge to find. */
{
    struct LLedge *bp;

    if (ir < 1 || ir > nreg || (bp=reg[ir]->bnd)==0) return(0);

    do {
	if (bp->edge==ie) return(bp);
	bp = bp->next;
	}
    while (bp != reg[ir]->bnd);

    return(0);	/* No luck if we got here. */
}


/*-----------------ADD_ANG----------------------------------------------
 * Add new value to sorted list of region angles.
 * If the new angle is smaller than all previous, no search is done.
 *----------------------------------------------------------------------*/
char * add_ang (r, lep)
    struct Sreg *r;	/* Region to update. */
    struct LLedge *lep; /* New edge to add. */
{
    struct LLedge *mina,*ep,*less,*greater;
    int f;

    if (r->maxa == 0) {	/* New list ? */
	r->maxa = lep;
	lep->gt = lep->lt = lep;
	}

    else {			/* Old list. */
      /*...Walk around the region in order of increasing angle. */
	mina=r->maxa->gt;
	for (f = 1 , ep = mina; (ep != mina) || f; ep = ep->gt , f = 0) 
	    if (ep->ang >= lep->ang) break;

      /*...Link in new value. */
	greater = ep;		
	less = ep->lt;

	lep->gt = greater;
	lep->lt = less;
	less->gt = lep;
	greater->lt = lep;

      /*...If we got a new maximum, update maxa. */
	if (lep->ang > r->maxa->ang) 
	    r->maxa = lep;

	}
    return(0);
}
	    
/*-----------------SET_ELEC---------------------------------------------
 * Set the elec field for each node.
 *----------------------------------------------------------------------*/
void set_elec()
{
    int in, ie; struct Sedge *ep;

    /* Each node starts off on no electrode */
    FOR (in, 1, nnode) node[in]->elec = NO_ELEC;

    /* Each edge passes its code on to its nodes, unless the node
     * already has a higher code defined. (Actually only the 
     * Pisces file cares)
     */
    FOR (ie, 1, nedge) {
	ep = edge[ie];
	if (ep->elec > node[ep->n[0]]->elec)
	    node[ep->n[0]]->elec = ep->elec;
	if (ep->elec > node[ep->n[1]]->elec)
	    node[ep->n[1]]->elec = ep->elec;
	}
}
