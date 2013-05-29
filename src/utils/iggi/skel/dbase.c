/*----------------------------------------------------------------------
 *
 * dbase.c - data base routines for skel
 *           Creation/deletion of nodes/edges/regions/triangles
 *           Addition/removal/finding edges
 *           These routines are as bulletproof as possible. 
 *	     On error, each call returns without making any changes.
 *	     Integrity of the linked lists is guaranteed. 
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the IGGI2 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Feb 85
 *---------------------------------------------------------------------*/
#include "general.h"
#include "dbase.h"
#include <stdlib.h>

/*-----------------CR_NODE----------------------------------------------
 * Create a node.
 *----------------------------------------------------------------------*/
char * cr_node(rv,x,y)
    node **rv;
    double x,y;
{
    node *new, *behind;  char *err, *prox();

    if (inhibit()) return ("No data base operations with triangles!");
    if (check && (err=prox(x,y))) return(err);

/*...Create it */
    new = (node *) malloc (sizeof (node));  
    if (!new) return("out of memory in cr_node");

    new->x = x; new->y = y;
    new->h = 0;
    new->link = 0;
    new->iocode = 0;

/*...Install it.*/
    if (root.node == 0)	/* Initialize node list */
	root.node = new->next = new->prev = new;
    else {			/* Add it to the end. */
	behind = root.node->prev;
	new->prev = behind;	behind->next = new;
	root.node->prev = new;	new->next = root.node;
	}

    *rv = new;
    return (0);
}


/*-----------------CR_EDGE----------------------------------------------
 * Create an edge
 *----------------------------------------------------------------------*/
char * cr_edge (i, j, rv)
    node *i, *j;
    edge **rv;
{
    edge *new, *behind; char *err, *badedge();

    if (inhibit()) return ("No data base operations with triangles!");
    if (i==0 || j==0) return ("bad data base call");
    if (i==j) return ("the ends of an edge must be distinct");
    if (check && (err = badedge (i, j))) return(err);

/*...Create it */
    new = (edge *) malloc (sizeof (edge));  
    if (!new) return("out of memory in cr_edge");

    new->n[0] = i; i->link++;
    new->n[1] = j; j->link++;
    new->link = 0; 
    new->elec =  0;
    new->iocode = 0;

/*...Install it */
    if (root.edge == 0)	/* Initialize edge list */
	root.edge = new->next = new->prev = new;
    else {			/* Add it to the end. */
	behind = root.edge->prev;
	new->prev = behind;	behind->next = new;
	root.edge->prev = new;	new->next = root.edge;
	}

    *rv = new;
    return (0);
}


/*-----------------CR_REG-----------------------------------------------
 * Create a (n empty) region.
 *----------------------------------------------------------------------*/
char * cr_reg (rv)
    region **rv;
{
    region *new, *behind;

    if (inhibit()) return ("No data base operations with triangles!");
    if (!rv) return ("bad data base call");
    /*...Create it */
    new = (region *) malloc (sizeof (region));    
    if (!new) return("out of memory in cr_reg");

    new->mat = new->iocode = new->len = 0;
    new->bnd = 0;

    /*...and install it */
    if (root.reg == 0)	/* Initialize reg list */
	root.reg = new->next = new->prev = new;
    else {			/* Add it to the end. */
	behind = root.reg->prev;
	new->prev = behind;	behind->next = new;
	root.reg->prev = new;	new->next = root.reg;
	}

    *rv = new;
    return (0);
}


/*-----------------CR_TRI-----------------------------------------------
 * Create a triangle.
 *----------------------------------------------------------------------*/
char * cr_tri (rv, n1, n2, n3, r, e1, e2, e3)
    triangle **rv;
    node *n1, *n2, *n3;
    region *r;
    int e1, e2, e3;
{
    triangle *new, *behind;

    if (!rv || !n1 || !n2 || !n3 || !r) return ("bad data base call");
    if (n1==n2 || n2==n3 || n3==n1) 
	return ("the nodes of a triangle must all be distinct");

  /*...Create it */
    new = (triangle *) malloc (sizeof (triangle));    
    if (!new) return("out of memory in cr_tri");

    new->n[0] = n1; new->n[1] = n2; new->n[2] = n3;
    new->r = r;
    new->iocode = 0;
    new->e[0] = e1; new->e[1] = e2; new->e[2] = e3;

  /*...Install it */
    if (root.tri == 0)	/* Initialize tri list */
	root.tri = new->next = new->prev = new;
    else {			/* Add it to the end. */
	behind = root.tri->prev;
	new->prev = behind;	behind->next = new;
	root.tri->prev = new;	new->next = root.tri;
	}

    *rv = new;
    return (0);
}

/*-----------------DS_NODE----------------------------------------------
 * Destroy node. Nodes on an edge must be freed first.
 * After destruction, n points into deep hyperspace.
 *----------------------------------------------------------------------*/
char *ds_node (n)
	node *n;
{
    if (inhibit()) return ("No data base operations with triangles!");
	if (!n) return ("bad data base call");
	if (n->link) return ("a linked node may not be removed");

	if (root.node == n) root.node = (n->next == n)? 0 : n->next;
	n->next->prev = n->prev;
	n->prev->next = n->next;
	free (n);
	return(0);
}


/*-----------------DS_EDGE----------------------------------------------
 * Destroy an edge. Edges inside regions must be unlinked first.
 *----------------------------------------------------------------------*/
char *ds_edge (e)
	edge *e;
{
	if (inhibit()) return ("No data base operations with triangles!");
	if (!e) return ("bad data base call");
	if (e->link) return ("a linked edge may not be removed");

	e->n[0]->link--;
	e->n[1]->link--;
	if (root.edge == e) root.edge = (e->next == e)? 0 : e->next;
	e->next->prev = e->prev;
	e->prev->next = e->next;
	free(e);
	return(0);
}

/*-----------------DS_REG-----------------------------------------------
 * Destroy a region.
 *----------------------------------------------------------------------*/
char *ds_reg (r)
	region *r;
{
	lledge *f, *b, *te;
	if (inhibit()) return ("No data base operations with triangles!");

	if (!r) return ("bad data base call");

    /*...Dispose of the boundary */
	for (b=0, f=r->bnd;  f != b; b=r->bnd)	
	{
	    f->edge->link--;
	    te = f->next;
	    free (f);
	    f = te;
	}
	    
    /*...Dispose of the region */
	if (root.reg == r) root.reg = (r->next == r)? 0 : r->next;
	r->next->prev = r->prev;
	r->prev->next = r->next;
	free(r);
	return(0);
}


/*-----------------E_IN_R-----------------------------------------------
 * Find which link of a region has the edge.
 *----------------------------------------------------------------------*/
lledge *e_in_r (r, e)
    region *r;
    edge *e;
{
    lledge *f, *b;

    if (!r || !e) return (0);	/* Really should make a stronger protest */
    WALK (r->bnd, f, b)
	if (f->edge == e) return(f);
    return(0);
}

/*-----------------LINK_EDGE--------------------------------------------
 * Link an edge into a region. 
 * We check only against the most obvious errors.
 *----------------------------------------------------------------------*/
char *link_edge (r, e, lep, pos)
    region *r;			/* Region to add to. */
    edge *e;			/* Edge to add. */
    lledge *lep;		/* Edge to enter around */
    int pos;			/* Whether before or after lep. */
{
    lledge *new,*ahead,*behind;
    if (inhibit()) return ("No data base operations with triangles!");

    if (!r || !e || (r->bnd && !lep)) return ("bad data base call");
    if (e->link >= 2) return ("Edge already belongs to two regions");
    if (e_in_r (r, e)) return ("Edge already belongs to this region!");
    if (!l_in_r (r, lep)) return ("Bad data base call");

  /*...Create new link */
    if (!(new = (lledge *) malloc (sizeof(lledge))))
        return("Out of memory");
    new->edge = e;
 
  /*...Include in linked list. */
     if (r->bnd == 0) {     	
        r->bnd = new->next = new->prev = new;
        } 
     else {
 	ahead = (pos == BEFORE) ? lep : lep->next;
 	behind =  ahead->prev;
 
 	behind->next = new; 	new->prev  = behind;
 	new->next  = ahead; 	ahead->prev = new;
        }
    r->len++;

  /*...Tell the edge that it is being pointed at. */ 
    e->link++;
    return(0); 
}

/*-----------------UNLINK_EDGE------------------------------------------
 *----------------------------------------------------------------------*/
char *unlink_edge (r, e)
	region *r;
	lledge *e;	/* already know the location of the edge */
{
	if (inhibit()) return ("No data base operations with triangles!");
	if (!r || !e ) return ("bad data base call");
	if (!l_in_r (r, e)) return ("bad data base call");
	
	e->edge->link--;	/* one less link to this edge */
	if (r->bnd == e) r->bnd = (e->next == e)? 0 : e->next;
	e->prev->next = e->next;	/* fix forward and reverse pointers */
	e->next->prev = e->prev;

	free (e);
	r->len--;
	return (0);
}

/*-----------------SHARED-----------------------------------------------
 * do edges a and b share a common node?
 * high order bits store the number of hits, low order the positions.
 *----------------------------------------------------------------------*/
int shared (a, b)
    edge *a, *b;
{
    return (
	(a->n[0] == b->n[0]) *16 +
	(a->n[1] == b->n[0]) *17 + 
	(a->n[0] == b->n[1]) *18 +
	(a->n[1] == b->n[1]) *19   
	   );
}

/*-----------------SPLIT_REG--------------------------------------------
 * Split a region in two. Just tweak the pointers around the edges.
 * Picture for this and the following routine:
 *          Re
 *         ----
 *       rs|\ |Rs
 *         | \|
 *         ----
 *          re
 *----------------------------------------------------------------------*/
char *split_reg (r, rs, Rs, nr)
    region *r;		/* Region to break */
    lledge *rs, *Rs;	/* Pointers to start of new regions */
    region **nr;	/* Pointer to the new region created. */
{
    lledge *re, *Re;	/* Ends of new regions */
    region *R;		/* New region */
    lledge *l,*f;	/* For walking around boundaries. */
    char *err;

    if (inhibit()) return ("No data base operations with triangles!");
    if (!r || !rs || !Rs || !nr) return ("bad data base call");
    if (rs == Rs) return("Start, end of split are identical");
    if (!l_in_r (r, rs) || !l_in_r (r, Rs))
	return ("bad data base call");

    if (err = cr_reg (&R)) return (err);
    R->mat = r->mat;

    re = Rs->prev; 
    Re = rs->prev;

  /*...Split. */
    re->next = rs; 	rs->prev = re;
    Re->next = Rs;	Rs->prev = Re;
    r->bnd = rs;
    R->bnd = Rs;

  /*...Adjust lengths. */
    r->len = 0; WALK (r->bnd, l, f) r->len++;
    R->len = 0; WALK (R->bnd, l, f) R->len++;

  /*...Point the parameter at the new region. */
    *nr = R;
    return(0);
}


/*-----------------JOIN_REGION------------------------------------------
 *Join two regions. More pointer trickiness.
 *On return, R points into hyperspace, since that region has been zapped.
 *----------------------------------------------------------------------*/
char *join_reg (r, R, rs, Rs)
    region *r, *R;	/* Regions to join. */
    lledge *rs, *Rs;	/* Edge after the join. */
{
    lledge *re, *Re, *l, *f;
    
    if (inhibit()) return ("No data base operations with triangles!");
    if (!r || !R || !rs || !Rs) return ("bad data base call");
    if (!l_in_r (r, rs) || !l_in_r (R, Rs)) return ("bad data base call");

    re = rs->prev;
    Re = Rs->prev;
  /*...Join. */
    re->next = Rs;	Rs->prev = re;
    rs->prev = Re;	Re->next = rs;
    R->bnd = 0;
    ds_reg (R);

  /*...Adjust length */
    r->len = 0; WALK (r->bnd, l, f) r->len++;

  /*...Always return 0. Nothing could go wrong here, right? Arf, arf. */
    return(0);
}
