static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/list.c,v 1.5 85/10/16 20:45:33 conor Exp $";
/***********************************************************************
 *                                                                     *
 * list.c - build useful cross-reference lists.                        *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84                                                *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"
#include <stdlib.h>
#include <stdio.h>

/*-----------------LIST_P2T---------------------------------------------
 * Build list of triangles at a node. 
 *----------------------------------------------------------------------*/
list_p2t ()
{
    int in,it,j; struct Slink *tmp;
    static before = 0;

  /*...Clear previous stuff if neccesary. */
    if (before) FOR (in, 1, nnode) garbage (node[in]->p2t);
    FOR (in, 1, nnode) node[in]->p2t = 0;

  /*...Add each triangle to the list at each of its nodes. */
    FOR (it, 1, ntri)
	FOR (j, 0, 2) {
	    in = tri[it]->n[j];
	    tmp = node[in]->p2t;
	    node[in]->p2t = (struct Slink *) malloc(sizeof (struct Slink));
	    node[in]->p2t->i = it;
	    node[in]->p2t->next = tmp;
	    }
    before++;
    return(0);
}

/*-----------------LIST_T2T---------------------------------------------
 * Make a list of the neighbors of a triangle.
 * Uses previously generated list of triangles at a node.
 * Calls the routine tnabor to determine whether triangles are nbrs.
 *----------------------------------------------------------------------*/
list_t2t ()
{
    int it,j,in,k1,k2,tnabor();
    struct Slink *sp;

    FOR (it, 1, ntri) FOR (j, 0, 2) tri[it]->e[j] = -1;

    FOR (it, 1, ntri)
	FOR (j, 0, 2) {
	    in = tri[it]->n[j];
	    for (sp = node[in]->p2t; sp != 0; sp = sp->next) 
		if ((it != sp->i) && tnabor(it,sp->i,&k1,&k2)) {
		    tri[it]->e[k1] = sp->i;
		    tri[sp->i]->e[k2] = it;
		    }
	    }
    return(0);
}

/*-----------------TNABOR-----------------------------------------------
 * Are two triangles neighbours?
 *----------------------------------------------------------------------*/
int tnabor(it,in,kt,kn)
    int it,in,*kt,*kn;
{
    int jt,jn,indt,indn,match;

    if (it==in) return(0);
    match = indt = indn = 0;

    for (jt=0; jt<=2; jt++)
	for (jn=0; jn<=2; jn++) 
	    if (tri[it]->n[jt] == tri[in]->n[jn]) {
		indt += jt;
		indn += jn;
		match++;
		}

    if (match < 2) return(0);
    else if (match > 2) {
	printf("error in tnabor\n");
	return(0);
	}
    else {
	*kt = 3 - indt;
	*kn = 3 - indn;
	return(1);
	}
}

/*-----------------LIST_P2P---------------------------------------------
 * List of neighbors at a point.
 * Collects all points, sorts by node number and removes duplicates.
 * N.B. definition of neighbor includes the node itself.
 *----------------------------------------------------------------------*/
list_p2p ()
{
    int in, t, j, swap;
    struct Slink *tmp, *l, *m, *bl;
    static int before;

  /*...Garbage disposal? */
    if (before) FOR (in, 1, nnode) garbage (node[in]->p2p);
    FOR (in, 1, nnode) node[in]->p2p = 0;

    FOR (in, 1, nnode) {			             /* For each node */
	/*...Copy all the neighbours from the list of triangles */
	for (l = node[in]->p2t; l != 0; l = l->next) { /* For each nbr */
	    t = l->i;
	    FOR (j, 0, 2) {			     /* For each of its nodes */
		tmp = node[in]->p2p;
		node[in]->p2p = (struct Slink *) malloc (sizeof (struct Slink));
		node[in]->p2p->i = tri[t]->n[j];
		node[in]->p2p->next = tmp;
		}
	    }

	/*...Bubble sort lists (OK for small lists) */
	for (l = node[in]->p2p; l != 0; l = l->next) {
	    for (m = l->next; m != 0; m = m->next) {
		if (l->i > m->i) {
		    swap = l->i;
		    l->i = m->i;
		    m->i = swap;
		    }
		}
	    }

	/*...Remove duplicates. */
	if (node[in]->p2p)
	    for (bl = node[in]->p2p; l = bl->next; ) 
		if (l->i == bl->i) {
		    bl->next = l->next;
		    free(l);
		    }
		else bl = l;

	}

    before++;
    return(0);
}


/*-----------------GARBAGE----------------------------------------------
 * Garbage a linked list. On return head points nowhere.
 *----------------------------------------------------------------------*/
garbage (head)
    struct Slink *head;
{
    struct Slink *tmp, *xtmp;

    for (tmp = head; tmp != 0; tmp = xtmp) {
	xtmp = tmp->next;
	free(tmp);
	}
}
