static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/flip.c,v 1.5 85/10/25 14:47:35 conor Exp $";
/***********************************************************************
 *                                                                     *
 * flip.c - Routines for doing the Lawson flip.                        *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Dec.84                                                *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"	/* Must know what the triangles are! */
#include "thyme.h"	/* CPU timing functions */
#include <stdlib.h>

/*-----------------FLIP-------------------------------------------------
 * Triangle flipper - makes triangulation Delauney.
 *----------------------------------------------------------------------*/
char * flip()
{
    int it,in,j,count,all_done,need_flip(),*todo,*done;
    double elap = log_cpu (0.0, "");

    /*...Need to remember which nodes have been done, which to do. */
    if (!(
	(todo = (int *) calloc(1+ntri, sizeof(int)) ) &&
	(done = (int *) calloc(1+ntri, sizeof(int)) )
       )) return("flip: insufficient storage for flag arrays");

    for (it=1; it <= ntri; it++) todo[it] = TRUE;	

  /*...Smoothing loop. */
    count = 0;
    do {
	all_done = TRUE;
	for (it=1; it <= ntri; it++) {	
	    done[it] = !todo[it];
	    todo[it] = FALSE;
	    }

	for (it=1; it <= ntri; it++) {
	    if (done[it]) continue;
	    for (j=0; j<=2; j++) {	/* Check each nbr of same material. */
		in = tri[it]->e[j];	
		if (in >= 0 && tri[in]->reg == tri[it]->reg) 
		    if (need_flip(it,in)) {
			do_flip(it,in,todo);
			all_done = FALSE;
		    }
		}
	    }
	}
    while (!all_done && ++count < 30);	/* Limit in case of roundoff problem.*/

    free(todo);	free(done);

    log_cpu (elap, "Lawson flip");
    return(0);
}

/*-----------------NEED_FLIP--------------------------------------------
 * Decide whether a given pair satisfies the Lawson criterion.
 *----------------------------------------------------------------------*/
int need_flip(it,in)
    int it,in;		/* Triangle and neighbour */
{
    struct Scord c,d;
    double r,dist(),geom();
    int jn;

  /*...If it is neighbour jn of in, then node jn of in is not in it. */
    for (jn=0; jn<=2; jn++) if (tri[in]->e[jn] == it) break;

  /*...Compute circumcentre of it and see if node jn of in is inside. */
    ccentre(tri[it]->n[0], tri[it]->n[1], tri[it]->n[2], &c, &r);
    d.x = node[tri[in]->n[jn]]->x;
    d.y = node[tri[in]->n[jn]]->y;
    if (dist(&d,&c) < 0.999*r) return(1);
    else return(0);
}

/*-----------------DO_FLIP----------------------------------------------
 * Mechanics of flip.
 *----------------------------------------------------------------------*/
do_flip(tp, tq, todo)
    int tp,tq;
    int todo[];
{
    int snodes[4],sneib[6],j,jp,jq,tr,fixme;

  /*...Find which nodes are unique. */
    for (jp=0; jp<=2; jp++) if (tri[tp]->e[jp] == tq) break;
    for (jq=0; jq<=2; jq++) if (tri[tq]->e[jq] == tp) break;

  /*...Copy nodes out - order is guaranteed to remain c-clockwise. */
    snodes[0] = tri[tp]->n[jp];
    snodes[1] = tri[tp]->n[(jp+1)%3];
    snodes[2] = tri[tq]->n[jq];
    snodes[3] = tri[tq]->n[(jq+1)%3];
    sneib[0] = tri[tq]->e[(jq+1)%3];
    sneib[1] = tq;
    sneib[2] = tri[tp]->e[(jp+2)%3];
    sneib[3] = tri[tp]->e[(jp+1)%3];
    sneib[4] = tp;
    sneib[5] = tri[tq]->e[(jq+2)%3];

  /*...and back in. */
    for (j=0; j<3; j++) {
	tri[tp]->n[j] = snodes[j];
	tri[tq]->n[j] = snodes[(j+2)%4];
	tri[tp]->e[j] = sneib[j];
	tri[tq]->e[j] = sneib[j+3];
	}

  /*...have to fix the zeroth neighbor of tp, tq */
    for (tr=tp; tr==tp || tr==tq; tr += tq-tp) {/* for tr = each of tp, tq */
	if ((fixme = tri[tr]->e[0]) < 0) continue;
	for (j = 0; j < 3; j++) 
	    if (tri[fixme]->e[j] == tp+tq-tr) tri[fixme]->e[j] = tr;
    }

  /*...mark them all. */
    for(j=0; j<3; j++) if (tri[tp]->e[j] >= 0) todo[tri[tp]->e[j]] = TRUE;
    for(j=0; j<3; j++) if (tri[tq]->e[j] >= 0) todo[tri[tq]->e[j]] = TRUE;


}
