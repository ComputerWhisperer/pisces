static char rcsid[]="$Header: hinit.c,v 1.1 85/05/16 20:26:26 conor Exp $";
/*************************************************************************
 *                                                                       *
 * hinit.c - Compute local spacing around nodes.                         *
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
#include "geom.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
/*-----------------HINIT------------------------------------------------
 * Initialize the local spacing for each node.
 *----------------------------------------------------------------------*/
char * hinit()
{
	int j,in,jn,ie,jmin,jmax;
	double d0,l,lej,dij,dne,ndist(),l_edge(),hmin,hM;
	static char err[40]; char *perr, *d_perp();
	struct Scord alph;
	struct LLedge * p_edge[MAXNODE], *ptmp, *bp;
	short done[MAXNODE];

      /*...Negate user values so we can recognize them */
	FOR (in, 1, nnode) node[in]->h *= -1;

        /* For each node, compute the min distance to another. */
        for (in=1; in<=nnode; in++) { 

	    for (jn=1; jn<=nnode; jn++) {
		if (jn==in) continue;

		dij = ndist (in, jn);
		if (dij < node[in]->h && node[in]->h >= 0) node[in]->h = dij;
		if (dij < node[jn]->h && node[jn]->h >= 0) node[jn]->h = dij;
		if (dij == 0) {
		    sprintf(err,"Nodes %d and %d are superimposed",in,jn);
		    return(err);
		    }
		}
	    }


	/* 
	 * For each edge, compute the minimum distance to each node. 
	 */
	for (ie=1; ie<=nedge; ie++) {
	    for (in=1; in<=nnode; in++) {
		if (in==edge[ie]->n[0] || in==edge[ie]->n[1]) continue;

		perr = d_perp (edge[ie], node[in],&alph);
		if (perr) return(err);

		/* Check for closeness. */
		if (alph.x <= EPS || alph.x >= 1-EPS)
		    continue; 			/* Not between ends */

		lej = l_edge (ie);
		dne = lej*alph.y;		/* dne is the distance. */
		dne = dabs(dne);
		if (dne < node[in]->h && node[in]->h >= 0) node[in]->h = dne;

		for (j=0; j<=1; j++) {
		    if (dne < node[jn=edge[ie]->n[j]]->h) {    /* Too close */
			d0 = ndist (jn, in);	
			l = sqrt (d0*d0 - dne*dne);		
			if (dne < l && node[jn]->h >= 0) node[jn]->h = dne;
			}
		    }
		}
	    }


	/* Finally, round down big h values so that interval ratios < mr */
	/* A. Make linked list of edges for each node. */
	for (in=1; in<=nnode; in++) if (node[in]->h < 0) node[in]->h *= -1;
	for (in=1; in<=nnode; in++) p_edge[in]=0;

	for (ie=1; ie <= nedge; ie++) 
	    for (j=0; j <= 1; j++) {
		jn = edge[ie]->n[j];
		ptmp = p_edge[jn];
		p_edge[jn] = (struct LLedge *) malloc(sizeof(struct LLedge));
		p_edge[jn]->edge = ie;
		p_edge[jn]->next = ptmp;
		}

	/* B. Search points in order of increasing h, and round down big h */
	for (in=1; in<=nnode; in++) done[in] = FALSE;
	for (in=1; in<=nnode; in++) {

	    /* Set jmin to index of minimum remaining h. */
	    for (hmin = MAXFLOAT,jmin=0,jn=1; jn<=nnode; jn++) {
		if (done[jn]) continue;
		if (node[jn]->h < hmin) {
		    hmin = node[jn]->h;
		    jmin = jn;
		    }
		} 
	    if (jmin==0) return("Internal error, jmin==0 in hinit");

	    /* 
	     * Examine each of the edges connecting jmin, and round down.
	     * Formula for maximum step(hM) results from the requirement      
	     *          hM <= mr**n *hm    
	     * where hm=min. step, and maximum ratio=mr, and n is given by 
	     * 		(mr**(n+1)-1)/(mr-1)*hm = length of edge.            
	     */
	    for (bp = p_edge[jmin]; bp != 0; bp = bp->next) {
		ie = bp->edge;
		lej = l_edge (ie);
		jmax = edge[ie]->n[0] + edge[ie]->n[1] - jmin;
		hM  = (hmin + (mr-1)*lej)/mr;
		if (node[jmax]->h > hM) node[jmax]->h = hM;
		}
	    done[jmin]=TRUE;
	    }

	/* Free up the claimed memory. */
	for (in=1; in <= nnode; in++) 
	    for (bp=p_edge[in]; bp != 0; bp=ptmp){
		ptmp=bp->next;
		free(bp);
		}
		

	/* Sanity check against zero. */
	for (in=1; in<=nnode; in++)
	    if (node[in]->h == 0) {
		sprintf(err,"Well node %d had spacing zero.",in);
		return(err);
		}
	return(0);

}
