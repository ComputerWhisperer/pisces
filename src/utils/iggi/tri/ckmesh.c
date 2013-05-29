static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/ckmesh.c,v 1.4 85/10/29 17:11:57 conor Exp $";
/*************************************************************************
 *                                                                       *
 * ckmesh.c - check the mesh for consistency, set up counter-clockwise   *
 *            data structure.                                            *
 *            Exits on error.                                            *
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

char *ckmesh()
{
    int ie; short nflag[MAXNODE];
    char *err, *ck_clock();
    static char ebuf[160];
    int f,nn,in,j,ir;
    struct LLedge *bp,*bnd;
    

  /*...Check we got anything at all. */
    if (nnode <= 2 || nedge <= 2 || nreg <= 0) {
	sprintf(ebuf,"Require >2 nodes, >2 edges, >0 regions, got(%d,%d,%d)\n",
	   nnode, nedge, nreg);
	return(ebuf);
	}

  /*...Check each edge is reasonable. If so, mark its nodes.*/
    for (in=1; in<=nnode; in++) nflag[in] = 0;

    for (ie=1; ie<=nedge; ie++) {
	for (j=0; j<=1; j++) {
	    nn = edge[ie]->n[j];
	    if (nn <= 0 || nn > nnode) {
		sprintf(ebuf,"Edge %d is invalid, node %d is bad (%d)\n",
		   ie, j, nn); return(ebuf);
		}
	    else
		nflag[nn]++;
	    }
	/* Make sure @ didn't pick the magic no electrode code */
	if (edge[ie]->elec==NO_ELEC) {
	    sprintf(ebuf,"Edge %d has an invalid electrode code\n",ie);
	    return(ebuf);
	    }
	}

  /*...Check each node belongs to some edge. */
    for (in=1; in<=nnode; in++) 
	if (nflag[in]<=1) {
	    sprintf(ebuf,"Node %d belongs to %d edge(s)!",in,nflag[in]);
	    return(ebuf);
	    }

  /*...Check each region. */
    for (ir=1; ir <= nreg; ir++) {

      /*...Check there is a boundary. */
	if (reg[ir]->len <= 2) {
	    sprintf(ebuf,"Region %d has only %d edges\n",ir,reg[ir]->len);
	    return(ebuf);
	    }

      /*...Walk around boundary and check for reasonable edges. */
	bnd=reg[ir]->bnd;
	for (f=1,bp=bnd; (bp!=bnd) || f; bp=bp->next,f=0) {
	    if (bp->edge <= 0 || bp->edge > nedge) {
		sprintf(ebuf,"Edge of region %d is invalid (%d > %d)\n", 
		    ir, bp->edge, nedge);
		return(ebuf);
		}
	    if ((edge[bp->edge]->r[0] != ir) &&
		(edge[bp->edge]->r[1] != ir)) {
		sprintf(ebuf,"Internal ptr error : region %d edge %d\n",
		    ir,bp->edge);
		return(ebuf);
		}
	    }

	/* Set up the counter-clockwise structures. */
	err = ck_clock(ir);
	if (err) {
	    sprintf(ebuf,"Error in region %d:\n%s\n",ir,err);
	    return(ebuf);
	    }
	}
    return(0);
}

/*-----------------CK_CLOCK---------------------------------------------
 * Initialize and check counter-clockwise structures for region # ir. 
 *----------------------------------------------------------------------*/
char * ck_clock(ir)
    int ir;
{
    struct LLedge *ep,*tmp,*bnd,*ptrs[MAXNODE];
    struct Sedge *enext,*eprev;
    int f,acomp(),mod(),n1,n0,no,io;
    double tang,intang();
    static char err[80];

    /* A. generate iscc = whether the edge parallels the region. */

    bnd = reg[ir]->bnd;

    for(f=1,ep=bnd; (ep!=bnd) || f; ep=ep->next, f=0) {
	n1 = edge[ep->edge]->n[1];
	n0 = edge[ep->edge]->n[0];
	enext = edge[ep->next->edge];
	eprev = edge[ep->prev->edge];
	if (((n1==enext->n[1]) || (n1==enext->n[0])) && 
	    ((n0==eprev->n[1]) || (n0==eprev->n[0])))
	   ep->iscc = TRUE;

	if (((n1==eprev->n[1]) || (n1==eprev->n[0])) &&
	    ((n0==enext->n[0]) || (n0==enext->n[1])))
	   ep->iscc = FALSE;

	if ((ep->iscc != FALSE) && (ep->iscc != TRUE)) {
	    sprintf(err, "Region discontinuous around edge %d",ep->edge);
	    return(err);
	    }
	}

    /* 
     * B. Calculate internal angles. Sum of external angles
     *    should be 2*pi. 
     */
    tang = 0;
    for(f=1,ep=bnd ; (ep!=bnd) || f; ep=ep->next,f=0) {
	ep->ang = intang (nB(ep->prev), nB(ep), nB(ep->next));
	tang += PI - ep->ang;
	}

    if (debug2) printf("Region %d has angle %24.15lf\n",ir,tang);

    /* Fine if 2*PI */
    if (dabs(tang-2*PI) < EPS)
	; /* No complaints */


    /* Hopeless if zero. */
    else if (dabs(tang) < EPS) return("Region crosses itself.");

    /* Fix if -2*PI */
    else if (dabs(tang+2*PI) < EPS) {
	for(f=1,ep=bnd; (ep!=bnd) || f; ep=ep->next,f=0) {
	    tmp = ep->next;		/* Swap pointers to make it c-c. */
	    ep->next = ep->prev;
	    ep->prev = tmp;
	    ep->iscc = !ep->iscc;	/* If it was cc before it isn't now */
	    ep->ang = 2*PI - ep->ang;
	    }
	}

    /* Huh? */
    else return("Region has malformed boundary");

    /* C. Sort angles by size. */
    for (no=0,f=1,ep=bnd; (ep != bnd)|| f; ep=ep->next,f=0) 
	ptrs[no++]=ep;

    qsort(ptrs, no, sizeof(struct LLedge *), acomp);

    reg[ir]->maxa=ptrs[no-1];
    reg[ir]->mina=ptrs[0];
    for (io=0; io<no ; io++) {
	ptrs[io]->gt = ptrs[mod(io+1,no)];
	ptrs[io]->lt = ptrs[mod(io-1,no)];
	}

    return(0);
	
} /* End of ck_clock. */

/* 
 * ACOMP - comparison routine for quicksort. 
 */
int acomp (lep1, lep2)
    struct LLedge **lep1,**lep2;
{
    double v;
    v = (*lep1)->ang - (*lep2)->ang;
    if      (v<0)  return(-1);
    else if (v==0) return(0);
    else           return(1);
}
