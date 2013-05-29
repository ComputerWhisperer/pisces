/*static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/tridata.c,v 1.2 85/10/18 13:43:57 conor Exp $";*/
/*************************************************************************
 *                                                                       *
 * tridata.c - Does the data base work to split a region.                *
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
#include <stdlib.h>

void edge_pl (struct Sedge *i, int color);

/*-----------------SP_REG-----------------------------------------------
 * Split region at top of stack (nreg), leaving result
 * in regions nreg, nreg+1. Shorter half is at nreg+1.
 *----------------------------------------------------------------------*/
char * sp_reg (lep1, lep2)
	struct LLedge *lep1, *lep2;	/* Edges to link */
{
    char *err;
    int ie,ir,iR,f,enew,ltmp,llen,slen,lreg,sreg;
    struct LLedge *bp,*bnd,*plep1,*plep2,*maxa,*less;
    struct Sreg *parent;
    double intang();

  /*...Make lep2-lep1 the shorter path. */
    for (llen=0,bp=lep2 ; bp != lep1; bp=bp->next) llen++;
    for (slen=0,bp=lep1 ; bp != lep2; bp=bp->next) slen++;
    if  (llen < slen) {
	bp=lep1; lep1=lep2; lep2=bp;
	ltmp=llen; llen=slen; slen=ltmp;
	}

  /*...Create new edge with ends of split. */
    enew = 0;
    err = cr_edge(&enew, 0, nB(lep1), nB(lep2)); 	if (err) return(err);
    if (debug1) edge_pl(edge[enew],1); 

  /*...Remove parent, create son and daughter regions */
    parent = reg[nreg];
    reg[nreg--] = 0;
    lreg = sreg = 0;
    err = cr_reg(&lreg, parent->mat);			if (err) return(err);
    err = cr_reg(&sreg, parent->mat);			if (err) return(err);

  /*...Add new edge as root of new regions. */
    err = ad_edge(lreg,enew,NL,TRUE,BEFORE);if (err) return(err);
    err = ad_edge(sreg,enew,NL,FALSE,BEFORE);	if (err) return(err);

  /*...Splice old boundary into two new boundaries. */
    plep1 = lep1->prev;
    plep2 = lep2->prev;
    reg[lreg]->len = 1+llen;	/* 1+ because we didn't count the divider. */
    reg[sreg]->len = 1+slen;

    reg[lreg]->bnd->next = lep2;
    reg[lreg]->bnd->prev = plep1;
    lep2->prev           = reg[lreg]->bnd;
    plep1->next          = reg[lreg]->bnd;

    reg[sreg]->bnd->next = lep1;
    reg[sreg]->bnd->prev = plep2;
    lep1->prev            = reg[sreg]->bnd;
    plep2->next           = reg[sreg]->bnd;

  /* 
   * Update region pointers of edges. 
   * Since the long region is where the old one was, skip it.
   */
    bnd = reg[sreg]->bnd;
    for (bp = bnd->next; bp != bnd; bp=bp->next) {
	ie = bp->edge;
	if (edge[ie]->r[0]==lreg)      edge[ie]->r[0] = sreg;
	else if (edge[ie]->r[1]==lreg) edge[ie]->r[1] = sreg;
	else return("Multiple edge pointer in sp_reg");
	}

  /* 
   * Update angle lists.
   * A. Split list of angles, preserving order. Omit angle at
   *    beginning, end of divider.
   */
    maxa = parent->maxa;
    for (f=1, bp = maxa; (bp != maxa) || f; bp = less,f=0) {
	less = bp->lt;
	if (bp != lep1 && bp != lep2) {
	    ie = bp->edge;
	    if (edge[ie]->r[0] > nreg-2) ir = edge[ie]->r[0];
	    else if (edge[ie]->r[1] > nreg-2) ir = edge[ie]->r[1];
	    else return("E->R inconsistency in sp_reg");
	    add_ang(reg[ir],bp);
	    }
	}

  /*...B. Compute new angles and add to lists in appropriate positions */
    for (ir = lreg, iR = sreg ; ir <= sreg; ir++, iR--) {
	bnd = reg[ir]->bnd;
	bnd->ang   = intang (nB(bnd->prev), nB(bnd), nB(bnd->next));
	reg[iR]->bnd->next->ang -= bnd->ang;

	add_ang(reg[ir], bnd);
	add_ang(reg[iR], reg[iR]->bnd->next);
	}

  /*...Finally, get rid of parent storage. */
    free(parent);
    return(0);
}

/*-----------------DUPL-------------------------------------------------
 * DUPL : duplicate a region, leaving the new copy at the top of the stack.
 * The user region is duplicated before triangulating it becuase the 
 * triangulation recursively breaks whatever region it's working on into
 * smaller pieces, and we want the user regions to remain after we're done.
 *----------------------------------------------------------------------*/
char * dupl(ir)
    int ir;
{

    int rnew,f,enew;
    char *err, *cr_reg(), *cr_edge(), *ad_edge();
    struct LLedge *bnd, *bp, *nbp, *destiny[MAXNODE];
    struct Sedge *ep;

  /*   
   *...A. Create new region.
   */
    rnew = 0;
    err = cr_reg (&rnew, reg[ir]->mat); 		if(err) return(err);

  /*...B. Duplicate edges and linked list. */
    bnd = reg[ir]->bnd;
    for (f=1, bp = bnd; (bp != bnd) || f; bp = bp->next, f=0) {
	ep = edge[bp->edge];

       /*...Duplicate edge - because edge[bp->edge] already belongs to
	*...1 or 2 user regions, so we can't make it belong to the
	*...temporary triangulation regions too.
	*/
	enew = 0;				    
	err = cr_edge(&enew, ep->elec, ep->n[0], ep->n[1]); 
        if(err) return(err);
						    /* Add edge to rnew. */
	err = ad_edge (rnew, enew,  reg[rnew]->bnd, bp->iscc, BEFORE);
	if(err) return(err);
						    
	reg[rnew]->bnd->prev->ang = bp->ang;	    /* Copy angle. */
	edge[enew]->orig = ep->orig;

      /*...The tricky part : keep track of where edges go */
	destiny[bp->edge] = reg[rnew]->bnd->prev;
	}

  /*
   *...C. Duplicate angle linked list, using destiny array.
   *...   bp walks around old boundary, nbp around new.
   */
    reg[rnew]->maxa = destiny[reg[ir]->maxa->edge];
    
    for (f=1, bp = bnd, nbp=reg[rnew]->bnd; 
	    (bp != bnd) || f; bp = bp->next, f=0, nbp = nbp->next) {
	nbp->gt = destiny[bp->gt->edge];
	nbp->lt = destiny[bp->lt->edge];
	}

    return(0);

}

