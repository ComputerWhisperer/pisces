static char rcsid[]="$Header: bfill.c,v 1.2 85/10/18 13:43:22 conor Exp $";
/*************************************************************************
 *                                                                       *
 * bfill.c - routines to subdivide edges.                                *
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
#include <math.h>
#include <stdio.h>

#ifdef DONTSUBDIVIDE
#define dontsubdivide 1
#else
#define dontsubdivide 0
#endif

/*-----------------BFILL------------------------------------------------
 * Bfill subdivides all edges prior to triangulation.
 *----------------------------------------------------------------------*/
char * bfill()
{
    char *err, *dvpram(), *dvedge();
    int ie,nnew,old_nedge;
    double lej,ratio,first,l_edge();

  /*...Keep track of old #edges because #edges changes in the loop. */
    old_nedge=nedge;
    for (ie=1; ie <= old_nedge; ie++) {
	
       /*...Get spacing parameters */
	lej = l_edge (ie);
	if (dontsubdivide) continue;
/*
	if (edge[ie]->elec == dontsubdivide) continue;
*/
	err=dvpram (node[edge[ie]->n[0]]->h, node[edge[ie]->n[1]]->h, 
		    lej, &nnew, &ratio, &first);
	if (err) return(err);

	/* Add nodes */
	err=dvedge (ie, lej, nnew, ratio, first); 	if (err) return(err);
	}
	return(0);
}


/*-----------------DVEDGE-----------------------------------------------
 * Divide an edge, using the spacing parameters calculated in dvpram.
 * Called from bfill, and also during the triangulation (triang.c).
 *----------------------------------------------------------------------*/
char * dvedge (ie, lej, nnew, ratio, first)
    int ie;		/* Edge to divide. */
    int nnew;		/* Number of new nodes to add */
    double lej;		/* Length of edge. */
    double ratio;	/* Ratio of each interval to next. */
    double first;	/* First interval as a fraction of lej. */
{
    int inew, inode;
    char *err, *cr_node(), *sp_edge();
    double x,y,dx,dy,h,frac;
 
    if (nnew==0) return(0);
    x  = node [edge[ie]->n[0]]->x;
    y  = node [edge[ie]->n[0]]->y;
    dx = node [edge[ie]->n[1]]->x - x;
    dy = node [edge[ie]->n[1]]->y - y;
    frac = first;
    h =  frac*lej;
    if (ratio < 1) h *= ratio;

    for (inew=1; inew<=nnew; inew++) {

	x += frac*dx;
	y += frac*dy;
	
	inode=0;
	err = cr_node (&inode, x, y, h);		if (err) return(err);
	if (debug1) node_pl(node[inode],1);
	err = sp_edge (ie,inode); 			if (err) return(err);
	if (edge[ie]->n[0] != inode) ie = nedge;

	h *= ratio;
	frac *= ratio;
	}
    return(0);
}

/*-----------------SP_EDGE----------------------------------------------
 * This does the database grunt work of adding one new point to an edge.
 *----------------------------------------------------------------------*/
char * sp_edge(ie,ip)
    int ie;	/* Edge to split */
    int ip;	/* Point to add  */
{
    struct Sedge *ep;
    char *es;
    int enew,j;
    struct LLedge *lep;

    if (ie < 1 || ie > nedge)    return("Bad sub handed to sp_edge");
    ep = edge[ie]; 		if (ep==0) return("Bad edge handed to sp_edge");

    /* Create new edge with nodes ip,n[1] and make old have n[0],ip */
    enew=0;
    es = cr_edge (&enew, ep->elec, ip, ep->n[1]); 	if (es) return(es);
    ep->n[1] = ip;

  /*...Update each region that the edge belongs to. */
    for (j=0; j<=1; j++) {
	if (ep->r[j]==0) continue;

	lep = eindex (ep->r[j], ie);
	if (lep == 0) return("internal error: edge not in region (sp_edge)");

      /*...Add new edge after old, so that old retains its angle. */
      /*...This makes it necessary to fix clockwise edges.        */
	es = ad_edge (ep->r[j], enew, lep, lep->iscc, AFTER); 
	if (es) return(es);
	if (!lep->iscc) {
	    lep->edge = enew;
	    lep->next->edge = ie;
	    }

	lep->next->ang = PI;
	add_ang (reg[ep->r[j]], lep->next);

      /*...Mark old and new edge as being subdivisions of old. */
	edge[ie]->orig = edge[enew]->orig = ie;
	}
    return(0);
}


/*-----------------DVPRAM-----------------------------------------------
 * Spacing parameters for dividing an edge.
 *----------------------------------------------------------------------*/
char * dvpram (double hl, double hr, double el, int *nnew, double *ratio, double *first)
{
    double r,rn,hl2,hr2;

    if (hl==0 || hr==0) {
	return("Dvpram handed 0 h values");
	}

    /* Check for easy cases. */
    *nnew  = 0;
    *ratio = 1.0;
    *first = 1.0;
    if (hl > el) hl = el;
    if (hr > el) hr = el;

    /* Spacing is just length of edge. */
    if (dabs(el-hl) <= EPS*el && dabs(el-hr) <= EPS*el) return(0);

    /* Spacing is same at both ends. */
    if (dabs(hr-hl) <= EPS*el) {
	*nnew  = (int) ((el / hl)-0.49999);
	*first = 1.0 / (*nnew+1);
	return(0);
	}

    /* Ideally, the spacing increases by a fixed ratio, starting at
     * hleft, and increasing to hright, so that
     *     elength = hl + r*hl + r**2*hl + ... + r**n*hl, and 
     *                                           \------->is hr.
     * Then elength = (r**(n+1)-1)/(r-1)*hl = (r*hr-1)/(r-1)*hl
     * which can be solved as r = (el-hl/el-hr), and
     * plug back in hr/hl = r**n to calculate n.
     * However, n may not be an integer - round and adjust first step 
     * accordingly.
     */

    hr2 = hl/mr + (1-1/mr)*el;
    hl2 = hr/mr + (1-1/mr)*el;
    hr = dmin(hr,hr2);
    hl = dmin(hl,hl2);

    r = (el-hl)/(el-hr);
    rn = hr/hl;
    *nnew = (int) ((log(rn)/log(r))+0.5);
    if (debug2) 
       printf("rn=%g,r=%g,fnew=%g,nnew=%d\n",r,rn,((log(rn)/log(r))+0.5),*nnew);

    if (*nnew == 0) return(0);

    *ratio = exp(log(rn)/ *nnew);
    if (*ratio < 1/mr) *ratio = 1/mr;  
    if (*ratio > mr  ) *ratio = mr;                 
    

    *first = (*ratio-1) / (exp ((*nnew+1) * log(*ratio)) - 1);
    return(0);
}
