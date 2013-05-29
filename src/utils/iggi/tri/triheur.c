//static char rcsid[]="$Header: /users/suprem/ig2/tri/RCS/triheur.c,v 1.2 85/10/30 23:24:08 conor Exp $";
/*************************************************************************
 *                                                                       *
 * triheur.c - heuristics for breaking up regions                        *
 *          chop() - whether we can cut off a triangle                   *
 *          divide() - whether we can cut across the middle              *
 *          quad() - special case for quadrilaterals                     *
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

char * dvpram (double hl, double hr, double el, int *nnew, double *ratio, double *first);

/*-----------------CHOP-------------------------------------------------
 * try to cut a triangle off a region.
 * this gets called twice, the first time searching for a good-looking
 * chop, the second time taking whatever we must.
 * Returns answer in the form of a pointer to the middle edge of the tri.
 *----------------------------------------------------------------------*/
struct LLedge * chop (r, must)
    struct Sreg *r;		/* Region to split. */
    int must;			/* Is some split necessary? */
{
    struct LLedge *bestp,*bp,*mina;
    int f,i,j,k;
    double h,h1,h2,lej,m,g,c,best,mgeo, angM,cross(),geom(),dmin(),ndist();

    bestp = 0;
    best = 0;
    if (must) {	      /* If some split is necessary... */
	angM = PI-EPS;	  /* Consider any triangle angle up to PI */
	mgeo = EPS ;      /* Don't be fussy about geometry. */
	}
    else {
	angM = PI/2;	/* Consider acute triangles only. */
	mgeo = mgeom;   
	}

  /*...Start with smallest angles in region and work upwards. */
      mina = r->maxa->gt;
      for (f=1,bp = mina; (bp != mina) || f; bp = bp->gt, f=0) {
	if (bp->ang > angM) break;	

      /*...Is the triangle worth anything? */
	i = nB(bp->prev); j = nB(bp); k = nB(bp->next);
	if ((g=geom (i,j,k)) > mgeo) {

	  /*...Is it too big? */
	    lej = ndist(i,k);
	    h1 = dmin(node[i]->h,lej) / dmax(node[i]->h,lej);
	    h2 = dmin(node[k]->h,lej) / dmax(node[k]->h,lej);
	    if ((h=dmin(h1,h2)) > mgeo)

	      /*...Does the line [nearly] cross the boundary? */
		if ((c=cross(r, bp->prev, bp->next)) > mgeo) { 

		  /*...Got one! If it passed the strict test, it's good.
		   *...Otherwise keep looking for something better. */
		    if (!must) return(bp);
		    else {
			m = dmin(h,g);
			m = dmin(m,c);
			if (m > best) {
			    best = m;
			    bestp = bp;
			    }
			}
		    }
		}
	}
	return(bestp);
}

/*-----------------DIVIDE-----------------------------------------------
 * Choose good ways to cut a region in half.
 * Returns pointers to the edge following each end of the cut.
 *----------------------------------------------------------------------*/
int divide(r, lep1, lep2)
    struct Sreg *r;
    struct LLedge **lep1,**lep2;
{
    struct LLedge *bp1,*bp2,*maxa;
    double piby2, piby4, ang1, ang2, intang(), cross();
    int f1,f2;

    piby2 = PI/2;
    piby4 = PI/4;


  /*...Try every combination of large angles. */
    maxa = r->maxa;
    for (f1=1, bp1=maxa;   ((bp1 != maxa) || f1) && bp1->ang > piby2; 
	 					bp1=bp1->lt, f1=0) {

	for (f2=1, bp2=maxa; ((bp2 != maxa) || f2) && bp2->ang > piby2; 
						bp2=bp2->lt, f2=0) {

	    if (bp2==bp1 || bp1==bp2->next || bp2==bp1->next) continue;
	
	  /*...Screen out edges which are subdivisions of a common parent.
	   *...(Just a cheap trick to save time because they are collinear)
	    if (edge[bp1->edge]->orig != 0 &&
	        edge[bp1->edge]->orig == edge[bp2->edge]->orig) continue;
          */
	  /* 
	   * Check dividing line is inside the region, and 
	   * angles are not too small.
	   */
	    ang1 = intang (nB(bp1->prev), nB(bp1), nB(bp2));
	    ang2 = intang (nB(bp2->prev), nB(bp2), nB(bp1));
	    if (ang1 < piby4 || ang2 < piby4) continue;
	    if (bp1->ang - ang1 < piby4 || bp2->ang - ang2 < piby4) continue;

	  /*...Check for [near] crossings */
	    if (cross(r, bp1, bp2) < 0.5) continue;

	  /*...Hey, must have found one. */
	    *lep1 = bp1;
	    *lep2 = bp2;
	     return(1);
	     }
	}
  /*...No luck if here. */
  return(0);
}

/*-----------------CROSS------------------------------------------------
 * Evaluates "closeness" of region nodes to given segment.
 * Return value is the closest node divided by a measure of the local
 * spacing. The bigger this value, the better.
 * -1 means crossing.
 *----------------------------------------------------------------------*/
double cross(r, lp1, lp2)
    struct Sreg *r;
    struct LLedge *lp1,*lp2;
{
    struct LLedge *bp;		/* Walks around the region. */
    struct Sedge split;		/* Line joining nB(lp1) and nB(lp2) */
    struct Scord alph,pval;	/* Present & prev projection of node on split */
    double sign,lej,near,h1,h2,hmax,ratio,first,ndist();
    int nnew;


    split.n[0] = nB(lp1); 	
    split.n[1] = nB(lp2); 	
    lej = ndist (nB(lp1), nB(lp2));
    dvpram (node[nB(lp1)]->h, node[nB(lp2)]->h, lej, &nnew, &ratio, &first);
    near = MAXFLOAT;

    pval.x = 0.5;	pval.y = 1;
    for (sign=1, bp=lp1->next; bp != lp1 ; bp=bp->next) {
	
	if (bp==lp2) {		/* Change sign crossing the equator. */
	    pval.x = 0.5; pval.y = sign = -1; 
	    continue;
	    }

	d_perp (&split, node[nB(bp)], &alph);	

      /*...We have a crossing if previous y * this y < 0 */
	if (pval.y * alph.y < 0) {	/* Then check it intersects inside */
		double inter= (pval.y*alph.x - alph.y*pval.x)/(pval.y - alph.y);
		if (inter > -EPS && inter < 1+EPS) 
		    return(-1);
		}
	pval.y = alph.y;	pval.x = alph.x;

      /*...Now do a check to see if the node is too close to the split. */
	if (sign*alph.y < 0) continue;		   /* Skip wraparound points. */
	if (alph.x < -EPS || alph.x > 1+EPS) continue; /* Not between ends. */
	h1 = node[nB(bp)]->h / lej;		      /* Local spacing at bp. */
	h2 = first + (ratio-1)*alph.x;		      /* l.s. along split.    */
	hmax = dmax(h1,h2);
	near = dmin (near, dabs(alph.y)/hmax);
	}

    return(near);
}

/*-----------------QUAD-------------------------------------------------
 * choose Vornoi partition of a quadrilateral.
 *----------------------------------------------------------------------*/
struct LLedge * quad(rg)
    struct Sreg *rg;	/* Region to split */
{
    int f,i,j,k; char *err, *ccentre();
    struct Scord c,z;
    double dist(),r;
    struct LLedge *bp;

    for (bp = rg->maxa->gt, f=0; f<=1; f++, bp = bp->next) {
	i = nB(bp->prev);		/* Compute circumcircle */
	j = nB(bp);
	k = nB(bp->next);
	err = ccentre(i,j,k,&c,&r);	if (err) return(0);

					/* If other node is outside, ok. */
	z.x = node[nB(bp->next->next)]->x;
	z.y = node[nB(bp->next->next)]->y;
	if (dist(&z,&c) > 0.999*r) return(bp);
	}
    return(0);
}
