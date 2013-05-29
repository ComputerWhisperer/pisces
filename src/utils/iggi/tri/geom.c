static char rcsid[]="$Header: geom.c,v 1.1 85/05/16 20:25:59 conor Exp $";
/*************************************************************************
 *                                                                       *
 * geom.c - geometry routines for tri.                                   *
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

/*-----------------GEOM-------------------------------------------------
 * Returns a value representing the goodness of a triangle, in [-1...+1].
 * Negative values are for clockwise triangles. 
 *----------------------------------------------------------------------*/
double geom (i,j,k)
    int i,j,k;
{
#define NORM 3.464101616   /* So equilaterals come out one. */
    double x1,y1,x2,y2,x3,y3,det,dd;

    x1 = node[k]->x - node[j]->x;
    y1 = node[k]->y - node[j]->y;
    x2 = node[j]->x - node[i]->x;
    y2 = node[j]->y - node[i]->y;
    x3 = node[i]->x - node[k]->x;
    y3 = node[i]->y - node[k]->y;
    det = x2*y1-x1*y2; 
    dd = x1*x1 + y1*y1 + x2*x2 + y2*y2 + x3*x3 + y3*y3;
    return (det*NORM/dd);
}

/*-----------------CCENTRE----------------------------------------------
 * Compute circumcentre of a triangle.
 *----------------------------------------------------------------------*/
char * ccentre (i, j, k, c, r)
    int i,j,k;		/* Indices of points. */
    struct Scord *c;	/* Centre. */
    double       *r;    /* Radius. */
{
    struct Scord p,q,dp,dq,alph;
    double dist();
    int lil();

  /*...Center is defined by intersection of perp. bisectors of sides. */
    p.x = 0.5 * (node[j]->x + node[i]->x);
    p.y = 0.5 * (node[j]->y + node[i]->y);
    q.x = 0.5 * (node[k]->x + node[j]->x);
    q.y = 0.5 * (node[k]->y + node[j]->y);
    dp.x = -0.5 * (node[j]->y - node[i]->y);
    dp.y =  0.5 * (node[j]->x - node[i]->x);
    dq.x = -0.5 * (node[k]->y - node[j]->y);
    dq.y =  0.5 * (node[k]->x - node[j]->x);

    if (lil (&p,&dp,&q,&dq,&alph))	return("Flat triangle in ccentre.");
    c->x = p.x + alph.x*dp.x;
    c->y = p.y + alph.x*dp.y;
    p.x  = node[i]->x;
    p.y  = node[i]->y;
    *r   = dist(&p,c);
    return(0);
}

/*-----------------D_PERP-----------------------------------------------
 * Compute the directed perpendicular distance from edge to point.
 * (Same as lil routine, but simplifies call.)
 * Convention : if *ep is vertical, and *np is in the right half plane,
 * signed distance is positive.
 * Value returned is in units of the length of the edge.
 *----------------------------------------------------------------------*/
char * d_perp(ep,np,alph)
    struct Sedge *ep;
    struct Snode *np;
    struct Scord  *alph;
{
    struct Scord p,dp,q,dq;
    int ier,i,j;
    static char err[40];

    i = ep->n[0];
    j = ep->n[1];
    p.x = node[i]->x;
    p.y = node[i]->y;
    dp.x = node[j]->x - p.x;
    dp.y = node[j]->y - p.y;
    q.x = np->x;
    q.y = np->y;
    dq.x = - dp.y;
    dq.y =   dp.x;

    ier = lil(&p,&dp,&q,&dq,alph);
    if (ier) {
	sprintf(err,"Edge %d-%d has 0 length!",i,j);
	return(err);
	}
    else
	return(0);
}
/*-----------------DIST-------------------------------------------------
 * Compute the distance from point a to point b.
 * In 3 popular flavours.
 *----------------------------------------------------------------------*/
double dist(a,b)
    struct Scord *a,*b;
{
    double dx,dy;

    dx = a->x - b->x;
    dy = a->y - b->y;

    return (sqrt (dx*dx + dy*dy));
}

double ndist(a,b)
    int a,b;
{
    double dx,dy;

    dx = node[a]->x - node[b]->x;
    dy = node[a]->y - node[b]->y;

    return (sqrt (dx*dx + dy*dy));
}

double l_edge(ie)
    int ie;
{
    double ndist();
    return (ndist(edge[ie]->n[0], edge[ie]->n[1]));
}

/*-----------------INTANG-----------------------------------------------
 * Return the internal angle of points p1-p2-p3.
 * Result in the range 0->2*PI.
 *----------------------------------------------------------------------*/
double intang(p1,p2,p3)
    int p1,p2,p3;
{
    double dx1,dy1,dx2,dy2,l1,l2,denom,cosa,sina,alph;

    dx1 = node[p2]->x - node[p1]->x;
    dy1 = node[p2]->y - node[p1]->y;
    dx2 = node[p3]->x - node[p2]->x;
    dy2 = node[p3]->y - node[p2]->y;
    l1 =  sqrt (dx1*dx1 + dy1*dy1);
    l2 =  sqrt (dx2*dx2 + dy2*dy2);
    denom = l1*l2;
    if (denom <= 0) return(MAXFLOAT);
    cosa = (dx1*dx2 + dy1*dy2) / denom;
    sina = (dx1*dy2 - dx2*dy1) / denom;

    /*Protect against rounding error*/
    if (cosa > 1) cosa = 1;
    if (cosa < -1) cosa = -1;

    alph =  acos(cosa);
    if (sina<0) alph= -alph;
    return(PI-alph);
}

/*-----------------LIL--------------------------------------------------
 * Compute the intersection of two lines in p,dp form.
 * Return 0 if ok,
 *        1 if parallel.
 *----------------------------------------------------------------------*/
int lil(p,dp,q,dq,alph)
    struct Scord *p,*dp,*q,*dq,*alph;
{
    double dx,dy,det;

    dx  = p->x - q->x;
    dy  = p->y - q->y;
    det = dp->y*dq->x - dp->x*dq->y;
    if (det == 0) {
	alph->x = 0;
	alph->y = 0;
	return(1);
	}
    alph->x = (dx*dq->y - dy*dq->x)/det;
    alph->y = (dx*dp->y - dy*dp->x)/det;
    return(0);
}



