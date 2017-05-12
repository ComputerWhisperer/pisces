/*----------------------------------------------------------------------
 *
 * geom.c - Assorted geometry routines.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Feb85
 *---------------------------------------------------------------------*/

#include <math.h>	/* For sqrt function. */
#include "general.h"
#include "dbase.h"

/*-----------------DIS--------------------------------------------------
 * (The inevitable) distance function.
 * Don't call it with big arguments!
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

double 
dis (double ax, double ay, double bx, double by)
#else

double dis (ax, ay, bx, by)
    double ax,ay,bx,by;
#endif
{
    double dx = ax-bx,  dy = ay-by;
    return (sqrt (dx*dx + dy*dy));
}

/*-----------------EDIS-------------------------------------------------
 * Perpendicular distance from a point to an edge.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

double 
edis (struct Sedge *e, double x, double y)
#else

double edis (e, x, y)
    struct Sedge *e;
    double x,y;
#endif
{
    double dx, dy, c1, c2;

  /*...Compute coordinates of node with respect to edge. */
    dx = e->n[1]->x - e->n[0]->x;
    dy = e->n[1]->y - e->n[0]->y;
    lil (e->n[0]->x, e->n[0]->y, dx, dy, x, y, -dy, dx, &c1, &c2);

  /*...Reject points which aren't between the ends of the edge */
    if (c1 < 0 || c1 > 1) return (MAXFLOAT);

  /*...Otherwise return the perpendicular distance. */
    return (dabs (c2 * sqrt (dx*dx + dy*dy)));
}


/*-----------------PTNREG-----------------------------------------------
 * Determine whether the point (x0,y0) is within the region. Algorithm
 * 112 from CACM. This little gem works for clockwise, anticlockwise,
 * or even self-crossing regions.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
ptnreg (double x0, double y0, struct Sreg *r)
#else

int ptnreg (x0, y0, r)
	double x0, y0;
	struct Sreg *r;
#endif
{
    struct LLedge *f, *b;
    int flag;
    double xi, yi, xin, yin;
    node *n;

  /*
   * If a line to infinity crosses an odd number of edges, its inside.
   * Only works for closed regions.
   */
    flag = TRUE;

    WALK (r->bnd, f, b) {
	n = f->edge->n[0];	xi = n->x;	yi = n->y;
	n = f->edge->n[1];	xin = n->x;	yin = n->y;

	if ((y0 <= yi) == (y0 > yin))
	    if ((x0-xi - (y0-yi)*(xin-xi)/(yin-yi) ) < 0)
		flag = ! flag;
	}
	
    return (!flag);

}

/*-----------------OBTUSE-----------------------------------------------
 * Test whether a triangle is obtuse.
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
obtuse (struct Stri *t)
#else

int obtuse (t)
    struct Stri *t;
#endif

{
#define oberr -1e-10	/* Tolerance */
     
double 	x0, x1, x2, y0, y1, y2, delx10, delx20, delx21,
	dely10, dely20, dely21, d10, d20, d21;
int obt;

    x0 = t->n[0]->x;	y0 = t->n[0]->y;
    x1 = t->n[1]->x;	y1 = t->n[1]->y;
    x2 = t->n[2]->x;	y2 = t->n[2]->y;
    delx10 = x1-x0;
    dely10 = y1-y0; d10 = sqrt(delx10*delx10+dely10*dely10);
    delx20 = x2-x0;
    dely20 = y2-y0; d20 = sqrt(delx20*delx20+dely20*dely20);
    delx21 = x2-x1;
    dely21 = y2-y1; d21 = sqrt(delx21*delx21+dely21*dely21);
 
  /*...check dot products...*/
    obt = FALSE;
    if ( (delx10*delx20 + dely10*dely20) / (d10*d20)  < oberr)
       obt = TRUE;
       else if ( (-delx10*delx21 - dely10*dely21) / ( d10*d21 )  < oberr)
               obt = TRUE;
               else if ( (delx20*delx21 + dely20*dely21) / (d20*d21)  < oberr)
                       obt = TRUE;
    return (obt);
}


/*-----------------LIL--------------------------------------------------
 * Compute the intersection of two lines in p,dp form.
 * Return 0 if ok, 
 *        1 if parallel (or either dp, dq = 0)
 *----------------------------------------------------------------------*/
#ifdef ANSI_FUNC

int 
lil (double px, double py, double dpx, double dpy, double qx, double qy, double dqx, double dqy, double *ap, double *aq)
#else

int lil (px, py, dpx, dpy, qx, qy, dqx, dqy, ap, aq)
    double px, py, dpx, dpy, qx, qy, dqx, dqy, *ap, *aq;
#endif
{
    double dx,dy,det;

    dx  = px - qx;
    dy  = py - qy;
    det = dpy*dqx - dpx*dqy;
    if (det == 0) {
	*ap = 0;
	*aq = 0;
	return(1);
	}
    *ap = (dx*dqy - dy*dqx)/det;
    *aq = (dx*dpy - dy*dpx)/det;
    return(0);
}
