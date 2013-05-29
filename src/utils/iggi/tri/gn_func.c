static char rcsid[]="$Header: gn_func.c,v 1.1 85/05/16 20:26:14 conor Exp $";
/***********************************************************************
 *                                                                     *
 * gn_func.c - function to be optimized, independent variable update.  *
 *                                                                     *
 * Copyright c 1985 The board of trustees of the Leland Stanford       *
 *                  Junior University. All rights reserved.            *
 * This subroutine may not be used outside of the SUPREM4 computer     *
 * program without the prior written consent of Stanford University.   *
 *                                                                     *
 * Original: CSR Nov.84                                                *
 * Modified: CSR Dec.84 (Added Hessian terms)                          *
 * Modified: CSR Jan.85 (Removed "" for speed)                         *
 *                                                                     *
 ***********************************************************************/
#include "general.h"
#include "dbase.h"
#include <math.h>		
#include <stdlib.h>
#include <stdio.h>

int ntfunc = 3;				/* Number of functions per triangle */

#define ASCALE 0.2			/* About 12 degrees */
#define SIXTY  1.04719755119659774614	/* Sixty degrees */

/*-----------------FUNC-------------------------------------------------
 * Returns the values of the functions to be optimized.
 * Straightforward if inefficient : sinh ((angle - 60)/ASCALE);
 * Chosen because it grows rapidly as angle -> 90, and
 * has a non-zero derivative at 60, unlike a square/cube law.
 *----------------------------------------------------------------------*/
double func (ifunc)
    int ifunc;		/* Index of function */
{
    int ie, it, n0, n1, n2;
    double ax, ay, bx, by, cosa, ang, arg;
    it = (ifunc-1)/ntfunc+1;	/* Number of triangle. */
    ifunc = (ifunc-1)%ntfunc;	/* Now in the range 0..ntfunc-1 */
    ie = ifunc;			/* Which edge 0..2 */
    n0 = tri[it]->n[ie];
    n1 = tri[it]->n[(ie+1)%3];
    n2 = tri[it]->n[(ie+2)%3];
    ax = node[n1]->x - node[n0]->x;
    ay = node[n1]->y - node[n0]->y;
    bx = node[n2]->x - node[n0]->x;
    by = node[n2]->y - node[n0]->y;
    cosa = (ax*bx + ay*by) / sqrt ((ax*ax+ay*ay)*(bx*bx+by*by));
    ang = acos(cosa);
    arg = (ang-SIXTY)/ASCALE;
    return (sinh(arg) - ASCALE/ang);
}


/*-----------------JACOBI-----------------------------------------------
 * Returns the jacobi element 
 *    d f        /  d x     for each of its dependent variables j.
 *       ifunc         j   
 *----------------------------------------------------------------------*/
jacobi (ifunc, jval)
    int ifunc;		/* Index of function */
    double jval[];	/* Jacobian values */
{
    int ie, it, n0, n1, n2, iload;
    double ax,bx,ay,by,n2a,n2b,cosa,sqab,cosh;
    double dangdax,dangdbx,dangday,dangdby;
    double ang, dfdang, earg;


    it = (ifunc-1)/ntfunc+1;	/* Number of triangle. */
    ie = (ifunc-1)%ntfunc;	/* Which angle */

    n0 = tri[it]->n[ie];	/* n0 is at the angle subtended by n1-n2. */
    n1 = tri[it]->n[(ie+1)%3];
    n2 = tri[it]->n[(ie+2)%3];
    ax = node[n1]->x - node[n0]->x;
    ay = node[n1]->y - node[n0]->y;
    bx = node[n2]->x - node[n0]->x;
    by = node[n2]->y - node[n0]->y;

    /*...Compute common terms */
    n2a = ax*ax + ay*ay;
    n2b = bx*bx + by*by;
    sqab = sqrt(n2a*n2b);
    cosa = (ax*bx + ay*by) / sqab;
    ang = acos(cosa);
    earg = exp((ang-SIXTY)/ASCALE);
    cosh = 0.5*(earg + 1/earg);
    dfdang = cosh/ASCALE + ASCALE/(ang*ang) ;

    /*...Jacobian */
    dangdax =  ay/n2a;
    dangday = -ax/n2a;
    dangdbx = -by/n2b;
    dangdby =  bx/n2b;
    iload = 2*ie;
    jval[iload++] = -dfdang*(dangdax+dangdbx);
    jval[iload++] = -dfdang*(dangday+dangdby);	if (iload==6) iload=0;
    jval[iload++] = dfdang*dangdax;
    jval[iload++] = dfdang*dangday;		if (iload==6) iload=0;
    jval[iload++] = dfdang*dangdbx;
    jval[iload++] = dfdang*dangdby;
    return (0);
}

/*-----------------LSCALE-----------------------------------------------
 * Compute length scales per point = min diameter of surrounding triangles.
 *----------------------------------------------------------------------*/
char * lscale(scale)
    double **scale;	/* Vector returned */
{
    double minx,maxx,miny,maxy,xdi,ydi,x,y;
    int iv, it, j, in, ix, iy;
    static char err[40];	/* Static so we can print it in main! */

    if (!(*scale = (double *) calloc(2*nnode+1,sizeof(double)) )) 
	return("Out of memory in lscale.");

    FOR (iv, 1, 2*nnode) (*scale)[iv] = MAXFLOAT;
    FOR (it, 1, ntri) {
	minx = miny = MAXFLOAT;
	maxx = maxy = -MAXFLOAT;
	FOR (j, 0, 2) {
	    x = node[tri[it]->n[j]]->x;
	    y = node[tri[it]->n[j]]->y;
	    if (x > maxx) maxx = x;
	    if (x < minx) minx = x;
	    if (y > maxy) maxy = y;
	    if (y < miny) miny = y;
	    }
	xdi = maxx - minx;
	ydi = maxy - miny;
	if (xdi==0 || ydi==0) 
	    {sprintf(err,"Triangle %d is flat!",it); return(err);}
	FOR (j, 0, 2) {
	    in = tri[it]->n[j];
	    ix = 2*(in-1)+1;
	    iy = ix+1;
	    if (xdi < (*scale)[ix]) (*scale)[ix] = xdi;
	    if (ydi < (*scale)[iy]) (*scale)[iy] = ydi;
	    }
	}
    return(0);
}

/*-----------------DO_STEP----------------------------------------------
 * Use the computed step to update the value of the independent variables.
 *----------------------------------------------------------------------*/
char * do_step (step, dir)
    double step[];	/* Step to take */
    double dir;		/* Positive or negative direction? */
{
    int in, ix;

   /*...For the grid problem, simply increment the grid locations. */
    for (ix = 0, in = 1; in <= nnode; in++) {
	node[in]->x += dir * step[++ix];
	node[in]->y += dir * step[++ix];
	}
    return(0);
}
    

/*-----------------TEST_STEP--------------------------------------------
 * Compute the value of (but don't move to) a new location.
 *----------------------------------------------------------------------*/
double test_step (step, dir, tol, scale)
    double dir;			/* Damping */
    double step[];		/* Vector of updates */
    double tol, scale[];	/* Tolerance, local scales. */
{
/* DECLARE */
    int ifunc;
    double fv, fsum, func();

    do_step(step, dir);
    for (fsum=0, ifunc=1; ifunc <= ntri*ntfunc; ifunc++) {
        fv = func (ifunc);
        fsum += fv*fv;
    }
    do_step(step, -dir);
    return(fsum);
}
