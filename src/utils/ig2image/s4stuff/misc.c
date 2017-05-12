

static char SccsID[] = "@(#)misc.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   misc.c                Version 3.1     */
/*   Last Modification : 9/14/87  10:39:27 */

#include <math.h>
#include "global.h"
#include "constant.h"
#include "geom.h"

/************************************************************************
 *									*
 *	dist( a     ,  b     ) - this function returns the distance 	*
 *  between two points. if b is null, just return magnitude.            *
 *									*
 ************************************************************************/
/*
float dist ( a, b)
float a[2], b[2];
{
    float tx, ty;

    tx = a[0];
    ty = a[1]; 
    if (b) {
	tx -= b[0];
	ty -= b[1]; 
    } 
    return ( hypot (tx, ty) );
}
*/   

/************************************************************************
 *									*
 *	area_tri(a1,a2,b1,b2,c1,c2) - this routine returns the area of 	*
 *  the triangle formed by the coordintes passed.			*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

float 
area_tri (double a1, double a2, double b1, double b2, double c1, double c2)
#else

float area_tri(a1,a2,b1,b2,c1,c2)
float a1,a2,b1,b2,c1,c2;
#endif
{
    return( (b1*(c2-a2) + c1*(a2-b2) + a1*(b2-c2) ) * 0.5 );
}



/************************************************************************
 *									*
 *	pt_in_tri(x,y,tr) - this routine determins if the pt x,y is in  *
 *  triangle number tr.							*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
pt_in_tri (double x, double y, int tr)
#else

pt_in_tri(x,y,tr)
float x,y;
int tr;
#endif
{
    int locint;
    float tx1, tx2, tx3, ty1, ty2, ty3;
    float abtol, artol, tsign;
    int n1, n2, n3;

    /*get triangle coordinates*/
    n1 = tri[tr]->nd[0];
    n2 = tri[tr]->nd[1];
    n3 = tri[tr]->nd[2];
    tx1 = pt[ nd[n1]->pt ]->cord[0];
    ty1 = pt[ nd[n1]->pt ]->cord[1];
    tx2 = pt[ nd[n2]->pt ]->cord[0];
    ty2 = pt[ nd[n2]->pt ]->cord[1];
    tx3 = pt[ nd[n3]->pt ]->cord[0];
    ty3 = pt[ nd[n3]->pt ]->cord[1];

    /*calculate tolerances*/
    abtol = 1e-04;
    artol = area_tri(tx1,ty1, tx2,ty2, tx3,ty3);
    tsign = artol/fabs(artol);
    artol = fabs(artol);
    artol = artol * abtol;

    /*begin the testing*/
    if ( (tsign * area_tri( tx1,ty1, tx2,ty2, x,y )) <= -artol)
	return( FALSE );
    if ( (tsign * area_tri( tx2,ty2, tx3,ty3, x,y )) <= -artol)
	return( FALSE );
    if ( (tsign * area_tri( tx3,ty3, tx1,ty1, x,y )) <= -artol)
	return( FALSE );
    
    /*if all that failed, retun true*/
    return( TRUE );
}

/*-----------------PTNREG-----------------------------------------------
 * Determine whether the point (x0,y0) is within the region. Algorithm
 * 112 from CACM. This little gem works for clockwise, anticlockwise,
 * or even self-crossing regions.
 *----------------------------------------------------------------------*/
/*
int ptnreg (x0, y0, nr, r)
	float x0, y0;		The point in question
	int nr;			The number of points in the region boundary
	float r[][2];		The coordinates of the points. 
{
    int flag, i;
    double xi, yi, xin, yin;

   * If a line to infinity crosses an odd number of edges, its inside.

    flag = TRUE;

    xi = r[nr-1][0]; 
    yi = r[nr-1][1];
    
    for (i = 0; i < nr; i++) {
	xin = r[i][0];
	yin = r[i][1];

	if ((y0 <= yi) == (y0 > yin))
	    if ((x0-xi - (y0-yi)*(xin-xi)/(yin-yi) ) < 0)
		flag = ! flag;

	xi = xin;
	yi = yin;
	}

     What a short, strange trip it's been 

    return (!flag);
}
*/
