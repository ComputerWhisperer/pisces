

static char SccsID[] = "@(#)do_1d.c	1.2\t3/30/90";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   do_1d.c                Version 3.6     */
/*   Last Modification : 11/16/88  09:46:49 */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <gplot.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "plot.h"
#include "material.h"
extern d_compar();

#define ABS(x)  (((x)>0.0)?(x):(-(x)))

/* A doubly linked list to represent the boundary */
typedef struct b_str { struct b_str *left, *right; int ie, j; } b_typ;

/************************************************************************
 *									*
 *	do_1d( ptype, val, bound, out ) - This routine plots the one 	*
 * dimensional data on the already initialized screen.  It plots either	*
 * in ptype at value val.  bound is a flag for drawing material bounds	*
 * on the screen.  If out is non-NULL, then the data is printed instead	*
 *									*
 * Original:	MEL	1/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
do_1d (
    int ptype,			/*type of one d cross section*/
    double val,			/*the value in x or y*/
    struct d_str data[],
    int mat1,
    int mat2,			/* two sides of an interface */
    int byarc			/* whether to go by arclength or x */
)
#else

do_1d( ptype, val, data, mat1, mat2, byarc )
int ptype;			/*type of one d cross section*/
float val;			/*the value in x or y*/
struct d_str data[];
int mat1, mat2;			/* two sides of an interface */
int byarc;			/* whether to go by arclength or x */
#endif
{
    int t, i, j, nx, num, in;
    int count = 0;
    int vmax;
    float arr[3][3];
    float p[2][3];
    float sign;
    float tmp, xo, xn, yo, yn;
    char a[80], b[80];

    /* Looking for x-y cross-sections */
    if ( (ptype == XSEC) || (ptype == YSEC) ) {
	
	/*step through all the triangles*/
	for( t = 0; t < ne; t++ ) {
	    if (!leaf (tri[t])) continue;

	    /*step through each triangle index*/
	    for( i = 0; i < 3; i++ ) {
		nx = tri[t]->nd[i];

		if (ptype==XSEC) {
		    arr[i][0] = pt[ nd[nx]->pt ]->cord[1]; 
		    arr[i][2] = pt[ nd[nx]->pt ]->cord[0];
		    arr[i][1] = z [ nx ];	
		}
		else if(ptype==YSEC){
		    arr[i][0] = pt[ nd[nx]->pt ]->cord[0]; 
		    arr[i][2] = pt[ nd[nx]->pt ]->cord[1];
		    arr[i][1] = z [ nx ];	
		}
	    }

	    /*now that we have all that done, calculate the line*/
	    num = intersect(arr, val, p);
	    for(j = 0; j < num; j++) {
		/*should probably save and sort these guys*/
		data[count].x = p[j][0];
		data[count].y = p[j][1];
		data[count].mat = mattyp[ tri[t]->regnum ];
		count++;
	    }
	}
        /*sort the data*/ 
        qsort(data, count, sizeof(struct d_str), d_compar);
	
	    
	/*eliminate the duplicates*/
	sprintf(a, "%16e\t%16e", data[0].x * 1e4, data[0].y);

	for(vmax = count, i = count = 1; i < vmax; i++) {
	    sprintf(b, "%16e\t%16e", data[i].x * 1e4, data[i].y);
	    
	    if ( (strcmp(a, b) != 0) || (data[count].mat != data[i].mat) ) {
		data[count].x = data[i].x;
		data[count].y = data[i].y;
		data[count].mat = data[i].mat;
		count++;
		strcpy(a, b);
	    }
	}
        /*check to make sure the materials are contiguous*/
        for(i = 1; i < count - 1; i++) {
	    if ( fabs(data[i].x - data[i-1].x) < 1.0e-10 ) {
		if ( data[i].mat != data[i+1].mat ) {
		    tmp = data[i-1].mat; data[i-1].mat = data[i].mat; data[i].mat = tmp;
		    tmp = data[i-1].y; data[i-1].y = data[i].y; data[i].y = tmp;
		    tmp = data[i-1].x; data[i-1].x = data[i].x; data[i].x = tmp;
		}
	    }
	}
    }
    /*return the number of data points*/
    return( count );
}
	

#ifdef ANSI_FUNC

int 
d_compar (struct d_str *f1, struct d_str *f2)
#else

d_compar(f1, f2)
struct d_str *f1, *f2;
#endif
{
    if ( f1->x > f2->x )
	return( 1 );
    else if ( f1->x < f2->x )
	return( -1 );
    else if ( f1->mat > f2->mat )
	return( 1 );
    else if ( f1->mat < f2->mat )
	return( -1 );
    else
	return( 0 );
}

