

static char SccsID[] = "@(#)sint.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   sint.c                Version 1.3     */
/*   Last Modification : 9/7/88  09:07:00 */

#include <stdio.h>
#include <math.h>
extern double erf(), erfc();
#include <ctype.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "expr.h"
#include "plot.h"


/************************************************************************
 *									*
 *	sol_interp() - This routine evaluates two the two parameter fns	*
 *  that are used to interpolate solution variables.			*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

float 
sol_interp (int type, double val1, double val2)
#else

float sol_interp( type, val1, val2 )
int type;
float val1;
float val2;
#endif
{
    register int i;
    struct d_str data[MAXPNT];
    int count;
    float ret;

    /*get a one dimensional line profile*/
    switch( type ) {
    case X :	count = do_1d( YSEC, val1*1.0e-4, data, -1, -1, 0 );
		break;
    case Y :	count = do_1d( XSEC, val1*1.0e-4, data, -1, -1, 0 );
		break;
    case Z :	count = do_1d( XSEC, val1*1.0e-4, data, -1, -1, 0 );
		break;
    }

    /*x and y functions work similarly*/
    if ( (type == X) || (type == Y) ) {
	
	/*run up the list and find the intersection*/
	for(i = count-1; ((data[i].y > val2) == (data[i-1].y > val2)) && (i > 1); i--);

	/*interpolate to the answer*/
	if ( data[i].y == data[i-1].y ) 
	    ret = data[i].x;
	else
	    ret = (val2-data[i-1].y) * (data[i].x-data[i-1].x) / (data[i].y-data[i-1].y) +
	      data[i-1].x;
	
	return( ret * 1.0e4 );
    }
    else {
	val2 *= 1.0e-4;
	
	/*run up the list and find the intersection*/
	for(i = count-1; ((data[i].x > val2) == (data[i-1].x > val2)) && (i > 1); i--);

	/*interpolate to the answer*/
	if ( data[i].x == data[i-1].x ) 
	    ret = data[i].x;
	else
	    ret = (val2-data[i-1].x) * (data[i].y-data[i-1].y) / (data[i].x-data[i-1].x) +
	      data[i-1].y;
	
	return( ret );
    }
}





/************************************************************************
 *									*
 *	sol_interp() - This routine evaluates two the two parameter fns	*
 *  that are used to interpolate solution variables.			*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

float 
interface (int type, double loc)
#else

float interface( type, loc )
int type;
float loc;
#endif
{
    register int i;
    struct d_str data[MAXPNT];
    int count, match;
    int mat1, mat2;
    float ret;

    /*get a one dimensional line profile*/
    count = do_1d( XSEC, loc*1.0e-4, data, -1, -1, 0 );

    /*split out the material numbers*/
    type /= MAXMAT;

    mat2 = type % MAXMAT;
    mat1 = (type - mat2) / MAXMAT;

    /*run up the data and find the intersection*/
    for(match = i = 0; (i < count-1) && (!match); i++) {
	if ( (data[i].mat == mat1) && (data[i+1].mat == mat2) ) match = i;
	if ( (data[i].mat == mat2) && (data[i+1].mat == mat1) ) match = i+1;
    }
    if (match)
	return( data[match].x * 1.0e4 );
    else
	return( -1.0 );
}

