/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


#include <stdio.h>
#include <math.h> 
#include "struct.h"

/*  calcnode - calculate the number of nodes necessary given the
 *		region width, the ratio between neighboring points,
 *  		and either one of the following spacings:
 *		  1) the initial spacing 
 *		  2) the final desired spacing
 *              one of the above spacings must be set to zero while
 *             	the other should be specified in microns.
 */

extern double dmax();

calcnode( width, ratio, init, final )
double width;
double ratio;
double init;
double final;
{
    double tmp;
    double node;

    if ( width <= 0 )  {
	fprintf( stderr, "width <= 0. abort\n" );
	exit( ERROR );
    }
    if ( (init != 0.0) && (final != 0.0) )  {
	 printf(" only one of these can be nonzero \n");
	 exit(-1);
    }
    else if ( (final == 0.0) && (ratio > 1.0) ) 
	node = floor(log( 1 - (1 - ratio)*width/init )/log(ratio) + 0.5);
    else  if ( (init == 0.0) && (ratio < 1.0) )  {
	tmp = final/(width * (1 - ratio));
	node = floor(1 + ( log(tmp/(1+tmp*ratio)) / log(ratio) ) + 0.5 );
    }
    else if ( (final == 0.0) && (ratio == 1.0) )  
	node = floor( width/init + 0.5 );
    else if ( (init == 0.0) && (ratio == 1.0) )
	node = floor( width/final + 0.5 );
/*
    else if ( (final == 0.0) && (ratio < 1.0) )  {
      this case can be dangerous.  just start adding points until the
        location is within one spacing of the desired width
     
        node = 1.0;
        while (!done)  {
	    tmp = init * ( (1 - pow(ratio,node)) / (1 - ratio) );
	    printf(" width-tmp is %g \n", width-tmp);
	    if ( abs(width - tmp) < dmax(0.02, (init * pow(ratio,node))) )
	        done = 1.0;
	    else
	        node += 1.0;
        }
    }
*/
    else  {
	printf(" case not handled currently\n");
	exit(-1);
    }

    if (node  <= 0.0)
	node = 1.0;

    return((int)node);
}
