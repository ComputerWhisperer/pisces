
/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* calcratio.c
 *   determine ratio necessary to provide a specified initial and
 *    final spacing within a desired distance
 */

#include <stdio.h>
#include <math.h>

#ifdef ANSI_FUNC

double 
calcratio (double init, double final, double distance, int *steps)
#else
double calcratio( init, final, distance, steps )
double init;
double final;
double distance;
int *steps;
#endif

{

    int done = 0;
    double ratio = 0.0;
    double nsteps = 0.0;
    double dist2 = 0.0;

    if ( init > final )  {
	ratio = 0.99;
	while ( (!done) && (ratio > 0.40) )  {
	    nsteps = floor( log(final/init)/log(ratio) + 0.5 );
	    dist2 = init * ( 1.0 - pow( ratio, nsteps + 1.0 ) ) / ( 1.0 - ratio );
	    if ( fabs( dist2 - distance ) <= 0.1 * distance )
	        done = 1;
	    ratio = ratio - 0.01;
	}
	if ( ratio < 0.66 )
	    fprintf( stderr, 
"warning: ratio is < 0.66  (may be some instability in the solution)\n" );
    }
    else if ( init < final )  {
	ratio = 1.01;
	while ( (!done) && (ratio < 2.50) )  {
	    nsteps = floor( log(final/init)/log(ratio) + 0.5 );
	    dist2 = init * ( 1.0 - pow( ratio, nsteps + 1.0 ) ) / ( 1.0 - ratio );
	    if ( fabs( dist2 - distance ) <= 0.1 * distance )
	        done = 1;
	    ratio = ratio + 0.01;
	if ( ratio > 1.50 )
	    fprintf( stderr, 
"warning: ratio is > 1.50  (may be some instability in the solution)\n" );
	}
    }
    else  {
	ratio = 1.0;
	nsteps = distance/init;
    }
    *steps = (int)nsteps;
    return( ratio );
}
