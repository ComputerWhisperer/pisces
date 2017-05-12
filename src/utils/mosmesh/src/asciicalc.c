/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



/* asciicalc.c
 *	determine junction depth and standard deviation from
 *	an ascii doping profile
 */

#include <stdio.h>
#include <math.h>

extern double dmax();

#ifdef ANSI_FUNC

int 
asciicalc (FILE *dopingfile, double *junction, double *std_dev, double *peakdop, double *sub_dop)
#else

asciicalc( dopingfile, junction, std_dev, peakdop, sub_dop )
FILE *dopingfile;
double *junction;
double *std_dev;
double *peakdop;
double *sub_dop;
#endif
{
    double peakloc = 0.0;
    double depth = 0.0;
    double conc = 0.0;
    int done = 0;

    /*  first pass to determine substrate doping.  take the concentration
     *   at the back of the wafer to be the substrate doping
     */
    while (feof(dopingfile) == 0)
	fscanf( dopingfile, "     %lf    %lf", &depth, &conc );

    /*  since there are cases where the doping goes to 0 at the
     *   back contact (for some strange reason)  we'll just take
     *   the maximum value from all of the doping profiles
     */
    *sub_dop = dmax( *sub_dop, fabs(conc) );

    /* rewind file */
    rewind( dopingfile );

    while ( (!done ) && (feof(dopingfile) == 0) )  {
	fscanf( dopingfile, "     %lf    %lf", &depth, &conc );
	if ( fabs(conc) > *peakdop )  {
	    *peakdop = fabs(conc);
	    peakloc = depth;
	}
	if ( (fabs(conc) <= *sub_dop) && (*peakdop > *sub_dop) )  {
	    done = 1;
	    *junction = depth;
	}
    }
    *std_dev = (*junction - peakloc) / sqrt( log(*peakdop/(*sub_dop)) );
}
