/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	xdmax.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:30:17		*/



/*  xdmax.c
 *	calculate maximum depletion width in the channel based on constant
 *	  substrate doping.  use this to determine extent of vertical
 *	  gridding in the channel.  if drain bias is high, then the
 *	  depletion width formed by the pn-junction will be larger
 *	  (and used for gridding instead of xdmax)
 */

#include <math.h>

double xdmax( nsub )
double nsub;  /* substrate doping */
{
    double ni = 1.45e10; /* intrinsic doping concentration */
    double epsilon_si = 1.04e-12; /* permittivity of silicon */
    double q = 1.6e-19;  /* electron charge */
    double kt_q = 0.026; /* thermal voltage */
    double phi;  /* difference in potential from the fermi level to
		  *	the intrinsic level
		  */

    phi = kt_q * log( nsub/ni );

    return( sqrt(4.0*epsilon_si*phi/q/nsub) );
}
