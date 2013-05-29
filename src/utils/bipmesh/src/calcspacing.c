/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	calcspacing.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:24		*/



/*  calcspacing - determines spacing between 2 nodes given an initial
 *    	spacing, the ratio of spacing between neighboring points, and
 *  	the number of nodes that have been traversed from the initial
 *	point to the current position.  
 */

#include <math.h>
#include "struct.h"

double calcspacing( initial, ratio, nodes )
double initial;
double ratio;
int nodes;

{
    double spacing;

    spacing = initial * pow( ratio, (double)nodes );

    return(spacing);
}	
