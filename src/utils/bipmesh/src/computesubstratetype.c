/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	computesubstratetype.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:25		*/



/* computesubstratetype.c
 *
 *  computes type of substrate (n or p) based on the doping in the emitter
 *   region.  used for suprem3 ascii profiles.
 */

#include <stdio.h>
#include "struct.h"

computesubstratetype( dop, type )
dop_str dop;
int *type;
{
   FILE *fp;
   double depth;
   double conc;
   int i;

   fp = fopen( dop.dop_tag.sup3_dop.filename, "r" );
   /* go 5 points into the silicon in case surface effects
    *	change things
    */
   for ( i = 0; i < 5; i++ )
       fscanf( fp, "     %lf    %lf", &depth, &conc );
   if ( conc < 0 )
       *type = P_TYPE;
   else
       *type = N_TYPE;
   fclose( fp );
}
