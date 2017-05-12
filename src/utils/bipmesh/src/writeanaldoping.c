/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	writeanaldoping.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:30:10		*/



/* writeanaldopant.c - prints out dopant card for an analytic profile */

#include <stdio.h>
#include "struct.h"

extern double RATIO_LAT;

#ifdef ANSI_FUNC

int 
writeanaldoping (FILE *fp, dop_str *dopant, double left_edge, double right_edge, char *str)
#else

writeanaldoping( fp, dopant, left_edge, right_edge, str )
FILE *fp;
dop_str *dopant;
double left_edge;
double right_edge;
char *str;
#endif

{
    char *dop_type[2];

    dop_type[N_TYPE] = "n.type";
    dop_type[P_TYPE] = "p.type";

    fprintf( fp, "\n$ %s\n", str );

    switch ( dopant->type )  {
	case N_GAUSSIAN:
	case P_GAUSSIAN:
    fprintf( fp, "doping reg=1 gauss char=%g peak=%g conce=%e %s\n",
       (dopant->dop_tag).anal_dop.std_dev, (dopant->dop_tag).anal_dop.peak_loc,
       (dopant->dop_tag).anal_dop.peak_dop, dop_type[ dopant->type - 10 ] );

    fprintf( fp, "+\tx.lef=%g x.rig=%g ratio.la=%g\n",
	left_edge, right_edge, RATIO_LAT );

    break;

	case N_ERFC:
	case P_ERFC:
    fprintf( fp, "doping reg=1 erfc char=%g peak=%g conce=%e %s\n",
       (dopant->dop_tag).anal_dop.std_dev, (dopant->dop_tag).anal_dop.peak_loc,
       (dopant->dop_tag).anal_dop.peak_dop, dop_type[ dopant->type - 12 ] );

    fprintf( fp, "+\tx.lef=%g x.rig=%g ratio.la=%g erfc.lat\n",
	left_edge, right_edge, RATIO_LAT );

    break;
    }  /* end switch */
}
