/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


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
    fprintf( fp, "doping reg=2 gauss char=%g peak=%g conc=%e %s\n",
       (dopant->dop_tag).anal_dop.std_dev, (dopant->dop_tag).anal_dop.peak_loc,
       (dopant->dop_tag).anal_dop.peak_dop, dop_type[ dopant->type ] );

    fprintf( fp, "+\tx.lef=%g x.rig=%g ratio.la=%g\n",
	left_edge, right_edge, RATIO_LAT );
}
