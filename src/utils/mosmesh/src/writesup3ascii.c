/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* writesup3ascii.c - prints out dopant card for an analytic profile */

#include <stdio.h>
#include "struct.h"

extern double RATIO_LAT;

#ifdef ANSI_FUNC

int 
writesup3ascii (FILE *fp, dop_str dopant, double left_edge, double right_edge, char *str)
#else

writesup3ascii( fp, dopant, left_edge, right_edge, str )
FILE *fp;
dop_str dopant;
double left_edge;
double right_edge;
char *str;
#endif

{

    fprintf( fp, "\n$ %s\n", str );
    fprintf( fp, "doping reg=2 ascii infile=%s\n" ,
       dopant.dop_tag.sup3_dop.filename ); 

    fprintf( fp, "+\tx.lef=%g x.rig=%g ratio.la=%g\n",
	left_edge, right_edge, RATIO_LAT );
}
