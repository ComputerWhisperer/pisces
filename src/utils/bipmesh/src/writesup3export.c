/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	writesup3export.c		Version 1.2		*/
/*	Last Modification:	1/29/90 01:40:01		*/



/* writesup3export.c - prints out dopant card for an analytic profile */

#include <stdio.h>
#include "struct.h"

extern double RATIO_LAT;
extern exporttype;

writesup3export( fp, dop, left_edge, right_edge, str )
FILE *fp;
dop_str *dop;
double left_edge;
double right_edge;
char *str;

{
    char *dop_type[9];
    dop_str dopant;

    dopant = *dop;

    dop_type[0] = "error";
    dop_type[1] = "error";
    dop_type[2] = "error";
    dop_type[3] = "error";
    dop_type[4] = "error";
    dop_type[BORON] = "boron";
    dop_type[PHOSPHORUS] = "phosphorus";
    dop_type[ARSENIC] = "arsenic";
    dop_type[ANTIMONY] = "antimony";

    fprintf( fp, "\n$ %s\n", str );

    if ( exporttype == 1  )  
        fprintf( fp, "doping reg=1 ascii suprem3 infile=%s %s\n" ,
           dopant.dop_tag.sup3_dop.filename, 
           dop_type[dopant.dop_tag.sup3_dop.type] );
    else
        fprintf( fp, "doping reg=1 suprem3 infile=%s %s\n" ,
           dopant.dop_tag.sup3_dop.filename, 
           dop_type[dopant.dop_tag.sup3_dop.type] );

    fprintf( fp, "+\tx.lef=%g x.rig=%g ratio.la=%g\n",
	left_edge, right_edge, RATIO_LAT );
}
