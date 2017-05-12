/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*	writebaseIV.c		Version 1.2		*/
/*	Last Modification:	1/24/90 10:34:57		*/


/*
 * writebaseIV.c  -  writes to the output file information 
 *   necessary for pisces to generate base IV characteristics
 */

#include <stdio.h>
#include <math.h>
#include "struct.h"
extern dop_str dop_data[4];
extern vert_str vert_info;

#ifdef ANSI_FUNC

int 
writebaseIV (FILE *fp, double maxcollector, double minbase, double maxbase, double baseincrement)
#else

writebaseIV( fp, maxcollector, minbase, maxbase, baseincrement)
FILE *fp;
float maxcollector;
float minbase;
float maxbase;
float baseincrement;
#endif

{
    int nsteps;
    float vbase = minbase;
    float collectorincrement = 0.1;
    float mincollector = 0.0;

    fprintf( fp, "\n$ base characteristics. Vc is %f, maxVb is %f\n",
	maxcollector, maxbase );

    fprintf( fp, "symb newton carriers=2\n" );
    fprintf( fp, "method autonr itlimit=30\n" );

    fprintf( fp, "models consrh auger bgn conmob fermi\n" );

    fprintf( fp, "$ solve for the initial step.  save in file temp1\n" );
    fprintf( fp, "solve initial v2=%f v3=%f outfile=temp1\n",
	minbase, mincollector );
    fprintf( fp, "regrid potential step=0.2 outf=mesh3\n\n" );

    fprintf( fp, "symb newton carriers=2\n" );
    fprintf( fp, "method autonr itlimit=30\n" );
    fprintf( fp, "solve initial v2=%f v3=%f outfile=temp1\n",
	minbase, mincollector );

    /* ramp up collector voltage to maxcollector */
    nsteps = (maxcollector - mincollector)/collectorincrement;
    if ( fabs((double)((float)nsteps * collectorincrement)) <
		fabs((double)(maxcollector-mincollector)) )
	    nsteps++;

    fprintf( fp, "solve v3=%f vstep=%f nsteps=%d electrode=3\n",
	mincollector, collectorincrement, nsteps );


    fprintf( fp, "$ save I-V information in collectorIV.log\n" );
    fprintf( fp, "log ivfile=baseIV.log\n" );


    nsteps = (maxbase - minbase)/baseincrement;
    if ( fabs((double)((float)nsteps * baseincrement)) <
		fabs((double)(maxbase-minbase)) )
	    nsteps++;

    fprintf( fp, "solve v2=%f vstep=%f nsteps=%d electrode=2\n",
	minbase, baseincrement, nsteps );

    fprintf( fp, "$ plot statement in ascii format in file iv.ascii\n" );
    fprintf( fp, "plot.1d x.axis=v3 y.axis=i3 ascii outfile=iv.ascii\n" );

}
