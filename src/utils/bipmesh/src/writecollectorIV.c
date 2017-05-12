/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



/*	writecollectorIV.c		Version 1.3		*/
/*	Last Modification:	10/24/90 13:35:00		*/

/*
 * collectorIV.c  -  writes to the output file information 
 *   necessary for pisces to generate collector IV characteristics
 */

#include <stdio.h>
#include <math.h>
#include "struct.h"
extern dop_str dop_data[4];
extern vert_str vert_info;

#ifdef ANSI_FUNC

int 
writecollectorIV (FILE *fp, double mincollector, double maxcollector, double minbase, double maxbase, double collectorincrement, double baseincrement)
#else

writecollectorIV( fp, mincollector, maxcollector, minbase, maxbase, 
	collectorincrement, baseincrement )
FILE *fp;
float mincollector;
float maxcollector;
float maxbase;
float minbase;
float collectorincrement;
float baseincrement;
#endif

{
    int nsteps;
    float ibase = minbase;

    fprintf( fp, "\n$ collector characteristics. maxVc is %f, maxVb is %f\n",
	maxcollector, maxbase );

    fprintf( fp, "contact num=2 current res=50\n" );
    fprintf( fp, "symb newton carriers=2\n" );
    fprintf( fp, "method autonr itlimit=40\n" );
    fprintf( fp, "models consrh auger bgn conmob fermi\n" );
    fprintf( fp, "$ resolve for the initial step.  save in file temp1\n" );
    fprintf( fp, "solve initial outfile=temp1\n" );
    fprintf( fp, "regrid potential step=0.2 outf=mesh3\n\n" );

    /*  need to resolve after the regrid */
    fprintf( fp, "contact num=2 current res=50\n" );
    fprintf( fp, "symb newton carriers=2\n" );
    fprintf( fp, "method autonr itlimit=40\n" );
    fprintf( fp, "models consrh auger bgn conmob fermi\n" );
    fprintf( fp, "$ resolve for the initial step.  save in file temp1\n" );
    fprintf( fp, "solve initial outfile=temp1\n" );


    /* the step for the actual initial bias step */
    fprintf( fp, "solve i2=%g outfile=temp1\n", ibase );
	
    fprintf( fp, "$ save I-V information in collectorIV.log\n" );
    fprintf( fp, "log ivfile=collectorIV.log\n" );

	
    while ( fabs((double)ibase) < fabs((double)(maxbase + baseincrement)) )  {
	if ( ibase != minbase )
	    fprintf( fp, "solve v1=0 i2=%f v3=%f v4=0 outfile=temp1\n", 
		ibase, mincollector );


	nsteps = maxcollector/collectorincrement;
	if ( fabs((double)((float)nsteps * collectorincrement)) <
		fabs((double)(maxcollector-mincollector)) )
	    nsteps++;

	fprintf( fp, "solve v3=%f vstep=%f nsteps=%d electrode=3\n",
	    mincollector, collectorincrement, nsteps );

	/* since the is done before the next step check to see if the
	 *  next base bias already puts us over the top before printing
	 *  these lines
	 */
	if ( fabs((double)(ibase + baseincrement)) <= fabs((double)maxbase) ) {
	    fprintf( fp, "$ load previous solution before ramping base\n" );
	    fprintf( fp, "load infile=temp1\n" );
	} /* end if */
	ibase += baseincrement;
    } /* end while */


    fprintf( fp, "$ plot statement in ascii format in file iv.ascii\n" );
    fprintf( fp, "plot.1d x.axis=v3 y.axis=i3 ascii outfile=iv.ascii\n" );

}
