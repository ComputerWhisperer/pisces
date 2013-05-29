/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*
 * writedrainIV.c  -  writes to the output file information 
 *   necessary for pisces to generate drain IV.characteristics
 */

#include <stdio.h>
#include <math.h>
#include "struct.h"
extern dop_str dop_data[10];
extern vert_str vert_info;

writedrainIV( fp, mindrain, maxdrain, mingate, maxgate, drainincrement, gateincrement )
FILE *fp;
float mindrain;
float maxdrain;
float maxgate;
float mingate;
float drainincrement;
float gateincrement;

{
    int nsteps;
    float vgate = mingate;

    fprintf( fp, "\n$ drain characteristics. maxVd is %f, maxVg is %f\n",
	maxdrain, maxgate );

    /* determine majority carrier based on the substrate.  if they
     *  counterdope then tough luck
     */
    if ( (dop_data[2].type == P_TYPE) 
		|| (vert_info.substrate_dop_type == N_TYPE) ) /* PMOS device */
        fprintf( fp, "symb gummel carriers=1 holes\n" );
    else
        fprintf( fp, "symb gummel carriers=1 electrons\n" );

    fprintf( fp, "method iccg damped\n");

    /* specify gate material here.  assume n poly with a
     *   surface mobility reduction factor of 0.6
     */
/*
    fprintf( fp, "$ srfmob may not yet be defined in your verion. if not \n" );
    fprintf( fp, "$ use the following instead\n" );
    fprintf( fp, "$  models conmob\n" );
    fprintf( fp, "$  material num=1 g.surf=0.5\n" );
*/
    fprintf( fp, "models srfmob\n" );
    fprintf( fp,"$ change the below line if another gate material is used\n");
    fprintf( fp, "contac num=1 n.poly\n");

    fprintf( fp, "$ solve for the initial step.  save in file temp1\n" );
    fprintf( fp, "solve initial v1=%f v3=%f outfile=temp1\n",
	mingate, mindrain );

    fprintf( fp, "$ save I-V information in drainIV.log\n" );
    fprintf( fp, "log ivfile=drainIV.log\n" );

    fprintf( fp, "$ in most cases no current should flow at the first gate\n" );
    fprintf( fp, "$ bias point.  thus you may want to delete the next 4 lines\n" );
    while ( fabs((double)vgate) < fabs((double)(maxgate + gateincrement)) )  {
	if ( vgate != mingate )
	    fprintf( fp, "solve v1=%f v2=0 v3=%f v4=0 outfile=temp1\n", 
		vgate, mindrain );

	if ( (dop_data[2].type == N_TYPE)
			|| (vert_info.substrate_dop_type == P_TYPE) )  {
	    if ( (gateincrement > 0.25) && (vgate < 1.0) )  {
		fprintf( fp, "$ for large gate increments, it is safer to slowly ramp\n" );
		fprintf( fp, "$  the gate voltage to 1V before using larger steps\n" );
		nsteps = ( gateincrement )/0.20;
		fprintf( fp, "solve v1=%f v3=%f vstep=0.20 nsteps=%d electrode=1\n",
		    mingate + 0.20, mindrain,  nsteps );
		fprintf( fp, "solve v1=%f outfile=temp1\n", gateincrement );
		vgate += gateincrement;
		fprintf( fp, "regrid potential step=0.2\n\n" );
	    }
	}
	else  {
	    if ( (gateincrement < -0.25) && (vgate > -1.0) )  {
		fprintf( fp, "$ for large gate increments, it is safer to slowly ramp\n" );
		fprintf( fp, "$  the gate voltage to -1V before using larger steps\n" );
		nsteps = ( -gateincrement )/0.20;
		fprintf( fp, "solve v1=%f v3=%f vstep=-0.20 nsteps=%d electrode=1\n",
		    mingate - 0.20, mindrain, nsteps );
		fprintf( fp, "solve v1=%f outfile=temp1\n", gateincrement );
		vgate += gateincrement;
		fprintf( fp, "regrid potential step=0.2\n" );
	    }

	}



        fprintf( fp, "$ change to Newtons method with an automated Newton-Richardson\n");
        if ( (dop_data[2].type == P_TYPE) 
	    || (vert_info.substrate_dop_type == N_TYPE) ) /* PMOS device */  
            fprintf( fp, "symb newton carriers=1 holes\n" );
        else
            fprintf( fp, "symb newton carriers=1 electrons\n" );
        fprintf( fp, "method autonr\n" );

	nsteps = maxdrain/drainincrement;
	if ( fabs((double)((float)nsteps * drainincrement)) <
		fabs((double)(maxdrain-mindrain)) )
	    nsteps++;

	fprintf( fp, "solve v3=%f vstep=%f nsteps=%d electrode=3\n",
	    mindrain, drainincrement, nsteps );

	/* since the is done before the next step check to see if the
	 *  next gate bias already puts us over the top before printing
	 *  these lines
	 */
	if ( fabs((double)(vgate + gateincrement)) <= fabs((double)maxgate) ) {
	    fprintf( fp, "$ change to gummel for next gate bias point\n" );
            if ( dop_data[2].type == P_TYPE ) /* PMOS device */
                fprintf( fp, "symb gummel carriers=1 holes\n" );
            else
                fprintf( fp, "symb gummel carriers=1 electrons\n" );
	   
            fprintf( fp, "method iccg damped\n");
	    fprintf( fp, "$ load previous solution before ramping gate\n" );
	    fprintf( fp, "load infile=temp1\n" );
	} /* end if */
	vgate += gateincrement;
    } /* end while */


    fprintf( fp, "$ plot statement in ascii format in file iv.ascii\n" );
    fprintf( fp, "plot.1d x.axis=v3 y.axis=i3 ascii outfile=iv.ascii\n" );

}
