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
 * writegateIV.c  -  writes to the output file information 
 *   necessary for pisces to generate gate IV.characteristics
 */

#include <stdio.h>
#include <math.h>
#include "struct.h"

extern dop_str dop_data[10];

writegateIV( fp, maxdrain, mingate, maxgate, gateincrement )
FILE *fp;
float maxdrain;
float mingate;
float maxgate;
float gateincrement;

{
    int nsteps;

    
    nsteps = (maxgate-mingate)/gateincrement;
    if ( fabs((double)((float)nsteps * gateincrement)) <
	    fabs((double)(maxgate-mingate)) )
	nsteps++;

    fprintf( fp, "$ gate characteristics. Vd is %f, minVg is %f maxVg is %f\n",
	maxdrain, mingate, maxgate );

    /* determine majority carrier based on the substrate.  if they
     *  counterdope then tough luck
     */
    if ( dop_data[2].type == P_TYPE ) /* PMOS device */
        fprintf( fp, "symb gummel carriers=1 holes\n" );
    else
        fprintf( fp, "symb gummel carriers=1 electrons\n" );

    fprintf( fp, "method iccg damped\n");

    /* specify gate material here.  */
/*
    fprintf( fp, "$ srfmob may not yet be defined in your verion. if not \n" );
    fprintf( fp, "$ use the following instead\n" );
    fprintf( fp, "$  models conmob\n" );
    fprintf( fp, "$  material num=1 g.surf=0.5\n" );
*/
    fprintf( fp, "models srfmob\n" );
    fprintf( fp,"$ change the below line if another gate material is used\n");
    fprintf( fp, "contac num=1 n.poly\n");

    fprintf( fp, "$ solve for the initial step.  save in file initial\n" );
    fprintf( fp, "solve initial v1=%f outfile=initial\n", mingate );

    fprintf( fp, "$ save I-V information in gateIV.log\n" );
    fprintf( fp, "log ivfile=gateIV.log\n" );

    /* solve for drain current in gate volt increments  */
    fprintf( fp, "regrid potential step=0.2\n" );

    /* determine majority carrier based on the substrate.  if they
     *  counterdope then tough luck
     */
    if ( dop_data[2].type == P_TYPE ) /* PMOS device */
        fprintf( fp, "symb gummel carriers=1 holes\n" );
    else
        fprintf( fp, "symb gummel carriers=1 electrons\n" );

    fprintf( fp, "method iccg damped\n");

    fprintf( fp, "solve v1=%f v3=%f vstep=%f nsteps=%d  electrode=1\n",
	    mingate, maxdrain, gateincrement, nsteps );

    fprintf( fp, "$ plot statement in ascii format in file iv.ascii\n" );
    fprintf( fp, "plot.1d x.axis=v1 y.axis=i3 ascii outfile=iv.ascii\n" );

}
