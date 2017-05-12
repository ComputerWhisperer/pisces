
/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	depl_width.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:27		*/




#include <math.h>

#ifdef ANSI_FUNC

double 
depl_width (double rev_bias, double pconc, double nconc)
#else
/*  determine the depletion depth - returned in microns */
double depl_width( rev_bias, pconc, nconc )
double rev_bias;
double pconc;
double nconc;
#endif

{
    double q = 1.6e-19;
    double eps_silicon = 11.7 * 8.85e-14;
    double ni = 1.45e10;
    double kt_q = 0.026;
    double phi;
    double dop;
    double depl;


    if ( nconc == 0 )  {
	phi = kt_q * log( pconc/ni );
	dop = 1./pconc;
    }
    else if ( pconc == 0 )  {
	phi = kt_q * log( nconc/ni );
	dop = 1./nconc;
    }
    else  {
	phi = kt_q * log( pconc*nconc/(ni*ni) );
	dop = 1./nconc + 1./pconc;
    }

    depl = 1.0e4 * sqrt( ( 2 * eps_silicon * (phi+rev_bias) * dop ) / q );

    return(depl);
}
