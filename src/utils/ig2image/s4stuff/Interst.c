

static char SccsID[] = "@(#)Interst.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   Interst.c                Version 3.17     */
/*   Last Modification : 12/1/88 20:13:45 */


#include <stdio.h>
#include <math.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "impurity.h"
#include "defect.h"
#include "material.h"
#include "diffuse.h"
#include "matrix.h"

#include "plot.h"
extern float form_eval();


/************************************************************************
 *									*
 *	This file contains definitions of the Interstitial routines. It *
 *  includes the routine to calculate diffusion coefficients, the one	*
 *  to calculate any coupling terms between species, and the one to	*
 *  to calculate the boundary condition terms for the Intersitial.	*
 *  Provided the user does not want to change the parameters or access	*
 *  other data, they should be able to modify these extensively without *
 *  any harm to the convergence or solution techniques.			*
 *  									*
 *  Several data base access routines are defined in the file species.c	*
 *  to make it easier for the user to implement routines and get at	*
 *  other data.								*
 *									*
 ************************************************************************/

/************************************************************************
 *									*
 *	comp_intparam() - This routine computes the temperature 	*
 *  dependent interstitial parameters.					*
 *									*
 *  Original:	MEL	3/88						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
comp_intparam (double temp)
#else

comp_intparam( temp )
float temp;
#endif
{
    double Vt = kb * temp;
    register int mat1, mat2;
    register int wi = which(I);


    /*for all the materials*/
    for(mat1 = 0; mat1 < MAXMAT; mat1++) {

	/*bulk parameters*/
	Di(I,mat1) = D0(I,mat1) * exp( - DE(I,mat1) / Vt );
	Kr(I,mat1) = Kr0(I,mat1) * exp( - KrE(I,mat1) / Vt );
	Cstar(I,mat1) = Cstar0(I,mat1) * exp( - CstarE(I,mat1) / Vt );
	ktrap(I,mat1) = ktrap0(I,mat1) * exp( - ktrapE(I,mat1) / Vt );
	Tfrac(mat1) = Tfrac0(mat1) * exp( - TfracE(mat1) / Vt );
	Dfrac[wi][mat1][CN] = Dfrac0[wi][mat1][CN]*exp(-DfracE[wi][mat1][CN]/Vt);
	Dfrac[wi][mat1][CM] = Dfrac0[wi][mat1][CM]*exp(-DfracE[wi][mat1][CM]/Vt);
	Dfrac[wi][mat1][CDM] = Dfrac0[wi][mat1][CDM]*exp(-DfracE[wi][mat1][CDM]/Vt);
	Dfrac[wi][mat1][CP] = Dfrac0[wi][mat1][CP]*exp(-DfracE[wi][mat1][CP]/Vt);
	Dfrac[wi][mat1][CDP] = Dfrac0[wi][mat1][CDP]*exp(-DfracE[wi][mat1][CDP]/Vt);

	/*interface terms*/
	for(mat2 = 0; mat2 < MAXMAT; mat2++) {

	    /*surface recombination*/
	    Ksurf(I,mat1,mat2) = Ksurf0(I,mat1,mat2) * exp( - KsurfE(I,mat1,mat2) / Vt );
	    Krat(I,mat1,mat2) = Krat0(I,mat1,mat2) * exp( - KratE(I,mat1,mat2) / Vt );
	    Kpow(I,mat1,mat2) = Kpow0(I,mat1,mat2) * exp( - KpowE(I,mat1,mat2) / Vt );

	    /*growth model for injection*/
	    theta(I,mat1,mat2) = theta0(I,mat1,mat2) * exp( - thetaE(I,mat1,mat2) / Vt );
	    Gpow(I,mat1,mat2) = Gpow0(I,mat1,mat2) * exp( - GpowE(I,mat1,mat2) / Vt );
	    
	    /*time dependent injection model*/
	    A(I,mat1,mat2) = A0(I,mat1,mat2) * exp( - AE(I,mat1,mat2) / Vt );
	    t0(I,mat1,mat2) = t00(I,mat1,mat2) * exp( - t0E(I,mat1,mat2) / Vt );
	    Tpow(I,mat1,mat2) = Tpow0(I,mat1,mat2) * exp( - TpowE(I,mat1,mat2) / Vt );
	}
    }
}


#ifdef ANSI_FUNC

float 
form_eval (char *expr, double total, float cord[2])
#else

float form_eval( expr, total, cord )
char *expr;
float total;
float cord[2];
#endif
{
    float val, string_to_real();

    fmacro( "t", total, "%e" );
    fmacro( "x", cord[0]*1.0e4, "%e" );
    fmacro( "y", cord[1]*1.0e4, "%e" );

    val = string_to_real( expr, -1.0 );
    if ( val < 0 ) {
	fprintf(stderr, "problems with the formula %s", expr);
	val = 0;
    }

    umacro( "t" );
    umacro( "x" );
    umacro( "y" );
    return( val );
}
