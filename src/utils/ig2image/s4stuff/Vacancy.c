

static char SccsID[] = "@(#)Vacancy.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   Vacancy.c                Version 3.18     */
/*   Last Modification : 12/1/88 20:12:37 */

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

extern float form_eval();

/************************************************************************
 *									*
 *	This file contains definitions of the Vacancy routines. It 	*
 *  includes the routine to calculate diffusion coefficients, the one	*
 *  to calculate any coupling terms between species, and the one to	*
 *  to calculate the boundary condition terms for the Vacancies.	*
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
 *	comp_vacparam() - This routine computes the temperature 	*
 *  dependent vacancy parameters.					*
 *									*
 *  Original:	MEL	3/88						*
 *									*
 ************************************************************************/
comp_vacparam( temp )
float temp;
{
    double Vt = kb * temp;
    register int mat1, mat2;
    register int wi = which(V);


    /*for all the materials*/
    for(mat1 = 0; mat1 < MAXMAT; mat1++) {

	/*bulk parameters*/
	Di(V,mat1) = D0(V,mat1) * exp( - DE(V,mat1) / Vt );
	Kr(V,mat1) = Kr0(V,mat1) * exp( - KrE(V,mat1) / Vt );
	Cstar(V,mat1) = Cstar0(V,mat1) * exp( - CstarE(V,mat1) / Vt );
	ktrap(V,mat1) = ktrap0(V,mat1) * exp( - ktrapE(V,mat1) / Vt );
	Dfrac[wi][mat1][CN] = Dfrac0[wi][mat1][CN]*exp(-DfracE[wi][mat1][CN]/Vt);
	Dfrac[wi][mat1][CM] = Dfrac0[wi][mat1][CM]*exp(-DfracE[wi][mat1][CM]/Vt);
	Dfrac[wi][mat1][CDM] = Dfrac0[wi][mat1][CDM]*exp(-DfracE[wi][mat1][CDM]/Vt);
	Dfrac[wi][mat1][CP] = Dfrac0[wi][mat1][CP]*exp(-DfracE[wi][mat1][CP]/Vt);
	Dfrac[wi][mat1][CDP] = Dfrac0[wi][mat1][CDP]*exp(-DfracE[wi][mat1][CDP]/Vt);

	/*interface terms*/
	for(mat2 = 0; mat2 < MAXMAT; mat2++) {

	    /*surface recombination*/
	    Ksurf(V,mat1,mat2) = Ksurf0(V,mat1,mat2) * exp( - KsurfE(V,mat1,mat2) / Vt );
	    Krat(V,mat1,mat2) = Krat0(V,mat1,mat2) * exp( - KratE(V,mat1,mat2) / Vt );
	    Kpow(V,mat1,mat2) = Kpow0(V,mat1,mat2) * exp( - KpowE(V,mat1,mat2) / Vt );

	    /*growth model for injection*/
	    theta(V,mat1,mat2) = theta0(V,mat1,mat2) * exp( - thetaE(V,mat1,mat2) / Vt );
	    Gpow(V,mat1,mat2) = Gpow0(V,mat1,mat2) * exp( - GpowE(V,mat1,mat2) / Vt );
	    
	    /*time dependent injection model*/
	    A(V,mat1,mat2) = A0(V,mat1,mat2) * exp( - AE(V,mat1,mat2) / Vt );
	    t0(V,mat1,mat2) = t00(V,mat1,mat2) * exp( - t0E(V,mat1,mat2) / Vt );
	    Tpow(V,mat1,mat2) = Tpow0(V,mat1,mat2) * exp( - TpowE(V,mat1,mat2) / Vt );
	}
    }
}


