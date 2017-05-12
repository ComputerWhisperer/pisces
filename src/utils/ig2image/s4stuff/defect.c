

static char SccsID[] = "@(#)defect.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   defect.c                Version 3.12     */
/*   Last Modification : 12/1/88 20:14:21 */

#include <stdio.h>
#include <math.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "impurity.h"
#include "defect.h"
#include "material.h"
#include "matrix.h"
#include "diffuse.h"


static int vadd = 0, iadd = 0, tadd = 0;

/************************************************************************
 *									*
 *	defect() - This routine sets up the defect card and its params.	*
 *									*
 *  Original:	MEL	11/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
init_pseudo (double temp)
#else

init_pseudo( temp )
double temp;
#endif
{
    register int i, s;

    /*compute the temperature dependent interstitial and vacancy parameters*/
    comp_intparam( temp );
    comp_vacparam( temp );

    /*test for vacancies*/
    if (imptosol[V] == -1) {
	add_impurity( V, 0.0, -1 );
	s = imptosol[V];
	for(i = 0; i < nn; i++) nd[i]->sol[s] = Cstar(V, nd[i]->mater);
	vadd = TRUE;
    }

    /*test to see if interstitials are already included*/
    if (imptosol[I] == -1) {
	add_impurity( I, 0.0, -1 );
	s = imptosol[I];
	for(i = 0; i < nn; i++) nd[i]->sol[s] = Cstar(I, nd[i]->mater);
	iadd = TRUE;
    }

    /*set up traps if need be*/
    if ( (imptosol[T] == -1) && trap_on ) {
	add_impurity( T, 0.0, -1 );
	s = imptosol[T];
	for(i = 0; i < nn; i++) nd[i]->sol[s] = Tfrac(nd[i]->mater) * Ttot(nd[i]->mater);
	tadd = TRUE;
    }

    /*set up the potential*/
    if ( imptosol[ Psi ] == -1 )  {
	add_impurity( Psi, 0.0, -1 );
	s = imptosol[Psi];
	for(i = 0; i < nn; i++) nd[i]->sol[s] = 0.0;
    }
}


/************************************************************************
 *									*
 *	get_defaults - This routine assigns the needed values their 	*
 * equilibrium value as an initial condition.				*
 *									*
 * Original:	MEL	5/88						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
get_defaults (int imp)
#else

get_defaults(imp)
int imp;
#endif
{
    int ret;

    if ( imp == I ) {
	ret = iadd;
	iadd = FALSE;
    }
    if ( imp == V ) {
	ret = vadd;
	vadd = FALSE;
    }

    return( ret );
	
}





/************************************************************************
 *									*
 *	get_Cstar( c ) - This routine calculates the equilibrium conc	*
 *  of interstitials as a function of n/ni.				*
 *									*
 *  Original:	MEL	11/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
get_Cstar (int imp, int nv, double temp, double *noni, double *equil, double *dequ)
#else

get_Cstar( imp, nv, temp, noni, equil, dequ )
int imp;
int nv;
float temp;
double *noni;
double *equil;
double *dequ;
#endif
{
    register int i, mat;
    register int f = which(imp);
    double num, den, c;
    double Vt = kb * temp;

    /*$dir no_recurrence*/
    for( i = 0; i < nv; i++ ) {
	mat = nd[i]->mater;
	c = noni[i];
	den  = Dfrac[f][mat][CN] + Dfrac[f][mat][CM] + Dfrac[f][mat][CDM] +
	       Dfrac[f][mat][CP] + Dfrac[f][mat][CDP];

	/*compute the concentration of these guys*/
	num  = c * ( Dfrac[f][mat][CM] + c * Dfrac[f][mat][CDM]) + 
		   ( Dfrac[f][mat][CP] + Dfrac[f][mat][CDP] / c ) / c +
		    Dfrac[f][mat][CN];
	equil[i] = Cstar(f,mat) * num / den;

	/*compute the derivative w/r to c*/
	num = (Dfrac[f][mat][CM] + 2*c*Dfrac[f][mat][CDM]) - 
	      ( Dfrac[f][mat][CP] + 2*Dfrac[f][mat][CDP] / c ) / (c*c);

	dequ[i] = Cstar(f,mat) * c * num / (den * Vt);
    }
}




