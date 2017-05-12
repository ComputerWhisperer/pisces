

static char SccsID[] = "@(#)active.c	1.2\t5/3/89";

/*************************************************************************
 *									 *
 *     Copyright c 1987 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   active.c                Version 3.10     */
/*   Last Modification : 11/29/88 16:16:16 */


#include <stdio.h>
#ifndef CONVEX
#include <math.h>
#else
#include <fastmath.h>
#endif
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "impurity.h"
#include "defect.h"
#include "diffuse.h"
#include "material.h"
#include "plot.h"
#include "more_imp.h"

/************************************************************************
 *									*
 *	active() - This routine computes the active concentration and	*
 * the electron concentration at the passed node.			*
 *									*
 * Original:	7/87							*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
get_active (
    int simple,
    double temp,
    double **chem,		/*the solution values to use in calculation*/
    double *net,		/*the net active concentration*/
    double **dnet,		/*partial of the active concentration*/
    double **active,	/*the active substitutional concentrations*/
    double **equil,		/*the equilibrium concentration*/
    double **dequ		/*the equilibrium concentration*/
)
#else

get_active( simple, temp, chem, net, dnet, active, equil, dequ )
int simple;
float temp;
double **chem;		/*the solution values to use in calculation*/
double *net;		/*the net active concentration*/
double **dnet;		/*partial of the active concentration*/
double **active;	/*the active substitutional concentrations*/
double **equil;		/*the equilibrium concentration*/
double **dequ;		/*the equilibrium concentration*/
#endif
{
    register int i, j, imp;
    double Vt = kb * temp;
    double sg;
    int vsol = imptosol[V];
    int isol = imptosol[I];
    int psol = imptosol[Psi];

    /*zero the net doping and calculate the n/ni*/
    for(i = 0; i < nn; i++) {
	net[i] = 0.0;
	noni[i] = exp( chem[psol][i] / Vt );
    }


    /*compute equilibrium values*/
    get_Cstar( I, nn, temp, noni, equil[isol], dequ[isol] );
    get_Cstar( V, nn, temp, noni, equil[vsol], dequ[vsol] );

    for(i = 0; i < nn; i++) {
	if ( IS_PSEUDO ( I ) ) chem[isol][i] = equil[isol][i];
	if ( IS_PSEUDO ( V ) ) chem[vsol][i] = equil[vsol][i];
    }

    /*compute the non clustered concentration of the dopants*/
    for( j = 0; j < n_imp; j++ ) {
	imp = soltoimp[j];
	sg = (imp == B)?(-1.0):(1.0);
	if ( impur[imp].active != NULL )  {
	    impur[imp].active( simple, nn, temp, chem, active, equil, noni );

	    if ( !simple ) {
		if ( imp == As ) {
		    /*$dir no_recurrence*/
		    for(i = 0; i < nn; i++) {
			net[i] += active[j][i];
			dnet[j][i] = dact[j][j][i];
			dnet[vsol][i] += dact[j][vsol][i];
			dnet[psol][i] += dact[j][psol][i];
		    }
		}
		else {
		    /*$dir no_recurrence*/
		    for(i = 0; i < nn; i++) {
			net[i] += sg * active[j][i];
			dnet[j][i] = sg * dact[j][j][i];
		    }
		}
	    }
	    else {
		/*$dir no_recurrence*/
		for(i = 0; i < nn; i++) net[i] += sg * active[j][i];
	    }
	}
    }
}


