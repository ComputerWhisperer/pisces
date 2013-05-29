/*	newget_solval.c		Version 1.8		*/
/*	Last Modification:	3/31/90 09:36:22		*/

/*************************************************************************
 *									 *
 *     Copyright c 1989 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/



#include <stdio.h>
#include <math.h>
extern double erf(), erfc();
#include <ctype.h>
#include "global.h"
#include "constant.h"
#include "geom.h"
#include "expr.h"
#include "material.h"
#include "impurity.h"
#include "more_imp.h"
#include "diffuse.h"


/************************************************************************
 *									*
 *  newget_solval( val, type, takeabs)	             			*
 *  This routine sets   						*
 *  up a vector array based on the value contained in the type		*
 *  parameter.								*
 *									*
 *  Original:	MEL	8/85						*
 *  Modified:   GC      4/89  -  allow net chemical  concentration to   *
 *				  be assigned to val			*
 *				 add electron and hole concentrations   *
 * 				 add net carrier concentration          *
 *		RH      4/89 - added noprompt and takeabs flags so that *
 * 				the user isn't asked every frame if     *
 * 				absolute value is to be taken		*
 *									*
 ************************************************************************/
newget_solval( val, type, takeabs)
float *val;
int type;
int takeabs;
{
    int index;
    register int i, j;
    double *sol[MAXIMP], *act[MAXIMP], *equ[MAXIMP], *foo[MAXIMP];
    double net[MAXPNT];
    char line[80];
    char absolute;

    /* set up a temperature */
    last_temp = 25.0;

    /*two main cases here, the first is an impurity*/
    switch( type ) {
    case As   :
    case P    :
    case Sb   :
    case B    :
    case ELE  :
    case HOL  :
    case XVEL :
    case YVEL :
    case T    :
    case Au   :
    case Sxx  :
    case Syy  :
    case Sxy  :
    case Cs   :
    case ELECUR :
    case HOLCUR :
    case TOTCUR:
    case DLT:
	index  = imptosol[ type ];
	if ( index == -1 ) return(1);
	for(i = 0; i < nn; i++)   {
	    val[ i ] = nd[ i ]->sol[index];
	}
	return( 0 );
        break;
    
    case OXY  :
	index = imptosol[ (imptosol[O2] != -1)? O2 : H2O ];
	if ( index == -1 ) return(1); 
	for(i = 0; i < nn; i++)  {
	    val[ i ] = nd[ i ]->sol[index];
	}
	return(0);
        break;
    
    case NETCHEM : 
	for (i = As; i <= B; i++)  {
	    index  = imptosol[i];
	    if (index != -1) {
	        switch (i)  {
		    case As:
		    case P:
		    case Sb:  
			for (j = 0; j < nn; j++)
			   val[j] -= (float)(nd[j]->sol[index]);
			break;

		    case B:   
			for (j = 0; j < nn; j++)
			   val[j] += (float)(nd[j]->sol[index]);
			break;
	       } /* end switch */
	    } /* end if (index != -1) */
	} /* end for i */
        index = imptosol[Cs];
	if (index != -1)  {
	    for (j = 0; j < nn; j++)
		val[j] += (float)nd[j]->sol[index];
	} /* if (index != -1) */
        break;
    case NETCARRIERS  :
	for (i = ELE; i <= HOL; i++)  {
	    index  = imptosol[i];
	    if (index != -1) {
	        switch (i)  {
		    case ELE:
			for (j = 0; j < nn; j++)
			   val[j] -= (float)(nd[j]->sol[index]);
			break;

		    case HOL:   
			for (j = 0; j < nn; j++)
			   val[j] += (float)(nd[j]->sol[index]);
			break;
	       } /* end switch */
	    } /* end if (index != -1) */
	} /* end for i */
        break;

    case I    :
    case V    :
    case Psi  :
    case ELEC :
    case CIS  :
    case CVS  :
    case NETACT :
	if (last_temp == 0.0) 
	    return(1);

	/*first allocate and initialize the arrays*/
	init_pseudo( last_temp );
	comp_mat( last_temp );
	for(j = 0; j < n_imp; j++) {
	    sol[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    act[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    foo[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    equ[j] =(double *)malloc( sizeof(double) * (nn + 1) );
	    for(i = 0; i < nn; i++) sol[j][i] = nd[i]->sol[j];
	}

	/*compute the active concentration at each node*/
	get_active( TRUE, last_temp, sol, net, foo, act, equ, foo );
	get_defaults(equ);

	/*get we want exactly*/
	switch( type ) {

	case I    : for(i = 0; i < nn; i++) val[i] = nd[i]->sol[imptosol[I]];
		    break;
	case V    : for(i = 0; i < nn; i++) val[i] = nd[i]->sol[imptosol[V]];
		    break;
	case Psi  : for(i = 0; i < nn; i++) val[i] = nd[i]->sol[imptosol[Psi]];
		    break;
	case ELEC : for(i = 0; i < nn; i++) val[i] = noni[i] * Ni(nd[i]->mater);
		    break;
	case CIS  : for(i = 0; i < nn; i++) val[i] = equ[imptosol[I]][i];
		    break;
	case CVS  : for(i = 0; i < nn; i++) val[i] = equ[imptosol[V]][i];
		    break;
	case NETACT  : for(i = 0; i < nn; i++) val[i] = (float)net[i];
		    break;
	} /* end switch (type) */

	/*free stuff up*/
	for(j = 0; j < n_imp; j++) {
	    free( sol[j] );
	    free( act[j] );
	    free( foo[j] );
	    free( equ[j] );
	}

 
	break;

    case X    :
	for( i = 0; i < nn; i++ ) val[i] = pt[ nd[i]->pt ]->cord[0];
	break;

    case Y    :
	for( i = 0; i < nn; i++ ) val[i] = pt[ nd[i]->pt ]->cord[1];
	break;
    
    case ELI  :
	if (last_temp == 0.0) 
	    return(1);
	
	for(i = 0; i < nn; i++) val[i] = Ni(nd[i]->mater);
	break;

    default :
	for(i = 0; i < nn; i++) val[i] = 0.0;
	break;
    }


    if (takeabs) {
	for (i = 0; i < nn; i++)
	    val[i] = (float)fabs((double)val[i]);
    }

    return( 0 );
}
