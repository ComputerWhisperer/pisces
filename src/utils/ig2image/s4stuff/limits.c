

static char SccsID[] = "@(#)limits.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   limits.c                Version 3.1     */
/*   Last Modification : 9/14/87  10:39:32 */

#include <stdio.h>
#include "global.h"
#include "constant.h"
#include "geom.h"


/************************************************************************
 *									*
 *	dev_lmts(dxmin, dxmax, dymin, dymax) - this routine calculates  *
 *  the limits of the device.						*
 *									*
 *    Original : CSR           Stanford University        Nov, 1983 	*
 *    Revision : MEL	       (C translation)		  Oct, 1984	*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
dev_lmts (float *dxmin, float *dxmax, float *dymin, float *dymax)
#else

dev_lmts(dxmin, dxmax, dymin, dymax)
float *dxmin, *dxmax, *dymin, *dymax;
#endif
{
    register float *val;
    register struct pt_str **p;

    /*initialize max and mins to first values**/
    *dxmin = *dxmax = pt[0]->cord[0];
    *dymin = *dymax = pt[0]->cord[1];

    /*loop through all the points*/
    for(p = pt; *p != NULL; p++) {
	val = p[0]->cord;
	*dxmin = (*dxmin < *val) ? *dxmin : *val;
	*dxmax = (*dxmax > *val) ? *dxmax : *val;
	val++;	/*advance to y coordinate*/
	*dymin = (*dymin < *val) ? *dymin : *val;
	*dymax = (*dymax > *val) ? *dymax : *val;
    }
}


