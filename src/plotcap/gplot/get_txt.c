/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* 09 feb 87 (mje)
 *
 * "get_txt" : look in the textual 'plotcap' file for the plot device
 *	given by 'devnam' and set up 'gplot' common according to the
 *	entry.
 *
 * returns:
 *	= 0 : DONE, OK.
 *	=-1 : capfile not found.
 *	=-3 : device not found.
 *	< 0 : error from 'pgetent'.
 *
 */

#include "auxfns.h"
#include "gp_mis12.h"
#include "gp_def.h"
#include "gp_com.h"


int
get_txt(capfil, devnam)
	char  *capfil, *devnam;
	{
	int  ierr;

	if ((ierr=pgetent(capfil, devnam)) == 0) {
		g_prog = alu_mkp(MAXPROG) ;
		g_idat = alu_mkb(MKB_IDAT, MAXIDAT) ;
		ierr  = gp_setcap();              /* pick up the parameters */
		}
	
	return(ierr);
	}

#ifdef SET_BIN
int 
get_bin(capfil, devnam)
	char *capfil, *devnam;
	{
#ifdef lint
	if (capfil == devnam) return 0 ;
#endif
	/* fprintf(stderr,"Can't GET From BIN files yet\n") ; */
	return(-1) ;
	}
#endif /*SET_BIN*/

