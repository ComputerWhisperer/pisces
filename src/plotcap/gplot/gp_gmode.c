/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 19 aug 1986 (mje)
 *
 * "gp_gmode" : Change mode (context switch) from graphics to alpha or what-
 *	ever.
 *
 * written:  Michael Eldredge (mar 85)
 * modified: MJE (aug 86) GTOA will always do the post, even if there is no
 *	GTOA sequence. Thus it can truely replace the POST call.
 */

#include "auxfns.h"
#include "al_alu18.h"
#include "gp_mis12.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"


gp_gmode(new_mode)
	int	 new_mode ;
	{

	/* don't do it twice */
	if (new_mode == g_agmode) return ;

	switch (new_mode) {
	case MODE_G:
		docmd(C.ATOG);		/* back to graphics */
		break ;

	case MODE_A:
		docmd(C.GTOA) ;		/* into alpha */
		docmd(ALU_POST) ;	/* make sure cmd gets out */
		break ;

	default:			/* This is an ERROR */
		new_mode = g_agmode ;
		break ;
		}

	g_agmode = new_mode ;
	}
