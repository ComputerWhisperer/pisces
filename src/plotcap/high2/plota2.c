/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 17 may 84 (mje)
 */

#include "gplot.h"


/*
 *ff
 * "plota" : Plot with the pen up or down to the point (x,y).
 *
 * calling sequence:
 *	#include <gplot.h>
 *
 *	plota(x, y, pen)
 *
 * where:
 *	x	- (float) X value (in inches, say) to which to plot.
 *	y	- (float) Y value to which to plot.
 *	pen	- (int) Indication of pen possition (up or down).
 *		= G_DRAW :: pen down (draw a line to (x,y) ).
 *		= G_MOVE :: pen up (no draw, just move to (x,y) ).
 *
 * Notes:
 *of
 * written: Michael Eldredge (apr 84)
 */
plota(x, y, pen)
	float   x, y;
	int     pen;
	{

	switch (pen) {
	case G_DRAW:
	case G_MOVE:
		gplot(x, y, pen);  /* MOVE or DRAW */
		break;
		}
	}
