/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*
 * Tue Jan 30 15:52:09 PST 1990 (dredge--stanford)
 * 
 * "dopnt" : see if this is an X or Y point. When we have both plot it.
 *
 * written:  Michael Eldredge (sep 84)
 * modified: MJE (mar 87)
 *	+++ added absolute value stuff.
 *	+++ log data before playing with it.
 * modified: mje (jan 90) the code that converts from data coords to
 *	screen coords is now in the function fixpnt() since it is
 *	used in other places.
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


dopnt(rval)
	float rval;
	{
	
	/* this is X or Y or a value in an unused column */
	colcnt++ ;
	if (colcnt == colX) 
		{ newX = rval;  gotX = T; }
	if (colcnt == colY) 
		{ newY = rval;  gotY = T; }

	if (colcnt >= ncols) colcnt = 0; 

	/* if we have a new X and new Y, then plot them */
	if (gotX && gotY) {

		if (NewPlot) { fxphys(); NewPlot = F; }

		/* since $abs, $addto, $mulby commands are logged, better
		 *	log the data before adjusting the values
		 */
		logit_dat(newX, newY);

		x1 = fixpnt('x', newX) ;
		y1 = fixpnt('y', newY) ;

		if (by_how & BY_LINE) plota(x1, y1, pen);
		else                  plota(x1, y1, G_MOVE);

		pen = G_DRAW;

		if (by_how & BY_SYMB) {
			if (lin_typ != 1) gnline(1);
			if (pen_symb != pen_data) gnpen(pen_symb) ;

			symbl2(x1, y1, s_sym, -1, s_siz, s_siz, 0.0, .5, .5);

			if (pen_symb != pen_data) gnpen(pen_data) ;
			if (lin_typ != 1) gnline(lin_typ);
			}

		x0 = x1;
		y0 = y1;

		gotX = gotY = F;
		pnt_count++ ;
		}
	}
