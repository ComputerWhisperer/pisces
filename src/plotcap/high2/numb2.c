/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Date: 18 jul 85 (mje)
 *ff
 * numb2: routine that writes a number in graphics mode
 *	 
 * calling sequence:
 *	len = numb2(x0, y0, val, size, ang, hornt, wornt, form);
 *
 * parms:
 *	x0 -- (float) x-coordinate of where number is to start
 *	y0 -- (float) y-coordinate of where number is to start
 *	val-- (float) floating point number to be output
 *	size -- (float) maximum number of character positions to be
 *		filled in writing the number
 *	ang-- (float) angle at which the number is to be written
 *	hornt - (float) heigth orientation.
 *	wornt - (float) width orientation.
 *	form  - (char []) C-type format to use (w/o the `%'). "%d" format
 *		should not be used.  use "%g" to get the same effect.
 * return value:
 *      numb2 returns the length of the string written.
 *
 *of
 * Author: Dan Lopez (feb 84)
 * Mod #1: Michael Eldredge (may 84)
 *	Fixed to use the new 'symb2' routine (wornt/hornt).
 * Mod #2: Michael Eldredge (jul 85)
 *	Allow formats to begin with '%' or not.
 */
#include <string.h>
#include <stdio.h>

int
numb2(x0, y0, fpval, size, angle, hornt, wornt, form)
	float x0, y0, fpval, size, angle, hornt, wornt;
	char *form;
	{

	char obuf[80], vform[20], *fmt ;
	int len;

	fmt = form ;
	if (*form != '%') {
		vform[0] = '%';
		strcpy(vform + 1, form);
		fmt = vform ;
		}

	sprintf(obuf, fmt, fpval);		/* format it */

	len = strlen(obuf);
	symb2(x0 , y0, obuf, len, size, angle, hornt, wornt);

	return(len);
	}
