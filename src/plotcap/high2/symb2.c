/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 17 dec 85 (mje)
 *ff
 * "symb2" : symbol drawing routine. Will draw one or more characters
 *	from a given starting point at a given angle and size. Or will
 *	draw a sigle centered symbol (for marking plot points).
 *	NOTE: This routine is obviated by symbl2().
 *
 * calling sequence:
 *	symb2(x0, y0, cbuf, cnt, size, angle, hornt, wornt)
 *
 * where:
 *	x0	- (float) Starting point in x.
 *	y0	- (float) Starting point in y.
 *	cbuf	- (char []) String of characters to draw.
 *	cnt	- (int) Number of character is 'cbuf'
 *	size	- (float) Size in plot units (ie: inches) for each characer.
 *	angle	- (float) Angle (degrees) at which to draw the string of
 *		  characters (from x,y to the horizontally to the right
 *		  is 0 degrees).
 *	hornt	- (float) Height orientation of character string.
 *		  Range: 0 <= hornt <= 1.0
 *		  0.0 : x,y is bottom of symbol string.
 *		  1.0 : x,y is top of symbol string.
 *	wornt   - (float) Width orientation of character string.
 *		  Range: 0 <= wornt <= 1.0
 *		  0.0 : x,y is left of symbol string.
 *		  1.0 : x,y is right of symbol string.
 *
 * notes:
 *	> THIS ROUTINE IS OBVIATED BY symbl2(), IN FACT IT SIMPLY CALLS
 *	  symbl2().
 *
 * see also:
 *	mksym(1), gplot(3), plotcap(5)
 *of
 * written: Michael Eldredge (nov 83)
 * mod # 1: Michael Eldredge (dec 85)
 *	Simply calls symbl2() now giving size to both hsize and wsize.
 */

/* "symb2": Draw the given character buffer.*/
symb2(x0,y0, cbuf, cnt, size, angle, hornt, wornt)
	float  x0,y0, size, angle;
	char   cbuf[];
	int    cnt;
	float  hornt, wornt;
	{

	symbl2(x0,y0, cbuf, cnt, size, size, angle, hornt, wornt) ;
	}
