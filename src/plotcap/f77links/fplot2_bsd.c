/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 19 nov 85 (mje)
 * gplot2: general plotting routine (device 'somewhat' independent) plot
 *	functions.
 *
 * calling sequence:
 *	call gplot2(cmd, sub, xval, yval)
 *
 * where:
 *	cmd	- (integer) gplot command argument.
 *	sub	- (integer) possible sub-command.
 *	xval	- (real)    Real value usually associated with the x axis.
 *	yval	- (real)    Real value usually associated with the y axis.
 *
 * NOTE: For full documentation on the use of 'gplot2' see 'gplot2.c'.
 *
 * written: Michael Eldredge  (may 84)
 */

fplot2_(cmd, sub, xval, yval)  /* f77 callable 'gplot2' */
	long  *cmd,  *sub;
	float *xval, *yval;
	{
	gplot2((int)*cmd, (int)*sub, *xval, *yval);
	}
