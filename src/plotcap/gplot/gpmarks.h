/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 16 mar 87 (dredge)
 *
 * "gpmarks.h":  List of ``registered'' G_MARK marks.
 *
 * notes:
 *	+++ all marks must be greater than zero.
 *	+++ User marks (non-registered) should be large (e.g. > 100) to
 *	    leave room for future registered marks.
 */

/* Standard/registered defines -- some general graphic entities */
#define GM_AXIS		1	/* some single axis */
#define GM_STRING	2	/* A string (say from symbl2() */
#define GM_CURVE	3	/* some data list */
#define GM_ARC		4
#define	GM_POLY		5	/* some polygon -- general segment */
#define	GM_THING	6	/* whatever */
#define GM_SYMB		7	/* a data marking symbol (box,X,star,etc) */


