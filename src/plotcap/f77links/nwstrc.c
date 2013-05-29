/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* NOTE: THIS IS DEFINED TO BE STATIC AND IS #INCLUDE'D IN FILES THAT
 *  WANT TO USE IT.  IT IS PRETTY SMALL, SO A COUPLE OF COPIES WON'T HURT
 *  -- THEN THERE IS NO WORRY ABOUT NAME CONFLICTS...
 */
 
/* date: 19 nov 85 (mje)
 * "nwstrc" : Make a New string and copy into it (to be free'd later).
 *	Since all the f77 cover functions to this, let's make it a function
 *	for all to use.  Input is the string pointer from f77 and its f77
 *	declared length.  Output is a pointer to a null terminated, trailing
 *	blanks removed string that has been malloc()'ed up.
 */
#include <stdlib.h>
#include <string.h>

static char *
nwstrc(char *fstr, long fsiz)
	{

	char	*bp ;
	int	 l ;

	bp = malloc(fsiz + 1) ;
	strncpy(bp, fstr, (int)fsiz) ;
	for (l = fsiz -1; l >= 0; l--)
		if (bp[l] != ' ') break ;
	bp[l+1] = '\0' ;

	return bp ;
	}
