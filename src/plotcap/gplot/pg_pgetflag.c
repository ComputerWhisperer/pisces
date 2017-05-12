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
 *ff
 * "pgetflag" : return true or false if the given capability is in entry.
 *
 * calling sequence:
 *	<i> = pgetflag(id)
 *
 * where:
 *	id   	- (char *) Null terminated string containing the name of the
 *		capability for which to search the entry. 
 *
 * returns:
 *	<i>	- (int) Return code of the  routine.
 *		=  0 : FALSE return. 'id' not given in entry.
 *		=  1 : TRUE  return. 'id' was given in entry.
 *
 * notes:
 *	1. There are no error returns from this routine, it was either there
 *		or not.
 *of
 * written: Michael Eldredge (dec 83) 
 * mod # 1: Michael Eldredge (nov 85)
 *	Use new pget routines.
 */

#ifdef ANSI_FUNC

int 
pgetflag (char *id)
#else

int
pgetflag(id)
	char *id;
#endif
	{

	char *pg_fndid();

	/* start */
	if (pg_fndid(id)) return(1);
	else		  return(0);
	}
