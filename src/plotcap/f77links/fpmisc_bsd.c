/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 30 jan 86 (mje)
 * gpmisc: misc gplot functions -- f77 entry point.
 *
 * calling sequence:
 *	integer function gpmisc(cmd, sub, iv, fv, str)
 *
 * where:
 *	cmd	- (integer) gpmisc command argument.
 *	sub	- (integer) possible sub-command.
 *	iv	- (integer(*)) integer array.
 *	fv	- (real(*)) real array.S
 *	str	- (char*)   string.
 *
 * written: Michael Eldredge  (ded 86)
 */

long
fpmisc_(cmd, sub, iv, fv, str, strLEN)  /* f77 callable 'gpmisc' */
	long  *cmd,  *sub;
	long   iv[] ;
	float  fv[] ;
	char*  str ;
	long   strLEN ;	/* created by the f77 compiler */
	{
	long	iret ;	

	iret = gpmisc((int)*cmd, (int)*sub, iv, fv, str) ;

	return iret ;
	}
