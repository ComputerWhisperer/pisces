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
 * Wed Sep 20 12:04:22 PDT 1989 (dredge--stanford)
 *ff
 * "pgetnum" : return the number associated with the name capability ('id').
 *
 * calling sequence:
 *	<i> = pgetnum(id, nval)
 *
 * where:
 *	id   	- (char *) Null terminated string containing the name of the
 *		capability for which to search the entry. 
 *	nval    - (float  *) pointer to a float where, if the 'id' is found,
 *		the numeric value associated with it is returned.
 *
 * returns:
 *	<i>	- (int) Return code of the  routine.
 *		=  0 : OK. Numeric value found, converted and placed in '*nval'.
 *		= -1 : Given 'id' not found in entry.
 *		= -2 : 'id' found, but no numeric value given.
 *		= -3 : 'id' found, but bad numeric value given. (ie: associated
 *			value was not a number.
 *
 * notes:
 *	1. '*nval' is of type 'float', but integer values are interpreted
 *		and then converted to floating point.  Thus values could
 *		be any of:
 *			0xff   (hex)
 *			0377   (octal)
 *			255    (decimal)
 *			255.0  
 *			2.55e2
 *	    All of the above would return ``255.0'' (decimal) in a 'float'
 *		variable.
 *	2. The conversion is done quick and easily.  This means that no check
 *		is made for extranious characters in the value.  Thus an entry
 *		`` foo=12junk34: '' would return successfully with a numberic
 *		value 12.
 *	3. There are two possible versions of this routine, a terse version
 *		and a verbose version (compiled with VERBOSE defined).  The
 *		verbose version prints error messages similar to the above
 *		on the 'stderr' and returns the error code; while the terse
 *		form simply returns the error code. The verbose version is 
 *		called 'pgetnumV'
 *of
 * written: Michael Eldredge (dec 83) 
 */

#include "auxfns.h"
#include "pg_def.h"


#ifdef ANSI_FUNC

int 
pgetnum (char *id, float *nval)
#else

int
pgetnum(id, nval)
	char  *id;
	float *nval;
#endif
	{

	int	 ival ;
	char	 buf[132] ;
	char	*np ;
	char	*pg_fndid();

	/* start */
	defrot("pgetnum"); 

	*nval = 0;
	/* try to find the entry, see if it has a value. */
	if (!(np=pg_fndid(id)) ) return(-1);  /* cap not found */
		
	if (*np++ != '=')  { 
		(void)pg_er(E_NVAL, id,0,0);	/*no value given */
		return(-2);
		}

	pg_init(np) ;		/* init parser */

	switch (entok(buf,sizeof(buf))) {
	case LXINT:	*nval = (float)atoi(buf) ;	break ;
	case LXOCT:	sscanf(buf, "%o", &ival) ; *nval = (float)ival;	break ;
	case LXHEX:	sscanf(buf, "%x", &ival) ; *nval = (float)ival;	break ;
	case LXREL:	sscanf(buf, "%f",  nval) ;	break ;
	case GT_OVR:
		(void)pg_er(GT_OVR,buf,0,0) ;
		return(-4) ;
		break ;
	default:
		(void)pg_er(E_NUMB, id, 0,0) ;	/* Bad numeric value */
		return(-3) ;
		break ;
		}

	return(0);		/* All was ok.... */
	}
