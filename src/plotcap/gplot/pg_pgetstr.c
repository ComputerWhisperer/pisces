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
 * Wed Sep 20 12:06:57 PDT 1989 (dredge--stanford)
 *ff
 * "pgetstr" : return the charater string associated with the name capability.
 *
 * calling sequence:
 *	<i> = pgetstr(id, area)
 *
 * where:
 *	id   	- (char *) Null terminated string containing the name of the
 *		capability for which to search the entry. 
 *	area    - (char *) User buffer to which to return the string.
 *
 * returns:
 *	<i>	- (int) Return code of the  routine.
 *		>= 0 : OK. String value found and copied to 'area'.
 *			Return value is number of characters added to 'area'.
 *		= -1 : Given 'id' not found in entry.
 *		= -2 : 'id' found, but no string value given.
 *
 * notes:
 *	1. No check is made for overflow of buffer pointed to by 'area'.
 *	2. 'pgetstr' converts certain escape characters as it goes. These
 *    	   include:
 *		\E   to 033 (escape)
 *		\n \f \r \t as in C.
 *		\nnn where 'nnn' is  a three character octal number.
 *		^x   where 'x' is any character.
 *		\\   to \   (two become just one).
 *	3. There are two possible versions of this routine, a terse version
 *		and a verbose version (compiled with VERBOSE defined).  The
 *		verbose version prints error messages similar to the above
 *		on the 'stderr' and returns the error code; while the terse
 *		form simply returns the error code.
 *of
 * written: Michael Eldredge (dec 83) 
 * modified: MJE (sep 89) changed calling sequence to entok()
 */

#include "auxfns.h"
#include "pg_def.h"

#define isoctal(CH) (CH >= '0' && CH <= '7')

int
pgetstr(id, area)
	char *id, *area;
	{

	char	*np, *ap = area ;
	int	 n ;
	char	 buf[132] ;
	char	*pg_fndid();

	/* start */
	defrot("pgetstr");
	*area = '\0';      /* incase error */

	if (! (np=pg_fndid(id))) return(-1);   /* cap not found */
		
	if (*np++ != '=')   {                 /* no value given */
		pg_er(E_NVAL, id,0,0);	/* no value given */ 
		return(-2);
		}

	pg_init(np) ;		/* Init parser */

	while ((n = entok(buf,sizeof(buf))) != GT_MRK) {
		/* cat strings? */
		if (n == ';' || n == ',') continue ;	/* FIX THIS */
		ap += pg_cvtstr(ap, buf) ;
		}
	
	if (n == GT_OVR) return pg_er(E_OVR,buf,0,0) ;

	return((int)(ap - area)) ;
	}


/* NOTE: called from pgetprg() also, so it can't be static */
/* "pg_cvtstr": copy string interp'ing escape chars and embedded octal
 *	constants ("\032" -> ' ')
 */
int
pg_cvtstr(dp, sp)
	char	*dp ;
	/*register*/ char	*sp ;
	{

	register int  c ;
	int	 i , n ;
	char	*d1 = dp ;

	while ((c = *sp++)) {
			
		if (c == GT_ESC) {	/* Escaped char */
			if (! *sp ) continue ;

			i = 0 ;
			for (i = 0; isoctal(sp[i]) && i < 3; )
				i++ ;

			/* a 1 to 3 digit octal constant? */
			if (i > 0) {
				if (i == 3 && *sp > '3') --i ;
				n = 0 ;
				while (i--) n = n * 8 + (*sp++ - '0') ;
				c = (char)n ;
				}

			else  {
				switch ((c = *sp++)) {
				case 'E':	c = 033 ;	break ;
				case 'r':	c = '\r';	break ;
				case 'n':	c = '\n';	break ;
				case 'b':	c = '\b';	break ;
				case 't':	c = '\t';	break ;
				case 'f':	c = '\f';	break ;
					}
				}
			}

		/* A control characer */
		else if (c == '^') {
			if ( *sp &&
			     (((c = *sp++) == '@')	||
			      ( c >= 'a' && c <= 'z')	||
			      ( c >= 'A' && c <= 'Z') ) )  c &= 037 ;
			}

		*dp++ = c ;
		}

	/* remember length of string found */ 
	return( (int)(dp - d1) ); 
	}
