/* ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 *
 *    Copyright c 1988 The Board of Trustees of the Leland Stanford
 *    Junior University. All rights reserved.  This routine may not
 *    be used without the prior written consent of Stanford University.
 *
 * ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 */

/* sgi Fortran callable C files need this comment (and the one
 * at the end):
 */
/*CENTRY*/

/* =====================================================================
 * "XGETEN": Get environment variables from the UNIX environment.
 *
 *  If you can't getenv() then just use xgeten_def.f
 *
 *  Usage:
 *     call xgetenv(key, val)
 *     if (val(1:1).ne.' ') then
 *         -- we match a value in the environment --
 *     else
 *         -- no match --
 *     endif
 *
 *  Original: Michael Eldredge -- Stanford University (may 88)
 *
 * -----------------------------------------------------------------------
 */

/*CENTRY*/
void
xgeten(key, val)
	char	key[] ;
	char	val[] ;
	{
	int*	_stack_ = (int*)&val ;
	int	keyLEN = _stack_[1] ;
	int	valLEN = _stack_[1] ;
	int	i ;
	char	tmpkey[512] ;
	char*	p ;
	char*	s ;
	char*	getenv() ;

	for (i = 0 ; i < valLEN; ) val[i++] = ' ' ;

	for (p = key, s = tmpkey, i = 0 ; i < keyLEN; i++) {
		if (*p == ' ') break ;
		*s++ = *p++ ;
		}
	*s = '\0' ;

	if ((p = getenv(tmpkey))) {
		for (i = 0, s = val; *s && i < valLEN; )
			*s++ = *p++ ;
		}

	}

/*ENDCENTRY*/
