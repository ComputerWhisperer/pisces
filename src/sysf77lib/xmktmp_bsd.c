/*cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 *
 *    Copyright c 1988 The Board of Trustees of the Leland Stanford
 *    Junior University. All rights reserved.  This routine may not
 *    be used without the prior written consent of Stanford University.
 *
 *ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 */

#include <string.h>
#include <stdio.h>

#define DEF_KEY		"xmktmp"
#define TEMPLATE	"/tmp/%s.XXXXXX"

#define TEMPSIZE	256


/*=======================================================================
 * "XMKTMP": Return a unique file name.
 *
 *  Usage:
 *     character*(BUFLEN) str
 *     character*(5) progname
 *     data progname/'pisc2'/
 *
 *     call xmktmp(str, LEN(str), progname, LEN(progname))
 *
 *  Notes:
 *     + Most of the temp file name format is determined here. The user
 *	 program need not know what form we are using, but should
 *	 supply and identifier that my optionally be used in
 *	 making the unique name.
 *
 *  Original: Michael Eldredge -- Stanford University (may 88)
 *  Modified: Michael Eldredge -- Stanford University (aug 89)
 *	Change calling sequence to un-PISCES the template.
 */

/* ----------------------------------------------------------------------- */

/* FORTAN callable mktmp(): */
xmktmp_(str, len, key, keylen)
	char	str[] ;
	int*	len ;
	char*	key ;
	int*	keylen ;
	{

	char*	p ;
	char*	s ;
	char	buf[TEMPSIZE] ;
	char	lkey[TEMPSIZE] ;
	int	i ;

	strncpy(lkey, key, *keylen) ;
	lkey[*keylen] = '\0' ;

	(void)sprintf(buf,TEMPLATE,lkey) ;

	(void)mktemp(buf) ;

	for (p = &str[*len] ; p != str; ) *--p = ' ' ;	/* clear */

	for (i = 0, p = buf, s = str ; *p && i < *len ; ) *s++ = *p++ ;
	}
