/*
 * Fri Feb  2 16:14:29 PST 1990 (dredge--stanford)
 *-----------------------------------------------------------------------
 *   Copyright 1990 by
 *   The Board of Trustees of the Leland Stanford Junior University
 *   All rights reserved.
 *
 *   This routine may not be used without the prior written consent
 *   of the Board of Trustees of the Leland Stanford University.
 *-----------------------------------------------------------------------
 */

/*
 * "rdline": Fortran callable prompt-and-get-line routine.  This
 *	version calls the 'readline()' routine which allows command
 *	line editing (in a KSH-like format).
 *
 * Calling sequence:
 *	call rdline(prompt,plen, buf,blen, ncin)
 *
 * Where:
 *	prompt	(char*)		Prompt string to issue.
 *	plen	(int)		Length of prompt.  (1:plen)
 *	buf	(char[])	Destination buffer.  Read user
 *			input input buf.
 *	blen	(int)		Max size of buf.
 *	ncin	(int*)		Number of chars actually read in Or
 *			ncin = -1 if EOF.
 *
 * Original: Michael Eldredge -- Stanford (jan 90)
 */

/*
 *	SUBROUTINE RDLINE(prompt,plen,buf,blen,ncin)
 *	character*(*) prompt
 *	integer       plen
 *	character*(*) buf
 *	integer       blen
 *	integer       ncin
 */


#include <string.h>

char*	read_line() ;		/* interface to gnu readline() call */


/* "rdline": */
rdline_(prompt,plen,buf,blen,ncin)
	char*	prompt ;
	int*	plen ;
	char*	buf ;
	int*	blen ;
	int*	ncin ;
	{

	int	i ;
	char*	p ;
	char	tmp[1024] ;


	/* no matter what, clear the array */
	for (i = 0 ; i < *blen; i++) buf[i] = ' ' ;

	/* local copy of the prompt */
	strncpy(tmp,prompt, *plen) ;
	tmp[*plen] = '\0' ;

	/* Get a good line */
	p = read_line(tmp) ;

	/* see what we got back */
	if (!p) *ncin = -1 ;
	else {
		*ncin = strlen(p) ;
		strncpy(buf, p, (int)*ncin) ;
		}

	}
