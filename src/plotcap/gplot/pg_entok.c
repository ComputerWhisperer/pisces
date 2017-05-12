/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

#include "auxfns.h"
#include "pg_lex.h"
#include "pg_def.h"

static int get1c(int literal);

/* date: 09 feb 87 (mje)
 * "entok": return the next token from the cap file
 *
 * written:  Michael Eldredge (jan 86)
 */
#ifdef ANSI_FUNC

int 
entok (
    char *buf,		/* returned token value string */
    int blen		/* how much room? */
)
#else

int
entok(buf,blen)
	char	*buf ;		/* returned token value string */
	int	blen ;		/* how much room? */
#endif
	{

	int	 ch, ch0, n ;
	int	 newn = 0, match = 0 , keep = 0 ;	/* set to quiet lint */
	char	*bp, *bend ;
	Bool	 lit = F ;

	defrot("entok") ;

	ch = 0 ; 
	bp   =  buf ;
	bend = &buf[blen-1] ;	/* room for the NULL at the end */

	/* Loop collecting the next token from the current input file */
	for (;;) {
		ch0 = ch ;
		ch  = get1c(lit) ;	/* get next character */
		if (bp >= bend) {	/* overflow buffer? */
			n = GT_OVR ;
			break ;
			}

		if (! lit) {
			if ((n = GLEXS(ch,buf))==0)
				continue ;
			if (n == LXEOS) break ;
			}
		else {
			if (ch <= 0) break ;
			if (ch == match) {
				if (ch0 == '\\') {
					if (keep) {
						if (p.savquo) *bp++   = ch ;
						else           bp[-1] = ch ;
						}
					continue ;
					}
				if (p.savquo && keep) 
					*bp++ = match ;	/*keep quotes*/
				*bp = 0 ;
				lit = F ;
				n = newn ;
				}
			else {
				if (keep) *bp++ = ch ;
				continue;
				}
			}

		/* Got a real token yet? */
		if (n == LXSKP) { ; }

		else if (n <= 0 || n > LXTOP) break ;

		/* Start of a comment? */
		else if (n == GT_CMT) {
			lit = T ;
			match = GT_ECM ;
			bp = buf ;
			newn= LXSKP ;
			keep = 0 ;
			}

		/* Start of a literal string ? */
		else if (n == GT_QU1 || n == GT_QU2) {
			lit = T ;
			match = n ;
			newn = LXSTR ;
			keep = 1 ;

			/* Non-standard Getok, keep the string intact */
			if (p.savquo) *bp++ = match ;
			else bp = buf ;     /* Reset so quote isn't copied */
			}

		else break ;

		}/*for collecting a token... */

	if (n == GT_OVR) pg_er(E_OVR,buf,0,0) ;
	return(n) ;
	}

/* "get1c" : return one charater from the current file.
 *	If not literal, glexs will handle the pushed back characters, BUT
 *	if we are parsing a literal string we must go and check glexs'
 *	pushed back character to see if there is one and then take it.
 */
#ifdef ANSI_FUNC

static int 
get1c (Bool literal)
#else

static int
get1c(literal)
	Bool	 literal ;
#endif
	{
	int	 ch ;

	/* 1. A pushed back character, only needed if literal string */
	if (literal && p.lexst[ST_PSH] != NO_PUSHD) {
		ch = p.lexst[ST_PSH] ;	/* get pushed char */
		p.lexst[ST_PSH] = NO_PUSHD ;
		}

	/* 2. Get from the input stream if not already at the EOF */
	else if (!p.ateof) {
		if (p.infile) {			/* read from file */
			ch = getc(p.fpcap) ;
			if (ch == '\n') p.linnum++ ;
			}
		else    ch = *p.bpcap++ ;	/* read from string */

		if (ch == EOF)  p.ateof = T ;
		}

	/* 3. Already at EOF, just keep returning it until parsing done */
	else ch = EOF ;
	
	return(ch) ;
	}
