
/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 21 apr 85 (mje)
 *
 * "glexs" : Stream general lexical analyzer.
 *
 * Original: Michael Eldredge (apr 85)
 */

/* Most systems now have strchr() (even if they also have index()), but
 *	just incase you don't -- use the following define
 */
#ifdef BSD
#  define strchr index
#endif

#include <string.h>

#ifndef _U
#  include <ctype.h>
#endif
#ifndef member
#  define member(S,C)	(C && strchr(S,C))
#endif

#define T 1
#define F 0

/* ------------- Things that may want to be changed -------------- */

/* sym1 is first symbol charater, symb is any following symbol chars
 *  skip is delimiters that never return as a token (like white space)
 *    num is decimal digits, oct is octal digits, hex is hexadecimal digits
 */
#define is_sym1(C)	(isalpha(C) || member(symbs,C))
#define is_symb(C)	(isalnum(C) || member(symbs,C))
#define is_skip(C)	(member(skips,C))
#define is_num(C)	isdigit(C)
#define is_oct(C)	(C >= '0' && C <= '7')
#define is_hex(C)	(isdigit(C) || member("abcdef",C))

/* ------------- End of things easily changed -------------------- */

/* The following is an internal copy of "glexs.h", included to keep this 
 *  routine to one file only.
 */
#define LXMADA	0		/* Current token parsing still going on */
#define LXEOS	-1
#define LXEOF	LXEOS

#define LXTOP	256	/* So we know upper bounds... */
#define LXSYM	260
#define LXREL	261
#define LXINT	262
#define LXOCT	263
#define LXHEX	264
#define LXMAX	269	/* So someone can find the next available number */

/* What stuff in the state array is.... */
#define ST_STA	 0
#define ST_CH0	 1	/* previous char */
#define ST_PSH	 2    /* pushed back char */
#define ST_TOK	 3	/* The tok to be, what are we working on ? */
#define ST_TYP	 4	/*  The sub-token (or sub-state?) */
#define ST_WHT	 5	/* Had white flag */
#define ST_LOC	 6	/* current possition in output buffer */
#define ST_HEX	 7	/* Hex ok? */
#define ST_OCT	 8	/* Oct ok? */
#define ST_REL	 9	/* reals/floats ok? */
#define MAX_ST	10

#define NO_PUSHD -2	/* No pushed back character for glexs */
/* End of "glexs.h" */


#define LXDOT	(int)('.')

/* when we are doing a float, which state: MANtissa or EXPonent */
#define R_MAN	 1
#define R_EXP	 2

/* Make these concepts a little easier to read */
#define hex_ok st[ST_HEX]
#define oct_ok st[ST_OCT]
#define rel_ok st[ST_REL]

#define S_INIT	   0	/* User requested init */
#define S_NEW	   1	/* On a new field */
#define S_SKIPW	   2	/* Bag white space and prepare for a new field */
#define S_CHAR1	   3	/* This is the first character */
#define S_CHARN	   4	/* Not the first character */


#ifdef ANSI_FUNC

int 
glexs (
    int st[],
    char *symbs,	/* symbol chars (other than alphanums) */
    char *skips,	/* white space chars */
    int c1,
    char buf[]
)
#else
int
glexs(st, symbs, skips, c1, buf)
	int	 st[] ;
	char	*symbs ;	/* symbol chars (other than alphanums) */
	char	*skips ;	/* white space chars */
	int	 c1 ;
	char	 buf[] ;
#endif
	{

	int	c, cl ; 
	int	more , push ;

	/* ---- start of glexs ---- */
 once_more:
	c = c1 ;
	cl = (isupper(c)? tolower(c): c) ;
	push = F ;		/* Took a pushed character yet */
	more = T ;		/* Need more chars to finish this token */

	/* ---- start glexs ---- */
	switch (st[ST_STA]) {

	case S_INIT:		/* User requested init of stuff */
		st[ ST_PSH ] = NO_PUSHD ;
		st[ ST_CH0 ] = 0 ;		/* prev char */
		/*FALLTHROUGH*/

	case S_NEW:		/* set up for new sequence */
		st[ ST_STA ] = S_SKIPW ;
		st[ ST_TOK ] = 0 ;		/* No token yet */
		st[ ST_WHT ] = F ;
		st[ ST_LOC ] = 0 ;
		buf[0] = 0 ;

		if (st[ST_PSH] != NO_PUSHD) {
			c = st[ST_PSH] ;
			cl = (isupper(c)? tolower(c): c) ;
			st[ST_PSH] = NO_PUSHD ;
			push = T ;
			}
		/*FALLTHOUGH*/

	case S_SKIPW:		/* Skipping white space */
		if (is_skip(c)) {
			st[ ST_WHT ] = T ;
			st[ ST_CH0 ] = cl;
			if (push) goto once_more ;
			return(0) ;		/* On spaces */
			}

		st[ ST_STA ] = S_CHAR1 ;
		/*FALLTHROUGH*/

	case S_CHAR1:
		/* if first character, decide if Number, Symbol, or Other */
		st[ ST_TOK ] = c ;	/* If tok is just char, asgn now */
		st[ ST_STA ] = S_CHARN ;	/* Still collecting */
		buf[0] = c ;
		buf[1] = 0 ;

		/* End of string ? */
		if (c <= 0) {
			st[ST_TOK] = LXEOS ;
			more	= F ;
			}

		/* An integer ? (or maybe a float...) */
		else if (is_num(c)) {
			st[ST_TOK] = LXINT ;  /* assume int now */
			st[ST_TYP] = LXINT ;
			if (oct_ok && c == '0') st[ST_TYP] = LXOCT ;
			}

		/* A symbol ? */
		else if (is_sym1(c)) st[ST_TOK] = LXSYM ;

		/* leading dot could be a float: .02 */
		else if (!rel_ok || c != '.')  { /* Not a float... */
			st[ST_STA] = S_NEW ;
			if (push) st[ST_PSH] = c1 ;
			more = F ;
			}
		break ;

	case S_CHARN:
		/* more than one character ... */
		switch (st[ST_TOK]) {		/* what are we working on? */
		case LXINT:		/* an integer */
			if (st[ST_TYP]==LXINT && is_num(c)) more = T ;
			else if (oct_ok && (st[ST_TYP]==LXOCT && is_oct(c)))
				more = T ;
			else if (hex_ok && (st[ST_TYP]==LXHEX && is_hex(cl))) {
				more = is_hex(cl) ;
				if (st[ST_CH0] == 'x') st[ST_LOC] = 0 ;
				}

			else if (hex_ok && (st[ST_LOC]==1 && 
					cl=='x' && st[ST_CH0]=='0')) {
				st[ST_TYP] = LXHEX ;
				}
			else if (rel_ok && (st[ST_TYP]!=LXHEX && 
					(c=='.'||cl=='e'))) {
				st[ST_TOK] = LXREL ;
				if (c == '.')	    st[ST_TYP] = R_MAN ;
				else if (cl == 'e') st[ST_TYP] = R_EXP ;
				}

			else {
				st[ST_PSH] = c ;
				more = F ;
				}
			break ;

		case LXREL:
			more = F ;
			if (is_num(c))	more = T ;
			else if ((c=='+'||c=='-') && st[ST_CH0]=='e') more =T;
			else if (cl=='e' && st[ST_TYP]!=R_EXP) {
				st[ST_TYP] = R_EXP ;
				more = T ;
				}
			else  {
				st[ST_PSH] = c ;
				more = F ;
				}
			break ;

		case LXDOT:
			if ((more = is_num(c))) {
				st[ST_TOK] = LXREL ;
				st[ST_TYP] = R_MAN ;
				}

			/* Ooops, took one too many */
			else if (c != LXEOS) st[ST_PSH] = c ;
			break ;
		
		case LXSYM:
			more = is_symb(c) ;
			if (!more) st[ST_PSH] = c ;
			break;
			}

		break ;		/*of next chars: CHARN */
		}

	st[ST_CH0] = cl ;

	if (more) {
		buf[ st[ST_LOC]++ ] = c ;
		buf[ st[ST_LOC]	  ] = '\0' ;		/* always term */
		if (push) goto once_more ;
		return(0) ;	/* Still working */
		}

	if (push) st[ST_PSH] = c1 ;

	if ((oct_ok || hex_ok) && st[ST_TOK] == LXINT)
		st[ST_TOK] = st[ST_TYP] ;

	st[ST_STA] = S_NEW ;
	if (st[ST_TOK] == LXEOS) st[ST_STA] = S_INIT ;
	return(st[ST_TOK]) ;
	}
