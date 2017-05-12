/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: (22 mar 85) dredge@Fuji
 *
 * "stab.c" : symbol table routines, Make, add-to, get-from, etc.... 
 *
 * written: Michael Eldredge (jan 85)
 */

/* Some symbol table manipulation stuff */
#define strequ(A,B)	(strcmp(A,B)==0)

#include <stdlib.h>

/* The symbol table and its entries */
static	char	*stab ;			/* Symbols... */



static struct _sent {			/* Symbol table entries */
	char  *s_sym ;
	int    s_val ;
	char   s_typ ;
	} *sent ;

static  int	epnt ;			/* Current count of enties */
static	char   *sloc ;			/* Next table location */
static	char   *smax ;			/* end of the table, overflow */
static	int	emax ;			/* Max number of entries */
static	int	have1 = 0 ;		/* Have a table allocate? */


static int stab_fd(char *sym);

/* "stab_mk" : Clean an old table out (if need be) and make a new one
 *	If either size is 0 then free space from before.
 *	If already have a stab in use, free it before making the new one.
 * returns:
 *	0	- OK. It was made.
 *	-1	- Not enough space.
 */
#ifdef ANSI_FUNC

int 
stab_mk (int mxsize, int mxents)
#else

int
stab_mk(mxsize, mxents)
	int	 mxsize, mxents ;
#endif
	{

	/* If we have a table release it */
	if (have1) {
		free(        stab ) ;
		free((char *)sent ) ;
		}

	have1 = 0 ;

	/* They just wanted to free the memory */
	if (mxsize <= 0 || mxents <= 0) return(0) ;

	stab = malloc(mxsize) ;		/* make the symbols */
	if (! stab ) return(-1) ;	/* error */
	smax = (stab + mxsize) ;
	sloc = stab ;

	sent = (struct _sent *)malloc( mxents * sizeof(struct _sent) ) ;
	if (! sent ) {
		free(stab) ;
		return(-1) ;
		}
	emax = mxents ;
	epnt = 0 ;

	have1 = 1 ;
	return(0) ;
	}

/* "stab_iq" : Inquire about the current symbol table 
 * returns:
 *	0	- Have a symbol table, current counts returned.
 *	-1	- Haven't allocated anything yet.
 */
#ifdef ANSI_FUNC

int 
stab_iq (int *curloc, int *curent)
#else

int
stab_iq(curloc, curent)
	int	*curloc, *curent ;
#endif
	{

	if (!have1) return(-1) ;

	*curloc = (int)(smax - sloc) ;
	*curent = epnt ;

	return(0) ;
	}


/* "stab_gt" : Given a symbol, return its type and value 
 * returns:
 *	-1	- No table allocted yet (stab_mk not called yet).
 *	-2	- Symbol not found.
 *	<n>	- Symbol number.
 */
#ifdef ANSI_FUNC

int 
stab_gt (char *sym, int *val, int *typ)
#else

int
stab_gt(sym, val, typ)
	char *sym ;
	int  *val , *typ ;
#endif
	{
	struct _sent *sp ;
	int    n ;

	if (!have1) return(-1) ;

	if ((n = stab_fd(sym)) >= 0) { 
		sp = &sent[n] ; 
		*val =       sp->s_val ; 
		*typ = (int)(sp->s_typ) ; 
		return(n) ; 
		}
	return(-2);
	}

/* "stab_pt" : Put a symbol and its values into the table (add it)
 *	No checking is done to see if it is already in the table, it is just
 *	added to the end.
 * returns:
 *	<n>	- Symbol number in table.
 *	-1	- Stab never allocated.
 *	-2	- No more entry slots for the symbol.
 *	-3	- No more space for symbol.
 */
#ifdef ANSI_FUNC

int 
stab_pt (char *sym, int val, int typ)
#else

int
stab_pt(sym, val, typ)
	char  *sym ;
	int    val , typ ;
#endif
	{
	struct _sent *sp ;
	int    nsym ;

	if (!have1) return(-1) ;

	if (epnt > emax) return(-2) ;		/* No more slots */

	sp = &sent[ (nsym = epnt++) ] ;		/* new s_entry */

	sp->s_val = val ;
	sp->s_typ = (char)typ ;
	sp->s_sym = sloc ;

	while ( (*sloc++ = *sym++ )) 
		if (sloc >= smax) return(-3) ;
	
	return(nsym) ;
	}

/* "stab_gn" : Given the symbol number, return the symbol itself 
 * returns:
 *	(char *)0	- Symbol number invalid or stab never allocated.
 *	<else>		- character pointer to static symbol.
 */
#ifdef ANSI_FUNC

char *
stab_gn (int n)
#else

char *
stab_gn(n)
	int  n ;
#endif
	{
	if (!have1 || n < 0 || n >= epnt) return( (char *)0 ) ;
	return( sent[n].s_sym ) ;
	}


/* "stab_fd" : Look for the given symbol in the table 
 * returns:
 *	-1	- Stab never allocated.
 *	-2	- Not found.
 *	<n>	- Symbol number
 */
#ifdef ANSI_FUNC

static int 
stab_fd (char *sym)
#else

static int
stab_fd(sym)
	char *sym ;
#endif
	{
	int  i ;

	if (!have1) return(-1) ;

	for (i = 0 ; i < epnt ; i++) {
		if (strequ(sent[i].s_sym, sym)) return(i) ;
		}
	
	return( -2 ) ;
	}
