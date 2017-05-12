/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

#include <stdio.h>
#include "auxfns.h"
#include "pg_lex.h"

/* some loaders will not load data only (ie: common) from a library.  So
 *  the definition of common must not be in its own file; thus we put
 *  it here.
 */
#define COMMON
#include "pg_def.h"
#undef  COMMON

/* Just to get the proper definition of the errno variable for file open
 *	error return.
 */
#include <errno.h>
#ifdef convex
extern int errno ;
#endif

static	void	getspec() ;
static int set_nam(char *cap, char *dev);
static int gat_nam(int check);
static int gat_ent(int keep);
static sync_up(void);
static void getspec(char *buf, int bufsz);
static int cpy_in(char *dst, char *dstend, char *src);


/* date: 09 feb 87 (mje)
 * "pgetent": search 'capfile' for 'devname' resolving all indirections (LIKEs)
 *	and file indirections (%next ... and 'dev@next_file')
 *
 * written:  Michael Eldredge (jan 85)
 */
#ifdef ANSI_FUNC

int 
pgetent (
    char *capfile,		/* Beginning file to search */
    char *devname		/* Device for which to look */
)
#else

int
pgetent(capfile, devname)
	char	*capfile ;		/* Beginning file to search */
	char	*devname ;		/* Device for which to look */
#endif
	{

	int	 iret = E_ERR ;
	Bool	 done ;
	defrot("pgetent") ;

	p.linnum = -1 ;			/* Incase open error */
	pg_init((char *)0 ) ;
	if (!set_nam(capfile, devname)) return(E_ERR) ;	/* Open the file ??? */
	p.linnum = 0 ;			/*For error reporting, input linenum */

	for (done = F ; !done ; ) {
		switch (gat_nam(CHECK)) {
		case E_EOF:
			if (p.nxtfil[0] != CNULL) {
				if (!set_nam(p.nxtfil, "")) {
					done = T ;
					iret = E_ERR ;
					}
				p.nxtfil[0] = CNULL ;
				}
			else {
				iret = pg_er(E_NODV, p.devnam, 0, 0) ;
				done = T ;
				}
			break ;

		case  E_OK:		/* Matched! */
			iret = gat_ent(KEEP) ;
			if      (iret == E_NO || iret == E_ERR)  done = T ;
			else if (iret == E_EOF) {
				iret = pg_er(E_UEOF, "", 0, 0) ;
				done = T ;
				}
			/* else a LIKE was found and set up */
			break ;

		case E_ERR:		/* Is this correct? */
			iret = E_ERR ;
			done = T ;
			break ;

		default:
			iret = gat_ent(NO_KEEP) ;	/* Skip over current */
			break ;
			}
		}

	p.linnum = -1 ;			/* don't report file name in errors */
	return (iret) ;
	}

/* "pg_init": init things (common) for pget-ing
 * note: note "static" function; all pgetXXX() funcs use this.
 */
#ifdef ANSI_FUNC

int 
pg_init (
    char *forwhat	/*Getting from cap file. OR Entok from buffer*/
)
#else

pg_init(forwhat)
	char	*forwhat ;	/*Getting from cap file. OR Entok from buffer*/
#endif
	{

	if (! forwhat) {		/* from cap file */
		p.infile	= T ;
		p.defbuf[0] = CNULL ;
		p.defp      = p.defbuf ;

		p.devnam[0] = CNULL ;
		p.capfil[0] = CNULL ;
		p.nxtfil[0] = CNULL ;

		p.fpcap	    = NULL  ;
		p.savquo    = T ;	/* Keep all quotes with strings */
		p.lexst[ST_OCT] = F ;
		p.lexst[ST_HEX] = F ;
		p.lexst[ST_REL] = F ;
		strcpy(p.skips, ENT_SKIP) ;
		}

	else {		/* Entok from buffers */
		p.infile	= F ;
		p.bpcap		= forwhat ;
		p.savquo	= F ;
		p.lexst[ST_OCT] = T ;
		p.lexst[ST_HEX] = T ;
		p.lexst[ST_REL] = T ;
		strcpy(p.skips, BUF_SKIP) ;
		}

	/* and both need this stuff */
	p.lexst[0]  = 0 ;	/* init parser */
	p.ateof	    = F ;
	}

/* "set_nam": given a cap file and a devname that may contain a capfile
 *	(eg: devnam = "tekc@/usr/tmp/Plotcap") get the devname and file name
 *	in the proper places in common.
 */
#ifdef ANSI_FUNC

static int 
set_nam (char *cap, char *dev)
#else

static int
set_nam(cap, dev)
	char	*cap , *dev ;
#endif
	{
	char	 c , *bp ;

	defrot("set_nam") ;

	/* first move the device name in */
	if (*dev) {
		bp = p.devnam ;
		while ((c = *dev++) != CNULL && c != GT_FIL)
			*bp++ = c ;
		*bp = CNULL ;
		}

	if (c == GT_FIL) cap = dev ;

	/* now get the file name */
	if (*cap) {
		bp = p.capfil ;
		while ((c = *cap++) != CNULL) *bp++ = c ;
		*bp = CNULL ;

		/* Try to open the cap file now, closing old one if need be */
		if (p.fpcap) fclose(p.fpcap) ;
		if ((p.fpcap = fopen(p.capfil, "r")) == NULL) {
			/* some systems (like VMS) don't like the standard
			 *  `` extern int errno ; '' definintion.  But
			 *  errno.h should have proper definition.
			 * See the "#include <errno.h>" above.
			 */

			p.fpcap = NULL ;
			pg_er(E_CFIL, p.devnam,0,errno) ;
			return(E_NO) ;
			}
		}

	else {		/* Use the same cap file... */
		if (p.fpcap) rewind(p.fpcap) ;
		else {
			pg_er(E_CREW, "",0,0) ;
			return(E_ERR) ;
			}
		}

	/* reset parser states */
	p.ateof = F ;
	p.lexst[0] = 0 ;
	p.infile = T ;
	p.linnum = 0 ;
	p.savquo = T ;

	return(E_OK) ;
	}

/* "gat_nam": gather a name list together into the internal buffer space 
 *	If 'check' is set, check each name against 'devnam'
 */
#ifdef ANSI_FUNC

static int 
gat_nam (Bool check)
#else

static int
gat_nam(check)
	Bool	 check ;
#endif
	{

	char	 buf[NAM_SIZE] ;
	char	 nam[NAM_SIZE], *np = nam ;	/* collect the name list */
	char*	 enam = &nam[NAM_SIZE-1] ;
	int	 iret = E_NO ;			/* Assume no match */
	Bool	 done, gotone ;

	defrot("gat_nam") ;


    re_try:
	switch (entok(buf,sizeof(buf))) {
	case GT_CMD:		/* A special commmand */
		getspec(buf,sizeof(buf)) ;
		goto re_try ;
		break ;

	case GT_SYM:		/* A device name perhaps? */
		for (done=F; !done ; ) {
			gotone = F ;
			do {
				np += cpy_in(np, enam, buf) ;

				switch (entok(buf,sizeof(buf))) {
				case GT_OVR:
					iret = pg_er(E_NBIG, buf, 0,0) ;
				case GT_END:		/* No more names */
					done = T ;
				case GT_OR:		/* Continue checking */
					gotone = T ;
					break ;
					}

				} while (!gotone) ;
			*np = CNULL ;

			if (check && strequ(nam, p.devnam)) {
				iret = E_OK ;	/* matched */
				check= F ;	/* don't need to anymore */
				}

			np = nam ;
			buf[0] = CNULL ;	/* Kludge...no copy in */
			}
		break ;

	case LXEOF:		/* Unexpected error... maybe... */
		/* iret = pg_er(E_UEOF, "", 0,0) ; */
		iret = E_EOF ;
		break ;
	case GT_OVR:
		iret = pg_er(E_NBIG, buf, 0,0) ;
		break ;
	default:
		iret = pg_er(E_UTOK, buf, 0, 0) ;
		/* Error... */
		break ;
		}

	return (iret) ;
	}

/* "gat_ent" : gather together the currently pointed to entry */
#ifdef ANSI_FUNC

static int 
gat_ent (
    Bool keep		/* save or not save what we get? */
)
#else

static int
gat_ent(keep)
	Bool	 keep ;		/* save or not save what we get? */
#endif
	{

	char	*ep = p.defp, *np  ;		/* prev entry found... */
	char	*endp = &p.defbuf[sizeof(p.defbuf)-1] ;
	char	 buf[NAM_SIZE] ;
	int	 n , iret = E_NO ;

	defrot("gat_ent") ;

	while ((n = entok(buf,sizeof(buf))) != GT_END	&& n != LXEOF
							&& n != GT_OVR) {

		if (n == GT_NOT) {		/* A "not"ed capability */
			*ep++ = GT_NOT ;
			n = entok(buf,sizeof(buf)) ;
			if (n == GT_OVR) break ;
			}

		if (n != LXSYM) {
			sync_up() ;	/* error */
			pg_er(E_SYMB, p.devnam, n, 0) ;
			iret = E_ERR ;
			continue ;
			}

		np = ep ;		/* remember where name begins */
		ep += cpy_in(ep,endp, buf) ;
		n = entok(buf,sizeof(buf)) ;
		if (n == GT_OVR) break ;

		if (n == GT_END) {
			*ep++ = GT_MRK ;
			continue ;
			}

		if (n != GT_EQU) {
			/* Error */
			sync_up() ;
			pg_er(E_NEQU,p.devnam, n, 0) ;
			iret = E_ERR ;
			continue ;
			}

		*ep++ = GT_EQU ;

		while ((n = entok(buf,sizeof(buf))) != GT_END && n != LXEOF
							      && n != GT_OVR) {
			ep += cpy_in(ep,endp, buf) ;
			}
		if (n == GT_OVR) break ;

		*ep++ = GT_MRK ;		/* Between caps */
		}

	if (keep) p.defp = ep ;
	*p.defp = CNULL ;

	/* buffer overflow */
	if (n == GT_OVR) { pg_er(E_EBIG,buf,0,0) ; return(E_ERR) ; }
	/* unexpected EOF ..... */
	if (n == LXEOF)
		return(E_EOF) ;

	/* Check for a like capability */
	if (keep && strnequ(np, LIKE_CAP, LIKE_SIZ)) {
		p.defp = np ;		/* backup before LIKE */
		ep = np ;		/* for errors */
		np += LIKE_SIZ ;
		p.savquo   = F ;
		p.lexst[0] = 0 ;	/* reset genlex */
		p.infile   = F ;
		p.bpcap    = np;

		if ((n = entok(buf,sizeof(buf))) == GT_OVR)
			iret = pg_er(E_EBIG, buf, 0,0) ;
		else if	(n < 0) iret = pg_er(E_NTOK, ep, 0,0) ;
		else if (!set_nam("", buf)) iret = E_ERR ;
		else iret = E_OK ;

		*p.defp = CNULL ;
		}

	return(iret) ;
	}

/* "sync_up": Sync up the input file.  Basically, just find the end of the
 *	current entry to that parsing can resume.
 */
#ifdef ANSI_FUNC

static 
sync_up (void)
#else

static 
sync_up()
#endif
	{

	int	 n ;
	char	 buf[NAM_SIZE] ;

	while ((n = entok(buf,sizeof(buf)))  != EOF
					&& n != GT_END
					&& n != GT_OVR) ;
	}

#ifdef ANSI_FUNC

static void 
getspec (char *buf, int bufsz)
#else

static void
getspec(buf,bufsz)
	char	*buf ;
	int	bufsz ;
#endif
	{

	defrot("getspec");

	p.savquo = F ;		/* We don't want the quotes passed on here */

	if (entok(buf,bufsz) != GT_SYM) (void)pg_er(E_SCMD, buf, 0,0) ;

	else if (strequ(buf, NEXT_CMD)) {
		if (entok(p.nxtfil,sizeof(p.nxtfil)) != GT_STR) {
			(void)pg_er(E_NEXT, p.nxtfil, 0,0) ;
			p.nxtfil[0] = CNULL ;
			}
		}

	else (void)pg_er(E_UCMD, buf, 0,0) ; 	/* unknown special command */

	p.savquo = T ;
	}

/* "cpy_in": Like strcpy(), but return then number of chars copied */
#ifdef ANSI_FUNC

static int 
cpy_in (
    char *dst,
    char *dstend,	/* last available character */
    char *src
)
#else

static int
cpy_in(dst,dstend, src)
	char	*dst ;
	char	*dstend ;	/* last available character */
	char	*src ;
#endif
	{

	register int n = 0 ;
	char*	bdst = dst ;	/* remember the beginning (in case error) */

	defrot("cpy_in") ;

	while (dst != dstend && (*dst++ = *src++) ) n++ ;
	if (dst == dstend) {
		*dstend = '\0' ;
		--n ;
		pg_er(E_OVR, bdst,0,0) ;
		}
	return n ;
	}


/* "pg_fndid" : Find the requested id in the current internal buffer.  If
 *	found then return a pointer to the first character after the '='. 
 *	Else return a null pointer. (Not found).
 */
#ifdef ANSI_FUNC

char *
pg_fndid (char *id)
#else

char *
pg_fndid(id)
	char	*id ;
#endif
	{

	register char	*ep1, *ep ;
	int	 len_eid ;
	int	 len_id = strlen(id) ;
	Bool	 hasNot ;			/* Not'ed capability */

	for (ep = p.defbuf ; *ep ; ++ep) {
		ep1 = ep ;
		while (*ep != GT_EQU && *ep != GT_MRK) ++ep ;

		if ((hasNot = (*ep1 == GT_NOT))) ++ep1 ;

		len_eid = (int)(ep - ep1) ;

		/* See if matching id's */
		if (len_eid == len_id && strnequ(ep1, id, len_id)) {
			if (hasNot) return( (char *)0 ) ;	/* not'ed */
			else        return( ep ) ;		/* Good one */
			}

		/* No match yet, keep looking */
		if (*ep == GT_EQU)
			while (*ep != GT_MRK) ++ep ;
		}

	return( (char *)0 ) ;	/* never found anything */
	}
