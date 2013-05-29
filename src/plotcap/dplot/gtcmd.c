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
 * Thu Sep 21 11:11:38 PDT 1989 (dredge--stanford)
 * 
 * "gtcmd" : decode a character string and convert it to the 4 parameters for
 *	a call to gplot.
 *
 * calling sequence:
 *	ierr = gtcmd(buf, &cmd, &sub, &xval, &yval);
 *
 * where:
 *	buf	- (char *) String to decode.
 *	gplot values returned.
 *
 * returns:
 *	0	- ok, maps are working.
 *	-1	- Setup error, no gplot.h file found.
 *	-2	- Setup error, too many chars.
 *	-3	- Setup error, too many maps.
 *
 * examples:
 *
 *	gtcmd("G_PEND", ... );
 *	gtcmd("99",     ... );     these are the same.
 *
 *	gtcmd("G_MOVE,,4.3,0.213", ... );
 *	gtcmd("GMOVE,,4.3,0.213", ... );  these are the same.
 *
 *	gtcmd("G_AREA,G_BEGIN,0, 0.", ... );
 *
 * written:  Michael Eldredge (sep 84)
 * modified: Michael Eldredge (may 85)  Included the system depends.
 * modified: Michael Eldredge (feb 87)  Added error checking and fixed
 *	bug with ",," sequence.
 * modified: Michael Eldredge (mar 87)  See if gplot.h is in the environment.
 * modified: Michael Eldredge -- Stanford (sep 88)
 *	Skip white space in argument fields so that:
 *	$gplot  "G_DRAW,  ,   1.23 ,  3"
 * 	will work.  This really helps for reading fortran generate lists.
 * modified: mje -- stanford (sep 89) Include the location of the
 *	system gplot.h file `` #include "dp_gploc.h" ''
 */

#include <stdio.h>
#include <ctype.h>
#include "dp_def.h"
/* ^ just for the dperr() macor */

#include "dp_gploc.h"

#ifndef GPLOTH
#  define  GPLOTH "/usr/include/local/gplot.h"
#endif
/* try the environment first */
#define GP_ENV	"GPLOTH"

#ifndef BSD
#   define index   strchr
#endif

#define NMAPS 55
#define NCHRS 600

static map_err = 0 ; 	/* an error on setup? */
static inited = 0;
static struct _maps {
	char *mac ;
	char *val ;
	} *mp, maps[NMAPS+1] ;	/* last MUST be null */

static char   *mc, mapstr[NCHRS] ;


/* take a gplot command line type thing and make the maps */
int
gtcmd(bp, c, s, x, y)
	char   *bp;
	int    *c, *s;
	float  *x, *y;
	{

	int  i, oops = F ;
	char *ep, *vp;
	char tmp[40] ;
	char *gtmap(), *index();

	*c = *s = 0 ;
	*x = *y = 0.;

	for (i = 0, ep = bp; ep && i < 4; i++) {
		while (*bp && *bp != ',' && isspace(*bp)) ++bp ;
		if (ep = index(bp , ',')) *ep = 0;

		if (! *bp) vp = "0" ;	/* a ",," field */
		else {
			sscanf(bp, "%s", tmp) ;	/* remove surrounding ' ' */
			if ( !(vp = gtmap(tmp))) {
				dperr2("unknown gplot define '%s'", tmp) ;
				vp = "0" ;
				oops = T ;
				}
			}

		switch (i) {
		case 0: sscanf(vp, "%d" , c);  break;
		case 1: sscanf(vp, "%d" , s);  break;
		case 2: sscanf(vp, "%f" , x);  break;
		case 3: sscanf(vp, "%f" , y);  break;
			}

		if (ep) *ep++ = ',' ;	/* restore buffer */
		bp = ep ;
		}

	/* cmd == 0 is G_NULL */
	if (oops) *c = 0 ;	/* to avoid adverse happenings */
	return map_err ;
	}


/* subroutine to read in a gplot include file and get the symbols,etc.
 * in core 
 */
static
gtincl(fp)
	FILE *fp;
	{

	char  buf[132] ;
	char  com[132], word[132], value[132] ;
	char  *addit();

	mc = mapstr ;
	mp = maps   ;

	
	while ( fgets(buf, 132, fp) > 0) {

		if (sscanf(buf, "%s %s %s", com, word, value) != 3) continue;

		if (strcmp(com, "#define") != 0) continue;

		if (strncmp(word, "G_", 2) != 0) continue;

		if (mp >= &maps[NMAPS]) {
			dperr("gplot.h map: Too many maps") ;
			map_err = -3 ;
			break ;
			}
		mp->mac = addit(word);
		mp->val = addit(value);
		if (map_err) break ;

		mp++ ;
		}

	mp->mac = (char *)0;
	}



static char *
addit(str)
	char *str ;
	{
	char   *p;
	int	n = strlen(str) + 1 ;	/* room for NULL */

	if (mc+n >= &mapstr[NCHRS]) { 
		if (!map_err) dperr("gplot.h map: Too many chars") ;
		map_err = -2 ;
		return (char*)0 ;
		}

	p = mc ;
	strcpy(mc, str);
	mc +=  n ;

	return(p);
	}

/* try to return a mapping to a macro name */
#define is_numb(C) (isdigit(C) || (C)=='.' || (C)=='+' || (C)=='-')
static char *
gtmap(umac)
	char *umac ;
	{
	FILE *fp;
	char* name ;
	char* getenv() ;

	/* no mapping needed */
	if (is_numb(*umac)) return umac ;

	if (! inited) {
		inited = 1 ;
		if (! (name = getenv(GP_ENV))) name = GPLOTH ;
		fp = fopen(name, "r");
		if (fp == NULL) {
			maps[0].mac = (char *)0 ;
			dperr2("gplot.h map: Can't open %s", name) ;
			map_err = -1 ;
			return( (char *)0 );
			}
		gtincl(fp);
		fclose(fp);
		if (map_err) return (char*)0 ;	/* don't even try */
		}

	for (mp = maps ; mp->mac ; mp++) {
		if (aresame(umac, mp->mac) ) return(mp->val) ;
		}

	return ( (char *)0 );
	}

/* see if the two strings are the same, ignoring case and underbars */
#define bagund(P)  while (*P && *P=='_') P++

/* Note: s1 is mapped to upper case */
static int
aresame(s1, s2)
	register char *s1, *s2;
	{
	char c1 ;

	for(;;s1++ , s2++) {
		bagund(s1);
		bagund(s2);

		if (! *s1 || ! *s2) break;	/* at one end */

		c1 = islower(*s1)? toupper(*s1): *s1 ;
		if (c1 != *s2) return(0) ;   /* no good */
		}

	if (! *s1 && ! *s2) return(1);
	return(0);
	}
