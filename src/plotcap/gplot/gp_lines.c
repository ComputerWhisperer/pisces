/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 07 may 86 (mje)
 *
 * "gp_lines.c": gplot interal line type routines. These are used to
 *	simulate different linetypes for devices that can't do it 
 *	for themselves.
 *
 * written:  Michael Eldredge (apr 84)
 * modified: Michael Eldredge (apr 85)
 */

#include "auxfns.h"
#include "gplot.h"
#include "al_alu18.h"
#include "gp_def.h"
#include "gp_com.h"


/* number of bits in line pattern definition and macros to collect/breakout
 *  patter and scale from one word
 */
#define PATSIZE 8
#define makepat(P,S)  ((S<<8) | (P&0377))
#define pattern(L)    (pats[L-2])
#define getpatt(L)    (pattern(L) & 0377)
#define getscal(L)    ((pattern(L) >> 8) & 0377)

/* count up or down the bit pattern (shouldn't make much diff, but...) */
#define GO_UP
#ifdef GO_UP
#  define ToNextBit   ( curbit=(++curbit>=PATSIZE ? 0 : curbit) )
#else 
#  define ToNextBit   ( curbit=(--curbit < 0 ? PATSIZE-1 : curbit) )
#endif

/* inches per bit in line type bit pattern */
#define BITSIZE (0.020)

static char   bits[PATSIZE];
static int    curbit;
#define ThisBit    bits[curbit]

static float  cosA , sinA;

/* patterns and scales for each line type */
static unsigned int pats[] = {             /*  0 & 1 are pre-set types */
		makepat( 0x80 , 1 )  ,     /*  2  */
		makepat( 0x88 , 1 )  ,     /*  3  */
		makepat( 0x88 , 2 )  ,     /*  4  */
		makepat( 0x88 , 3 )  ,     /*  5  */
		makepat( 0x88 , 4 )  ,     /*  6  */
		makepat( 0xd6 , 2 )  ,     /*  7  */
		makepat( 0xfa , 2 )  ,     /*  8  */
		makepat( 0x99 , 2 )        /*  9  */
	};

#define NTYPES ( (sizeof(pats)/sizeof(int)) + 2)

static 	int    scale ;         /* scale of patterns */

static 	float  dashlen;        /* length of current dash segement */
static 	int    dashpen;        /* move or draw of current dash segment */

static int getnextlen(float *len);
static void drawto(float len, int pen);
/*
 */
gp_linplt()
	{

	register float togo;      /* amount of line segment to go */
	register Bool  isdraw;    /* is the plota call to draw or move */
	int   save_cmd = g_cmd ;	/* cause we'll fool with it.. */
	float xseg, yseg;
	double sqrt();


	/* If we are just moving to new point, do that first and then we'll
	 *  sequence through the dashed lines (to keep spacing,etc matched
	 * If we are drawing to the new point, remember that fact and then
	 *  draw and move through the dashed line sequence.
	 */
	if (!(isdraw = (g_cmd == G_DRAW))) {   /* just a move, do now! */
		(void)hard_work() ;	/* cmd, x, y all set by gplot2 */
		}

	xseg = g_x1 - g_Plast.x;      /* how long is the segment ?? */
	yseg = g_y1 - g_Plast.y;
	togo = sqrt((double)(xseg*xseg  +  yseg*yseg));

	/* cosine and sine between old and new points */
	cosA = (togo == 0.0 ? 0.0 : (xseg / togo));
	sinA = (togo == 0.0 ? 0.0 : (yseg / togo));

	/* for quicker computing, set global x and y to beginning of segment
	 *  and as we move/draw dashed line, we'll update it
	 */
	g_x1 = g_Plast.x ;   /* starting point */
	g_y1 = g_Plast.y ;

	for(;;) {
		if (dashlen <= 0.0) dashpen = getnextlen(&dashlen);

		/* remaining segment is less than dashed segment */
		if (togo <= dashlen) {
			if (isdraw) drawto(togo, dashpen);
			dashlen -= togo;
			break;            /* that's it */
			}

		/* remaining segment is longer that dashed segment */
		else {
			if (isdraw) drawto(dashlen, dashpen);
			togo -= dashlen;
			dashlen = 0.0;
			}

		}
	g_cmd = save_cmd ;
	}


/* "drawto" : draw or move from `thisX', `thisY' a line `len' long */
static void drawto(float len, int pen)
	{

	g_cmd = pen ;

	g_i0 = round(g_x1    * C.USCAL * (float)C.PUNX) + C.ORGX ;
	g_j0 = round(g_y1    * C.USCAL * (float)C.PUNY) + C.ORGY ;

	g_x1 += len * cosA;   /* get the new (x,y) point */
	g_y1 += len * sinA;

	g_i1 = round(g_x1    * C.USCAL * (float)C.PUNX) + C.ORGX ;
	g_j1 = round(g_y1    * C.USCAL * (float)C.PUNY) + C.ORGY ;

	(void)hard_work() ;
	}


/*
 *ff
 * "lnset" : Change the current line type for drawing done by `plota'.
 *
 * calling sequence:
 *	<n> = lnset(type)
 *
 * where:
 *	type	- (int) New line type for `plota' to use in drawing.
 *		  Valid line types are 1 through MAXTYPES.  Any `type'
 *		  greater than the maximum are take as modulus that
 *		  maximum.
 *	<n>	- (int) Return value of `lnset'.  This is always the 
 *		  maximum line type that can be given.
 *of
 * written: Michael Eldredge (apr 84) 
 */
int
gp_lnset(type)
	int  type;
	{

	int newpattern, i;
	int linetype ;

	/* Line type 0 and 1 are preset and are handled at a higher
	 *  level.  Thus if it is one of these types, just return 
	 */
	if (type <= 1) return(NTYPES) ;		/* no change, just report */

	/* Now: 0 & 1 get trapped differently, but anything else is taken
	 *  mod the actual table size, so that anything greater than 1 will
	 *   use the tables (vs. the old attempt to say that type%NTYPES
	 *    equal to 1 is line 1 and == 0 is line 0
	 */
	linetype = (type % (NTYPES-2)) + 2 ;

	/* set up for new type */
	newpattern = getpatt(linetype);    /* get the bit pattern */
	scale      = getscal(linetype);    /*  and its scaling    */

	for (i=0; i< PATSIZE; i++) {
		bits[i] = ((newpattern & 01) == 0);   /* set into array */
		newpattern >>= 1;              /* shift down     */
		}

	curbit = 0;        /* set to beginning of bit pattern */
	dashlen = 0.0;     /* reset for new type */

	return(NTYPES);  /* letem know how many types are possible */
	}


/* "getnextlen" :  get the next dashed line segment length and pen condition */
static int getnextlen(float *len)
	{
	char FirstBit;    /* bit on or off */
	int  nbits;      /* number in current sequence */

	/* count the number of bits that are the same */
	/*
	printf("  FirstBit is %s (curbit %d)", (ThisBit? "ON":"OFF"),curbit);
	*/
	nbits = 0;
	for(FirstBit = ThisBit; FirstBit == ThisBit; ToNextBit) nbits++ ;

	*len = (float)(nbits * scale) * BITSIZE;  /* turn into inches */

	/*
	printf(" nbits %d,  len %g\n", nbits, *len);
	*/

	/* if bit on then drawing, else moving */
	return(FirstBit ? G_DRAW : G_MOVE);
	}


/*
 * "lndef" : Define a new line type for use by `plota'.
 *
 * calling sequence:
 *	<n> = lndef(type, patt, scal)
 *
 * where:
 *	type	- (int) The line type to redefine (Thus all subsequent calls
 *		  to `lnset(type)' will use the new pattern for plotting.
 *	patt	- (int) The (8) bit pattern defining the new line type. Each
 *		  bit set to 1 will cause drawing and each set to 0 will
 *		  cause skipping.
 *	scal	- (int) The scaling of the bits in `patt'.  Each bit in
 *		  the pattern will actually be the scale times the width
 *		  of one bit wide when drawn (or skipped).
 *	<n>	- (int) The old pattern and scale previously associated with
 *		  the given line type number.  The scale is in the high byte
 *		  and the pattern is in the low byte.
 *
 * Notes:
 *	1. Line type 1 is always a solid line.  This can not be redefined.
 *
 * Examples:
 *	`#' will be ON, and '_' will be OFF.
 *
 *	patt = 0xAA , scale = 1  -> Gives ``#_#_#_#_''
 *
 *	patt = 0xAA , scale = 2  -> Gives ``##__##__##__##__''
 *	
 *	patt = 0xFA , scale = 1  -> Gives ``#####_#_''
 *
 *	patt = 0xFA , scale = 2  -> Gives ``##########__##__''
 *
 * written: Michael Eldredge (apr 84)
 */
int
lndef(ntype, patt, scal)
	int ntype, patt, scal;
	{

	int oldpat;

	if (ntype < 0) return(0);
	if ((ntype = (ntype % NTYPES)) <= 1) return (0);

	oldpat = pattern(ntype);               /* remember old pattern */
	pattern(ntype) = makepat(patt, scal);  /*  and set in new pattern */

	return(oldpat);
	}
