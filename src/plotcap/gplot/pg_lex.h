/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 12 may 85 (mje)
 *
 * Defines for the general lexical analyser.
 *
 * written: Michael Eldredge (may 85)
 */

/* Beginning of "glexs.h" */

#define LXMADA	0		/* Current token parsing still going on */
#define LXEOS	-1
#define LXEOF	LXEOS

#define LXTOP	 256	/* So we know upper bounds... */
#define LXSYM	260
#define LXREL	261
#define LXINT	262
#define LXOCT	263
#define LXHEX	264
#define LXREG	264	/* Register name */
#define LXVAR	265	/* Variable name */
#define LXFVR	266	/* Floating point variable name */
#define LXMAX	 269	/* So someone can find the next available number */

/* Some macros that should be a little useful */
#define LX_INIT(ST)	ST[ST_STA] = S_INIT
#define LX_TOKLEN(ST)	ST[ST_LOC]
#define LX_HADWHT(ST)	ST[ST_WHT]
#define LX_GETPSH(ST,C) (ST[ST_PSH] != NO_PUSHD ? C=ST[ST_PSH],1 : 0)
#define LX_CLRPSH(ST)	ST[ST_PSH] = NO_PUSHD

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

#define	 LXSTR	(LXMAX+1)	/* String token */
#define	 LXSKP	(LXMAX+2)	/* Comment to be skipped */
