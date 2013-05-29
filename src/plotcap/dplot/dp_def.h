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
 * Tue Jan 30 15:24:10 PST 1990 (dredge--stanford)
 * "dp_def.h" : definitions file for DPLOT software.
 */

/* ---- Stuff that may want to be changed ----- */

/* the default promt */
#define MAKEPRMT()	sprintf(dp_prmt, "dplot(%.1f) ", REV_CODE)
/*
#define MAKEPRMT()	strcpy(dp_prmt, ">> ")
*/

/* pysical plot default parameters. BYPERC should be true if by percents */
#define  BYPRC   1
#define  CORNX  0.15	/* a little extra for the long labels... */
#define  CORNY  0.10
#define  PLENX  0.75
#define  PLENY  0.75
#define  PMAXX  10.
#define  PMAXY  10.
#define  ASPECT 0.0

/* Symbol size (default) */
#define SYMB_SIZE   .09 

/* Pen sizes: default */
#define PEN_AXIS	1		/* for axis drawing */
#define PEN_DATA	3		/* for data drawing */
#define PEN_SYMB	PEN_AXIS	/* for symbols */


/* character to mark commands: <CMD_CHAR><command name> */
#define CMD_CHAR '$'
#define SYS_CHAR '!'
#define SIN_CHAR '<'
#define CMT_CHAR '#'

#define BUFFSIZE  256
#define WORDSIZE  256
#define PRMTSZ    20

#define MAX_SEG_DEL 20	/* maximum number of deletable segments */

/* ------ End of stuff that may want to be changed -------- */

/* Some system dependancies.  These (at least now) don't need to be before
 *  the above ``easy-to-change'' stuff
 */
#ifndef BSD
#   define index   strchr
#   define rindex  strrchr
#endif

/* ifdef VFORK ??? */
#ifdef BSD
#   define fork vfork
#endif

/* Function declarations: */
float	fixpnt(/* char which, float val */) ;
double	Log_10(/* float */) ;



/* bit masks for plotting-by types */
#define BY_LINE  01
#define BY_SYMB  02

#define DOBACK -1
#define NOBACK  0

#define T 1
#define F 0

#define round     .0001

#define mkcmd(W,C,S)       sscanf(W, "%[^.].%s", C, S)
#define strequ(S1,S2)      (strcmp(S1,S2)==0)
#define strnequ(S1,S2,N)   (strncmp(S1,S2,N)==0)
#define min(A,B)           (A < B ? (A) : (B))
#define max(A,B)           (A > B ? (A) : (B))
#define abs(V) ((V) < 0.0? -(V) : (V))

#define dperr(S) dperr2("%s",S)

/* how to get a word from current file: */
#define W_ANY	0
#define W_MUST	1
#define W_LITR	2

/* type of word found with getword */
#define EOF_W  -1
#define REG_W   1
#define SYS_W   2
#define SIN_W   3

#define EXTERN extern
#define numb   short
#define bool   char
#define NULLC  '\0'
