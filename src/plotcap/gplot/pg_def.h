/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* Tue Sep 19 16:29:38 PDT 1989 (dredge--stanford)
 * "pg_def.h" : Definitions for the Plocap file reading routines.
 *
 * written: Michael Eldredge (nov 85)
 */

#include "pg_lex.h"

/* ---- THINGS EASILY CHANGED ---------------------------------------------- */

#define CAP_SIZE 2048		/* Buffer for capabilies from cap file */
#define NAM_SIZE  256		/* File name buffer size */

#define MAX_LIKES 5		/* Before complaining of loop */
#define LIKE_CAP  "LIKE="	/* Word for like */
#define LIKE_SIZ  (sizeof(LIKE_CAP)-1)

#define MAX_TLABS 10		/* pgetprg(): max temp labels 1$; 2$; ... */

#define NEXT_CMD  "next"	/* word for next-file-is command */

#define GT_CMD '%'		/* Special command in the file */
#define GT_END ':'		/* End of each field */
#define GT_OR  '|'		/* <dev name> GT_OR <dev name> ... */
#define GT_EQU '='		/* Assignment <cap name> GT_EQU <cap list> */
#define GT_NOT '!'		/* Turn off a capability: GT_NOT <cap name> */
#define GT_CMT '#'		/* Comment start. */
#define	GT_ECM '\n'		/* End comment */
#define GT_QU1 '\''		/* Single quote */
#define GT_QU2 '"'		/* Double quote */
#define GT_ESC '\\'		/* Escaped-special-char special char */

/* File name mark: devname="<dev name> [ GT_FIL <file name> ]" */
#define GT_FIL '@'

/* Mark caps internally: <cap name> GT_EQU <cap list> GT_MRK <cap name> .... */
#define GT_MRK '\n'		

#define ENT_SYMS "_"		/* Additional symb chars (other than alnums) */
#define ENT_SKIP " \t\n\f"	/* White space characters */
#define BUF_SKIP ""		/*  parsing of capfile removed white space */
#define MAX_SKIPS	6	/* max skip chars */

/* -END OF THINGS EASILY CHANGED ------------------------------------------ */

/* some boolean defines... */
#define CHECK	 T
#define NO_CHECK F
#define KEEP	 T
#define NO_KEEP	 F

#define GT_SYM LXSYM		/* Symbols... */
#define GT_STR LXSTR		/* Strings */
#define GT_OVR -11		/* buffer overflow from Entok() */

#define CNULL '\0'

/* ------------------------------------------------------------------------ */

/* Just to make this easier to write later on */
#define  GLEXS(CH,BUF)	glexs(p.lexst,ENT_SYMS,p.skips, CH, BUF)

/* --Error reporting/writing support -------------------------------------- */
#define E_OK	1
#define E_NO	0
#define E_ERR	-1
#define E_EOF	-2

#ifdef DEBUG
#  define pg_er(NUM,SVAL,IVAL,SYSERR) pg_erset(rotnam, NUM, SVAL, IVAL, SYSERR)
#else
/* NOTE: SAME FOR NOW, BEFORE SHIPPING REMOVE THE ROTNAM stuff */
#  define pg_er(NUM,SVAL,IVAL,SYSERR) pg_erset(rotnam, NUM, SVAL, IVAL, SYSERR)
#endif


/* Error list */
#define E_CFIL	1	/* opening cap file */
#define E_CREW	2	/* rewinding cap file */
#define E_UEOF	3	/* Unexpected EOF */
#define E_SYMB	4	/* Not a symbol/name */
#define E_NEQU	5	/* Missing equal sign */
#define E_NTOK	6	/* no token/bad token */
#define	E_NEXT	7	/* next parm must be a string */
#define	E_UCMD	8	/* Unknown special command */
#define E_SCMD	9	/* special command not a word */
#define E_NODV  10	/* Device not found */
#define	E_UTOK	11	/* unknown token in cap file */
#define E_USUB	12	/* undeclared subr name */
#define E_PAS2	13	/* error in pass2 of pgetprg */
#define E_LDED	14	/* already loaded id */
#define E_NSTB	15	/* no stab ever allocated */
#define E_STSX	16	/* sybmbol table index entries (too many) */
#define E_STSY	17	/* sybmbol table symbols (too many) */
#define E_UTMP	18	/* temp label undefined */
#define E_PSYN	19	/* makpar syntax error */
#define E_NVAL	20	/* no value given for capability */
#define	E_NUMB	21	/* bad numeric format */
#define E_UINTR	22	/* unknown intrinsic */
#define E_TMPLX	23	/* too many temp labels */
#define E_TMPLD	24	/* temp label already defined */
#define E_UOFMT	25	/* unknown output format */
#define E_UIFMT	26	/* unknown input  format */
#define E_UOPER	27	/* unknown operator */
#define E_UFFMT 28	/* illegal floating point format */
#define E_NBIG  29	/* name too big for buffer, sorry */
#define E_EBIG  30	/* entry too big for buffer, sorry */
#define E_OVR   31	/* general buffer overflow */


/* ------------------------------------------------------------------------ */

/* Common variables for pgetent(), etc.
 * Note that we will undefine EXTERN at the end of this file
 */
#ifndef COMMON
#  define EXTERN extern
#else
#  define EXTERN
#endif

#define p pg_com
EXTERN struct	_pg_com {

	char	 defbuf[CAP_SIZE] ;	/* Buffer to hold definition */
	char	*defp ;			/*   and pointer therein */
	char	 devnam[NAM_SIZE] ;	/* Device name */
	char	 capfil[NAM_SIZE] ;	/* Cap file to search */
	char	 nxtfil[NAM_SIZE] ;	/* Continue on to the next file */

	Bool	 infile ;		/* reading file or buffer */
	FILE	*fpcap ;		/* Open file pointer */
	char	*bpcap ;		/* Open buffer pointer */
	int	 linnum ;		/* Line number in current file */
	Bool	 ateof ;		/* At eof in current file? */
	int	 lexst[MAX_ST] ;	/* Lex state vector */
	char	 skips[MAX_SKIPS] ;	/* White space characters */
	Bool	 savquo ;		/* Save quotes in strings ? */

	int	 pgerr1 ;		/* Error? */
	int	 pgerr2 ;		/*   error sub value */
	} pg_com ;

/* undefine incase someone else has the same idea... */
#undef EXTERN
/* ------------------------------------------------------------------------ */
