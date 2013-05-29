/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* "gp_def.h" : various symbol definitions file for GPLOT software.
 * Wed Sep 20 11:55:00 PDT 1989 (dredge--stanford)
 */

/* Stuff that may want to be changed.--------------------------------------*/

/* Locally "Stats" */
#ifndef STATS_FILE
#   define STATS_FILE "Stats"
#endif

/* environment variables */
/* were PLOTCAP GPLOTCAP */
#define LOC_PCAP "PLOTCAP"
#define LOC_GCAP "GPCAP"
#define DEF_DEV  "DEFPDEV"
#define DEF_FIL  "DEFPFIL"

/* constants */
#define MAXPROG   1024		/* pgetprg program size */
#define MAXIDAT   1024		/* pgetprg input char data size */
#define OSIZDEF   2048		/* Default output buffer size */
#define ISIZDEF     80		/* Default input  buffer size */
#define NAMRLN     256		/* File name max length */

#define STABSIZE   128		/* Default Symbol table size */
#define STABENTS    20		/* Default Number of Symbol table enties */

/* default plot file output will be the tty output, and input from tty */
#define LUODEF  1
#define LUIDEF	0

/* Pi over 180 */
#define PIO180 0.01745329252

#define NHUGE	32000		/* some big int */

/* End of may what to change stuff.----------------------------------------*/

/* Interface to lowest level: how to do a given sequence */
#define  docmd(E)	(void)alu12(E)

/* Gplot values for the alu */
#define	g_cmd	g_ivar1[CVAR]
#define	g_sub	g_ivar1[SVAR]
#define	g_i1	g_ivar1[IVAR]
#define	g_j1	g_ivar1[JVAR]
#define	g_i0	g_ivar0[IVAR]
#define	g_j0	g_ivar0[JVAR]

#define g_x1	g_fvar1[XVAR]
#define g_y1	g_fvar1[YVAR]


/* plot types */
#define  T_NULL    0
#define  T_VECT  0x01
#define  T_RAST  0x02
#define  T_SAVE  0x04
#define  T_DEBUG 0x08
#define  T_ANY   0x0f	/* Bit-wise OR of all of the above */

/* Modes (like those set with G_GTOA and G_ATOG) */
#define	 MODE_G	  G_ATOG
#define  MODE_A   G_GTOA

/* xform order types */
#define	 XF_R	01
#define  XF_S   02
#define  XF_T   03

#define  XFORD(A,B,C)  ( ((A)<<6) | ((B)<<3) | (C) )
#define  XF_DEF XFORD(XF_T, XF_R, XF_S)

#define X_NEW	0	/* False: gp_reform() from common g_ratax, etc. */
#define	X_DEFS	1	/* True:  gp_reform() from the defaults in C. */

struct _ipoint2d {         /* 2d int   type point */
	int x,y;
	};
#define ipoint2d  struct _ipoint2d

struct _fpoint2d {         /* 2d float type point */
	float x,y;
	};
#define fpoint2d  struct _fpoint2d

#define  round(F)	((int)( (F) + 0.5) )

