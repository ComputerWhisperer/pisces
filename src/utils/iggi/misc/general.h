/* $Header: /users/suprem/ig2/misc/RCS/general.h,v 1.1 85/05/17 14:32:31 conor Exp $ */

/*----------------------------------------------------------------------
 *
 * General definitions, used by all my C programs.
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Nov 84.
 *---------------------------------------------------------------------*/

/***************************************************
 *                                                 *
 *            Constants.                           *
 *                                                 *
 ***************************************************/

#define MAXFLOAT        1.0e38		/* Maximum floating point on Vax. */
#define LARGE		1.0e19		/* Large but can be manipulated.  */
#define EPS             1.0e-6		/* Typical roundoff for unity.    */
#define PREC            1.0e-12		/* Precision around unity.	  */
#define TRUE            1
#define FALSE           0
#define MAYBE           -1
#define PI              3.1415926535897932
#define LOG2            0.6931471805599453

#define NL		(char *) 0

#define DIRSIZ		  80    /* Longest path name tolerated. */


/***************************************************
 *                                                 *
 *            Standard macros.                     *
 *                                                 *
 ***************************************************/
#define FOR(i,l,u) for(i = (l); i <= (u); i++)

/***************************************************
 *                                                 *
 *            Standard procedures.                 *
 *                                                 *
 ***************************************************/
double dmax(), dmin(), dabs();
int    imax(), imin(), iabs();
int    iflor(), iceil();
#if 0
int round(double);
#endif
int    strequ(), mod();

