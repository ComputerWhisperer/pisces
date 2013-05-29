/*************************************************************************
 *									 *
 *   Original : MEL         Stanford University        Sept, 1984	 *
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   constant.h                Version 3.3     */
/*   Last Modification : 9/22/88  %U */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*some useful constants for boolean expressions*/
#define TRUE 1
#define FALSE 0

/*some global generally used sizes of arrays and structures*/

/*the maximum number of different materials that may be present*/
#define MAXMAT 10

/*the maximum number of points*/
#define MAXPNT 3000

/*the maximm number of triangles*/
#define MAXTRI 6000

/*define the number of impurities*/
#define MAXIMP 30

/*a useful boltzmann constant value*/
#define KB  8.62e-5
#define kb  8.62e-5

#ifndef MAXFLOAT
#define MAXFLOAT        1.0e38		/* Maximum floating point on Vax. */
#endif
#define LARGE		1.0e19		/* Large but can be manipulated.  */
#define EPS             1.0e-6		/* Typical roundoff for unity.    */
#define PREC            1.0e-12		/* Precision around unity.	  */
#define MAYBE           -1
#define PI              3.1415926535897932
#define LOG2            0.6931471805599453

#define NIL		(char *) 0	/* avoid conflict with sdtio */
