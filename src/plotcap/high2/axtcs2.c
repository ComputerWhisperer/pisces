/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 05 feb 87 (dredge)
 *ff
 * "axtcs2" : Calculate tic-mark positioning.
 *
 * calling sequence:
 *	axtcs2(fmin, fmax, lexpnd, llog, axmin, axmax, axdel, ntics)
 *
 * where:
 *	fmin	- (float) Input: minimum data value for axis.
 *	fmax	- (float) Input: maximum data value for axis.
 *	lexpnd	- (int)   Input: boolean flag, expand axis? 
 *		  1 => axmin <= fmin, etc.
 *		  0 => axmin >= fmin, etc.
 *	lloc	- (int)   Input: boolean flag. 1::axis is logarithmic.
 *	axmin	- (float *) Output: Minimum value for axis label.
 *	axmax	- (float *) Output: Maximum value for axis label.
 *	axdel	- (float *) Output: Inter-tic value.
 *	ntics	- (int   *) Output: Number of unlabeled tics between labels.
 *
 * notes:
 *	1. Still doesn't do examples like 1.00233 to 1.00237 right.
 *of
 * written: Mark Pinto, Conor Rafferty (aug 84)
 * mod # 1: Conor Rafferty (jan 85) Port to C/error checking.
 * mod    : Michael Eldredge (feb 87) cleanup for release.
 * modified:Michael Eldredge (may 88) Don't round up ends for linear case.
 */

#include <math.h>

static float  ln10 = 2.302585092; /* natural log of 10 */
#define LOG_10(V)  (log(V)/ln10)

static int ifloor(float x);
static int iceil(float x);

int axtcs2(double fmin, double fmax, int lexpnd, int llog, float *axmin, float *axmax, float *axdel, int *ntics)
#if 0
/* "axtcs2": Compute some "good" values for axplt2() */
/* INPUT */
    float fmin, fmax;		/* ...minimum/maximum function (data) value*/
    int    lexpnd;		/* ...expand axis? 1 =>  axmin <= fmin etc.
				 *		   0 =>  axmin >= fmin etc.
				 */
    int    llog;		/* ...axis is logarithmic */
/* OUTPUT */
    float *axmin, *axmax;	/* ...min,max labeled tic value */
    float *axdel;		/* ...distance between tics. */
    int    *ntics;		/* ...# of little 'uns between big 'uns. */
#endif
{
      int     iaxlen, i, idim = 4 ;
      static int itic[] = { 3, 4, 4, 4};

      float  scal;
      static float  del[]  = {0.2, 0.5, 1.0, 2.0};
      static float  edge[] = {1.5, 3.5, 7.5, 100.};

/*************
 **  START  **
 *************/

if (llog) {
    if (fmin <= 0) fmin = 1e-38;
    if (fmax <= 0) fmax = 1e-38;
    fmin = LOG_10(fmin) ;
    fmax = LOG_10(fmax) ;
}

/*...Check against bad call. */
if (fmax < fmin) { 		/*...Reversed bounds */
    scal = fmin;
    fmin = fmax;
    fmax = scal;
} else if (fmin == fmax) { 	/*...No range. */
    if (fmin > 0) {
	fmax = 2*fmin;
	fmin = 0;
    } else if (fmin == 0) {
	fmin = -1;
	fmax =  1;
    } else {
	fmax = 0;
	fmin = 2*fmin;
    }
}
/* ...Now have good bounds no matter what we got passed. */

 
  if(!llog) {

/*
 *----------
 *  Linear
 *----------
 */
      *axdel = fmax-fmin;
      i      = ifloor (LOG_10(*axdel));
      scal   = exp (i*ln10);				/* Max 10**i < axdel */
      iaxlen = ifloor (*axdel/scal);		

/* ...Adjust it so we get even spacing */
      for (i=0; i < idim-1; i++)
	if (iaxlen <= edge[i]) break;

/* ...Get specifics */
      *axdel = scal*del[i];
      *ntics = itic[i];

/* ...Stretch bounds a little to avoid nipping ends. */
/*      fmin -= 1e-6*(fmax-fmin);
 *      fmax += 1e-6*(fmax-fmin);
 */
      if (lexpnd) {
         *axmin = ifloor(fmin/ *axdel);
         *axmax = iceil (fmax/ *axdel);
      } else {
         *axmin = iceil (fmin/ *axdel);
         *axmax = ifloor(fmax/ *axdel);
      }

      *axmin *= *axdel;
      *axmax *= *axdel;

      return(0);
/*
 *--------------------
 *  Logarithmic axis
 *--------------------
 *
 *...Assume a mark every decade.  This will get messy if some fool
 *...(excuse me!) wants to plot a VERY large range of numbers, but...
 *
 *...Round up or down depending on expand flag
 */
  } else {
    fmin -= 1e-6*(fmax-fmin);
    fmax += 1e-6*(fmax-fmin);
    if (lexpnd) {
         *axmax = iceil (fmax);
         *axmin = ifloor(fmin);
      } else {
         *axmax = ifloor(fmax);
         *axmin = iceil (fmin);
      }
 
      if(*axmax - *axmin + 1 <= 5) {
         *ntics = -1;
      } else {
         *ntics = -2;
      }

      /* ...axplt2 wants true values, not logs. */
      *axmax = exp (*axmax * ln10);
      *axmin = exp (*axmin * ln10);
      *axdel = 10.0;

      return(0);
  }
}

static int ifloor(float x)
{
    return ( (x >= 0)? (int) x : -1 + (int) x );
}

static int iceil(float x)
{
    return( (x > 0)? 1 + (int) x : (int) x );
}

