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
 * Wed Jan 31 16:20:02 PST 1990 (dredge--stanford)
 * "doaxis.c" : set up and draw a pair of axises using 'axplt'.  Should
 *              simplify doing axis plotting.  Several default values are
 *              assumed. 'which' is either character 'x' or 'y' to decide
 *	        which pair to plot.
 *
 * written: Michael Eldredge (oct 83)
 * mod    : MJE (sep 84)
 * mod    : MJE (dec 85) Added conor's axtcs() routine.
 * mod    : MJE (apr 86) Labels/tics based on average axis length
 */

/* axplt2 values. See documentation on axplt2 for what they all mean. */
/* some values that shall remain set .... */
#define X_ANG      0.0
#define Y_ANG     90.0
#define X_TLANG    0.
#define Y_TLANG  -90.0
#define X_TLOC     0
#define Y_TLOC     1
#define X_TANG    90.
#define Y_TANG    90.
#define X_LFORM   -1
#define Y_LFORM    1

/* linear(I) and log(R) */
#define I_UTIC     3
#define R_UTIC    -1

#define VFORM   "g"

/* KLUDGE for YOSHI */
float	Lab_size  = 0.0 ;
float	Tlab_size = 0.0 ;


doaxis(which, x0,y0, x1,y1, min, max, label, logplot, avlen)
	char   which, *label;
	float  x0,y0, x1,y1, min, max;
	char   logplot;
	float  avlen ;
	{

	float  xst1,yst1, xst2, yst2;
	float  len, axang ;
	float  Fval, Eval, Flab, Llab ;
	float  ltic ;
	int    utic ;
	float  labht, tlang ;
	int    tloc ;
	float  tang, tht, tdp;
	float  axtht ;
	int    lform ;
	char  *vform ;

	float  t_hite, tl_hite, l_hite ;  /* tic, tic label, label/title */

	double pow();


	/* always set these parms up */
	t_hite = 0.020 * avlen ;
	tl_hite= 0.025 * avlen ;
	l_hite = 0.025 * avlen ;
	/*
	t_hite =  0.10 ;
	tl_hite=  0.15 ;
	l_hite =  0.15 ;
	*/

	Fval   = min ;
	Eval   = max ;
	Flab   = min ;
	Llab   = max ;

	labht  = tl_hite ;
	axtht  = l_hite  ;
	vform  = VFORM   ;


	/* do things a little differently for x-axis or y-axis */
	if (which == 'x') {
		xst1  =  x0;
		yst1  =  y0;
		xst2  =  x0;
		yst2  =  y1;
		len    =  x1 - x0;

		axang  = X_ANG ;

		tlang  = X_TLANG ;
		tloc   = X_TLOC ;
		tang   = X_TANG ;
		tht    = 0.0  ;
		tdp    = t_hite ;

		lform  = X_LFORM;
		}

	else if (which == 'y') {
		xst1  =  x0;
		yst1  =  y0;
		xst2  =  x1;
		yst2  =  y0;
		len   =  y1 - y0;

		axang  = Y_ANG ;

		tlang  = Y_TLANG ;
		tloc   = Y_TLOC ;
		tang   = Y_TANG ;
		tht    = t_hite ;
		tdp    = 0.0    ;

		lform  = Y_LFORM;
		}

	else  return;               /* only can do 'x' or 'y' */

	/* do things a little differently if logplot or linear */
	if (logplot) {
		ltic  =  10.0;         /* value between labeled tics */
		utic  =  R_UTIC ;      /* number of little tics (-1:logplot) */
		}
	else {
		/* Use Conor's tick mark guessing routine. */
		axtcs2(Fval, Eval, 0, logplot, &Flab, &Llab, &ltic, &utic) ;
		}

	/* KLUDGE for YOSHI */
	if (Tlab_size > 0.0) labht = Tlab_size ;
	if (Lab_size > 0.0)  axtht = Lab_size ;

	/* now plot the left (for y) or bottom (for x) axis */
	axplt2(xst1, yst1, len, axang, Fval, Eval, Flab, Llab, ltic, utic,
	       labht, tlang, tloc, tang, tht, tdp, label, axtht, lform, vform);

	/* things not to happen in other plot */
	labht = 0.0 ;
	axtht = 0.0 ;

	/* and the right (for y) or top (for x) axis */
	axplt2(xst2, yst2, len, axang, Fval, Eval, Flab, Llab, ltic, utic,
	       labht, tlang, tloc, tang, tht, tdp, label, axtht, lform, vform);
	}
