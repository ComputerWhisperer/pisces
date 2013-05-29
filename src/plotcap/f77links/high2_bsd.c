/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

#include "nwstrc.c"
#include <string.h>
#include <stdlib.h>

/*
 * Thu Oct  5 10:47:02 PDT 1989 (dredge--stanford)
 *
 * high2: Library mappings from f77 high2(3L) calls to the C versions.
 *
 * NOTE: The f77 version (here) creates its own null terminated copy of
 *	strings. (Using 'nwstrc()')
 *
 * written: Michael Eldredge (nov 85)
 * mod    : MJE (dec 85)  Added call for symbl2() and symb2().
 */

/*subroutine*/
faxpt2_(xst, yst, len, axang, Fval, Eval, Flab, Llab, ltic, utic, labht,
		tlang, tloc, tang, tht, tdp, title, axtht, lform, vform, 
			L_title, L_vform)
	float	*xst, *yst, *len, *axang ;
	float	*Fval, *Eval, *Flab, *Llab ;
	float	*ltic ;
	long	*utic ;
	float	*labht , *tlang ;
	long	*tloc ;
	float	*tang, *tht, *tdp ;
	char	*title ;
	float	*axtht ;
	long	*lform ;
	char	*vform ;
	long	 L_title, L_vform ;	/* Declared sizes from f77 */
	{

	char	*tp , *vp, *nwstrc()  ;

	tp = nwstrc(title, L_title ) ;
	vp = nwstrc(vform, L_vform ) ;

	axplt2(*xst, *yst, *len, *axang, *Fval, *Eval, *Flab, *Llab, 
		*ltic, (int)*utic, *labht, *tlang, (int)*tloc, *tang, *tht, 
			*tdp, tp, *axtht, (int)*lform, vp ) ;

	free(tp) ;
	free(vp) ;
	}

/* "axtcs2"  ... */
faxtc2_(fmin, fmax, lexpnd, llog, axmin, axmax, axdel, ntics)
	float	*fmin, *fmax ;
	long	*lexpnd, *llog ;
	float	*axmin, *axmax, *axdel ;
	long	*ntics ;
	{
	int	 nt ;

	axtcs2(*fmin, *fmax, (int)*lexpnd, (int)*llog, axmin, axmax,
		axdel, &nt) ;

	*ntics = (long)nt ;
	}

/* "numb2" ... */
long int
fnumb2_(x0, y0, val, size, ang, hornt, wornt, form, L_form)
	float	*x0, *y0, *val, *size, *ang ;
	float	*hornt, *wornt ;
	char	*form ;
	long	 L_form ;
	{

	long	 n ;
	char	*fp , *nwstrc() ;
	
	fp = nwstrc(form, L_form) ;

	n = (long)numb2(*x0, *y0, *val, *size, *ang, *hornt, *wornt, fp) ;

	free(fp) ;

	return n ;
	}


/* "plota" ... */
fplota_(x, y, pen)
	float	*x, *y ;
	long	*pen ;
	{

	plota(*x, *y, (int)*pen ) ;
	}


/* "symbl2" ... */
fsymb2_(x0, y0, cbuf, cnt, hsize, wsize, angle, hornt, wornt, L_cbuf)
	float	*x0, *y0 ;
	char	*cbuf ;
	long	*cnt ;
	float	*hsize, *wsize, *angle, *hornt, *wornt ;
	long	 L_cbuf ;	/* Not used since cnt is passed in */
	{
#ifdef lint
	*cnt = L_cbuf; 
#endif

	symbl2(*x0, *y0, cbuf, (int)*cnt,*hsize,*wsize,
				*angle, *hornt, *wornt);
	}

/* "symb2" ... */
fsym2_(x0, y0, cbuf, cnt, size, angle, hornt, wornt, L_cbuf)
	float	*x0, *y0 ;
	char	*cbuf ;
	long	*cnt ;
	float	*size, *angle, *hornt, *wornt ;
	long	 L_cbuf ;	/* Not used since cnt is passed in */
	{
#ifdef lint
	*cnt = L_cbuf; 
#endif

	symbl2(*x0, *y0, cbuf, (int)*cnt, *size, *size,
			*angle, *hornt, *wornt);
	}
