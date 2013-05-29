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
 * Thu Apr  5 09:42:49 PDT 1990 (dredge--stanford)
 * routines: gp_xfrm , gp_reduct3 , xform , gp_unxfrm
 *
 * Modified: Ken Kosai -- Hughes Santa Barbara Research
 *	needed declaration of cos() in gp_unxfrm().
 */

/* date: 23 may 84 (mje)
 * "gp_xfrm" : Plot transformation link to the gplot package.
 *
 * usage:
 *	gp_xfrm(cmd,sub, xval, yval, aval);
 *
 * where:
 *	cmd	- (int) Command to gp_xfrm. One of:
 *		  G_SCALE    - set scaling factors.
 *		  G_TRANS    - set translation factors.
 *		  G_ROTATE   - set rotation factors.
 *	sub	- (int) Sub-command for some commands.
 *	xval	- (float) Usually the transformation factor for x-axis.
 *	yval	- (float) Usually the transformation factor for y-axis.
 *	aval	- (float) Usually the rotation angle.
 *
 * notes:
 *	> This has been separated from 'gplot2' to avoid slowing 'gplot2'
 *	  with extra checks and parameters.
 *	> The form is likely to change.
 *
 * written: Michael Eldredge (may 84)
 */

#include <stdio.h>
#include "auxfns.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"

gp_xfrm(cmd, sub, xval, yval, aval)
	int   cmd, sub ;
	float xval, yval, aval;
	{
	double sr, cr, xr, yr;
	double InRads;
	static double pio180 = PIO180 ;   /* pi / 180 */
	double sin(), cos();


	/* start of gp_xfrm */
	xr = xval;
	yr = yval;
#ifdef lint
	if (sub == cmd) return ;
#endif

	/* find out which set of constants we need to redo */
	switch(cmd) {
	case G_SCALE:
		gp_reduct3(  xr ,  0.0 ,  0.0 ,    /* scaling */
			 0.0 ,   yr ,  0.0  );
		/*       0.0 ,  0.0 ,  1.0  */
		break;

	case G_TRANS:                           /* translation */
		gp_reduct3( 1.0 ,  0.0 ,   xr ,
			 0.0 ,  1.0 ,   yr  );
		/*       0.0 ,  0.0 ,  1.0  */
		break;

	case G_ROTATE:                          /* rotation */
		InRads = (double)aval * (-pio180);
		cr = cos(InRads);
		sr = sin(InRads);

		xr = yr = 0.0;  /* incase x & y are zero */
		if (xval != 0.0 || yval != 0.0) {
			xr = -cr*xval - sr*yval + xval;
			yr =  sr*xval - cr*yval + yval;
			}

		gp_reduct3(  cr ,   sr ,  xr ,
			 -sr ,   cr ,  yr );
		/*       0.0 ,  0.0 ,  1.0  */
		break;

		}/* of switch */

	}/* of gp_xfrm */


/* "gp_reduct3" : A quick way to reduce the matricies (multiply them 
 *	together) since last row is always the same. 
 *
 * This routine does the following multiplication:
 *	M = MN
 * Where 'M' is the current tranformation matrix and 'N' is the latest
 * set of tranformation constants.  'M' actually is:
 *	M: /                           \
 *	   | XXconst  XYconst  Xconst  |
 *	   | YXconst  YYconst  Yconst  |
 *	   |    0        0       1     |
 *	   \                           /
 *
 *	N: /                           \
 *	   |    a        b       c     |
 *	   |    d        e       f     |
 *	   |    0        0       1     |
 *	   \                           /
 */
/*static: nope! needed by gpmisc() */
gp_reduct3(a, b, c,    d, e, f)
	double a,b,c, d,e,f;   /* constants for Rows 1 & 2 */
	{
	double txx, txy, tyx, tyy;    /*temp values until conversion is done */

	txx      = g_XXcon * a  +  g_XYcon * d;
	tyx      = g_YXcon * a  +  g_YYcon * d;

	txy      = g_XXcon * b  +  g_XYcon * e;
	tyy      = g_YXcon * b  +  g_YYcon * e;

	g_Xcon  = g_XXcon * c  +  g_XYcon * f  + g_Xcon;
	g_Ycon  = g_YXcon * c  +  g_YYcon * f  + g_Ycon;

	g_XXcon = txx ;
	g_YXcon = tyx ;

	g_XYcon = txy ;
	g_YYcon = tyy ;
	}


/* "xform" : transform data using conants saved in common.
 *       Given an old point (Xold,Yold), scale/rotate/translate it
 *       using the conants defined by the 'redo_con' routine.
 *       The new point will be (Xnew,Ynew).
 *
 * date: 12 jan 87 (mje)
 *
 * calling sequence:
 *       xform(&Pnew, &Pold);
 *
 * written:  Michael Eldredge (oct 83)
 * modified: Michael Eldredge (jan 87) Take fpoint2d structs now...
 */

/*xform(Xnew,Ynew, Xold, Yold)*/
xform(Pnew, Pold)
	fpoint2d *Pnew, *Pold ;
	{
	float	 xtemp ;	/* HPUX likes this if Pold->x::Pnew->x */

	xtemp   = Pold->x * g_XXcon  +  Pold->y * g_XYcon  + g_Xcon;
	Pnew->y = Pold->x * g_YXcon  +  Pold->y * g_YYcon  + g_Ycon;
	Pnew->x = xtemp ;
	}

/* "uform" : inverse of "xform" */
uform(Xold,Yold, Xnew, Ynew)
	float	*Xold, *Yold, Xnew, Ynew ;
	{
	float	 den ;

	den = (g_XXcon * g_YYcon) - (g_XYcon * g_YXcon) ;

	*Xold = (g_YYcon*Xnew - g_XYcon*Ynew + g_XYcon*g_Ycon - g_Xcon*g_YYcon)
		/ den ;

	*Yold = (g_XXcon*Ynew - g_YXcon*Xnew + g_Xcon*g_YXcon - g_XXcon*g_Ycon)
		/ den ;
	}


/* date: 10 feb 87 (mje)
 *
 * "gp_unxfrm" : Extract the transformation factors from the current
 *	transformation constants.  Note that the current constants may
 *	have been built up in a more complex manner or in a different
 *	order that the extraction uses.  But if the extracted factors
 *	are reissued (in the specified order), the resulting transformation
 *	constants will be the same.  Thus, here we extract an equivalent
 *	set of factors from the current set of constants.
 *
 * calling sequence:
 *	gp_unxfrm(order)
 *
 * where:
 *	order	- (int) An integer specifying the equivalent
 *		  matrix order.  The bits[8-6] are the first, bits[5-3]
 *		  the next and the lowest bits [2-0] the last.
 *		  Thus if the transformation calls are
 *		  issued in this order with the extracted factors, the
 *		  resulting set of constants will be the same.
 *		  Order is given by the characters 'T' : translation,
 *		  'S' : scaling, 'R' : rotation.  Only one of each 
 *		  can be used (and each must be used).
 *  NOTE: that for now (and maybe forever) the only order that will work
 *	is "TRS".
 *
 * returns:
 *	Actual order used to undo the matrix (which can be given to gp_reform).
 *
 * written:  Michael Eldredge (may 84)
 * modified: Michael Eldredge (feb 87) Bug fix in scale undo-ing.
 */
/* interesting way to get this, huh? */
#define PI (PIO180*180.)

int
gp_unxfrm(order)
	int order;
	{
	double arad ;	/* angle in radians */
	double t ;
	double atan(), sin(), cos() ;
	int	did_order = XFORD(XF_T, XF_R, XF_S) ;

	/*
	if (order == X_TRS) {
	*/

	g_tranx  = g_Xcon ;
	g_trany  = g_Ycon ;

	if (g_YYcon == 0.0) {
		arad = PI/2.  ;
		g_angle = 90. ;
		}
	else {
		arad = atan((double)(g_XYcon/g_YYcon)) ;
		g_angle = arad / PIO180 ;
		}
	g_angle = -g_angle ;
	g_rotax = g_rotay = 0.0 ;	/* always! */

	t = sin(arad) ;
	if (t != 0.0) g_scaly = g_XYcon / t ;
	else	      g_scaly = g_YYcon / cos(arad) ;

	if (g_YYcon != 0.0) g_scalx =  g_XXcon/g_YYcon * g_scaly ;
	else		    g_scalx = -g_YXcon/ t ;
	
	xfrm_ord = order;

	return did_order ;
	}


/* "gp_reform" : reset xformation stuff
 * 	if defs==T (defs==X_DEFS) then reset to the original values.
 *	else take the current (common) values of g_.... and set them
 *		in calling gplot2() so that the values will go to save files.
 *	Use the given order.
 */
gp_reform(order, defs)
	int  order;
	int  defs ;	/* use the defaults OR those from unxfrm */
	{
	int   i, xwch[3] ;

	g_XXcon = 1.0 ;  g_XYcon = 0.0 ;  g_Xcon = 0.0 ;
	g_YXcon = 0.0 ;  g_YYcon = 1.0 ;  g_Ycon = 0.0 ;

	/* set in 'original' values.  Most usually, nothing */
	if (order == 0) return;   /* don't bother */

	xwch[0] = (order & 0700) >> 6;  /* extract the orders */
	xwch[1] = (order & 0070) >> 3;
	xwch[2] = (order & 0007)     ;

	for (i = 0; i < 3; i++) {

		switch (xwch[i]) {
		case XF_R:  /* rotate */
			if (defs) gp_xfrm(G_ROTATE,0,C.ROTAX,C.ROTAY,C.ANGLE);
			else {
				gplot2(G_ROTATE,0,g_rotax, g_rotay) ;
				gplot2(G_ANGLE ,0,g_angle, 0.0) ;
				}
			break;

		case XF_S:  /* scale  */
			if (defs) gp_xfrm(G_SCALE ,0, C.SCALX, C.SCALY, 0.0);
			else	  gplot2 (G_SCALE ,0, g_scalx, g_scaly) ;
			break;

		case XF_T:  /* translate */
			if (defs) gp_xfrm(G_TRANS, 0, C.TRANX, C.TRANY, 0.0);
			else	  gplot2 (G_TRANS ,0, g_tranx, g_trany) ;
			break;
			}/* of switch */
		}
	}
