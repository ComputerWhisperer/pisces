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
 * Thu Sep 21 10:19:41 PDT 1989 (dredge--stanford)
 *
 * "gplot2": Main gplot user callable routine. This is the do-all  function.
 *
 * written : Michael Eldredge (jan 84)
 * mod     : Michael Eldredge (may 86) Modify for dis18()
 * modified: Michael Eldredge (feb 87) and many times between.
 *	Moved the soft_area stuff below transformation.
 * modified: MJE (jul 87) Some compilers won't load a data only file (ie:
 *	common) from a library, so gp_com.c had be included here instead
 *	of its own file.
 * modified: MJE -- stanford (sep 89) Added the G_SHINT oper.
 */

#include "auxfns.h"
#include "gplot.h"
#include "al_alu18.h"
#include "gp_def.h"

/* Make sure that we have space for the common data */
#define COMMONFILE
#include "gp_vers.h"
#include "gp_com.h"
#undef  COMMONFILE


/* remember the first point in an area define */
static  float  areaX1, areaY1 ;

static void x_kludge() ;
static void gp_setbox() ;

void 
AdjustClip (int Which, int How, double x, double y);

#ifdef ANSI_FUNC

void 
gplot2 (int cmd, int sub, double x, double y)
#else

gplot2(cmd, sub, x, y)
	int   cmd, sub ;
	float x, y;
#endif
	{

	/* start */
#ifndef lint
	defrot("gplot2");
#endif  /*lint*/

	/* set it and then fall through and open the device.
	 *  this is necessary since we want asave or save to
	 *  record the hint in case of a play back.  Ie: it needs
	 *  to be in the meta file and the device has to be open
	 *  for the meta file to receive it.
	 */
	if (cmd == G_SHINT) {
		g_xshint = x ;
		g_yshint = y ;
		}

	if (!g_allset)  {
		if (cmd == G_PEND || cmd == G_GTOA) 
			return;    /* nothing to unset ! */

		(void)gp_setup();
		g_drawmd = G_MSET;   /* setting bits is default mode */
		}

	if (plot_type == T_NULL) return;    /* do nada (prob err on set up) */
	/* ^ do we want to 'gp_setdn()' if cmd .eq. PEND ??? */

	if (asav_on) {
		if (cmd < G_NOT_SAVED) 
			(void)gpwrite(lu_asav, cmd, sub, x, y);/*auto-save on*/
		}

	alu_2old() ;		/* move current vals to previous vals */

	/* Set current values.... */
	g_cmd = cmd ;
	g_sub = sub ;
	g_x1  = x   ;
	g_y1  = y   ;

	/* if just saving calls do that now, before any data manipulation */
	if (plot_type == T_SAVE) {
		save_work();
		return ;
		}

	/* The check on mode is to avoid a subr call even though gpmode will
	 *  do the same check.
	 */
	if (g_agmode != MODE_G && g_cmd != G_GTOA)
		gp_gmode(MODE_G) ;

	(void) x_kludge() ;		/* kludge for rotate & angle calls */

	switch(cmd) {
	case G_DRAW:
	case G_MOVE:
		gp_work();
		g_lastX = x;
		g_lastY = y;
		break;

	case G_DMODE:
	case G_CLEAR:
	case G_PEND:
		(void)gp_work();
		if (cmd == G_PEND) gp_setdn() ;		/* In gp_work ??*/
		break;

	/* Do area define and fill */
	case G_AREA:
		switch (sub) {   /* are we beginning this stuff or what? */
		case G_ON:
			break;	   /* most likely defining, check first */

		case G_BEGIN:	   /* begin an area define */
			areaX1 = x ;		/* just remember the first */
			areaY1 = y ;		/*  point for later */
#ifdef Why
			g_cmd = G_MOVE ;	/* and get us to there */
			g_sub = 0 ;
#endif
#ifdef And_why_this
			if (!soft_area) {g_cmd = G_MOVE; g_sub = 0; }
#endif
			break;

		case G_STOP:
		case G_OFF:
			/* make sure we've returned to beginning of poly */
			if (g_lastX!=areaX1 || g_lastY!=areaY1) {
				g_x1  = areaX1 ;
				g_y1  = areaY1 ;
				g_sub = G_ON ;	   /* slip in an extra cmd */

				gp_work() ;	/* back to start */
				if (!soft_area) g_cmd = G_NULL ;
				else		g_sub = G_STOP ;
				}

			break;
			}/*of AREA switch on sub */

		gp_work() ;
		g_lastX = x;
		g_lastY = y;
		break;

	case G_FILS:	/* select area fill patter/colour */
		if (!soft_area) gp_work();
		else	        gp_arset(sub) ;
		break;

	/* scale and translation stuff are easy... */
	case G_TRANS:
	case G_SCALE:
		gp_xfrm(cmd,sub, x,y, 0.0);
		break;

	case G_ANGLE:		/* handled by "x_kludge" */
	case G_ROTATE:
		break;

	case G_CLIPL:    /* low clip values */
	case G_CLIPH:    /* high clip values */
		AdjustClip(cmd, sub, x, y);   /* fix up clipping vals */
		break;

	case G_ASAVE:   /* Auto-save mode (on, off, stop) */
		if (sub == G_ON && lu_asav != -1) asav_on = T;
		else if (sub == G_OFF)           asav_on = F;
		else if (sub == G_STOP) {
			if (lu_asav != -1) gpclose(lu_asav);
			lu_asav = -1;
			asav_on = F;
			}/*also done int gp_setdn() */

		break;

	case G_LINE:    /* change current line type */
		/* Line type 0 maybe should be: set DMODE == M_CLR? 
		 *  but for now, just ignore it
		 */
		if (g_sub > 0)
			(void) gp_work() ;
		break;
	case G_PEN:
		if (g_sub > 0) (void) gp_work() ;

	/* Reset some (or all values) */
	case G_RESET:
		g_cmd = sub ;
		switch (sub) {
		case 0:
			break;
		case G_ROTATE:
		case G_ANGLE:
		case G_SCALE:
		case G_TRANS:
			gp_reform(xfrm_ord, X_DEFS);
			break;
		case G_CLIPH:
		case G_CLIPL:
			AdjustClip(g_cmd, G_RESET, x, y) ;
			break ;
		case G_LINE:
		case G_PEN:
			g_sub = 1 ;
			(void)gp_work() ;
			break ;
		case G_FILS:
			g_sub = 0 ;
			if (!soft_area) gp_work();
			else	        gp_arset(/*sub=*/0) ;
			break ;
		default:		/* misc others */
			if (sub == G_DMODE) g_sub = G_MSET ;
			else sub = -1 ;

			if (sub != -1) (void)gp_work() ;
			break ;
			}/*of switch on sub-command */
		break ;

	case G_DABLE:
		switch (sub) {
		case G_AREA:
			soft_area = T ;
			break ;
		case G_LINE:
			soft_line = T ;
			break;
		case G_PEN:
			soft_pen  = T ;
			break;
			}
		break ;
			

	case G_REVCO:	/* Ignore. this is just to be gotten */
	case G_MARK:    /* just ignore marks, there're for files */
	case G_SHINT:	/* already taken care of */
		break;

	case G_NULL:	/* null effect command - for outboard subs to setup */
		(void) gp_work() ;
		break;

	/* could be some other less known command, send it on for more
	 *  checking by the approp. plot type routine.
	 */
	default:
		(void) gp_work();
		break;

		}/* of switch */

	}/* of gplot2 */


/* "AdjustClip" : adjust clipping values according to Which and How 
 *     	If values are outside  bounds of physical screen, set to phyical
 *	    screen.
 *  Which : {G_CLIPH , G_CLIPL}
 *  How   : {G_RESET , G_ONLOG , G_ONPYS }
 */
/*static --called from gp_pdev.c */
#ifdef ANSI_FUNC

void 
AdjustClip (int Which, int How, double x, double y)
#else

AdjustClip(Which, How, x, y)
	int	Which, How ;
	float	x, y ;
#endif
	{

	/* just change dobreak flag.. */
	if (How == G_DOBREAK || How == G_NOBREAK) {
		g_dobreak = (How == G_DOBREAK) ;
		return ;
		}
		
	if (Which == G_CLIPH) {    /* High bound of clipping */
		if (How == G_RESET) {
			g_clphx = C.CLPHX;
			g_clphy = C.CLPHY;
			g_logclp = F ;		/* clip on physical axis */
			gp_setbox(g_phsbox, g_clplx,g_clply, g_clphx,g_clphy);
			gp_setbox(g_logbox, g_clplx,g_clply, g_clphx,g_clphy);
			}

		else {
			if (How == G_ONPHS) {
				g_clphx = min( x, C.CLPHX); 
				g_clphx = max(g_clphx , g_clplx);
				g_clphy = min( y, C.CLPHY);  
				g_clphy = max(g_clphy , g_clply);
				g_logclp = F ;
				}
			else {
				g_clphx = x ;
				g_clphy = y ;
				g_logclp = T ;
				}
			gp_setbox(g_logclp?g_logbox:g_phsbox,
				g_clplx, g_clply, g_clphx, g_clphy) ;

			}
		}


	else {                   /* low bound of clipping */
		if (How == G_RESET) {
			g_clplx = C.CLPLX ;
			g_clply = C.CLPLY ;
			g_logclp = F ;		/* clip on physical axis */
			gp_setbox(g_phsbox, g_clplx,g_clply, g_clphx,g_clphy);
			gp_setbox(g_logbox, g_clplx,g_clply, g_clphx,g_clphy);
			}
		
		else {
			if (How == G_ONPHS) {
				g_clplx = max( x, C.CLPLX ); 
				g_clplx = min(g_clplx , g_clphx);
				g_clply = max( y, C.CLPLY );
				g_clply = min(g_clply , g_clphy);
				g_logclp = F ;
				}
			else {
				g_clplx = x ;
				g_clply = y ;
				g_logclp = T ;
				}
			gp_setbox(g_logclp?g_logbox:g_phsbox,
				g_clplx, g_clply, g_clphx, g_clphy) ;
			}
		}
	}

/* fill in the box with the low and high points */
#ifdef ANSI_FUNC

static void 
gp_setbox (fpoint2d box[], double x0, double y0, double x1, double y1)
#else

static void
gp_setbox(box, x0, y0, x1, y1)
	fpoint2d box[] ;
	float	 x0, y0, x1, y1 ;
#endif
	{
	float	t ;
	if (x0 > x1) { t = x0; x0 = x1 ; x1 = t ; }	/* just in case */
	if (y0 > y1) { t = y0; y0 = y1 ; y1 = t ; }

	box[0].x = x0 ;
	box[0].y = y0 ;
	box[1].x = x0 ;
	box[1].y = y1 ;
	box[2].x = x1 ;
	box[2].y = y1 ;
	box[3].x = x1 ;
	box[3].y = y0 ;
	}



/* "x_kludge" : This is a Kludge so that calling sequence can still have only
 *  4 params in gplot2 (cmd,sub,x,y) and not add a fifth for rotation angles.
 *   The extra parameter is only a problem when saving gplot 
 *    calls.  There may be a couple of angles used but that means
 *     each saved record will need the extra float saved.
 * So saving happens with point and angle being separate calls to
 *  gplot, but they get implemented (here) at the same time.
 */
#ifdef ANSI_FUNC

static void 
x_kludge (void)
#else

static void
x_kludge()
#endif
	{
	static	Bool   rot_todo = F , ang_todo = F;
	static	float  xrot=0.0, yrot=0.0, arot=0.0;   /* rotation factors */

	switch (g_cmd) {
	case G_ANGLE:
		if (ang_todo) {		/* Don't skip a double call */
			gp_xfrm(G_ROTATE,0,0.0,0.0, arot) ;
			arot = 0.0 ; ang_todo = F ;
			}
		if (rot_todo) {
			gp_xfrm(G_ROTATE,0,xrot,yrot, g_x1);
			xrot = yrot = 0.0;
			rot_todo = F;
			}
		else {
			arot = g_x1;     /* wait till we know for sure */
			ang_todo = T;
			}
		break ;

	case G_ROTATE:
		if (rot_todo) {		/* Don't skip a double call */
			gp_xfrm(G_ROTATE,0, xrot, yrot, 0.0) ;
			xrot = yrot = 0.0 ; rot_todo = F ;
			}
		if (ang_todo) {
			gp_xfrm(G_ROTATE,0,g_x1,g_y1, arot);
			arot = 0.0;
			ang_todo = F;
			}
		else {
			xrot = g_x1;     /* wait till we know for sure */
			yrot = g_y1;
			rot_todo = T;
			}
		break;
	default:
		if (rot_todo || ang_todo) {
			gp_xfrm(G_ROTATE,0, xrot,yrot,arot);
			xrot = yrot = arot = 0.0 ;
			rot_todo = ang_todo = F;
			}
		break ;
		}/*of switch on g_cmd */
	}
