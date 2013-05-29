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
 * Fri Feb  2 09:28:26 PST 1990 (dredge--stanford)
 *
 * "gp_work" : Do the work of sending out sequences to the plot device/file.
 *		We do general work first and then call the correct specific
 *		worker routine.
 *
 *
 * written:  Michael Eldredge (jan 84)
 * modified: Michael Eldredge (sep 84)
 * modified: Michael Eldredge (may 86) Post is now mapped to GtoA.
 * modified: MJE & Mark Law (Dec 86) 
 *	+++ added the last_ok flag to make sure that clipped areas get drawn
 *	correctly.  If any MOVE was done out of bounds then P_last is invalid
 *	and we move to the new point. Else when we finally come back in, draw
 *	to the new point.
 *	+++ Fixed posting (quit doing it) in save_work().
 *	+++ save_work() uses gp_fmp routines for buffering....
 * modified: Michael Eldredge (jan 87) Fix clipping Bug.  Quite the wrong
 *	methodology... Must clip before tranforming (else it doesn't work on
 *	a rotated axis). Still a bug, but.... (if you leave one side and 
 *	return through another, it won't catch up correctly).
 * modified: Michael Eldredge (feb 87) Moved the soft_area below tranformation.
 * modified: MJE (mar 87) fixed rast_work:AREA bug -- didn't do anything.
 * modified: MJE (aug 88)
 *	+++ Redid clipping (again).
 *	+++ Added software pen style (thick pens).
 * modified: MJE (jan 90) save previous point (P0) for Move,draw,area
 *	only when we do a move,draw,or area.  Fixed the line-dragged-from-
 *	(0,0) problem manefested during, say: MV,DR,DR,ATOG,DR,DR
 *	During the ATOG, P0 was set to (0,0) to the next draw would see
 *	that we haven't done a move and would move to P0 = (0,0) then draw.
 */

#include "auxfns.h"
#include "gp_mis12.h"
#include "al_alu18.h"
#include "gplot.h"
#include "gp_def.h"
#include "gp_com.h"

#define Clippable_Command(C) ((C)==G_DRAW || (C)==G_MOVE || (C)==G_AREA)

static phys_work(struct _fpoint2d *P_1, struct _fpoint2d *P_0, int cmd);
static vect_work(void);
static rast_work(void);

static	fpoint2d  P0	= { 0.0, 0.0 } ;

gp_work(void)
{
	fpoint2d  P1 ;
	fpoint2d  Pa, Pb ;	/* temp points */
	int       nclip ; 
	static fpoint2d l_Plast ;
	static Bool     l_wentout = F ;
	static Bool     l_lastok  = F ;
#ifndef lint
	defrot("gp_work");
#endif
	P1.x = g_x1 ;
	P1.y = g_y1 ;

	/* if drawing or moving, transform and clip values */
	if (Clippable_Command(g_cmd)) {
		if (g_logclp) {		/* clip on logical axis */
			nclip = fclip(&Pa, &Pb, &P1, &P0, g_logbox) ;
			if (nclip < 0) {	/* all out */
				l_wentout = T ;
				l_lastok = l_lastok && (g_cmd != G_MOVE) ;
				goto see_ya ;
				}

			/* else nclip is >= 0, so we may need to catch up */
			if (l_wentout) {	/* we were out; coming in */
				int	l_cmd = g_cmd ;

				if (!l_lastok) l_cmd = G_MOVE ;
				phys_work(&Pb, &l_Plast, l_cmd) ;
				l_lastok = l_wentout = F ;
				}
			
			if (nclip > 0) {	/* going out */
				l_Plast = Pa ;
				l_lastok = g_cmd != G_MOVE ;
				l_wentout = T ;
				}

			phys_work(&Pa, &Pb, g_cmd) ;
			}

		/* don't bother with clipping on the logical axis */
		else {
			phys_work(&P1, &P0, g_cmd) ;
			}

		/* only update these if they have been used */
	    see_ya:
		P0.x = P1.x ;
		P0.y = P1.y ;
		}

	/* do the work for the other types of commands.*/
	else simp_work(&P1, &P0);

	/* NOTE: see_ya: P0 <- P1 was here, but we don't want to update
	 *	P0 if we just did an ATOG, for example.
	 */
	}

static phys_work(struct _fpoint2d *P_1, struct _fpoint2d *P_0, int cmd)
	{

	fpoint2d	P_to , P_fr ;
	fpoint2d	Pa, Pb ;
	int		nclip ;
	int		real_cmd = g_cmd ;
	static fpoint2d p_Plast ;
	static Bool     p_wentout = F ;
	static Bool 	p_lastok  = F ;

	g_cmd = cmd ;

	/* Logical to Physical conversion */
	xform(&Pa, P_1) ;
	xform(&Pb, P_0) ;

	nclip = fclip(&P_to, &P_fr, &Pa, &Pb, g_phsbox) ;
	if (nclip < 0) {
		p_wentout = T ;
		p_lastok = p_lastok && (cmd != G_MOVE) ;
		goto see_ya ;
		}

	if (p_wentout) {
		int	save_cmd = g_cmd ;

		if (!p_lastok) g_cmd = G_MOVE ;
		simp_work(&P_fr, &p_Plast) ;
		g_cmd = save_cmd ;
		p_lastok = p_wentout = F ;
		}

	if (nclip > 0) {	/* going out */
		p_Plast = P_to ;
		p_lastok= cmd != G_MOVE ;
		p_wentout= T ;
		}

	simp_work(&P_to, &P_fr) ;

    see_ya:
	g_cmd = real_cmd ;
	}


/* "simp_work": Do the work, after scaling, etc... We may need to call
 *	this elsewhere (not in this file,ie: from gp_area() ) so
 *	it cannot be declared as a static function.
 */
simp_work(struct _fpoint2d *Pto, struct _fpoint2d *Pfr)
{
	register int	n, dir, pn ;
	int	 i1, j1, i0, j0 ;
	int	 di, dj ;
	int	 isave, jsave ;
	Bool	 horz_pen ;

	/* Set the current points for the alu */
	g_x1 = Pto->x ;
	g_y1 = Pto->y ;

	g_i1 = round(Pto->x * C.USCAL * (float)C.PUNX) + C.ORGX ;
	g_j1 = round(Pto->y * C.USCAL * (float)C.PUNY) + C.ORGY ;
	g_i0 = round(Pfr->x * C.USCAL * (float)C.PUNX) + C.ORGX ;
	g_j0 = round(Pfr->y * C.USCAL * (float)C.PUNY) + C.ORGY ;
#ifdef DEBUG_CHK
#define CHK(v) if (v < 0) v = 0
	CHK(g_i1) ;  CHK(g_j1) ;
	CHK(g_i0) ;  CHK(g_j0) ;
#endif /*DEBUG*/

	/* DO THE WORK */
	if (plot_type == T_VECT) vect_work() ;
	else			 rast_work() ;

	/* Soft pen style code. Draw thicker lines in software */
	/*  We can only draw with a 1xN or Nx1 pen (no round or square
	 *   pens), so depending on the angle of the segment, there
	 *   will be a caligraphic look.  So we use an Nx1 pen, but
	 *   if the segment is horizontal, we use a 1xN pen.  This isn't
	 *   visible when N is small, but can be ugly when N gets
	 *   large. (mje aug 88)
	 */
#define abs(A) ((A) >= 0? (A): -(A))
	if (g_cmd == G_DRAW && soft_pen && g_pentyp > 1) {
		pn = g_pentyp /* % 10 */ ;

		di = g_i1 - g_i0 ;	/* use di,dj as tmp holders */
		dj = g_j1 - g_j0 ;
		/* change pen to avoid caligraphic look */
		horz_pen = abs(di) <= abs(dj) ;

		isave = i1 = g_i1;
		jsave = j1 = g_j1 ;
		i0 = g_i0 ;
		j0 = g_j0 ;
		di = dj = 0 ;

		for (n = 1, dir = 1 ; n < pn ; ++n, dir = -dir) {
			if (horz_pen)	di = n * dir ;
			else		dj = n * dir ;

			g_cmd = G_MOVE ;
			g_i1  = i0 += di ;
			g_j1  = j0 += dj ;
			if (plot_type == T_VECT) vect_work() ;
			else			 rast_work() ;

			g_cmd = G_DRAW ;
			g_i1 = i1 += di ;
			g_j1 = j1 += dj ;
			if (plot_type == T_VECT) vect_work() ;
			else			 rast_work() ;
			}
		/* restore point */
		g_cmd = G_MOVE ;
		g_i1  = isave ;
		g_j1  = jsave ;
		if (plot_type == T_VECT) vect_work() ;
		else			 rast_work() ;
		g_cmd = G_DRAW ;
		}
	}


/* Do a draw with the hardware, called by gp_line() to get little lines..
 *  If we are doing things through aluXX() then return the ent that was used.
 * Note: this is called from gp_lines.c so cannot be declared a
 *  static functions.
 */
int
hard_work(void)
{
	int	this_ent = NOENT ;

#ifdef DEBUG
	CheckI() ;
#endif
	if (plot_type == T_VECT) {
		this_ent = (g_cmd == G_DRAW ? C.DRAW : C.MOVE);

		/* first draw for this plot ?? OPEN the plot */
		if (!something && this_ent != NOENT) {
			docmd(C.PLOP);
			something = T;
			}

		docmd(this_ent);
		}

	else  /* if (plot_type == T_RAST) */ {
		if (g_cmd == G_DRAW) {
			something = T ;
			prast(g_i1, g_j1, g_i0, g_j0);
			}
		}


	return this_ent ;
	}

#ifdef DEBUG
static
CheckI() {
	switch (g_cmd) {
	case G_DRAW:
	case G_MOVE:
	case G_AREA:
	if (g_i1 < 0 || g_i1 > C.PIXX)
		fprintf(stderr,"vect_work: i1 %d, PIXX %d\n",g_i1,C.PIXX) ;
	if (g_j1 < 0 || g_j1 > C.PIXY)
		fprintf(stderr,"vect_work: j1 %d, PIXY %d\n",g_j1,C.PIXY) ;
		break ;
		}
	}
#endif /*DEBUG*/


/* do the work for vector type devices */
static
vect_work(void)
{

	int    this_ent;
	float	x = g_x1, y = g_y1 ;

	/* start */

	this_ent = NOENT;      /* remember current entry point */
	switch (g_cmd) {
	case G_DRAW:
	case G_MOVE:
		if (soft_line)	gp_linplt() ;
		else		this_ent = hard_work() ;

		g_Plast.x = x ;
		g_Plast.y = y ;
		break;

	case G_AREA:
		if (soft_area) gp_area() ;
		else {
			this_ent = C.AREA ;

			if (this_ent != NOENT) {
				if (!something) {
					docmd(C.PLOP) ;
					something = T ;
					}
				docmd(this_ent) ;
				}
			}
		g_Plast.x = x ;
		g_Plast.y = y ;
		break ;

	case G_CLEAR:
		this_ent = C.PCLR;
		/* ok to try to clear if ICLR */
		if (something) docmd(C.PLCL);
		if (something || C.ICLR) docmd(C.PCLR);
		something = F;
		break;

	case G_PEND:   /* close the plot */
		if (something) docmd(C.PLCL); 
		something = F;
		break;

	/* less used commands, incase someone wants to use them */
	case G_LINE:
		if (hwline_ok && C.LINE != NOENT) docmd(C.LINE);
		else {
			(void) gp_lnset(g_sub) ;
			if (g_sub != 1) soft_line = T ;
			}
		g_lintyp = g_sub ;
		if (g_lintyp == 1) soft_line = F ;
		break;

	case G_PEN:
		if (g_sub < 0) g_sub = -g_sub ;
		if (hwpen_ok && C.PEN != NOENT) docmd(C.PEN);
		else {
			if (g_sub != 1) soft_pen = T ;
			}
		g_pentyp = g_sub ;	/* so gpgeti() can report it */
		if (g_pentyp == 1) soft_pen = F ;
		break;

	case G_FILS:    /* select area fill patter/colour */
		if (g_sub < 0) g_sub = -g_sub ;
		docmd(C.FILS);
		g_filtyp = g_sub ;	/* so gpgeti() can report it */
		break;

	case G_USR1:
		docmd(C.USR1);
		break;
	case G_USR2:
		docmd(C.USR2);
		break;

	case G_CLOC:
		docmd(C.CLOC);
		break ;

	case G_PAUSE:
		docmd(C.PAUS);
		break ;

	/* context switching commands */
	case G_GTOA:
	case G_ATOG:
		gp_gmode(g_cmd) ;
		break;

#ifdef Never_Again
	/* This should fall into disuse, replaced by gtoa, but until then...*/
	case G_POST:
		if (!C.IGPO) gp_gmode(G_GTOA) ; /* ignore post ?? */
		break;
#endif
	/* change drawing mode (how do set pixels...) */
	case G_DMODE:
		if (g_sub == G_MSET) {
			docmd(C.MSET);
			C.DRAW = C.DRWST;
			}
		else if (g_sub == G_MCLR) {
			docmd(C.MCLR);
			C.DRAW = C.DRWCL;
			}
		else if (g_sub == G_MCMP) {
			docmd(C.MCMP);
			C.DRAW = C.DRWCM;
			}
		g_drawmd = g_sub;  /* just FYI */
		break;

	case G_NULL:	/*  do nothing, some other reason for this */
		docmd(ALU_NULL) ;
		break;

	default:
		docmd(C.UNKN);
		break;

		}/* of switch */

	last_ent = this_ent;        /* will be NOENT unless changed */
	last_cmd = g_cmd; 

	} /* of routine */


/* do all the work for a rastor type plotter */
static
rast_work(void)
{

	/* start */

	switch (g_cmd) {
	case G_DRAW:
		if (soft_line) gp_linplt() ;
		else           (void)hard_work() ;

		g_Plast.x = g_x1 ;
		g_Plast.y = g_y1 ;
		break;

	case G_AREA:
		gp_area() ;
		g_Plast.x = g_x1 ;
		g_Plast.y = g_y1 ;
		break ;

	case G_MOVE:
		break;

	case G_CLEAR:
	case G_PEND:
		/* output pixel array, clear it */
		if (something) {
			(void)out_pix();
			if (g_cmd != G_PEND) clear_pix();
			something = F;
			}
		break;


	/* the less used commands, if they want to be used... */
	case G_LINE:
		(void)gp_lnset(g_sub) ;
		g_lintyp = g_sub ;
		soft_line = (g_lintyp != 1) ;
		break;
	case G_PEN:
		if (g_sub < 0) g_sub = -g_sub ;
		g_pentyp = g_sub ;
		break ;

	case G_PAUSE:
		docmd(C.PAUS);
		break ;

	case G_USR1:
		docmd(C.USR1);
		break;

	case G_USR2:
		docmd(C.USR2);
		break;

	/* change drawing mode for setpix */
	case G_DMODE:
		if (g_sub == G_MSET || 
			   g_sub == G_MCLR || 
				  g_sub == G_MCMP) {
			g_drawmd = g_sub;     /* new drawing mode */
			}
		break;

	case G_NULL:	/*  do nothing, some other reason for this */
		break;

	default:
		docmd(C.UNKN);
		break;

		}/* of switch */

	}/* of routine */


/* gplot2() calls this directly so this must not be decl'ed static */
save_work(void)
{
	static	 Bool	 first = T ;

	/* Any command greater than G_NOT_SAVED will be ignored... */
	if (g_cmd >= G_NOT_SAVED) return;

	if (C.SFORM == NOENT) { /* do we have a save format? */
		/* header record wasn't done by the original open, so add it */
		if (first) (void)gpwrhed(lu_oplt) ;
		/* Default format */
		(void)gpwrite(lu_oplt, g_cmd,g_sub,g_x1,g_y1);
		}

	else {
		docmd(C.SFORM);
		}

	if (first) first = F ;

	if (g_cmd == G_PEND) {
		gp_setdn() ;	/* take it down */
		first = T ;	/* next time will be first again */
		}
	}

