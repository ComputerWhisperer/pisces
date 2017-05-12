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
 * Thu Feb  1 11:42:59 PST 1990 (dredge--stanford)
 *
 * "gp_area.c": Area fill routines.  These are used for devices that do not
 *	themselves support area fills.  This is quite unfinished, but is
 *	does show what I am after.  I would like someone else to experiment
 *	with the area-patterns until some good values are found.
 *
 * written:  Michael Eldredge (jan 85)
 * modified: Michael Eldredge (feb 87) Fix to use local rotates and to
 *	be called after tranformation/clipping.
 * modified: Michael Eldredge -- Stanford (aug 87) Fix area fill/last
 *	point problem.  Do some simple bounds checking.
 * modified: Michael Eldredge -- Stanford (jan 90) 
 *	+++ Fixed the incorrect assumption that an intersection at a
 *	vertex constituted a degenerate vertex pair and therefore could
 *	be skipped.
 *	+++ Increased the AREA_VMAX.  Turns out that people make
 *	more use of it then had been planned.
 */


#include "auxfns.h"
#include "al_alu18.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"


#define	 AREA_VMAX	1024		/* Max verticies */
#define	 AREA_IMAX	 40		/* Max intersections */
#define  SPC	.05

/* !!!! HP/UX will not constant fold floats.  Therefore, in the decl of
 *   apats below we can't do things like:  { 2 , 45.0 , 2*SPC },
 *   so,this primitive form of getting multiples of SPC:
 */
#define	 SPC1  0.05
#define	 SPC2  0.10
#define  SPC3  0.15
#define	 SPC4  0.20

static	 fpoint2d alist[AREA_VMAX+1] ;
static	 int	  aloc ;
static	 float	  blx, bly, bhx, bhy ;	/* bounds of the area */

static	 int	  iloc ;		/* intersections */
static	 float    ilist[AREA_IMAX] ;	/* intersection list */

static	 struct _area_pat {
	 float	 ar_rot1 ;		/* rotation of line */
	 float	 ar_rot2 ;		/* rotation of line */
	 float	 ar_spc ;		/* spacing of lines */
	 } apats[] = {
		{ 45.0	,  45.0  , SPC1 } ,	/*Special*/
		{ 45.0	,  45.0	 , SPC1 } ,	/*  1 */
		{  0.0	,   0.0  , SPC1 } ,	/*  2 */
		{135.0	, 135.0  , SPC1 } ,	/*  3 */
		{ 90.0	,  90.0  , SPC1 } ,	/*  4 */

		{ 45.0	, 135.0	 , SPC3 } ,	/*  5 */
		{  0.0	,  90.0  , SPC3 } ,	/*  6 */

		{ 45.0	,  45.0	 , SPC3 } ,	/*  7 */
		{  0.0	,   0.0  , SPC3 } ,	/*  8 */
		{135.0	, 135.0  , SPC3 } ,	/*  9 */
		{ 90.0	,  90.0  , SPC3 } ,	/* 10 */

		{ 45.0	, 135.0	 , SPC1 } ,	/* 11 */
		{  0.0	,  90.0  , SPC1 } ,	/* 12 */

		{ 30.0	,  30.0	 , SPC2 } ,	/* 13 */
		{120.0	, 120.0  , SPC2 } ,	/* 14 */
		{ 60.0	,  60.0  , SPC2 } ,	/* 15 */
		{150.0	, 150.0  , SPC2 } ,	/* 16 */
	} ;

static	 int	 curpat = 1 ;		/* currently selected patter */

#define NTYPES (sizeof(apats) / sizeof(struct _area_pat))

static void gpa_pass(float ang);
static int al_outs(char *str);
static void gpa_draw(float yinc, float ang);
static int isort();
static void l_rotate(float ang);
static void l_xform(fpoint2d *Pnew, float oldx, float oldy);
/* "gp_area": Handle gplot calls for area help...
 *	Expect commands to be in common (g_cmd/g_sub/etc....)
 */
#ifdef ANSI_FUNC

void 
gp_area (void)
#else

void gp_area()
#endif
	{

	int	lin_sav ;
	defrot("gp_area") ;

	switch (g_sub) {
	case G_BEGIN:
		aloc = 0 ;
		/* Fall through */

	case G_ON:			/* Collect vectors */
		if (aloc >= AREA_VMAX) {
			if (aloc++ == AREA_VMAX) /* inc so only one warning*/
				gperr2("too many points in polygon (max %d)",
					AREA_VMAX) ;
			break ;
			}

		alist[aloc  ].x = g_x1 ;
		alist[aloc++].y = g_y1 ;
		break ;

	case G_STOP:
	case G_OFF:
		/* just in case ... */
		if (aloc < 3) { aloc = 0 ; break ; }
		if (aloc > AREA_VMAX) aloc = AREA_VMAX ;

		lin_sav = g_lintyp ;		/*   and line type */
		g_cmd = G_LINE ;
		g_sub = 1 ;		/* always solid line */
		gp_work() ;

		gpa_pass(-apats[curpat].ar_rot1) ;	/* rotate all */
		gpa_draw( apats[curpat].ar_spc, apats[curpat].ar_rot1 ) ;

		if (apats[curpat].ar_rot1 != apats[curpat].ar_rot2) {
		    gpa_pass( apats[curpat].ar_rot1) ;	/* fix */
		    gpa_pass(-apats[curpat].ar_rot2) ;
		    gpa_draw( apats[curpat].ar_spc, apats[curpat].ar_rot2 ) ;
		    }

		g_cmd = G_LINE ;
		g_sub = lin_sav ;		/* restore old */
		gp_work() ;

		aloc = 0 ;
		break ;
		}
	}

#ifdef ANSI_FUNC

static void gpa_pass(float ang)
#else

static void 
gpa_pass (ang)
float ang;
#endif
	{

	int	i ;
	fpoint2d* p = alist ;

	l_rotate(ang) ;
	for (i = 0 ; i < aloc; p++, i++) {
		l_xform(p, p->x, p->y) ;
		if (i == 0) {
			blx = bhx = p->x ;
			bly = bhy = p->y ;
			}
		else {
			if (p->x > bhx) bhx = p->x ;
			if (p->x < blx) blx = p->x ;
			if (p->y > bhy) bhy = p->y ;
			if (p->y < bly) bly = p->y ;
			}
		}

	l_rotate(-ang) ;
	}
		

#ifdef ANSI_FUNC

static void gpa_draw(float yinc, float ang)
#else

static void 
gpa_draw (yinc, ang)
float yinc;
float ang;
#endif
	{

	float	y ;
	int	n ;
	fpoint2d  P0, P1 ;
	fpoint2d  pi, l1, l2 ;
	float	xv, yv ;
		
	defrot("gpa_draw") ;

	l_rotate(ang) ;

	l1.x = blx ;		/* Bounds at lo,hi x values */
	l2.x = bhx ;

	for (y = bly ; y < bhy ; y += yinc) {
		l1.y = y ;
		l2.y = y ;
		iloc = 0 ;
		for (n = 0 ; n <= aloc-2; n++) {
			if (alist[n].y == alist[n+1].y) {
				continue ;	/* horizontal */
				}

			/* Check for intersection.... */
			if (fnsect(&pi, &l1      , &l2,
					&alist[n], &alist[n+1])) {
				if (iloc >= AREA_IMAX) {
					gperr2("Too many crossings (max %d)",
						AREA_IMAX) ;
					break ;
					}
				ilist[iloc++ ] = pi.x ;
				}
			}

		/* if only 0 or 1 intersection, bag it */
		if (iloc <= 1) continue ;

		isort();		/* sort intersections */

		g_sub = 0 ;
		P0.x = 0; P0.y = 0 ;

		/* for each vertex pair draw a line */
		for ( n = 0 ; n < iloc ;  ) {
			xv = ilist[n++] ;
			if (n >= iloc) break ;	/* safety */
			yv = ilist[n++] ;

			l_xform(&P1, xv, y) ;
			g_cmd = G_MOVE ;
			simp_work(&P1, &P0) ;

			l_xform(&P0, yv, y) ;
			g_cmd = G_DRAW ;
			simp_work(&P0, &P1) ;
			}
		}

	l_rotate(-ang) ;		/* reset rotation */
	}

/* date: 26 feb 87 (mje)
 * "gp_arset" : set a new area fill pattern for subsequent area fill calls 
 * modified:  User settable style.
 */
#ifdef ANSI_FUNC

int 
gp_arset (int n)
#else

int
gp_arset(n)
	int	 n ;
#endif
	{

	if (n > 1) {		/* simple: use internal values */
		curpat = (n % NTYPES) ;
		}

	else if (n < 0) {	/* define a simple pattern */
		if (g_x1 > 0.0) {	/* non-zero spacings */
			apats[0].ar_rot1 = g_x1 ;
			apats[0].ar_rot2 = g_y1 ;	/* angle */
			apats[0].ar_spc  = -(float)n*SPC; /* spacings */
			curpat = 0 ;		/* special */
			}
		}

	g_filtyp = curpat ;	/* for gpgeti() */
	}


/* "isort" : sort the intersection verticies (by x).  
 *	This is a very simple non-recursive bubble sort method.
 *	In general there are not that many points to sort, so....
 */
#ifdef ANSI_FUNC

static int 
isort (void)
#else

static int isort()
#endif
	{

	register float	 temp ;
	register int	 i , done ;

  once_more:
	done = T ;
	for (i = 0 ; i < iloc-1; i++) {
		if (ilist[i] > ilist[i+1]) {
			temp       = ilist[i]   ;
			ilist[i]   = ilist[i+1] ;
			ilist[i+1] = temp ;
			done = F ;
			}
		}

	if (!done) goto once_more ;
	}

/* ----------------------------------------------------------------------- */
/* We need to do some local rotating to draw the hashes.  This cannot
 *	use the internal xform() stuff b/c we:
 *		ROT(-ang);
 *		collect-points;
 *		ROT(ang); ROT(ang) ;
 *		draw-points;
 *		ROT(-ang) ;
 *	which leaves things correct. But during the drawing and if there
 *	were other transformations in the global matrix, we've doubled
 *	up their effect.  So we just want to rotate only!!
 */
static	double	l_xx = 1.0,  l_xy = 0.0 ;	/* rotation constants. */
static	double	l_yx = 0.0,  l_yy = 1.0 ;

/* "l_rotate": simple form of gp_xfrm(G_ROTATE, ... ) and reduct3() */
/* in degees */
#ifdef ANSI_FUNC

static void l_rotate(float ang)
#else

static void 
l_rotate (ang)
float ang;
#endif
	{
	double	txx, txy, tyx, tyy ;	/* until new eval is done */
	double	inrads, cr, sr ;
	double	cos(), sin() ;

#define Testing
#ifdef  Testing
	l_xx = l_yy = 1.0 ;
	l_xy = l_yx = 0.0 ;
#endif

	inrads = (double)ang * -PIO180 ;
	cr = cos(inrads) ;
	sr = sin(inrads) ;

	txx = l_xx * cr  +  l_xy * -sr ;
	tyx = l_yx * cr  +  l_yy * -sr ;

	txy = l_xx * sr  +  l_xy * cr ;
	tyy = l_yx * sr  +  l_yy * cr ;

	l_xx = txx ;  l_xy = txy ;
	l_yx = tyx ;  l_yy = tyy ;
	}

/* "l_xform": simpified/local version of xform() */
#ifdef ANSI_FUNC

static void l_xform(fpoint2d *Pnew, float oldx, float oldy)
#else

static void 
l_xform (Pnew, oldx, oldy)
fpoint2d *Pnew;
float oldx;
float oldy;
#endif
	{
	float	xtemp ;		/* see xform() */

	xtemp	= oldx * l_xx  +  oldy * l_xy ;
	Pnew->y	= oldx * l_yx  +  oldy * l_yy ;
	Pnew->x = xtemp ;
	}
/* ----------------------------------------------------------------------- */

