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
 * Tue Jan 30 15:21:33 PST 1990 (dredge--stanford)
 *
 * "docmd1.c" : some more dplot commands: sub and sub-sub types
 * 
 * mje -- stanford (jan 90)
 *	+ Make sure we draw symbols with pen_symb.
 *	+ Added fixpnt() to convert from data coords to screen coords
 *	  since we need it in a couple of places now (beyond dopnt())
 *	+ Added ability to put labels relative (data values) or absolute
 *	  (screen coords).
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


/* ---- scale some points ---- */
float
fixpnt(which, val)
	char	which ;
	float	val ;
	{

	float	new = val ;
	double  pow() ;

	/* NEED THIS? */
	if (NewPlot) { fxphys() ; NewPlot = F ; }

	switch (which) {
	/* I. From DATA to SCREEN coords */
	case 'x':
		if (absX) new = abs(new) ;
		new = (new * mulbyX) + addtoX ;
		if (logXplot) new = Log_10(new) - Log_10(minX) ;
		else	      new -= minX ;

		new = (new * rateX) + plotX0 ;
		break ;

	case 'y':
		if (absY) new = abs(new) ;
		new = (new * mulbyY) + addtoY ;
		if (logYplot) new = Log_10(new) - Log_10(minY) ;
		else	      new -= minY ;

		new = (new * rateY) + plotY0 ;
		break ;

	/* II. From SCREEN coords to DATA coords */
	case 'X':
		new = (new - plotX0) / rateX ;

		if (logXplot) new = pow((double)new,(double)10.0) + minX ;
		else          new += minX ;

		new = (new - addtoX) / mulbyX ;
		/* if (absX) ... ;; not much we can do here! */
		break ;
	case 'Y':
		new = (new - plotY0) / rateY ;

		if (logYplot) new = pow((double)new,(double)10.0) + minY ;
		else          new += minY ;

		new = (new - addtoY) / mulbyY ;
		/* if (absY) ... ;; not much we can do here! */
		break ;

	/* III. Error, someone gave the wrong key! */
	default:
		dperr2("Ooops. The developers gave a bad value to fixpnt(%c)",
				which);
		break ;
		}

	return new ;
	}


/* ----- to set up the physical dimentions of the plotter ----- */

/* "stphys" : set some of the physical properites up */
fxphys()
	{
	float  lenx, leny;
	float  maxx, maxy;
	float	t ;		/* temp value */
	float	fv[4] ;		/* returns from gpgeti() */
	int	iv[4] ;

	gpgeti(G_PSIZE, iv, fv) ;		/* pick up plot size */
	sizeX = fv[0];  sizeY = fv[1] ; 

	/* we always set the low/left corn the same-ish way... */
	plotX0 = p_cornX * (p_cornXp ? sizeX : 1.0) ;
	plotY0 = p_cornY * (p_cornYp ? sizeY : 1.0) ;

	/* find the limit in each direction */
	maxx   = sizeX ;
	maxy   = sizeY ;
	if (p_maxX > 0.0 && p_maxX < maxx) maxx = p_maxX ;
	if (p_maxY > 0.0 && p_maxY < maxy) maxy = p_maxY ;

	/* the length of each leg */
	lenx   = p_lenX  * (p_lenXp  ? sizeX : 1.0) ;
	leny   = p_lenY  * (p_lenYp  ? sizeY : 1.0) ;

	if (plotX0 + lenx > maxx) lenx = maxx - plotX0 ;
	if (plotY0 + leny > maxy) leny = maxy - plotY0 ;

	if (p_aspect > 1.0) {
		if ((t = lenx / p_aspect) <= leny)  leny = t;
		else				    lenx = leny * p_aspect ;
		}

	else if (p_aspect > 0.0 && p_aspect < 1.0) {
		if ((t = leny * p_aspect) <= lenx)  lenx = t;
		else                                leny = lenx / p_aspect ;
		}

	plotX1 = plotX0 + lenx ;
	plotY1 = plotY0 + leny ;

	redo_rate();
	}

stphys(cmd, sub, rval, perc)
	char *cmd, *sub;
	float rval ;
	bool  perc;
	{
	bool haveSUB = *sub, unsub = F ;

	if (strequ(cmd, "origin" )) {
		if (perc) rval /= 100. ;
		if (!haveSUB) {
			p_cornX = p_cornY = rval ;
			p_cornXp= p_cornYp= perc ;
			}

		else if (strequ(sub, "x")) {
			p_cornX = rval ;
			p_cornXp= perc ;
			}

		else if (strequ(sub, "y")) {
			p_cornY = rval ;
			p_cornYp= perc ;
			}

		else unsub = T ;
		}

	else if (strequ(cmd, "len" )) {
		if (perc) rval /= 100. ;
		if (!haveSUB) {
			p_lenX = p_lenY = rval ;
			p_lenXp= p_lenYp= perc ;
			}

		else if (strequ(sub, "x")) {
			p_lenX = rval ;
			p_lenXp= perc ;
			}

		else if (strequ(sub, "y")) {
			p_lenY = rval ;
			p_lenYp= perc ;
			}

		else unsub = T ;
		}

	else if (strequ(cmd, "aspect")) {
		if (!haveSUB || rval == 0.0)	p_aspect = rval ;

		else if (strequ(sub,"x") || strequ(sub,"x/y")) 
			p_aspect = rval ;
		else if (strequ(sub,"y") || strequ(sub,"y/x")) 
			p_aspect = 1.0 / rval ;

		else unsub = T ;
		}

	else if (strequ(cmd, "max")) {
		if (!haveSUB) p_maxX = p_maxY = rval ;

		else if (strequ(sub, "x"))  p_maxX = rval ;
		else if (strequ(sub, "y"))  p_maxY = rval ;

		else unsub = T ;
		}

	else dperr2("(phys) Unknown sub-command `%s'", cmd);

	if (unsub) dperr2("(phys) Unknown sub-sub-cmd `%s'", sub);

	fxphys();
	}



/* some debug commands... */
ddcmd(cmd, sub, arg, rval)
	char  *cmd, *sub, *arg;
	float  rval;
	{

	float atof();
#ifndef lint
	int  haveSUB = *sub;
	int  haveARG = *arg;
#endif
#ifdef lint
	if (rval > 0.0) rval = 0.0 ;	/* lint! be quite! */
	*arg = '\0' ;			/*  -ditto- */
#endif

	if  (strequ(cmd, "show")) {

		if (strequ(sub, "phys")) {

			printf("\tSize (%g, %g)\n" , sizeX, sizeY);
			printf("\tX0 %g, \tX1 %g, \tLenX %g\n" ,
				plotX0, plotX1, plotX1-plotX0 );
			printf("\tY0 %g, \tY1 %g, \tLenY %g\n" ,
				plotY0, plotY1, plotY1-plotY0 );
			printf("\torigin (%g%c, %g%c)\n", 
				p_cornX, (p_cornXp ? '%' : '\0') , 
				p_cornY, (p_cornYp ? '%' : '\0') ) ;
			printf("\tlen  (%g%c, %g%c)\n", 
				p_lenX, (p_lenXp ? '%' : '\0') , 
				p_lenY, (p_lenYp ? '%' : '\0') ) ;
			printf("\tmax (%g, %g), aspect %g\n", 
				p_maxX, p_maxY, p_aspect);
			}/*..phys */

		else if (strequ(sub, "rate")) {
			
			printf("\trate (%g, %g)\n" , rateX, rateY);
			printf("\tmin/max X: %g\t%g\n", minX, maxX );
			printf("\tmin/max Y: %g\t%g\n", minY, maxY );
			printf("\tX: %s, Y: %s\n", 
				(logXplot ? "log" : "linear"),
				(logYplot ? "log" : "linear")  );
			}/*..rate*/

		}/*.show*/
	}/*of debug commands */


/* set the label locations for - label.loc (line.label, symb.label) */
int
stlloc(cmd, have_arg, rval)
	char  *cmd;
	bool   have_arg ;
	float  rval;
	{
	bool	took_arg = F ;

	/* Commands that need the argument */
	if      (! *cmd ) {
		if (have_arg) { xlloc = ylloc = rval ; took_arg = T ; }
		else dperr("no location given for label.loc") ;
		}
	else if (strequ(cmd,"x")) {
		if (have_arg) { xlloc = rval ; took_arg = T ; }
		else dperr("no location given for loc.x") ;
		}
	else if (strequ(cmd,"y")) {
		if (have_arg) { ylloc = rval ; took_arg = T ; }
		else dperr("no location given for loc.y") ;
		}

	/* in plot coords */
	else if (strequ(cmd,"abs") || strequ(cmd,"screen")) {
		if (have_arg) dperr("no value needed") ;
		else llocs_rel = F ;
		}
	/* relative to the data -- data space*/
	else if (strequ(cmd,"rel") || strequ(cmd,"data")) {
		if (have_arg) dperr("no value needed") ;
		else llocs_rel = T ;
		}

	else dperr2("unknown sub-sub-command `%s' for `label.loc'", cmd);

	return took_arg ;
	}

/* "dplabel" : draw labels at current "xlloc,ylloc" */
dplabel(type, thelab)
	char *type, *thelab ;
	{

	float x = llocs_rel? fixpnt('x',xlloc): xlloc ,
	      y = llocs_rel? fixpnt('y',ylloc): ylloc ;

	if      (strequ(type, "symb")) {
		if (lin_typ != 1) gnline(1) ;
		symbl2(x, y, s_sym, -1, s_siz, s_siz, 0.0, 0.0, .5);
		if (lin_typ != 1) gnline(lin_typ) ;
		x += 1.5 * s_siz ;
		}

	else if (strequ(type, "line")) {
		float ytmp = y + .5 * s_siz ;
		gmove(x - s_siz , ytmp);  /* draw using current line type */
		gdraw(x + s_siz , ytmp);

		x += 1.5 * s_siz ;
		}

	/* now the label ... */
	if (lin_typ != 1) gnline(1) ;
	if (pen_symb != pen_data) gnpen(pen_symb) ;

	symbl2(x , y, thelab, strlen(thelab),
			l_siz,l_siz, 0.0, .0, .0);

	if (pen_symb != pen_data) gnpen(pen_data) ;
	if (lin_typ != 1) gnline(lin_typ) ;

	/* should be able to set 'symbl2' params: size, angle */
	}
