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
 * Tue Jan 30 16:06:20 PST 1990 (dredge--stanford)
 *
 * "dump_it" : dump out (to the given file) everything that it would take
 *	to recreate the current dplot state.
 * written: Michael Eldredge (apr 84)
 * mod    : MJE (may 86) added everything except clip values.
 * modified: MJE (jan 90) added the relative/absolute label locs
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"

dump_it(fp)
	FILE *fp;
	{
	char *s;

	/* dump in a form that dplot could re-read and use */
	fprintf(fp,"\n# current Dplot settings are:\n# Rev %g\n\n", dp_rcode);

	/* Physical settings first */
	if (p_cornX != CORNX && p_cornXp != BYPRC) {
		fprintf(fp," $phys.origin.x ") ;
		if (p_cornXp) fprintf(fp, "%g%%\n", p_cornX * 100.) ;
		else          fprintf(fp, "%g\n"  , p_cornX ) ;
		}
	if (p_cornY != CORNY && p_cornYp != BYPRC) {
		fprintf(fp," $phys.origin.y ") ;
		if (p_cornYp) fprintf(fp, "%g%%\n", p_cornY * 100.) ;
		else          fprintf(fp, "%g\n"  , p_cornY ) ;
		}

	if (p_lenX != PLENX && p_lenXp != BYPRC) {
		fprintf(fp," $phys.len.x ") ;
		if (p_lenXp) fprintf(fp, "%g%%\n", p_lenX * 100.) ;
		else	     fprintf(fp, "%g\n"  , p_lenX) ;
		}
	if (p_lenY != PLENY && p_lenYp != BYPRC) {
		fprintf(fp," $phys.len.y ") ;
		if (p_lenYp) fprintf(fp, "%g%%\n", p_lenY * 100.) ;
		else	     fprintf(fp, "%g\n"  , p_lenY) ;
		}

	if (p_aspect != ASPECT) fprintf(fp," $phys.aspcect %g\n", p_aspect);

	if (p_maxX != PMAXX) fprintf(fp," $phys.max.x %g\n", p_maxX) ;
	if (p_maxY != PMAXY) fprintf(fp," $phys.max.y %g\n", p_maxY) ;

	
	/* General dplot settings */
	fprintf(fp," $scale.x  %g   $scale.y  %g\n",scaleX, scaleY);
	fprintf(fp," $offset.x %g   $offset.y %g\n",transX, transY);
	fprintf(fp," $rotate %g (degrees) $rotate.x %g  $rotate.y %g\n",
					rotateA,rotateX,rotateY);
	fprintf(fp," $ncols %d   $col.x %d  $col.y %d\n", ncols, colX,colY);
	fprintf(fp," $min.x %g  $max.x %g  $%s.x\n", 
				minX,maxX, (logXplot ? "log" : "linear") );
	fprintf(fp," $min.y %g  $max.y %g  $%s.y\n",
				minY,maxY, (logYplot ? "log" : "linear") );
	
	s = "line" ;
	if      (by_how == (BY_LINE | BY_SYMB)) s = "both" ;
	else if (by_how == BY_SYMB) s = "symb" ;
	fprintf(fp," $by.%s", s);

/*	if (by_how & BY_LINE)	*/
		fprintf(fp, " $line %d\n", lin_typ);
/*	if (by_how & BY_SYMB) 	*/
		fprintf(fp, " $symb %d  $symb.size %g\n", (int)*s_sym, s_siz);
	fprintf(fp," $label.size %g\n", l_siz) ;


	if (*title) {
		fprintf(fp," $title ");  otstr(fp, title); fprintf(fp,"\n");
		}
	if (*labelX) {
		fprintf(fp," $label.x "); otstr(fp,labelX); fprintf(fp,"\n");
		}
	if (*labelY) {
		fprintf(fp," $label.y "); otstr(fp,labelY); fprintf(fp,"\n");
		}

	fprintf(fp, "\n") ;

	fprintf(fp," $pen.axis %d   $pen.data %d $pen.symb %d\n",
		pen_axis, pen_data, pen_symb) ;

	fprintf(fp,"\n") ;
	fprintf(fp," $label.loc.%s\n", llocs_rel? "data": "screen") ;
	fprintf(fp," $label.loc.x %g  $label.loc.y %g\n", xlloc, ylloc) ;

	if (addtoX != 0.0) fprintf(fp, " $addto.x %g", addtoX) ;
	if (addtoY != 0.0) fprintf(fp, " $addto.y %g", addtoY) ;
	fprintf(fp,"\n") ;
	if (mulbyX != 1.0) fprintf(fp, " $mulby.x %g", mulbyX) ;
	if (mulbyY != 1.0) fprintf(fp, " $mulby.y %g", mulbyY) ;
	fprintf(fp,"\n") ;

	fprintf(fp, " $abs.x.%s", absX? "on": "off" ) ;
	fprintf(fp, " $abs.y.%s", absY? "on": "off" ) ;
	fprintf(fp,"\n") ;


	fflush(fp);
	}
