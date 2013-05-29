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
 * Tue Jan 30 15:24:41 PST 1990 (dredge--stanford)
 *
 * "docmd.c" : command interpretation routine for 'dplot' program.
 *
 * calling sequence:
 *	docmd(cmd, sub, arg)
 *
 * where:
 *	cmd	- (char *) Command to do (without leading '$').
 *	sub     - (char *) sub-command for command. (Or *sub == 0 if no sub).
 *	arg     - (char *) 1. possible argument for command or null as above.
 *			   2. a possible number to use as data.  If cmd is null
 *				this is taken as a possible number.  If it is
 *				a number it is checked for x or y column, etc
 *				and plotted if it is (x or y). Else it is 
 *				ignored.
 *
 * written : Michael Eldredge (oct 83)
 * mod     : MJE (dec 85) Added the "pen" command.
 * mod     : MJE (apr 86) Fixed logging.
 * mod     : MJE (may 86) Report gplot rev code.
 * modified: MJE (jul 86) Fix axis: setting/resetting of clip values.
 * modified: MJE (dec 86) Added reset.cols, pen.up, pen.down
 *	++ Added $chdir command (for Mark Law).
 * modified: MJE (feb 87) 
 *	++ Bug fixes.  Added $axis.box command.
 *	++ added $echo command.
 * modified: MJE (mar 87)
 *	+++ Added absolute value commands.
 *	+++ Added $segment deletion commands.
 * modified: MJE (may 87) Fixed calls to gpgeti() for getting current clip
 *	values in "axis" command.  Forgot to set iv[0] = G_ONLOG, so it was
 *	nondeterministic.
 * modified: MJE (mar 88) $ncol will reset colcnt now.
 * modified: MJE (jul 88) $axis will pick up the pen.
 * modified: MJE -- stanford (sep 89) REV_CODE included here.
 * modified: MJE -- stanford (jan 90) Added $pen.symb command.
 */

#include <stdio.h>
#include "dp_vers.h"
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"

docmd(cmd, sub, arg)
	char *cmd, *sub, *arg;
	{

	float rval;
	float x, y, xt1, yt1;
	float fv[8] ;		/* for gpmisc() */
	int   iv[8] ;		/*  this too... */
	int   c, s;
	int   n ;
	bool  haveCMD, haveSUB;
	bool  haveARG, usedARG;   /* to show if we have value and if used it */
	bool  haveNUM ;
	FILE  *fpnew;
	
	char  scmd[WORDSIZE] , ssub[WORDSIZE] ;


	/* Some DEBUG stuff ..... a command like "$.show.x ..." is for Debug */
	/* clear possible buffers for later */
	scmd[0] = 0 ;
	ssub[0] = 0;

	if (! *cmd && *sub) strcpy(cmd, "debug"); /* for ``$.sub.subsub'' */

	/* set up some flags for later. */
	ErrCode = 0;     /* no errors!! */
	haveERR = F;
	haveCMD = *cmd;
	haveSUB = *sub;
	haveARG = *arg;
	usedARG = F;     /*  to putback if not used */

	rval    = 0.0;
	haveNUM = (haveARG && sscanf(arg, "%f", &rval));


	/* CHECK: if no command and not a number, just forget it.
	 *	  if no command but is a  number, work with it.
	 *        if command try to do something with it 
	 */
	/* nothing for us to worry about. */
	if (!haveCMD && !haveNUM) {
		return(NOBACK);
		}

	/* if not a command, BUT is a number .... */
	if (!haveCMD && haveNUM) dopnt(rval);

	/* commands.......*/
	/*  the types of commands are: 
	 *  1. debug commands...
	 *  2. commands with RARG values,
	 *  3. commands with no values,
	 *  4. commands with character values.
	 */

	/* 1. debug commands, these can go away some day */
	else if (strequ(cmd,"post"))	/* Not needed, but... */
		ggtoa();

	else if (strequ(cmd,"version") || strequ(cmd,"revcode")) {
		(void)gpgeti(G_REVCO, iv, fv) ;
		printf("\tDplot Version %.1f (release %c) ; Gplot Rev %d\n",
				REV_CODE, (char)(REL_CODE + (int)'A' -1),
				iv[0] ) ; /* rev code :: iv[0] */
		}


	/* This should stay, as a means to see what current settings
	 *  are, but should be called ``show'' or something. (mje)
	 */
	else if (strequ(cmd,"dump"))    /* dump out common */
		{
		if (!haveSUB) dump_it(stdout);
		else {
			if (strequ(sub,"to"))  { /* need 'to' file name */
				if (haveARG) {
					usedARG = T;
					if ((fpnew=fopen(arg,"w")) != NULL) {
						dump_it(fpnew);
						fclose(fpnew);
						}

					else dperr2("on open of '%s'",arg);
					}

				else dump_it(stdout);  /* no file, so...*/
				}

			else dperr2("unknown sub-command '%s'", sub);
			}

		}


	/* 2. commands with RARG values.  These will use 'rval'.
	 */
	else if (strequ(cmd,"offset"))   /* set origin */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   transX = transY = 0.0;   /* no change */
		   if (!haveSUB) transX = transY = rval;

		   else if (strequ(sub,"x")) transX = rval;
		   else if (strequ(sub,"y")) transY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   if (!haveERR) 
			  gtrans(transX, transY);
		   }
		}

	else if (strequ(cmd,"scale"))     /* set scale */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   scaleX = scaleY = 1.0;   /* no change */
		   if (!haveSUB) scaleX = scaleY = rval;

		   else if (strequ(sub,"x")) scaleX = rval;
		   else if (strequ(sub,"y")) scaleY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   if (!haveERR)
			gscale(scaleX, scaleY);
		   }
		}

	else if (strequ(cmd,"rotate"))    /* set rotation parms */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   rotateX = rotateY = rotateA = 0.0; /* no change */
		   if ((!haveSUB) || strequ(sub,"angle"))
			rotateA = rval;

		   else if (strequ(sub,"x")) rotateX = rval;
		   else if (strequ(sub,"y")) rotateY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   if (!haveERR) {
			grotat(rotateX, rotateY, rotateA);
			}
		   }
		}


	else if (strequ(cmd,"addto")) {    /* data offset */
		if (!haveNUM) dperr2("no value given for '%s'", cmd);

		else {
			usedARG = T;
			if (! haveSUB)  addtoX = addtoY = rval ;

			else if (strequ(sub,"x")) addtoX = rval ;
			else if (strequ(sub,"y")) addtoY = rval ;

			else dperr2("unknown sub-command `%s'" , sub);
			}
		}


	else if (strequ(cmd,"mulby")) {    /* data offset */
		if (!haveNUM) dperr2("no value given for '%s'", cmd);

		else {
			usedARG = T;
			if (! haveSUB)  mulbyX = mulbyY = rval ;

			else if (strequ(sub,"x")) mulbyX = rval ;
			else if (strequ(sub,"y")) mulbyY = rval ;

			else dperr2("unknown sub-command `%s'" , sub);
			}
		}

	/* clipping values */
	else if (strequ(cmd,"clip")) {	/* set new clip values */
		if (haveSUB) {
			usedARG = T ;
			mkcmd(sub, scmd, ssub);	/* break out sub-sub */

			s = G_ONLOG ;	/* set on logical axis */

			if      (strnequ(scmd,"hi", 2)) n = G_CLIPH ;
			else if (strnequ(scmd,"lo", 2)) n = G_CLIPL ;
			else if (strequ(scmd, "reset")) {
				gplot2(G_CLIPL, G_RESET, 0., 0.) ;
				gplot2(G_CLIPH, G_RESET, 0., 0.) ;
				goto clip_done;
				}

			else dperr2("unknown sub-command `%s'" , scmd);

			/* get current settings of clip values */
			gpmisc(n, G_GET, iv, fv, "") ;
			x = fv[0];  y = fv[1] ;

			/* no sub-sub, set to axis corners */
			if      (! *ssub  && ! haveNUM) {
				usedARG = F ;

				if (NewPlot) {  /* make sure we have plotNN */
					NewPlot = F ;
					fxphys();
					}
				if (n == G_CLIPL) {
					x = plotX0 ;
					y = plotY0 ;
					}
				else {
					x = plotX1 ;
					y = plotY1 ;
					}
				}
					
			else if (! *ssub )         x = y = rval ;
			else if (strequ(ssub,"x")) x = rval ;
			else if (strequ(ssub,"y")) y = rval ;
			else if (strequ(ssub,"reset")) s = G_RESET ;

			else dperr2("unknown sub-sub-cmd for `%s'",cmd);

			if (!haveERR) gplot2(n, s, x, y);
			}

		else dperr2("no sub-command for `%s'", cmd);

	   clip_done: s = 1 ;	/* just a goto target */
		}/* of clip */

	else if (strequ(cmd,"ncols"))     /* number of columns input */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   if (haveSUB)
			dperr("no subcommand needed");

		   else {
			ncols = (int) (rval + round);
			colcnt = 0 ;	/* reset */
			pen   = G_MOVE;
			}

		   }
		}


	else if (strequ(cmd,"col"))     /* set column numbers */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   pen = G_MOVE;

		   if (!haveSUB) colX = colY = rval;

		   else if (strequ(sub,"x")) colX = rval;
		   else if (strequ(sub,"y")) colY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   }
		}

	else if (strequ(cmd, "segment")) {	/* a segment to be del'd */
		if (!haveNUM && haveSUB) {
			if (strequ(sub,"reset"))
				seg_count = seg_depth = 0 ;
			else if (strequ(sub, "show")) {
				if (seg_count) {
					printf("\tSegments to be deleted:");
					for (n = 0 ; n < seg_count; )
						printf(" %d", seg_list[n++]);
					printf("\n") ;
					}
				else printf("\tNo segments to delete\n") ;
				}
			else dperr2("Bad subcommand '%s'", sub) ; 
			}
		else {
			usedARG = T ;
			n = (int)(rval+round) ;
			if (!haveSUB || strequ(sub,"del"))
				(void)seg_set(n, 'd') ;
			else if (strequ(sub, "keep"))
				(void)seg_set(n, 'k') ;
			else dperr2("unknown subcommand 'segment.%s'",sub) ;
			}
		}


	else if (strequ(cmd,"min"))     /* set mininum values */
		{
		if (!haveNUM)
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   pen = G_MOVE;
		   usedARG = T;

		   if (!haveSUB) minX = minY = rval;

		   else if (strequ(sub,"x")) minX = rval;
		   else if (strequ(sub,"y")) minY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   if (!haveERR) { redo_rate(); }
		   }
		}


	else if (strequ(cmd,"max"))     /* set maximum values */
		{
		if (!haveNUM) 
			dperr2("no value given for '%s'", cmd);
		else
		   {
		   pen = G_MOVE;
		   usedARG = T;

		   if (!haveSUB) maxX = maxY = rval;

		   else if (strequ(sub,"x")) maxX = rval;
		   else if (strequ(sub,"y")) maxY = rval;

		   else dperr2("unknown sub-command '%s'",sub);

		   if (!haveERR) { redo_rate(); }
		   }
		}


	else if (strequ(cmd,"skip"))     /* skip over <n> lines */
		{
		if (!haveNUM) dperr2("no value given for '%s'", cmd);

		else {
		   usedARG = T;
		   if (haveSUB) dperr("no subcommand needed");

		   else skipover = (int) (rval + round);
		   }
		}


	else if (strequ(cmd,"stop"))     /* stop after <n> lines */
		{
		if (!haveNUM) dperr2("no value given for '%s'", cmd);
		else
		   {
		   usedARG = T;
		   if (haveSUB) dperr("no subcommand needed");

		   else stopafter = (int) (rval + round);
		   }
		}


	else if (strequ(cmd,"symb")) {   /* change plot symbol*/
		if (!haveARG) dperr2("no argument given for '%s'", cmd);

		else {
		   usedARG = T;
		   if (!haveSUB || strequ(sub,"type")) {
			if (haveNUM) s_sym[0] = (char) ((int)(rval+round));
			else dperr2("numeric value required for `%s'" ,cmd);
			}

		   else if (strequ(sub,"siz") || strequ(sub,"size")) {
			if (haveNUM) s_siz = rval;
			else dperr2("numeric value required for `%s'" , cmd);
			}

		   else if (strequ(sub,"label")) dplabel("symb", arg);

		   else dperr2("unknown sub-command `%s'",sub);

		   if (!haveERR) { redo_rate(); } /* why do this ? */
		   }
		}

	else if (strequ(cmd,"line")) {   /* set line type */
		if (!haveARG) dperr2("no argument given for '%s'", cmd);

		else {
		   usedARG = T;
		   if (!haveSUB || strequ(sub , "type")) {
			if (haveNUM) {
				pen = G_MOVE;
				lin_typ = (int) (rval + round);
				gnline(lin_typ) ;
				}
			else dperr2("numeric value required for `%s'", cmd);
			}

		   else if (strequ(sub, "label")) dplabel("line", arg);
		   }
		}

	else if (strequ(cmd,"pen")) {   /* set pen size */
		if (!haveARG) {
			if (!haveSUB) dperr("bad 'pen' command") ;
			else if (strequ(sub, "up"))   pen = G_MOVE ;
			else if (strequ(sub, "down")) pen = G_DRAW ;
			else if (strequ(sub, "data")) {
				pen_data = PEN_DATA;
				gnpen(pen_data) ;
				}
			else if (strequ(sub, "axis"))
				pen_axis = PEN_AXIS;
			else if (strequ(sub, "symb"))
				pen_axis = PEN_SYMB;
			else dperr2("bad sub-command '%s'",sub);
			}

		else if (haveNUM) {
		   usedARG = T;
		   if (!haveSUB || strequ(sub , "data")) {
			pen_data = (int) (rval + round) ;
			gnpen(pen_data) ;	/* set it now */
			}

		   else if (strequ(sub, "axis"))
			pen_axis = (int) (rval + round) ;

		   else if (strequ(sub, "symb"))
			pen_symb = (int) (rval + round) ;

		   else dperr2("bad sub-command '%s'",sub) ;
		   }
		else dperr2("bad argument to '%s'", cmd) ;
		}

	/* KLUDGE for YOSHI -- install a real version of this someday */
	else if (strequ(cmd,"axsize")) {
		extern float Lab_size, Tlab_size ;

		if (!haveARG) dperr2("no argument given for '%s'", cmd);

		else {
		   usedARG = T;
		   if (!haveSUB || strequ(sub , "title")) {
			if (haveNUM) Lab_size = rval ;
			else	     Lab_size = 0.0 ;
			gnpen(pen_symb) ;	/* set it now */
/* Tue Jan 30 15:27:58 PST 1990:: ^ mje: why? */
			}

		   else if (strequ(sub, "label")) {
			if (haveNUM) Tlab_size = rval ;
			else	     Tlab_size = 0.0 ;
			}
		   }
		}


	/* set some physical parameters.... */
	else if (strequ(cmd, "phys")) {
		usedARG = T;

		if (!haveSUB) dperr("value required for 'phys'");
		else if (!haveNUM) dperr("require numeric value");
		else {
			mkcmd(sub, scmd, ssub);
			stphys(scmd, ssub, rval, (arg[strlen(arg)-1] == '%'));
			}
		}

	/* HACK for Geert: call axplt2() directly */
	else if (strequ(cmd, "axplt2")) {
		if (!haveARG) dperr("values required for 'axplt2'") ;
		else {
			DoAxplt2(arg) ;
			usedARG = T ;
			}
		}


	/* 3. these commands take no values,
	 *  they are a. boolean flags OR  b. action commands.
	 *  Or now c. input from gpgeti (ie: G_CLOC)
	 */
               
	else if (strequ(cmd,"axis"))  /* draw an axis (now) */
		{
		int   len;
		float avlen ;		/* average axis length */

		if (NewPlot) {
			NewPlot = F ;
			fxphys();
			}

		/* make sure line type is one for this, and new pen size */
		if (lin_typ != 1)         gnline(1);
		if (pen_axis != pen_data) gnpen(pen_axis) ;

		/* make sure clipping is un-set */
		iv[0] = G_ONLOG ;
		(void)gpgeti(G_CLIPL, iv, fv) ;	/* get low values */
		x   = fv[0]; y   = fv[1] ;

		iv[0] = G_ONLOG ;
		(void)gpgeti(G_CLIPH, iv, fv) ;	/* get high values */
		xt1 = fv[0]; yt1 = fv[1] ;

		/* reset to no clipping */
		gplot2(G_CLIPL, G_RESET, 0., 0.) ;
		gplot2(G_CLIPH, G_RESET, 0., 0.) ;

		/* Base sizes on average axis length */
		avlen = (plotX1-plotX0 + plotY1-plotY0) / 2.0 ;

		/* plot the title of the plot. */
		len = strlen(title);
		symbl2(plotX0 + (plotX1-plotX0)/2., plotY1+(0.020*avlen), 
					title, len, 
					0.025*avlen, 0.025*avlen, 0., 0., .5);

		if (haveSUB) {
			if (strequ(sub, "box")) {  /* just a simple one */
				gmove(plotX0, plotY0) ;
				gdraw(plotX1, plotY0) ;
				gdraw(plotX1, plotY1) ;
				gdraw(plotX0, plotY1) ;
				gdraw(plotX0, plotY0) ;
				}

			else dperr2("unknown sub-command '%s'", sub) ;
			}

		else {	/* normal full axis */
			/* plot the Y-axis */
			doaxis('y', plotX0,plotY0, plotX1,plotY1, minY, maxY,
				labelY, logYplot, avlen);

			/* plot the X-axis */
			doaxis('x', plotX0,plotY0, plotX1,plotY1, minX, maxX,
				labelX, logXplot, avlen);
			}

		if (lin_typ != 1)	  gnline(lin_typ) ; /*reset line type*/
		if (pen_axis != pen_data) gnpen(pen_data) ; /* reset pen size*/

		gplot2(G_CLIPL, G_ONLOG, x  , y  ); /* reset clips */
		gplot2(G_CLIPH, G_ONLOG, xt1, yt1);
		pen = G_MOVE ;
		}/* of axis plotting */


	else if (strequ(cmd,"clear"))     /* clear screen now */
		{
		gclear();

		if (NewPlot) { NewPlot = F ; fxphys(); }
		}

	else if (strequ(cmd,"log"))       /* set LOGPLOT flags */
		{
		if (!haveSUB) logXplot = logYplot = T;

		else if (strequ(sub,"x")) logXplot =T;
		else if (strequ(sub,"y")) logYplot =T;

		else dperr2("unknown sub-command '%s'",sub);

		if (!haveERR) redo_rate();
		}

	
	else if (strequ(cmd,"linear"))       /* un-set LOGPLOT flags */
		{
		if (!haveSUB) logXplot = logYplot = F;

		else if (strequ(sub,"x")) logXplot =F;
		else if (strequ(sub,"y")) logYplot =F;

		else dperr2("unknown sub-command '%s'",sub);

		if (!haveERR) redo_rate();
		}

	else if (strequ(cmd, "abs")) {		/* take absolute values */
		if (!haveSUB) absX = absY = T ;

		else {
			n = T ;		/* assume turned on */
			mkcmd(sub, scmd, ssub);	/* break out sub-sub */
			if (*ssub) {
				if      (strequ(ssub, "off")) n = F ;
				else if (strequ(ssub, "on"))  n = T ;
				else dperr2("unknown sub-subcommand '%s'",
								ssub);
				}

			if (!haveERR) {
				if      (strequ(scmd,"x"))   absX = n ;
				else if (strequ(scmd,"y"))   absY = n ;
				else if (strequ(scmd,"on"))  absX = absY = T ;
				else if (strequ(scmd,"off")) absX = absY = F ;
				else dperr2("unknown sub-command '%s'",scmd) ;
				}
			}
		}

	else if (strequ(cmd,"by"))       /* plot by flags. */
		{
		if (!haveSUB) by_how = BY_LINE;    /* default by line */

		else if (strequ(sub,"line"))  by_how = BY_LINE;
		else if (strequ(sub,"symb"))  by_how = BY_SYMB;
		else if (strequ(sub,"both"))  by_how = (BY_LINE | BY_SYMB);

		else dperr2("unknown sub-command '%s'",sub);
		}


	else if (strequ(cmd,"reset"))      /* get to initial state */
		{
		if (!haveSUB) dp_reset();
		else if (strequ(sub, "points") || strequ(sub, "cols")) {
			gotX = gotY = F ;
			colcnt = 0 ;
			}
		}

	else if (strequ(cmd, "locate")) {	/* Use tablet locator */
		iv[2] = (haveNUM? (int)rval : 0) ;

		/* Use Get-cursor-location to set new label locations */
		gpgeti(G_CLOC, iv, fv) ;

		xlloc = fv[0] ;		/* new x */
		ylloc = fv[1] ;		/* new y */

		if (llocs_rel) {	/* need to convert to data values */
			xlloc = fixpnt('X', xlloc) ;
			ylloc = fixpnt('Y', ylloc) ;
			}

		/* Make sure the changed location gets changed */
		sprintf(scmd, "%g", xlloc) ;
		logit("label", "loc.x", scmd) ;
		sprintf(scmd, "%g", ylloc) ;
		logit("label", "loc.y", scmd) ;
		}


	/* Commands that take string arguments */
	else if (strequ(cmd,"file") )   /* include a file. */
		{
		if (!haveARG) dperr("no file name given");

		else {
		        usedARG = T;
			/* be nice and see if it is a binary save file; if so
			 *  just do it as if they said $tplot file
			 */
			n = gpopen(arg, GPO_RD|GPO_HD) ;
			if (n >= 0) {
				close(n) ;	/* oop, a gplot file */
				n = sv_file(arg) ;
				if (n < 0) dperr2("on open of tplot file '%s'",
									arg);
				}

			/* Not a gplot save file; just text file it */
			else {
				fpnew = fopen(arg, "r");
				if (fpnew != NULL) {
					dp_file(arg, fpnew);
					fclose(fpnew);
					}

				else dperr2("on open of file '%s'", arg);
				}
			}
		}

	else if (strequ(cmd,"tplot"))   /* plot back a tplot file */
		{
		if (!haveARG) dperr("no file name given");

		else {
		        usedARG = T;
			ErrCode = sv_file(arg);

			if (ErrCode < 0) 
				dperr2("on open of tplot file '%s'", arg);
			}
		}

	/* set the plot device */
	else if (strequ(cmd,"dev") || strequ(cmd,"device") || strequ(cmd,"d")){
		if (haveARG) {
			usedARG = T;

			if ((ErrCode = setpdev(arg)) == 0)  {
				fxphys();
				NewPlot = F ;
				}

			else dperr2("setting plot dev '%s'",arg);
			}

		else dperr("no plot device given");
		}

	/* plotting output file */
	else if (strequ(cmd, "outfile")) {
		if (haveARG) {
			usedARG = T;

			if ((ErrCode=setpfil(arg)) != 0) 
				dperr2("setting plot output file `%s'", arg);
			}
		else dperr("no output file given");
		}

	/* set auto saving on  */
	else if (strequ(cmd, "asave")) {

	/* should be:
	 *	asave      namr
	 *	asave.file namr
	 *	asave.on
	 *	asave.off
	 *	asave.stop
	 */

		if (haveARG) {
			usedARG = T;

			if (haveSUB) {
				if      (strequ(sub,"file")) setsfil(arg);
				else if (strequ(sub,"on")) 
					gplot2(G_ASAVE,G_ON,0.,0.);
				else if (strequ(sub,"off")) 
					gplot2(G_ASAVE, G_OFF, 0.,0.);
				else if (strequ(sub, "stop"))
					gplot2(G_ASAVE, G_STOP, 0., 0.);

				else dperr2("unknown sub-command `%s'", sub);
				}

			else setsfil(arg);
			}

		else dperr2("no file name for %s", cmd);
		}



	/* take input from the tty */
	else if (strequ(cmd,"tty")) dp_file(ttynam,stdin);


	/* set axis title */
	else if (strequ(cmd,"title")) {
		if (!haveARG) dperr("no title given");

		else {
			if (haveSUB)dperr("no sub-command needed for `title'");
			else {
				strcpy(title , arg);	/* save title */
				usedARG = T ;
				}
			}
		}


	/* set axis labels...... */
	else if (strequ(cmd,"label")) {
		if (!haveARG) {
			if (haveSUB) {
				mkcmd(sub, scmd, ssub);
				(void)stlloc(ssub,haveARG,rval);
				}
			else dperr("no label given") ;
			}

		else {
		        usedARG = T;
			if (haveSUB) {
				/* break down the sub command, just in case */
				mkcmd(sub, scmd, ssub);

				if      (strequ(sub,"x")) strcpy(labelX,arg);
				else if (strequ(sub,"y")) strcpy(labelY,arg);
			        else if (strequ(sub,"siz")||strequ(sub,"size")){
			            if (haveNUM) l_siz = rval;
			            else
				       dperr2("numeric value required for `%s'",
					       cmd);
				    }

				else if (strequ(scmd,"loc")) 
					usedARG = stlloc(ssub,haveARG,rval);
				
				else dperr2("unknown sub-command '%s'", sub);
				}

			/* else if no sub-command, draw a label now at curloc*/
			else dplabel("label", arg) ;
			}
		}

	/* shell escape/catch output back here. A brand new command!(apr 84) */
	else if (strequ(cmd,"sys")) {
		char *com_line;

		if (haveARG) com_line = arg;
		else         com_line = 0 ;    /* for now. Prompt for it */

		if (haveSUB) {
			if (strequ(sub, "in")) sysin(com_line, T);

			else dperr2("unknown sub-command `%s'", sub);
			}

		else {      /* just a quick shell escape */
			sysin(com_line, F);
			}

		}

	/* Change our idea of the current working directory */
	else if (strequ(cmd, "chdir") || strequ(cmd, "cd")) {
#ifdef CHDIR 
		char*	getenv() ;

		if (!haveARG) {		/* no arg, get home name */
			if (!(arg = getenv("HOME")) &&
			    !(arg = getenv("LOGDIR"))  )
				dperr("No home directory set") ;
			else haveARG = T ;
			}

		if (haveARG) {
			if (chdir(arg) < 0) {	/* error? */
				dperr("change directory failed") ;
				/* if loader can't find this, just delete it */
				perror("\t") ;
				}
			}
#else
		dperr2("%s is not implemented in this version", cmd) ;
#endif /*CHDIR*/
		}/*of chdir command */

	/* simple ECHO command (for script files - so you know where you are */
	else if (strequ(cmd, "echo")) {
		int	with_nl = T ;
		if (haveSUB) {
			if (strequ(sub,"nl")) with_nl = F ;
			else dperr2("Unknown echo sub-command '%s'",sub);
			}
		if (haveARG) printf("%s", arg) ;
		if (with_nl) printf("\n") ;
		}

	/* take a litteral gplot command.  This probably should go away
	 *  some day, but for degugging, etc it is helpful.
	 */
	else if (strequ(cmd, "gplot") || strequ(cmd, "g")) {
		if (haveARG) {
			bool	on = T ;
			usedARG = T;
			/* parse the command */
			if (gtcmd(arg, &c, &s, &x, &y) == 0) {
				if (seg_on(c, s))
					gplot2(c, s, x, y); /* and do it ... */
				}

			if (NewPlot) { fxphys() ; NewPlot = F; }
			}

		else dperr("No arguments given to 'gplot' command");
		}

	else if (strequ(cmd, "gpget")) {
		char	sv[100] ;
		int	ni, nf, nx ;

		if (haveARG) {
			usedARG = T ;
			for (n = 0; n < 8; n++) { iv[n] = 0; fv[n] = 0.0; }
			if (gtcmd(arg, &c, &s, &x, &y) == 0) {
				iv[0] = s ;
				n = gpmisc(c, G_GET, iv, fv, sv) ;
				if (n < 0) dperr2("gpmisc error %d\n",n) ;
				else {
					ni = n & 0xf ;
					nf = (n >> 4) & 0xf ;
					nx  = (ni > nf? ni: nf) ;
					printf("\tiv\tfv\t(return %d)\n",n) ;
					for (n = 0 ; n < nx ; n++) {
						printf("%d\t", n) ;
						if (n<ni) printf("%d",iv[n]);
						printf("\t") ;
						if (n<nf) printf("%g",fv[n]) ;
						printf("\n") ;
						}
						
					}
				}
			}
		else dperr("No argument given to 'gpget' command") ;
		}

	/* some cheap debug commads */
	else if (strequ(cmd, "D") || strequ(cmd, "debug")) {
		mkcmd(sub, scmd, ssub);
		ddcmd(scmd, ssub, arg, rval);
		}
			
			
	/* UNKNOWN command */
	else dperr2("Unknown command '%s'", cmd);

	/* debug stuff */
	if (!haveERR && haveCMD)
		logit(cmd, sub, arg);   /* log the commands */

	/* putback unused arg */
	if ((haveARG || haveNUM) && !usedARG) return(DOBACK) ;

	/* else */
	return(NOBACK); 

	}/* of docmd */


/* HACK for Geert */
#include <ctype.h>

#define T_INT	1
#define T_FLT	2
#define T_STR	3

static struct a2_args {
	short	type ;		/* float, int, char* */
	union {
		float	f ;
		int	i ;
		char*	s ;
		} t ;
	} aa[] = {
		{ /*  0 */	T_FLT	} ,	/* xst	*/
		{ /*  1 */	T_FLT	} ,	/* yst	*/
		{ /*  2 */	T_FLT	} ,	/* len	*/
		{ /*  3 */	T_FLT	} ,	/* axang	*/
		{ /*  4 */	T_FLT	} ,	/* Fval	*/
		{ /*  5 */	T_FLT	} ,	/* Eval	*/
		{ /*  6 */	T_FLT	} ,	/* Flab	*/
		{ /*  7 */	T_FLT	} ,	/* Llab	*/
		{ /*  8 */	T_FLT	} ,	/* ltic	*/
		{ /*  9 */	T_INT	} ,	/* utic	*/
		{ /* 10 */	T_FLT	} ,	/* labht	*/
		{ /* 11 */	T_FLT	} ,	/* tlang	*/
		{ /* 12 */	T_INT	} ,	/* tloc	*/
		{ /* 13 */	T_FLT	} ,	/* tang	*/
		{ /* 14 */	T_FLT	} ,	/* tht	*/
		{ /* 15 */	T_FLT	} ,	/* tdp	*/
		{ /* 16 */	T_STR	} ,	/* title	*/
		{ /* 17 */	T_FLT	} ,	/* axtht	*/
		{ /* 18 */	T_INT	} ,	/* lform	*/
		{ /* 19 */	T_STR	}  	/* vform	*/
	} ;

/* "DoAxplt2":  call axplt2() directly */
DoAxplt2(str)
	char*	str ;
	{

	register char*  s = str ;
	register char*	p ;
	register int	i ;
	int	iv ;
	float	fv ;
	char*	sv ;
	float	atof() ;

	for (i = 0 ; i < 20 ; i++) {
		p = s ;
		while (*s && *s != ',') s++ ;
		if (*s) *s++ = '\0' ;

		if (aa[i].type == T_FLT || aa[i].type == T_INT) {
			while (*p && isspace(*p)) ++p ;

			if (aa[i].type == T_FLT) aa[i].t.f = atof(p) ;
			else			 aa[i].t.i = atoi(p) ;
			}

		else if (aa[i].type == T_STR) {
			aa[i].t.s = p ;
			}
		}

	axplt2(	aa[ 0].t.f,
		aa[ 1].t.f,
		aa[ 2].t.f,
		aa[ 3].t.f,
		aa[ 4].t.f,
		aa[ 5].t.f,
		aa[ 6].t.f,
		aa[ 7].t.f,
		aa[ 8].t.f,
		aa[ 9].t.i,
		aa[10].t.f,
		aa[11].t.f,
		aa[12].t.i,
		aa[13].t.f,
		aa[14].t.f,
		aa[15].t.f,
		aa[16].t.s,
		aa[17].t.f,
		aa[18].t.i,
		aa[19].t.s
		) ;
	}
