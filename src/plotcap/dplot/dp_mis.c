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
 * Wed Jan 31 11:22:14 PST 1990 (dredge--stanford)
 * "dp_mis.c" : miscelanious routines 
 *
 * modified: Michael Eldredge (mar 87) added segment command.
 * modified: mje -- stanford (sep 89) added include of version codes
 * modified: mje -- stanford (jan 90) added separate label size.
 */

#include <stdio.h>
/* version (possibly) needed for the call to makeprmpt() below */
#include "dp_vers.h"
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"


/* "dp_init" : This is called in the main to init stuff and to reset state */
dp_init()
	{
	char   *getenv();
	FILE   *fp;
	char	*rc ;


	ErrCode = 0;
	fplog   = 0;     /* no logging */
	log_lev = 0;     /* number of files in, `do_file' depth */

	dp_reset();

	/* -----find out who the user is for fun-ish error reporting ---- */
	strcpy(ttynam, getenv("USER") );
	strcat(ttynam, "'s terminal"  );

	MAKEPRMT();	/* set up default prompt */

	/* --- see if the user has a start up file ----- */
	/* First see if they have a preferred name */
	if (rc = getenv("DPLOTRC")) {
		fp = fopen(rc, "r") ;
		}

	/* otherwise, use standard names.... */
	else {
		/* check current directory */
		fp = fopen(".dplotrc", "r") ;
		if (fp == NULL) {
			if ((rc = getenv("HOME")) || (rc = getenv("LOGDIR"))) {
				/* Borrow the title buffer */
				strcpy(title, rc) ;	/* try home directory*/
				strcat(title, "/.dplotrc") ;   
				fp = fopen(title, "r") ;
				title[0] = 0 ;/* restore title buf, thank you */
				}
			}
		}

	if (fp != NULL) {
		dp_file(title, fp);
		fclose(fp);
		}

	redo_rate() ;
	}


/* "dp_reset" : initialize all values etc for DPLOT program.
 *
 * Wed Jan 31 11:22:41 PST 1990 (dredge--stanford)
 */

dp_reset()
	{

	/* set up all common variables */
 	x0 = x1 = 0.0;
	y0 = y1 = 0.0;
	pen = G_MOVE;

	scaleX = scaleY = 1.0;      /* default scaling */
	transX = transY = 0.0;      /* default translation */
	rotateX= rotateY= 0.0;      /* rotate about (x,y) */
	rotateA= 0.0;               /* rotation angle     */
	/* should we do: greset(G_SCALE) ? */

	gotX   = gotY = F;          /* don't have vales yet */
	colcnt = 0 ;
	logXplot = logYplot = F;    /* linear plots is default   */

	by_how  = BY_LINE;          /* plotting by lines (not by symbols) */
	s_siz   = SYMB_SIZE ;       /* default symbol size */
	s_sym[0]= (char) 0;         /* default symbol      */
	l_siz   = 1.5 * s_siz ;	    /* historical setting! */
	lin_typ = 1;

	absX   = absY   = F ;
	addtoX = addtoY = 0.0 ;
	mulbyX = mulbyY = 1.0 ;

	pen_data = PEN_DATA ;
	pen_axis = PEN_AXIS ;
	pen_symb = PEN_SYMB ;

	xlloc = ylloc = 0.0 ;
	llocs_rel = F ;

	lin_count = pnt_count = 0 ;

	redo_rate();
	}



/* "redo_rate" : re-calculates the rates for x and y plotting 
 */

redo_rate()
	{
	float  wind;

	/* first for X window. */
	if (logXplot) wind = Log_10(maxX) - Log_10(minX) ;
	else          wind = maxX - minX;

	rateX = 0.0;
	if (wind > 0.0) rateX = (plotX1 - plotX0) / wind ;

	/* then for Y window. */
	if (logYplot) wind = Log_10(maxY) - Log_10(minY) ;
	else          wind = maxY - minY;

	rateY = 0.0;
	if (wind > 0.0) rateY = (plotY1 - plotY0) / wind;
	}


/* "Log_10" : our own little log routine that takes floats and deals with
 *		zero as an argument by returning a very large neg number.
 */
double
Log_10(val)
	float val;
	{
	double log10();
	static double big = -38.0 ;  /* close enough to a big neg */

	if (val <= 0.0)   return(big);
	else              return( log10((double) val) );
	}


/* "logit" : log commands if so desired. */
logit(cmd,sub,arg)
	char *cmd, *sub, *arg;
	{
	int	n = log_lev;

	if (!fplog) return;

	if (*cmd) {
		/* skip saving these */
		if ( strequ(cmd, "file")	||
		    (strequ(cmd, "tplot" ) && log_val == 2)	||
		     strequ(cmd, "col" )	||
		     strequ(cmd, "locate")	||
		     strequ(cmd, "tty")		||
		     strequ(cmd, "ncols")
		    ) fprintf(fplog, "#==>> ") ;

		while (n--) putc('\t', fplog);  /* indent to level */

		fprintf(fplog,"$%s", cmd);
		if (*sub) fprintf(fplog,".%s ", sub);
		else      putc(' ', fplog) ;

		/* See if it is a string to be output... */
		if ( (strequ(cmd, "label") && !strnequ(sub,"loc", 3)) ||
		      strequ(cmd, "sys")	||
		      strequ(cmd, "title")	||
		      strequ(cmd, "file")	||
		      strequ(cmd, "tplot")	||
		      strequ(sub, "label")
		      ) otstr(fplog, arg) ;	/* It is a string, quote it */

		else fprintf(fplog," %s", arg);
		putc('\n', fplog) ;
		}

	return;
	}

/* "logit_dat" : log the data points that we've got.
 */
logit_dat(x, y)
	float x, y;
	{
	int n = log_lev;

	if (!fplog) return;

	while (n--) putc('\t', fplog);  /* indent to level */
	fprintf(fplog, "%g %g\n", x, y);
	}

/* "logit_gp": log gplot calls for rereading if needed */
logit_gp(c, s, x, y)
	int	c, s ;
	float	x, y ;
	{
	int	n = log_lev ;

	if (!fplog || log_val <= 1) return ;

	while (n--) putc('\t', fplog) ;	/* indent to level */
	fprintf(fplog, "$gplot %d,%d,%g,%g\n", c, s, x, y) ;
	}


/* "otstr" : output a string, escaping quotes if necessary */
otstr(fp, str)
	FILE  *fp;
	char  *str;
	{
	char *index();
	bool sing, doub, both;
	char q1 = '\'', q2 = '"', qu;

	/* do we have single or double quotes in the string, (hope not!) */
	sing = index(str, q1) != 0;
	doub = index(str, q2) != 0;

	/* if only one kind, set 'qu' to other kind */
	if (!(both = sing && doub)) qu = (doub ? q1 : q2);
	else                        qu = q2;   /*pick one, it'll get escaped */

	putc(qu , fp);
	while (*str) {
		if (both && *str == qu) putc('\\', fp);
		putc(*str++, fp);
		}
	putc(qu , fp);
	}

/* "seg_set": define a segment to be deleted: 'd' (or remove it from the delete
 *	list : 'k'), or see if it is set: '?'
 */
int
seg_set(nseg, action)
	int	nseg ;
	char	action ;
	{
	int	i , j ;
	int	iret = 0 ;

	if (nseg == 0) return ;
	if (nseg < 0) nseg = -nseg ;

	/* search for given segment */
	for (i = 0, j = -1 ; j == -1 && i < seg_count ; i++) {
		if (seg_list[i] == nseg) j = i ;
		}

	/* if adding to the delete list... */
	if (action == 'd') {
		if (j == -1) {
			if (seg_count < MAX_SEG_DEL) {
				seg_list[ seg_count++ ] = nseg ;
				}
			else dperr2("Sorry, can only delete %d segments",
							MAX_SEG_DEL) ;
			}
		/* else already in list... */
		}

	else if (action == 'k') {	/* keep segment, don't delete */
		if (j != -1) {			/* in the list, remove it */
			for (i = j + 1; i < seg_count; )
				seg_list[j++] = seg_list[i++] ;
			--seg_count ;
			}

		/* else not in the list */
		}

	else if (action == '?')
		iret = (j != -1) ;

	return iret ;
	}

/* "seg_on": see if a seg is turned keeping on or off.  */
int
seg_on(cmd, sub)
	int	cmd ;
	int	sub ;
	{
	int	is_on ;

	if (cmd != G_MARK) return (seg_depth <= 0)  ;	/* leave it alone */

	if      (sub > 0 && seg_set(sub, '?'))  is_on = (++seg_depth <= 0) ;
	else if (sub < 0 && seg_set(sub, '?')) {
		is_on = (seg_depth-- <= 0) ;	/* check before changing */
		if (seg_depth < 0) seg_depth = 0 ;  /* safety first */
		}

	return is_on ;
	}


