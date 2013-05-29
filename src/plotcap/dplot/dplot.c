/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 24 aug 87 (mje)
 * 
 * "dplot" : A general plotting program/manipulater. 
 *
 * Notes:
 *	1. Opens of text files should be done with a routine that first checks
 *		to make sure it is not a `tplot' file. (to prevent the core
 *		dump).
 *
 * written : Michael Eldredge (oct 83)
 * mod     : MJE (feb 86) add preamble to log files.
 * modified: MJE (aug 87) make sure to 'exit(0) ;' each time.
 */

#include <stdio.h>
#include "dp_def.h"
#include "dp_com.h"
#include "gplot.h"

#define  isdigit(C) ((C) >= '0' && (C) <= '9')

#define gsomething() (gpmisc(G_NULL, G_GET, iv, fv, "") >= 0)
static	int   iv[2] ;
static	float fv[2] ;


main(argc, argv)
	int   argc;
	char  *argv[];

	{
	char   cmd[WORDSIZE],  sub[WORDSIZE] ;
	int    na;
	int    NullArgs = T ;    /* null effect arguments only? */
	char   DefType, *ap;


	/* ----- Start by init'ing things and resetting for plotting ---- */
	dp_init();
	log_val = 0 ;			/* no auto-logging for now */


	/* ---- step through each argment, remember if it had an effect ---- */

	DefType = 'f';                 /* files are text files by default */
	for (na=1; na < argc; na++) {
		
		if      (strequ(argv[na], "-T")) DefType = 't';  /* tplot */
		else if (strequ(argv[na], "-F")) DefType = 'f';  /* text  */

		else if (strequ(argv[na], "-l") || strequ(argv[na],"-l2")) {
			if (log_val++ == 0) {	/* open once */
			   if ((fplog = fopen("dplot.log", "w")) != NULL) {
			      /* If re-read, these must always be set so: */
			      fprintf(fplog,"#! dplot\n") ;
			      fprintf(fplog,"# -- To re-read: always 2 cols\n");
			      fprintf(fplog,"$ncols 2\n") ;
			      fprintf(fplog,"$col.x 1\n") ;
			      fprintf(fplog,"$col.y 2\n\n") ;
			      }

			   else fplog = 0;	/* error on open */
			   }
			if (strequ(argv[na], "-l2")) log_val++ ;
			}

		else if (strequ(argv[na], "-t")) {  /* do a tplot file */
			if (++na < argc) docmd("tplot", "", argv[na]);
			else dperr("no tplot file name given");
			NullArgs = F ;  /* had an effect (presumably) */
			}

		else if (strequ(argv[na], "-f")) {  /* text file to do */
			if (++na < argc) docmd("file", "", argv[na]);
			else dperr("no file name given");
			NullArgs = F ;  /* had an effect (presumably) */
			}

		else if (strequ(argv[na], "-o")) {  /* plot output file */
			if (++na < argc) docmd("outfile", "", argv[na]);
			else dperr("no plot output file given");
			}

		else if (strequ(argv[na], ",") || 
			 strequ(argv[na], "-") ||
			 strequ(argv[na], "-i")
			   ) {
			docmd("tty", "" , "");
			}

		else if (argv[na][0] == '-') {   /* oooh! a command! */

			ap = argv[na]; ap++;  /* past the minus */
			*cmd = 0;
			*sub = 0;

			mkcmd(ap, cmd, sub);

			/* look for argument (possible) */
			if (na+1 < argc && 
			   (argv[na+1][0] != '-' || isdigit(argv[na+1][1])) ) {
				/* give next arg as poss arg, if not used, 
				 *  move arg count back (put back the arg).
				 */
				if (docmd(cmd, sub, argv[++na]) == DOBACK) na--;
				}

			/* no argument given */
			else docmd(cmd, sub, "");

			/* well this may not be true.... but too hard to check
			 *  all the way in docmd... so just leave it. mje
			 */
			NullArgs = F ;  /* had an effect (presumably) */
			}

		/* not a command, must be an input file of 'DefType' type */
		else {
			if (DefType == 't') docmd("tplot", "", argv[na]);
			else                docmd("file" , "", argv[na]);
			NullArgs = F ;  /* had an effect (presumably) */
			}

		}/* for each arg */

	/* go interactive if we didn't do anything in the above */
	/*if (NullArgs) */
	if (!gsomething()) 
		dp_file(ttynam, stdin) ;  /* incase Args didn't do anything.. */

	/* --- Make sure all plotting functions get finished up ---- */
	if (lin_typ != 1) gnline(1) ;
	gpend();

	if (fplog) fclose(fplog);   /* close up log file if used */

	exit(0) ;		/* clean return */
	}/* of main */
