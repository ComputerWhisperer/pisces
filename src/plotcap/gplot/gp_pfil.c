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
 * Wed Jan 31 17:16:36 PST 1990 (dredge--stanford)
 *ff
 * "gp_pfil" : setup the plotting output file for GPLOT.
 *
 * calling sequence:
 *	<i> = gp_pfil(namr, sub)
 *
 * where:
 *	namr  - (char buffer) File name to set up as the plotting output
 *		file. If 'namr' is null then use default file name
 *		from plotcap entry if one exists.
 *	sub	- (int) G_BEGIN: Open the file given by namr.
 *			G_STOP : Finish and close current file (namr not used)
 *	Return- (int) If no errors (all's OK) return 0; else return -1.
 *of
 * Notes:
 *	1.  Here, the 'creat' routine will either create the given file
 *		if it does not exist OR is will open the already existing
 *		and truncate it to 0 bytes.  The 'open' system call will
 *		NOT truncate an already existing file, thus if the new
 *		file is shorter, there will be garbage at the end.  Thus
 *		'creat' is the proper routine to "open" the file. (mje)
 *	2.  If the filename (namr) has `+' as its first character the 
 *		remainder of the string is taken as the file name which is
 *		opened in append mode.
 *	3.  If the filename (namr) has `|' as its first character the
 *		remainder of the string is taken as a command to which to
 *		pipe the plot output.
 *	4.  If the filename (namr) has '||' as its first characters the
 *		remainder of the string is taken as a command to which to
 *		pipe the plot output (as in #3) AND from which the stderr
 *		can be read (for G_CLOC) functions.
 *	5.  If the filename is simply "." then output to the stdout.  A
 *		simple way to over-ride the defaults and get output to
 *		the terminal.
 *
 * written:  Michael Eldredge (jan 84)
 * modified: Michael Eldredge (dec 86) Added Cross pipe stuff FILE = "|| tool"
 *	to support ``tools'' on Suns/X etc returning G_CLOC info.
 * modified: Michael Eldredge (dec 86) Use only gpopen() to open/create files.
 *	(This is our own buffering stuff....). (Trying to get rid of stdio)
 *	+ Changed calling sequence to be called from gpmisc() and added
 *	  setpdev() to gplot.h
 * modified: MJE (jul 88) Changed order of "default" file names.
 *	Now: given -> DEFPFIL -> plotcap FILE= -> stdout
 *	Was: given -> plotcap FILE= -> DEFPFIL -> stdout
 * modified: MJE (oct 88) A file name of "-" will special case to stdout
 *	(like ".") has...
 * modified: MJE (oct 89) Don't close ends of the pipe in the child if we
 *	never opened them! (from mark law).
 */

/* used in gp_copen() (maybe...) */
#include <signal.h>
#include <unistd.h>
#include "auxfns.h"
#include "gp_mis12.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"
/* ^ just for the definition of g_stop */

/* For gp_copen() */
#define	COP_PIPE	1	/* pipe to a command */
#define	COP_CROSS	2	/* cross pipe: wr to stdin, rd from stderr */

static pfilUnset(void);
static int gp_copen(char *cmdstr, int mode, int lus[]);

/* "gp_pfil" : the real file setup routine */
#ifdef ANSI_FUNC

int 
gp_pfil (
    char *namr,
    int sub	/* G_BEGIN, G_STOP */
)
#else

gp_pfil(namr, sub)
	char*	namr;
	int	sub ;	/* G_BEGIN, G_STOP */
#endif
	{
	char  *np, *up;     /* local pointer to name and uniq part */
	Bool  appendit = F; /* open in append mode ?? */
	Bool  tocmd    = F; /* open to write to a command ?? */
	Bool  tocross  = F; /* create a cross pipe? */
	int	 lu, lus[2] ;	/* lus for cross pipe */
	int   ierr = 0;
	char  *getenv(), *needunq();

	if (sub == G_STOP) {
		pfilUnset() ;	/* must have done gp_setdn() else where! */
		return 1 ;
		}

	if (*namr) { 	/* user gave us a file name */
		/* special case: stdout */
		if (strequ(namr, ".") || strequ(namr, "-")) {
			al_poki(AP_LUPTO , LUODEF ) ;

			lu_oplt = LUODEF ;		/* for un setting */
			g_filset = T;
			return(0);
			}
			
		np = namr;
		up = needunq(np);     /* see if name requires unique-ing */
		}

	else if (np = getenv(DEF_FIL)) {
		up = needunq(np) ;
		}

	else if (*plot_file) {
		np = plot_file;       /* use internal name */
		up = punq_loc ;       /* already looked for unique-ing */
		}

	else {                        /* just use LUODEF */
		al_poki(AP_LUPTO , LUODEF ) ;

		lu_oplt = LUODEF ;		/* for un setting */
		g_filset = T;
		return(0);
		}

	if      (*np == '+') { appendit  = T;	np++; }
	else if (*np == '|') {
		if (*++np == '|') tocross = T ;	/* double pipe */
		else		  tocmd   = T;
		++np ;
		}

	(void)mkuniq(np, up);/* if a uniq name is needed, this will get one */

	/* try to open the output file for writing, if err: reset to default*/
	if      (tocmd)	{		/* just pipe to a command */
		lu = gp_copen(np, COP_PIPE, lus) ;
		if (lu >= 0) lu = lus[0] ;	/* no error */
		}
	else if (tocross) {		/* pipe to and read from */
		lu = gp_copen(np, COP_CROSS, lus) ;
		if (lu >= 0) {
			lu = lus[0] ;			/* to */
			al_poki(AP_LUGTO, lus[1]) ;	/* from */
			}
		}

	else lu = gpopen(np, (appendit? (GPO_WR|GPO_AP): GPO_WR) ) ;

	/* a good open was done */
	if (lu >= 0) g_filset = T;
	else {			/* a bad open, just use Default lu */
		lu = LUODEF;
		ierr = -1;
		}

	al_poki(AP_LUPTO , lu ) ;
	lu_oplt = lu ;			/* remember for unsetting */
		
	return(ierr);
	}

#ifdef ANSI_FUNC

static 
pfilUnset (void)
#else

static
pfilUnset()
#endif
	{
	if (lu_oplt != LUODEF) {
		gpclose(lu_oplt);	/* post and release buffers */
		lu_oplt = LUODEF;
		al_poki(AP_LUPTO , lu_oplt ) ;
		}

	g_filset = F;
	}

/* command open: return 0 if opened ok. else -1 if some error */
#ifdef ANSI_FUNC

static int 
gp_copen (
    char *cmdstr,	/* the command string to open */
    int mode,		/* COP_PIPE, COP_CROSS */
    int lus[]		/* the input (if 1 or 3) & ouput (if 2 or 3) */
)
#else

static int
gp_copen(cmdstr, mode, lus)
	char*	cmdstr ;	/* the command string to open */
	int	mode ;		/* COP_PIPE, COP_CROSS */
	int	lus[] ;		/* the input (if 1 or 3) & ouput (if 2 or 3) */
#endif
	{

	int	pip_to[2] ;
	int	pip_fr[2] ;
	int	pid ;
	char*	getenv(), *rindex() ;
#ifdef GETENV_SHELL
	char*	shell = getenv("SHELL") ;
#else
	char*	shell = (char*)0 ;	/* just use /bin/sh */
#endif
	char*	name ;

	/* we need this pipe for either case */
	if (pipe(pip_to) < 0) return -1 ;	/* write to program */

	/* only if reading from also (cross pipe) */
	if (mode == COP_CROSS) {	/* read and write */
		if (pipe(pip_fr) < 0) {
			close(pip_to[0]) ;  close(pip_to[1]) ;
			return -1 ;
			}
		}

	/* fork the new command */
	if ((pid = fork()) < 0) {
		close(pip_to[0]) ;  close(pip_to[1]) ;
		if (mode == COP_CROSS) {close(pip_fr[0]); close(pip_fr[1]); }
		return -1 ;	/* couldn't fork */
		}

	/* the child (program to be): stdin:pip_to[0], stdout:pip_fr[1] */
	if (pid == 0) {
		close(0) ;		/* going to get a new stdin */
		dup(pip_to[0]) ;

		if (mode == COP_CROSS) {
			close(1) ;	/* and maybe stdout */
			dup(pip_fr[1]) ;
			}

		/* need none of these now... */
		close(pip_to[0]) ;  close(pip_to[1]) ;
		if (mode == COP_CROSS) {
			close(pip_fr[0]) ;  close(pip_fr[1]) ;
			}

		/* fire up a shell to run the program */
		if (!shell || ! *shell) shell = "/bin/sh" ;
		if (!(name = rindex(shell, '/'))) name = shell ;
		else ++name ;

		execl(shell, name, "-c", cmdstr, (char *) 0) ;	/* do it! */

		return -1 ;		/* couldn't start shell */
		}

	/* PARENT (gplot still) */
	lus[0] = pip_to[1] ;
	close(pip_to[0]) ;
	if (mode == COP_CROSS) {
		lus[1] = pip_fr[0] ;
		close(pip_fr[1]) ;
		}

/* To prevent zombies if we can, ignore the death of the child */
#ifdef SIG_CLD		/* SYS V */
#  define SIG_CHLD SIG_CLD
#endif

#ifdef SIG_CHLD
	signal(SIG_CHLD, SIG_IGN) ;
#endif

	return 0 ;
	}
