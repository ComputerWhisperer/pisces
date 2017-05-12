/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Contents:
 *	1. pdevUnset  -  Internal, undo the set up of a device.
 *	2. gp_pdev    -  User callable, setup for a given plot device.
 */

#include "mkuniq.h"
#include "auxfns.h"
#include "gp_mis12.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gp_pclocs.h"
#include "gplot.h"
/*        ^ only need for the G_SCALE definition below */

#ifndef PCAP_FILE
#  define PCAP_FILE "plotcap"
#endif
#ifndef SYS_GCAP
#  define SYS_GCAP "plotcap.b"
#endif

#include <stdlib.h>


/* Wed Aug 17 14:15:06 PDT 1988 (dredge -- stanford)
 *ff
 * "gp_pdev" : Set the plot device for the 'gplot' plotting routine.
 *
 * calling sequence:
 *	<n> = gp_pdev(devnam, sub);
 *
 * where:
 *	devnam	- (char *) name of plot device to setup.  The device name
 *		is searched for in the gplot capabilities file and if 
 *		found, 'gplot' is set up to do plotting to that device.
 *	sub	- (int) G_BEGIN: Try to open then named device.
 *			G_STOP : Close the current device (devnam not used)
 *
 * returns:
 *	<n>	- (int) Return value.
 *			= 0: setup OK.
 *			< 0: error !!!
 *		  	     = -1 : cap file not found
 *			     = -3 : device not found in given cap file.
 *
 *of
 * written:  Michael Eldredge (aug 84)
 * modified: MJE (Dec 86) clean up, callable from gpmisc().
 * modified: MJE (aug 88) moved clipping setup to gp_setup().
 *
 * notes:
 *	+++ if SET_BIN is defined then we will try to read pre-parsed binary
 *	version of plotcap files (If one exists).
 *	1. Text descriptions of plot capabilities: 'plotcap' file.
 *	2. The binary parsed form to be used by 'gplot',  the 'gplotcap' file.
 *	It is also possible for the user to give another name for either of 
 *	these files, by setting an environment variable to the alternate name.
 *	Thus in C shell, to set text file name:
 *	% setenv PLOTCAP=local.pcap
 *	And to set the binary file name:
 *	% setenv GPLOTCAP=local.gpcap
 *
 *	The order of looking for files is;
 *		1. local gplotcap
 *		2. local plotcap
 *		3. system gplotcap
 *		4. system plotcap
 *	+++ BINARY is not supported-any-more / in-this-release.
 */

/* return from 'get_bin' and 'get_txt' for ``capfile not found'' */
#define NO_CAPFILE -1

static pdevUnset(void);

/* "gp_pdev" : the real device setup routine */
#ifdef ANSI_FUNC

int 
gp_pdev (
    char *devnam,
    int sub		/* G_BEGIN or G_STOP */
)
#else

int
gp_pdev(devnam, sub)
	char*	devnam;
	int	sub ;		/* G_BEGIN or G_STOP */
#endif
	{

	int  ierr;
	char *capfile;

	/* Just show the old one down.. */
	if (sub == G_STOP) {
		if (g_devset) pdevUnset() ;
		return 0 ;
		}
		
	/* start: if all is set already, unset it first */
	if (g_allset) gp_setdn();		/* finish up the last plot */
	if (g_devset) pdevUnset();		/* unset the current device */

#ifdef DO_STATS
	init_stats() ;
	/* ierr is just as easy to use as the dummy loop variable here */
	for (ierr=0 ; ierr<9 ; ierr++) 
		if (! (gp_stats.st_dnam[ierr] = devnam[ierr])) break ;
	gp_stats.st_dnam[ierr] = 0 ;
#endif

#ifdef SET_BIN
	/* local binary file */
	if ((capfile = getenv(LOC_GCAP)))
		ierr = get_bin(capfile, devnam);

	/* local text file */
	else
#endif
	     if ((capfile = getenv(LOC_PCAP))) 
		ierr = get_txt(capfile, devnam);

	/* try the system wide binary file, then the system wide text file */
	else {
#ifdef SET_BIN
		ierr = get_bin(SYS_GCAP, devnam) ;

		if (ierr == NO_CAPFILE)
#endif
			ierr = get_txt(PCAP_FILE, devnam);
		}

	/* if no errors then get all ready for gplot */
	/* whether caps picked up from text or bin file, common is all set */
	if (ierr == 0) {

		gp_reform(xfrm_ord, X_DEFS) ;     /* redo xformation consts */

		/* ^^^^ Should this be in opposite order??? 
		 *    1st - set in USCAL
		 *    2nd - set in new order?
		 */

		punq_loc = needunq(plot_file);  /* make unique names? */

		g_pentyp  = 1 ;
		if (plot_type == T_RAST) {
			if (C.RPXB > 0) bytes_line = C.PIXX / C.RPXB;
			else            bytes_line = 0;

			tot_bytes = bytes_line * C.PIXY;

			pixels = malloc((unsigned)tot_bytes);
			clear_pix();
			hwline_ok = F ;
			soft_line = F ;
			soft_area = T ;		/* alway uses soft areas */
			soft_pen  = T ;
			hwpen_ok  = F ;
			}

		else if (plot_type == T_VECT) {
			/*set up drawing mode program entries, default:  DRAW*/
			C.DRWST = C.DRAW;   /* default mode saved */
			if (C.DRWCL == NOENT) C.DRWCL = C.DRAW;
			if (C.DRWCM == NOENT) C.DRWCM = C.DRAW;

			hwline_ok = (C.LINE != NOENT) ;
			soft_area = (C.AREA == NOENT) ;
			hwpen_ok  = (C.PEN  != NOENT) ;
			soft_pen  = F ;
			}

		/*
		else if (plot_type == T_SAVE) {
			}
		*/

		/* NOTE make sure that BFSZ is ok */
		(void) alu_mkb(MKB_OBUF, C.BFSZ) ;  /* make the output buff */
		(void) alu_mkb(MKB_IBUF, C.INSZ) ;  /* input buffer size */

		alu_givl(&g_ivar1, &g_ivar0, &g_fvar1, &g_fvar0) ;

		something = F;
#ifdef DO_STATS
		save_stats() ;		/* Save some stats */
#endif
		g_devset = T ;         /* device is now setup */
		}

	/* oops, error on setup */
	else {
		plot_type = T_NULL;  /* everything to NULL till fixed */
		g_devset = T;
		}

	return (ierr);
	}


/* date: 23 aug 84 (mje)
 * "pdevUnset" : undo some of the things done by 'setpdev'. mostly release 
 *	some of the buffer space used
 */
#ifdef ANSI_FUNC

static 
pdevUnset (void)
#else

static
pdevUnset()
#endif
	{

	(void) alu_mkb(MKB_OBUF, FREE_IT) ;
	(void) alu_mkb(MKB_IBUF, FREE_IT) ;

	if (plot_type == T_RAST && pixels) {
		free(pixels);
		pixels = 0;
		}

	g_devset = F;
	}
