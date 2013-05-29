/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Mon Aug 15 11:43:00 PDT 1988 (dredge -- stanford)
 *
 * "gp_setup" : Once common values have been input (by 'gp_setcap' or
 *	by 'get_gp', create buffers, and related values, so that
 *	plotting can be done.
 *
 * written:  Michael Eldredge (may 84)
 * modified: Michael Edlredge (aug 88) In setdn, make sure plot was closed.
 * modified: MJE -- Stanford (aug 88) INIT will now querey the device
 *	for sizes.
 */

#include "auxfns.h"
#include "al_alu18.h"
#include "gp_def.h"
#include "gp_com.h"
#include "gplot.h"

int
gp_setup()
	{

	char *defdev ;                  /* name of default device */
	char *getenv();
	float	val ;

	defrot("gp_setup");

	/* get the default plot device.  This could be one of:
	 *   1. DEF_DEV in the environment.
	 *   2. ``TERM'' variable in the environment.
	 *   3. A device called ``default'' in the plotcap file.
	 */
	if ( !(defdev = getenv(DEF_DEV)) && !(defdev = getenv("TERM")) ) 
		defdev = "default";

	/* no plot device yet, try to get one that we can use */
	if (!g_devset && gp_pdev(defdev, G_BEGIN) <0) {  /* didn't find one */
		plot_type = T_NULL;  /* oops, error */
		gperr2("can't find plot device '%s'", defdev);
		g_allset = T;   /* not really, but let everyone think so */
		return(-1);
		}

	/* no output file, see if there is a default file to use */
	if (!g_filset && gp_pfil("", G_BEGIN) <0) {   /* didn't get the file */
		plot_type = T_NULL;  /* oops, error */
		gperr("can't open plot output file");
		g_allset = T;   /* not really, but let everyone think so */
		return(-1);
		}

	/* setup some gplot variables and get everything setup  */
	if (plot_type != T_NULL) {   /* finish setting up if no errors */

		/* adjust this to be the maximum pixel value */
		C.PIXX-- ;
		C.PIXY-- ;

		/* Check values to make sure nothing real bogus */
		if (C.PIXX <= 0) C.PIXX = 1;
		if (C.PIXY <= 0) C.PIXY = 1;
		if (C.PUNX <= 0) C.PUNX = 1;
		if (C.PUNY <= 0) C.PUNY = 1;

		/* --- Prepare the default sizes: --- */
		g_i1 = C.PIXX ;		/* pass to docmd() */
		g_j1 = C.PIXY ;

		/* --- Init --- */
		docmd(C.INIT) ;

		/* --- Different size? --- */
		if (g_i1 > 0) C.PIXX = g_i1 ;	/* device give actual sizes? */
		if (g_j1 > 0) C.PIXY = g_j1 ;

		/* --- Did the user give us some preferred sized? --- */
		if (g_xshint > 0.0 && g_yshint > 0.0) {
			int	px = g_xshint * C.PUNX ;
			int	py = g_yshint * C.PUNY ;
			if (px < C.PIXX && px > 0) C.PIXX = px ;
			if (py < C.PIXY && py > 0) C.PIXY = py ;
			}

		/* --- Misc setup --- */
		sv_igpo  = C.IGPO ;

		if (C.CLPLX < 0.0) C.CLPLX = 0.0;   /* setup clipping bounds */
		if (C.CLPLY < 0.0) C.CLPLY = 0.0;
		val = (float)(C.PIXX - C.ORGX) / (float)C.PUNX ;
		if (C.CLPHX > val || C.CLPHX < 0.0) C.CLPHX = val ;
		val = (float)(C.PIXY - C.ORGY) / (float)C.PUNY ;
		if (C.CLPHY > val || C.CLPHY < 0.0) C.CLPHY = val ;

		/* set original clipping values */
		AdjustClip(G_CLIPL, G_RESET, 0., 0. ) ;
		AdjustClip(G_CLIPH, G_RESET, 0., 0. ) ;
		}

	something = F;
	g_allset    = T;        /* all set */

	return(0);            /* did it, all setup, no problems! */
	}



/* "gp_setdn" : Unsetup everything, close files, free buffers and
 *	reset flags.
 *	Basically, reset to the pre-init and setup state.
 *
 * date: 30 jan 84 (mje)
 */

gp_setdn()
	{


	/* Make sure things are back to default values */
	if (g_lintyp != 1 && hwline_ok) {
		g_sub = 1 ;
		docmd(C.LINE) ;
		}
	if (g_pentyp != 1 && hwpen_ok) {
		g_sub = 1 ;
		docmd(C.PEN) ;
		}

	/* If there still is something, then pend wasn't called... */
	if (something) {
		docmd(C.PLCL) ;		/* close the plot */
		something = 0 ;
		}

	/* if we were doing something, finish up */
	if (plot_type != T_NULL) {
		docmd(C.PEND) ;		/* ending sequence and ... */
		docmd(ALU_POST) ;	/*  post the last of it */
		}

	(void)gp_pfil("", G_STOP) ;		/* close the plot file */

	if (lu_asav != -1) {	/* just like in gplot2():C_ASAVE,G_STOP */
		gpclose(lu_asav) ;
		lu_asav = -1 ;
		asav_on = F ;
		}
	
	g_allset = F;
	}
