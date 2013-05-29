/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* date: 17 mar 87 (mje)
 * "sv_file" : open the named file and see if it is a save file.  If so
 *	read the saved plot commands and re-issue them.
 *
 * NOTE: We will recognize several save forms here, either with special 
 *	opens or by checking the first several bytes of the file for some
 *	distinquishing characteristic.
 * written:  Michael Eldredge (jun 85)
 * modified: Michael Eldredge (dec 86) Use new gpopen() stuff.
 * modified: Michael Eldredge (feb 87) #ifdef out the DOT_GP_HACK stuff.
 * modified: Michael Eldredge (mar 87) added segment delete.
 */

/* define to force "*.gp" files to be opened as gplot-save files */
/* #define DOT_GP_HACK 1 */


#include "gplot.h"

int
sv_file(name)
	char *name;
	{

	int lu, iret = 0;
	float xval, yval;
	int   cmd, sub;
	int	on = 1 ;


	/* GPLOT: see if it looks like a "gplot" save file */
	if ( (lu = gpopen(name, GPO_RD|GPO_HD)) >= 0) {
		while (gpread(lu, &cmd, &sub, &xval, &yval) > 0) {
			if (cmd == G_PEND) continue ;

			if (seg_on(cmd, sub)) {
				logit_gp(cmd, sub, xval, yval) ;
				gplot2(cmd, sub, xval, yval);
				}
			}

		gpclose(lu);
		}

#ifdef DOT_GP_HACK
	/* HACK-GPLOT: NOTE: Fix this, but..... 
	 *  since some stuff (ie: gplot itself) doesn't use gpopen which
	 *  gives that first record - if it fails then let's look at the 
	 *  extent of the filename.  If it is ".gp" then just assume it is
	 *  a gplot file. (mje: 15jul85).
	 */
	else if ( (lu = HACKopen(name)) >= 0) {
		while (gpread(lu, &cmd, &sub, &xval, &yval) > 0) {
			if (cmd == G_PEND) continue ;

			if (seg_on(cmd, sub)) {
				logit_gp(cmd, sub, xval, yval) ;
				gplot2(cmd, sub, xval, yval);
				}
			}

		gpclose(lu);
		}

	/* End of mondo hack */
#endif /* DOT_GP_HACK*/


	/* TPLOT: see if it looks like a "tplot" save file */
	else if ( (lu = tpopen(name, 0 )) >= 0) {
		while (tpread(lu, &xval, &yval, &cmd) > 0) {
			if (cmd == G_PEND) continue ;
			
			sub = 0;
			if (cmd < 0) {
				sub = -cmd;
				cmd = G_LINE;
				}

			if (seg_on(cmd, sub)) {
				logit_gp(cmd, sub, xval, yval) ;
				gplot2(cmd, sub, xval, yval);
				}
			}

		close(lu);
		}

	/* ERROR: nobody seemed to know the file type */
	else iret = -1 ;	/* couldn't figure out what kind it is */

	return(iret);
	}


#ifdef DOT_GP_HACK
/* MORE OF HACK on GPLOT FILE OPENS */
static int
HACKopen(name)
	char	*name ;
	{

	int	 lu = -1, l ;

	if ((l = strlen(name)) > 3) {
		if (strcmp(&name[l-3], ".gp") == 0)	/* ok, name fits */
			lu = open(name, 0) ;
		}

	return(lu) ;
	}
#endif /*DOT_GP_HACK*/
