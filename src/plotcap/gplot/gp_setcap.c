/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 03 may 88 (mje-stanford)
 *
 * "gp_setcap": Given that pgetent() has been called (we have a device
 *	definition) get all the parameters and get gplot setup....
 *
 * written:  Michael Eldredge (apr 84)
 * modified: Michael Eldredge -- Stanford (may 88)
 *	Upped the MAXIDATA and MAXPROG sizes and added a check
 *	after the call to pgetprg() to PANIC if the sizes have been
 *	exceeded.
 */



#include "auxfns.h"
#include "al_alu18.h"
#include "gp_mis12.h"
#include "gp_def.h"
#include "gp_com.h"

/* set in gp_caps.h */
static	int stabsiz, stabent ;

#include "gp_caps.h"


#ifdef ANSI_FUNC

int 
gp_setcap (void)
#else

int
gp_setcap()
#endif
	{

	int   tcnt ;    /* number of types found in entry */
	float val;
	int   ival, n;
	int   nbytes ;
#ifdef DO_STATS
	int	 istat1, istat2 ;
#endif

	defrot("gp_setcap");

	/* first find out what type of device we are setting caps for */
	tcnt = 0;
	tcp = Tcap;
	while (tcp->ptyp >= 0) {
		if (pgetflag(tcp->nam))  {   /* got this type ?? */
			plot_type = tcp->ptyp;
			tcnt++;              /* remember how many types given*/
			}
		tcp++;
		}

	/* special case: if no device given but NULL was, then we can
	 *  continue checking this entry.  (Some enties may only be LIKEd to,
	 *   and the entry with the LIKE will have a type, but (mkpcap for
	 *    example will complain the the LIKEd entry has no type 
	 */
	if (tcnt == 0 && pgetflag("NULL")) { plot_type = T_NULL; tcnt = 1; }

	if (tcnt != 1) {   /* can only be one type of device */
#ifdef DEBUG
		if (tcnt == 0)
			gperr("must have one of: VECT, RAST, SAVE, DEBUG");
		if (tcnt >  1)
			gperr("can only have one of: VECT, RAST, SAVE, DEBUG");
#else
		if (tcnt == 0) gperr("must have one of: VECT, RAST, SAVE");
		if (tcnt >  1) gperr("can only have one of: VECT, RAST, SAVE");
#endif
		/* error already, just get out of here */
		return(-1);
		}

	/* get boolean caps */
	bcp = Bcap;
	while (bcp->ptyp >= 0) {
		if ((bcp->ptyp & plot_type) || bcp->ptyp == T_ANY) {
			*(bcp->loc) = pgetflag( bcp->nam );
			}
		bcp++ ;
		}

	/* now integer types */
	icp = Icap;
	while (icp->ptyp >= 0) {
		if ((icp->ptyp & plot_type) || icp->ptyp == T_ANY) {
			if (pgetnum(icp->nam,&val) < 0) *(icp->loc) = icp->def;
			else  *(icp->loc) = (int) (val + .00001);
			}
		icp++ ;
		}

	/* now float types */
	fcp = Fcap;
	while (fcp->ptyp >= 0) {
		if ((fcp->ptyp & plot_type) || fcp->ptyp == T_ANY) {
			if (pgetnum(fcp->nam,&val) < 0) *(fcp->loc) = fcp->def;
			else  *(fcp->loc) = val;
			}
		fcp++ ;
		}

	/* string type caps */
	scp = Scap;
	while (scp->ptyp >= 0) {
		if ((scp->ptyp & plot_type) || scp->ptyp == T_ANY) {
			if ((n=pgetstr(scp->nam,scp->loc)) >0) scp->loc[n] = 0;
			else strcpy(scp->loc, scp->def);
			}
		scp++ ;
		}

	/* programs and subroutines */
	g_poff = 0;
	g_ioff = 0;

	/* allocate the symbol table for parsing the ``programs'' */
	if (stab_mk(stabsiz, stabent) < 0) {
		gperr("PANIC: Can't allocate a symbol table! No room.") ;
		}


	/* program type caps */
	pcp = Pcap;
	while (pcp->ptyp >= 0) {
		if (pcp->ptyp & plot_type) {
			ival = pgetprg(pcp->nam, g_prog, &g_poff, 
							g_idat, &g_ioff) ;
			if (ival > 0) *pcp->loc = ival;
			else         *(pcp->loc) = pcp->def;
			}
		pcp++;
		}
	if (g_poff > MAXPROG) {
		gperr2("PANIC: plotcap program too big! MAXPROG=%d",g_poff) ;
		gperr2("       Talk to the maintainer; MAXPROG>%d",MAXPROG);
		return -1 ;
		}
	if (g_ioff > MAXIDAT) {
		gperr2("PANIC: plotcap character data too big! MAXIDAT=%d",
								g_ioff) ;
		gperr2("       Talk to the maintainer; MAXIDAT>%d",MAXIDAT);
		return -1 ;
		}

#ifdef DO_STATS
	gp_stats.st_type = plot_type ;
	gp_stats.st_psiz = g_poff ;
	gp_stats.st_dsiz = g_ioff ;
	gp_stats.st_stmx = stabsiz ;
	gp_stats.st_semx = stabent ;
	if (stab_iq(&istat1, &istat2) >= 0) {
		gp_stats.st_stsz = istat1 ;
		gp_stats.st_sesz = istat2 ;
		}
#endif

	/* More just in-casing... */
	if (C.RPXB < 0 || C.RPXB > 8) C.RPXB = 0;
	/*			   ^ Fix this. Variable */

	if (plot_type == T_RAST) {
		if (C.RPXB > 0) nbytes = C.PIXX / C.RPXB; /* bytes per line */
		else            nbytes = 0;

		/* Get a good output buffer size */
		C.BFSZ = nbytes ;  /* output buffer size */
		}

	if (plot_type == T_VECT) {
		C.BFSZ = C.BFSZ ;
		}
		
	return(0);   /* all was ok, we gone. */
	}/* of routine */
