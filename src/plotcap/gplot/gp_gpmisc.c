/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* Thu Jul 28 14:49:30 PDT 1988 (dredge--stanford)
 * 
 * "gpmisc" : Misc. Gplot routines (with more parameters or string parms).
 *
 * written:  Michael Eldredge (apr 85)
 * modified: MJE (may 86) added the G_REVCO command return.
 * modified: MJE (dec 86) removed gpgets() call. Added more commands.
 *	added gpseti() to reset scaling etc....
 *	+ Combined gpgeti()/gpseti()/gpmisc() and added #defines
 *	to gplot.h for setpdev()/setpfil()/etc....
 * modified: MJE (feb 87)
 * modified: Michael Eldredge (mar 87) returns of clip values changed.
 *	+ return iv[] and fv[] counts in return code.
 *	+ fixed get/xform data
 *	+ added get/set/mul xform matrix.
 *	+ return something==T ?
 * modified: MJE (may 87) Fixed bug with G_SET of xform matrix. Was:
 *	all y-values were being returned, NOT SET!!
 * modified: MJE (jul 87) CLOC (sub==G_CHECK) returns 0 if CLOC capability
 *	is defined -1 otherwise.
 * modified: MJE - stanford (may 88) Fixed some returns to avoid core
 *	dumping when device==NULL
 * modified: MJE -- stanford (jul 88) CLIP returns now show if it was
 *	from PHS or LOG.
 */

#include "auxfns.h"
#include "gplot.h"
#include "al_alu18.h"
#include "gp_def.h"
#include "gp_com.h"

/* Make sure that gplot is set and ready to go... */
#define MustBeSet()	gplot2(G_NULL, 0, g_lastX, g_lastY)

/* hide the counts in the return code. */
#define CODE_BITS 4
#define MASK ((1<<CODE_BITS)-1)
#define CODE(NF,NI) ( ((NF)&MASK)<<CODE_BITS | ((NI)&MASK) )


/* "gpmisc": misc. gplot functions */
#ifdef ANSI_FUNC

int 
gpmisc (int cmd, int sub, int iv[], float fv[], char str[])
#else

int
gpmisc(cmd, sub, iv, fv, str)
	int	 cmd ;
	int	 sub ;
	int	 iv[] ;
	float	 fv[] ;
	char	 str[] ;
#endif
	{
	float	 x, y ;			/* scratch floats */
	float	 a ;
	int	 flg ;
	char*	 nam ;
	int	 ierr = 0 ;
	int	 ni = 0, nf = 0 ;	/* how many of each are returned */

	defrot("gpmisc") ;

	switch (cmd) {
	case G_CLOC:
		MustBeSet() ;
		if (sub != G_CHECK) {
			gplot2(G_CLOC, iv[2], g_lastX, g_lastY) ;
			iv[0] = g_i1 ;
			iv[1] = g_j1 ;
			iv[2] = g_sub;

			/* Convert pixels back to inches (or what ever) */
			if (C.USCAL != 0.0 && C.PUNX!=0.0 && C.PUNY!=0.0) {
			    x = (float)(g_i1 - C.ORGX) / (C.USCAL * C.PUNX) ;
			    y = (float)(g_j1 - C.ORGY) / (C.USCAL * C.PUNY) ;
			    }
			else x = y = 0.0 ;

			uform ( &fv[0], &fv[1], x, y ) ;
			ni = 3 ; nf = 2 ;
			}

		else {	/* G_CHECK to see if we can */
			ni = nf = 0 ;	/* 0 means ok (CLOC will work) */
			if (C.CLOC == ALU_NOENT) ierr = -1 ;  /* No go */
			}
		break ;

	case G_PSIZE:
		MustBeSet() ;
		fv[0] = fv[1] = 1. ;
		fv[2] = fv[3] = 1. ;
		if (C.USCAL != 0.) {
			if (C.PUNX != 0) {
				a = C.USCAL * (float)C.PUNX ;
				fv[0] = (float)(C.PIXX - C.ORGX) / a ;
				fv[2] = 1.0 / a ;
				}
			if (C.PUNY != 0) {
				a = C.USCAL * (float)C.PUNY ;
				fv[1] = (float)(C.PIXY - C.ORGY) / a ;
				fv[3] = 1.0 / a ;
				}
			}
		/* why not? */
		iv[0] = C.PIXX ; iv[1] = C.PIXY ;
		iv[2] = C.PUNX ; iv[3] = C.PUNY ;
		ni = 4; nf = 4 ;
		break ;

	case G_CLIPL:
		MustBeSet() ;
		if (iv[0] == G_ONPHS || !g_logclp) {
			fv[0] = g_phsbox[0].x ;
			fv[1] = g_phsbox[0].y ;
			iv[0] = G_ONPHS ;
			}
		else {
			fv[0] = g_logbox[0].x ;
			fv[1] = g_logbox[0].y ;
			iv[0] = G_ONLOG ;
			}
		nf = 2 ;
		ni = 1 ;
		break ;

	case G_CLIPH:
		MustBeSet() ;
		if (iv[0] == G_ONPHS || !g_logclp) {
			fv[0] = g_phsbox[2].x ;
			fv[1] = g_phsbox[2].y ;
			iv[0] = G_ONPHS ;
			}
		else {
			fv[0] = g_logbox[2].x ;
			fv[1] = g_logbox[2].y ;
			iv[0] = G_ONLOG ;
			}
		nf = 2 ;
		ni = 1 ;
		break ;

	case G_REVCO:
		iv[0] = gp_revcode ;
		iv[1] = g_allset ;
		ni = 2 ;
		break ;

	case G_NULL:
		ni = nf = 0 ;	/* 0 will mean something=T */
		if (!something || !g_allset) ierr = -1 ;
		break ;

	case G_LINE:
		MustBeSet() ;
		iv[0] = g_lintyp ; 	/* current line style */
		iv[1] = (C.NLINE == NHUGE? 0: C.NLINE) ;
		ni = 2 ;
		break ;
	case G_PEN:
		MustBeSet() ;
		iv[0] = g_pentyp ; 	/* current pen style */
		iv[1] = (C.NPEN == NHUGE? 0: C.NPEN) ;
		ni = 2 ;
		break ;
	case G_FILS:
		MustBeSet() ;
		iv[0] = g_filtyp ; 	/* current fill style */
		iv[1] = (C.NFILS == NHUGE? 0: C.NFILS) ;
		ni = 2 ;
		break ;

	case G_ROTATE: case G_ANGLE:
	case G_SCALE:
	case G_TRANS:
#define X_TRS XFORD(XF_T, XF_R, XF_S)
		MustBeSet() ;

		if (sub != G_SET) {	/* not SET then GET */
			iv[0] = gp_unxfrm(X_TRS) ;	/* undo matrix */
			fv[0] = g_tranx ; fv[1] = g_trany ;
			fv[2] = g_rotax ; fv[3] = g_rotax ; fv[4] = g_angle ;
			fv[5] = g_scalx ; fv[6] = g_scaly ;
			ni = 1 ; nf = 7 ;
			}

		else {
			g_tranx = fv[0] ; g_trany = fv[1] ;
			g_rotax = fv[2] ; g_rotax = fv[3] ; g_angle = fv[4] ;
			g_scalx = fv[5] ; g_scaly = fv[6] ;
			/* iv[0] has the order returned from gp_unxfrm() */
			gp_reform(iv[0], X_NEW) ;	/* reset matrix */
			}
		break ;

	/* Play with the xformation matrix itself.
	 *  internally it looks like:
	 *	Sx   0  Tx	a b c
	 *	 0  Sy  Ty	d e f
	 *	 0   0   1
	 *  But most people prefer:
	 *	Sx   0   0	a d
	 *	 0  Sy   0	b e
	 *	Tx  Ty   1	c f
	 *  So send it out/get it back that way.
	 */
	case G_XMAT:
		MustBeSet() ;
		/* replace current matrix with that given */
		if (sub == G_SET) {
			g_XXcon = fv[0] ;  g_YXcon = fv[1] ;
			g_XYcon = fv[2] ;  g_YYcon = fv[3] ;
			g_Xcon =  fv[4] ;  g_Ycon  = fv[5] ;
			/*.........Had error here ^^^^^^^, order switched */
			}

		/* multiply given one in */
		else if (sub == G_MUL) {
			gp_reduct3(fv[0], fv[2], fv[3],
				   fv[1], fv[3], fv[5] ) ;
				/*   0  ,   0  ,   1   */
			}

		/* GET the matrix. */
		else {
			fv[0] = g_XXcon ;  fv[1] = g_YXcon ;
			fv[2] = g_XYcon ;  fv[3] = g_YYcon ;
			fv[4] = g_Xcon  ;  fv[5] = g_Ycon  ;
			nf = 6 ;
			}
		break ;

	case G_PDEV:	/* New Plot DEVICE (setpdev) */
		if (sub != G_GET) ierr = gp_pdev(str, G_BEGIN) ;
		break ;

	case G_PFIL:	/* New Plot file (setpfil) */
		if (g_filset) gp_setdn() ;	/* finish up prev */

		ierr = gp_pfil(str, G_BEGIN) ;
		break ;

	case G_ASAVE:
		/* close up prev asave file (if there is one) */
		if (lu_asav != -1) {
			(void)gpwrite(lu_asav, G_PEND, 0, 0.0, 0.0) ;
			gpclose(lu_asav) ; 	/* close prev */
			}

		flg = GPO_WR|GPO_HD ;	/* open for writing & add header */
		nam = str ;
		if (*nam == '+') { ++nam ; flg |= GPO_AP ; }

		if ((lu_asav = gpopen(nam, flg)) >= 0) asav_on = T ;
		else {
			asav_on = F ;
			lu_asav = -1 ;
			ierr    = - 1 ;
			}
		break ;

	default:
		gperr2("unknown gpmisc command %d", cmd)  ;
		ierr = -1 ;
		break ;
		}

	return (ierr < 0? ierr: CODE(nf,ni) ) ;
	}
