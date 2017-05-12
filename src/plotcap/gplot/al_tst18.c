/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 27 jul 87 (dredge -- Stanford)
 * "tst18" : get some info about an alu18() prog, run through a program 
 * Original: Michael Eldredge (mar 85)
 * Modified: MJE (nov 85)	Convert from tst12 to tst18.
 * Modified: MJE (jul 87) 'entry' is reserved in VMS/C. Changed to 'ent'
 */

#include <stdio.h>
#include "al_alu18.h"
/* #include "al_dis18.h" */

#define TTYP_DEF  0
#define TTYP_END  1
#define TTYP_JSS1 2
#define TTYP_JSS  3
#define TTYP_OUT  4
#define TTYP_AST  5
#define TTYP_KEPT 6

static	int	jssret[MAXRET] ;
static	int	curret ;


#ifdef ANSI_FUNC

int 
tst18 (int prog[], p_ent ent, char idat[])
#else

int
tst18(prog, ent, idat)
	int	prog[] ;
	p_ent	ent ;
	char	idat[] ;
#endif
	{
	int	plc , nout = 0 ;
	int	ntyp, nval ;
	int	n , done = 0 ;
	int	z[3]   , s[3] , a[2] ;
	int	iz = 0 , is = 0 , jmark = -1 ;
	int	ast_loc = -1 ;		/* Where is an assertion? */

	plc = ent ;

	for (n = 0 ; n < 3 ; n++) { z[n] = s[n] = 0 ; }

	/* Loop through each routine */
	while (!done) {
		n = tst18i(prog, plc , idat, &ntyp, &nval) ;

		switch (ntyp) {
		case TTYP_OUT:
			nout += nval ;
			z[iz] += nval ;
			s[is] += nval ;
			break ;
		case TTYP_JSS:
			is++ ;
			jmark = curret ;
			/*FALL THROUGH */
		case TTYP_JSS1:
			jssret[ curret++ ] = plc ;
			plc = prog[ nval ] ;
			break ;

		case TTYP_END:
			if (curret) plc = jssret[ --curret ] ;
			else        done = 1 ;

			if (curret == jmark) {
				jmark = -1 ;
				is++ ;
				}
			break ;

		case TTYP_AST:
			iz++ ;
			ast_loc = plc ;		/* remember where assert */
			break; 

		case TTYP_KEPT:		/* end of keep section */
			iz++ ;
			/* prog[ plc ] = NOP ;		/* NOP now */
			break ;

			}

		plc += n ;
		}

	if (is == 0) {		/* never had a JSS.* */
		s[1] = s[0] ;
		s[0] = 0 ;
		}

	a[0] = z[1] ;
	a[1] = (s[0] != 0 ? z[0]+z[1]-s[0] : 0 ) ;

	if (ast_loc >= 0) {
		prog[   ast_loc ] = (AST | WB | DAT) ;	/* To set WB */
		prog[ ++ast_loc ] = a[0] ;
		prog[ ++ast_loc ] = a[1] ;
		}

	/*
	printf("tst18: nout %d\n" , nout ) ;
	printf("\t Z %3d :: %3d %3d %3d\n", z[0]+z[1]+z[2],z[0],z[1],z[2]);
	printf("\t S %3d :: %3d %3d %3d\n", s[0]+s[1]+s[2],s[0],s[1],s[2]);
	printf("\t Assert : %3d %3d\n", a[0], a[1] ) ;
	*/

	return(nout) ;
	}


/* "tst18i" : */
#ifdef ANSI_FUNC

int 
tst18i (int prog[], int plc, char idat[], int *ityp, int *ival)
#else

int
tst18i(prog, plc , idat, ityp, ival)
	int	prog[], plc, *ityp, *ival ; 
	char	idat[] ;
#endif
	{

	int	ir, wr, mr, rr ;
	int	nwords ;
	char	*cp ;

	/* --- start ---- */
	*ityp = TTYP_DEF ;	/* default type for now */

	ir = prog[ plc++ ] ;
	wr = gtw(ir) ;
	mr = gtm(ir) ;
	rr = gtr(ir) ;
	ir = gti(ir) ;

	nwords = irwds(ir, mr) ;	/* How many words does it take */

	/* IRs that cause side effects or do I/O */
	switch (ir) {
	case JSS:
		if (wr == JSS_STAR) *ityp = TTYP_JSS ;
		else                *ityp = TTYP_JSS1 ;
		*ival = rr ;	/* Into jss-table */
		break ;

	case MVO:	
		*ityp = TTYP_OUT ;
		if (mr & IIO)  *ival = 0 ;

		else if (! (mr & ASC)) {		/* Binary form */
			if (! (*ival = ftob(gtiof(rr)) )) *ival = RAWI_BYTES ;
			}

		else {
			cp = &idat[ prog[plc] ] ;
			if (*++cp >= '0' && *cp <= '9') *ival = atoi(cp) ;
			else 				*ival = 6 ; /*YUCK!*/
			}
		break ;

	case CPO:
		*ityp = TTYP_OUT ;
		*ival = (rr ? rr : prog[plc] ) ;
		break ;

	case END:
		*ityp = TTYP_END ;
		break ;

	case AST:		 /* Start of assert/keep section */
		if (wr == WA) *ityp = TTYP_AST ;
		/* Only change if wr == WA; else already done */
		break ;

	case KPE:
		*ityp = TTYP_KEPT ;	/* Endo of keep-section */
		break ;

		}

	return( nwords ) ;
	}


/* "irwds" : return how many words a given instruction will take */
#ifdef ANSI_FUNC

int 
irwds (int ir, int mr)
#else

int
irwds(ir, mr)
	int	ir, mr ;
#endif
	{

	int   nwds  = 1;		/* at least one word */
	int   ityp  = irtype(ir) ;

	/* ---- start prdat ---- */
	switch(ityp) {
	case IT_NORM:
		if (mr == DAT) nwds++ ;
		break ;

	case IT_IO:
		if (mr & ASC)  nwds++ ;
		break ;
	case IT_MISC:
		switch (ir) {
		case CPO:
		case AST:
			nwds++ ;
			if (mr == DAT) nwds++ ;
			break ;
		default:
			if (mr == DAT) nwds++ ;
			break ;

			}/*switch on ir for MISC */

		}/*switch on ir type */

	return(nwds) ;
	}
