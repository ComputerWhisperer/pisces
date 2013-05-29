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
 * Tue Sep 19 16:31:01 PDT 1989 (dredge--stanford)
 *
 * "pg_erset" : record an error for later printing.....
 *
 * note: for now we just print it.
 */

#include <stdio.h>
#include "auxfns.h"
#include "pg_def.h"
#define pr fprintf

#define CLEAN_SZ	50	/* how much of a wierd buffer to print */
static void	clean_buf() ;

int
pg_erset(rot, eno, sval, ival, syseno)
	char	*rot ;		/* routine with the error */
	int	 eno ;		/* pg - error number */
	char	*sval ;		/* possible string value */
	int	 ival ;		/* possible integer value */
	int	 syseno ;	/* system error number (!= 0) */
	{

	char	tmp[NAM_SIZE] ;
	p.pgerr1 = -1 ;		/* had an error */
	p.pgerr2 = eno ;	/*  ... and it was... */

	pr(stderr, "*** [%s] ", rot ) ;
	if (p.linnum >= 0) pr(stderr, "(%s::%d)", p.capfil, p.linnum) ;
	pr(stderr,": ") ;

	switch (eno) {
	case E_CFIL:
		pr(stderr,"opening cap file (dev %s)", sval);
		break;
	case E_CREW:
		pr(stderr,"rewinding cap file (dev %s)", sval) ;
		break ;
	case E_UEOF:
		pr(stderr,"unexpected EOF") ;
		break ;
	case E_SYMB:
		pr(stderr,"Not a symbol (dev %s), tok %d", sval, ival) ;
		break ;
	case E_NEQU:
		pr(stderr,"Not an equal sign (dev %s), tok %d", sval, ival) ;
		break ;
	case E_NTOK:
		pr(stderr,"No token or bad token `%s'", sval) ;
		break ;
	case E_NEXT:
		pr(stderr,"Next file param not a string `%s'", sval);
		break ;
	case E_UCMD:
		pr(stderr,"Unknown special command `%s'", sval) ;
		break ;
	case E_SCMD:
		pr(stderr,"Command is not a word `%s'", sval) ;
		break ;
	case E_NODV:
		pr(stderr,"Device `%s' not found", sval) ;
		break ;
	case E_UTOK:
		pr(stderr,"Unknown token in cap file `%s'", sval) ;
		break ;
	case E_USUB:
		pr(stderr,"Undeclared subroutine name `%s'", sval) ;
		break ;
	case E_PAS2:
		pr(stderr,"Error in pass2 of program entry `%s'",sval) ;
		break ;
	case E_LDED:
		pr(stderr,"Entry already loaded `%s'", sval) ;
		break ;
	case E_NSTB:
		pr(stderr,"No symbol ever allocated!!! PANIC!!") ;
		break ;
	case E_STSX:
		pr(stderr,"Out of symbol table entries! on `%s'", sval) ;
		break ;
	case E_STSY:
		pr(stderr,"Out of symbol table room! on `%s'", sval) ;
		break ;
	case E_UTMP:
		pr(stderr,"Temp label `%d$' never defined", ival) ;
		break ;
	case E_PSYN:
		pr(stderr,"Parser syntax error near `%s'", sval) ;
		break ;
	case E_NVAL:
		pr(stderr,"No value given for capability `%s'", sval) ;
		break ;
	case E_NUMB:
		pr(stderr,"Bad numeric format: `%s'", sval) ;
		break ;
	case E_UINTR:
		pr(stderr,"Unknown intrinsic function `%s'", sval);
		break ;
	case E_TMPLX:
		pr(stderr,"Too many temp labels at `%d$'", ival) ;
		break ;
	case E_TMPLD:
		pr(stderr,"Temp label `%d$' already defined", ival ) ;
		break ;
	case E_UOFMT:
		pr(stderr,"Illegal output format `%s'", sval) ;
		break ;
	case E_UIFMT:
		pr(stderr,"Illegal input format `%s'", sval) ;
		break ;
	case E_UFFMT:
		pr(stderr,"Illegal floating point format `%s'", sval) ;
		break ;
	case E_UOPER:
		pr(stderr,"Unknown operator `%s'", sval) ;
		break ;
	case E_EBIG:
		clean_buf(tmp,sval) ;
		pr(stderr,"Entry definition too big for buffer:\n `%s'",tmp) ;
		break ;
	case E_NBIG:
		clean_buf(tmp,sval) ;
		pr(stderr,"Name string too big for buffer:\n `%s'",tmp) ;
		break ;
	case E_OVR:
		clean_buf(tmp,sval) ;
		pr(stderr,"Buffer overflow parsing entries:\n `%s'",tmp) ;
		break ;

	default:
		pr(stderr,"Unknown error = %d, s=`%s', i=%d",eno,sval,ival) ;
		break ;
		}

	if (syseno != 0) {
#ifndef unix
		perror("\t") ;
#else
		pr(stderr," [%s]", sys_errlist[syseno] ) ;
#endif
		}

	pr(stderr,  "\n") ;

	return(E_ERR) ;
	}

static void
clean_buf(newbf,buf)
	char*	newbf ;
	char*	buf ;
	{

	int	sz = CLEAN_SZ - 3 ;	/* room for the "..." */

	for ( ; *buf && --sz >= 0; ++buf ) {
		if (!isprint(*buf)) *newbf++ = '~' ;
		else		    *newbf++ = *buf ;
		}
	strcpy(newbf,"...") ;
	}
