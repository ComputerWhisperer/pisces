/*----------------------------------------------------------------------
**  Copyright 1986 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* date: 26 nov 85 (dredge@su-fuji)
 *
 * "dis18" : Disassemble our little "programs".
 * written: Michael Eldredge (jan 85)
 * mod # 1: MJE (nov 85)	Convert from dis12 to dis18.
 */

#include "auxfns.h"
#include <stdio.h>
#include "pg_lex.h"
#include "al_alu18.h"
#include "al_dis18.h"


char *stab_gn() ;

static void prdat(FILE *fp, int l, int *p, int cnt);

/* Disassemble a program sending output to FP */
int dis18(FILE *fp, int prog[], int poff, char idat[])
	{

	int	plc = 0 ;
	int	njss ;
	int	fsyms ;
	char   *label ;
	char   *stab_gn() ;

	/* start dis18 */
	prdat(fp, plc, prog, 1 ) ;

	njss = prog[ plc++ ] ;
	fsyms = njss & STAB_FUNCS ;
	njss  = njss & 0x0ff ;

	fprintf(fp,"*\t[MaxJss %d, ", njss ) ;
	fprintf(fp,"Funcs Stab %c, " , (fsyms? 'T': 'F') ) ;
	fprintf(fp,"]\n") ;

	/* print out the jss jump table */
	while (njss--) {
		prdat(fp, plc , &prog[plc] , 1) ;
		fprintf(fp, "\t(%d)\n" , prog[plc++] ) ;
		}

	/* Loop through each routine */
	while (plc < poff) {
		label = (char *)0 ;		/* no label */

		if (fsyms)  label = stab_gn( prog[plc++] ) ;

		if (label) {
			prdat(fp, 0 , prog, 0 ) ;
			fprintf(fp,"%s:\n" , label ) ;
			}

		while (dis18i(fp, prog, &plc, idat)) {
			if (plc > poff ) break ;
			}
		}
	}


/* "dis18i" : Disassemble one instuction taking as may words as needed from
 *	prog.
 */
int dis18i(FILE *fp, int prog[], int *plc, char idat[])
	{

	int	nwd, ir, wr, mr, rr, xr, xxr, it ;
	int	n = *plc ;
	struct	_strint *lp ;
	Bool	flt_type = F ;

	char *is = "???" , *ws = "?" , *ms = "?", *rs = "r?";
	char *fs = "?" ;

	char	junk[50] ;		/* TRASH */
	/* start dis18i */

	nwd= prog[ (*plc)++ ] ;
	wr = gtw(nwd) ;
	mr = gtm(nwd) ;
	rr = gtr(nwd) ;
	ir = gti(nwd) ;
	it = irtype(ir) ;

	/* floating point instruction??? */
	if (it == IT_FLT) flt_type = T ;
	else	switch (ir) {
		case FLOD:  case FSTR:  case FMVO:
			flt_type = T ;
			break ;
			}


	/* print out plc and instructions in hex */
	prdat(fp, n, &prog[ n ] , irwds(ir, mr) ) ;

	if (gtc(nwd)) {		/* commented ir? */
		fprintf(fp,"*") ;
		}

	/* Find instruction name .... */
	for (lp = i18list ; *lp->sv ; lp++) {	/* Instructions */
		if (lp->iv == ir) { is = lp->sv ; break ; }
		}

	switch (wr) {
	case WA:  ws = "a" ; break;
	case WB:  ws = "b" ; break;
		}

	switch (ir) {
	/* I/O instructions .... */
	case MVO:
	case MVI:
	case FMVO:
		if (mr & ASC) {
			fs = &idat[ prog[ (*plc)++ ] ] ;
			}
		else {
			switch (gtiof(rr)) {	/* GeT IO Format */
			case RAWF:  fs = "%R" ; break ;
			case BYTE:  fs = "%B" ; break ;
			case WORD:  fs = "%W" ; break ;
			case LONG:  fs = "%L" ; break ;
				}/*of switch on form of rr */
			}
		
		rs = (char *)0 ;
		if (mr & IIO) rs = r18list[gtior(rr)].sv ;

		if (!rs) fprintf(fp,"\t%s.%s %s\n"     , is, ws, fs);
		else     fprintf(fp,"\t%s.%s %s,@%s\n" , is, ws, fs, rs);
		break ;

	/* Unary instructions ... */
	case JSS:
		switch (wr) {			/* Different for JSS */
		case WA:  ws = "@" ; break;
		case WB:  ws = "*" ; break;
			}

		ir = prog[ rr ] ;		/* jss tab entry */

		fs = "????" ;
		if (ir > 0) fs = stab_gn( prog[ir -1] ) ;
		else        fs = stab_gn( -ir ) ;

		fprintf(fp,"\t%s.%s %s\t(j=%d e=%d)\n" , is, ws, fs, rr, ir) ;
		break ;
	case NEG:
	case FNEG:
		fprintf(fp,"\t%s.%s\n" , is, ws) ; 
		break ;
	case END:
		fprintf(fp,"\t%s\n" , is) ; 
		break ;

	case ITR:			/* alu intrinsics */
		for (xxr = -rr, lp = itr18list; *lp->sv; lp++) 
			if (lp->iv == xxr) break ;

		if (*lp->sv) rs = lp->sv ;
		fprintf(fp,"\t%s   %s\t(#%d)\n", is, rs, rr) ;
		break ;

	/* Three word instructions ... */
	case CPO:
	case AST:
		fs = "s" ;
		if (mr == DAT) {
			rr = prog[ (*plc)++ ] ;
			fs = "l" ;
			}

		xr = prog[ (*plc)++ ] ;
		fprintf(fp,"\t%s.%s #%d,@%d" ,is,fs,rr,xr) ;

		if (ir == CPO) {
			fprintf(fp,"\t\"") ;
			prstr(fp, &idat[xr], rr, 32);	/* echo the str */
			fprintf(fp,"\"\n");
			}
		else fprintf(fp,"\n") ;
		break ;

	case BCC:
		rs = (rr >= 0 && rr <= 7) ? cc18lst[rr].sv : "??" ;
		ws = (wr == WB) ? "l" : "U" ;
		xr = prog[ (*plc)++ ] ;
		fprintf(fp,"\t%s%s.%s %d\n" , is, rs, ws, xr) ;
		break ;


	default:

#define T_REG -3
#define T_VAR -4
		switch (mr) {
		case REG:   
			rs = 0 ;
			for (lp = r18list; !rs && *lp->sv; lp++)
				if (lp->iv == rr) rs = lp->sv ;
				
			if (! rs ) { sprintf(junk,"?R%d",rr); rs = junk ;}

			fprintf(fp,"\t%s.%s %s\n" , is, ws, rs);  
			break ;
		case VAR:
			rs = 0 ;
			lp = (flt_type)? fv18list: iv18list ;
			for ( ; !rs && *lp->sv; lp++)
				if (lp->iv == rr) rs = lp->sv ;

			if (! rs ) { sprintf(junk,"?V%d",rr); rs = junk ;}

			fprintf(fp,"\t%s.%s %s\n"  , is, ws, rs) ;
			break ;
		case IND:
			fprintf(fp,"\t%s.%s @%s\n", is, ws, rs);  
			break ;
		case DAT:
			xr = prog[ (*plc)++ ] ;
			if (!flt_type)
		  	  fprintf(fp,"\t%s.%s #%d\t(0%o)\n", is,ws,xr,xr); 
			else
			  fprintf(fp,"\t%s.%s #%g\n", is,ws, F_I(xr) ); 
			break ;
		default:
			fprintf(fp,"\t?:%s.%s (%s)%s %d\n", is,ws,ms,rs,xr); 
			break;
			}/*of switch on mr */
		break ;
		}/*of switch on ir */

	return( ir != END) ;
	}


#define CNTRL   0140
#define UNCNTRL 0100
#define DEL    '\177'
#define BIG	0200
#define FULL	0377	/* avoid sign extention */

/* "prstr" : print the given string to the given file */
int prstr(FILE *fp, char *str, int cnt, int lim)
	{
	int c ;
	int  n = 0 ;				/* TRASH */

	if (lim > 0 && cnt > lim) {		/* TRASH */
		cnt = lim ;			/* TRASH */
		n   = 1  ;			/* TRASH */
		}				/* TRASH */

	while (cnt-- > 0){
		c = *str++ ;

		/* Standard control characters */
		if (member("\\^\n\t\f\r\b\033", c)) {
			putc('\\', fp) ;
			switch (c) {
			case '\\':	/* Themselves, but need the \ first */
			case '^':
				break ;
			case '\n': c = 'n'; break ;
			case '\t': c = 't'; break ;
			case '\f': c = 'f'; break ;
			case '\r': c = 'r'; break ;
			case '\b': c = 'b'; break ;
			case '\033': c='E'; break ;	/* ESC */
				}
			}

		else if ( c & BIG ) {			/* 8th bit set */
			fprintf(fp,"\\%03o", c & FULL) ;
			continue ;
			}

		else if (! (c >= ' ' && c <= '~')) {	/* unprintable */
			c = ((c == DEL) ? '?' : (c | UNCNTRL)) ;
			putc('^', fp) ;
			}
		putc(c, fp) ;
		}

	if (n) fprintf(fp, "...") ;		/* TRASH */
	}

/* "prdat" : print the plc and data for an instruction */
static void
prdat(FILE *fp, int l, int *p, int cnt)
	{
	switch (cnt) {
	case 0: fprintf(fp,"                 " ) ;                    break ;
	case 1: fprintf(fp,"%04d %04x        ", l, p[0] ) ;           break ;
	case 2: fprintf(fp,"%04d %04x%04x    ", l, p[0],p[1] ) ;      break ;
	case 3: fprintf(fp,"%04d %04x%04x%04x", l, p[0],p[1],p[2] ) ; break ;
		}
	}
