%> header
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
 * Wed Sep 20 12:10:58 PDT 1989 (dredge--stanford)
 *
 * "pgetprg" : Plotcap ``program'' compiler.
 *
 * notes:
 *   + Although the functions pgetent(), pgetflag(), pgetstr(), and pgetnum()
 *	are self contained - not related to gplot() or aluXX() - this function
 *	pgetprg() is written like those routines but does have specific 
 *	understanding of the aluXX() stuff.  Most notably it has the aluXX
 *	common declaration included and will set stuff right into it.  As
 *	such is is really a member of the al_XXXX.[ch] routines AND a member
 *	of the pg_XXXXXX.[ch] routines.
 *
 * written:  Michael Eldredge (dredge@su-fuji) aug 84 
 * modified: Michael Eldredge (nov 85) to use alu12()
 * modified: Michael Eldredge (may 86) to use dis18(), the new plotcap(V3)
 *	format files.  We still, however, use alu12() -slightly modified.
 * modified: Michael Eldredge (may 86) accepts the floating point vars/vals.
 *	Note that this is very simple/trivial support, but....
 * modified: Michael Eldredge (dec 86) Change 'goto Label' from == 2$ to
 *	== $2.  Thus we can no have a goto statement like: "...; $2; ..."
 * modified: Michael Eldredge (dec 86) CPO #0 are not output, since rr=0
 *	in CPO means take next word as count.  Now GTOA="" is the noop
 *	it should be.
 * Modified: MJE (jul 87)
 *	'entry' is a reserved word for VMS/C.  Changed to 'ent'.
 *	al_com18.c may not get demand loaded by some loaders, so included it
 *	  here.
 * Modified: mje -- stanford (sep 89) fixed entok(() calling seq.
 */

#include "auxfns.h"
#include "pg_def.h"
#include "al_alu18.h"

/* Make sure the names,etc common is actually defined. */
#define COMMON
#include "al_dis18.h"
#undef COMMON

/* Temp label handler [ tlbtab() ] definitions. */
#define TL_RESET 0
#define TL_DEF	 1
#define TL_GET	 2
#define TL_CHK	 3
#define TLE_SET	 -1
#define TLE_OVER -2


/* ALU header word is MAXJSS and any of: STAB_LOCAL or STAB_FUNCS */
#define ALU_HEADER  (MAXJSS | STAB_FUNCS)

/* Local (to this file) copies of all this */
static	int	*pg_prog ;
static	int	*pg_poff ;
static	char	*pg_idat ;
static	int	*pg_ioff ;
static	int	jsscur  ; 
static	int	entcur  ;


#define ppv(V) 	pg_prog[ (*pg_poff)++ ] = (V)
#define piv(V)	pg_idat[ (*pg_ioff)++ ] = (V)

int
pgetprg(id, prog, poff, idat, ioff)
	char *id ;
	int  prog[], *poff ;
	char idat[]; 
	int  *ioff;
	{
	
	int  i , n, ent ;
	static int  jssloc ;	/* remember from last time the highest */
	char *cp ;
	char *stab_gn() ;

	/* start pgetprg */
	defrot("pgetprg") ;

	/* Get local copies of destinations, so all routines can access */
	pg_prog = prog ;
	pg_poff = poff ;
	pg_idat = idat ;
	pg_ioff = ioff ;


	/* If this is the first time in, then we need to set up the beginning
	 *  of the program with an id word and ``subroutine'' short jumps.
	 */
	if (*pg_poff <= 0) {		/* id is maxjss (size of jump tab) */
		*pg_poff = 0 ;
		ppv(ALU_HEADER) ;		/* put header word */	

		for (i=0; i < MAXJSS; i++) 
			ppv(NOJUMP) ;	/* no jumps for now */
		jsscur = JSS1 ;		/* no jss's yet */
		jssloc = jsscur ;
		}


	/* Build code for given ID */
	if ((ent = getprog(id, 'E')) < 0)  return(ent) ;


	/* --- resolve JSS's ---- */
	if (jsscur != jssloc) {
		for (i = jssloc ; i < jsscur ; i++) {
			if ((n = pg_prog[ i ]) <= 0) {	/* <0 :: UNDEF'd */
				n = getprog( (cp = stab_gn(-n)) , 'J') ;
				/* undeclared subr? */
				if (n < 0) (void)pg_er(E_USUB, cp, 0,0) ;
				/* else tst18..... */
				}
			}

		jssloc = jsscur ;
		}
	/* --- end of resolving JSS's ---- */

	/* --- Second pass to resolve AST's --- */
	i = tst18(pg_prog, ent, pg_idat) ;
	if (i < 0) (void)pg_er(E_PAS2, "", i, 0) ;	/* pass2 error */

	return (ent) ;
	}

static int
getprog(id, id_type)
	char *id , id_type ;     
	{

	int  len ;
	int	ch ;
	int  cnt, indat0;        /* beginning of a string in 'indat' */
	char *np, *bp ;
	Bool in_buf;             /* workin on a buffer of characters - flag */
	int  ent;              /* the entry point to this ``prog'' */
	int  sav_P, sav_I;       /* remember poff & ioff in case error */
	Bool	litchar ;
	int	n ;
	int	sv, st ;

	char *pg_fndid();

	/* start pgetprg */
	defrot("getprog") ;

	/* Try to find the cap given by id */
	if (! (np = pg_fndid(id)) ) return -1 ; /*pg_er(E_NVAL, id, 0,0) ;*/

	/* no value for given cap */
	if (*np++ != '=') {
		return(-2) ;	
		}

	pg_init(np) ;		/* init the parser */
		
	sav_P = *pg_poff;    /* in case errors, remember original state */
	sav_I = *pg_ioff;
	p.pgerr1  = 0;      /* assume no errors to start. */

	(void) tlbtab(TL_RESET, st, &sv) ;	/* clear old local symbols */

#ifdef lint
	indat0 = *pg_ioff ;
#endif

	/* Are we working on an entry or a JSS entry */
	if (id_type == 'J') {
		if ((n = stab_gt(id, &sv, &st)) < 0) {	/* not defined yet */
			n = stab_pt(id, jsscur, 'J') ;
			pg_prog[ jsscur++ ] = *pg_poff + 1;
			}

		else  pg_prog[ sv ] = *pg_poff + 1;
		}

	else /* if (id_type == 'E') */	{
		if ((n = stab_gt(id, &sv, &st)) < 0) { /* OK, not def'd yet */
			n = stab_pt(id, *pg_poff + 1, id_type) ;
			}

		else {
			/* already loaded id... */
			return pg_er(E_LDED, id, 0,0) ;
			}
		}

	/* if (n < 0) then symbol table error */
	switch (n) {
	case -1: return pg_er(E_NSTB, "", 0,0) ;	/* no table alloc'd */
	case -2: return pg_er(E_STSX, id, 0,0) ;	/* no more entries */
	case -3: return pg_er(E_STSY, id, 0,0) ;	/* no more room */
		}

	ppv( n ) ;		/* symbol number in stab */
	ent  = *pg_poff ;
	entcur = ent ;


	in_buf = F;
	cnt    = 0 ; 

	makprg() ;

	if (p.pgerr1 < 0)  {            /* did we get any errors ??? */
		*pg_poff = sav_P;    /* restore original state */
		*pg_ioff = sav_I;
		return(p.pgerr1);
		}

	ppi(END, 0, 0, 0, 0) ;

	/* See if there are any unresolved temp labels */
	sv = 0 ;
	while ((n = tlbtab(TL_CHK, 0, &sv)) >= 0) 
		(void)pg_er(E_UTMP, "", n, 0) ;	/* undef'd temp label */

	return (ent);    /* ALL was good !!! return entry point */
	}

/* "maki23" : Make the 2 or 3 word instruction sequence (ie: CPO).  If 
 *	val1 is small enough put it in the REG field of the instruction,
 *	else in the word after the instruction.
 *	Return number of words put.
 */
static int
maki23(ir, val1, val2)
	int	ir   ;
	int	val1 ;	/* number of chars to copy out */
	int	val2 ;	/* beginning of character seq to be copied */
	{
	int  nw = (IS_SMALL(val1) ? 2 : 3) ;

#ifndef lint
	defrot("maki23") ;
#endif

	if (nw == 2)  ppi(ir, 0, REG, val1, 0   ) ;
	else          ppi(ir, 0, DAT, 0,    val1) ;

	ppv(val2) ;

	return(nw) ;
	}


/* Comment out an instruction / Don't put it */
#define DONTPUT(IR) IR |= GTCR 
/* #define DONTPUT(IR)   return */

/* DEBUG: for now: simple ppi, more complex later */
static
ppi(ir, wr, mr, rr, xr)
	{
	int	ityp , irt ;	/* temp copy of ir */

	static int lir = -1, lwr = -1, lmr = -1, lrr = -1 ;

#ifndef lint
	defrot("ppi") ;
#endif
	irt = gti(ir) ;
        if (ir == NOP)  return ;

	/*    Avoid:   ``lod.b b''   */
	if (ir == LOD && ((wr == WA && rr == TA) || (wr == WB && rr == TB)) )
		DONTPUT(irt) ;

	/* Optimize: Avoid  ``str.a r5 ; lod.a r5'' */
	if (ir==LOD && lir==STR) {
		if (mr==lmr && mr==REG && wr==lwr && rr==lrr)
			DONTPUT(irt) ;
		/* if ((iw & (GTMR|GTWR|GTRR)) == (lw & (GTMR|GTWR|GTRR))) */
		}

	/* Put the instruction */
	ppv(irt | gtw(wr) | gtm(mr) | gtr(rr) ) ;

	ityp =  irtype(ir) ;
	switch (ityp) {

	case IT_NORM:
		if (mr == DAT)	ppv(xr) ;
		break ;

	case IT_IO:
		if (mr & ASC) ppv(xr) ;
		break ;

	case IT_ONE:		/* no xr for one words */
		break ;
	default:
		switch (ir) {
		case CPO:
		case AST:
			if (mr == DAT) ppv(xr) ;
			break ;
			}
		break ;
		}/*of switch on ir type */

	lir = ir ;
	lmr = mr ;
	lwr = wr ;
	lrr = rr ;
	}


/* PASSn: for now: put a string */
static
pis(str, len, term)
	char  *str ;
	int    len, term ;
	{

	if (len <= 0) len = strlen(str) ;

	while (len--) piv(*str++) ;

	if (term) piv('\0') ;
	}

/* "tlbtab" : Maintain temp key/value table */
static int
tlbtab(cmd, lab, val)
	int	 cmd ;
	int	 lab, *val ;
	{
	static	 int labs[MAX_TLABS] , vals[MAX_TLABS] ;
	static	 int lmax = 0 ;
	int	 i, found = F , n = -1 ;

	/* ---- start ---- */
	if (cmd == TL_RESET) {		/* clear the table (empty it) */
		lmax = 0 ;
		return(0) ;
		}

	if (cmd == TL_DEF) {		/* Define a temp label */

		for (i = 0 ; i < lmax; i++) {
			if (n < 0 && labs[i] < 0) n = i ;  /* is open slot */

			if (lab == labs[i]) {

				/* Undefined, so define it now */
				if (vals[i] < 0) {
					pg_prog[-vals[i] ] = *val ;/* def it */

					if (!found) {
						found = T ;
						vals[i] = *val ;
						}
					else labs[i] = -1 ; /* open slot */
					}

				else return(-1) ;	/* already defined */
				}
			}

		if (!found) {
			if (n < 0) {	/* reuse an old slot if we can */
				if (lmax >= MAX_TLABS) return(-2) ;
				n = lmax++ ;
				}
			labs[n] = lab ;
			vals[n] = *val ;
			}
		}/*of Define a label */

	/* Look up a label, if not found, add it as undefined */
	else if (cmd == TL_GET) {
		for (i = 0; i < lmax ; i++) {
			if (n < 0 && labs[i] < 0) n = i ;  /* is open slot */

			if (labs[i] == lab && vals[i] >= 0) {
				*val = vals[i] ;
				return(0) ;
				}
			}

		if (n < 0) {
			if (lmax >= MAX_TLABS) return(-2) ;
			n = lmax++ ;
			}

		labs[n] = lab ;
		vals[n] = - *val ;
			
		return(1) ;
		}

	/* Look for undefined labels.... */
	else if (cmd == TL_CHK) {
		for ( ; (*val)++ < lmax; )
			if (labs[*val] >= 0 && vals[*val] < 0)
				return(labs[*val]) ;
		return(-1) ;
		}

	return(0) ;
	}/*of Routine */

/* "pglex": lexical analyser for getting ``programs'' from plotcap files.
 *	This is the basic lex (entok) used for all pg_pgetXXX.c routines
 *	with a little extra mapping done.
 */
static int
pglex(str,strsz, ival, rval)
	char	 str[] ;
	int	 strsz ;
	int	*ival ;
	float	*rval ;
	{

	register int	n ;		/* token type */
	register struct _strint *ip ;
	float	 atof() ;

	switch ((n = entok(str,strsz))) {
	case LXINT:
		*ival = atoi(str) ;
		break ;

	case LXOCT:
		n = LXINT ;
		sscanf(str, "%o", ival) ;
		break ;

	case LXHEX:
		n = LXINT ;
		sscanf(str, "%x", ival) ;
		break ;

	case LXREL:
		*rval = atof(str) ;
		break ;

	case LXSYM:			/* See what type of symbol */
		
		/* register name? */
		for (ip = r18list; *ip->sv; ip++) {
			if (strequ(str, ip->sv)) {
				*ival = ip->iv ;
				return(LXREG) ;
				}
			}

		/* variable name? */
		for (ip = iv18list; *ip->sv; ip++) {
			if (strequ(str, ip->sv)) {
				*ival = ip->iv ;
				return(LXVAR) ;
				}
			}

		for (ip = fv18list; *ip->sv; ip++) {
			if (strequ(str, ip->sv)) {
				*ival = ip->iv ;
				return(LXFVR) ;	/* Floating point... */
				}
			}

		*ival = strlen(str) ;	/* NOTE: get len from lexst[] */
		break ;

	case GT_MRK:			/* End of string MaRK --> EOS */
		n = LXEOS ;
		break;
		}

	return n ;
	}


/* date: (# <29 Mar 1985> [dredge@Fuji] #)
 *
 * "makprg" : Make the "alu18" program sequence by parsing/compiling the
 *	given input string.
 *
 * notes:
 *	> This parser is generated with  "mpp" -> "mkpar" -> "mpp".
 *
 * written:  Michael Eldredge (jan 85)
 * modified: Michael Eldredge (may 86) Handles floating point data in a
 *	very simple manner.  %f = X; is ok but %f = X + 2.0 is Not.
 */


%> global

/* Use our own names for the parser and error routine */
%define	 PARSER_NAME	
static
makprg()
%enddef

%define  PARSER_PERR	(void)pg_er(E_PSYN, sval, 0,0)

%define  LEX_FUNC	pglex(sval,sizeof(sval), &ival, &rval)
#define  SVALSIZE	256

%define  BEGIN_STATE	S_begin
%define  END_STATE  	S_end
%define  ERR_STATE  	S_err


%> local

	/* Local variables used by the actions */
	long	l_ir, l_wr, l_mr, l_rr , l_xr ;
	long 	r_ir, r_wr, r_mr, r_rr , r_xr ;

	int	hival , loval ;		/* bit ranges */
	int	is_neg ;
	int	i , j , m , n ;		/* temp values for actions */
	int	frm_loc   ;		/* location of current format string */
	struct	_strint	*ip ;

	/* Binary format chars/types. Must end in NULL */
	static 	char bforms[] = { 'R'  , 'B'  , 'W'  , 'L'  , '\0' } ;
	static	int  btypes[] = { RAWF , BYTE , WORD , LONG        } ;

	/* Ascii format chars.  Just for validity checking */
	static	char aforms[] = "defg" ;

	static	char fforms[] = "efg" ;
	static	char iforms[] = "d" ;

	/* Operator character list */
	static	char operc[] = {'+','-','*','/','&','|','^','<','>','\0' };
	static	int  operi[] = {ADD,SUB,MUL,DIV,AND,IOR,EOR,SFL,SFR      };

	/* Used by lexical function */
	char	sval[SVALSIZE] ;
	int	ival ;
	float	rval ;

	defrot("makprg") ;

%> actions

/* Parser actions for our parser (generated by "mkpar") */

new_stat {{
	l_ir = NOP ;  l_mr = REG ;  l_wr = WA ;  l_rr = -1 ;  l_xr = -1 ;
	r_ir = LOD ;  r_mr = -1  ;  r_wr = WA ;  r_rr = -1 ;  r_xr = -1 ;
	is_neg = F ;
	frm_loc = -1 ;
	}}

str_reg {{
	l_ir = STR ; l_rr = ival ;
	l_mr = REG ;
	}}

intrin {{			/* ALU intrinsics */
	/* Look up the intrinsic name */
	for (ip = itr18list;  *ip->sv; ip++)
		if (strequ(sval, ip->sv)) break ;

	if (*ip->sv) ppi(ITR, 0, 0, -(ip->iv), 0) ;	/* Do intrinsic #n */
	else (void)pg_er(E_UINTR, sval, 0,0) ;	/* unkn intrinsic */
	}}

sav_llab {{
	n = ival ;		/* Temp label number */
	}}

put_llab {{
	/* Set the temp label with current offset */
	i = tlbtab(TL_DEF, n, pg_poff) ;
	if (i == TLE_OVER) (void)pg_er(E_TMPLX, "",n, 0);  /* too many*/
	if (i == TLE_SET ) (void)pg_er(E_TMPLD, "",n,0);   /* already def'd */
	}}

keeps {{			/* Beginning of a keep section */
	ppi(AST, WA, DAT, 0, 0) ;	/* just put zero's for now, */
	ppv(0) ;			/*  ... pass2 will fix 'em */
	}}

keepe {{			/* End of a keep section */
	ppi(KPE, 0, 0, 0, 0) ;		/* Will get changed by pass2 */
	}}

str_val {{
	l_ir = STR ; l_rr = ival ;
	l_mr = VAR ;
	}}

/* Floating point Store-value */
fstr_val {{
	l_ir = FSTR; l_rr = ival ;
	l_mr = VAR ;
	}}


jss_sym {{
	/* Save the symbol table stuff */
	if (stab_gt(sval, &n, &i) < 0)  {
		n = stab_pt(sval, jsscur, 'J') ;

		switch (n) {
		/* 1. no table alloc'd, 2. too many ents, 3. no room */
		case -1: (void)pg_er(E_NSTB, "",0,0) ;		return ;
		case -2: (void)pg_er(E_STSX, sval, 0,0) ;	return ;
		case -3: (void)pg_er(E_STSY, sval, 0,0) ;	return ;
			}

		pg_prog[ jsscur ] = -n ;
		n = jsscur++ ;
		}

	ppi(JSS , l_wr, REG, n, l_xr) ;
	}}

on_star {{		/* here is where we switch from e1 to e2 */
	l_wr = JSS_STAR ;
	}}

cpo_str {{		/* copy out a string */
	i = pg_cvtstr(&pg_idat[ *pg_ioff ], sval) ;
	if (i > 0) {
		(void)maki23(CPO, i, *pg_ioff) ;
		*pg_ioff += i ;
		}
	/*else ppi(NOP,0,REG,0,0) ; */
	}}
	/* NOTE: if ("string 1"; "string 2"; ) then do only one cpo */

set_out {{
	l_ir = MVO ; l_wr = WA ; l_mr = BIN ; l_rr = RAWF;
	frm_loc = *pg_ioff ;	/* will be beginning of format if ASC */
	piv('%') ;
	}}

out_sym {{
	if (member(bforms, *sval)) {	/* binary format ? */
		l_mr = BIN ;
		l_rr = btypes[ offset(bforms, *sval) ] ;
		*pg_ioff = frm_loc ;	/* reset idat. Bag "%..." string */
		}

	else if (member(iforms, *sval)) {	/* INTEGERS */
		l_mr = ASC ;		/* ascii format */
		piv(*sval) ;   piv('\0') ;
		l_xr = frm_loc ;	/* where is that format string? */
		}

	else if (member(fforms, *sval)) {	/* FLOATING POINTS */
		l_xr = frm_loc ;
		s    = S_fout_2 ;	/* ooops, goto float format */
		needt= F ;		/* don't need a new token either */
		}

	else (void)pg_er(E_UOFMT, sval, 0,0) ;  /* unkn output format */
	frm_loc = -1 ;
	}}

/* Floating point output */
fout_sym {{
	if (member(fforms, *sval)) {
		l_mr = ASC ;
		piv(*sval) ; piv('\0') ;
	/*	l_xr = frm_loc ; */
		l_ir = FMVO ;
		}

	else (void)pg_er(E_UFFMT, sval, 0,0) ;	/* uknown float out form */
	}}

f_swit {{		/* oops, we were doing integer, but it should be */
	if      (l_ir == LOD) l_ir = FLOD ;
	else if (l_ir == MVO) l_ir = FMVO ;
	else if (l_ir == STR) l_ir = FSTR ;
	}}

out_int {{
	pis(sval, 0, F) ;	/* Numeric format part */
	}}

out_flt {{
	pis(sval, 0, F) ;	/* Numeric format part */
	}}

out_ind {{
	l_mr |= IIO ;	/* indirect output */
	l_rr |= ival;   /* the indirect register */
	}}

do_tst {{		/* Put out the test intructions */
	ppi(TST, WA, l_mr, l_rr) ;
	}}

compop {{			/* A comparison operator */
	switch (*sval) {
	case '=':	n = CC_EQ ;		break ;
	case '!':	n = CC_LT | CC_GT;	break ;
	case '>':	n = CC_GT ;		break ;
	case '<':	n = CC_LT ;		break ;
		}
	}}

is_goto {{			/* A goto branch (bra - branch always) */
	n = CC_LT|CC_EQ|CC_GT ;		/* always branch... */
	}}

compop2 {{			/* Is < or > really <= or >= ??? */
	n |= CC_EQ ;
	}}

#gollab {{		/* the goto label */
#	m = ival ;
#	}}

put_bcc {{			/* Now put the Bcc instruction */
	i = *pg_poff + 1;	/* TEMP, incase not defined yet... */

	/* Look up label ival... */
	if ((j = tlbtab(TL_GET, ival, &i)) == TLE_OVER)
		(void)pg_er(E_TMPLX, "", n, 0) ;   /* too many tlabels */

	j = (j >= 0) ? WB : WA ;	/* WB for defined */

	/* N set in is_goto, compop, or compop2 */ 
	ppi(BCC, j, DAT, n) ;		/* n::CC */
	ppv(i) ;
	}}

neg_val {{
	is_neg = T ;
	}}

v_val {{
	r_mr = VAR ;  r_rr = ival ;
	}}
v_reg {{
	r_mr = REG ;  r_rr = ival ;
	}}

v_int {{
	r_mr = DAT ;  r_xr = ival ;
	}}

/* Floating point loads */
f_var {{			/* floating point variable X, Y */
	r_ir = FLOD ;
	r_mr = VAR ;
	r_rr = ival ;
	}}

f_val {{			/* a floating point constant */
	r_ir = FLOD ;
	r_mr = DAT ;
	r_xr = I_F( rval ) ;	/* put it in an integer .... */
	}}


in_set {{
	r_rr = RAWF ;
	frm_loc = *pg_ioff ;
	piv('%') ;
	}}

in_val {{
	if (member(bforms, *sval)) {	/* binary format ? */
		r_mr = BIN ;
		r_rr = btypes[ offset(bforms, *sval) ] ;
		*pg_ioff = frm_loc ;	/* reset idat. bag the "%..." */
		}

	else if (member(aforms, *sval))  {
		r_mr = ASC ;	/* ascii format */
		piv(*sval) ; piv('\0') ;
		r_xr = frm_loc ;
		}

	else (void)pg_er(E_UIFMT, sval, 0,0) ;	/* unkn input format */
	frm_loc = -1 ;
	}}

in_int {{
	pis(sval, 0, F) ;	/* don't really want user doing this, but */
	}}

#..  never
#..in_flt {{
#..	pis(sval, 0, F) ;	/* don't really want user doing this, but */
#..	}}
#..  never

in_put {{	
	n = ( (r_ir != LOD) ? WB : WA) ;
	ppi(MVI, n, r_mr, r_rr, r_xr) ;
	if (r_ir == LOD) r_ir = NOP ;
	r_wr = WA ;
	r_mr = REG;
	r_rr = TB ;
	}}

sub_hi {{
	hival = ival ; loval = ival ;
	}}

sub_lo {{
	loval = ival ;
	}}

sub_end {{
	i = 0 ;		/* No reversal for now */
	if (hival < loval) {
		i =1 ;			/* reverse bits later */
		n     = hival ;		/* flip hi and lo */
		hival = loval ;
		loval = n     ;
		}

	/* make the bit mask */
	m  =  (1 << (hival - loval +1)) -1 ;
	if (loval) m <<= loval ;

	/* and lay out those instructions */
	n = ( (r_ir != LOD) ? WB : WA) ;	/* if prev is LOD don't use */
	ppi(LOD, n, r_mr, r_rr, -1) ;		/*  regB, regA is Ok. */

	ppi(AND, n, DAT, 0, m) ;
	if (loval) ppi(SFR, n, DAT, 0, loval) ;
	if (i)     ppi(RBT, n, DAT, 0, loval) ;

	/* and fix up for the next attempt to finish an instruction */
	if (r_ir == LOD) r_ir = NOP ;		/* NOP says don't finish prev */
	r_mr = REG ;
	r_wr = WA  ;
	r_rr = TB  ;	/* get in the temp reg we've been using */
	}}

/* Finish up the (simple) floating point sequence */
f_done {{
	ppi(r_ir, r_wr, r_mr, r_rr, r_xr) ;
	ppi(l_ir, l_wr, l_mr, l_rr, l_xr) ;
	}}
	
save_oper {{
	/* put out the previous (just finished) instruction(s) */

	if (is_neg) {		/* negate value ? */
		if (r_mr == DAT)  r_xr = -r_xr ;	/* just negate data */
		else {
			if (r_ir == LOD) {	
				ppi(r_ir, r_wr, r_mr, r_rr, r_xr) ;
				ppi(NEG , r_wr,  REG,  0  ,  -1 ) ;
				}

			else {
				n = WB ;
				ppi(LOD, WB, REG, r_rr, r_xr);
				ppi(NEG, WB, REG,   0 , -1  );
				ppi(r_ir,WA, REG,  TB , r_xr);
				}
			r_ir = NOP ;
			}
		}

	ppi(r_ir, r_wr, r_mr, r_rr, r_xr) ;

	/* probably can ditch this since it will get done at S_new_stat */
	r_ir = -1 ;
	r_wr = WA ;
	r_mr = REG;
	r_rr = -1 ;
	r_xr = -1 ;
	is_neg = F ;


	/* resolve the operation */
	if (s == S_val) {
		if (member(operc,*sval)) r_ir = operi[ offset(operc, *sval) ] ;
		else (void)pg_er(E_UOPER, sval, 0,0) ;	/* unkn operator */
		}

	else {
		ppi(l_ir, l_wr, l_mr, l_rr, l_xr) ;	/* the lhs instruct */
		}
	}}


cnt_errs {{
	}}



%> rules

# Parsing state diagram for ``programs'' of the plotcap (V.3) kind
#
# Note: This version has the old "Limited Floating Point" support
#	that the old alu06() used to have. In other words, we do simple
#	assignment floating point, no complex expressions.
#   e.g. :      %f = 12.4 ; %W = X ; 
#	NOT!!!! -> %f = X + 2.0 ;
#
# dredge may 86
# dredge dec 86

begin
	:	-> -lhs		=> new_stat	# init and reset....
	;

end	: ;

lhs
	: LXREG	-> geteq	=> str_reg
	| LXVAR	-> geteq	=> str_val
	| LXSYM	-> new		=> jss_sym
	| LXSTR	-> new		=> cpo_str
	| "*	-> jss_star	=> on_star
	| "%	-> out_1	=> set_out
	| ".	-> intrin
	| "{	-> begin	=> keeps
	| "}	-> new		=> keepe
	| LXINT	-> loclab	=> sav_llab
	| LXFVR	-> fgeteq	=> fstr_val	# Floating point output.
	| "$	-> goto_lab	=> is_goto	# bra <goto>
	|	-> -new
	;

intrin	
	: LXSYM	-> new		=> intrin
	|	-> err
	;

loclab
	: "$	-> begin	=> put_llab
	|	-> err
	;

jss_star
	: LXSYM	-> new		=> jss_sym
	|	-> err
	;

out_1
	: LXSYM	-> out_ind	=> out_sym	# check %f
	| LXINT	-> out_2	=> out_int
	| LXREL	-> fout_2	=> out_flt	# Float format
	|	-> -out_ind
	;

out_2
	: LXSYM	-> out_ind	=> out_sym	# need to check %2f
	|	-> err
	;

fout_2		# Output for floats.
	: LXSYM	->  fgeteq	=> fout_sym
	|	-> -err
	;

out_ind
	: "@	-> out_ir
	|	-> -geteq
	;

out_ir
	: LXREG	-> geteq	=> out_ind
	|	-> err
	;

geteq
	: "=	-> val_1st
	| "?	-> compare	=> do_tst
	|	-> err
	;

fgeteq		# For floating point support.  Real simple here.
	: "=	-> fval
	|	-> err
	;

goto_lab	# a BRA <goto> instruction...
	: LXINT	-> new		=> put_bcc	# put the BRA
	|	-> err
	;

compare
	: "=	-> needeq	=> compop
	| "!	-> needeq	=> compop
	| "<	-> chckeq	=> compop
	| ">	-> chckeq	=> compop
	|	-> err
	;

needeq
	: "=	-> cmpdest
	|	-> err
	;

chckeq
	: "=	-> cmpdest	=> compop2
	|	-> -cmpdest
	;

# -begin old way--------------------------------
#cmpdest
#	: LXINT	-> chkglab	=> gollab
#	|	-> err
#	;
#
#chkglab
#	: "$	-> newcmp	=> put_bcc
#	|	-> err
#	;
# -endof old way--------------------------------
# begin.new.way.....
cmpdest
	: "$	-> chkglab
	|	-> err
	;

chkglab
	: LXINT	-> newcmp	=> put_bcc
	|	-> err
	;
# end.new.way.....


newcmp
	: "|	-> compare
	|	-> -new
	;

# first time, check to see if we are really doing floats.
val_1st
	: LXREL	-> -fval	=> f_swit	# switch over to floats..
	| LXFVR	-> -fval	=> f_swit
	|	-> -val				# ok. it is still an int.
	;

val
	: "+	-> val_type
	| "-	-> val_type	=>  neg_val
	|	-> -val_type
	;

fval		# floating point value.
	: LXFVR	-> f_done	=> f_var
	| LXREL	-> f_done	=> f_val
# ...	| "%	-> f_in1			# floating point input.
	|	-> err
	;

# Finish up the floating point sequence 
f_done
	:	-> -new		=> f_done
	;


val_type
	: LXREG	-> sub		=> v_reg
	| LXVAR -> sub		=> v_val
	| LXINT	-> oper		=> v_int
#	| LXREL	-> oper		=> v_flt
	| "%	-> in_1		=> in_set
	|	-> err
	;

in_1
	: LXSYM	-> in_put	=> in_val
	| LXINT	-> in_2		=> in_int
#	| LXREL	-> in_2		=> in_flt
	|	-> in_put
	;

in_2
	: LXSYM	-> in_put	=> in_val
	|	-> err
	;

in_put
	: 	-> -oper	=> in_put
	;

# See if a bit sub-field was given.
sub
	: "[	-> sub_hi
	|	-> -oper
	;

sub_hi
	: LXINT	-> sub_rng	=> sub_hi
	|	-> err
	;

sub_rng
	: "-	-> sub_lo
	| "/	-> sub_lo
	| "]	-> oper		=> sub_end
	|	-> err
	;

sub_lo
	: LXINT	-> sub_end	=> sub_lo
	|	-> err
	;

sub_end
	: "]	-> oper		=> sub_end
	|	-> err
	;


oper
	: "+	-> val		=> save_oper
	| "|	-> val		=> save_oper
	| "-	-> val		=> save_oper
	| "&	-> val		=> save_oper
	| "*	-> val		=> save_oper
	| "/	-> val		=> save_oper
	| "<	-> val		=> save_oper
	| ">	-> val		=> save_oper
	| "^	-> val		=> save_oper
	| 	-> -new		=> save_oper
	;

new
	: ";	-> begin	# Let's try to allow both ; and
	| ",	-> begin	#  ,  Will this be a problem?
	| LXEOS	-> end
	|	-> err
	;

err
	: 	-> find_new	=> cnt_errs
	;

find_new
	: ";	-> begin
	| LXEOS	-> end
	|	-> find_new
	;

