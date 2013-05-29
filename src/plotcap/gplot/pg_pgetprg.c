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

static int getprog(char *id, int id_type);
static int maki23(int ir, int val1, int val2);
static ppi(int ir, int wr, int mr, int rr, int xr);
static pis(char *str, int len, int term);
static int tlbtab(int cmd, int lab, int *val);
static int pglex(char str[], int strsz, int *ival, float *rval);
static makprg(void);


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



/* date: 29 jan 86 (mje)
 * This is the parser subroutine that uses the tables built by 'mkpar'.
 */

/* Use our own names for the parser and error routine */


#define  SVALSIZE	256




/* States in our parsing. */
#define  S_lhs              1
#define  S_begin            2
#define  S_end              3
#define  S_geteq            4
#define  S_new              5
#define  S_jss_star         6
#define  S_out_1            7
#define  S_intrin           8
#define  S_loclab           9
#define  S_fgeteq           10
#define  S_goto_lab         11
#define  S_err              12
#define  S_out_ind          13
#define  S_out_2            14
#define  S_fout_2           15
#define  S_out_ir           16
#define  S_val_1st          17
#define  S_compare          18
#define  S_fval             19
#define  S_needeq           20
#define  S_chckeq           21
#define  S_cmpdest          22
#define  S_chkglab          23
#define  S_newcmp           24
#define  S_val              25
#define  S_val_type         26
#define  S_f_done           27
#define  S_sub              28
#define  S_oper             29
#define  S_in_1             30
#define  S_in_put           31
#define  S_in_2             32
#define  S_sub_hi           33
#define  S_sub_rng          34
#define  S_sub_lo           35
#define  S_sub_end          36
#define  S_find_new         37

/* Actions to be done. */
#define  A_NoAction         0
#define  A_new_stat         1
#define  A_str_reg          2
#define  A_str_val          3
#define  A_jss_sym          4
#define  A_cpo_str          5
#define  A_on_star          6
#define  A_set_out          7
#define  A_keeps            8
#define  A_keepe            9
#define  A_sav_llab         10
#define  A_fstr_val         11
#define  A_is_goto          12
#define  A_intrin           13
#define  A_put_llab         14
#define  A_out_sym          15
#define  A_out_int          16
#define  A_out_flt          17
#define  A_fout_sym         18
#define  A_out_ind          19
#define  A_do_tst           20
#define  A_put_bcc          21
#define  A_compop           22
#define  A_compop2          23
#define  A_f_swit           24
#define  A_neg_val          25
#define  A_f_var            26
#define  A_f_val            27
#define  A_f_done           28
#define  A_v_reg            29
#define  A_v_val            30
#define  A_v_int            31
#define  A_in_set           32
#define  A_in_val           33
#define  A_in_int           34
#define  A_in_put           35
#define  A_sub_hi           36
#define  A_sub_end          37
#define  A_sub_lo           38
#define  A_save_oper        39
#define  A_cnt_errs         40
/* This must be a unique token (ie: a number that you
 *  are not returning from your lexical analyser).
 */
#define  Default      -9393	/* Default token match */


/* parse tables .... */
static short	 ptokens[] = {
	/*         <Token>    , <Action>       , <Next State>   */

	/*   0 */  Default    , A_new_stat     ,  -S_lhs          ,
	/*   3 */  LXREG      , A_str_reg      ,   S_geteq        ,
	           LXVAR      , A_str_val      ,   S_geteq        ,
	           LXSYM      , A_jss_sym      ,   S_new          ,
	           LXSTR      , A_cpo_str      ,   S_new          ,
	           (int)'*'   , A_on_star      ,   S_jss_star     ,
	           (int)'%'   , A_set_out      ,   S_out_1        ,
	           (int)'.'   , A_NoAction     ,   S_intrin       ,
	           (int)'{'   , A_keeps        ,   S_begin        ,
	           (int)'}'   , A_keepe        ,   S_new          ,
	           LXINT      , A_sav_llab     ,   S_loclab       ,
	           LXFVR      , A_fstr_val     ,   S_fgeteq       ,
	           (int)'$'   , A_is_goto      ,   S_goto_lab     ,
	           Default    , A_NoAction     ,  -S_new          ,
	/*  42 */  LXSYM      , A_intrin       ,   S_new          ,
	           Default    , A_NoAction     ,   S_err          ,
	/*  48 */  (int)'$'   , A_put_llab     ,   S_begin        ,
	           Default    , A_NoAction     ,   S_err          ,
	/*  54 */  LXSYM      , A_jss_sym      ,   S_new          ,
	           Default    , A_NoAction     ,   S_err          ,
	/*  60 */  LXSYM      , A_out_sym      ,   S_out_ind      ,
	           LXINT      , A_out_int      ,   S_out_2        ,
	           LXREL      , A_out_flt      ,   S_fout_2       ,
	           Default    , A_NoAction     ,  -S_out_ind      ,
	/*  72 */  LXSYM      , A_out_sym      ,   S_out_ind      ,
	           Default    , A_NoAction     ,   S_err          ,
	/*  78 */  LXSYM      , A_fout_sym     ,   S_fgeteq       ,
	           Default    , A_NoAction     ,  -S_err          ,
	/*  84 */  (int)'@'   , A_NoAction     ,   S_out_ir       ,
	           Default    , A_NoAction     ,  -S_geteq        ,
	/*  90 */  LXREG      , A_out_ind      ,   S_geteq        ,
	           Default    , A_NoAction     ,   S_err          ,
	/*  96 */  (int)'='   , A_NoAction     ,   S_val_1st      ,
	           (int)'?'   , A_do_tst       ,   S_compare      ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 105 */  (int)'='   , A_NoAction     ,   S_fval         ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 111 */  LXINT      , A_put_bcc      ,   S_new          ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 117 */  (int)'='   , A_compop       ,   S_needeq       ,
	           (int)'!'   , A_compop       ,   S_needeq       ,
	           (int)'<'   , A_compop       ,   S_chckeq       ,
	           (int)'>'   , A_compop       ,   S_chckeq       ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 132 */  (int)'='   , A_NoAction     ,   S_cmpdest      ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 138 */  (int)'='   , A_compop2      ,   S_cmpdest      ,
	           Default    , A_NoAction     ,  -S_cmpdest      ,
	/* 144 */  (int)'$'   , A_NoAction     ,   S_chkglab      ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 150 */  LXINT      , A_put_bcc      ,   S_newcmp       ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 156 */  (int)'|'   , A_NoAction     ,   S_compare      ,
	           Default    , A_NoAction     ,  -S_new          ,
	/* 162 */  LXREL      , A_f_swit       ,  -S_fval         ,
	           LXFVR      , A_f_swit       ,  -S_fval         ,
	           Default    , A_NoAction     ,  -S_val          ,
	/* 171 */  (int)'+'   , A_NoAction     ,   S_val_type     ,
	           (int)'-'   , A_neg_val      ,   S_val_type     ,
	           Default    , A_NoAction     ,  -S_val_type     ,
	/* 180 */  LXFVR      , A_f_var        ,   S_f_done       ,
	           LXREL      , A_f_val        ,   S_f_done       ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 189 */  Default    , A_f_done       ,  -S_new          ,
	/* 192 */  LXREG      , A_v_reg        ,   S_sub          ,
	           LXVAR      , A_v_val        ,   S_sub          ,
	           LXINT      , A_v_int        ,   S_oper         ,
	           (int)'%'   , A_in_set       ,   S_in_1         ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 207 */  LXSYM      , A_in_val       ,   S_in_put       ,
	           LXINT      , A_in_int       ,   S_in_2         ,
	           Default    , A_NoAction     ,   S_in_put       ,
	/* 216 */  LXSYM      , A_in_val       ,   S_in_put       ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 222 */  Default    , A_in_put       ,  -S_oper         ,
	/* 225 */  (int)'['   , A_NoAction     ,   S_sub_hi       ,
	           Default    , A_NoAction     ,  -S_oper         ,
	/* 231 */  LXINT      , A_sub_hi       ,   S_sub_rng      ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 237 */  (int)'-'   , A_NoAction     ,   S_sub_lo       ,
	           (int)'/'   , A_NoAction     ,   S_sub_lo       ,
	           (int)']'   , A_sub_end      ,   S_oper         ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 249 */  LXINT      , A_sub_lo       ,   S_sub_end      ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 255 */  (int)']'   , A_sub_end      ,   S_oper         ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 261 */  (int)'+'   , A_save_oper    ,   S_val          ,
	           (int)'|'   , A_save_oper    ,   S_val          ,
	           (int)'-'   , A_save_oper    ,   S_val          ,
	           (int)'&'   , A_save_oper    ,   S_val          ,
	           (int)'*'   , A_save_oper    ,   S_val          ,
	           (int)'/'   , A_save_oper    ,   S_val          ,
	           (int)'<'   , A_save_oper    ,   S_val          ,
	           (int)'>'   , A_save_oper    ,   S_val          ,
	           (int)'^'   , A_save_oper    ,   S_val          ,
	           Default    , A_save_oper    ,  -S_new          ,
	/* 291 */  (int)';'   , A_NoAction     ,   S_begin        ,
	           (int)','   , A_NoAction     ,   S_begin        ,
	           LXEOS      , A_NoAction     ,   S_end          ,
	           Default    , A_NoAction     ,   S_err          ,
	/* 303 */  Default    , A_cnt_errs     ,   S_find_new     ,
	/* 306 */  (int)';'   , A_NoAction     ,   S_begin        ,
	           LXEOS      , A_NoAction     ,   S_end          ,
	           Default    , A_NoAction     ,   S_find_new    
	} ;

/* State pointers. */
static short	 pstates[] = {
	 -1 , 	/* --- never used --- */
	  3 , 	/* S_lhs        */
	  0 , 	/* S_begin      */
	 -1 , 	/* S_end        */
	 96 , 	/* S_geteq      */
	291 , 	/* S_new        */
	 54 , 	/* S_jss_star   */
	 60 , 	/* S_out_1      */
	 42 , 	/* S_intrin     */
	 48 , 	/* S_loclab     */
	105 , 	/* S_fgeteq     */
	111 , 	/* S_goto_lab   */
	303 , 	/* S_err        */
	 84 , 	/* S_out_ind    */
	 72 , 	/* S_out_2      */
	 78 , 	/* S_fout_2     */
	 90 , 	/* S_out_ir     */
	162 , 	/* S_val_1st    */
	117 , 	/* S_compare    */
	180 , 	/* S_fval       */
	132 , 	/* S_needeq     */
	138 , 	/* S_chckeq     */
	144 , 	/* S_cmpdest    */
	150 , 	/* S_chkglab    */
	156 , 	/* S_newcmp     */
	171 , 	/* S_val        */
	192 , 	/* S_val_type   */
	189 , 	/* S_f_done     */
	225 , 	/* S_sub        */
	261 , 	/* S_oper       */
	207 , 	/* S_in_1       */
	222 , 	/* S_in_put     */
	216 , 	/* S_in_2       */
	231 , 	/* S_sub_hi     */
	237 , 	/* S_sub_rng    */
	249 , 	/* S_sub_lo     */
	255 , 	/* S_sub_end    */
	306   	/* S_find_new   */
	} ;

/* offsets from current state pointer to Next_state & This_action */
#define   NEWSTAT	2
#define   NEWACTS	1


static

makprg()

	{
	register int	s , t , a ;
	int	sp ,tk , needt ;
	int	state , token ;		/* current state and token */
	int	nexts ;			/* the next state */


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


	/* ---- start ---- */
	s     = S_begin ;	/* initial state */
	needt = 1 ;			/* do we need the next token? */

	while (s != S_end) {		/* run through things */
		/* get the next token if we need one */
		if (needt) { t = pglex(sval,sizeof(sval), &ival, &rval) ; }
		needt = 1 ;

		/* find out what to do now (what's next state & what action) */
		for ( sp = pstates[ s ] ; ; sp += 3) {
			tk = ptokens[sp] ;
			if ( (tk == Default) || (tk == t) ) break ;
			}

		state = s ;			/* remember these to actions */
		token = t ;

		s = ptokens[ sp + NEWSTAT ] ;	/* next state */
		a = ptokens[ sp + NEWACTS ] ;	/* this action to take */

		if (s < 0) {
			needt = 0 ;
			s     = -s;
			}
		nexts = s ;		/* remember the next state */

		/* print that we found an error */
		if (s == S_err) { (void)pg_er(E_PSYN, sval, 0,0) ; }

		/* ---- Actions list ------ */
		switch (a) {	
			case A_NoAction:	/* nothing to do */
				break ;

/* Parser actions for our parser (generated by "mkpar") */

case A_new_stat:
	l_ir = NOP ;  l_mr = REG ;  l_wr = WA ;  l_rr = -1 ;  l_xr = -1 ;
	r_ir = LOD ;  r_mr = -1  ;  r_wr = WA ;  r_rr = -1 ;  r_xr = -1 ;
	is_neg = F ;
	frm_loc = -1 ;
	break ;

case A_str_reg:
	l_ir = STR ; l_rr = ival ;
	l_mr = REG ;
	break ;

case A_intrin:			/* ALU intrinsics */
	/* Look up the intrinsic name */
	for (ip = itr18list;  *ip->sv; ip++)
		if (strequ(sval, ip->sv)) break ;

	if (*ip->sv) ppi(ITR, 0, 0, -(ip->iv), 0) ;	/* Do intrinsic #n */
	else (void)pg_er(E_UINTR, sval, 0,0) ;	/* unkn intrinsic */
	break ;

case A_sav_llab:
	n = ival ;		/* Temp label number */
	break ;

case A_put_llab:
	/* Set the temp label with current offset */
	i = tlbtab(TL_DEF, n, pg_poff) ;
	if (i == TLE_OVER) (void)pg_er(E_TMPLX, "",n, 0);  /* too many*/
	if (i == TLE_SET ) (void)pg_er(E_TMPLD, "",n,0);   /* already def'd */
	break ;

case A_keeps:			/* Beginning of a keep section */
	ppi(AST, WA, DAT, 0, 0) ;	/* just put zero's for now, */
	ppv(0) ;			/*  ... pass2 will fix 'em */
	break ;

case A_keepe:			/* End of a keep section */
	ppi(KPE, 0, 0, 0, 0) ;		/* Will get changed by pass2 */
	break ;

case A_str_val:
	l_ir = STR ; l_rr = ival ;
	l_mr = VAR ;
	break ;

/* Floating point Store-value */
case A_fstr_val:
	l_ir = FSTR; l_rr = ival ;
	l_mr = VAR ;
	break ;


case A_jss_sym:
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
	break ;

case A_on_star:		/* here is where we switch from e1 to e2 */
	l_wr = JSS_STAR ;
	break ;

case A_cpo_str:		/* copy out a string */
	i = pg_cvtstr(&pg_idat[ *pg_ioff ], sval) ;
	if (i > 0) {
		(void)maki23(CPO, i, *pg_ioff) ;
		*pg_ioff += i ;
		}
	/*else ppi(NOP,0,REG,0,0) ; */
	break ;
/* NOTE: if ("string 1"; "string 2"; ) then do only one cpo */

case A_set_out:
	l_ir = MVO ; l_wr = WA ; l_mr = BIN ; l_rr = RAWF;
	frm_loc = *pg_ioff ;	/* will be beginning of format if ASC */
	piv('%') ;
	break ;

case A_out_sym:
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
	break ;

/* Floating point output */
case A_fout_sym:
	if (member(fforms, *sval)) {
		l_mr = ASC ;
		piv(*sval) ; piv('\0') ;
	/*	l_xr = frm_loc ; */
		l_ir = FMVO ;
		}

	else (void)pg_er(E_UFFMT, sval, 0,0) ;	/* uknown float out form */
	break ;

case A_f_swit:		/* oops, we were doing integer, but it should be */
	if      (l_ir == LOD) l_ir = FLOD ;
	else if (l_ir == MVO) l_ir = FMVO ;
	else if (l_ir == STR) l_ir = FSTR ;
	break ;

case A_out_int:
	pis(sval, 0, F) ;	/* Numeric format part */
	break ;

case A_out_flt:
	pis(sval, 0, F) ;	/* Numeric format part */
	break ;

case A_out_ind:
	l_mr |= IIO ;	/* indirect output */
	l_rr |= ival;   /* the indirect register */
	break ;

case A_do_tst:		/* Put out the test intructions */
	ppi(TST, WA, l_mr, l_rr, 0) ;
	break ;

case A_compop:			/* A comparison operator */
	switch (*sval) {
	case '=':	n = CC_EQ ;		break ;
	case '!':	n = CC_LT | CC_GT;	break ;
	case '>':	n = CC_GT ;		break ;
	case '<':	n = CC_LT ;		break ;
		}
	break ;

case A_is_goto:			/* A goto branch (bra - branch always) */
	n = CC_LT|CC_EQ|CC_GT ;		/* always branch... */
	break ;

case A_compop2:			/* Is < or > really <= or >= ??? */
	n |= CC_EQ ;
	break ;


case A_put_bcc:			/* Now put the Bcc instruction */
	i = *pg_poff + 1;	/* TEMP, incase not defined yet... */

	/* Look up label ival... */
	if ((j = tlbtab(TL_GET, ival, &i)) == TLE_OVER)
		(void)pg_er(E_TMPLX, "", n, 0) ;   /* too many tlabels */

	j = (j >= 0) ? WB : WA ;	/* WB for defined */

	/* N set in is_goto, compop, or compop2 */ 
	ppi(BCC, j, DAT, n, 0) ;		/* n::CC */
	ppv(i) ;
	break ;

case A_neg_val:
	is_neg = T ;
	break ;

case A_v_val:
	r_mr = VAR ;  r_rr = ival ;
	break ;
case A_v_reg:
	r_mr = REG ;  r_rr = ival ;
	break ;

case A_v_int:
	r_mr = DAT ;  r_xr = ival ;
	break ;

/* Floating point loads */
case A_f_var:			/* floating point variable X, Y */
	r_ir = FLOD ;
	r_mr = VAR ;
	r_rr = ival ;
	break ;

case A_f_val:			/* a floating point constant */
	r_ir = FLOD ;
	r_mr = DAT ;
	r_xr = I_F( rval ) ;	/* put it in an integer .... */
	break ;


case A_in_set:
	r_rr = RAWF ;
	frm_loc = *pg_ioff ;
	piv('%') ;
	break ;

case A_in_val:
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
	break ;

case A_in_int:
	pis(sval, 0, F) ;	/* don't really want user doing this, but */
	break ;


case A_in_put:	
	n = ( (r_ir != LOD) ? WB : WA) ;
	ppi(MVI, n, r_mr, r_rr, r_xr) ;
	if (r_ir == LOD) r_ir = NOP ;
	r_wr = WA ;
	r_mr = REG;
	r_rr = TB ;
	break ;

case A_sub_hi:
	hival = ival ; loval = ival ;
	break ;

case A_sub_lo:
	loval = ival ;
	break ;

case A_sub_end:
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
	break ;

/* Finish up the (simple) floating point sequence */
case A_f_done:
	ppi(r_ir, r_wr, r_mr, r_rr, r_xr) ;
	ppi(l_ir, l_wr, l_mr, l_rr, l_xr) ;
	break ;

case A_save_oper:
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
	break ;


case A_cnt_errs:
	break ;



			}/*end of action switch table */

		}/*of while states still parsing */

	}/*of parsing routine*/
