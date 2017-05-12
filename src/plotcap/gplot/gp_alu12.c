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
 * Thu Apr  5 09:46:37 PDT 1990 (dredge--stanford)
 *
 * "alu12" : Well here we are again.  This is our little state machine that
 *	is supposed to be very simple, but has gotten out of hand once more.
 *
 * written: Michael Eldredge (jan 85)
 * mod # 1: Michael Eldredge (nov 85) clean ups....
 * mod # 2: Michael Eldredge (may 86) Convert to use alu18 compiler, etc.
 *	In preparation for writting alu18().
 * mod # 3: Michael Eldredge (may 86) Add some simple floating point support.
 * mod # 4: MJE (feb 87) Try to quite lint.  Added union ut stuff to input/
 *	output RAWI values.
 * mod # 5: MJE (apr 87) ``#if defined(X) || defined(Y) '' makes some
 *	compilers very unhappy. Fixed to be simple ``#ifdef''
 * modified: MJE (sep 88) For coping out raw bytes, we used strncpy().
 *	However, on some systems (e.g. Suns) strncpy() stops coping
 *	at a null instead of blindly coping all N bytes.  Anyway, the
 *	fix is to use bcopy() to copy the raw bytes.  This should have
 *	been done in the first place.  But beware, some implementations
 *	of bcopy() may bad (e.g. early Sun 4 versions had some problems).
 * modified: Ken Kosai -- Huges Santa Barbara Research
 *	include <ctype.h> was include "ctype.h"
 */

#ifdef HEAVY_DEBUG
#   ifndef ALU_SIM
#       undef  HEAVY_DEBUG
#   endif
#endif

#include "auxfns.h"
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include "al_alu18.h"
#include "gp_gplot.h"
#include "gp_mis12.h"

/* DEBUG */
#ifndef DEBUG
static
#endif
	int	 times_in = 0 ;


#define	 W_IDEV	1
#define  W_ODEV	2


static	int	*alu_prog = (int*)0 ;	/* Program/code to run */
static	char	*alu_idat = (char*)0 ;	/* Input data for program */

static	char	*alu_obuf = (char*)0 ;	/* Output buffer */
static	char	*alu_opnt ;		/*   Current pointer in output buf */
static	char	*alu_omax ;		/*   &obuf[SIZE]		*/

static	char	*alu_ibuf = (char*)0 ;	/* Input buffer */
static	char	*alu_ipnt ;		/*   Current pointer in input buf */
static	char	*alu_imax ;		/*   &ibuf[SIZE]  */

static	int	 alu_lupi = 0 ;		/* Input  device LU (put to) */
static	int	 alu_lugi = 0 ;		/* Input  device LU (get from) */
static	int	 alu_lupo = 1 ;		/* Output device LU (put to) */
static	int	 alu_lugo = 1 ;		/* Output device LU (get from) */

static	int	*alu_luput= &alu_lupo ;	/* Current `put' lu */
static	int	*alu_luget= &alu_lugo ;	/* Current `get' lu */
static	int	 alu_curd = W_ODEV ;	/* Current device (input | output) */


static	long	 alu_obc = 0 ;		/* Output byte counter */

static	int	 alu_ivar1[MAXIVARS] ;	/* Current  integer values */
static	int	 alu_ivar0[MAXIVARS] ;	/* Previous integer values */
static	int	*alu_ivars ;		/*  Which of the above are we using */

static	float	 alu_fvar1[MAXFVARS] ;	/* Current  float   values */
static	float	 alu_fvar0[MAXFVARS] ;	/* Current  float   values */
static	float	*alu_fvars ;		/*  Which of the above are we using */


static   int     regs[MAXREGS] ;	/* General Purpose registers */

static	 int	 alu_cc ;		/* Condition codes */

static	 int	*rettab[MAXRET] ;	/* return from JSS table */
static	 int	 retcur = 0 ;		/* current return pointer */

#define PUSH_RET(L)	rettab[ retcur++ ] = (L)
#define POP_RET()	rettab[ --retcur ]

#define	NILPC	(int *)0
static	int	*working = NILPC ;	/* in the middle of jss.* */
static	int	 retlim  = 0 ;
/* retlim is how far down the return stack we need to get before leaving
 *  aluXX.  Thus in JSS.* sequences, we set retlim to the current stack
 *  location so that we leave aluXX after each run through the JSS seq.
 *  Finally, when we are finished with the JSS seq (when last_ent != ent)
 *  we set retlim back down again to finish from whence we came.
 */

static	int	 assert   = 0 ;		/* Assert space in OB */
static	Bool	 dirty    = F ;		/* In assertion, Postable buffer? */

static	int	*nextpc   = NILPC ;
static	int	 last_ent = NOENT ;

/* #if defined(ALU_SIM) || defined(DEBUG)  */
#ifdef ALU_SIM
#  define NEED_PC
#endif
#ifdef DEBUG
#  ifndef NEED_PC
#    define NEED_PC
#  endif
#endif

#ifdef NEED_PC
#   define PCOFF(PC)	((int)(PC - alu_prog))
#endif

#ifdef DEBUG
   static	int	 trace = F ;	/* Run in trace mode */
#endif

static int al_outs(char *str);
static int al_outi(int ival, int nbyte);
static int al_geti(int nbyte);
static int al_gtia(char	*frm);
static int al_atoi(char **p, int b, int m);
static int al_post();
static int al_revb(int val, int n);
static void AL_dump(int *curpc);

/* "alu12" : if "ent" is < 0 then a special alu command (ie: POST) */
#ifdef ANSI_FUNC

void alu12(int ent)
#else

int 
alu12 (ent)
int ent;
#endif
	{

	register  int  *pc, ir, nwd, rr, wr, mr ;
	int	n, val ;
	int	ityp ;		/* instruction type */
	long	nl ;

	Bool	 room ;
	char	*sp , tmpbuf[100] ;
	char	*loc ;
#ifdef DEBUG
	int	 nir ;
#endif
#ifdef NEED_PC
	int	 plc ;
#endif

	/* ---- start alu12 ---- */
	times_in++ ;

	/* may have special contol commands for the alu */
	if (ent <= 0) {		/* control command */
		switch (ent) {
		case ALU_RESET:
#ifdef ALU_SIM
			printf("alu: simulator   ON\n") ;
#endif
			retcur  = 0 ;
			working = NILPC ;
			retlim  = 0 ;
			nextpc  = NILPC ;
			last_ent= NOENT ;
			
			break ;

		/* post: If anything in output buffer, post it */
		case ALU_POST:
			al_post() ;
			break ;

		case ALU_NULL:
			last_ent = ent ;
		/*	if (working) goto workout ;	/* Finish old one */
			break ;

		case -11:
			for (ent = 0; ent < 10 ;) 
				printf("r%d %d\n" , ent, regs[ent]) ;
			break ;

		default:	/* Take unknowns as NOENTs */
			break ;
			}/* of switch on control commands */

		last_ent = ent ;
		return ;
		}/*of if control command */

	/* May be working on a JSS.* sequence. Continue or Finish it */
/*workout:*/
	if (working) {	
		room = ( !assert || (assert < (int)(alu_omax - alu_opnt)) ) ;

		if (last_ent == ent && room) pc = working ;	/* still */
		else {
			pc = POP_RET() ;		/* return point */
			working = NILPC ;		/* no more work */
			retlim  = 0 ;
			assert  = 0 ;
			if (!room) dirty = T ;

			nextpc = (ent > 0 ? &alu_prog[ ent ] : NILPC) ;
			}
		}

	else pc = &alu_prog[ ent ] ;	/* Normal situation */

	last_ent = ent ;

#ifdef DEBUG
	trace = (alu_prog[0] & HEAD_TRACE) ;
#endif

	for (;;) {
#ifdef DEBUG
		if (trace) {
#endif
#ifdef NEED_PC
		plc = PCOFF(pc) ;
		dis18i(stdout, alu_prog, &plc, alu_idat) ;
#endif
#ifdef DEBUG
		}
#endif
		ir = gti( (nwd = *pc++) ) ;	/* Get Instruction */

		ityp = irtype(ir) ;
		switch (ityp) {

		/* NORMal instructins ------------------------------ */
		case IT_NORM :

			wr = (gtw(nwd) == WA ? TA : TB) ;
			mr =  gtm(nwd) ;

			switch (mr) {
			case REG: 
				if ((rr = gtr(nwd)) < PSUEDO_REGS) {
					val = regs[ rr ] ;
					}
				else if (rr == OB)
					val = (int)(alu_opnt-alu_obuf);
				else if (rr == BC) 
#ifndef lint
					val = alu_obc ;
#else
					nl  = alu_obc ;
#endif
				break ;
			case VAR: val = alu_ivars[ gtr(nwd) ] ; break ;
			case DAT: val = *pc++ ;			break ;
			default:
				fprintf(stderr,"alu:NORM:unkn mr\n");
				break;
				}
			switch (ir) {
			case LOD: regs[ wr ]  =  val ;   break ;
			case ADD: regs[ wr ] +=  val ;   break ;
			case AND: regs[ wr ] &=  val ;   break ;
			case IOR: regs[ wr ] |=  val ;   break ;
			case SFR: regs[ wr ] >>= val ;   break ;
			case SFL: regs[ wr ] <<= val ;   break ;
			case SUB: regs[ wr ] -=  val ;   break ;
			case MUL: regs[ wr ] *=  val ;   break ;
			case DIV: regs[ wr ] /=  val ;   break ;
			case EOR: regs[ wr ] ^=  val ;   break ;
			case RBT: regs[ wr ] = al_revb(regs[wr], val);break;

			case FLOD:
#ifndef lint
				if (mr == VAR) val = I_F(alu_fvars[gtr(nwd)]) ;
#else
				if (mr == VAR) val = alu_fvars[gtr(nwd)] ;
#endif
				regs[ wr ]  =  val ;
				break ;
			default:
				fprintf(stderr,"alu12: Unknown Norm ir\n") ;
				fprintf(stderr,"\tir %d\n" , ir) ;
				break ;
				}/*of switch on ir for NORM */

			break ;  /*of NORM istructs */

		/* One word instructions ------------------------------ */
		case IT_ONE:	/* Handle sub-ir's individually */

			switch (ir) {
			case JSS:
				PUSH_RET(pc) ;		/* save pc */
				pc = &alu_prog[ alu_prog[ gtr(nwd) ] ] ;

				if (gtw(nwd) == JSS_STAR) {
					working = pc ;     /* on ourself */
					retlim  = retcur ;
					}
				break ;/*JSS */

			case END:
				if (retcur == retlim) {
					if (!(pc = nextpc)) goto at_end ;
					nextpc = NILPC ;
					}

				else pc = POP_RET() ;
				break ;
			
			case TST:		/* Test a value */
				mr = gtm(nwd) ;
				rr = gtr(nwd) ;
				switch (mr) {
				case REG:  val = regs[rr] ;      break ;
				case VAR:  val = alu_ivars[rr];  break ;
				/*default: error..... */
					}

				alu_cc = 0 ;
				if      (val <  0) alu_cc = CC_LT ;
				else if (val == 0) alu_cc = CC_EQ ;
				else if (val >  0) alu_cc = CC_GT ;
				break ;

			case ITR:		/* alu intrinsic */
				/* NOTE: alu18 has GX_xxx defined as small
				 *  negative numbers. The intrinsic number
				 *  in alu12 (us) is stored as a possitive
				 *  number.  So, to do the compare we need
				 *  the negative.... mje08may86
				 */
				rr = -gtr(nwd) ;
				switch ( rr ) {

				case GX_P0:	/* Set point flag 0 */
					alu_ivars = alu_ivar0 ;
					alu_fvars = alu_fvar0 ;
					break ;

				case GX_P1:	/* Set point flag 1 */
					alu_ivars = alu_ivar1 ;
					alu_fvars = alu_fvar1 ;
					break ;

				case GX_POST:
					if (alu_opnt > alu_obuf) al_post() ;
					break ;

				case GX_READ:
					al_post() ;	/* All out */
					n = read(*alu_luget, alu_ibuf, 100) ;
					alu_imax = &alu_ibuf[n] ;
					alu_ipnt =  alu_ibuf ;
					break ;

				case GX_ODEV:		/*Writes to odev now*/
					if (alu_curd != W_ODEV) {
						al_post() ;
						alu_curd  = W_ODEV ;
						alu_luput = &alu_lupo ;
						alu_luget = &alu_lugo ;
						}
					break ;

				case GX_IDEV:		/*Writes to idev now*/
					if (alu_curd != W_IDEV) {
						al_post() ;
						alu_curd  = W_IDEV ;
						alu_luput = &alu_lupi ;
						alu_luget = &alu_lugi ;
						}
					break ;

				case GX__TRACE:	/* Allow tracing from*/
				case GX__NOTRACE:	/*  the plotcap file */
#ifdef DEBUG
					trace = (rr == GX__TRACE) ;
#endif
					break ;

				case GX__ABORT:
				case GX__DUMP:
					printf("\n\n\tAlu12 Dumping...\n") ;
					AL_dump(pc) ;

					if (rr == GX__ABORT) {
						printf("\n\tAlu12 Abort.\n");
						sleep(1);
						abort() ;
						}
					printf("\n") ;
					break ;

					}/*switch on intrins */

				break ;  /*intrins ir */

			case KPE:	/* End of keeping */
				if (dirty) al_post() ;
				dirty = F ;
				break ;
					

			case NOP:
				break ;		/* a NOP */
#ifdef DEBUG
			default:
				fprintf(stderr,"alu12: Unknown One ir\n") ;
				fprintf(stderr,"\tir %d\n" , ir) ;
				break ;
#endif
				}/*of switch on ONE ir's */

			break ; /*of case type ONE */
		
		/* Misc Instructions -------------------------------------- */
		case IT_MISC :
			mr = gtm(nwd) ;
			rr = gtr(nwd) ;
			wr = (gtw(nwd) == WA ? TA : TB) ;

			switch (ir) {
			case STR:
				switch (mr) {
				case REG:
					if (rr < PSUEDO_REGS)
						regs[rr] = regs[wr] ;
					else if (rr == BC)
						alu_obc = regs[wr] ;
					else {
			fprintf(stderr,"alu: write-only reg r%d\n", rr) ;
						}
					break ;
				case VAR:
					alu_ivars[rr] = regs[wr] ;
					break ;
					}
				break ;/* STR */

			case FSTR:
				switch (mr) {
				case REG:
					if (rr < PSUEDO_REGS)
						regs[rr] = regs[wr] ;
					else if (rr = BC)
						alu_obc = regs[wr] ;
					else {
			fprintf(stderr,"alu: write-only (f)reg r%d\n", rr) ;
						}
					break ;
				case VAR:
					/*NOSTRICT*/
					alu_fvars[rr] = F_I(regs[wr]) ;
					break ;
					}
				break;/* FSTR */

			case CPO:		/* CPO cnt pnt */
				if (!(n = rr)) n = *pc++ ;    /* count */
				sp = &alu_idat[ *pc++ ] ;   /* data pointer */
				al_outb(sp, n) ;
				break ;/* CPO */

			case BCC:		/* Conditional branch */
				n = *pc++ ;	/* rr has CC to check */
				if (rr & alu_cc) pc = &alu_prog[n] ;
				break ;


			/* Assert output space */
			case AST:
				/* room = alu_omax - alu_opnt */
				n = *pc ;	  		  /* TRASH */
				rr= (int)(alu_omax - alu_opnt) ;  /* TRASH */
				if ( *pc++ >= (int)(alu_omax - alu_opnt) ) {
					al_post() ;
					}

				assert = *pc++ ;	/* Working asst */
				dirty  = F ;
				break ;

#ifdef DEBUG
			default:
				fprintf(stderr,"alu12: Unknown Misc ir\n") ;
				fprintf(stderr,"\tir %d, wd %d (0x%x)\n" , 
					nir, nwd, nwd) ;
				break ;
#endif
				}/*of switch on MISC ir's */
			break ;	/*of MISC */


		/* I/O Instructions --------------------------------------- */
		case IT_IO :
			wr = (gtw(nwd) == WA ? TA : TB) ;
			rr = gtior(nwd) ;
			
			switch (ir) {
			case MVO:
			case FMVO:
				val = regs[ wr ]  ;
				loc = (char *)0 ;
				if (nwd & IIO)  {
					nl = alu_obc ;	/* remember byte cnt*/
					loc = alu_opnt ;
					alu_opnt = &alu_obuf[ regs[ rr ] ] ;
#ifdef DEBUG
					/* Error checking.... */
		if (alu_opnt < alu_obuf)
			printf("@@@@ alu_opnt too low @@@@\n");
		if (alu_opnt >= loc) {
			printf("@@@@ alu_opnt too high (rr %d)@@@@\n", rr);
			AL_dump(pc) ;
			}
#endif
					}

				if (! (nwd & ASC)) {		/* binary? */
					(void)al_outi(val, gtiof(nwd)) ;
					}

				else {				/* Ascii? */
					sp = &alu_idat[ *pc++ ] ;
					if (ir == MVO)
						sprintf(tmpbuf, sp, val);
					else/*if ir == FMVO then*/
						sprintf(tmpbuf, sp, F_I(val));
					(void) al_outs(tmpbuf) ;
					}

				if (loc) {
					alu_opnt = loc ;
					alu_obc   = nl   ;  /* restore bc */
					}

				break ;	/*of MVO/FMVO */


			case MVI:
				if (!(nwd & ASC)) {
					val = al_geti(gtiof(nwd) ) ;
					}
				else {
					val = al_gtia( &alu_idat[*pc++] ) ;
					}

				regs[ wr ] = val ;
				break ; /*of MVI */

			default:
				fprintf(stderr,"alu12: Unknown I/O ir\n") ;
				fprintf(stderr,"\tir %d\n" , ir) ;
				break ;
				}/*of switch on I/O ir's */

			}/* of switch on instruction type */

		}/* of for running section */

	/* END will jump here if we are finished. Should be something like
	 *  end sets a done flag and we go: while (!done) { stuff }, but for
	 *   speed we don't want to pop out of switches and go to the bottom
	 *    of the loop, then the top, then test done, then goto at_end
	 */
at_end:

	return ;
	}/* of alu routine */



/* ALU create buffer space routines */

/* "alu_mkp" : malloc and init program array */
#ifdef ANSI_FUNC

int *alu_mkp(int progsz)
#else

int *
alu_mkp (progsz)
int progsz;
#endif
	{

	if (progsz > 0) {
		alu_prog = (int *)malloc((unsigned)(sizeof(int) * progsz)) ;
		}

	else if (progsz == 0) {		/* free the space */
		if (alu_prog) free( (char *)alu_prog ) ;
		alu_prog = (int *)0 ;
		}

	return( alu_prog ) ;
	}

/* "alu_mkb" : malloc and init a character buffer */
#ifdef ANSI_FUNC

char *
alu_mkb (int buftyp, int bufsiz)
#else

char *
alu_mkb(buftyp, bufsiz)
	int	 buftyp, bufsiz ;
#endif
	{
	char	**bp, **mp, **pp ;
	char    *junk = (char*)0 ;

	switch (buftyp) {		/* what type to malloc up??? */
	case MKB_OBUF:				/* an output buffer */
		bp = &alu_obuf ;
		mp = &alu_omax ;
		pp = &alu_opnt ;
		break ;
	case MKB_IBUF:				/* an output buffer */
		bp = &alu_ibuf ;
		mp = &alu_imax ;
		pp = &alu_ipnt ;
		break ;
	case MKB_IDAT:				/* an input  buffer */
		bp = &alu_idat ;
		mp = &junk     ;
		pp = &junk     ;
		break ;
	default:
		break ;
		}

	/* If size is greater than zero then make the buffer */
	if (bufsiz > 0) {
		*bp = malloc(bufsiz) ;		/* buffer pointer */
		*pp = *bp ; 			/* location pointer */
		*mp = *bp + bufsiz ;		/* max location pointer */
		}

	/* Else if size is Zero return the space */
	else {
		if (*bp) {
			free( *bp ) ;
			*bp = (char *)0 ;
			}
		}

	return( *bp ) ;
	}


/* ALU OUTPUT routines...  Uses a whole bunch of specific ones ... */

/* "al_outb" : Add a buffer to the output buffer. */
#ifdef ANSI_FUNC

int al_outb(char *buf, int len)
#else

int 
al_outb (buf, len)
char *buf;
int len;
#endif
	{
	while (len-- > 0) {
		if (alu_opnt == alu_omax) al_post() ;
		*alu_opnt++ = *buf++ ;
		alu_obc++ ;
		}
	}

/* "al_outs" : Add a null terminated string to the output buffer */
#ifdef ANSI_FUNC

static int al_outs(char *str)
#else

static int 
al_outs (str)
char *str;
#endif
	{
	while (*str) {
		if (alu_opnt == alu_omax) al_post() ;
		*alu_opnt++ = *str++ ;
		alu_obc++ ;
		}
	}

#ifdef NEEDED_SOMEDAY
/* "al_outc" : Add a single character to the output buffer */
#ifdef ANSI_FUNC

static int al_outc(char c)
#else

static int 
al_outc (c)
char c;
#endif
	{
	if (alu_opnt == alu_omax) al_post() ;
	*alu_opnt++ = c ;
	alu_obc++ ;
	}
#endif /*NEEDED_SOMEDAY*/

/* "al_outi" : Output from an integer a byte, word, or long */
#ifdef ANSI_FUNC

static int al_outi(int ival, int nbyte)
#else

static int 
al_outi (ival, nbyte)
int ival;
int nbyte;
#endif
/* Nbyte is format */
	{
	char *bp ;
	union {
		char	ubuf[RAWI_BYTES] ;
		int	uint ;
		} ut ;

	if ( (alu_opnt + RAWI_BYTES) > alu_omax ) al_post() ;

	if ((nbyte = ftob(nbyte))) {
		alu_opnt = alu_opnt + nbyte ;
		bp       = alu_opnt ;
		alu_obc  += nbyte ;
		while(--nbyte >=0) {
			*--bp  = (char) (ival & 0377) ;
			ival >>= 8;                  /* get next in low byte */
			}
		}

	else {
#ifndef Old_way
		ut.uint = ival ;
		bcopy(ut.ubuf, alu_opnt, RAWI_BYTES) ;
		alu_opnt += RAWI_BYTES ;
		alu_obc  += RAWI_BYTES ;
#else
		*(int *)alu_opnt = ival;
		alu_opnt += RAWI_BYTES ;
		alu_obc  += RAWI_BYTES ;
#endif
		}

	}


/* "al_geti" : Input to an integer as byte, word, or long */
/* Nbyte is format */
#ifdef ANSI_FUNC

static int al_geti(int nbyte)
#else

static int 
al_geti (nbyte)
int nbyte;
#endif
	{
	register int ival = 0 ; 
	union {
		char	 ubuf[RAWI_BYTES] ;	/* raw tmp buffer */
		int	 uint ;
		} ut ;

	if ((nbyte = ftob(nbyte))) {
		while(--nbyte >= 0)
			ival = (ival << 8) | (*alu_ipnt++ & 0377) ;
		}

	else {
#ifndef Old_way
		bcopy(alu_ipnt, ut.ubuf, RAWI_BYTES) ;
		ival = ut.uint ;
		alu_ipnt += RAWI_BYTES ;
#else
		ival = *(int *)alu_ipnt ;
		alu_ipnt += RAWI_BYTES ;
#endif
		}
	return(ival) ;
	}

/* "al_gtia" : like al_geti (get an integer from the input) but we are given
 *	an ascii format.
 */
/* field can be any length */
#define VARIED	10000

#ifdef ANSI_FUNC

static int al_gtia(char	*frm)
#else

static int 
al_gtia (frm)
char *frm;
#endif
	{
	int	 n ;
	int	 base = 10 , mx = (int)(alu_imax - alu_ipnt) ;

	/* <%><length><base>  get the <length> */
	if (isdigit(*++frm)) {
		n = al_atoi(&frm, 10, VARIED) ;
		if (n > 0 && n < mx) mx = n ;
		}

	switch(*frm) {		/* get the <base> */
	case 'o':	base = 8  ; break ;
	case 'x':	base = 16 ; break ;
		}

	while (alu_ipnt != alu_imax && !isdigit(*alu_ipnt)) alu_ipnt++ ;

	n = al_atoi(&alu_ipnt, base, mx) ;
	return(n) ;
	}

/* base and max len */
#ifdef ANSI_FUNC

static int al_atoi(char **p, int b, int m)
#else

static int 
al_atoi (p, b, m)
char **p;
int b;
int m;
#endif
	{
	int	 v = 0 ;	/* value to return */
		
	while (m-- && isdigit( **p ) ) {
		v *= b ;
		v += *(*p)++ - '0' ;
		}

	return(v) ;
	}


/* "al_post" : Post the output buffer to the output device */
#ifdef ANSI_FUNC

static int 
al_post (void)
#else

static int al_post()
#endif
	{
	register  int  len = (int)(alu_opnt - alu_obuf) ;

#ifndef ALU_SIM
	if (len > 0) write(*alu_luput , alu_obuf, len ) ;
#else
	int  m, l ;
	m = (int)(alu_omax - alu_obuf) ;
	l = (int)(alu_opnt - alu_obuf) ;
	fprintf(fprrr, ">>>> post: len %d, max %d\n", l, m) ;
#endif /* ALU_SIM*/

	alu_opnt = alu_obuf ;
	}

/* "al_revb" : Reverse the lower N bits in an int */
#ifdef ANSI_FUNC

static int al_revb(int val, int n)
#else

static int 
al_revb (val, n)
int val;
int n;
#endif
	{
	int	t_bit, r_bit = 1 , temp = 0 ;
			   
	t_bit = 1 << (n-1) ;		/* highest bit to switch */

	while (n--) {
		if (val & r_bit) temp |= t_bit;
		t_bit >>= 1;
		r_bit <<= 1;
		}
	return temp;
	}

/* "alu_2old" : Make the current point values the old (previous) ones */
#ifdef ANSI_FUNC

int 
alu_2old (void)
#else

int alu_2old()
#endif
	{
	int	i ;

	for (i = MAXIVARS ; i-- ; ) alu_ivar0[i] = alu_ivar1[i] ;
	for (i = MAXFVARS ; i-- ; ) alu_fvar0[i] = alu_fvar1[i] ;
	}

/* "alu_givl" : Give pointers to the alu's value locations */
#ifdef ANSI_FUNC

int alu_givl(int **iv1, int **iv0, float **fv1, float **fv0)
#else

int 
alu_givl (iv1, iv0, fv1, fv0)
int **iv1;
int **iv0;
float **fv1;
float **fv0;
#endif
	{
	*iv1 = alu_ivar1 ;
	*iv0 = alu_ivar0 ;
	*fv1 = alu_fvar1 ;
	*fv0 = alu_fvar0 ;

	/* NOT the place for this.  Should be at alu12(ALU_INIT) ; */
	alu_ivars = alu_ivar1 ;
	alu_fvars = alu_fvar1 ;
	}

/* "al_poki" : Poke integers into alu variables */
#ifdef ANSI_FUNC

int al_poki(int cmd, int ival)
#else

int 
al_poki (cmd, ival)
int cmd;
int ival;
#endif
	{

	switch (cmd) {
	case AP_LUPTO:	alu_lupo = ival ;	break ;	/* put to odev */
	case AP_LUGTO:	alu_lugo = ival ;	break ;	/* get from odev */
	case AP_LUPTI:	alu_lupi = ival ;	break ;	/* put to idev */
	case AP_LUGTI:	alu_lugi = ival ;	break ; /* get from idev */
		}
	}

/* "AL_dump" : Dump out the current alu state information */
#ifdef ANSI_FUNC

static void AL_dump(int *curpc)
#else

static void 
AL_dump (curpc)
int *curpc;
#endif
 
	{
	int	 n , i ;
	char	*cp , buf[20];

	switch (alu_cc) {
	case CC_LT: cp = "le" ;	break ;
	case CC_EQ: cp = "eq" ;	break ;
	case CC_GT: cp = "gt" ;	break ;
	default: sprintf(buf,"#0%o", alu_cc) ; cp = buf ; break ;
		}

	printf("PC %d\tCC %s\tcalled %d\n" ,
		(int)(curpc - alu_prog), cp, times_in);

	n = (working? (int)(working-alu_prog) : -1 ) ;
	printf(" retlim %d, working %d\n", retlim, n ) ;
	printf(" Return stack: ") ;
	for (n = 0 ; n < retcur; n++) {
		if (! (n % 4)) printf("\n") ;
		i = (int)(rettab[n] - alu_prog) ;
		printf("\t%d", i );
		}
	printf("\n") ;

	n = (int)(alu_ipnt - alu_ibuf) ;
	printf(" Ibuf (%db): \"", n ) ;
	prstr(stdout, alu_ibuf, n, 70 ) ;
	printf("\"\n") ;

	printf(" out_bc %ld\n", alu_obc ) ;
	n = (int)(alu_opnt - alu_obuf) ;
	printf(" Obuf (%db): \"", n );
	prstr(stdout, alu_obuf, n, 70 );
	printf("\"\n") ;

	for (n = 0 ; n < MAXREGS;n++) {
		if (! (n % 4)) printf("\n") ;
		printf("r%d %-10d ",n,regs[n]);
		}
	printf("\n") ;

	for (n = 0 ; n < MAXIVARS;n++) {
		if (! (n % 4)) printf("\n") ;
		printf("i%d %-10d ",n,alu_ivars[n]);
		}
	printf("\n") ;
	fflush(stdout) ;
	}
