/* Fri Aug 12 17:46:11 PDT 1988 (dredge--stanford)
 *
 * "pg_dis18.h" : disassembly (assembly) definintions for alu18.
 *  original: MJE (nov 85)
 *  modified: MJE (jul 87) Some compilers don't like incomplete struct
 *	definitions (eg: `` { "" } '' ) Now:  `` { "", 0 } ''
 */


#include "gp_gplot.h"

/* Note that we will undefine EXTERN at the end of this file */
#ifdef COMMON
#  define EXTERN
#else
#  define EXTERN extern
#endif


/* Internal alu-18 definitions (mostly for disassembly printing) */

struct _strint {		/* string pointer/integer structure */
	char   *sv ;
	int	iv ;
	} ;

/* Instructions */
EXTERN struct _strint  i18list[]
#ifdef COMMON
	= {
	{ "nop", NOP } , { "jss", JSS } , { "neg", NEG } , { "itr", ITR } ,
	{ "kpe", KPE } , { "tst", TST } , { "end", END } ,

	{ "lod", LOD } , { "add", ADD } , { "sub", SUB } , { "mul", MUL } ,
	{ "div", DIV } , { "ior", IOR } , { "and", AND } , { "eor", EOR } ,
	{ "sfl", SFL } , { "sfr", SFR } , { "rbt", RBT } ,

	{ "mvo", MVO } , { "mvi", MVI } , 

	{ "flod",FLOD} , { "fstr",FSTR} , { "fadd",FADD} , { "fsub",FSUB} ,
	{ "fmul",FMUL} , { "fdiv",FDIV} , { "fneg",FNEG} , { "fmvo",FMVO} ,
	{ "cvfi",CVFI} , { "cvif",CVIF} ,

	{ "str", STR } , { "spf", SPF } , { "ast", AST } , { "b", BCC   } ,
	{ "cpo", CPO } ,
	{ "" , 0}
	}
#endif
	;

/* Registers */
EXTERN struct _strint  r18list[] 
#ifdef COMMON
	= {
	{ "r0" , R0 } , { "r1" , R1 } , { "r2" , R2 } , { "r3" , R3 } ,
	{ "r4" , R4 } , { "r5" , R5 } , { "r6" , R6 } , { "r7" , R7 } ,
	{ "ra" , TA } , { "rb" , TB } ,
	{ "OB" , OB } , { "BC" , BC } ,
	{ "", 0 }
	}
#endif
	;

/* Variables - integers */
EXTERN struct _strint	iv18list[] 
#ifdef COMMON
	= {
	{ "I"  , IVAR }	, { "J"  , JVAR } , { "C"  , CVAR } , { "S"  , SVAR } ,
	{ "", 0 }
	}
#endif
	;

/* Variables - floats */
EXTERN struct _strint	fv18list[] 
#ifdef COMMON
	= {
	{ "X"  , XVAR }	, { "Y"  , YVAR } ,
	{ "", 0 }
	}
#endif
	;


/* condition codes */
EXTERN struct _strint  cc18lst[] 
#ifdef COMMON
	= {
	{ "f"	, 0		} , { "gt"	, CC_GT			} ,
	{ "eq"	, CC_EQ		} , { "ge"	,(CC_GT|CC_EQ)		} ,
	{ "lt"	, CC_LT		} , { "ne"	,(CC_GT|CC_LT)		} ,
	{ "le"	,(CC_LT|CC_EQ)	} , { "ra"	,(CC_LT|CC_EQ|CC_GT)	} ,
	{ "", 0 }
	}
#endif
	;


/* ALU Intrinsic functions */
EXTERN struct	_strint	 itr18list[] 
#ifdef COMMON
	= { 
	{ "NULL"	, GX_NULL	} , /* Null command */
	{ "POST"	, GX_POST	} , /* Post current buffer */
	{ "P0"		, GX_P0  	} , /* Switch: use Previous points */
	{ "P1"		, GX_P1  	} , /* Use current points */
	{ "READ"	, GX_READ	} , /* Force read from Input device */
	{ "IDEV"	, GX_IDEV  	} , /* Switch output to Input device */
	{ "ODEV"	, GX_ODEV  	} , /* Switch output to Output device */
	{ "_ABORT"	, GX__ABORT  	} , /* Dump state of alu and abort */
	{ "_DUMP"	, GX__DUMP  	} , /* Dump state of alu and continue */
	{ "_TRACE"	, GX__TRACE  	} , /* Enable alu tracing */
	{ "_NOTRACE"	, GX__NOTRACE  	} , /* Disable alu tracing */
	{ "", 0 }
	}
#endif
	;

/* Undefine incase someone else has the same idea */
#undef EXTERN
/* ----------------------------------------------------------------------- */
