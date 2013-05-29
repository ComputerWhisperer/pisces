/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   %M%                Version %I%     */
/*   Last Modification : %G%  %U% */

#ifdef STATIC_ALLOCATION_TIME
#define EXTERN
#else
#define EXTERN extern
#endif

/*define a maximum depth for the expression parser*/
#define STACK_DEPTH 100

/*define token values for the different type of lexical values*/
#define  EXPR  	0x0001
#define  OP1   	0x0002		/* plus/minus */
#define  OP2   	0x0004		/* multiply/divide */
#define  FN    	0x0008		/* miscellaneous functions */
#define  VFN	0x0010		/* vector valued functions */
#define  LPAR  	0x0020		/* left parenthesis */
#define  RPAR  	0x0040		/* right parenthesis */
#define  RCONST 0x0080		/* real constant value */
#define  SOLVAL 0x0100		/* solution value, As, B, etc. */
#define  TOPSTK 0x0200		/* top of stack value */
#define  EOI    0x0400		/* end of input marker */
#define  OP3    0x0800		/* exponentiation */

/*define numbers for functions*/
#define LOG10	1
#define LOG	2
#define EXP	3
#define ERF	4
#define ERFC	5
#define ABS     6
#define SQRT    7
#define SIGN    8

/*type def a relationship for the value field*/
typedef union {
    float dval;
    int ival;
    struct vec_str *bval;
    } plt_val;

/*define a streucture to hold everything in*/
struct vec_str {
    plt_val value;
    int type;
    struct vec_str *right;
    struct vec_str *left;
    };


/*declare a structure for parsing*/
struct tok_str {
    int type;
    plt_val value;
    };

/*declare the global stack of the values to be handled*/
EXTERN struct tok_str stack[STACK_DEPTH];

/*current stack pointer*/
EXTERN int st_pnt;

/*the next input token read*/
EXTERN struct tok_str input_token;

/*this actually contains the tree used by other programs to set up plot vals*/
EXTERN struct vec_str *sol_exp;

extern char *parse_expr(), *eval_real(), *eval_vec();

