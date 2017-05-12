/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/
/*	eval.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:35		*/


/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   eval.c                Version 3.2     */
/*   Last Modification : 11/7/87  09:17:54 */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
extern double erf(), erfc(), pow();
#include <ctype.h>
#include "expr.h"


/************************************************************************
 *									*
 *	This file contains the code and definitions for a real		*
 *  number expression evaluator.  The parser is a pseudo LALR job	*
 *  modified to handle this ambiguous grammar.  The grammer rules	*
 *  are :								*
 *	expr  |  expr OP1 expr		OP1 = +,-			*
 *	      |  expr OP2 expr		OP2 = *,/			*
 *	      |  FN expr		FN = implemented functions	*
 *	      |  OP1 expr		unary minus and plus		*
 *	      |  ( expr )		paranthesis			*
 *	      |	 RCONST			real number constant		*
 *	      |  SOLVAL			solution value			*
 *									*
 *  This routine handles the parse tree as developed in reducing this	*
 *  grammar.								*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/


/************************************************************************
 *									*
 *	eval_real( expr, val ) - This routine evaluates the real number *
 *  expr contained in the parse tree expr.  If a vector variable is 	*
 *  found, it generates an error.  The value is returned in val when	*
 *  all is said and done.						*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

char *
eval_real (struct vec_str *expr, float *val)
#else

char *eval_real( expr, val )
struct vec_str *expr;
float *val;
#endif
{
    float lval, rval;
    char *err;

    /*evaluate the kids*/
    if ( expr->left != NULL ) 
	if ( (err = eval_real( expr->left, &lval ) ) != NULL ) return(err);
    if ( expr->right != NULL ) 
	if ( (err = eval_real( expr->right, &rval ) ) != NULL ) return(err);

    /*switch on the type of value contained in the passed argument*/
    switch( expr->type ) {
    case OP1 :	/*plus or minus*/
		/*check the unary case out*/
		if (expr->left == NULL) {
		    if ( expr->value.ival == '+' ) *val = rval;
		    if ( expr->value.ival == '-' ) *val = -rval;
		}
		else {
		    if ( expr->value.ival == '+' ) *val = lval + rval;
		    if ( expr->value.ival == '-' ) *val = lval - rval;
		}
		break;
    case OP2 :  /*multiplication / division*/
		if ( expr->value.ival == '*' ) *val = lval * rval;
		if ( expr->value.ival == '/' ) *val = lval / rval;
		break;
    case OP3 :  /*exponentiation*/
		if ( expr->value.ival == '^' ) *val = pow( lval, rval );
		break;
    case FN  :  /*a function specifier*/
		switch( expr->value.ival ) {
		case LOG10 :	*val = log10( rval );
				break;
		case LOG   :	*val = log( rval );
				break;
		case EXP   :	*val = exp( rval );
				break;
		case ERF   :	*val = (double)erf( rval );
				break;
		case ERFC  :	*val = (double)erfc( rval );
				break;
		case ABS   :	*val = (rval > 0.0)?(rval):(-rval);
				break;
		case SQRT  :	*val = (double)sqrt( rval );
				break;
		case SIGN  :	if ( rval > 0.0 ) *val = 1.0;
				else if ( rval < 0.0 ) *val = -1.0;
				else *val = 0.0;
				break;
		}
		break;
    case RCONST :  *val = expr->value.dval;
		break;
    case VFN	:
    case SOLVAL :  return( "a vector solution value is illegal here\n" );

    }
    return( NULL );
}




/************************************************************************
 *									*
 *	islogexp( expr ) - This routine returns true if the expression	*
 *  starts with a log base 10.  It is used by the plotting software.	*
 *									*
 *  Original:	MEL	12/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
islogexp (struct vec_str *expr)
#else

islogexp( expr )
struct vec_str *expr;
#endif
{
    return( (expr->type == FN) && (expr->value.ival == LOG10) );
}




/************************************************************************
 *									*
 *	free_expr - This is a memory disposer.				*
 *									*
 *  Original:	MEL	8/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

void 
free_expr (struct vec_str *t)
#else

free_expr( t )
struct vec_str *t;
#endif
{
    if ( t == NULL ) return;
    free_expr( t->left );
    free_expr( t->right );
    free(t);
}

