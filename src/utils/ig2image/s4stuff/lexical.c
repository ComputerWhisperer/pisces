

static char SccsID[] = "@(#)lexical.c	1.1\t4/25/89";

/*************************************************************************
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/
/*   lexical.c                Version 3.4     */
/*   Last Modification : 8/23/88  10:36:30 */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "constant.h"
#include "global.h"
#include "sysdep.h"
#include "expr.h"
#include "material.h"

/*from verify.c*/
char *parse_real(char *str, float *dval);


/************************************************************************
 *									*
 *	This file contains the code and definitions for a real		*
 *  number expression lexical analyzer.  The lexical analyzer is a 	*
 *  hack at best.							*
 *									*
 *	expr  |  expr OP1 expr		OP1 = +,-			*
 *	      |  expr OP2 expr		OP2 = *,/,^			*
 *	      |  FN expr		FN = implemented functions	*
 *	      |  OP1 expr		unary minus and plus		*
 *	      |  ( expr )		paranthesis			*
 *	      |	 RCONST			real number constant		*
 *	      |  SOLVAL			solution value			*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/




/************************************************************************
 *									*
 *	single_char( s, tok ) - This routine attempts to parse a single	*
 *  character.  The legal single characters are +,-,/,*,(,),^.  If one	*
 *  of these is found, the value in token is set up.			*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
single_char (char *s, struct tok_str *tok)
#else

single_char( s, tok )
char *s;
struct tok_str *tok;
#endif
{
    /*parse this particular character*/
    switch( *s ) {
    case '^' : tok->type = OP3;
	       tok->value.ival = *s;
	       return( 0 );
	       break;
    case ',' : tok->type = COM;
	       tok->value.ival = *s;
	       return( 0 );
	       break;
    case '*' :
    case '/' : tok->type = OP2;
	       tok->value.ival = *s;
	       return( 0 );
	       break;
    case '(' : tok->type = LPAR;
	       tok->value.ival = 0;
               return( 0 );
	       break;
    case ')' : tok->type = RPAR;
	       tok->value.ival = 0;
               return( 0 );
	       break;
    case '+' :
    case '-' : tok->type = OP1;
	       tok->value.ival = *s;
	       return( 0 );
	       break;
    case '\0' : tok->type = EOI;
	       tok->value.ival = 0;
               return( 0 );
	       break;
    default  : break;
    }
    return( -1 );
}



/************************************************************************
 *									*
 *	functions( str, tok ) - This routine parses the string in str	*
 *  to see if it matches any of the functions defined for vector 	*
 *  expressions.							*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
functions (char *str, struct tok_str *tok)
#else

functions( str, tok )
char *str;
struct tok_str *tok;
#endif
{
    char *str1, *str2;
    char *loc;
    int ij;
    int mat1 = -1, mat2 = -1;
    int i, match;
    
    /*sort of a mass case statement*/
    if ( ! strcmp( str, "log10" ) ) {
	tok->type = FN;		tok->value.ival = LOG10;
	return(0);
    }
    else if ( ! strcmp( str, "log" ) ) {
	tok->type = FN;		tok->value.ival = LOG;
	return(0);
    }
    else if ( ! strcmp( str, "exp" ) ) {
	tok->type = FN;		tok->value.ival = EXP;
	return(0);
    }
    else if ( ! strcmp( str, "erf" ) ) {
	tok->type = FN;		tok->value.ival = ERF;
	return(0);
    }
    else if ( ! strcmp( str, "erfc" ) ) {
	tok->type = FN;		tok->value.ival = ERFC;
	return(0);
    }
    else if ( ! strcmp( str, "abs" ) ) {
	tok->type = FN;		tok->value.ival = ABS;
	return(0);
    }
    else if ( ! strcmp( str, "sqrt" ) ) {
	tok->type = FN;		tok->value.ival = SQRT;
	return(0);
    }
    else if ( ! strcmp( str, "sign" ) ) {
	tok->type = FN;		tok->value.ival = SIGN;
	return(0);
    }
    else if ( ! strcmp( str, "xfn" ) ) {
	tok->type = FN;		tok->value.ival = X;
	return(0);
    }
    else if ( ! strcmp( str, "yfn" ) ) {
	tok->type = FN;		tok->value.ival = Y;
	return(0);
    }
    else if ( ! strcmp( str, "zfn" ) ) {
	tok->type = FN;		tok->value.ival = Z;
	return(0);
    }
    else if ( (loc = (char *)strchr( str, '@' )) != NULL ) {

	    /*split the string*/
	    str1 = str;
	    str2 = &(loc[1]);
	    *loc = '\0';

	    /*find the first string*/
	    for(match = -1, i = 0; MatNames[i] != NULL; i++) {
		if ( (*str1 == MatNames[i][1]) ) {
		    ij = substring( &(MatNames[i][1]), str1 );
		    if ( ij > match ) { match = ij; mat1 = i; }
		}
	    }
	    
	    /*do it for the second string*/
	    for(match = -1, i = 0;  MatNames[i] != NULL; i++) {
		if ( (*str2 == MatNames[i][1]) ) {
		    ij = substring( &(MatNames[i][1]), str2 );
		    if ( ij > match ) { match = ij; mat2 = i; }
		}
	    }

	    if ( (mat1 == -1) || (mat2 == -1) || (mat1 == mat2) ) return( -1 );

	    tok->type = FN;
	    tok->value.ival = MAXMAT * (mat1 * MAXMAT + mat2);
	    return(0);
    }
    else return( -1 );
}

    


/************************************************************************
 *									*
 *	lexical( str, tok ) - This routine reads from string a single 	*
 *  token and stores it in the input token location.			*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
#ifdef ANSI_FUNC

int 
lexical (char *str, struct tok_str *tok)
#else

lexical( str, tok )
char *str;
struct tok_str *tok;
#endif
{
    char *s, *t;
    char tmp[256];
    int i;

    /*if string is null, return error*/
    if ( ! str ) return( -1 );

    /*skip any leading white space*/
    for(s = str; *s && isspace( *s ); s++);

    /*parse this particular character*/
    if (single_char( s, tok ) == 0) {
	strcpy(str, s + 1);
	return( 0 );
    }

    /*it is getting harder*/
    if ( (t = (char *)parse_real(s, &(tok->value.dval))) != NULL )  {
	tok->type = RCONST;
	strcpy(str, t);
	return( 0 );
    }

    /*locate the end of the function/special string*/
    for(i = 0; strchr("()*-^/+ \t", *s) == NULL; tmp[i] = *s, s++, i++);
    tmp[i] = '\0';

    if ( functions( tmp, tok ) == 0 ) {
	strcpy(str, s);
	return( 0 );
    }
    if ( constants( tmp, tok ) == 0 ) {
	strcpy(str, s);
	return( 0 );
    }
    if ( vec_func( tmp, tok ) == 0 ) {
	strcpy(str, s);
	return( 0 );
    }
    if ( sol_values( tmp, tok ) == 0 ) {
	strcpy(str, s);
	return( 0 );
    }
    else 
	return( -1 );
}


