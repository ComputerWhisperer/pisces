/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	preal.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:52		*/



/*************************************************************************
 *									 *
 *   Original : MEL         Stanford University        Sept, 1984	 *
 *									 *
 *     Copyright c 1984 The board of trustees of the Leland Stanford 	 *
 *                      Junior University. All rights reserved.		 *
 *     This subroutine may not be used outside of the SUPREM4 computer	 *
 *     program without the prior written consent of Stanford University. *
 *									 *
 *************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include "expr.h"
#define FALSE 0
#define TRUE  1
int nn;

/************************************************************************
 *									*
 *	parse_real(str, dval) - this routine parses the string into the *
 *  real number dval.  It tries to trap errors before calling sscanf.	*
 *  This routine returns a pointer to the end of the real number, or 	*
 *  NULL if an error occured.						*
 *									*
 *	Original	Mark E. Law		Oct, 1984		*
 *									*
 ************************************************************************/
char *parse_real(str, dval)
char *str;
float *dval;
{
    char *s;
    int leading = FALSE, decimal = FALSE;

    s = str;

    if (s == NULL)
	return(NULL);

    /*skip over leading spaces*/
    while ( isspace( *s ) ) s++;

    /*handle any leading + or -*/
    if (( *s == '+' ) || ( *s == '-' ))
	s++;

    /*check off any leading digits*/
    for ( leading = isdigit( *s ); isdigit( *s ); s++);

    /*following any leading digits can be a decimal place*/
    if ( *s == '.' ) {
	s++;	/*skip over the decimal place*/
	/*skip over any decimal place digits*/
	for ( decimal = isdigit( *s ); isdigit( *s ); s++);
    }

    /*if we do not have either leading digits or decimal digits, error*/
    if ( ! (leading || decimal) ) 
	return( NULL );
    
    /*if the next char is an e, we have an exponential portion*/
    if ( *s == 'e' ) {
	s++;	/*skip over the exponent character*/
	/*skip over the exponent sign, if any*/
	if ( (*s == '+') || (*s == '-') ) s++;
	/*skip over the exponent digits*/
	while ( isdigit( *s ) ) s++;
    }

    sscanf(str, "%e", dval);
    return( s );
}
/*
char *get_solval( )
{
}


char *vfunc( )
{
}
*/

/************************************************************************
 *									*
 *	constants( str, tok ) - This routine parses the string in str	*
 *  to see if it matches any of the constants defined for vector 	*
 *  expressions and real number parsing.				*
 *									*
 *  Original:	MEL	2/85						*
 *									*
 ************************************************************************/
constants( str, tok )
char *str;
struct tok_str *tok;
{
    
    /*sort of a mass case statement*/
    if ( ! strcmp( str, "Kb" ) ) {
	tok->type = RCONST;		tok->value.dval = 8.62e-5;
	return(0);
    }
    return( -1 );
}
    
/*
sol_values( )
{
}



vec_func( )
{
}
*/
