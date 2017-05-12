

static char SccsID[] = "@(#)verify.c	1.1\t4/25/89";

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
/*   verify.c                Version 3.3     */
/*   Last Modification : 8/18/88  16:34:41 */

#include <stdio.h>
#include <ctype.h>
#include "global.h"
#include "sysdep.h"
#include "check.h"
extern val_str bool_check();
extern char *parse_expr(), *eval_real();


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
#ifdef ANSI_FUNC

char *
parse_real (char *str, float *dval)
#else

char *parse_real(str, dval)
char *str;
float *dval;
#endif
{
    char *s;
    int leading = FALSE, decimal = FALSE, exp = FALSE;

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


