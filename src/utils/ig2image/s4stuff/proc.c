

static char SccsID[] = "@(#)proc.c	1.1\t4/25/89";

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
/*   proc.c                Version 3.1     */
/*   Last Modification : 9/14/87  10:31:41 */

#include <stdio.h>
#include "shell.h"

/*here be stuff that should live in .h some day*/
#define FALSE 0
#define TRUE 1
/************************************************************************
 *									*
 *	substring(s, ss) - tests if ss is a substring of s. If it is,   *
 *	the number of characters in common is returned.			*
 *	It is intended to replace compare, which requires a comparison  *
 *      with strlen(ss) every time it is used.				*
 *									*
 *	Original	Conor S. Rafferty	Oct, 1986		*
 *									*
 ************************************************************************/
substring(s, ss)
    char *s, *ss;
{
    char *ass;

    for (ass=ss; (*s == *ss) && *ss; s++, ss++) ;

    if (!*ss) return (ss-ass);
    return (0);
}
