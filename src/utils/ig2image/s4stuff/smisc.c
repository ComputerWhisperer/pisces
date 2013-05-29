

static char SccsID[] = "@(#)smisc.c	1.1\t4/25/89";

/*----------------------------------------------------------------------
 *
 * smisc.c - easier interface to expression parser
 *
 * Copyright c 1985 The board of trustees of the Leland Stanford
 *                  Junior University. All rights reserved.
 * This subroutine may not be used outside of the SUPREM4 computer
 * program without the prior written consent of Stanford University.
 *
 * Original: CSR Thu Jul 16 16:47:28 1987
 *---------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include "global.h"
#include "expr.h"
#include "shell.h"


/*-----------------STRING_TO_REAL---------------------------------------
 * Hide everything from the innocent users
 *----------------------------------------------------------------------*/
float string_to_real( expr, defval)
     char *expr;
     float defval;
{
    char *parse_expr(), *eval_real(), *err, *dexpr;
    struct vec_str *rexp; int len;
    float val;

    /* First make a copy that we can write all over */
    len = 1 + strlen( expr);
    dexpr = (char *) malloc( len);
    strcpy( dexpr, expr);

    /* Expand macros */
    expand_macro( &dexpr, &len, macro);

    /* Try to parse */
    if((err = parse_expr( dexpr, &rexp)) || (err = eval_real( rexp, &val))) {
	fprintf( stderr, "bad expression: %s\n", err);
	val = defval;
    }
    free_expr( rexp);
    free( dexpr);
    return( val);
}

/*-----------------SMACRO, FMACRO, UMACRO-------------------------------
 * Define and undefine macros without including the whole expression parser.
 *----------------------------------------------------------------------*/

smacro( name, def)
     char *name, *def;
{
    char buffer[512];
    sprintf( buffer, "%s %s", name, def);
    define_macro( buffer, &macro);
}

fmacro( name, val, format)
     char *name, *format;
     float val;
{
    char buffer[512], formbuf[512];
    strcpy( formbuf, "%s ");
    strcat( formbuf, format);
    sprintf( buffer, formbuf, name, val);
    define_macro( buffer, &macro);
}

umacro( name)
     char *name;
{
    undef_macro( name, &macro);
}

/* Debug routine because dbx doesn't print the return values of functions
   that are called from the command line */
/*
dbxIS( param, name)
    int param;
    char *name;
{
    int i, j;
    i = is_specified( param, name);
    j = 0;
}
*/
