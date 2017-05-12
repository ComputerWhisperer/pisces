/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	read_deffloat.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:53		*/




#include <stdio.h>
#include <ctype.h>
#include "struct.h"
#include "expr.h"


extern FILE *defaultsfile;
extern FILE *tempfile;
extern FILE *changefile;
extern char *parse_expr();
extern char *eval_real();
extern change; /* 1 if we are in change mode */
extern tagnumber; /* number of current tag */
extern changetag; /* tag of next modification */
extern char changebuf[80]; /* stores replacement string */

#ifdef ANSI_FUNC

int 
read_deffloat (char *str, float *ans)
#else

read_deffloat( str, ans )
char *str;
float *ans;
#endif
{
    char buf[512];
    char *err = "";
    struct vec_str *out;
    float defaultvalue = 0.0;

    if (defaultsfile != NULL)  {
	if ( ( changetag == tagnumber ) && ( change == 1 ) ) {
	    /* flush entry from defaults file */
	    fgets( buf, 512, defaultsfile );
	    if ( (err = parse_expr(&(changebuf[0]), &out) ) != NULL )
		printf( "%s\n", err );
	    else if ( (err = eval_real(out, &defaultvalue) ) != NULL )
		printf( "%s\n", err );
	    free_expr( out );
	    fgets( buf, 512, changefile );
	    sscanf( buf, "%d %s", &changetag, changebuf );
	}
	else  {
	    fgets( buf, 512, defaultsfile );
	    sscanf( buf, "%f", &defaultvalue );
	}
	fprintf( tempfile, "%g\t-\t%s\t%d\n", defaultvalue, str, tagnumber );
	tagnumber++;
	*ans = defaultvalue;
    }
    else  {
	fprintf( stderr, "\tmust have a default file for batch mode\n" );
	exit( ERROR );
    }
}
