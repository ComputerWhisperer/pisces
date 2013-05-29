/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



#include <stdio.h>
#include <ctype.h>
#include "struct.h"
#include "expr.h"

extern FILE *defaultsfile;
extern FILE *tempfile;
extern FILE *changefile;
extern char *parse_expr();
extern char *eval_real();
extern change; /* 1 if we are in modify mode */
extern tagnumber; /* number of current tag */
extern changetag;  /* tag of next modification */
extern char changebuf[80]; /* stores replacement string */


read_float( str, ans )
char *str;
float *ans;
{
    char buf[512];
    char *err = "";
    struct vec_str *out;
    float val, defaultvalue;
    int i;

    while( err != NULL ) {

	defaultvalue = 0;

	if ( ( changetag == tagnumber ) && ( change == 1 ) )  {
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
	else
	    if (defaultsfile != NULL)  {
	        fgets( buf, 512, defaultsfile );
	        sscanf( buf, "%f", &defaultvalue );
	    }

	printf( "%s [%f] : ", str, defaultvalue );
	fflush( stdout );
	gets(buf);

	/*strip the leading spaces*/
	i=0; while( isspace ( buf[i] ) ) i++;

	/*  if no entry was made, then use the default value */
	if ( buf[i] == '\0' ) {
	    val = defaultvalue;
	    err = NULL;
	}
      	else  {

	    if ( (err = parse_expr( &(buf[i]), &out )) != NULL ) 
	        printf("%s\n", err);
	    else if ( (err = eval_real( out, &val )) != NULL ) 
	        printf("%s\n", err);
	    free_expr( out );
	}
	
    }
    fprintf( tempfile, "%g\t-\t%s\t%d\n", val, str, tagnumber );
    tagnumber++;
    *ans = val;
}
