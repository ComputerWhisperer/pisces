
/*	incfil.c		Version 1.4		*/
/*	Last Modification:	3/31/90 09:35:49		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/
/*
 *	incfil.c  - increments a pisces solution filename.  just a C
 *		translation of the incfil.F program.  returns the
 *		incremented filename in the same array as the
 *		input argument.
 *
 *	Goodwin Chin
 *	April 9, 1989
 *
 */

#include <stdio.h>

#ifdef CONVEX
#include <strings.h>
#else
#include <string.h>
#endif


#ifdef ANSI_FUNC

int 
incfil (char *filename)
#else
incfil(filename)
char *filename;
#endif
{
    int index = 80;
    int done;

    /* find last character in filename */
    index = strlen(filename);
    --index;
    done = 0;
    while (!done)  {
	if (filename[index] == 'z')  {
	    filename[index] = '0';
	    --index;
	}
	else if (filename[index] == 'Z')  {
	    filename[index] = 'a';
	    done = 1;
	}
	else if (filename[index] == '9')  {
	    filename[index] = 'A';
	    done = 1;
	}
	else  {
	    filename[index] = filename[index] + 1;
	    done = 1;
	}
    }
    return(0);
}
