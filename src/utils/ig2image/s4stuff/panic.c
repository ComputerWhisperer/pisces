

static char SccsID[] = "@(#)panic.c	1.1\t4/25/89";

/*----------------------------------------------------------------------
 *
 * panic - remedial action attempted
 *
 *---------------------------------------------------------------------*/

/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


#include <stdio.h>
#include <stdlib.h>

#ifdef ANSI_FUNC

void
panic (char *s)
#else
char *panic (s)
    char *s;
#endif
{
    
    fprintf(stderr,"suprem4 panic: %s\n",s);
    exit( -1 );
}
