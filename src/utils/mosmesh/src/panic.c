
/* panic.c - tells of error conditions */
#include <stdio.h>
#include <stdlib.h>
#define ERROR -1


#ifdef ANSI_FUNC

void 
panic (char *string)
#else
panic( string )
char *string;
#endif
{
    printf( "\t%s\n", string );
    exit( ERROR );
}
