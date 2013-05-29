
/*	panic.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:49		*/


/* panic.c - tells of error conditions */
#include <stdio.h>
#include <stdlib.h>
#define ERROR -1


panic( string )
char *string;
{
    printf( "\t%s\n", string );
    exit( ERROR );
}
