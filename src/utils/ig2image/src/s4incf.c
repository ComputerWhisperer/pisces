/*	s4incf.c		Version 1.4		*/
/*	Last Modification:	3/31/90 09:36:13		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/




/*	s4incf.c - increments a filename in a manner similar to the one used
 * 		in Suprem4.
 *	Goodwin Chin
 *	April 9, 1989
 */

#include <stdlib.h>
#include <stdio.h>

#ifdef CONVEX
#include <strings.h>
#else
#include <string.h>
#endif


#define ERROR -1


s4incf(filename)
char *filename;
{
    int extension;
    int index;
    int i;
    int j;
    char ext[3];

    /* strip off 3 digit extension */
    index = strlen(filename);
    j = 0;
    for (i = index-3; i < index; i++, j++)  
	ext[j] = filename[i];

    /* remove extension number from filename.  add it back after
     *   incrementing via the strcpy command
     */
    filename[index-3] = '\0';

    if ( (ext[0] < '0') || (ext[0] > '9') ||
	 	(ext[1] < '0') || (ext[1] > '9') ||
	 	(ext[2] < '0') || (ext[1] > '9') )  {
	fprintf(stderr, "illegal format for data file\n");
	fprintf(stderr, "format is filennn, where n is a digit\n");
	exit(ERROR);
    }
     
    sscanf(ext, "%d", &extension);

    if (extension >= 999)  {
	fprintf(stderr, "only 999 frames allowed\n");
	exit( ERROR );
    }
   
    extension++;


    sprintf(ext, "%3d", extension);

    /* pad ext with 0's */
    for (i = 0; i < 3; i++)
	if (ext[i] == ' ')
	    ext[i] = '0';

    strncat(filename, ext, 3);

    return(0);
}
    
