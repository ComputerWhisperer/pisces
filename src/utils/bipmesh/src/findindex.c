/*	%M%		Version %I%		*/
/*	Last Modification:	%G% %U%		*/

/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

#define FALSE 0
#define TRUE 1

#include "struct.h"

#ifdef ANSI_FUNC

int 
findindex (node_str array[], double value, int arraysize)
#else

findindex( array, value, arraysize )
node_str array[];
double value;
int arraysize;
#endif
{
    int low = 0;
    int high = arraysize - 1;
    int middle = arraysize/2;
    int done = FALSE;
    int key;
    while (!((high == middle) || (low == middle)))  {
	if ( (((0.99 * value) < array[middle].loc) &&
		((1.01 * value) > array[middle].loc) && (value >= 0.0))
		|| (((0.99 * value) > array[middle].loc) 
		 && ((1.01 * value) < array[middle].loc) 
		 && (value < 0.0) ) )
	    return(middle);
	else  {
	    if (value < array[middle].loc)  {
		high = middle;
		middle = (middle + low)/2;
	    }
	    else  {
		low = middle;
		middle = (middle + high)/2;
	    }
	}
    }
    return(FALSE);
}
