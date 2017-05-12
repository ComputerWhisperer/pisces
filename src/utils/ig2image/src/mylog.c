
/*	mylog.c		Version 1.3		*/
/*	Last Modification:	3/31/90 09:36:09		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/* mylog.c - takes logs of an array.  assume all numbers are > 1 so that
 *	we take the if the number is negative, the log becomes
 *	-(log(abs(number))).  for the case where number is < 1, we will
 *	assign a value of 0
 */
#include <math.h>

#ifdef ANSI_FUNC

int 
mylog (float *array, int size)
#else
mylog(array, size)
float *array;
int size;
#endif
{
    int index;
    for (index = 0; index < size; index++)  {
	if (fabs((double)array[index]) < 1.0)
	    array[index] = 0.0;
	else 
	    array[index] = (array[index] > 0) ? 
		(float)log10((double)array[index]) :
		-(float)log10((double)(-array[index]));
    }
    return(0);
}
