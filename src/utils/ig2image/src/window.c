/*	window.c		Version 1.6		*/
/*	Last Modification:	4/10/90 13:14:03		*/
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
#include <math.h>

window(x, y, xarray, yarray, xsize, ysize, xlo, xhi, ylo, yhi)
float x;
float y;
float *xarray;
float *yarray;
int xsize;
int ysize;
int *xlo;
int *xhi;
int *ylo;
int *yhi;
{
    static int lo;
    static int hi;
    static int middle;
    static int done;


    /* check for exceeding boundary conditions */
    if (x*1.0e4 > xarray[xsize-1])  {
	*xlo = xsize-1;
  	*xhi = xsize-1;
    }
    else if (x*1.0e4 < xarray[0])  {
	*xlo = 0;
	*xhi = 0;
    }
    else  {
    /* window in x direction */
        lo = 0;
        hi = xsize;
        done = 0;
	while (!done)  {
	    middle = (hi + lo)/2;
	    if ((xarray[middle] <= x*1.0e4) && (middle > lo))
	        lo = middle;
	    if ((xarray[middle] >= x*1.0e4) && (middle < hi))
	        hi = middle;
	    if ((hi + lo)/2 == middle)
	        done = 1;
        }
	if (x == xarray[lo]) hi = lo;
	if (x == xarray[hi]) lo = hi;
        *xlo = lo;
        *xhi = hi;
    }

    /* check for exceeding boundary conditions */
    if (y*1.0e4 > yarray[ysize-1])  {
	*ylo = ysize-1;
  	*yhi = ysize-1;
    }
    else if (y*1.0e4 < yarray[0])  {
	*ylo = 0;
	*yhi = 0;
    }
    else  {
        /* window in y direction */
        lo = 0;
        hi = ysize;
        done = 0;
        while (!done)  {
	    middle = (hi + lo)/2;
	    if ((yarray[middle] <= y*1.0e4) && (middle > lo))
	        lo = middle;
	    if ((yarray[middle] >= y*1.0e4) && (middle < hi))
	        hi = middle;
	    if ((hi + lo)/2 == middle)
	        done = 1;
	}
	if (y == yarray[lo]) hi = lo;
	if (y == yarray[hi]) lo = hi;
	*ylo = lo;
	*yhi = hi;
    }

    return(0);
}
