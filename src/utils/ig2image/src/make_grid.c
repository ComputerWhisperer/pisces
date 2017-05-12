/*	make_grid.c		Version 1.10		*/
/*	Last Modification:	3/31/90 09:37:22		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

#include <stdlib.h>
#include <stdio.h>
#include "bound.h"

#ifdef ANSI_FUNC

int 
make_grid (int xsize, int ysize, float **xd, float **yd, int *win_xmin_index, int *win_xmax_index, int *win_ymin_index, int *win_ymax_index)
#else

make_grid(xsize, ysize, xd, yd, 
	win_xmin_index, win_xmax_index, win_ymin_index, win_ymax_index)
int xsize;
int ysize;
float **xd;
float **yd;
int *win_xmin_index;
int *win_xmax_index;
int *win_ymin_index;
int *win_ymax_index;
#endif
{
    int index;
    int junk_index;
    int redox = 0;
    int redoy = 0;
    float newxmin = min_x;
    float newxmax = max_x;
    float newymin = min_y;
    float newymax = max_y;
    float *xdata;
    float *ydata;
    float loc;
    float spacing;
    float *ptr;
    float junk = 0.0;
    extern float Fmax();
    extern float Fmin();
    extern shiftwindow();
    char answer;
    char line[80];
    
    if ((xdata = (float *)malloc((xsize + 3) * sizeof(float))) == NULL)  {
	fprintf(stderr, "unable to allocate memory\n");
	exit(ERROR);
    }
    if ((ydata = (float *)malloc((ysize + 3) * sizeof(float))) == NULL)  {
	fprintf(stderr, "unable to allocate memory\n");
	exit(ERROR);
    }

    printf("current device limits are :\n");
    printf("\txmin = %f,\txmax = %f\n", min_x, max_x);
    printf("\tymin = %f,\tymax = %f\n", min_y, max_y);
    printf("\t\tDo you want to change any of these? [y for yes]  :  ");
    gets(line);
    if (sscanf(line, "%c", &answer) != EOF);
        if (answer == 'y')  {
	    printf("enter in new xmin [ret for no change]  :  ");
	    gets(line);
	    if ((line[0] != '\0') && (line[0] != '\n') 
			&& (line[0] != '\0'))  {
	        sscanf(line, "%f", &newxmin);
		if (newxmin < min_x)
		    *win_xmin_index = 0;
		else  {
		    *win_xmin_index = 1;
		    redox = 1;
		}
	    }
	    else  {
	        *win_xmin_index = 0;
		newxmin = min_x;
	    }
	    printf("enter in new xmax [ret for no change]  :  ");
	    gets(line);
	    if ((line[0] != '\0') && (line[0] != '\n')
			&& (line[0] != '\0'))  {
	        sscanf(line, "%f", &newxmax);
		if (newxmax < max_x)
		    redox = 1;
	    }
	    else  
		newxmax = max_x;
	    if (*win_xmin_index != 0)
	        *win_xmax_index = xsize + 1;
	    else
	        *win_xmax_index = xsize;
	    printf("enter in new ymin [ret for no change]  :  ");
	    gets(line);
	    if ((line[0] != '\0') && (line[0] != '\n')
			&& (line[0] != '\0'))  {
	        sscanf(line, "%f", &newymin);
		if (newymin < min_x)
		    *win_ymin_index = 0;
		else  {
		    *win_ymin_index = 1;
		    redoy = 1;
		}
	    }
	    else  {
	        *win_ymin_index = 0;
		newymin = min_y;
	    }
	    printf("enter in new ymax [ret for no change]  :  ");
	    gets(line);
	    if ((line[0] != '\0') && (line[0] != '\n')
			&& (line[0] != '\0'))  {
	        sscanf(line, "%f", &newymax);
		if (newymax < max_y)
		    redoy = 1;
	    }
	    else  
		newymax = max_y;
	    if (*win_ymin_index != 0)
	        *win_ymax_index = ysize + 1;
	    else
	        *win_ymax_index = ysize;

        }  /* end if answer == 'y'  */
        else  {
	    *win_xmin_index = 0;
	    *win_xmax_index = xsize;
	    *win_ymin_index = 0;
	    *win_ymax_index = ysize;
        } /* end if-then-else (answer == 'y') */


    /* for the case where the window coordinates are outside the
     *   current device dimensions, we need to redefine the spacing
     *   to be max( (max_x - min_x)/xsize ,
     *          (max(win_xmax, max_x) - min(win_xmin, min_x))/xsize )
     */
    spacing = Fmax( (max_x - min_x) / xsize,
        (Fmax(newxmax, max_x) - Fmin(newxmin, min_x)) / xsize );

    ptr = xdata;
    for (index = 0, loc = Fmin(newxmin, min_x); index < xsize + 3; index++)  {
	*ptr++ = loc;
	loc += spacing;	
    }
    /* adjust last point to coincide with edge */
    xdata[xsize] = Fmax(max_x, newxmax);

    spacing = Fmax( (max_y - min_y) / ysize,
        (Fmax(newymax, max_y) - Fmin(newymin, min_y)) / ysize );
    ptr = ydata;
    for (index = 0, loc = Fmin(newymin, min_y); index < ysize + 3; index++)  {
        *ptr++ = loc;
        loc += spacing;
    }
    /* adjust last point to coincide with edge */
    ydata[ysize] = Fmax(max_y, newymax);


    if (redox)
        shiftwindow(xdata, xsize+3, newxmin, newxmax, *win_xmin_index,
	    *win_xmax_index);
    if (redoy)
        shiftwindow(ydata, ysize+3, newymin, newymax, *win_ymin_index,
	    *win_ymax_index);
    
    *xd = xdata;
    *yd = ydata;

    return(0);
}    


#ifdef ANSI_FUNC

int 
shiftwindow (float *array, int size, double min, double max, int min_index, int max_index)
#else

shiftwindow( array, size, min, max, min_index, max_index )
float *array;
int size;
float min;
float max;
int min_index;
int max_index;
#endif
{
    float loc;
    float *ptr;
    float spacing;
    int i;

    array[min_index] = min;
    array[max_index+1] = array[size-3];
    array[max_index] = max;
    spacing = (max - min)/((float)max_index - (float)min_index);

    ptr = &(array[min_index+1]);
    for ( i = min_index+1, loc = min + spacing; i < max_index; i++ )  {
	*ptr++ = loc;
	loc += spacing;
    }

    return(0);
}

#ifdef ANSI_FUNC

float 
Fmax (double x, double y)
#else

float Fmax(x,y)
float x;
float y;
#endif
{
    return (x > y)? x: y;
}

#ifdef ANSI_FUNC

float 
Fmin (double x, double y)
#else

float Fmin(x,y)
float x;
float y;
#endif
{
    return (x < y)? x: y;
}
