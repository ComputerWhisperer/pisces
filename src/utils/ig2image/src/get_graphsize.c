/*	get_graphsize.c		Version 1.5		*/
/*	Last Modification:	3/31/90 09:35:53		*/
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
#include <math.h>
#include "bound.h"

#ifdef ANSI_FUNC

int 
get_graphsize (int *scalx, int *scaly, int *nxfac, int *nyfac, int *xfill, int *yfill, int macfile)
#else

get_graphsize( scalx, scaly, nxfac, nyfac, xfill, yfill, macfile )
int *scalx;
int *scaly;
int *nxfac;
int *nyfac;
int *xfill;
int *yfill;
int macfile;
#endif
{
    int selection;
    int xsize;
    int ysize;
    int max_xpixels;
    int max_ypixels;
    int fillx = 0;
    int filly = 0;
    float scale;
    char line[80];


    if (macfile)  {
	max_xpixels = MACMAXX;
	max_ypixels = MACMAXY;
    }
    else  {
	max_xpixels = SUNMAXX;
	max_ypixels = SUNMAXY;
    }

    printf("options for entering graph sizes are:\n");
    printf("\t1) scale of maximum size of %d pixels by %d pixels\n",
	max_xpixels, max_ypixels);
    printf("\t2) specifying the number of pixels in each direction\n");
    printf("\t\tplease enter either 1 or 2  : ");
    gets(line);
    if (sscanf(line, "%d", &selection) == EOF)  {
	fprintf(stderr, "must enter in 1 or 2\n");
	exit(ERROR);
    }
    switch (selection)  {
	case 1 :	printf("please enter in scale factor:\t");
			gets(line);
			if (sscanf(line, "%f", &scale) == EOF)  {
			    fprintf(stderr, "must enter in a scale factor\n");
			    exit(ERROR);
			}
			if ( (scale <= 0) || (scale > 1.0) )  {
	fprintf(stderr, "Illegal scale factor\n");
			    exit(ERROR);
		 	}
printf("please enter in number of pixels per point in X direction  :  ");
			gets(line);
			if (sscanf(line, "%d", nxfac) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
printf("please enter in number of pixels per point in Y direction  :  ");
			gets(line);
			if (sscanf(line, "%d", nyfac) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
			xsize = (int)floor(((double)max_xpixels)*scale/
				(double)*nxfac);
			ysize = (int)floor(((double)max_ypixels)*scale/
				(double)*nyfac);
			break;

	case 2:		printf("please enter in number of pixels in X dir.:\t");
			gets(line);
			if (sscanf(line, "%d", &xsize) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
			if ( (xsize <= 0) || (xsize > max_xpixels) )   {
	fprintf(stderr, "Illegal pixel size in X direction\n");
			    exit(ERROR);
		 	}
			printf("please enter in number of pixels in Y dir.:\t");
			gets(line);
			if (sscanf(line, "%d", &ysize) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
			if ( (ysize <= 0) || (ysize > max_ypixels) )  {
	fprintf(stderr, "Illegal pixel size in Y direction\n");
			    exit(ERROR);
		 	}
printf("please enter in number of pixels per point in X direction  :  ");
			gets(line);
			if (sscanf(line, "%d", nxfac) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
printf("please enter in number of pixels per point in Y direction  :  ");
			gets(line);
			if (sscanf(line, "%d", nyfac) == EOF)  {
			    fprintf(stderr, "must enter in an integer\n");
			    exit(ERROR);
			}
			fillx = xsize % *nxfac;
			xsize /= *nxfac;
			filly = ysize % *nyfac;
			ysize /= *nyfac;

			if (xsize  == 0)  {
	fprintf(stderr, "Too many pixels per point in X direction\n");
			    exit(ERROR);
		 	}
			if (ysize  == 0)  {
	fprintf(stderr, "Too many pixels per point in Y direction\n");
			    exit(ERROR);
		 	}
			break;
	
	default:	fprintf(stderr, "illegal graph size option\n");
			exit(ERROR);
			break;
    } /* end switch */
    *scalx = xsize;
    *scaly = ysize;
    *xfill = fillx;
    *yfill = filly;
    return(0);
}
