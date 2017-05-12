/*	write_imagetool.c		Version 1.13		*/
/*	Last Modification:	3/31/90 09:36:29		*/
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

#define NETCHEM 20
#define NETACT 21
#define NETCARRIERS 22
#define ERROR -1

#ifdef ANSI_FUNC

int 
write_imagetool (int material, int variable, char *image_filename, int takelog, int takeabs, int outfindex, int win_xmin_index, int win_xmax_index, int win_ymin_index, int win_ymax_index, int xsize, int ysize, float *xdata, float *ydata, int mode, double min_value, int plotxsize, int plotysize, int nxfac, int nyfac, int macfile)
#else

write_imagetool(material, variable, 
    image_filename, takelog, takeabs, outfindex,
    win_xmin_index, win_xmax_index, win_ymin_index, win_ymax_index,
    xsize, ysize, xdata, ydata, mode, min_value, plotxsize, plotysize,
    nxfac, nyfac, macfile)
int material;
int variable;
char *image_filename;
int takelog;
int takeabs;
int outfindex;
int win_xmin_index;
int win_xmax_index;
int win_ymin_index;
int win_ymax_index;
int xsize;
int ysize;
float *xdata;
float *ydata;
int mode;
float min_value;
int plotxsize;
int plotysize;
int nxfac;
int nyfac;
int macfile;
#endif
{
    static float *mydata;
    static float *val;

    int i;

    extern int nn;


    extern *newget_solval();
    extern fill_grid();
    extern mylog();
    extern makeframe();




    /* create array for solution value */
    if (val == NULL)
	if ((val = (float *)malloc(nn * sizeof(float))) == NULL)  {
	    fprintf(stderr, "memory allocation failed\n");
	    exit(ERROR);
	}

    for (i = 0; i < nn; i++)
	val[i] = 0.0;

    if (newget_solval(val, variable, takeabs) != 0)  {
	fprintf(stderr, "error generating solution array\n");
	exit(ERROR);
    }

    /* do we need to take a log */
    if (takelog)
	if (mylog(val,nn) != 0)  {
	    fprintf(stderr, "error taking log of solution array\n");
	    exit(ERROR);
	}

    if ((mydata = (float *)malloc((win_xmax_index - win_xmin_index + 1) * 
	(win_ymax_index - win_ymin_index + 1) * sizeof(float))) == NULL)  {
	    fprintf(stderr, "memory allocation failed\n");
	    exit(ERROR);
    }



    for (i = 0; i < (win_xmax_index - win_xmin_index + 1) * 
	    (win_ymax_index - win_ymin_index + 1); i++)
	mydata[i] = min_value;


   /*  interpolate solution values off grid */
    if (fill_grid(xsize + 3, ysize + 3, xdata, ydata, mydata,
	    val, material, win_xmin_index, win_xmax_index,
	    win_ymin_index, win_ymax_index, min_value) != 0)  {
	fprintf(stderr, "error filling data array\n");
	exit(ERROR);

    }

    if (makeframe(mydata, outfindex, plotxsize, plotysize, nxfac, nyfac, 
	image_filename, min_value, mode, macfile) != 0)  {
	fprintf(stderr, "error in creating binary image file\n");
	exit(ERROR);
   }

    free(mydata);
    return( 0 ); 
}
