/*	main.c		Version 1.18		*/
/*	Last Modification:	3/31/90 09:35:58		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/
/*  	Converts IGGI-2 files into ImageTool format
 *
 *	Goodwin Chin
 *	April 1, 1989
 *
 */

#define STATIC_ALLOCATION_TIME
#include "constant.h"
#include "global.h"
#include "geom.h"
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#ifdef CONVEX
#include <strings.h>
#else
#include <string.h>
#endif


#include "material.h"
#include "impurity.h"
#include "sysdep.h"
#include "expr.h"
#include "diffuse.h"
#include "defect.h"
#include "matrix.h"
#include "shell.h"
#include "plot.h"
#include "more_imp.h"

#define STATIC_ALLOCATION_TIME
#include "bound.h"
#include <fcntl.h>


main (argc, argv)
int argc;
char **argv;
{
    char *inf;
    char *image_filename;
    char line[80];
    char pisf[80];
    char tmpf[12];
    char pimeshfile[80];
    char response;
    int matnum = -1;
    int varnum = -1;
    int outfindex = 0;
    int flip = 0;
    int multiple_files = 0;
    int piscfiles = 0;
    int done = 0;
    int c;
    int quiet = 0;
    int macfile = 0;
    int mode = 0;
    int errflg;
    int key = 0;
    extern int optind;
    extern char *optarg;
    extern int opterr;
    int takelog = 0;
    int takeabs = 0;
    int win_xmin_index = 10000;
    int win_xmax_index = -10000;
    int win_ymin_index = 10000;
    int win_ymax_index = -10000;
    int plotxsize = -1;
    int plotysize = -1;
    int fd = 0;
    float *xdata;
    float *ydata;
    float x_min = 10000.0;
    float x_max = -10000.0;
    float y_min = 10000.0;
    float y_max = -10000.0;
    float fjunk = 0.0;
    int xsize = -1;
    int ysize = -1;
    int nxfac = -1;
    int nyfac = -1;
    int xfill;
    int yfill;
    float min_value = 10000.0;
    float max_value = -10000.0;
    float scale = 1.0;
    FILE *fp;
    int traps = 0;


    gmin = MAXFLOAT;
    gmax = -MAXFLOAT;
    min_x = 10000.0;
    max_x = -10000.0;
    min_y = 10000.0;
    max_y = -10000.0;
/* although *outf is not used, it is a handy hook if you want to debug
 *	stuff internal to the program
 */

    if ( (inf = malloc(80)) == NULL )  {
	fprintf(stderr, "unable to allocate memory\n");
	exit(ERROR);
    }

    if ( (image_filename = malloc(80)) == NULL )  {
	fprintf(stderr, "unable to allocate memory\n");
	exit(ERROR);
    }


    while ((c = getopt(argc, argv, "ab:celm:pq:v:")) != EOF)
	switch (c)  {
	    case 'a'  : takeabs = 1;
			break;

	    case 'b'  : if (sscanf(optarg, "%s", image_filename) == EOF)  {
			    fprintf(stderr, "need an filename\n");
			    exit(ERROR);
			}
			break;

	    case 'c'  : macfile = 1;
			break;

	    case 'e'  : multiple_files = 1;
			break;

	    case 'l'  : takelog = 1;
			break;

	    case 'm'  : if (sscanf(optarg, "%d", &matnum) == EOF)  {
			    fprintf(stderr, "need a material number\n");
			    exit(ERROR);
			}
			break;

	    case 'p'  : if (sscanf(optarg, "%s", pisf) == EOF)  {
			    fprintf(stderr, "need an filename\n");
			    exit(ERROR);
			}
			piscfiles = 1;
			matnum = 3;
			break;

	    case 'q'  : quiet = 1;
			break;

	    case 't'  : traps = 1;
			break;

	    case 'v'  : if (sscanf(optarg, "%d", &varnum) == EOF)  {
			    fprintf(stderr, "need a variable number\n");
			    exit(ERROR);
			}
			break;
	
	    case '?'  : errflg++;
			break;

	    default   : printf("switch not supported. ignore flag.\n");
			fflush(stdout);
			break;
	} /* end switch */
/*
    if ((errflg) || (argc == 1))  {
*/
    if (argc == 1)  {
	fprintf(stderr, 
 "ig2image [-ab<bin_file>celm<mat_num>p<sol_fil>qtv<var_num>?] msh_file\n");
	exit(ERROR);
    }

    /* now that we have parsed the input information, look for a mesh file */
    if (optind < argc)
	inf = argv[optind];
    else  {
	fprintf(stderr, "need a mesh file\n");
	exit(ERROR);
    }	

    if ( strlen(image_filename) == 0 )  {
	printf("File Name for binary images   :   ");
	gets(line);
	if (sscanf(line, "%s", image_filename) == EOF)  {
	    fprintf(stderr, "must specify a file name\n");
	    exit(ERROR);
	}
    }

    if ((macfile == 0) && !quiet)  {
	printf("Do you want MacIntosh output? [y for yes]  :  ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF) 
	    if (response == 'y')  
	        macfile = 1;
    }

    if ((multiple_files == 0) && !quiet)  {
	printf("Is there more than 1 file to process? [y for yes]  :  ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF) 
	    if (response == 'y')  
	        multiple_files = 1;
    }

    if (!quiet && (piscfiles == 0))  {
	printf("Is the input file from Pisces [y for yes]  :   ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF)  
	    if (response == 'y')  {
	        strcpy(pimeshfile,inf);
	        piscfiles = 1;
	        matnum = 3;
	        printf("Enter name of Pisces Solution File :    ");
	        gets(line);
  	        if (sscanf(line, "%s", pisf) == EOF)  {
		    fprintf(stderr, "must specify a file name\n");
		    exit(ERROR);
		}
                tmpf[0] = '.';
                tmpf[1] = 'p';
                tmpf[2] = 'i';
                tmpf[3] = 's';
                tmpf[4] = 'c';
                tmpf[5] = 'e';
                tmpf[6] = 's';
                tmpf[7] = 't';
                tmpf[8] = 'o';
                tmpf[9] = 's';
                tmpf[10] = '4';
                tmpf[11] = '\0';
printf("Are there deep level traps in the solution file? [y for yes] :  ");
		gets(line);
		if (sscanf(line, "%c", &response) != EOF)	
		    if (response == 'y')
			traps = 1;
            }
    }


    if (matnum == -1)  {
	printf("\nchoose from one of the materials:\n");
	printf("\tGas\t[0]:\n");
	printf("\tOxide\t[1]:\n");
	printf("\tNitride\t[2]:\n");
	printf("\tSilicon\t[3]:\n");
	printf("\tPolysilicon\t[4]:\n");
	printf("\tOxynitride\t[5]:\n");
	printf("\tAluminum\t[6]:\n");
	printf("\tAll\t[7]:\n");
	printf("\n");
	printf("Please Enter in the number of your choice:  ");
	gets(line);
  	if (sscanf(line, "%d", &matnum) == EOF)  {
	    fprintf(stderr, "must specify a material number\n");
	    exit(ERROR);
	}
    }

    if (varnum == -1)  {
	printf("\nchoose from one of the impurities:\n");
	printf("\tVacancies\t[0]:\n");
	printf("\tInterstitials\t[1]:\n");
	printf("\tArsenic\t[2]:\n");
	printf("\tPhosphorus\t[3]:\n");
	printf("\tAntimony\t[4]:\n");
	printf("\tBoron\t[5]:\n");
	printf("\tElectron Concentration\t[6]:\n");
	printf("\tHole Concentration\t[7]:\n");
	printf("\tX Velocity\t[8]:\n");
	printf("\tY Velocity\t[9]:\n");
	printf("\tOxygen Concentration\t[10,11]:\n");
/*
 *  combine Dry O2 and Wet O2 to OXY like in plot stuff
	printf("\tDry O2\t[10]:\n");
	printf("\tWet O2\t[11]:\n");
*/
	printf("\tInterstitial Traps\t[12]:\n");
	printf("\tGold\t[13]:\n");
	printf("\tPotential\t[14]:\n");
	printf("\tSxx Component of stress\t[15]:\n");
	printf("\tSyy Component of stress\t[16]:\n");
	printf("\tSxy Component of stress\t[17]:\n");
	printf("\tCesium\t[18]:\n");
	printf("\tChange in Interface Area\t[19]:\n");
        printf("\tNet Chemical Doping\t[20]:\n");
        printf("\tNet Active Doping\t[21]:\n");
        printf("\tNet Carrier Concentration\t[22]:\n");
        printf("\tElectron Current\t[23]:\n");
        printf("\tHole Current\t[24]:\n");
        printf("\tTotal Current\t[25]:\n");
	printf("\tDeep Level Traps\t[26]:\n");
	printf("\n");
	printf("Please Enter in the number of your choice:  ");
	gets(line);
  	if (sscanf(line, "%d", &varnum) == EOF)  {
	    fprintf(stderr, "must specify a variable number\n");
	    exit(ERROR);
	}
    }

    if ( (varnum < 0) || (varnum > 26) )  {
	fprintf(stderr, "Impurity value must be between 0 and 26\n");
	exit(ERROR);
    }

    if ( (matnum < 0) || (matnum > 7) )  {
	fprintf(stderr, "Material value must be between 0 and 7\n");
	exit(ERROR);
    }

    if ( ((varnum == NETCHEM) || (varnum == NETACT) || 
	(varnum == NETCARRIERS) || (varnum == TOTCUR))
	&& !quiet && (takeabs == 0) )   {
	printf(
"Do you want to take the absolute value of the data? [y for yes] :  ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF) 
	    if (response == 'y')
	        takeabs = 1;
    }

    if (!quiet && (takelog == 0))  { 
	printf(
"Do you want to take the log of the data? [y for yes] :  ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF)
	    if (response == 'y')
	        takelog = 1;
    }



    if (((varnum == NETCHEM) || (varnum == NETACT) || 
	(varnum == NETCARRIERS)) && (!takeabs) && (takelog))  {
	printf("OK to scale from min to -14 and 14 to max? [y for yes] :   ");
	gets(line);
	if (sscanf(line,"%c", &response) != EOF)
	    if (response ==  'y')
	        mode = 1;
    }

    /* change O2 and H2O to OXY here */
    if ((varnum == O2) || (varnum == H2O))
	varnum = OXY;

    /* scan all the files for extremes */
    if (piscfiles) { 
	    switch (varnum)  {
		case P:
		case ELE :  	key = 2;
			    	break;

		case B:
		case HOL :	key = 3;
				break;

		case Psi :  	key = 1;
				break;

		case NETCHEM :  
		case NETACT  :	key = 4;
				break;

		case NETCARRIERS :  
				fprintf(stderr, 
"case not suported. default to electron concentration\n");
				key = 2;
				break;

		case ELECUR :	key = 5;
				break;

		case HOLCUR :	key = 6;
				break;

		case TOTCUR :	key = 7;
				break;

		case DLT :	key = 8;
				break;

		default  :  
				fprintf(stderr, "illegal case\n");
				exit(ERROR);
				break;
	    }  /* end switch */
	    strcpy(line,pisf);
	    readp2_(pimeshfile, line, tmpf, &key, &multiple_files,
		&min_x, &max_x, &min_y, &max_y, &gmin, &gmax, &takelog,
		&takeabs, &traps);
    }
    else  {
	strcpy(line, inf);
	while (!done)  {
	    scan_s4file(line, varnum, matnum, &x_min, &x_max,
		    &y_min, &y_max, &min_value, &max_value, flip, 
		    scale, takelog, takeabs);
	    if (x_min < min_x) min_x = x_min;
	    if (x_max > max_x) max_x = x_max;
	    if (y_min < min_y) min_y = y_min;
	    if (y_max > max_y) max_y = y_max;
	    if (min_value < gmin) gmin = min_value;
	    if (max_value > gmax) gmax = max_value;

	    if (multiple_files)  {
		s4incf(line);
		if ((fp = fopen(line,"r")) == NULL) 
		    done = 1;
		else
		    fclose(fp);
	    }
	    else
		done = 1;
	} /* end while (!done) */
    } /* end if-then-else  piscfiles */

    min_value = (float)(floor((double)gmin) - 1.0);

    /* get graph dimensions to determine coordinates */
    if (get_graphsize(&xsize, &ysize, &nxfac, &nyfac, &xfill, &yfill,
		macfile) != 0)  {
	fprintf(stderr, "error finding graph dimensions\n");
	exit(ERROR);
    }


	/* generate grid */
    if (make_grid(xsize, ysize, &xdata, &ydata,
		&win_xmin_index, &win_xmax_index,
		&win_ymin_index, &win_ymax_index) != 0)  {
	fprintf(stderr, "error creating index array\n");
	exit(ERROR);
    }


/* for a windowed picture we need to modify the call to makeframe.  since
 *	we may end up with a different number of data points, print this
 *	out before calling makeframe
 */

    plotxsize = xsize * nxfac + xfill;
    printf("x dimension is %d pixels\n", plotxsize);

    plotysize = ysize * nyfac + yfill;
    printf("y dimension is %d pixels\n", plotysize);


        printf("gmin = %e\n", gmin);
        printf("gmax = %e\n", gmax);
 	printf("Do you want to specify global min and max? [y for yes] : ");
	gets(line);
	if (sscanf(line, "%c", &response) != EOF)
	    if (response == 'y') {
/* prompt the user for gmin and gmax */
	        printf("current gmin = %e\n", gmin);
	        printf("enter global min [ret for no change] : ");
	        gets(line);
	        if ((line[0] != '\0') && (line[0] != '\n') 
			&& (line[0] != '\0')) 
		   sscanf(line, "%f", &gmin);
	        printf("current gmax = %e\n", gmax);
	        printf("enter global max [ret for no change] : ");
	        gets(line);
	        if ((line[0] != '\0') && (line[0] != '\n') 
			&& (line[0] != '\0')) 
		    sscanf(line, "%f", &gmax);
	        if (gmax < gmin) {
		    printf("global max is less than global min");
		    exit(ERROR);
	        }
	    }

    done = 0;


    /* for the case of net doping from pisces, I have placed the
     *  answer in the As variable
     */
    if ((piscfiles) && ((varnum == NETCHEM)  || (varnum == NETACT)))
	varnum == As;

    while (!done)  {
	if (piscfiles)  {
	    key = 0;
	    readp2_(pimeshfile, pisf, tmpf, &key, &key, &fjunk,
		&fjunk, &fjunk, &fjunk, &fjunk, &fjunk, &takelog,
		&takeabs, &traps);
	    strcpy(inf, tmpf);
	}
	if (ig2_read(inf, flip, scale) < 0)   {
	    fprintf(stderr, "problems in reading the mesh file %s\n",
		inf);
	    exit(ERROR);
	}
	if (make_db() == -1)  {
	    fprintf(stderr, "errors in the geometrical connectivity\n");
	    exit(ERROR);
	}

	if (write_imagetool(matnum, varnum, image_filename, takelog, takeabs,
	    outfindex, win_xmin_index, win_xmax_index, win_ymin_index,
	    win_ymax_index, xsize, ysize, xdata, ydata, mode, min_value,
	    plotxsize, plotysize, nxfac, nyfac, macfile) != 0)  {
		fprintf(stderr, "error in write_imagetool\n");
		exit(ERROR);
	}
		

	if (multiple_files)  {
	    if (piscfiles) {
	        incfil(pisf);
		if ((fd = open(pisf, O_RDONLY)) == ERROR) 
		    done=1;
		else
		    close(fd);
	    }
	    else {
		s4incf(inf);
		if ((fp = fopen(inf, "r")) == NULL) 
		    done=1;
		else
		    fclose(fp);	
	    }
	}
	else
	    done = 1;
	outfindex++;
    } /* end while (!done)  */
}
