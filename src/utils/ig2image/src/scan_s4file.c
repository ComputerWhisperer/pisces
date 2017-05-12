
/*	scan_s4file.c		Version 1.11		*/
/*	Last Modification:	3/31/90 09:36:34		*/
/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/




/* scan_s4file - scans a suprem4 structure file to find the maximum and
 *	minimum extent. also finds the maximum and minimum data
 */


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "more_imp.h"
#include "expr.h"


#define BUFFERSIZE 512
#define ERROR -1
/*define constants for each impurity*/
#define V  0		/*Vacancies*/
#define I  1		/*Interstitials*/
#define As 2		/*Arsenic*/
#define P  3		/*Phosphorus*/
#define Sb 4		/*antimony*/
#define B  5		/*Boron*/
#define ELE 6		/*electron concentration*/
#define HOL 7		/*hole concentration*/
#define XVEL 8		/*X velocity*/
#define YVEL 9		/*Y velocity*/
#define O2 10		/*Dry O2*/
#define H2O 11		/*Wet O2*/
#define T  12		/*Interstitial Traps*/
#define Au 13		/*Gold*/
#define Psi 14		/*Potential*/
#define Sxx 15		/* Components of stress - not solution variables*/
#define Syy 16		/* but too expensive to recompute for plotting */
#define Sxy 17
#define Cs  18		/* Cesium for oxide charges */
#define DELA 19		/*change in interface area - not solution variable*/


#ifdef ANSI_FUNC

void 
scan_s4file (char *filename, int key, int mat, float *x_min, float *x_max, float *y_min, float *y_max, float *min_data, float *max_data, int flip, double scale, int take_log, int take_abs)
#else
scan_s4file( filename, key, mat, x_min, x_max, y_min, y_max,  min_data, 
	max_data, flip, scale, take_log, take_abs )
char *filename;
int key;
int mat;
float *x_min;
float *x_max;
float *y_min;
float *y_max;
float *min_data;
float *max_data;
int flip;
double scale;
int take_log;
int take_abs;
#endif
{
    FILE *fp;
    int i;
    int n_imp;
    int imp_num;
    int material;
    int done = 0;
    int soltoimp[30];  /* maximum number of impurities is 30 */
    float min_x = 10000.0;
    float max_x = -10000.0;
    float min_y = 10000.0;
    float max_y = -10000.0; 
    float max_value = -10000.0;
    float min_value = 10000.0;
    float value = 0.0;

    double x = 0.0;
    double y = 0.0;
    double h = 0.0;
    double scalx = scale;
    double scaly = ((flip) ? -scale : scale);
    char line[BUFFERSIZE];
    char flag[2];
    char *ip;

    if ( (fp = fopen(filename, "r")) == NULL )  {
	fprintf(stderr, "can't open file %s for read\n", filename);
	exit(-1);
    }

    for (i = 0; i < 30; i++)
	soltoimp[i] = 0;

    while (!feof(fp))  {
	fgets(line, BUFFERSIZE, fp);
	sscanf( line, "%1s", flag);
	switch (flag[0])  {

	    case 'c'  :
	if (sscanf(line,"c %d %lf %lf %lf", &i, &x, &y, &h) != 4)  {
	    fprintf(stderr, "need 4 items\n");
	    exit(-1);
	}
	else  {
	    x *= scalx;
	    y *= scaly;
	    if ( x  < min_x ) min_x = (float)x;
	    if ( x  > max_x ) max_x = (float)x;
	    if ( y  < min_y ) min_y = (float)y;
	    if ( y  > max_y ) max_y = (float)y;
	}
	break;
	
	    case 's'  :
	if (sscanf(line,"s %d", &n_imp) != 1)  {
	    fprintf(stderr, "no impurities on the s line\n");
	    exit(-1);
	}
	/*loop to read in all the solution numbers*/
	/*position ip after s %d*/
	ip = line + 1;
	while( isspace(*ip)) ip++;
	while(!isspace(*ip)) ip++;

	/* enter loop sitting on first character after digit */
	for(i = 0; i < n_imp; i++) {

	    /* better be a number */
	    if (sscanf(ip," %d", &(soltoimp[i])) != 1) {
	        fprintf(stderr,"less data than specified impurity number\n");
		exit(-1);
	    }

	    /* move up ip - depends on \n at end of line */
	    while( isspace( *ip)) ip++;
	    while(!isspace( *ip)) ip++;
	}
	break;

	    case 'n' :
	/*read the line data*/
	done = 0;
	if ( sscanf(line,"n %d %d", &i, &material ) != 2 )  {
	    fprintf(stderr, "incomplete node line\n");
	    exit(-1);
	}	

	if ( (mat != 7) && (mat != material) ) break;

	/*read the solution values*/
	ip = line + 1;

	while( isspace( *ip)) ip ++;
	while(!isspace( *ip)) ip ++;
	while( isspace( *ip)) ip ++;
	while(!isspace( *ip)) ip ++;

	for(i = 0; (i < n_imp) && !done; i++) {
	    if ( sscanf( ip, "%le", &x ) != 1 ) {
	        fprintf(stderr, "incomplete node line\n");
		exit(-1);
	    }
	    switch (key)  {
		case NETACT :
   fprintf(stderr, "NETACT not supported. default to NETCHEM\n");

		case NETCHEM  :
		    switch (soltoimp[i])  {
			case As :
			case P :
			case Sb : value -= (float)x;
				  break;

			case B :
			case Cs : value += (float)x;
			 	  break;
	
			default:  break;
		    } /* end switch (soltoimp[i]) */
		    break;

		case NETCARRIERS  :
		    switch (soltoimp[i])  {
			case HOL : value += (float)x;
				   break;

			case ELE : value -= (float)x;
				   break;

			default:  break;
		    } /* end switch (soltoimp[i]) */
		    break;

	        case OXY  :
		    if ((soltoimp[i] == O2) || (soltoimp[i] == H2O))  {
		        value = (float)x;
		        done = 1;
		    }
        	    break;

		default : 
		    if (soltoimp[i] == key)  {
			value = (float)x;
			done = 1;
	 	    }
		    break;
	    }  /* end switch (key) */
	    while( isspace( *ip)) ip ++;
	    while(!isspace( *ip)) ip ++;
	}

	if (take_abs)
	    value = (float)fabs((double)value);

	if (take_log)  {
	    if (fabs((double)value) < 1.0)
		value = 1.0;
	    else
		value = (value < 0) ? -log10(fabs((double)value)) 
		    : log10((double)value);
	}
	if (value < min_value) min_value = value;
	if (value > max_value) max_value = value;

	break;

	    default :
	    /* ignore this line */
	break;
	}
    } /* Next line */

    fclose(fp);
    *x_min = min_x;
    *x_max = max_x;
    *y_min = min_y;
    *y_max = max_y;
    *min_data = min_value;
    *max_data = max_value;
}
