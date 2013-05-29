/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*
 *	reads3as.c
 *		reads in suprem3 export format (ascii form).
 *		extracts information necessary for grid generation.
 *	goodwin chin
 *	
 *	inputs :
 *		supfil - pointer to name of Suprem3 file
 *	 	imptyp - type of impurity we're trying to obtain
 *				data for.
 *		oxth - oxide thickness(if present)
 *		peakcon - peak concentration
 *		junc - junction depth
 *		std - standard deviation assuming a gaussian profile with
 *			critical points at the junction depth and the
 *			location of the peak concentration
 *		subdop - substrate doping
 *		type - type of substrate (n=0,p=1)
 *		depth - depth of silicon
 */

#define STATIC_ALLOCATION_TIME
#include <stdio.h>
#include <math.h>
#include "struct.h"


#define 	S3SILICON 	1
#define 	S3OXIDE		2
#define		MAXIMPURITIES	4


#ifdef DEBUG

main(argc, argv)
int argc;
char *argv[];
{
    char name[20];
    int boron=5;
    double oxth;
    double peakcon;
    double junc;
    double std;
    double subdop;
    int type;
    double depth;

    if ( argc != 2 )  {
	printf( "usage: %s export_file\n", argv[0] );
	exit(-1);
    }
    sscanf( argv[1], "%s", name );
    reads3as( name, boron, &oxth, &peakcon, &junc, &std, &subdop,
	&type, &depth );
    printf( "oxide thickness is %g\n", oxth );
    printf( "peak concentration is %g\n", peakcon );
    printf( "junction is at %g\n", junc );
    printf( "standard deviation is at %g\n", std );
    printf( "substrate doping is %g\n", subdop );
    printf( "material type is %d\n", type );
    printf( "silicon thickness is %g\n", depth );
}
#endif /*DEBUG*/


reads3as( supfil, imptyp, oxth, peakcon, junc, std, subdop, type, depth )
char *supfil;
int imptyp;
double *oxth;
double *peakcon;
double *junc;
double *std;
double *subdop;
int *type;
double *depth;
{

	FILE	*fp ;
	int	 i, j ;
	int intflush;
	double flush;
	char flushchar[50];
	int mattype;
	double thickness;
	int topnode_silicon;
	int topnode;
	int doptag;
	double chem;
	double active;
	int imptype[MAXIMPURITIES];
	double *impchem;
	double *impactive;
	double *substrate;
	double *xarray;
	double peak_loc;
	int nlayers;
	int nimps;
	int nnodes;
	int done = 0;



	/* open the file */
	if (!(fp = fopen(supfil, "r"))) {
                char buf[128];
		sprintf(buf, "Unable to open %s\n", supfil) ;
                perror(buf);
		return (-1) ;
	}

	/* get the number of layers, impurities, and nodes */
	fscanf(fp, "%d %d %d", &nlayers, &nimps, &nnodes) ;

	impchem = salloc( double, nnodes );
	impactive = salloc( double, nnodes );
	substrate = salloc( double, nimps );
	xarray = salloc( double, nnodes );

	/* for each layer get the layer name string, material type,
	 * thickness, index of top node, orientation, and polysilicon
	 * grain size.
	 */
	for (i = 0 ; i < nlayers ; i++) {
	    fscanf(fp, "%s %d %lf %d %d %lf",
					flushchar,
					&mattype,
					&thickness,
					&topnode,
					&intflush,
					&flush );
	    if ( mattype == S3OXIDE )  
		*oxth = thickness;
	    else if ( mattype == S3SILICON )  {
		*depth = thickness;
		topnode_silicon = topnode;
	    }
	}

	/* for each impurity get the name string and index 
	 *
	 *  an offset of 4 due to ordering of impurities in struct.h 
	 */
	for (i = 0 ; i < nimps ; i++ ) {
	    fscanf(fp, "%s %d", flushchar, &(imptype[i])) ;
	    if ( imptype[i] == imptyp-4 )
	       doptag = i;
	}

	/* for each layer and impurity get the integrated dopant and
	 * interior concentration of polysilicon grains.
	 */
	for (i = 0 ; i < nlayers ; i++ ) 
	    for (j = 0 ; j < nimps ; j++) 
		fscanf( fp, "%lf %lf", &flush, &flush );

	/* for each point in the structure get the grid spacing, the
	 * distance from the surface of each node, and the chemical
	 * and active impurity concentrations at each node.
	 */
	for (i = 0 ; i < nnodes ; i++ ) {
	    fscanf( fp, "%lf %lf", &flush, &xarray[i] ) ;
	    for (j = 0 ; j < nimps ; j++ ) {
		fscanf( fp, "%lf %lf", &chem, &active );
		if ( j == doptag )  {
        	    impchem[i] = chem;
		    impactive[i] = active;
		}
		if ( i == nnodes - 1 )
		    substrate[j] = fabs(active);
  	    }
	}

	/*  scan arrays to extact information */

	*subdop = 0.0;
	*type = 1;
	for ( i=0; i < nimps; i++ )
	    if ( substrate[i] > *subdop )  {
		*subdop = substrate[i];

/*
		 only case where p_type is for Boron substrate.
		   n-type -> *type = 0
		   p-type -> *type = 1
*/

		
		if ( imptype[i] != 1 )  
		    *type = 0;
	    }

	*peakcon = 0.0;
	peak_loc = 0.0;

	for ( i = topnode_silicon; ((i < nnodes) && (!done)); i++ )  {
	    if ( fabs(impactive[i]) > *peakcon )  {
		*peakcon = fabs(impactive[i]);
		peak_loc = xarray[i];
	    }
	    if ( (fabs(impactive[i]) <= *subdop) && (!done) )  {
		*junc = xarray[i] - xarray[topnode_silicon];
  *std = (*junc - (peak_loc - xarray[topnode_silicon]))/sqrt(log(*peakcon/(*subdop)));
		done = 1;
	    }
	}
	fclose(fp);	
	return(0);
}
