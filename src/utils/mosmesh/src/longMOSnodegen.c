/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



#include <stdio.h>
#include <math.h>
#include "struct.h"


extern xnum;
extern ynum;
extern elnum;
extern lat_str lat_info;
extern vert_str vert_info;
extern node_str xnd[50];
extern node_str ynd[50];
extern elim_str elim[10];
extern dop_str dop_data[10];
extern double RATIO_LAT;
extern double dmax();
extern double dmin();
extern double calcspacing();
extern double depl_width();
extern double xdmax();
extern doublenode();
extern double calcratio();
extern debug;


longMOS()

/*  generate internal grid for a short channel MOS structure */

{
    register int i;
    double offset;
    double std1;
    double std2;
    double tmp;
    double psi0;
    int num;
    int done;
    double back;
    double chan_back;
    double rat;
    double contact_width = 0.1;
    double distance;
    double spacing;
    double sd_width12;	
    double sd_width22;
    double sd_width_drain;
    double sd_junc;
    double lat_sd_junc;
    double grid_spac;
    double minimum = 0.01;
    double zero = 0.0;
    double sdtol = minimum * (double)APPROXTOSTD;


/*  set up rectangular grid in the lateral(x) direction 
 *    assume that the grid will be symmetric with respect to
 *    the origin.
 */


/*  eliminate nodes that are not necessary:  eliminate nodes in x-direction
 *    for y > junction depth.
 *
 *  eliminate nodes in x direction under channel y > channel depth
 */

    xnum = 0;
    elnum = 0;
    elim[elnum+1].xlo = 1;

    /*  set relative shift of origin from left edge of device */
    offset = -(contact_width + lat_info.poly_metal + 0.5 * lat_info.poly_width);


    /* first 2 grid points must be at edges of the source(left) contact */
    xnd[xnum].num = 1;
    xnd[xnum].loc = offset;
    xnd[xnum].mat = OXIDE;
    xnd[xnum].rat = 1.0;
    xnum++;

    xnd[xnum].num = 2;
    xnd[xnum].loc = offset + contact_width;
    xnd[xnum].mat = OXIDE;
    xnd[xnum].rat = 1.0;
    xnum++;


    /* add grid out to edge of source depletion edge. use enough resolution
     *   so that doping profile comes out reasonably
     */

    /*  source depletion width must be less than the width possible
     *   if the heavily doped S/D is right at the gate edge.  the
     *   extra terms are due to reflect the distance to the depletion
     *   region edge from the current position (edge of contact)
     */
    sd_junc =  dmax( vert_info.phos_junc, vert_info.ars_junc );
    lat_sd_junc = RATIO_LAT * sd_junc;

    switch ( dop_data[1].type )  {
	case N_TYPE  :
	case P_TYPE  :
	    		std1 = dop_data[1].dop_tag.anal_dop.std_dev;
        		sd_width12 = depl_width( vert_info.drain_sub_bias, 
				vert_info.substrate_dop, 
    				dop_data[1].dop_tag.anal_dop.peak_dop );
		        break;

	case SUPREM3ASCII  :
	case SUPREM3EXPORT  :
	    		std1 = dop_data[1].dop_tag.sup3_dop.std_dev;
        		sd_width12 = depl_width( vert_info.drain_sub_bias, 
				vert_info.substrate_dop, 
    				dop_data[1].dop_tag.sup3_dop.peak_dop );
		        break;

	case NONE	:
			std1 = 1.0e20;  /* use a large number */
			sd_width12 = zero; /* use a small number */
		        break;

	default  :
		fprintf( stderr, "\tillegal case\n" );
		exit( ERROR );
		break;
    } /* end switch */

    switch ( dop_data[2].type )  {
	case N_TYPE  :
	case P_TYPE  :
	    		std2 = dop_data[2].dop_tag.anal_dop.std_dev;
        		sd_width22 = depl_width( vert_info.drain_sub_bias, 
				vert_info.substrate_dop, 
    				dop_data[2].dop_tag.anal_dop.peak_dop ) 
				- lat_info.spacer;
		        break;

	case SUPREM3ASCII  :
	case SUPREM3EXPORT  :
	    		std2 = dop_data[2].dop_tag.sup3_dop.std_dev;
        		sd_width22 = depl_width( vert_info.drain_sub_bias, 
				vert_info.substrate_dop, 
    				dop_data[2].dop_tag.sup3_dop.peak_dop ) 
				- lat_info.spacer; 
		        break;

	case NONE	:
			fprintf( stderr, "\tmust have a S/D profile\n" );
			exit( ERROR );
		        break;

	default  :
		fprintf( stderr, "\tillegal case\n" );
		exit( ERROR );
		break;
    } /* end switch */

    grid_spac = dmin( std1, std2 );
    sd_width_drain = dmax( sd_width12, sd_width22 );


/*  if a spacer grade to spacer edge.  need this point to specify the edge
 *    of the S/D implant
 */
    if ( lat_info.spacer > zero )  {
        /* grade down to spacer edge */
	rat = 1.25;
	distance = lat_info.poly_metal - lat_info.spacer;
	doublenode( rat, contact_width, 5.0*grid_spac, distance,
	    &(xnd[xnum].num), &(xnd[xnum].loc), &(xnd[xnum].rat),
	    &(xnd[xnum+1].num), &(xnd[xnum+1].loc), &(xnd[xnum+1].rat),
	    &(xnd[xnum+2].num), &(xnd[xnum+2].loc), &(xnd[xnum+2].rat) );

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
	}

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
  	}


	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
	    xnd[xnum].mat = OXIDE;
	    xnum++;
	}


        rat = 1.0/1.25;
	distance = lat_info.spacer;
        xnd[xnum].num = xnd[xnum-1].num + calcnode( distance, 
		rat, zero, 2.0 * grid_spac );
        xnd[xnum].loc = xnd[xnum-1].loc + distance;
        xnd[xnum].mat = POLY;
        xnd[xnum].rat = rat;
        elim[elnum].xlo = xnd[xnum].num;
        xnum++;
    }
    else  {
    /*  just grade to poly edge directly */
	rat = 1.25;
	distance = lat_info.poly_metal;
	doublenode( rat, contact_width, 2.0*grid_spac, distance,
	    &(xnd[xnum].num), &(xnd[xnum].loc), &(xnd[xnum].rat),
	    &(xnd[xnum+1].num), &(xnd[xnum+1].loc), &(xnd[xnum+1].rat),
	    &(xnd[xnum+2].num), &(xnd[xnum+2].loc), &(xnd[xnum+2].rat) );

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
	    if ( (xnd[xnum+1].loc == 0) && (xnd[xnum+2].loc == 0) )  {
       		elim[elnum].xlo = xnd[xnum].num;
		xnd[xnum].mat = POLY;
	    }
	    else
                xnd[xnum].mat = OXIDE;
	    xnum++;
	}

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
	    if ( xnd[xnum+1].loc == 0 )  {
    		elim[elnum].xlo = xnd[xnum].num;
		xnd[xnum].mat = POLY;
	    }
 	    else
                xnd[xnum].mat = OXIDE;
	    xnum++;
  	}


	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
    	    elim[elnum].xlo = xnd[xnum].num;
	    xnd[xnum].mat = POLY;
	    xnum++;
	}

    }



    /* grade down to estimated lateral source junction */
    distance = lat_sd_junc;
    rat = calcratio( 2.0 * grid_spac, grid_spac, distance, &(xnd[xnum].num) );
    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
    xnd[xnum].loc = xnd[xnum-1].loc + distance;
    xnd[xnum].mat = POLY;
    xnd[xnum].rat = rat;
    xnum++;

    /*  grade out to center of channel.  for a wide depletion region use
     *   fine uniform spacing.  else grade out slightly
     */
    if ( sd_width_drain > lat_info.poly_width/2.0 )  {
    /* grade out at grid_spac to drain junction */
	rat = 1.0;
	distance = 2.0 * (lat_info.poly_width/2.0 - lat_sd_junc);
	xnd[xnum].num = xnd[xnum-1].num + calcnode( distance, rat, zero, 
	    grid_spac );
	xnd[xnum].loc = xnd[xnum-1].loc + distance;
        xnd[xnum].mat = POLY;
        xnd[xnum].rat = rat;
        xnum++;
    }
    else  {
	distance = lat_info.poly_width/2.0 - lat_sd_junc;
        rat = calcratio( grid_spac, 2.0 * grid_spac, distance, &(xnd[xnum].num) );
	xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	xnd[xnum].loc = xnd[xnum-1].loc + distance;
        xnd[xnum].mat = POLY;
        xnd[xnum].rat = rat;
        xnum++;
     /* now grade in towards the drain junction. assume that the
      *  drain junction makes the edge of the gate
      */
        distance = lat_info.poly_width/2.0 - lat_sd_junc;
        rat = calcratio( 2.0 * grid_spac, grid_spac, distance, &(xnd[xnum].num) );
        xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
        xnd[xnum].loc = xnd[xnum-1].loc + distance;
        xnd[xnum].mat = POLY;
        xnd[xnum].rat = rat;
        xnum++;
    }

    /*  grade out towards the poly edge */
    distance = lat_sd_junc;
    rat = calcratio( grid_spac, 2.0 * grid_spac, distance, &(xnd[xnum].num) );
    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
    xnd[xnum].loc = xnd[xnum-1].loc + distance;
    xnd[xnum].mat = POLY;
    xnd[xnum].rat = rat;
    elim[elnum].xhi = xnd[xnum].num;
    xnum++;


    /*  if there is a spacer add a point there and continue gridding
     *    out to the drain contact.  else just grade out directly
     */
    if ( lat_info.spacer > zero )  {
        /* grade down to spacer edge */
	distance = lat_info.spacer;
        rat = calcratio( 2.0 * grid_spac, 5.0 * grid_spac, distance, 
	    &(xnd[xnum].num) );
        xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
        xnd[xnum].loc = xnd[xnum-1].loc + distance;
        xnd[xnum].mat = OXIDE;
        xnd[xnum].rat = rat;


        xnum++;

	rat = 1.25;
	distance = lat_info.poly_metal - lat_info.spacer;
        doublenode( rat, 5.0 * grid_spac, contact_width, distance,
	    &(xnd[xnum].num), &(xnd[xnum].loc), &(xnd[xnum].rat),
	    &(xnd[xnum+1].num), &(xnd[xnum+1].loc), &(xnd[xnum+1].rat),
	    &(xnd[xnum+2].num), &(xnd[xnum+2].loc), &(xnd[xnum+2].rat) );

	
	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
	}

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
  	}


	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
	    xnd[xnum].mat = OXIDE;
	    xnum++;
	}

    }
    else  {
    /*  just grade to poly edge directly */
	rat = 1.25;
	distance = lat_info.poly_metal;
        doublenode( rat, 2.0 * grid_spac, contact_width, distance,
	    &(xnd[xnum].num), &(xnd[xnum].loc), &(xnd[xnum].rat),
	    &(xnd[xnum+1].num), &(xnd[xnum+1].loc), &(xnd[xnum+1].rat),
	    &(xnd[xnum+2].num), &(xnd[xnum+2].loc), &(xnd[xnum+2].rat) );

	
	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
	}

	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
            xnd[xnum].mat = OXIDE;
	    xnum++;
  	}


	if ( xnd[xnum].loc != zero )  {
	    xnd[xnum].num = xnd[xnum-1].num + xnd[xnum].num;
	    xnd[xnum].loc = xnd[xnum-1].loc + xnd[xnum].loc;
	    xnd[xnum].mat = OXIDE;
	    xnum++;
	}
    }

    /*  add the last point at the right edge */
    rat = 1.0;
    xnd[xnum].num = xnd[xnum-1].num + 1;
    xnd[xnum].loc = xnd[xnum-1].loc + contact_width;
    xnd[xnum].mat = OXIDE;
    xnd[xnum].rat = rat;

    elim[elnum+1].xhi = xnd[xnum].num;
    xnum++;


    /*y direction nodes*/
    /*  keep ratio between grids small.  */
    ynum = 0;

    ynd[ynum].num = 1;
    ynd[ynum].loc = -vert_info.oxide_thick;
    ynd[ynum].mat = OXIDE;
    ynd[ynum].rat = 1.0;
    ynum++;

    /*  assuming that the inversion layer thickness is 100 angstroms,
     *    add enough grid points in the oxide to keep the changes
     *    gradual, let's say 25%
     */

    distance = vert_info.oxide_thick;
    rat = 1.0/1.25;
    ynd[ynum].num = ynd[ynum-1].num + calcnode( distance, rat, zero, minimum );
    ynd[ynum].loc = zero;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = rat;
    ynum++;

    ynd[ynum].num = ynd[ynum-1].num + 1;
    ynd[ynum].loc = 0.01;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.00;
    elim[elnum].ylo = ynd[ynum].num + 1;
    ynum++;



    /*ratio is 1.25!!!*/
    rat = 1.25;

    done = 0;
    /* iterate on rat parameter until final spacing is near grid_spac
     *  (doping standard deviation).  theoretically one could
     *  solve implicitly for the correct solution but due to round-off
     *  and the nonlinearity of the equation, it's better to iterate
     *  for now.
     */
    while ( (!done) && (rat > 1.02) )  {
        /*calculate with the spacing at the beginning of the interval*/
	distance = sd_junc - ynd[ynum-1].loc;
        num = calcnode( distance, rat, minimum, zero );
	tmp = calcspacing( minimum, rat, num );
	if ( debug == 1 ) {
	    printf( "\trat is %f, tmp is %f\n", rat, tmp );
	    fflush( stdout );
	}
	if ( tmp >= ( ((double)1.0 + sdtol) * grid_spac )   )
	   rat = rat - minimum;
	else
	   done = 1;
    }

    ynd[ynum].num = ynd[ynum-1].num + num;
    ynd[ynum].loc = ynd[ynum-1].loc + distance;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = rat;
    ynum++;

    elnum += 2; 
    for(i = 1; i < elnum; i++) elim[i].ylo = ynd[ynum-1].num;

    /*calculate maximum depletion width*/
    psi0 = 0.026 * log( vert_info.substrate_dop / 1.45e10 );
    back = sqrt(2.0 * 8.854e-14 * 11.8 * (psi0 + vert_info.drain_sub_bias) /
	       (1.62e-19 * vert_info.substrate_dop) ) * 1.0e4
	       + sd_junc;
    chan_back = xdmax( vert_info.substrate_dop ) * 1.0e4;
    back = dmax( back, chan_back );

    /*ratio is 1.25!!!*/
    rat = 1.25;

    distance = back - sd_junc;
    num = calcnode( distance, rat, tmp, zero );

    ynd[ynum].num = ynd[ynum-1].num + num;
    ynd[ynum].loc = ynd[ynum-1].loc + distance;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = rat;
    ynum++;


/*  add a few more points to extend the silicon depth to the next nearest 
 *    micron
 */
    spacing = calcspacing( tmp, rat, ynd[ynum-1].num - ynd[ynum-2].num );
    distance = floor( ynd[ynum-1].loc + 1.0 ) - ynd[ynum-1].loc;
    ynd[ynum].num = ynd[ynum-1].num + calcnode( distance, rat, spacing,
	zero );
    ynd[ynum].loc = ynd[ynum-1].loc + distance;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = rat;
    ynum++;


    if ( ynd[ynum-1].loc < vert_info.substrate_depth )  {
	spacing = calcspacing( spacing, rat, ynd[ynum-1].num - ynd[ynum-2].num);
	rat = 1.0;
	distance = floor(vert_info.substrate_depth - ynd[ynum-1].loc) + 1.0;
	ynd[ynum].num = ynd[ynum-1].num + calcnode(distance, rat, spacing, zero);
	ynd[ynum].loc = ynd[ynum-1].loc + distance;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = rat;
    }

    for(i = 0; i < elnum; i++) elim[i].yhi = ynd[ynum-1].num;

    if ( debug == 1 )  {
	printf( "\telnum is %d\n", elnum );
	for (i = 0; i < elnum; i++ )
	    printf( "\telim[%d].yhi = %d .ylo = %d .xhi = %d .xlo = %d\n",
		i, elim[i].yhi, elim[i].ylo, elim[i].xhi, elim[i].xlo );
    }
}
