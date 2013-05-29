/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	stride_node.c		Version 1.6		*/
/*	Last Modification:	1/29/90 01:40:15		*/

#include <stdio.h>
#include <math.h>
#include "struct.h"

#define FALSE 0
#define TRUE 1

extern double RATIO_LAT;
extern double dmax();
extern double dmin();
extern double calcspacing();
extern double depl_width();
extern double xdmax();
extern doublenode();
extern double calcratio();
extern debug;

stride_node()

/* generate internal grid for a bipolar structure.  to make life easier
 *	on the cpu, assume a symmetric structure.  thus the right edge is
 *	treated as a reflecting boundary
 */

{
    double offset;
    double spacing = 0.5;
    double last;   /* location of last contact */
    double base_left;
    double emitter_left;
    double lat_edge = 0.5;
    double min_spacing = 0.20;
    double loc;
    plane_str *plane;
    area_str *area;
    stride_dopstr *str_ptr;
    int i;
    int done;

    /*  add in eliminate information later */
    elnum = 0;
    elim[0].xlo = 1;
    elim[0].ylo = 1;

    offset = - ceil( lat_info.collector_width/2. 
	+ 2. * vert_info.collector_depth );
    
    /* grid in x direction */
    xnum = 0;

    xnd[xnum].num = 1;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = offset;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    electrode[3].xlo = xnd[xnum].localnum;
    xnum++;

    /* edge of collector mask */
    i = (int)ceil(2 * vert_info.collector_depth / spacing);
    xnd[xnum].num = i + 1;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = - lat_info.collector_width/2.;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    spacing = fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i;
    electrode[2].xlo = xnd[xnum].localnum;
    electrode[2].zlo = 1;
    electrode[2].zhi = 1;
    xnum++;

    /* collector contact */
    if (lat_info.contact_edge_spacing > 0.0)  {
	i = (int)ceil(lat_info.contact_edge_spacing / spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing, 
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	electrode[2].xlo = xnd[xnum].localnum;
	xnum++;
    }

    i = (int)ceil(lat_info.contact_width / spacing);
    xnd[xnum].num = xnd[xnum-1].num + i;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = last = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    spacing = dmax(min_spacing,
	fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
    electrode[2].xhi = xnd[xnum].localnum;
    xnum++;

    /* fix left edge of base region using global geometric considerations */
    xnd[xnum].loc = base_left = -lat_info.collector_width/2. +
        lat_info.contact_width +
        dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) +
        0.5 * (lat_info.collector_width - lat_info.base_width -
        lat_info.contact_width -
        dmax(3. * lat_info.contact_edge_spacing, lat_info.contact_width));
    i = (int)ceil((base_left - xnd[xnum-1].loc)/ spacing);
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].num = xnd[xnum-1].num + i;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    spacing = dmax(min_spacing, 
	fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
    xnum++;

    /* emitter edge */
    /* fix emitter edge using global geometric considerations */
    emitter_left = dmax(base_left + 0.5 * (lat_info.base_width -
            dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) -
            lat_info.contact_width - lat_info.emitter_width),
            base_left + lat_info.e_c_spacing);
    if ((emitter_left - base_left) >= min_spacing)  {
	xnd[xnum].localnum = xnum + 1;
	i = (int)ceil(emitter_left - xnd[xnum-1].loc / spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
        xnd[xnum].loc = emitter_left;
        xnd[xnum].mat = SILICON;
        xnd[xnum].rat = 1.0;
        electrode[0].xlo = reg[2].xlo = xnd[xnum].localnum;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
    xnum++;
    }
    else  
        electrode[0].xlo = reg[2].xlo = xnd[xnum-1].localnum;


    /* emitter contact */
    if (lat_info.contact_edge_spacing > 0.0)  {
	i = (int)ceil(lat_info.contact_edge_spacing / spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
        electrode[0].xlo = xnd[xnum].localnum;
	xnum++;
    }

    if ( (xnd[xnum-1].loc - last) < lat_info.contact_width )  {
	/* add some space so that we don't violate design rule */
	i  = (int)ceil((lat_info.contact_width + last - xnd[xnum-1].loc)
	    /spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = last + lat_info.contact_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing, 
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
        electrode[0].xlo = xnd[xnum].localnum;
	xnum++;
    }

    i = (int)ceil(lat_info.contact_width / spacing);
    xnd[xnum].num = xnd[xnum-1].num + i;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = last = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    spacing = dmax(min_spacing,
	fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
    electrode[0].xhi = xnd[xnum].localnum;
    electrode[0].zlo = 1;
    electrode[0].zhi = 1;
    xnum++;

    /* emitter edge */
    if (xnd[xnum-1].loc < emitter_left + lat_info.emitter_width)  {
	i = (int)ceil((emitter_left + lat_info.emitter_width - xnd[xnum-1].loc)
		 / spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = reg[2].xhi = xnum + 1;
	xnd[xnum].loc = emitter_left + lat_info.emitter_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing, 
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	xnum++;
    }
    else
	reg[2].xhi = xnd[xnum-1].localnum;

    /* edge of external base mask */

    if (lat_info.contact_edge_spacing > 0.0)  {
	i = (int)ceil(lat_info.contact_edge_spacing / spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing; 
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	electrode[1].xlo = xnd[xnum].localnum;
	xnum++;
    }

    if ( (xnd[xnum-1].loc - last) < lat_info.contact_width )  {
	/* add some space so that we don't violate design rule */
	i  = (int)ceil((lat_info.contact_width + last - xnd[xnum-1].loc)
	    /spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = last + lat_info.contact_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	electrode[1].xlo = xnd[xnum].localnum;
	xnum++;
    }

    /* base contact */
    i = (int)ceil(lat_info.contact_width / spacing);
    xnd[xnum].num = xnd[xnum-1].num + i;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = last = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    spacing = dmax(min_spacing,
	fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
    electrode[1].xhi = xnd[xnum].localnum;
    electrode[1].zlo = 1;
    electrode[1].zhi = 1;
    xnum++;

    /* force edge  of base mask */
    if (xnd[xnum-1].loc < base_left + lat_info.base_width)  {
	i = (int)ceil((base_left + lat_info.base_width - xnd[xnum-1].loc) /
		spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = base_left + lat_info.base_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	xnum++;
    }

    /* force edge of collector mask */
    /* end of external base mask */
    if (xnd[xnum-1].loc < lat_info.collector_width/2.)  {
	spacing *= 1.5;
	i = (int)ceil((lat_info.collector_width/2. - xnd[xnum-1].loc)/spacing);
	xnd[xnum].num = xnd[xnum-1].num + i;
	xnd[xnum].localnum = xnum + 1;
	xnd[xnum].loc = lat_info.collector_width/2.;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	spacing = dmax(min_spacing,
		fabs(xnd[xnum].loc - xnd[xnum-1].loc)/(double)i);
	xnum++;
    }

    spacing *= 1.5;
    xnd[xnum].localnum = xnum + 1;
    xnd[xnum].loc = -offset;
    i = (int)ceil((xnd[xnum].loc - xnd[xnum-1].loc) / spacing);
    xnd[xnum].num = xnd[xnum-1].num + i;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    electrode[3].xhi = xnd[xnum].localnum;
    xnum++;

    /* grid in y-direction */
    ynum = 0;
    spacing = 0.5;

    ynd[ynum].num = 1;
    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = -(lat_info.collector_length/2. + vert_info.collector_depth);
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    ynum++;

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = -lat_info.collector_length/2.;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    loc =  -(lat_info.base_length/2. +
	    dmax(vert_info.int_base_depth, vert_info.ext_base_depth)) -
	    ynd[ynum-1].loc;

    if (((loc - ynd[ynum-1].loc) > min_spacing) &&
	    ((-lat_info.base_length/2. - loc) > min_spacing))  {
	ynd[ynum].localnum = ynum + 1;
	ynd[ynum].loc = -(lat_info.base_length/2. + 
	    dmax(vert_info.int_base_depth,vert_info.ext_base_depth));
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = -lat_info.base_length/2.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    loc = -(lat_info.emitter_length/2. + vert_info.emitter_depth/2.);

    if ( ((loc - ynd[ynum-1].loc) > min_spacing)  &&
	    ((-lat_info.emitter_length/2. - loc) > min_spacing) )  {
	ynd[ynum].localnum = ynum + 1;
	ynd[ynum].loc = -(lat_info.emitter_length/2. + 
	    vert_info.emitter_depth/2.);
	ynd[ynum].num = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = electrode[0].ylo = electrode[1].ylo =
		electrode[2].ylo = reg[2].ylo = ynum + 1;
    ynd[ynum].loc = -lat_info.emitter_length/2.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    if (lat_info.contact_length < lat_info.emitter_length)  {
	ynd[ynum].localnum = electrode[0].ylo = electrode[1].ylo =
		electrode[2].ylo = ynum + 1;
	ynd[ynum].loc = -lat_info.contact_length/2.;
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = 0.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    if (lat_info.contact_length < lat_info.emitter_length)  {
	ynd[ynum].localnum = electrode[0].yhi = electrode[1].yhi =
		electrode[2].yhi = ynum + 1;
	ynd[ynum].loc = lat_info.contact_length/2.;
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = reg[2].yhi = ynum + 1;
    if (electrode[0].yhi == 0)
	electrode[0].yhi = electrode[1].yhi = electrode[2].yhi = ynum + 1;
    ynd[ynum].loc = lat_info.emitter_length/2.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    loc = lat_info.emitter_length/2. + vert_info.emitter_depth;

    if (((loc - lat_info.emitter_length/2.) > min_spacing) &&
	    ((lat_info.base_length/2. - loc) > min_spacing))  {
	ynd[ynum].localnum = ynum + 1;
	ynd[ynum].loc = lat_info.emitter_length/2. + 
	    vert_info.emitter_depth;
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = lat_info.base_length/2.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    loc = lat_info.base_length/2. + 
	dmax(vert_info.int_base_depth,vert_info.ext_base_depth);
    if (((loc - lat_info.base_length/2.) > min_spacing) &&
	    ((lat_info.collector_length/2. - loc) > min_spacing))  {
	ynd[ynum].localnum = ynum + 1;
	ynd[ynum].loc = lat_info.base_length/2. + 
	    dmax(vert_info.int_base_depth,vert_info.ext_base_depth);
	i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
	ynd[ynum].num = ynd[ynum-1].num + i;
	ynd[ynum].mat = SILICON;
	ynd[ynum].rat = 1.0;
	spacing = dmax( min_spacing, 
	    fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
	ynum++;
    }

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = lat_info.collector_length/2.;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    spacing = dmax( min_spacing, 
	fabs(ynd[ynum].loc - ynd[ynum-1].loc)/(double)i);
    ynum++;

    ynd[ynum].localnum = ynum + 1;
    ynd[ynum].loc = lat_info.collector_length/2. + vert_info.collector_depth;
    i = (int)ceil((ynd[ynum].loc - ynd[ynum-1].loc)/spacing);
    ynd[ynum].num = ynd[ynum-1].num + i;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    ynum++;

    /* grid in vertical direction */
    znum = 0;

    znd[znum].num = 1;
    znd[znum].localnum = znum + 1;
    znd[znum].loc = 0.0;
    znd[znum].mat = SILICON;
    znd[znum].rat = 1.0;
    znum++;

    znd[znum].num = 5;
    znd[znum].localnum = znum + 1;
    znd[znum].loc = vert_info.emitter_depth;
    znd[znum].mat = SILICON;
    znd[znum].rat = 1.0;
    spacing = vert_info.emitter_depth/5.0;
    znum++;

    i = (int)ceil((vert_info.int_base_depth - vert_info.emitter_depth)/spacing/1.25);
    znd[znum].num = znd[znum-1].num + i;
    znd[znum].localnum = znum + 1;
    znd[znum].loc = vert_info.int_base_depth;
    znd[znum].mat = SILICON;
    znd[znum].rat = 1.0;
    spacing *= 1.25;
    znum++;

    i = (int)ceil((vert_info.collector_depth - vert_info.int_base_depth)/
	spacing/1.5);
    znd[znum].num = znd[znum-1].num + i;
    znd[znum].localnum = znum + 1;
    znd[znum].loc = vert_info.collector_depth;
    znd[znum].mat = SILICON;
    znd[znum].rat = 1.0;
    spacing *= 1.50;
    znum++;

    /*  do a check if vert_info.substrate_depth is 0.  this can occur if
     *   an analytic profile was given.  In this case just extend the 
     *	 substrate to the nearest micron
     */
    if ( vert_info.substrate_depth == 0.0 )
	vert_info.substrate_depth = floor( vert_info.collector_depth + 1.01 );

    i = (int)ceil((vert_info.substrate_depth - vert_info.collector_depth)/
	spacing/1.5);
    znd[znum].num = znd[znum-1].num + i;
    znd[znum].localnum = znum + 1;
    znd[znum].loc = vert_info.substrate_depth;
    znd[znum].mat = SILICON;
    znd[znum].rat = 1.0;
    znum++;


    /* set up region structure */
    num_regions = 3;

	/* silicon substrate */
    reg[0].xlo = 1;
    reg[0].xhi = xnum;
    reg[0].ylo = 1;
    reg[0].yhi = ynum;
    reg[0].zlo = 1;
    reg[0].zhi = ynum;
    reg[0].type = 0;

    /* nplane only necessary for type > 0 */
    reg[0].nplane = 0;

    reg[0].eps = 11.8;
    reg[0].ni0 = 1.45e10;

	/* surface oxide */
    reg[1].xlo = 1;
    reg[1].xhi = ynum;
    reg[1].ylo = 1;
    reg[1].yhi = ynum;
    reg[1].zlo = 1;
    reg[1].zhi = 2;
    reg[1].type = 0;

    /* nplane only necessary for type > 0 */
    reg[1].nplane = 0;

    reg[1].eps = 3.9;
    reg[1].ni0 = 0.0;

	/* poly emitter */
/*
    reg[2].xlo = findindex(xnd, -lat_info.collector_width/2.
	+ dmax(2 * lat_info.contact_edge_spacing + lat_info.contact_width
	+ lat_info.e_c_spacing, 2 * lat_info.contact_width), xnum) + 1;
    reg[2].xhi = findindex(xnd, xnd[reg[2].xlo].loc + lat_info.emitter_width, 
	xnum) + 1;
    reg[2].ylo = findindex(ynd, -lat_info.emitter_length, ynum) + 1;
    reg[2].yhi = findindex(ynd, lat_info.emitter_length, ynum) + 1;
*/
    reg[2].zlo = 1;
    reg[2].zhi = 2;
    reg[2].type = 0;

    /* nplane only necessary for type > 0 */
    reg[2].nplane = 0;

    reg[2].eps = 11.8;
    reg[2].ni0 = 1.45e10;



    /* do doping stuff */
    ndop = 4;

    /* emitter */
    str_ptr = &(dop_data[0].dop_tag.stride_dop);
    str_ptr->num_planes = 1;
    /* use info from region specification for the emitter */
    str_ptr->xlo = emitter_left;
    str_ptr->xhi = emitter_left + lat_info.emitter_width;
    str_ptr->ylo = -lat_info.emitter_length;
    str_ptr->yhi = lat_info.emitter_length;
    str_ptr->zlo = 0;
    str_ptr->zhi = 0;

    /* internal base */
    str_ptr = &(dop_data[1].dop_tag.stride_dop);
    str_ptr->num_planes = 1;
    str_ptr->xlo = base_left;
    str_ptr->xhi = base_left + lat_info.base_width;
    str_ptr->ylo = -lat_info.base_length;
    str_ptr->yhi = lat_info.base_length;
    str_ptr->zlo = 0;
    str_ptr->zhi = 0;

    /* collector */
    str_ptr = &(dop_data[2].dop_tag.stride_dop);
    str_ptr->num_planes = 1;
    str_ptr->xlo = -lat_info.collector_width/2.;
    str_ptr->xhi = lat_info.collector_width/2.;
    str_ptr->ylo = -lat_info.collector_length;
    str_ptr->yhi = lat_info.collector_length;
    str_ptr->zlo = 0;
    str_ptr->zhi = 0;

    /* external base. since the external base implant is not
     *  present in the active base region, we need to make
     *	a ring around the emitter. 
     */
    str_ptr = &(dop_data[3].dop_tag.stride_dop);
    str_ptr->num_planes = 0;
    
    /* get info off internal base mask */
    str_ptr->xlo = base_left + lat_edge;
    str_ptr->xhi = base_left + lat_info.base_width - lat_edge;
    str_ptr->ylo = lat_info.emitter_length/2. + lat_edge;
    str_ptr->yhi = lat_info.base_length/2. - lat_edge;
    str_ptr->zlo = 0.;
    str_ptr->zhi = 0.;
    if ( 	(str_ptr->xhi > str_ptr->xlo) &&
		(str_ptr->yhi > str_ptr->ylo) &&
		(str_ptr->zhi >= str_ptr->zlo) )
	str_ptr->num_planes++;
	

    if (str_ptr->num_planes == 0) {
	str_ptr->xlo = base_left + lat_edge;
	str_ptr->xhi = base_left + lat_info.base_width - lat_edge;
	str_ptr->ylo = -lat_info.emitter_length/2. - lat_edge;
	str_ptr->yhi = -lat_info.base_length/2. + lat_edge;
	str_ptr->zlo = 0.;
	str_ptr->zhi = 0.;
	if ( 	(str_ptr->xhi > str_ptr->xlo) &&
		(str_ptr->yhi > str_ptr->ylo) &&
		(str_ptr->zhi >= str_ptr->zlo) )
	    str_ptr->num_planes++;
    }
    else  {
	area = salloc(area_str, 1);
	area->xlo = base_left + lat_edge;
	area->xhi = base_left + lat_info.base_width - lat_edge;
	area->ylo = -lat_info.emitter_length/2. - lat_edge;
	area->yhi = -lat_info.base_length/2. + lat_edge;
	area->zlo = 0.;
	area->zhi = 0.;
	if ( 	(area->xhi > area->xlo) &&
		(area->yhi > area->ylo) &&
		(area->zhi >= area->zlo) )  {
	    str_ptr->planes[0] = area;
	    area = salloc(area_str, 1);
	    str_ptr->num_planes++;
	}
    }


    if (str_ptr->num_planes == 0)  {
	str_ptr->xlo = emitter_left + lat_info.emitter_width + lat_edge;
	str_ptr->xhi = base_left + lat_info.base_width - lat_edge;
	str_ptr->ylo = -lat_info.emitter_length;
	str_ptr->yhi = lat_info.emitter_length;
	str_ptr->zlo = 0;
	str_ptr->zhi = 0;
	if ( 	(str_ptr->xhi > str_ptr->xlo) &&
		(str_ptr->yhi > str_ptr->ylo) &&
		(str_ptr->zhi >= str_ptr->zlo) )
	    str_ptr->num_planes++;
    }
    else  {
	area->xlo = emitter_left + lat_info.emitter_width + lat_edge;
	area->xhi = base_left + lat_info.base_width - lat_edge;
	area->ylo = -lat_info.emitter_length;
	area->yhi = lat_info.emitter_length;
	area->zlo = 0;
	area->zhi = 0;
	if ( 	(area->xhi > area->xlo) &&
		(area->yhi > area->ylo) &&
		(area->zhi >= area->zlo) )  {
	    if (str_ptr->num_planes == 1)
		str_ptr->planes[0] = area;
	    else 
		str_ptr->planes[1] = area;
	    str_ptr->num_planes++;
	}
    }


    /* let's do electrode specifications */
    nelect = 3;
	/* emitter */
    electrode[0].dir = 0;
/*
    electrode[0].xlo = findindex(xnd, xnd[reg[2].xlo].loc + 
	lat_info.contact_edge_spacing, xnum) + 1;
    electrode[0].xhi = findindex(xnd, xnd[reg[2].xhi].loc - 
	lat_info.contact_edge_spacing, xnum) + 1;
    electrode[0].ylo = findindex(ynd, -lat_info.contact_length/2., ynum) + 1;
    electrode[0].yhi = findindex(ynd, lat_info.contact_length/2., ynum) + 1;
    electrode[0].zlo = 1;
    electrode[0].zhi = 1;
*/
    if (electrode[0].cnt == 2)  {
	if (inputmode == 1)
	    read_defreal( "work function for emitter electrode",
		&(electrode[0].work) );
	else
	    read_real( "work function for emitter electrode",
		&(electrode[0].work) );
    }
    if (electrode[0].shp != 0)  {
	if (electrode[0].shp == 1)  {
	    plane = salloc(plane_str, 1);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[0].pln = plane;
	}
	else if (electrode[0].shp == 2)  {
	    plane = salloc(plane_str, 2);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[0].pln = plane;
	}
    }
    electrode[0].pot = 0.0;  /* this will get set in bias routine */

	/* base */
    electrode[1].dir = 0;
/*
    electrode[1].xlo = findindex(xnd, last - lat_info.contact_width, xnum) + 1;
    electrode[1].xhi = findindex(xnd, last, xnum) + 1;
    electrode[1].ylo = electrode[0].ylo;
    electrode[1].yhi = electrode[0].yhi;
    electrode[1].zlo = 2;
    electrode[1].zhi = 2;
*/
    if (electrode[1].cnt == 2)  {
	if (inputmode == 1)
	    read_defreal( "work function for base electrode",
		&(electrode[1].work) );
	else
	    read_real( "work function for base electrode",
		&(electrode[1].work) );
    }
    if (electrode[1].shp != 0)  {
	if (electrode[1].shp == 1)  {
	    plane = salloc(plane_str, 1);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[1].pln = plane;
	}
	else if (electrode[1].shp == 2)  {
	    plane = salloc(plane_str, 2);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[1].pln = plane;
	}
    }
    electrode[1].pot = 0.0;  /* this will get set in bias routine */

	/* collector */
    electrode[2].dir = 0;
/*
    electrode[2].xlo = findindex(xnd, -lat_info.collector_width/2.
	+ lat_info.contact_edge_spacing, xnum) + 1;
    electrode[2].xhi = findindex(xnd, -lat_info.collector_width/2.
	+ lat_info.contact_edge_spacing + lat_info.contact_width, xnum) + 1;
    electrode[2].ylo = electrode[0].ylo;
    electrode[2].yhi = electrode[0].yhi;
    electrode[2].zlo = 2;
    electrode[2].zhi = 2;
*/
    if (electrode[2].cnt == 2)  {
	if (inputmode == 1)
	    read_defreal( "work function for collector electrode",
		&(electrode[2].work) );
	else
	    read_real( "work function for collector electrode",
		&(electrode[2].work) );
    }
    if (electrode[2].shp != 0)  {
	if (electrode[2].shp == 1)  {
	    plane = salloc(plane_str, 1);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[2].pln = plane;
	}
	else if (electrode[2].shp == 2)  {
	    plane = salloc(plane_str, 2);
      /* figure out how to fill this up later */
	    plane->num = 0;
	    plane->xlo = 0;
	    plane->xhi = 0;
	    plane->ylo = 0;
	    plane->yhi = 0;
	    plane->zlo = 0;
	    plane->zhi = 0;
	    electrode[2].pln = plane;
	}
    }
    electrode[2].pot = 0.0;  /* this will get set in bias routine */

    return( 1 );
} 
