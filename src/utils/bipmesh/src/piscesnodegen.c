/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	piscesnodegen.c		Version 1.5		*/
/*	Last Modification:	1/29/90 01:40:07		*/

#include <stdio.h>
#include <math.h>
#include "struct.h"


extern double dmax();
extern double dmin();
extern double calcspacing();
extern double depl_width();
extern double xdmax();
extern doublenode();
extern double calcratio();
extern debug;

#ifdef ANSI_FUNC

int 
piscesnodegen (void)
#else

piscesnodegen()
#endif

/* generate internal grid for a bipolar structure.  to make life easier
 *	on the cpu, assume a symmetric structure.  thus the right edge is
 *	treated as a reflecting boundary
 */

{
    double offset;
    double last;
    double base_left;
    double emitter_left;

    /*  add in eliminate information later */
    elnum = 0;
    elim[0].xlo = 1;
    elim[0].ylo = 1;

    offset = - ceil( lat_info.collector_width/2. + 
	2. * vert_info.collector_depth );
    
    /* lay out horizontal grid first */
    xnum = 0;

    xnd[xnum].num = 1;
    xnd[xnum].loc = offset;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 2.0;
    xnum++;

    /* collector edge */
    xnd[xnum].num = 2;
    xnd[xnum].loc = - lat_info.collector_width/2.;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 2.0;
    electrode[2].xlo = xnd[xnum].num;
    electrode[2].ylo = 1;
    electrode[2].yhi = 1;
    xnum++;

    /* collector contact */
    if (lat_info.contact_edge_spacing > 0.0)  {
	xnd[xnum].num = 3;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[2].xlo = xnd[xnum].num;
	xnum++;
    }

    xnd[xnum].num = 4;
    xnd[xnum].loc = last = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    electrode[2].xhi = xnd[xnum].num;
    xnum++;

    /* fix left edge of base region using global geometric considerations */
    xnd[xnum].num = 6;
    xnd[xnum].loc = base_left = -lat_info.collector_width/2. +
	lat_info.contact_width + 
	dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) +
	0.5 * (lat_info.collector_width - lat_info.base_width -
	lat_info.contact_width - 
	dmax(3. * lat_info.contact_edge_spacing, lat_info.contact_width));
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    xnum++;

    /* emitter edge */
    /* fix emitter edge using global geometric considerations */
    emitter_left = dmax(base_left + 0.5 * (lat_info.base_width -
	    dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) -
	    lat_info.contact_width - lat_info.emitter_width), 
	    base_left + lat_info.e_c_spacing);
    if (emitter_left > base_left)  {
	xnd[xnum].num = 8;
	xnd[xnum].loc = emitter_left;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[0].xlo = xnd[xnum].num;
    xnum++;
    }
    else  
	electrode[0].xlo = xnd[xnum-1].num;

    if (lat_info.contact_edge_spacing > 0.0)  {
	xnd[xnum].num = xnd[xnum-1].num + 1;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[0].xlo = xnd[xnum].num;
	xnum++;
    }

    /* assume contact-to-contact spacing if emitter contact is too close
     *	to the collector
     */
    if ( (xnd[xnum-1].loc - last) < lat_info.contact_width )  {
        /* add some space so that we don't violate design rule */
        xnd[xnum].num = xnd[xnum-1].num + 1;
        xnd[xnum].loc = last + lat_info.contact_width;
        xnd[xnum].mat = SILICON;
        xnd[xnum].rat = 1.0;
	electrode[0].xlo = xnd[xnum].num;
        xnum++;
    }


    /* emtter contact */
    xnd[xnum].num = xnd[xnum-1].num + 2;
    xnd[xnum].loc = last  = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    electrode[0].xhi = xnd[xnum].num;
    electrode[0].ylo = 1;
    electrode[0].yhi = 1;
    xnum++;

    /* emitter edge */
    if (xnd[xnum-1].loc < emitter_left + lat_info.emitter_width)  {
	xnd[xnum].num = xnd[xnum-1].num + 2;
	xnd[xnum].loc = emitter_left + lat_info.emitter_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[1].xlo = xnd[xnum].num;
	xnum++;
    }

    if (lat_info.contact_edge_spacing > 0.0)  {
	xnd[xnum].num = xnd[xnum-1].num + 2;
	xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_edge_spacing; 
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[1].xlo = xnd[xnum].num;
	xnum++;
    }

    /* assume contact-to-contact spacing if base contact is too close
     *	to the emitter
     */
    if ( (xnd[xnum-1].loc - last) < lat_info.contact_width )  {
	xnd[xnum].num = xnd[xnum-1].num + 2;
	xnd[xnum].loc = last + lat_info.contact_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	electrode[1].xlo = xnd[xnum].num;
	xnum++;
    }


    /* base contact */
    xnd[xnum].num = xnd[xnum-1].num + 2;
    xnd[xnum].loc = xnd[xnum-1].loc + lat_info.contact_width;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 1.0;
    electrode[1].ylo = 1;
    electrode[1].yhi = 1;
    electrode[1].xhi = xnd[xnum].num;
    xnum++;

    if (xnd[xnum-1].loc < base_left + lat_info.base_width)  {
	xnd[xnum].num = xnd[xnum-1].num + 1;
	xnd[xnum].loc = base_left + lat_info.base_width;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	xnum++;
    }

    /* base edge */
    /* collector edge */
    if (xnd[xnum-1].loc < lat_info.collector_width/2.)  {
	xnd[xnum].num = xnd[xnum-1].num + 1;
	xnd[xnum].loc = lat_info.collector_width/2.;
	xnd[xnum].mat = SILICON;
	xnd[xnum].rat = 1.0;
	xnum++;
    }

    xnd[xnum].num = xnd[xnum-1].num + 1;
    xnd[xnum].loc = -offset;
    xnd[xnum].mat = SILICON;
    xnd[xnum].rat = 2.0;
    xnum++;


    /* grid in vertical direction */
    ynum = 0;

    ynd[ynum].num = 1;
    ynd[ynum].loc = 0.0;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    ynum++;

    ynd[ynum].num = 5;
    ynd[ynum].loc = vert_info.emitter_depth;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    ynum++;

    ynd[ynum].num = 12;
    ynd[ynum].loc = vert_info.int_base_depth;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.0;
    ynum++;

    ynd[ynum].num = 17;
    ynd[ynum].loc = vert_info.collector_depth;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.25;
    ynum++;

    /*  do a check if vert_info.substrate_depth is 0.  this can occur if
     *   an analytic profile was given.  In this case just extend the 
     *	 substrate to the nearest micron
     */
    if ( vert_info.substrate_depth == 0.0 )
	vert_info.substrate_depth = floor( vert_info.collector_depth + 1.01 );

    ynd[ynum].num = 20;
    ynd[ynum].loc = vert_info.substrate_depth;
    ynd[ynum].mat = SILICON;
    ynd[ynum].rat = 1.25;
    ynum++;

    return( 1 );
} 
