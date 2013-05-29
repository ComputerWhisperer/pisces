/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/



/*	write_pisces.c		Version 1.6		*/
/*	Last Modification:	10/19/90 13:40:11		*/


#include <stdio.h>
#include "struct.h"

extern writecollectorIV();
extern writeanaldoping();
extern double dmax();

write_pisces( name ) 
char *name;
{ 
    FILE *fp;
    register int i;
    int type;
    double base_left;
    double emitter_left;
    double lat_edge = 0.5;
 
    char *dop_type[2];

    dop_type[N_TYPE] = "n.type";
    dop_type[P_TYPE] = "p.type";

    fp = fopen( name, "w" );

    if ( fp == NULL ) {
	printf("can't open the output file %s\n", name);
	exit(-1);
    }


    fprintf(fp, "title BIPMESH automatic mesh - Version 9040\n\n");


    fprintf(fp, "$starting mesh card\n" );
    fprintf(fp, "mesh nx=%d ny=%d rect\n\n", xnd[xnum-1].num, 
							ynd[ynum-1].num );

    fprintf(fp, "$x mesh locations\n");
    for(i = 0; i < xnum; i++) 
	fprintf(fp, "x.mesh node=%d location=%g rat=%g\n", 
		     xnd[i].num, xnd[i].loc, xnd[i].rat);
    fprintf(fp, "\n$y mesh locations\n");
    for(i = 0; i < ynum; i++) 
	fprintf(fp, "y.mesh node=%d location=%g rat=%g\n", 
		     ynd[i].num, ynd[i].loc, ynd[i].rat);

/*
    fprintf(fp, "\n$eliminate some nodes that aren't needed\n");
    for(i = 0; i < elnum; i++)
	fprintf(fp, "elim y.dir ix.l=%d ix.h=%d iy.l=%d iy.h=%d\n",
	     elim[i].xlo, elim[i].xhi, elim[i].ylo, elim[i].yhi ); 
*/


    fprintf(fp, "\n$region definitions\n");
    fprintf(fp, "region num=1 ix.l=1 ix.h=%d iy.l=1 iy.h=%d silicon\n",
		    xnd[xnum-1].num, ynd[ynum-1].num);


    fprintf(fp, "\n$Emitter = 1 Base = 2 Collector = 3 Bulk = 4\n");
    for (i = 0; i < 3; i++)  
	fprintf(fp, "electrode num=%d ix.l=%d ix.h=%d iy.l=%d iy.h=%d\n",
		i + 1, electrode[i].xlo, electrode[i].xhi,
		electrode[i].ylo, electrode[i].yhi);
    fprintf(fp, "electrode num=4 ix.l=1 ix.h=%d iy.l=%d iy.h=%d\n", 
			    xnd[xnum-1].num, ynd[ynum-1].num, ynd[ynum-1].num);


/*  print out doping information. get points from bjtnodegen.c  */

    fprintf( fp, "\n$Doping Information\n" );

    /* compute mask edges */
    base_left = -lat_info.collector_width/2. +
        lat_info.contact_width +
        dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) +
        0.5 * (lat_info.collector_width - lat_info.base_width -
        lat_info.contact_width -
        dmax(3. * lat_info.contact_edge_spacing, lat_info.contact_width));

    emitter_left = dmax(base_left + 0.5 * (lat_info.base_width -
            dmax(2. * lat_info.contact_edge_spacing, lat_info.contact_width) -
            lat_info.contact_width - lat_info.emitter_width), 
	    base_left + lat_info.e_c_spacing);



    switch ( dopingmode )  {

      case 0  :
	switch  (dop_data[2].type) {
	    case  N_TYPE :
	    case  PHOSPHORUS :
	    case  ARSENIC :
	    case  ANTIMONY  :
	    case  N_GAUSSIAN  :
	    case  N_ERFC  :	type = P_TYPE;
				break;

	    case  P_TYPE  :
  	    case  BORON  :
	    case  P_GAUSSIAN  :
	    case  P_ERFC  :	type = N_TYPE;
				break;
	}

        fprintf(fp, "doping reg=1 uniform x.lef=%g x.rig=%g\n",
		xnd[0].loc, xnd[xnum-1].loc );
        fprintf(fp, "+\ty.top=0.0 y.bot=%g conce=%e %s\n",
		ynd[ynum-1].loc, vert_info.substrate_dop, dop_type[type] );


	if ( dop_data[0].type != NONE )
	    writeanaldoping( fp, &(dop_data[0]), emitter_left,
	        emitter_left + lat_info.emitter_width,
		"Emitter Region" );
	    
        if ( dop_data[1].dop_tag.anal_dop.peak_dop != 0.0 )   
	    writeanaldoping( fp, &(dop_data[1]), base_left,
		base_left + lat_info.base_width, "Internal Base" );

        if ( (dop_data[3].dop_tag.anal_dop.peak_dop != 0.0) &&
		(emitter_left - base_left > 2. * lat_edge) )   
	    writeanaldoping( fp, &(dop_data[3]), base_left + lat_edge,
		emitter_left - lat_edge, "External Base" );

        if ( (dop_data[3].dop_tag.anal_dop.peak_dop != 0.0) &&
		(base_left + lat_info.base_width - (emitter_left +
		lat_info.emitter_width) > 2. * lat_edge) ) 
	    writeanaldoping( fp, &(dop_data[3]), emitter_left +
		lat_info.emitter_width + lat_edge, base_left + 
		lat_info.base_width - lat_edge, "External Base" );

	writeanaldoping( fp, &(dop_data[2]), -lat_info.collector_width/2.,
	    lat_info.collector_width/2., "Collector" );

        break;

      case 1  : 


	/* for suprem3 export format, we need a substrate */
	if ( (dop_data[0].type == SUPREM3EXPORT) &&
	     (dop_data[2].type == SUPREM3EXPORT) )  
	    fprintf(fp, "doping reg=1 uniform x.left=%g x.right=%g conc=%e %s\n\n",
		xnd[0].loc, xnd[xnum-1].loc, vert_info.substrate_dop, 
		dop_type[vert_info.substrate_dop_type]);



        /*emitter doping*/
	switch (dop_data[0].type)  {
		case SUPREM3ASCII  :
	    		writesup3ascii( fp, &(dop_data[0]), emitter_left,
	        		emitter_left + lat_info.emitter_width, 
				"Emitter Region" );
			break;


		case SUPREM3EXPORT :
	    		writesup3export( fp, &(dop_data[0]), emitter_left,
	        		emitter_left + lat_info.emitter_width, 
				"Emitter Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */
	    
        /* internal base region */
	switch (dop_data[1].type)  {
		case SUPREM3ASCII  :

	    		writesup3ascii( fp, &(dop_data[1]), base_left,
				base_left + lat_info.base_width, 
				"Internal Base Region" );
			break;


		case SUPREM3EXPORT :

	    		writesup3export( fp, &(dop_data[1]), base_left, 
				base_left + lat_info.base_width, 
				"Internal Base Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */

        /* external base region */
	switch (dop_data[3].type)  {
		case SUPREM3ASCII  :

			if (emitter_left - base_left > 2. * lat_edge)    
	    		    writesup3ascii( fp, &(dop_data[3]), base_left + 
				lat_edge, emitter_left - lat_edge, 
				"External Base Region" );
			break;


		case SUPREM3EXPORT :

			if (emitter_left - base_left > 2. * lat_edge)    
	    		    writesup3export( fp, &(dop_data[3]), base_left +
				lat_edge, emitter_left - lat_edge, 
				"External Base Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */

        /* external base region */
	switch (dop_data[3].type)  {
		case SUPREM3ASCII  :

			if (base_left + lat_info.base_width - 
			(emitter_left + lat_info.emitter_width) > 2. * lat_edge)
	    writesup3ascii( fp, &(dop_data[3]), emitter_left +
		lat_info.emitter_width + lat_edge , base_left + 
		lat_info.base_width - lat_edge, "External Base Region" );
			break;


		case SUPREM3EXPORT :

			if (base_left + lat_info.base_width - 
			(emitter_left + lat_info.emitter_width) > 2. * lat_edge)
	    writesup3export( fp, &(dop_data[3]), emitter_left +
		lat_info.emitter_width + lat_edge , base_left + 
		lat_info.base_width - lat_edge, "External Base Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */

        /* Collector Region */
	switch (dop_data[2].type)  {
		case SUPREM3ASCII  :
			writesup3ascii( fp, &(dop_data[2]), 
			    -lat_info.collector_width/2.,
	    		    lat_info.collector_width/2., "Collector Region" );
			break;


		case SUPREM3EXPORT :
			writesup3export( fp, &(dop_data[2]), 
			    -lat_info.collector_width/2.,
	    		    lat_info.collector_width/2., "Collector Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */

        break;


      case 2  :
	printf( "\tSuprem3 mode not implemented yet!!\n" );
	break;

    } /* end switch (dopingmode) */

    /* regrid on doping */
    fprintf( fp, "regrid log doping step=6 outf=mesh2\n");

    switch ( computemode )  {

	case 0  :	writecollectorIV( fp, mincollector, maxcollector, 
				minbase, maxbase, collectorincrement,
				baseincrement );
			break;
	case 1  :	writebaseIV( fp, maxcollector, minbase, maxbase, 
				baseincrement );
			break;
    } 


    fprintf( fp, "plot.2d grid bound \n" );
    fprintf( fp, "plot.2d no.tic bound junc l.elect=1 l.bound=2\n" );

    fprintf( fp, "end\n" );
    fclose( fp );
}
