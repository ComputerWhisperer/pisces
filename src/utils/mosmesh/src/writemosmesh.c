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
extern computemode;
extern double qf;
extern float mingate;
extern float maxgate;
extern float mindrain;
extern float maxdrain;
extern float drainincrement;
extern float gateincrement;
extern dopingmode;
extern writedrainIV();
extern writeanaldoping();

#ifdef ANSI_FUNC

int 
write_info (char *name)
#else

write_info( name ) 
char *name;
#endif
{ 
    FILE *fp;
    register int i;
    int oxend, pstrt, pend;
    char *dop_type[2];

    dop_type[N_TYPE] = "n.type";
    dop_type[P_TYPE] = "p.type";

    fp = fopen( name, "w" );

    if ( fp == NULL ) {
	printf("can't open the output file %s\n", name);
	exit(-1);
    }


    fprintf(fp, "title MOSMESH automatic mesh - Version 9030\n\n");


    fprintf(fp, "$starting mesh card\n" );
    fprintf(fp, "mesh nx=%d ny=%d rect outf=nmesh\n\n", xnd[xnum-1].num, 
							ynd[ynum-1].num );

    fprintf(fp, "$x mesh locations\n");
    for(i = 0; i < xnum; i++) 
	fprintf(fp, "x.mesh node=%d location=%g rat=%g\n", 
		     xnd[i].num, xnd[i].loc, xnd[i].rat);
    fprintf(fp, "\n$y mesh locations\n");
    for(i = 0; i < ynum; i++) 
	fprintf(fp, "y.mesh node=%d location=%g rat=%g\n", 
		     ynd[i].num, ynd[i].loc, ynd[i].rat);

    fprintf(fp, "\n$eliminate some nodes that aren't needed\n");
    for(i = 1; i < elnum; i++)
	fprintf(fp, "elim y.dir ix.l=%d ix.h=%d iy.l=%d iy.h=%d\n",
	     elim[i].xlo, elim[i].xhi, elim[i].ylo, elim[i].yhi ); 


    /*find the oxide interface*/
    for(oxend = i = 0; (i < ynum) && (oxend == 0); i++) 
	if ( ynd[i].mat != OXIDE ) oxend = ynd[i].num;

    fprintf(fp, "\n$region definitions\n");
    fprintf(fp, "region num=1 ix.l=1 ix.h=%d iy.l=1 iy.h=%d oxide\n",
		    xnd[xnum-1].num, oxend);
    fprintf(fp, "region num=2 ix.l=1 ix.h=%d iy.l=%d iy.h=%d silicon\n",
		    xnd[xnum-1].num, oxend, ynd[ynum-1].num);

    /*find the poly edges*/
    for(pstrt = i = 0; (i < xnum) && (pstrt == 0); i++) 
	if ( xnd[i].mat == POLY ) pstrt = xnd[i].num;
    for(pend = 0; (i < xnum) && (pend == 0); i++) 
	if ( xnd[i].mat == OXIDE ) pend = xnd[i-1].num;

    fprintf(fp, "\n$Gate = 1 Source = 2 Drain = 3 Bulk = 4\n");
    fprintf(fp, "electrode num=1 ix.l=%d ix.h=%d iy.l=1 iy.h=1\n", 
			    pstrt, pend);
    fprintf(fp, "electrode num=2 ix.l=1 ix.h=2 iy.l=%d iy.h=%d\n", 
			    oxend, oxend);
    fprintf(fp, "electrode num=3 ix.l=%d ix.h=%d iy.l=%d iy.h=%d\n", 
			    xnd[xnum-1].num-1, xnd[xnum-1].num, oxend, oxend);
    fprintf(fp, "electrode num=4 ix.l=1 ix.h=%d iy.l=%d iy.h=%d\n", 
			    xnd[xnum-1].num, ynd[ynum-1].num, ynd[ynum-1].num);

/*  print out doping information */

/*  go find poly edges again.  this time save the index that points to
 *   the xnd array of information rather than the actual node location
 */
    for(pstrt = i = 0; (i < xnum) && (pstrt == 0); i++) 
	if ( xnd[i].mat == POLY ) pstrt = i;
    for(pend = 0; (i < xnum) && (pend == 0); i++) 
	if ( xnd[i].mat == OXIDE ) pend = i-1;
    
    fprintf( fp, "\n$Doping Information\n" );
    switch ( dopingmode )  {

      case 0  :

        if ( vert_info.ars_junc >= vert_info.phos_junc )
            fprintf(fp, "$ junction depth is %gum\n", vert_info.ars_junc );
        else
            fprintf(fp, "$ junction depth is %gum\n", vert_info.phos_junc );

        fprintf(fp, "doping reg=2 uniform x.lef=%g x.rig=%g\n",
		xnd[0].loc, xnd[xnum-1].loc );
        fprintf(fp, "+\ty.top=0.0 y.bot=%g conc=%e %s\n",
		ynd[ynum-1].loc, vert_info.substrate_dop,
		dop_type[ (dop_data[2].type == N_TYPE)?P_TYPE:N_TYPE ] );


        /*channel doping*/
	if ( dop_data[0].type != NONE )
	    writeanaldoping( fp, &(dop_data[0]), xnd[0].loc, xnd[xnum-1].loc,
	        "Channel Doping Region" );
	    
        /*left and right lightly doped drain part*/
        if ( dop_data[1].dop_tag.anal_dop.peak_dop != 0.0 ) {
	    writeanaldoping( fp, &(dop_data[1]), xnd[0].loc, 
		xnd[pstrt].loc, "Lightly Doped Source" );
	    writeanaldoping( fp, &(dop_data[1]), xnd[pend].loc,
		xnd[xnum-1].loc, "Lightly Doped Drain" );
    	}

        /*left and right heavily doped drain part*/
	writeanaldoping( fp, &(dop_data[2]), xnd[0].loc, 
	    xnd[pstrt].loc - lat_info.spacer, "Heavily Doped Source" );
	writeanaldoping( fp, &(dop_data[2]), xnd[pend].loc + lat_info.spacer, 
	    xnd[xnum-1].loc, "Heavily Doped Drain" );

        break;

      case 1  : 

        if ( vert_info.ars_junc >= vert_info.phos_junc )
            fprintf(fp, "$ junction depth is %gum\n", vert_info.ars_junc );
        else
            fprintf(fp, "$ junction depth is %gum\n", vert_info.phos_junc );


	/* for suprem3 export format, we need a substrate */
	if ( (dop_data[0].type == SUPREM3EXPORT) &&
	     (dop_data[2].type == SUPREM3EXPORT) )  
	    fprintf(fp, "doping reg=2 uniform x.left=%g x.right=%g conc=%e %s\n\n",
		xnd[0].loc, xnd[xnum-1].loc, vert_info.substrate_dop, 
		dop_type[vert_info.substrate_dop_type]);



        /*channel doping*/
	switch (dop_data[0].type)  {
		case SUPREM3ASCII  :
	    		writesup3ascii( fp, dop_data[0], xnd[0].loc,
	        		xnd[xnum-1].loc, "Channel Doping Region" );
			break;


		case SUPREM3EXPORT :
	    		writesup3export( fp, dop_data[0], xnd[0].loc,
	        		xnd[xnum-1].loc, "Channel Doping Region" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */
	    
        /*left and right lightly doped drain part*/
	switch (dop_data[1].type)  {
		case SUPREM3ASCII  :

	    		writesup3ascii( fp, dop_data[1], xnd[0].loc, 
				xnd[pstrt].loc, "Lightly Doped Source" );
	    		writesup3ascii( fp, dop_data[1], xnd[pend].loc,
				xnd[xnum-1].loc, "Lightly Doped Drain" );
			break;


		case SUPREM3EXPORT :

	    		writesup3export( fp, dop_data[1], xnd[0].loc, 
				xnd[pstrt].loc, "Lightly Doped Source" );
	    		writesup3export( fp, dop_data[1], xnd[pend].loc,
				xnd[xnum-1].loc, "Lightly Doped Drain" );
			break;

		case NONE :
			break;

		default  : 
			fprintf( stderr, "\tillegal doping type\n" );
			exit( ERROR );
			break;
	}  /* end switch */

        /*left and right heavily doped drain part*/
	switch (dop_data[2].type)  {
		case SUPREM3ASCII  :
			writesup3ascii( fp, dop_data[2], xnd[0].loc,
	    		  xnd[pstrt].loc - lat_info.spacer, "Heavily Doped Source" );
			writesup3ascii( fp, dop_data[2], xnd[pend].loc + 
	    		 lat_info.spacer, xnd[xnum-1].loc, "Heavily Doped Drain" );

			break;


		case SUPREM3EXPORT :
			writesup3export( fp, dop_data[2], xnd[0].loc, 
	    		xnd[pstrt].loc - lat_info.spacer, "Heavily Doped Source" );
			writesup3export( fp, dop_data[2], xnd[pend].loc + 
	    		 lat_info.spacer, xnd[xnum-1].loc, "Heavily Doped Drain" );
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

      default :
	printf("\tillegal doping mode\n");
	break;

    } /* end switch (dopingmode) */

    /* enter in interface information.  although we only really need to
     *  include this only if we simulate, lets help the user out
     */
    fprintf( fp, "\ninterface qf=%g\n", qf ); 


    switch ( computemode )  {

	case 0  :	writedrainIV( fp, mindrain, maxdrain, mingate, maxgate, 
				drainincrement, gateincrement );
			break;

	case 1  :	writegateIV( fp, maxdrain, mingate, maxgate, 
				gateincrement );
			break;
    }  /* end switch */

    fprintf( fp, "end\n" );
    fclose( fp );
}
