/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/* dump_info.c
 *	dumps out the data structure
 */

#include <stdio.h>
#include "struct.h"

extern lat_str lat_info;
extern vert_str vert_info;
extern dop_str dop_data[10];

#ifdef ANSI_FUNC

int 
dump_info (void)
#else

dump_info()
#endif
{
    struct anal_str an;
    struct sup3_str s3;
    int i;

    printf( "\tpoly_width is %g\n", lat_info.poly_width );
    printf( "\tspacer width is %g\n", lat_info.spacer );
    printf( "\tpoly to metal spacing is %g\n", lat_info.poly_metal );

    printf( "\tphosphorus junction is %g\n", vert_info.phos_junc );
    printf( "\tarsenic	 junction is %g\n", vert_info.ars_junc );
    printf( "\tchannel junction is %g\n", vert_info.chan_junc );
    printf( "\toxide thickness is %g\n", vert_info.oxide_thick );
    printf( "\tdrain to substrate bias is %g\n", vert_info.drain_sub_bias );
    printf( "\tsource to substrate bias is %g\n", vert_info.source_sub_bias );
    printf( "\tsubstrate doping is %g\n", vert_info.substrate_dop );

    i = 0;
    for ( i = 0; i < 3; i++ )  
	 switch ( dop_data[i].type )  {
	     case N_TYPE  :
	     case P_TYPE  :
			an = (dop_data[i]).dop_tag.anal_dop;
			printf( "\tpeak doping of %g\n", an.peak_dop );
			printf( "\tstd. dev. of %g\n", an.std_dev );
			printf( "\tpeak doping location of %g\n", an.peak_loc );
			fflush( stdout );
			break;

	    case SUPREM3ASCII  :
	    case SUPREM3EXPORT  :
			s3 = (dop_data[i]).dop_tag.sup3_dop;
			printf( "\tdoping type of %d\n", s3.type );
			printf( "\tpeak doping of %g\n", s3.peak_dop );
			printf( "\tstd. dev. of %g\n", s3.std_dev );
			printf( "\tfile is %s\n", s3.filename );
			fflush( stdout );
			break;

	    case NONE  :
			break;
	    default  :
			fprintf( stderr, "\tillegal file type\n" );
			exit( ERROR );
			break;
  	} /* end switch */
}
