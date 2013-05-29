/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	dump_info.c		Version 1.4		*/
/*	Last Modification:	1/29/90 01:39:53		*/



/* dump_info.c
 *	dumps out the data structure
 */

#include <stdio.h>
#include "struct.h"

extern lat_str lat_info;
extern vert_str vert_info;
extern dop_str dop_data[4];

dump_info()
{
    anal_dopstr an;
    sup3_dopstr s3;
    stride_dopstr strdop;
    int i;

    printf( "\temitter_width is %g\n", lat_info.emitter_width );
    if (stridedeck)
	printf( "\temitter_length is %g\n", lat_info.emitter_length );

    printf( "\tbase_width is %g\n", lat_info.base_width );
    if (stridedeck)
	printf( "\tbase_length is %g\n", lat_info.base_length );

    printf( "\tcollector_width is %g\n", lat_info.collector_width );
    if (stridedeck)
	printf( "\tcollector_length is %g\n", lat_info.collector_length );

    printf( "\tcontact_width is %g\n", lat_info.contact_width );
    if (stridedeck)
	printf( "\tcontact_length is %g\n", lat_info.contact_length );

    printf( "\tcontact_edge_spacing is %g\n", lat_info.contact_edge_spacing );
    printf( "\te_c_spacing is %g\n", lat_info.e_c_spacing );

    printf( "\temitter_depth is %g\n", vert_info.emitter_depth );
    printf( "\tint_base_depth is %g\n", vert_info.int_base_depth );
    printf( "\text_base_depth is %g\n", vert_info.ext_base_depth );
    printf( "\tcollector_depth is %g\n", vert_info.collector_depth );
    printf( "\tc_e_bias is %g\n", vert_info.c_e_bias );
    printf( "\tsubstrate doping is %g\n", vert_info.substrate_dop );
    printf( "\tsubstrate depth is %g\n", vert_info.substrate_depth );

    i = 0;
    for ( i = 0; i < 4; i++ )  
	if (stridedeck)
	    dump_stride(&((dop_data[i]).dop_tag.stride_dop));
	else  
	    switch ( dop_data[i].type )  {
		case N_TYPE	:
		case P_TYPE	:
		case N_GAUSSIAN	:
		case P_GAUSSIAN	:
		case N_ERFC	:
		case P_ERFC	:
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
