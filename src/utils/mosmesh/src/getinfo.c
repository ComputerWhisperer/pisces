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
#include "struct.h"

extern lat_str lat_info;
extern dop_str dop_data[10];
extern vert_str vert_info;
extern float maxdrain;
extern getprofiles();
extern getdefprofiles();
extern calc_junc();


#ifdef ANSI_FUNC

int 
get_info (int dopingmode, int inputmode)
#else

get_info( dopingmode, inputmode )
int dopingmode;
int inputmode;
#endif
{


    if ( inputmode == 1 )  {
        /*read in all the appropriate lateral data*/
        read_defreal( "Poly Gate Width(um)", &(lat_info.poly_width) );
        read_defreal( "Spacer Thickness(um) (Zero for none)", &(lat_info.spacer) );
        read_defreal( "Poly Metal Spacing(um)", &(lat_info.poly_metal) );

        /*
	   read in the vertical stuff.  substrate doping gathered directly
	   from suprem3 file.  also oxide thickness can be obtained from
	   suprem3 export format.  if it is 0 in the suprem3 export file
	   then the value that is asked below will be used 
	 */
	if ( dopingmode != 1 )
            read_defreal( "Substrate Doping", &(vert_info.substrate_dop) );
        read_defreal( "Gate Oxide Thickness(ang)", &(vert_info.oxide_thick) );
        /*  convert angstroms to microns */
        vert_info.oxide_thick *= 1e-4;

        /*read in all the doping information*/
        getdefprofiles();

        if ( maxdrain == 0.0 )  {
            read_defreal( "Maximum Drain Substrate Bias", &(vert_info.drain_sub_bias) );
	    if ( vert_info.drain_sub_bias < 0 )  {
	        printf( "\tForward biasing a junction. will reverse sign. rerun if\n" );
	        printf( "\t\tnot correct\n" );
	        fflush( stdout );
            }
        }
        else  
	    vert_info.drain_sub_bias = maxdrain;

        read_defreal( "Maximum Source Substrate Bias", &(vert_info.source_sub_bias) );
        if ( vert_info.source_sub_bias < 0 )  {
            printf( "\tForward biasing a junction. will reverse sign. rerun if\n" );
            printf( "\t\tnot correct\n" );
            fflush( stdout );
        }


    }
    else  {
        /*read in all the appropriate lateral data*/
        read_real( "Poly Gate Width(um)", &(lat_info.poly_width) );
        read_real( "Spacer Thickness(um) (Zero for none)", &(lat_info.spacer) );
        read_real( "Poly Metal Spacing(um)", &(lat_info.poly_metal) );

        /*
	   read in the vertical stuff.  substrate doping gathered directly
	   from suprem3 file.  also oxide thickness can be obtained from
	   suprem3 export format.  if it is 0 in the suprem3 export file
	   then the value that is asked below will be used 
	 */
	if ( dopingmode != 1 )
            read_real( "Substrate Doping", &(vert_info.substrate_dop) );
        read_real( "Gate Oxide Thickness(ang)", &(vert_info.oxide_thick) );
        /*  convert angstroms to microns */
        vert_info.oxide_thick *= 1e-4;

        /*read in all the doping information*/
        getprofiles();

        if ( maxdrain == 0.0 )  {
            read_real( "Maximum Drain Substrate Reverse Bias", &(vert_info.drain_sub_bias) );
	    if ( vert_info.drain_sub_bias < 0 )  {
	        printf( "\tForward biasing a junction. will reverse sign. rerun if\n" );
	        printf( "\t\tnot correct\n" );
	        fflush( stdout );
            }
        }
        else  
	    vert_info.drain_sub_bias = maxdrain;

        read_real( "Maximum Source Substrate Reverse Bias", &(vert_info.source_sub_bias) );
        if ( vert_info.source_sub_bias < 0 )  {
            printf( "\tForward biasing a junction. will reverse sign. rerun if\n" );
            printf( "\t\tnot correct\n" );
            fflush( stdout );
        }
    }
}
