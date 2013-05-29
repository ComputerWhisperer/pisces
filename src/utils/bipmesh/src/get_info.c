/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	get_info.c		Version 1.2		*/
/*	Last Modification:	1/24/90 10:34:05		*/




#include <stdio.h>
#include "struct.h"

extern lat_str lat_info;
extern dop_str dop_data[4];
extern vert_str vert_info;
extern float maxdrain;
extern getprofiles();
extern getdefprofiles();
extern calc_junc();


get_info( dopingmode, inputmode )
int dopingmode;
int inputmode;
{
    if ( inputmode == 1 )  {
        /*read in all the appropriate lateral data*/
        read_defreal( "Emitter Width(um)", &(lat_info.emitter_width) );
        read_defreal( "Base Width(um)", &(lat_info.base_width) );
        read_defreal( "Collector Width(um)", &(lat_info.collector_width) );
        read_defreal( "Contact  Width(um)", &(lat_info.contact_width) );
        read_defreal( "Contact-Diffusion-Edge Spacing(um)", 
	    &(lat_info.contact_edge_spacing) );
        read_defreal( "Emitter-Collector Spacing(um)", &(lat_info.e_c_spacing));
        /*
  	   read in substrate doping.  if not a Suprem 3 file then ask user
	   for doping concentration.
	 */
	if ( dopingmode != 1 )
            read_defreal( "Substrate Doping", &(vert_info.substrate_dop) );

        /*read in all the doping information*/
        getdefprofiles();
    }
    else  {
        /*read in all the appropriate lateral data*/
        read_real( "Emitter Width(um)", &(lat_info.emitter_width) );
        read_real( "Base Width(um)", &(lat_info.base_width) );
        read_real( "Collector Width(um)", &(lat_info.collector_width) );
        read_real( "Contact  Width(um)", &(lat_info.contact_width) );
        read_real( "Contact-Diffusion-Edge Spacing(um)", 
	    &(lat_info.contact_edge_spacing) );
        read_real( "Emitter-Collector Spacing(um)", &(lat_info.e_c_spacing) );
        /*
  	   read in substrate doping.  if not a Suprem 3 file then ask user
	   for doping concentration.
	 */
	if ( dopingmode != 1 )
            read_real( "Substrate Doping", &(vert_info.substrate_dop) );

        /*read in all the doping information*/
        getprofiles();
    }
}
