/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*	get3dinfo.c		Version 1.2		*/
/*	Last Modification:	1/22/90 13:20:37		*/




#include <stdio.h>
#include "struct.h"

extern float maxdrain;
extern getprofiles();
extern getdefprofiles();
extern calc_junc();


get_3dinfo( dopingmode, inputmode )
int dopingmode;
int inputmode;
{
    char buf[80];
    char answer;
    double temp;

    if ( inputmode == 1 )  {
        /*read in all the appropriate lateral data*/
	read_defreal( "lateral diffusion factor in x-direction",
	    &(xlat_factor) );
	read_defreal( "lateral diffusion factor in y-direction",
	    &(ylat_factor) );
        read_defreal( "Emitter Width(um)", &(lat_info.emitter_width) );
        read_defreal( "Emitter Length(um)", &(lat_info.emitter_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[0].cnt = 1;
	else
	    electrode[0].cnt = 2;
*/
	electrode[0].cnt = 1;
	electrode[0].typ = 1; /* contact on a semiconductor */
	electrode[0].shp = 0; /* rectilinear electrode shape */

        read_defreal( "Base Width(um)", &(lat_info.base_width) );
        read_defreal( "Base Length(um)", &(lat_info.base_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[1].cnt = 1;
	else
	    electrode[1].cnt = 2;
*/
	electrode[1].cnt = 1;
	electrode[1].typ = 1; /* contact on a semiconductor */
	electrode[1].shp = 0; /* rectilinear electrode shape */

        read_defreal( "Collector Width(um)", &(lat_info.collector_width) );
        read_defreal( "Collector Length(um)", &(lat_info.collector_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[2].cnt = 1;
	else
	    electrode[2].cnt = 2;
*/
	electrode[2].cnt = 1;
	electrode[2].typ = 1; /* contact on a semiconductor */
	electrode[2].shp = 0; /* rectilinear electrode shape */

        read_defreal( "Contact  Width(um)", &(lat_info.contact_width) );
        read_defreal( "Contact  Length(um)", &(lat_info.contact_length) );
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
	read_real( "lateral diffusion factor in x-direction",
	    &(xlat_factor) );
	read_real( "lateral diffusion factor in y-direction",
	    &(ylat_factor) );
        read_real( "Emitter Width(um)", &(lat_info.emitter_width) );
        read_real( "Emitter Length(um)", &(lat_info.emitter_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	printf( "Is the emitter contact ohmic? [%c]\t", answer );
        gets( buf );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[0].cnt = 1;
	else
	    electrode[0].cnt = 2;
*/
	electrode[0].cnt = 1;
	electrode[0].typ = 1; /* contact on a semiconductor */
	electrode[0].shp = 0; /* rectilinear electrode shape */

        read_real( "Base Width(um)", &(lat_info.base_width) );
        read_real( "Base Length(um)", &(lat_info.base_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	printf( "Is the base contact ohmic? [%c]\t", answer );
        gets( buf );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[1].cnt = 1;
	else
	    electrode[1].cnt = 2;
*/
	electrode[1].cnt = 1;
	electrode[1].typ = 1; /* contact on a semiconductor */
	electrode[1].shp = 0; /* rectilinear electrode shape */

        read_real( "Collector Width(um)", &(lat_info.collector_width) );
        read_real( "Collector Length(um)", &(lat_info.collector_length) );
/*
        fgets( buf, 80, defaultsfile );
	sscanf( buf, "%1s", &answer );
	printf( "Is the collector contact ohmic? [%c]\t", answer );
        gets( buf );
	sscanf( buf, "%1s", &answer );
	if ( (answer == 'y') || (answer == 'Y') )
	    electrode[2].cnt = 1;
	else
	    electrode[2].cnt = 2;
*/
	electrode[2].cnt = 1;
	electrode[2].typ = 1; /* contact on a semiconductor */
	electrode[2].shp = 0; /* rectilinear electrode shape */

        read_real( "Contact  Width(um)", &(lat_info.contact_width) );

        read_real( "Contact  Length(um)", &(lat_info.contact_length) );

        read_real( "Contact-Diffusion-Edge Spacing(um)", 
	    &(lat_info.contact_edge_spacing) );
	/* if contact edge spacing is 0, the current gridding algorithm
	 *	doesn't work properly.  force it to 0.5um
	 */
/*
	if (lat_info.contact_edge_spacing <= 0.0)
	    lat_info.contact_edge_spacing = 0.5;
*/

        read_real( "Emitter-Collector Spacing(um)", &(lat_info.e_c_spacing) );
	/* if contact-diffusion-edge spacing is 0, the current gridding 
	 *	algorithm doesn't work properly.  force it to 0.5um
	 */
/*
	if (lat_info.e_c_spacing <= 0.0)
	    lat_info.e_c_spacing = 0.5;
*/
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
