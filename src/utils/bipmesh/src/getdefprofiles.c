/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/

/*	getdefprofiles.c		Version 1.1		*/
/*	Last Modification:	8/16/89 08:29:42		*/



/* getdefprofiles - gathers default information for profiles */

#include <stdio.h>
#include "struct.h"
extern dop_str dop_data[4];
extern vert_str vert_info;
extern read_dop();
extern dopingmode;

getdefprofiles()

{
    int typeofprofile;
    char buf[3];


    switch ( dopingmode )  {
	case  0  : 
	
		typeofprofile = 0;
    		read_defdop( &(dop_data[0]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.emitter_depth) );
    		read_defdop( &(dop_data[1]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.int_base_depth) );
    		read_defdop( &(dop_data[3]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.ext_base_depth) );
		if ( vert_info.int_base_depth <= vert_info.emitter_depth )
		     panic( "base depth less than emitter depth. try again\n");
    		read_defdop( &(dop_data[2]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.collector_depth) );
		if ( ( vert_info.collector_depth <= vert_info.int_base_depth )
		  || ( vert_info.collector_depth <= vert_info.ext_base_depth) )
		     panic("collector depth less than base depth. try again\n");
		break;

  	case 1  :  	
	
		typeofprofile = 1;
    		read_defdop( &(dop_data[0]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.emitter_depth) );
    		read_defdop( &(dop_data[1]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.int_base_depth) );
    		read_defdop( &(dop_data[3]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.ext_base_depth) );
		if ( vert_info.int_base_depth <= vert_info.emitter_depth )
		     panic( "base depth less than emitter depth. try again\n");
    		read_defdop( &(dop_data[2]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.collector_depth) );
		if ( ( vert_info.collector_depth <= vert_info.int_base_depth )
		  || ( vert_info.collector_depth <= vert_info.ext_base_depth) )
		     panic("collector depth less than base depth. try again\n");
		if  (dop_data[2].type != SUPREM3EXPORT) 
		    computesubstratetype( dop_data[2], 
			&(vert_info.substrate_dop_type ) );
		break;

  	case 2  :  /* ask for information */
		printf( "\tType of profile for Emitter? [a or s]  :   ");
		fflush( stdout );
		scanf( "%s", buf );
		if ( (buf[0] == 's') || (buf[0] == 'S') )
		    typeofprofile = 1;
		else 
		    typeofprofile = 0;
    		read_defdop( &(dop_data[0]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.emitter_depth) );
		printf( "\tType of profile for Base? [a or s]  :   ");
		fflush( stdout );
		scanf( "%s", buf );
		if ( (buf[0] == 's') || (buf[0] == 'S') )
		    typeofprofile = 1;
		else 
		    typeofprofile = 0;
    		read_defdop( &(dop_data[1]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.int_base_depth) );
    		read_defdop( &(dop_data[3]), 
		    vert_info.substrate_dop, typeofprofile,
		    &(vert_info.ext_base_depth) );
		if ( vert_info.int_base_depth <= vert_info.emitter_depth )
		     panic( "base depth less than emitter depth. try again\n");
		printf( "\tType of profile for Collector? [a or s]  :   ");
		fflush( stdout );
		scanf( "%s", buf );
		if ( (buf[0] == 's') || (buf[0] == 'S') )
		    typeofprofile = 1;
		else 
		    typeofprofile = 0;
    		read_defdop( &(dop_data[2]), 
			vert_info.substrate_dop, typeofprofile,
			&(vert_info.collector_depth) );
		if ( ( vert_info.collector_depth <= vert_info.int_base_depth )
		  || ( vert_info.collector_depth <= vert_info.ext_base_depth) )
		     panic("collector depth less than base depth. try again\n");
		if  (dop_data[2].type != SUPREM3EXPORT) 
		    computesubstratetype( dop_data[2], 
			&(vert_info.substrate_dop_type ) );
		break;
    } /* end switch */
}
