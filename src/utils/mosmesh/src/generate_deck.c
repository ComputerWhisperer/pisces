/*----------------------------------------------------------------------
**  Copyright 1988 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*  generate_deck - generates internal data necessary for pisces deck */

#include <math.h>
#include "struct.h"
#include <stdio.h>

extern dopingmode;
extern shortMOS();
extern longMOS();
extern lat_str lat_info;
extern vert_str vert_info;
extern dop_str dop_data[10];
extern double depl_width();


#ifdef ANSI_FUNC

int 
generate_deck (void)
#else

generate_deck()
#endif
{
  switch ( dopingmode )  {
    case 0  :
      if ( lat_info.poly_width <= 1.5 * ( (vert_info.phos_junc * 0.8 * 2.0)
	+ depl_width(vert_info.drain_sub_bias, vert_info.substrate_dop, 
	  dop_data[2].dop_tag.anal_dop.peak_dop) 
	  + depl_width(vert_info.source_sub_bias,
	  vert_info.substrate_dop, dop_data[2].dop_tag.anal_dop.peak_dop) ) )  
	shortMOS();
      else  
	longMOS();

      break;

    case 1  :
      if ( lat_info.poly_width <= 1.5 * ( (vert_info.phos_junc * 0.8 * 2.0)
	+ depl_width(vert_info.drain_sub_bias, vert_info.substrate_dop, 
	  dop_data[2].dop_tag.sup3_dop.peak_dop) 
	  + depl_width(vert_info.source_sub_bias,
	  vert_info.substrate_dop, dop_data[2].dop_tag.sup3_dop.peak_dop) ) )  
	shortMOS();
      else  
	longMOS();

      break;

    default :
      printf( "\t other cases not implemented yet\n" );
      break;
  } /* end switch */
}
