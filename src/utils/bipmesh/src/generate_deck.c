/*----------------------------------------------------------------------
**  Copyright 1989 by
**  The Board of Trustees of the Leland Stanford Junior University
**  All rights reserved.
**
**  This routine may not be used without the prior written consent of
**  the Board of Trustees of the Leland Stanford University.
**----------------------------------------------------------------------
**/


/*	generate_deck.c		Version 1.3		*/
/*	Last Modification:	1/24/90 10:34:00		*/


/*  generate_deck - generates internal data necessary for pisces deck */

#include "struct.h"
#include <stdio.h>

extern dopingmode;
extern piscesnodegen();
extern stride_node();

#ifdef ANSI_FUNC

int 
generate_deck (void)
#else

generate_deck()
#endif
{
    switch ( dopingmode )  {
    	case 0  :
    	case 1  :
		if (stridedeck)
		    stride_node();
		else
    		    piscesnodegen();
      		break;

    	default :
      		printf( "\t other cases not implemented yet\n" );
      		break;
    } /* end switch */

    return( 1 );
}
